# HANA Optimization and Testing - Code Examples

## Part 1: HANA Optimization

### 1.1 HANA-Optimized vs Classical ABAP

#### ❌ BAD: Classical ABAP (data-to-code)
```abap
" Fetching all data and processing in application server
SELECT * FROM vbak INTO TABLE @DATA(lt_orders).
DATA(lv_total) = 0.
LOOP AT lt_orders INTO DATA(ls_order).
  lv_total = lv_total + ls_order-netwr.
ENDLOOP.
```

#### ✅ GOOD: HANA-Optimized (code-to-data)
```abap
" Processing in database
SELECT SUM( netwr ) 
  FROM vbak
  INTO @DATA(lv_total).
```

---

### 1.2 Avoiding SELECT in Loops

#### ❌ BAD: SELECT in loop
```abap
LOOP AT lt_customers INTO DATA(ls_customer).
  SELECT SINGLE * FROM kna1
    WHERE kunnr = @ls_customer-kunnr
    INTO @DATA(ls_kna1).
  " process
ENDLOOP.
```

#### ✅ GOOD: FOR ALL ENTRIES
```abap
SELECT * FROM kna1
  FOR ALL ENTRIES IN @lt_customers
  WHERE kunnr = @lt_customers-kunnr
  INTO TABLE @DATA(lt_kna1).
  
LOOP AT lt_customers INTO DATA(ls_customer).
  READ TABLE lt_kna1 WITH KEY kunnr = ls_customer-kunnr
    INTO DATA(ls_kna1).
  " process
ENDLOOP.
```

#### ✅ BETTER: Use CDS view with association
```abap
SELECT * FROM z_customer_with_details
  INTO TABLE @DATA(lt_data).
" All data including associations loaded in one query
```

---

### 1.3 ABAP Managed Database Procedure (AMDP)

#### AMDP Class Definition
```abap
CLASS zcl_amdp_sales_summary DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_amdp_marker_hdb.
    
    TYPES: BEGIN OF ty_sales_summary,
             customer_id   TYPE kunnr,
             order_count   TYPE i,
             total_revenue TYPE netwr,
             currency      TYPE waerk,
           END OF ty_sales_summary.
           
    TYPES: tt_sales_summary TYPE STANDARD TABLE OF ty_sales_summary WITH DEFAULT KEY.
    
    CLASS-METHODS get_sales_summary
      IMPORTING VALUE(iv_year) TYPE gjahr
      EXPORTING VALUE(et_result) TYPE tt_sales_summary.
      
ENDCLASS.

CLASS zcl_amdp_sales_summary IMPLEMENTATION.

  METHOD get_sales_summary BY DATABASE PROCEDURE
    FOR HDB
    LANGUAGE SQLSCRIPT
    OPTIONS READ-ONLY
    USING vbak vbap.
    
    et_result = SELECT
      h.kunnr as customer_id,
      count(distinct h.vbeln) as order_count,
      sum(i.netwr) as total_revenue,
      h.waerk as currency
    FROM vbak as h
    INNER JOIN vbap as i ON h.vbeln = i.vbeln
    WHERE substring(h.erdat, 1, 4) = :iv_year
    GROUP BY h.kunnr, h.waerk
    ORDER BY total_revenue DESC;
    
  ENDMETHOD.
  
ENDCLASS.
```

#### Calling AMDP
```abap
DATA: lt_result TYPE zcl_amdp_sales_summary=>tt_sales_summary.

zcl_amdp_sales_summary=>get_sales_summary(
  EXPORTING
    iv_year = '2025'
  IMPORTING
    et_result = lt_result ).

LOOP AT lt_result INTO DATA(ls_result).
  WRITE: / ls_result-customer_id,
           ls_result-order_count,
           ls_result-total_revenue,
           ls_result-currency.
ENDLOOP.
```

---

### 1.4 Complex AMDP with Multiple Operations
```abap
METHOD calculate_kpi BY DATABASE PROCEDURE
  FOR HDB
  LANGUAGE SQLSCRIPT
  OPTIONS READ-ONLY
  USING vbak vbap mara.
  
  -- Intermediate table for orders
  lt_orders = SELECT
    h.vbeln,
    h.kunnr,
    h.erdat,
    i.posnr,
    i.matnr,
    i.netwr,
    h.waerk
  FROM vbak as h
  INNER JOIN vbap as i ON h.vbeln = i.vbeln
  WHERE h.erdat >= :iv_from_date
    AND h.erdat <= :iv_to_date;
  
  -- Join with material master
  lt_enriched = SELECT
    o.vbeln,
    o.kunnr,
    o.matnr,
    m.matkl as material_group,
    o.netwr,
    o.waerk
  FROM :lt_orders as o
  LEFT JOIN mara as m ON o.matnr = m.matnr;
  
  -- Aggregate by customer and material group
  et_result = SELECT
    kunnr as customer_id,
    material_group,
    count(*) as line_count,
    sum(netwr) as total_value,
    waerk as currency
  FROM :lt_enriched
  GROUP BY kunnr, material_group, waerk
  HAVING sum(netwr) > 1000
  ORDER BY total_value DESC;
  
ENDMETHOD.
```

---

### 1.5 Performance Tips

#### Use Aggregations in SELECT
```abap
" Instead of looping
SELECT 
  kunnr,
  COUNT(*) AS order_count,
  SUM(netwr) AS total_amount,
  AVG(netwr) AS avg_amount
FROM vbak
GROUP BY kunnr
INTO TABLE @DATA(lt_summary).
```

#### Use CASE in SELECT
```abap
SELECT
  vbeln,
  CASE vbtyp
    WHEN 'C' THEN 'Order'
    WHEN 'G' THEN 'Contract'
    ELSE 'Other'
  END AS doc_type,
  netwr
FROM vbak
INTO TABLE @DATA(lt_orders).
```

#### Use JOINs Instead of Multiple SELECTs
```abap
SELECT
  h~vbeln,
  h~kunnr,
  k~name1,
  i~posnr,
  i~matnr,
  i~netwr
FROM vbak AS h
INNER JOIN kna1 AS k ON h~kunnr = k~kunnr
INNER JOIN vbap AS i ON h~vbeln = i~vbeln
INTO TABLE @DATA(lt_result).
```

---

## Part 2: ABAP Unit Testing

### 2.1 Basic Test Class Structure
```abap
CLASS ltc_travel DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      environment TYPE REF TO if_cds_test_environment.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      teardown.

    METHODS:
      test_validate_customer FOR TESTING,
      test_validate_dates FOR TESTING,
      test_calculate_total FOR TESTING,
      test_accept_travel FOR TESTING.

ENDCLASS.

CLASS ltc_travel IMPLEMENTATION.

  METHOD class_setup.
    " Setup test environment once for all tests
    environment = cl_cds_test_environment=>create(
      i_for_entity = 'ZI_TRAVEL_XXX' ).
  ENDMETHOD.

  METHOD class_teardown.
    " Cleanup test environment
    environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    " Setup before each test
    environment->clear_doubles( ).
  ENDMETHOD.

  METHOD teardown.
    " Cleanup after each test
    ROLLBACK ENTITIES.
  ENDMETHOD.

  METHOD test_validate_customer.
    " Given - Invalid customer
    DATA(lt_travel_data) = VALUE tt_travel(
      ( travel_id = '001'
        customer_id = '999999'  " Non-existent customer
        agency_id = '070001'
        begin_date = '20260101'
        end_date = '20260110' ) ).
        
    environment->insert_test_data( lt_travel_data ).
    
    " When - Create travel
    MODIFY ENTITIES OF zi_travel_xxx
      ENTITY travel
        CREATE FIELDS ( travel_id customer_id agency_id begin_date end_date )
        WITH lt_travel_data
      FAILED DATA(ls_failed)
      REPORTED DATA(ls_reported).
      
    " Then - Should have validation error
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_failed-travel
      msg = 'Failed structure should not be empty' ).
      
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_reported-travel
      msg = 'Reported structure should contain error message' ).
  ENDMETHOD.

  METHOD test_validate_dates.
    " Given - Invalid date range
    DATA(lt_travel_data) = VALUE tt_travel(
      ( travel_id = '002'
        customer_id = '000001'
        agency_id = '070001'
        begin_date = '20260110'
        end_date = '20260101' ) ).  " End before begin
        
    " When - Create travel
    MODIFY ENTITIES OF zi_travel_xxx
      ENTITY travel
        CREATE FIELDS ( travel_id customer_id agency_id begin_date end_date )
        WITH lt_travel_data
      FAILED DATA(ls_failed)
      REPORTED DATA(ls_reported).
      
    " Then - Should have validation error
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_failed-travel
      msg = 'Date validation should fail' ).
  ENDMETHOD.

  METHOD test_calculate_total.
    " Given - Travel with booking fee
    DATA(lt_travel_data) = VALUE tt_travel(
      ( travel_id = '003'
        customer_id = '000001'
        agency_id = '070001'
        begin_date = '20260101'
        end_date = '20260110'
        booking_fee = '100.00'
        currency_code = 'EUR' ) ).
        
    " When - Create travel
    MODIFY ENTITIES OF zi_travel_xxx
      ENTITY travel
        CREATE FIELDS ( travel_id customer_id agency_id 
                       begin_date end_date booking_fee currency_code )
        WITH lt_travel_data
      MAPPED DATA(ls_mapped).
      
    " Then - Total should be calculated (booking_fee * 1.1)
    READ ENTITIES OF zi_travel_xxx
      ENTITY travel
        FIELDS ( total_price )
        WITH CORRESPONDING #( ls_mapped-travel )
      RESULT DATA(lt_result).
      
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-total_price
      exp = '110.00'
      msg = 'Total price should be booking fee * 1.1' ).
  ENDMETHOD.

  METHOD test_accept_travel.
    " Given - Travel in Open status
    DATA(lt_travel_data) = VALUE tt_travel(
      ( travel_id = '004'
        customer_id = '000001'
        agency_id = '070001'
        begin_date = '20260101'
        end_date = '20260110'
        overall_status = 'O' ) ).
        
    MODIFY ENTITIES OF zi_travel_xxx
      ENTITY travel
        CREATE FIELDS ( travel_id customer_id agency_id 
                       begin_date end_date overall_status )
        WITH lt_travel_data
      MAPPED DATA(ls_mapped).
      
    " When - Accept travel
    MODIFY ENTITIES OF zi_travel_xxx
      ENTITY travel
        EXECUTE acceptTravel
        FROM CORRESPONDING #( ls_mapped-travel )
      RESULT DATA(lt_result).
      
    " Then - Status should be Accepted
    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-%param-overall_status
      exp = 'A'
      msg = 'Status should be Accepted' ).
  ENDMETHOD.

ENDCLASS.
```

---

### 2.2 Testing with Test Doubles (Mocking)
```abap
CLASS ltc_with_mocks DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_customer_service TYPE REF TO zif_customer_service.

    METHODS:
      setup,
      test_customer_processing FOR TESTING.
      
ENDCLASS.

CLASS ltc_with_mocks IMPLEMENTATION.

  METHOD setup.
    " Create mock object
    mo_customer_service = CAST #( 
      cl_abap_testdouble=>create( 'ZIF_CUSTOMER_SERVICE' ) ).
    
    " Configure mock behavior
    cl_abap_testdouble=>configure_call( mo_customer_service
      )->returning( VALUE ty_customer( 
                      customer_id = '000001'
                      name = 'Test Customer'
                      city = 'Test City' ) ).
    mo_customer_service->get_customer_details( '000001' ).
  ENDMETHOD.

  METHOD test_customer_processing.
    " Given - Service with mocked dependency
    DATA(lo_service) = NEW zcl_order_processor( 
      io_customer_service = mo_customer_service ).
    
    " When - Process order
    DATA(ls_result) = lo_service->process_order( 
      iv_order_id = '0000000001'
      iv_customer_id = '000001' ).
    
    " Then - Should use mocked customer data
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-customer_name
      exp = 'Test Customer'
      msg = 'Should use mocked customer data' ).
      
    " Verify mock was called
    cl_abap_testdouble=>verify_expectations( mo_customer_service ).
  ENDMETHOD.

ENDCLASS.
```

---

### 2.3 Common Assertions
```abap
" Equality
cl_abap_unit_assert=>assert_equals(
  act = lv_actual
  exp = lv_expected
  msg = 'Values should be equal' ).

" Not equal
cl_abap_unit_assert=>assert_differs(
  act = lv_actual
  exp = lv_not_expected
  msg = 'Values should differ' ).

" True/False
cl_abap_unit_assert=>assert_true(
  act = lv_boolean
  msg = 'Should be true' ).

cl_abap_unit_assert=>assert_false(
  act = lv_boolean
  msg = 'Should be false' ).

" Initial/Not initial
cl_abap_unit_assert=>assert_initial(
  act = lv_value
  msg = 'Should be initial' ).

cl_abap_unit_assert=>assert_not_initial(
  act = lv_value
  msg = 'Should not be initial' ).

" Bound/Not bound (for references)
cl_abap_unit_assert=>assert_bound(
  act = lo_object
  msg = 'Object reference should be bound' ).

cl_abap_unit_assert=>assert_not_bound(
  act = lo_object
  msg = 'Object reference should not be bound' ).

" Table size
cl_abap_unit_assert=>assert_table_contains(
  table = lt_result
  line = ls_expected_line
  msg = 'Table should contain line' ).

" Number of lines
cl_abap_unit_assert=>assert_equals(
  act = lines( lt_result )
  exp = 5
  msg = 'Should have 5 lines' ).
```

---

### 2.4 Testing Exception Handling
```abap
METHOD test_exception_raised.
  DATA: lx_exception TYPE REF TO zcx_business_error.
  
  TRY.
      " When - Call method that should raise exception
      zcl_my_class=>process_invalid_data( iv_data = 'invalid' ).
      
      " Then - Should not reach here
      cl_abap_unit_assert=>fail( 'Exception should have been raised' ).
      
    CATCH zcx_business_error INTO lx_exception.
      " Then - Exception was raised as expected
      cl_abap_unit_assert=>assert_equals(
        act = lx_exception->get_text( )
        exp = 'Invalid data provided'
        msg = 'Exception message should match' ).
  ENDTRY.
ENDMETHOD.
```

---

### 2.5 Performance Testing
```abap
METHOD test_performance.
  DATA: lv_start TYPE i,
        lv_end TYPE i,
        lv_runtime TYPE i.
  
  " Given
  GET RUN TIME FIELD lv_start.
  
  " When - Execute code under test
  zcl_performance_critical=>process_large_dataset( 
    it_data = lt_large_dataset ).
  
  GET RUN TIME FIELD lv_end.
  lv_runtime = lv_end - lv_start.
  
  " Then - Should complete within acceptable time (e.g., 1 second)
  cl_abap_unit_assert=>assert_true(
    act = xsdbool( lv_runtime < 1000000 )  " microseconds
    msg = |Runtime { lv_runtime } exceeded threshold| ).
ENDMETHOD.
```

---

## Quick Reference

### HANA Optimization Checklist
- ✅ Push calculations to database
- ✅ Use aggregations in SELECT
- ✅ Avoid SELECT in loops
- ✅ Use CDS views with associations
- ✅ Use AMDP for complex calculations
- ✅ Minimize data transfer
- ✅ Use JOINs instead of multiple SELECTs

### Testing Best Practices
- ✅ Follow Given-When-Then pattern
- ✅ Test one thing per test method
- ✅ Use descriptive test method names
- ✅ Keep tests independent
- ✅ Use test doubles for external dependencies
- ✅ Aim for >80% code coverage
- ✅ Test both happy path and error scenarios

---

*Last updated: December 2025*

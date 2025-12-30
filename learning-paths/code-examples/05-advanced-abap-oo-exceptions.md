# Advanced ABAP OO and Exception Handling

This guide covers advanced object-oriented programming concepts and comprehensive exception handling patterns in ABAP.

## Table of Contents
- [Object-Oriented Design Patterns](#object-oriented-design-patterns)
- [Advanced Class Concepts](#advanced-class-concepts)
- [Exception Handling](#exception-handling)
- [ABAP Unit Testing for OO](#abap-unit-testing-for-oo)
- [Best Practices](#best-practices)

---

## Object-Oriented Design Patterns

### 1. Singleton Pattern
Ensures only one instance of a class exists.

```abap
CLASS zcl_configuration_manager DEFINITION
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING VALUE(ro_instance) TYPE REF TO zcl_configuration_manager.

    METHODS:
      get_config_value
        IMPORTING iv_key         TYPE string
        RETURNING VALUE(rv_value) TYPE string.

  PRIVATE SECTION.
    CLASS-DATA: go_instance TYPE REF TO zcl_configuration_manager.

    DATA: mt_config TYPE SORTED TABLE OF zconfig_kv
                         WITH UNIQUE KEY key.

    METHODS:
      constructor.

ENDCLASS.

CLASS zcl_configuration_manager IMPLEMENTATION.

  METHOD get_instance.
    " Thread-safe singleton implementation
    IF go_instance IS NOT BOUND.
      CREATE OBJECT go_instance.
    ENDIF.
    ro_instance = go_instance.
  ENDMETHOD.

  METHOD constructor.
    " Load configuration on first instantiation
    SELECT key, value
      FROM zconfig_table
      INTO TABLE @mt_config.
  ENDMETHOD.

  METHOD get_config_value.
    TRY.
        rv_value = mt_config[ key = iv_key ]-value.
      CATCH cx_sy_itab_line_not_found.
        rv_value = ''.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

" Usage:
DATA(lo_config) = zcl_configuration_manager=>get_instance( ).
DATA(lv_value) = lo_config->get_config_value( 'MAX_RECORDS' ).
```

### 2. Factory Pattern
Creates objects without specifying their exact class.

```abap
" Interface for different payment methods
INTERFACE zif_payment_processor.
  METHODS:
    process_payment
      IMPORTING iv_amount       TYPE p
                is_payment_info TYPE zst_payment_info
      RETURNING VALUE(rv_success) TYPE abap_bool
      RAISING   zcx_payment_error.
ENDINTERFACE.

" Concrete implementations
CLASS zcl_credit_card_processor DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_payment_processor.
ENDCLASS.

CLASS zcl_paypal_processor DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_payment_processor.
ENDCLASS.

CLASS zcl_bank_transfer_processor DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_payment_processor.
ENDCLASS.

" Factory class
CLASS zcl_payment_factory DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      create_processor
        IMPORTING iv_payment_type    TYPE zpe_payment_type
        RETURNING VALUE(ro_processor) TYPE REF TO zif_payment_processor
        RAISING   zcx_invalid_payment_type.
ENDCLASS.

CLASS zcl_payment_factory IMPLEMENTATION.
  METHOD create_processor.
    CASE iv_payment_type.
      WHEN 'CREDIT_CARD'.
        ro_processor = NEW zcl_credit_card_processor( ).
      WHEN 'PAYPAL'.
        ro_processor = NEW zcl_paypal_processor( ).
      WHEN 'BANK_TRANSFER'.
        ro_processor = NEW zcl_bank_transfer_processor( ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_invalid_payment_type
          EXPORTING
            payment_type = iv_payment_type.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

" Usage:
TRY.
    DATA(lo_processor) = zcl_payment_factory=>create_processor( 'PAYPAL' ).
    DATA(lv_success) = lo_processor->process_payment(
      iv_amount       = 100
      is_payment_info = ls_payment_info
    ).
  CATCH zcx_invalid_payment_type INTO DATA(lx_error).
    " Handle error
ENDTRY.
```

### 3. Strategy Pattern
Defines a family of algorithms and makes them interchangeable.

```abap
" Strategy Interface
INTERFACE zif_discount_strategy.
  METHODS:
    calculate_discount
      IMPORTING iv_amount          TYPE p
                is_customer        TYPE zst_customer
      RETURNING VALUE(rv_discount) TYPE p.
ENDINTERFACE.

" Concrete Strategies
CLASS zcl_regular_discount DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_discount_strategy.
ENDCLASS.

CLASS zcl_regular_discount IMPLEMENTATION.
  METHOD zif_discount_strategy~calculate_discount.
    rv_discount = iv_amount * '0.05'.  " 5% discount
  ENDMETHOD.
ENDCLASS.

CLASS zcl_premium_discount DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_discount_strategy.
ENDCLASS.

CLASS zcl_premium_discount IMPLEMENTATION.
  METHOD zif_discount_strategy~calculate_discount.
    rv_discount = iv_amount * '0.15'.  " 15% discount
  ENDMETHOD.
ENDCLASS.

CLASS zcl_vip_discount DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_discount_strategy.
ENDCLASS.

CLASS zcl_vip_discount IMPLEMENTATION.
  METHOD zif_discount_strategy~calculate_discount.
    rv_discount = iv_amount * '0.25'.  " 25% discount
  ENDMETHOD.
ENDCLASS.

" Context Class
CLASS zcl_order_processor DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING io_discount_strategy TYPE REF TO zif_discount_strategy,

      calculate_final_amount
        IMPORTING iv_amount       TYPE p
                  is_customer     TYPE zst_customer
        RETURNING VALUE(rv_total) TYPE p.

  PRIVATE SECTION.
    DATA: mo_discount_strategy TYPE REF TO zif_discount_strategy.
ENDCLASS.

CLASS zcl_order_processor IMPLEMENTATION.
  METHOD constructor.
    mo_discount_strategy = io_discount_strategy.
  ENDMETHOD.

  METHOD calculate_final_amount.
    DATA(lv_discount) = mo_discount_strategy->calculate_discount(
      iv_amount   = iv_amount
      is_customer = is_customer
    ).
    rv_total = iv_amount - lv_discount.
  ENDMETHOD.
ENDCLASS.

" Usage:
DATA(lo_processor) = NEW zcl_order_processor(
  io_discount_strategy = NEW zcl_premium_discount( )
).

DATA(lv_final_amount) = lo_processor->calculate_final_amount(
  iv_amount   = 1000
  is_customer = ls_customer
).
```

### 4. Observer Pattern
Defines a one-to-many dependency between objects.

```abap
" Observer Interface
INTERFACE zif_order_observer.
  METHODS:
    on_order_created
      IMPORTING is_order TYPE zst_order,
    on_order_cancelled
      IMPORTING is_order TYPE zst_order.
ENDINTERFACE.

" Subject Class
CLASS zcl_order_manager DEFINITION.
  PUBLIC SECTION.
    METHODS:
      attach_observer
        IMPORTING io_observer TYPE REF TO zif_order_observer,

      detach_observer
        IMPORTING io_observer TYPE REF TO zif_order_observer,

      create_order
        IMPORTING is_order TYPE zst_order
        RAISING   zcx_order_error,

      cancel_order
        IMPORTING iv_order_id TYPE zorder_id
        RAISING   zcx_order_error.

  PRIVATE SECTION.
    DATA: mt_observers TYPE TABLE OF REF TO zif_order_observer.

    METHODS:
      notify_created
        IMPORTING is_order TYPE zst_order,

      notify_cancelled
        IMPORTING is_order TYPE zst_order.
ENDCLASS.

CLASS zcl_order_manager IMPLEMENTATION.
  METHOD attach_observer.
    APPEND io_observer TO mt_observers.
  ENDMETHOD.

  METHOD detach_observer.
    DELETE mt_observers WHERE table_line = io_observer.
  ENDMETHOD.

  METHOD create_order.
    " Create order logic
    INSERT zorders FROM is_order.
    IF sy-subrc = 0.
      notify_created( is_order ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_order_error.
    ENDIF.
  ENDMETHOD.

  METHOD cancel_order.
    " Cancel order logic
    SELECT SINGLE * FROM zorders
      WHERE order_id = @iv_order_id
      INTO @DATA(ls_order).

    IF sy-subrc = 0.
      UPDATE zorders SET status = 'CANCELLED'
        WHERE order_id = iv_order_id.
      notify_cancelled( ls_order ).
    ENDIF.
  ENDMETHOD.

  METHOD notify_created.
    LOOP AT mt_observers INTO DATA(lo_observer).
      lo_observer->on_order_created( is_order ).
    ENDLOOP.
  ENDMETHOD.

  METHOD notify_cancelled.
    LOOP AT mt_observers INTO DATA(lo_observer).
      lo_observer->on_order_cancelled( is_order ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

" Concrete Observers
CLASS zcl_email_notifier DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_order_observer.
ENDCLASS.

CLASS zcl_email_notifier IMPLEMENTATION.
  METHOD zif_order_observer~on_order_created.
    " Send email notification
    DATA(lv_email) = |Order { is_order-order_id } created successfully|.
    " Send email logic here
  ENDMETHOD.

  METHOD zif_order_observer~on_order_cancelled.
    " Send cancellation email
    DATA(lv_email) = |Order { is_order-order_id } cancelled|.
    " Send email logic here
  ENDMETHOD.
ENDCLASS.

CLASS zcl_inventory_updater DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_order_observer.
ENDCLASS.

CLASS zcl_inventory_updater IMPLEMENTATION.
  METHOD zif_order_observer~on_order_created.
    " Update inventory
    " Decrease stock for ordered items
  ENDMETHOD.

  METHOD zif_order_observer~on_order_cancelled.
    " Restore inventory
    " Increase stock for cancelled items
  ENDMETHOD.
ENDCLASS.

" Usage:
DATA(lo_order_mgr) = NEW zcl_order_manager( ).

" Register observers
lo_order_mgr->attach_observer( NEW zcl_email_notifier( ) ).
lo_order_mgr->attach_observer( NEW zcl_inventory_updater( ) ).

" Create order (will notify all observers)
TRY.
    lo_order_mgr->create_order( ls_order ).
  CATCH zcx_order_error INTO DATA(lx_error).
    " Handle error
ENDTRY.
```

---

## Advanced Class Concepts

### 1. Abstract Classes and Methods

```abap
CLASS zcl_abstract_document DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      " Abstract method - must be implemented by subclasses
      process ABSTRACT
        IMPORTING iv_data TYPE string
        RAISING   zcx_document_error,

      " Concrete method with default implementation
      validate
        IMPORTING iv_data         TYPE string
        RETURNING VALUE(rv_valid) TYPE abap_bool.

  PROTECTED SECTION.
    DATA: mv_document_type TYPE string.

  PRIVATE SECTION.
    METHODS:
      log_operation
        IMPORTING iv_message TYPE string.
ENDCLASS.

CLASS zcl_abstract_document IMPLEMENTATION.
  METHOD validate.
    " Default validation logic
    rv_valid = abap_true.
    IF iv_data IS INITIAL.
      rv_valid = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD log_operation.
    " Logging implementation
  ENDMETHOD.
ENDCLASS.

" Concrete implementation
CLASS zcl_pdf_document DEFINITION INHERITING FROM zcl_abstract_document.
  PUBLIC SECTION.
    METHODS:
      process REDEFINITION.
ENDCLASS.

CLASS zcl_pdf_document IMPLEMENTATION.
  METHOD process.
    " PDF-specific processing
    IF validate( iv_data ) = abap_false.
      RAISE EXCEPTION TYPE zcx_document_error
        EXPORTING
          textid = zcx_document_error=>invalid_data.
    ENDIF.

    " Process PDF
    mv_document_type = 'PDF'.
    " ... PDF processing logic ...
  ENDMETHOD.
ENDCLASS.
```

### 2. Friend Classes

```abap
CLASS zcl_bank_account DEFINITION.
  PUBLIC SECTION.
    METHODS:
      get_balance
        RETURNING VALUE(rv_balance) TYPE p,

      deposit
        IMPORTING iv_amount TYPE p.

  PROTECTED SECTION.
    " Only friend classes can access protected members
    METHODS:
      set_balance
        IMPORTING iv_balance TYPE p.

  PRIVATE SECTION.
    DATA: mv_balance TYPE p DECIMALS 2.

    " Declare friend class
    FRIENDS zcl_bank_auditor.
ENDCLASS.

CLASS zcl_bank_account IMPLEMENTATION.
  METHOD get_balance.
    rv_balance = mv_balance.
  ENDMETHOD.

  METHOD deposit.
    mv_balance = mv_balance + iv_amount.
  ENDMETHOD.

  METHOD set_balance.
    mv_balance = iv_balance.
  ENDMETHOD.
ENDCLASS.

" Friend class with access to protected members
CLASS zcl_bank_auditor DEFINITION.
  PUBLIC SECTION.
    METHODS:
      audit_account
        IMPORTING io_account TYPE REF TO zcl_bank_account.
ENDCLASS.

CLASS zcl_bank_auditor IMPLEMENTATION.
  METHOD audit_account.
    " Can access protected method because of FRIENDS declaration
    DATA(lv_balance) = io_account->get_balance( ).

    " Perform audit
    IF lv_balance < 0.
      " Adjust balance (only auditor can do this)
      io_account->set_balance( 0 ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

### 3. Events and Event Handling

```abap
" Publisher class with event
CLASS zcl_data_processor DEFINITION.
  PUBLIC SECTION.
    EVENTS:
      data_processed
        EXPORTING VALUE(ev_record_count) TYPE i
                  VALUE(ev_status)       TYPE string.

    METHODS:
      process_data
        IMPORTING it_data TYPE STANDARD TABLE.

  PRIVATE SECTION.
    METHODS:
      raise_processed_event
        IMPORTING iv_count  TYPE i
                  iv_status TYPE string.
ENDCLASS.

CLASS zcl_data_processor IMPLEMENTATION.
  METHOD process_data.
    " Process data
    DATA(lv_count) = lines( it_data ).

    " Raise event
    raise_processed_event(
      iv_count  = lv_count
      iv_status = 'SUCCESS'
    ).
  ENDMETHOD.

  METHOD raise_processed_event.
    RAISE EVENT data_processed
      EXPORTING
        ev_record_count = iv_count
        ev_status       = iv_status.
  ENDMETHOD.
ENDCLASS.

" Subscriber class
CLASS zcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_data_processed
        FOR EVENT data_processed OF zcl_data_processor
        IMPORTING ev_record_count ev_status,

      register_for_events
        IMPORTING io_processor TYPE REF TO zcl_data_processor.
ENDCLASS.

CLASS zcl_event_handler IMPLEMENTATION.
  METHOD on_data_processed.
    " Handle the event
    WRITE: / 'Processed', ev_record_count, 'records with status:', ev_status.
  ENDMETHOD.

  METHOD register_for_events.
    " Register event handler
    SET HANDLER me->on_data_processed FOR io_processor.
  ENDMETHOD.
ENDCLASS.

" Usage:
DATA(lo_processor) = NEW zcl_data_processor( ).
DATA(lo_handler) = NEW zcl_event_handler( ).

" Register handler for events
lo_handler->register_for_events( lo_processor ).

" Process data (will trigger event)
lo_processor->process_data( lt_data ).
```

---

## Exception Handling

### 1. Custom Exception Classes

```abap
" Base exception class
CLASS zcx_business_error DEFINITION
  INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    INTERFACES: if_t100_message,
                if_t100_dyn_msg.

    CONSTANTS:
      BEGIN OF zcx_business_error,
        msgid TYPE symsgid VALUE 'ZMSG',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MV_ERROR_CODE',
        attr2 TYPE scx_attrname VALUE 'MV_OBJECT_ID',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_business_error.

    DATA:
      mv_error_code TYPE string READ-ONLY,
      mv_object_id  TYPE string READ-ONLY.

    METHODS:
      constructor
        IMPORTING
          textid      LIKE if_t100_message=>t100key OPTIONAL
          previous    LIKE previous OPTIONAL
          error_code  TYPE string OPTIONAL
          object_id   TYPE string OPTIONAL.
ENDCLASS.

CLASS zcx_business_error IMPLEMENTATION.
  METHOD constructor.
    super->constructor( previous = previous ).

    mv_error_code = error_code.
    mv_object_id = object_id.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = zcx_business_error.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" Specific exception classes
CLASS zcx_customer_not_found DEFINITION
  INHERITING FROM zcx_business_error.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF customer_not_found,
        msgid TYPE symsgid VALUE 'ZMSG',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE 'MV_CUSTOMER_ID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF customer_not_found.

    DATA:
      mv_customer_id TYPE kunnr READ-ONLY.

    METHODS:
      constructor
        IMPORTING
          textid       LIKE if_t100_message=>t100key OPTIONAL
          previous     LIKE previous OPTIONAL
          customer_id  TYPE kunnr OPTIONAL.
ENDCLASS.

CLASS zcx_customer_not_found IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
      previous   = previous
      error_code = 'CUST_NOT_FOUND'
      object_id  = customer_id
    ).

    mv_customer_id = customer_id.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = customer_not_found.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

### 2. Exception Handling Patterns

```abap
CLASS zcl_customer_service DEFINITION.
  PUBLIC SECTION.
    METHODS:
      get_customer
        IMPORTING iv_customer_id    TYPE kunnr
        RETURNING VALUE(rs_customer) TYPE zst_customer
        RAISING   zcx_customer_not_found
                  zcx_database_error.

  PRIVATE SECTION.
    METHODS:
      validate_customer_id
        IMPORTING iv_customer_id TYPE kunnr
        RAISING   zcx_business_error.
ENDCLASS.

CLASS zcl_customer_service IMPLEMENTATION.
  METHOD get_customer.
    " Validate input
    TRY.
        validate_customer_id( iv_customer_id ).
      CATCH zcx_business_error INTO DATA(lx_validation).
        " Re-raise with additional context
        RAISE EXCEPTION TYPE zcx_customer_not_found
          EXPORTING
            previous    = lx_validation
            customer_id = iv_customer_id.
    ENDTRY.

    " Database operation with exception handling
    TRY.
        SELECT SINGLE *
          FROM zcustomers
          WHERE customer_id = @iv_customer_id
          INTO @rs_customer.

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_customer_not_found
            EXPORTING
              customer_id = iv_customer_id.
        ENDIF.

      CATCH cx_sy_sql_error INTO DATA(lx_sql).
        " Wrap system exception in custom exception
        RAISE EXCEPTION TYPE zcx_database_error
          EXPORTING
            previous   = lx_sql
            error_code = 'DB_SELECT_ERROR'
            object_id  = |Customer { iv_customer_id }|.
    ENDTRY.
  ENDMETHOD.

  METHOD validate_customer_id.
    IF iv_customer_id IS INITIAL.
      RAISE EXCEPTION TYPE zcx_business_error
        EXPORTING
          error_code = 'INVALID_INPUT'
          object_id  = 'CUSTOMER_ID'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" Usage with comprehensive error handling:
CLASS zcl_customer_controller DEFINITION.
  PUBLIC SECTION.
    METHODS:
      display_customer
        IMPORTING iv_customer_id TYPE kunnr.
ENDCLASS.

CLASS zcl_customer_controller IMPLEMENTATION.
  METHOD display_customer.
    DATA(lo_service) = NEW zcl_customer_service( ).

    TRY.
        DATA(ls_customer) = lo_service->get_customer( iv_customer_id ).

        " Display customer data
        WRITE: / 'Customer:', ls_customer-name.

      CATCH zcx_customer_not_found INTO DATA(lx_not_found).
        " Specific handling for customer not found
        MESSAGE lx_not_found TYPE 'E'.
        WRITE: / 'Customer', lx_not_found->mv_customer_id, 'not found'.

      CATCH zcx_database_error INTO DATA(lx_db_error).
        " Specific handling for database errors
        MESSAGE lx_db_error TYPE 'E'.

        " Log error details
        DATA(lv_error_msg) = lx_db_error->get_text( ).
        " ... logging logic ...

      CATCH zcx_business_error INTO DATA(lx_business).
        " General business error handling
        MESSAGE lx_business TYPE 'E'.

      CATCH cx_root INTO DATA(lx_root).
        " Catch-all for unexpected errors
        MESSAGE 'An unexpected error occurred' TYPE 'E'.
        " Log full exception stack
        DATA(lv_full_text) = lx_root->get_longtext( ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
```

### 3. CLEANUP Blocks

```abap
CLASS zcl_file_processor DEFINITION.
  PUBLIC SECTION.
    METHODS:
      process_file
        IMPORTING iv_filename TYPE string
        RAISING   zcx_file_error.

  PRIVATE SECTION.
    DATA:
      mv_file_opened TYPE abap_bool,
      mv_lock_set    TYPE abap_bool.

    METHODS:
      open_file
        IMPORTING iv_filename TYPE string
        RAISING   zcx_file_error,

      close_file,

      acquire_lock
        RAISING zcx_lock_error,

      release_lock.
ENDCLASS.

CLASS zcl_file_processor IMPLEMENTATION.
  METHOD process_file.
    TRY.
        " Acquire resources
        open_file( iv_filename ).
        acquire_lock( ).

        " Process file
        " ... processing logic ...

      CATCH zcx_file_error INTO DATA(lx_file).
        " Handle file error
        RAISE EXCEPTION lx_file.

      CATCH zcx_lock_error INTO DATA(lx_lock).
        " Handle lock error
        RAISE EXCEPTION TYPE zcx_file_error
          EXPORTING
            previous = lx_lock.

      CLEANUP.
        " Always execute cleanup, even if exception occurs
        IF mv_lock_set = abap_true.
          release_lock( ).
        ENDIF.

        IF mv_file_opened = abap_true.
          close_file( ).
        ENDIF.
    ENDTRY.
  ENDMETHOD.

  METHOD open_file.
    " Open file logic
    " ...
    mv_file_opened = abap_true.
  ENDMETHOD.

  METHOD close_file.
    " Close file logic
    " ...
    mv_file_opened = abap_false.
  ENDMETHOD.

  METHOD acquire_lock.
    " Lock logic
    " ...
    mv_lock_set = abap_true.
  ENDMETHOD.

  METHOD release_lock.
    " Release lock logic
    " ...
    mv_lock_set = abap_false.
  ENDMETHOD.
ENDCLASS.
```

---

## ABAP Unit Testing for OO

### 1. Test Class Setup

```abap
CLASS zcl_calculator DEFINITION.
  PUBLIC SECTION.
    METHODS:
      add
        IMPORTING iv_a            TYPE i
                  iv_b            TYPE i
        RETURNING VALUE(rv_result) TYPE i,

      divide
        IMPORTING iv_dividend     TYPE p
                  iv_divisor      TYPE p
        RETURNING VALUE(rv_result) TYPE p
        RAISING   zcx_division_by_zero.
ENDCLASS.

CLASS zcl_calculator IMPLEMENTATION.
  METHOD add.
    rv_result = iv_a + iv_b.
  ENDMETHOD.

  METHOD divide.
    IF iv_divisor = 0.
      RAISE EXCEPTION TYPE zcx_division_by_zero.
    ENDIF.
    rv_result = iv_dividend / iv_divisor.
  ENDMETHOD.
ENDCLASS.

" Test class
CLASS ltc_calculator DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_calculator.  " Class Under Test

    METHODS:
      setup,
      teardown,

      " Test methods
      test_add FOR TESTING,
      test_divide_success FOR TESTING,
      test_divide_by_zero FOR TESTING.
ENDCLASS.

CLASS ltc_calculator IMPLEMENTATION.
  METHOD setup.
    " Create instance before each test
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD teardown.
    " Cleanup after each test
    FREE mo_cut.
  ENDMETHOD.

  METHOD test_add.
    " Arrange
    DATA(lv_a) = 5.
    DATA(lv_b) = 3.

    " Act
    DATA(lv_result) = mo_cut->add(
      iv_a = lv_a
      iv_b = lv_b
    ).

    " Assert
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 8
      msg = 'Addition failed'
    ).
  ENDMETHOD.

  METHOD test_divide_success.
    " Act
    DATA(lv_result) = mo_cut->divide(
      iv_dividend = 10
      iv_divisor  = 2
    ).

    " Assert
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 5
      msg = 'Division failed'
    ).
  ENDMETHOD.

  METHOD test_divide_by_zero.
    " Assert that exception is raised
    TRY.
        DATA(lv_result) = mo_cut->divide(
          iv_dividend = 10
          iv_divisor  = 0
        ).

        " If we get here, test failed
        cl_abap_unit_assert=>fail(
          msg = 'Expected exception not raised'
        ).

      CATCH zcx_division_by_zero.
        " Expected exception caught - test passes
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
```

### 2. Test Doubles (Mocks/Stubs)

```abap
" Interface for database access
INTERFACE zif_customer_repository.
  METHODS:
    get_customer
      IMPORTING iv_id             TYPE kunnr
      RETURNING VALUE(rs_customer) TYPE zst_customer
      RAISING   zcx_customer_not_found.
ENDINTERFACE.

" Production implementation
CLASS zcl_customer_db_repository DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_customer_repository.
ENDCLASS.

CLASS zcl_customer_db_repository IMPLEMENTATION.
  METHOD zif_customer_repository~get_customer.
    SELECT SINGLE *
      FROM zcustomers
      WHERE id = @iv_id
      INTO @rs_customer.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_customer_not_found.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" Business logic class that depends on repository
CLASS zcl_customer_service DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING io_repository TYPE REF TO zif_customer_repository,

      get_customer_name
        IMPORTING iv_id          TYPE kunnr
        RETURNING VALUE(rv_name) TYPE string
        RAISING   zcx_customer_not_found.

  PRIVATE SECTION.
    DATA: mo_repository TYPE REF TO zif_customer_repository.
ENDCLASS.

CLASS zcl_customer_service IMPLEMENTATION.
  METHOD constructor.
    mo_repository = io_repository.
  ENDMETHOD.

  METHOD get_customer_name.
    DATA(ls_customer) = mo_repository->get_customer( iv_id ).
    rv_name = ls_customer-name.
  ENDMETHOD.
ENDCLASS.

" Test class with mock repository
CLASS ltc_customer_service DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA:
      mo_service        TYPE REF TO zcl_customer_service,
      mo_mock_repository TYPE REF TO lcl_mock_repository.

    METHODS:
      setup,
      test_get_customer_name FOR TESTING.
ENDCLASS.

" Mock implementation (defined locally in test class)
CLASS lcl_mock_repository DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_customer_repository.

    DATA: ms_mock_customer TYPE zst_customer.
ENDCLASS.

CLASS lcl_mock_repository IMPLEMENTATION.
  METHOD zif_customer_repository~get_customer.
    " Return predefined test data
    rs_customer = ms_mock_customer.
  ENDMETHOD.
ENDCLASS.

CLASS ltc_customer_service IMPLEMENTATION.
  METHOD setup.
    " Create mock repository
    mo_mock_repository = NEW lcl_mock_repository( ).

    " Set up test data
    mo_mock_repository->ms_mock_customer = VALUE #(
      id   = '12345'
      name = 'John Doe'
    ).

    " Inject mock into service
    mo_service = NEW zcl_customer_service( mo_mock_repository ).
  ENDMETHOD.

  METHOD test_get_customer_name.
    " Act
    DATA(lv_name) = mo_service->get_customer_name( '12345' ).

    " Assert
    cl_abap_unit_assert=>assert_equals(
      act = lv_name
      exp = 'John Doe'
      msg = 'Customer name mismatch'
    ).
  ENDMETHOD.
ENDCLASS.
```

---

## Best Practices

### 1. SOLID Principles

```abap
" S - Single Responsibility Principle
" Each class has one reason to change

" ❌ BAD: Multiple responsibilities
CLASS zcl_customer_manager_bad DEFINITION.
  PUBLIC SECTION.
    METHODS:
      create_customer,
      send_email,
      generate_report,
      calculate_discount.
ENDCLASS.

" ✅ GOOD: Separate responsibilities
CLASS zcl_customer_manager DEFINITION.
  PUBLIC SECTION.
    METHODS: create_customer, update_customer.
ENDCLASS.

CLASS zcl_email_service DEFINITION.
  PUBLIC SECTION.
    METHODS: send_email.
ENDCLASS.

CLASS zcl_report_generator DEFINITION.
  PUBLIC SECTION.
    METHODS: generate_customer_report.
ENDCLASS.

" O - Open/Closed Principle
" Open for extension, closed for modification

" ✅ GOOD: Use inheritance or interfaces
INTERFACE zif_shape.
  METHODS:
    calculate_area
      RETURNING VALUE(rv_area) TYPE p.
ENDINTERFACE.

CLASS zcl_circle DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_shape.
    DATA: mv_radius TYPE p.
ENDCLASS.

CLASS zcl_rectangle DEFINITION.
  PUBLIC SECTION.
    INTERFACES: zif_shape.
    DATA: mv_width TYPE p, mv_height TYPE p.
ENDCLASS.

" L - Liskov Substitution Principle
" Derived classes must be substitutable for base classes

" I - Interface Segregation Principle
" Don't force classes to implement unused methods

" ❌ BAD: Fat interface
INTERFACE zif_worker_bad.
  METHODS: work, eat, sleep, file_taxes.
ENDINTERFACE.

" ✅ GOOD: Segregated interfaces
INTERFACE zif_workable.
  METHODS: work.
ENDINTERFACE.

INTERFACE zif_eatable.
  METHODS: eat.
ENDINTERFACE.

" D - Dependency Inversion Principle
" Depend on abstractions, not concretions
" (Already demonstrated in test doubles example)
```

### 2. Exception Handling Best Practices

```abap
" ✅ DO: Use specific exceptions
TRY.
    " operation
  CATCH zcx_customer_not_found.
    " Specific handling
  CATCH zcx_database_error.
    " Different specific handling
ENDTRY.

" ❌ DON'T: Catch generic exceptions without re-raising
TRY.
    " operation
  CATCH cx_root.  " Too generic
    " Silent fail - bad!
ENDTRY.

" ✅ DO: Clean up resources
TRY.
    " acquire resources
  CATCH cx_error.
    " handle
  CLEANUP.
    " release resources
ENDTRY.

" ✅ DO: Provide context in exceptions
RAISE EXCEPTION TYPE zcx_business_error
  EXPORTING
    textid     = zcx_business_error=>customer_not_found
    previous   = lx_previous_exception
    error_code = 'CUST001'
    object_id  = lv_customer_id.
```

---

**Last Updated:** December 2025

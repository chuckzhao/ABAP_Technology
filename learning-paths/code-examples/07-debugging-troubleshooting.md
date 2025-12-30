# Debugging and Troubleshooting in ABAP

This guide covers debugging techniques, performance analysis, and troubleshooting strategies in ABAP.

## Table of Contents
- [Debugging Techniques](#debugging-techniques)
- [Performance Analysis](#performance-analysis)
- [Error Analysis and Tracing](#error-analysis-and-tracing)
- [Memory Management](#memory-management)
- [Troubleshooting Common Issues](#troubleshooting-common-issues)

---

## Debugging Techniques

### 1. Breakpoints and Watchpoints

```abap
CLASS zcl_debug_example DEFINITION.
  PUBLIC SECTION.
    METHODS:
      process_orders
        IMPORTING it_orders TYPE ztt_order.

  PRIVATE SECTION.
    METHODS:
      validate_order
        IMPORTING is_order       TYPE zst_order
        RETURNING VALUE(rv_valid) TYPE abap_bool,

      calculate_total
        IMPORTING is_order       TYPE zst_order
        RETURNING VALUE(rv_total) TYPE p.
ENDCLASS.

CLASS zcl_debug_example IMPLEMENTATION.
  METHOD process_orders.
    LOOP AT it_orders INTO DATA(ls_order).
      " Static breakpoint - always stops here
      BREAK-POINT.

      " Conditional breakpoint - stops only if condition is true
      IF ls_order-order_id = '12345'.
        BREAK-POINT.
      ENDIF.

      " Dynamic breakpoint based on system field
      IF sy-uname = 'DEVELOPER'.
        BREAK-POINT.
      ENDIF.

      " Checkpoint group for conditional debugging
      " Activate in transaction SAAB
      BREAK-POINT ID zcl_debug_example.

      " Assertion - triggers debugger if condition fails
      ASSERT ls_order-customer_id IS NOT INITIAL.

      " Process order
      IF validate_order( ls_order ) = abap_true.
        DATA(lv_total) = calculate_total( ls_order ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validate_order.
    " Watch this variable in debugger
    DATA(lv_customer_exists) = abap_false.

    SELECT SINGLE @abap_true
      FROM zcustomers
      WHERE customer_id = @is_order-customer_id
      INTO @lv_customer_exists.

    rv_valid = lv_customer_exists.
  ENDMETHOD.

  METHOD calculate_total.
    DATA: lv_subtotal  TYPE p DECIMALS 2,
          lv_tax       TYPE p DECIMALS 2,
          lv_discount  TYPE p DECIMALS 2.

    " Set watchpoint on lv_subtotal to monitor changes
    lv_subtotal = is_order-amount.
    lv_tax = lv_subtotal * '0.19'.  " 19% tax
    lv_discount = lv_subtotal * '0.05'.  " 5% discount

    rv_total = lv_subtotal + lv_tax - lv_discount.
  ENDMETHOD.
ENDCLASS.

" Debugging tips:
" 1. /h in command field - Opens debugger
" 2. F5 - Step into
" 3. F6 - Step over
" 4. F7 - Return from function
" 5. F8 - Continue execution
```

### 2. Log Points (Non-Breaking Debug)

```abap
CLASS zcl_logpoint_example DEFINITION.
  PUBLIC SECTION.
    METHODS:
      process_batch
        IMPORTING it_data TYPE ztt_data.
ENDCLASS.

CLASS zcl_logpoint_example IMPLEMENTATION.
  METHOD process_batch.
    DATA: lv_start_time TYPE timestampl,
          lv_end_time   TYPE timestampl.

    GET TIME STAMP FIELD lv_start_time.

    LOOP AT it_data INTO DATA(ls_data).
      " Instead of BREAK-POINT, use log point (transaction SAAB)
      " This logs data without stopping execution

      " Log using application log
      TRY.
          DATA(lo_log) = cl_bali_log=>create_with_header(
            header = cl_bali_header_setter=>create(
              object    = 'ZDEBUG'
              subobject = 'BATCH'
            )
          ).

          DATA(lo_message) = cl_bali_message_setter=>create(
            severity = if_bali_constants=>c_severity_information
            id       = 'ZMSG'
            number   = '001'
          ).

          lo_log->add_item( lo_message ).

          " Add free text
          DATA(lo_free_text) = cl_bali_free_text_setter=>create(
            severity = if_bali_constants=>c_severity_information
            text     = |Processing record { sy-tabix }: { ls_data-id }|
          ).

          lo_log->add_item( lo_free_text ).

          " Save log
          cl_bali_log_db=>get_instance( )->save_log(
            log                = lo_log
            assign_to_current_appl_job = abap_true
          ).

        CATCH cx_bali_runtime.
          " Continue processing even if logging fails
      ENDTRY.

      " Process data
      " ...
    ENDLOOP.

    GET TIME STAMP FIELD lv_end_time.
  ENDMETHOD.
ENDCLASS.
```

### 3. External Debugging

```abap
" For debugging background jobs, RFC calls, or web services
CLASS zcl_external_debug DEFINITION.
  PUBLIC SECTION.
    METHODS:
      enable_external_debugging
        IMPORTING iv_user TYPE sy-uname.
ENDCLASS.

CLASS zcl_external_debug IMPLEMENTATION.
  METHOD enable_external_debugging.
    " Enable debugging for specific user
    " Use transaction SICF for web services
    " Use SM59 for RFC debugging

    " In code, add:
    " 1. For RFC: Check 'Enable Debugging' in SM59
    " 2. For HTTP: Set breakpoint and use /h in URL parameter

    " Alternative: Activate debugging externally
    CALL FUNCTION 'SYSTEM_RESET_SNAP'
      EXPORTING
        uname = iv_user.

    MESSAGE |External debugging enabled for { iv_user }| TYPE 'I'.
  ENDMETHOD.
ENDCLASS.
```

---

## Performance Analysis

### 1. Runtime Analysis (Transaction SAT/SE30)

```abap
CLASS zcl_performance_test DEFINITION.
  PUBLIC SECTION.
    METHODS:
      analyze_performance.

  PRIVATE SECTION.
    METHODS:
      slow_method,
      optimized_method.
ENDCLASS.

CLASS zcl_performance_test IMPLEMENTATION.
  METHOD analyze_performance.
    " Start runtime analysis programmatically
    DATA: lv_handle TYPE sytabix.

    " Enable runtime analysis
    CALL FUNCTION 'SAT_START_MEASUREMENTS'
      EXPORTING
        e_test_id = lv_handle.

    " Execute code to measure
    slow_method( ).

    " Stop runtime analysis
    CALL FUNCTION 'SAT_STOP_MEASUREMENTS'
      EXPORTING
        e_test_id = lv_handle.

    MESSAGE 'Runtime analysis completed' TYPE 'I'.
  ENDMETHOD.

  METHOD slow_method.
    " Example of inefficient code
    SELECT * FROM zorders INTO TABLE @DATA(lt_orders).

    LOOP AT lt_orders INTO DATA(ls_order).
      " Nested SELECT in loop - VERY SLOW
      SELECT SINGLE name
        FROM zcustomers
        WHERE customer_id = @ls_order-customer_id
        INTO @DATA(lv_customer_name).

      ls_order-customer_name = lv_customer_name.
      MODIFY lt_orders FROM ls_order.
    ENDLOOP.
  ENDMETHOD.

  METHOD optimized_method.
    " Optimized version
    SELECT o~*, c~name AS customer_name
      FROM zorders AS o
      INNER JOIN zcustomers AS c
        ON o~customer_id = c~customer_id
      INTO TABLE @DATA(lt_orders_optimized).

    " Single SELECT with JOIN - MUCH FASTER
  ENDMETHOD.
ENDCLASS.
```

### 2. SQL Trace (Transaction ST05)

```abap
CLASS zcl_sql_trace DEFINITION.
  PUBLIC SECTION.
    METHODS:
      analyze_sql_performance.

  PRIVATE SECTION.
    METHODS:
      execute_queries.
ENDCLASS.

CLASS zcl_sql_trace IMPLEMENTATION.
  METHOD analyze_sql_performance.
    " Enable SQL trace programmatically
    CALL FUNCTION 'TRCACTIVATE'
      EXPORTING
        trace_mode = 'SAT'.  " SQL + Buffer trace

    " Execute queries to trace
    execute_queries( ).

    " Deactivate trace
    CALL FUNCTION 'TRCDEACTIVATE'.

    MESSAGE 'SQL trace completed - Check ST05' TYPE 'I'.
  ENDMETHOD.

  METHOD execute_queries.
    " Example 1: Missing WHERE clause - Table scan
    SELECT * FROM zorders
      INTO TABLE @DATA(lt_all_orders).

    " Example 2: Optimized with WHERE clause
    SELECT * FROM zorders
      WHERE order_date >= @sy-datum
      INTO TABLE @DATA(lt_recent_orders).

    " Example 3: Using index
    SELECT * FROM zorders
      WHERE customer_id = '12345'  " Assuming index exists
      INTO TABLE @DATA(lt_customer_orders).

    " Example 4: SELECT SINGLE with key
    SELECT SINGLE * FROM zorders
      WHERE order_id = '12345'
      INTO @DATA(ls_order).
  ENDMETHOD.
ENDCLASS.
```

### 3. Performance Measurement in Code

```abap
CLASS zcl_performance_measurement DEFINITION.
  PUBLIC SECTION.
    METHODS:
      measure_execution_time.

  PRIVATE SECTION.
    METHODS:
      get_runtime
        IMPORTING iv_start        TYPE i
                  iv_end          TYPE i
        RETURNING VALUE(rv_runtime) TYPE p.
ENDCLASS.

CLASS zcl_performance_measurement IMPLEMENTATION.
  METHOD measure_execution_time.
    DATA: lv_start TYPE i,
          lv_end   TYPE i.

    " Method 1: Using GET RUN TIME
    GET RUN TIME FIELD lv_start.

    " Code to measure
    SELECT * FROM zorders
      WHERE order_date >= @sy-datum
      INTO TABLE @DATA(lt_orders).

    GET RUN TIME FIELD lv_end.

    DATA(lv_runtime) = get_runtime(
      iv_start = lv_start
      iv_end   = lv_end
    ).

    WRITE: / 'Execution time (microseconds):', lv_runtime.

    " Method 2: Using timestamps
    DATA: lv_timestamp_start TYPE timestampl,
          lv_timestamp_end   TYPE timestampl.

    GET TIME STAMP FIELD lv_timestamp_start.

    " Code to measure
    LOOP AT lt_orders INTO DATA(ls_order).
      " Processing
    ENDLOOP.

    GET TIME STAMP FIELD lv_timestamp_end.

    DATA(lv_duration) = cl_abap_tstmp=>subtract(
      tstmp1 = lv_timestamp_end
      tstmp2 = lv_timestamp_start
    ).

    WRITE: / 'Duration (seconds):', lv_duration.
  ENDMETHOD.

  METHOD get_runtime.
    rv_runtime = ( iv_end - iv_start ) / 1000000.  " Convert to seconds
  ENDMETHOD.
ENDCLASS.
```

### 4. Code Inspector (Transaction SCI)

```abap
" Best practices for Code Inspector
CLASS zcl_code_quality DEFINITION.
  PUBLIC SECTION.
    METHODS:
      good_example,
      bad_example.
ENDCLASS.

CLASS zcl_code_quality IMPLEMENTATION.
  METHOD good_example.
    " ✅ Good: Use modern ABAP syntax
    SELECT * FROM zorders
      WHERE order_date = @sy-datum
      INTO TABLE @DATA(lt_orders).

    " ✅ Good: Proper exception handling
    TRY.
        DATA(lv_result) = 10 / 0.
      CATCH cx_sy_zerodivide INTO DATA(lx_error).
        MESSAGE lx_error TYPE 'E'.
    ENDTRY.

    " ✅ Good: Use constants instead of magic numbers
    CONSTANTS: lc_tax_rate TYPE p DECIMALS 2 VALUE '0.19'.
    DATA(lv_tax) = 100 * lc_tax_rate.

    " ✅ Good: Descriptive variable names
    DATA: lv_customer_name TYPE string,
          lv_order_total   TYPE p DECIMALS 2.
  ENDMETHOD.

  METHOD bad_example.
    " ❌ Bad: Using obsolete syntax
    TABLES: zorders.  " Obsolete

    " ❌ Bad: SELECT * without INTO TABLE
    SELECT * FROM zorders.
      " Processing
    ENDSELECT.

    " ❌ Bad: No exception handling
    DATA(lv_result) = 10 / 0.

    " ❌ Bad: Magic numbers
    DATA(lv_tax) = 100 * '0.19'.

    " ❌ Bad: Cryptic variable names
    DATA: n TYPE string,
          t TYPE p.
  ENDMETHOD.
ENDCLASS.
```

---

## Error Analysis and Tracing

### 1. Application Log (Transaction SLG1)

```abap
CLASS zcl_application_log DEFINITION.
  PUBLIC SECTION.
    METHODS:
      create_application_log
        IMPORTING iv_object    TYPE balobj_d
                  iv_subobject TYPE balsubobj
        RAISING   cx_bali_runtime.

  PRIVATE SECTION.
    DATA: mo_log TYPE REF TO if_bali_log.
ENDCLASS.

CLASS zcl_application_log IMPLEMENTATION.
  METHOD create_application_log.
    " Create log with header
    mo_log = cl_bali_log=>create_with_header(
      header = cl_bali_header_setter=>create(
        object    = iv_object
        subobject = iv_subobject
        external_id = |{ sy-datum }{ sy-uzeit }|
      )
    ).

    " Add information message
    DATA(lo_info_msg) = cl_bali_message_setter=>create(
      severity = if_bali_constants=>c_severity_information
      id       = 'ZMSG'
      number   = '001'
    ).
    mo_log->add_item( lo_info_msg ).

    " Add warning
    DATA(lo_warning) = cl_bali_message_setter=>create(
      severity = if_bali_constants=>c_severity_warning
      id       = 'ZMSG'
      number   = '002'
    ).
    mo_log->add_item( lo_warning ).

    " Add error with exception
    TRY.
        " Some operation that might fail
        DATA(lv_result) = 10 / 0.

      CATCH cx_sy_zerodivide INTO DATA(lx_error).
        DATA(lo_exception) = cl_bali_exception_setter=>create(
          severity  = if_bali_constants=>c_severity_error
          exception = lx_error
        ).
        mo_log->add_item( lo_exception ).
    ENDTRY.

    " Add free text
    DATA(lo_free_text) = cl_bali_free_text_setter=>create(
      severity = if_bali_constants=>c_severity_information
      text     = 'Processing completed with warnings'
    ).
    mo_log->add_item( lo_free_text ).

    " Save log to database
    cl_bali_log_db=>get_instance( )->save_log(
      log = mo_log
      assign_to_current_appl_job = abap_true
    ).

    " Get log handle for display
    DATA(lv_log_handle) = mo_log->get_handle( ).

    " Display log (can also be viewed in SLG1)
    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_log = lv_log_handle.
  ENDMETHOD.
ENDCLASS.
```

### 2. Custom Trace Logging

```abap
CLASS zcl_trace_logger DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor,

      trace
        IMPORTING iv_level   TYPE i DEFAULT 3
                  iv_message TYPE string,

      get_trace_log
        RETURNING VALUE(rt_log) TYPE ztt_trace_log.

  PRIVATE SECTION.
    CLASS-DATA:
      mt_trace_log   TYPE ztt_trace_log,
      mv_trace_level TYPE i.
ENDCLASS.

CLASS zcl_trace_logger IMPLEMENTATION.
  METHOD class_constructor.
    " Initialize trace level from customizing or parameter
    mv_trace_level = 3.  " 0=OFF, 1=ERROR, 2=WARNING, 3=INFO, 4=DEBUG
  ENDMETHOD.

  METHOD trace.
    " Only log if level is enabled
    IF iv_level > mv_trace_level.
      RETURN.
    ENDIF.

    " Create trace entry
    DATA(ls_trace) = VALUE zst_trace_log(
      timestamp = |{ sy-datum } { sy-uzeit }|
      level     = SWITCH #( iv_level
                    WHEN 1 THEN 'ERROR'
                    WHEN 2 THEN 'WARNING'
                    WHEN 3 THEN 'INFO'
                    WHEN 4 THEN 'DEBUG'
                    ELSE 'UNKNOWN' )
      message   = iv_message
      program   = sy-repid
      user      = sy-uname
    ).

    APPEND ls_trace TO mt_trace_log.

    " Optionally write to database
    INSERT ztrace_log FROM ls_trace.
  ENDMETHOD.

  METHOD get_trace_log.
    rt_log = mt_trace_log.
  ENDMETHOD.
ENDCLASS.

" Usage:
zcl_trace_logger=>trace(
  iv_level   = 3
  iv_message = 'Starting order processing'
).

zcl_trace_logger=>trace(
  iv_level   = 1
  iv_message = |Error processing order { lv_order_id }|
).
```

### 3. System Log Analysis (Transaction SM21)

```abap
CLASS zcl_system_log DEFINITION.
  PUBLIC SECTION.
    METHODS:
      read_system_log
        IMPORTING iv_from_date TYPE sy-datum DEFAULT sy-datum
                  iv_to_date   TYPE sy-datum DEFAULT sy-datum
        RETURNING VALUE(rt_log) TYPE STANDARD TABLE.
ENDCLASS.

CLASS zcl_system_log IMPLEMENTATION.
  METHOD read_system_log.
    " Read system log entries
    CALL FUNCTION 'TH_READ_SYSLOG'
      EXPORTING
        from_date       = iv_from_date
        to_date         = iv_to_date
      TABLES
        syslog_entries  = rt_log
      EXCEPTIONS
        no_logs_found   = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      MESSAGE 'Error reading system log' TYPE 'I'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

---

## Memory Management

### 1. Memory Analysis (Transaction ST02, S_MEMORY_INSPECTOR)

```abap
CLASS zcl_memory_management DEFINITION.
  PUBLIC SECTION.
    METHODS:
      analyze_memory_usage.

  PRIVATE SECTION.
    METHODS:
      memory_efficient_code,
      memory_inefficient_code.
ENDCLASS.

CLASS zcl_memory_management IMPLEMENTATION.
  METHOD analyze_memory_usage.
    " Get current memory consumption
    DATA: lv_heap_size TYPE i,
          lv_used_heap TYPE i.

    CALL FUNCTION 'SYSTEM_GET_MEMORY_SIZE'
      IMPORTING
        current_size = lv_heap_size
        used_size    = lv_used_heap.

    WRITE: / 'Total heap size:', lv_heap_size,
           / 'Used heap:', lv_used_heap.

    " Demonstrate memory-efficient vs inefficient code
    memory_efficient_code( ).
  ENDMETHOD.

  METHOD memory_efficient_code.
    " ✅ Good: Use package size for large selects
    SELECT * FROM zorders
      PACKAGE SIZE 1000
      INTO TABLE @DATA(lt_package).

      " Process package
      " ...

      " Clear package to free memory
      CLEAR lt_package.
    ENDSELECT.

    " ✅ Good: Use FREE to release memory immediately
    DATA: lt_large_table TYPE TABLE OF zorders.
    SELECT * FROM zorders INTO TABLE lt_large_table.

    " Process data
    " ...

    " Release memory
    FREE lt_large_table.

    " ✅ Good: Use LOOP without INTO for read-only operations
    SELECT * FROM zorders INTO TABLE @DATA(lt_orders).

    LOOP AT lt_orders REFERENCE INTO DATA(lr_order).
      " Read-only access using reference - no copying
      WRITE: / lr_order->order_id.
    ENDLOOP.
  ENDMETHOD.

  METHOD memory_inefficient_code.
    " ❌ Bad: Loading entire table into memory
    SELECT * FROM zorders INTO TABLE @DATA(lt_all_orders).

    " ❌ Bad: Copying in loop
    LOOP AT lt_all_orders INTO DATA(ls_order).
      " Creates a copy of each row
    ENDLOOP.

    " ❌ Bad: Not freeing memory after use
    DATA: lt_temp_table TYPE TABLE OF zorders.
    SELECT * FROM zorders INTO TABLE lt_temp_table.
    " ... processing ...
    " Memory not released until end of program
  ENDMETHOD.
ENDCLASS.
```

### 2. Garbage Collection and Memory Leaks

```abap
CLASS zcl_memory_leak_example DEFINITION.
  PUBLIC SECTION.
    METHODS:
      demonstrate_memory_leak,
      demonstrate_proper_cleanup.

  PRIVATE SECTION.
    DATA: mt_objects TYPE TABLE OF REF TO zcl_heavy_object.
ENDCLASS.

CLASS zcl_memory_leak_example IMPLEMENTATION.
  METHOD demonstrate_memory_leak.
    " ❌ Bad: Objects accumulate without cleanup
    DO 1000 TIMES.
      DATA(lo_object) = NEW zcl_heavy_object( ).
      APPEND lo_object TO mt_objects.

      " Objects never released - memory leak!
    ENDDO.
  ENDMETHOD.

  METHOD demonstrate_proper_cleanup.
    " ✅ Good: Explicit cleanup
    DATA: lt_objects TYPE TABLE OF REF TO zcl_heavy_object.

    DO 1000 TIMES.
      DATA(lo_object) = NEW zcl_heavy_object( ).
      APPEND lo_object TO lt_objects.
    ENDDO.

    " Process objects
    " ...

    " Clean up
    CLEAR lt_objects.      " Release references
    FREE lt_objects.       " Free memory

    " Alternatively, let objects go out of scope
    " (automatic cleanup at method end)
  ENDMETHOD.
ENDCLASS.
```

---

## Troubleshooting Common Issues

### 1. Database Lock Issues (Transaction SM12)

```abap
CLASS zcl_lock_management DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_locks_properly
        IMPORTING iv_order_id TYPE zorder_id
        RAISING   zcx_lock_error.

  PRIVATE SECTION.
    METHODS:
      enqueue_order
        IMPORTING iv_order_id TYPE zorder_id
        RAISING   zcx_lock_error,

      dequeue_order
        IMPORTING iv_order_id TYPE zorder_id.
ENDCLASS.

CLASS zcl_lock_management IMPLEMENTATION.
  METHOD handle_locks_properly.
    TRY.
        " Acquire lock
        enqueue_order( iv_order_id ).

        " Critical section - modify order
        UPDATE zorders
          SET status = 'PROCESSING'
          WHERE order_id = iv_order_id.

        COMMIT WORK.

      CATCH zcx_lock_error INTO DATA(lx_lock).
        ROLLBACK WORK.
        RAISE EXCEPTION lx_lock.

      CLEANUP.
        " Always release lock, even if error occurs
        dequeue_order( iv_order_id ).
    ENDTRY.

    " Explicit unlock
    dequeue_order( iv_order_id ).
  ENDMETHOD.

  METHOD enqueue_order.
    " Lock object function module (created in SE11)
    CALL FUNCTION 'ENQUEUE_EZORDER'
      EXPORTING
        mode_zorders   = 'E'  " Exclusive lock
        mandt          = sy-mandt
        order_id       = iv_order_id
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_lock_error
        EXPORTING
          order_id = iv_order_id.
    ENDIF.
  ENDMETHOD.

  METHOD dequeue_order.
    CALL FUNCTION 'DEQUEUE_EZORDER'
      EXPORTING
        mode_zorders = 'E'
        mandt        = sy-mandt
        order_id     = iv_order_id.
  ENDMETHOD.
ENDCLASS.
```

### 2. Short Dump Analysis (Transaction ST22)

```abap
CLASS zcl_dump_prevention DEFINITION.
  PUBLIC SECTION.
    METHODS:
      prevent_common_dumps.
ENDCLASS.

CLASS zcl_dump_prevention IMPLEMENTATION.
  METHOD prevent_common_dumps.
    " ✅ Prevent ITAB_LINE_NOT_FOUND
    TRY.
        DATA(ls_customer) = lt_customers[ customer_id = '12345' ].
      CATCH cx_sy_itab_line_not_found.
        " Handle missing entry
    ENDTRY.

    " Or use READ TABLE
    READ TABLE lt_customers INTO DATA(ls_cust)
      WITH KEY customer_id = '12345'.
    IF sy-subrc = 0.
      " Found
    ENDIF.

    " ✅ Prevent ZERODIVIDE
    IF lv_divisor <> 0.
      DATA(lv_result) = lv_dividend / lv_divisor.
    ELSE.
      " Handle zero division
    ENDIF.

    " ✅ Prevent MOVE_TO_LIT_NOTALLOWED
    DATA: lv_value TYPE string.
    " Don't assign to literal
    " 'ABC' = lv_value.  " ❌ Wrong
    lv_value = 'ABC'.    " ✅ Correct

    " ✅ Prevent CONVT_NO_NUMBER
    TRY.
        DATA(lv_number) = CONV i( '123ABC' ).
      CATCH cx_sy_conversion_no_number.
        MESSAGE 'Invalid number format' TYPE 'E'.
    ENDTRY.

    " ✅ Prevent STRING_OFFSET_TOO_LARGE
    DATA(lv_string) = 'Hello'.
    IF strlen( lv_string ) > 10.
      DATA(lv_char) = lv_string+10(1).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

### 3. RFC Connection Issues (Transaction SM59)

```abap
CLASS zcl_rfc_troubleshooting DEFINITION.
  PUBLIC SECTION.
    METHODS:
      test_rfc_connection
        IMPORTING iv_destination TYPE rfcdest
        RETURNING VALUE(rv_success) TYPE abap_bool.

  PRIVATE SECTION.
    METHODS:
      handle_rfc_exceptions
        IMPORTING iv_destination TYPE rfcdest.
ENDCLASS.

CLASS zcl_rfc_troubleshooting IMPLEMENTATION.
  METHOD test_rfc_connection.
    " Test RFC connection
    CALL FUNCTION 'RFC_PING'
      DESTINATION iv_destination
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.

    rv_success = COND #( WHEN sy-subrc = 0 THEN abap_true
                         ELSE abap_false ).
  ENDMETHOD.

  METHOD handle_rfc_exceptions.
    TRY.
        CALL FUNCTION 'Z_REMOTE_FUNCTION'
          DESTINATION iv_destination
          EXPORTING
            iv_param = 'value'
          EXCEPTIONS
            communication_failure = 1 MESSAGE lv_msg
            system_failure        = 2 MESSAGE lv_msg
            OTHERS                = 3.

        CASE sy-subrc.
          WHEN 1.
            " Network or connection issue
            MESSAGE |Communication failure: { lv_msg }| TYPE 'E'.

          WHEN 2.
            " Remote system error
            MESSAGE |System failure: { lv_msg }| TYPE 'E'.

          WHEN 3.
            " Function module error
            MESSAGE 'RFC call failed' TYPE 'E'.
        ENDCASE.

      CATCH cx_root INTO DATA(lx_error).
        MESSAGE lx_error->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
```

### 4. Common Performance Issues

```abap
CLASS zcl_performance_issues DEFINITION.
  PUBLIC SECTION.
    METHODS:
      fix_performance_issues.
ENDCLASS.

CLASS zcl_performance_issues IMPLEMENTATION.
  METHOD fix_performance_issues.
    " ❌ Issue 1: SELECT in LOOP
    " Bad:
    SELECT * FROM zorders INTO TABLE @DATA(lt_orders).
    LOOP AT lt_orders INTO DATA(ls_order).
      SELECT SINGLE name FROM zcustomers
        WHERE customer_id = @ls_order-customer_id
        INTO @ls_order-customer_name.
      MODIFY lt_orders FROM ls_order.
    ENDLOOP.

    " ✅ Fix: Use JOIN or FOR ALL ENTRIES
    SELECT o~*, c~name AS customer_name
      FROM zorders AS o
      LEFT OUTER JOIN zcustomers AS c
        ON o~customer_id = c~customer_id
      INTO TABLE @DATA(lt_orders_fixed).

    " ❌ Issue 2: No WHERE clause
    " Bad:
    SELECT * FROM zorders INTO TABLE @DATA(lt_all).

    " ✅ Fix: Always use WHERE clause
    SELECT * FROM zorders
      WHERE order_date >= @lv_from_date
      INTO TABLE @DATA(lt_filtered).

    " ❌ Issue 3: Not using buffering
    " Bad: Repeated reads from database
    DO 100 TIMES.
      SELECT SINGLE * FROM zt000
        WHERE mandt = @sy-mandt
        INTO @DATA(ls_t000).
    ENDDO.

    " ✅ Fix: Read once and buffer
    SELECT SINGLE * FROM zt000
      WHERE mandt = @sy-mandt
      INTO @DATA(ls_t000_buffered).
    " Subsequent reads use buffer

    " ❌ Issue 4: Unnecessary sorting
    " Bad:
    SELECT * FROM zorders INTO TABLE @DATA(lt_unsorted).
    SORT lt_unsorted BY order_date DESCENDING.

    " ✅ Fix: Use ORDER BY in SELECT
    SELECT * FROM zorders
      ORDER BY order_date DESCENDING
      INTO TABLE @DATA(lt_sorted).
  ENDMETHOD.
ENDCLASS.
```

---

## Best Practices Summary

### Debugging Checklist
- ✅ Use conditional breakpoints to avoid stopping unnecessarily
- ✅ Leverage watchpoints for variable monitoring
- ✅ Use log points for non-breaking debugging in production
- ✅ Enable checkpoint groups for team debugging
- ✅ Document debugging sessions for knowledge sharing

### Performance Checklist
- ✅ Run SAT (runtime analysis) before and after optimization
- ✅ Use ST05 (SQL trace) to identify slow queries
- ✅ Check ST02 for memory bottlenecks
- ✅ Run Code Inspector (SCI) regularly
- ✅ Set performance budgets (e.g., queries < 100ms)

### Error Handling Checklist
- ✅ Always use TRY-CATCH for risky operations
- ✅ Log errors to application log (SLG1)
- ✅ Provide meaningful error messages
- ✅ Use CLEANUP blocks for resource management
- ✅ Monitor ST22 for runtime errors

---

**Last Updated:** December 2025

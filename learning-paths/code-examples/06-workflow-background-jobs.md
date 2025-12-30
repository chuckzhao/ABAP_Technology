# Workflow, Background Jobs, and Batch Processing

This guide covers workflow development, background job scheduling, and batch processing patterns in ABAP.

## Table of Contents
- [Workflow Development](#workflow-development)
- [Background Job Programming](#background-job-programming)
- [Batch Processing](#batch-processing)
- [Job Monitoring and Error Handling](#job-monitoring-and-error-handling)
- [Best Practices](#best-practices)

---

## Workflow Development

### 1. Simple Workflow with Events

```abap
" Workflow event raising from ABAP program
CLASS zcl_order_processor DEFINITION.
  PUBLIC SECTION.
    METHODS:
      create_order
        IMPORTING is_order TYPE zst_order
        RAISING   zcx_order_error.

  PRIVATE SECTION.
    METHODS:
      raise_workflow_event
        IMPORTING iv_event_id TYPE swo_event
                  iv_order_id TYPE zorder_id.
ENDCLASS.

CLASS zcl_order_processor IMPLEMENTATION.
  METHOD create_order.
    " Insert order into database
    INSERT zorders FROM is_order.

    IF sy-subrc = 0.
      COMMIT WORK.

      " Raise workflow event for approval
      raise_workflow_event(
        iv_event_id = 'CREATED'
        iv_order_id = is_order-order_id
      ).
    ELSE.
      ROLLBACK WORK.
      RAISE EXCEPTION TYPE zcx_order_error.
    ENDIF.
  ENDMETHOD.

  METHOD raise_workflow_event.
    " Create business object key
    DATA: lv_objkey TYPE swo_typeid.
    lv_objkey = iv_order_id.

    " Raise event
    CALL FUNCTION 'SWE_EVENT_CREATE'
      EXPORTING
        objtype           = 'ZORDER'
        objkey            = lv_objkey
        event             = iv_event_id
      EXCEPTIONS
        objtype_not_found = 1
        OTHERS            = 2.

    IF sy-subrc <> 0.
      " Log error but don't fail the transaction
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

### 2. Business Object (BOR) Definition

```abap
" Business Object Type definition (in transaction SWO1)
" Object Type: ZORDER
" Object Name: Order

" Key definition
BEGIN_OBJECT ZORDER.
  KEY order_id TYPE zorder_id.

  " Attributes
  PROPERTY customer_id TYPE kunnr READ-ONLY.
  PROPERTY total_amount TYPE p DECIMALS 2 READ-ONLY.
  PROPERTY status TYPE zorder_status READ-ONLY.

  " Methods
  METHOD approve.
    " Approval logic
    UPDATE zorders
      SET status = 'APPROVED'
      WHERE order_id = order_id.
    COMMIT WORK.
  ENDMETHOD.

  METHOD reject.
    " Rejection logic
    UPDATE zorders
      SET status = 'REJECTED'
      WHERE order_id = order_id.
    COMMIT WORK.
  ENDMETHOD.

  " Events
  EVENT created.
  EVENT approved.
  EVENT rejected.

END_OBJECT.
```

### 3. Workflow Task Implementation

```abap
" Workflow task method for approval decision
CLASS zcl_wf_order_approval DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_swf_ifs_workitem_exit.

    METHODS:
      execute_approval_task
        IMPORTING iv_workitem_id TYPE sww_wiid
                  iv_order_id    TYPE zorder_id
        EXPORTING ev_result      TYPE sww_wiresult.
ENDCLASS.

CLASS zcl_wf_order_approval IMPLEMENTATION.
  METHOD if_swf_ifs_workitem_exit~event_raised.
    " Handle workflow event
    CASE event.
      WHEN 'APPROVE'.
        " Approval logic
        DATA(lo_processor) = NEW zcl_order_processor( ).
        " ... approval processing ...

      WHEN 'REJECT'.
        " Rejection logic
        " ... rejection processing ...
    ENDCASE.
  ENDMETHOD.

  METHOD execute_approval_task.
    " Get order details
    SELECT SINGLE *
      FROM zorders
      WHERE order_id = @iv_order_id
      INTO @DATA(ls_order).

    IF sy-subrc = 0.
      " Check approval criteria
      IF ls_order-total_amount < 10000.
        " Auto-approve small orders
        ev_result = 'APPROVED'.
      ELSE.
        " Manual approval required
        ev_result = 'PENDING'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

### 4. Start Workflow from ABAP

```abap
CLASS zcl_workflow_starter DEFINITION.
  PUBLIC SECTION.
    METHODS:
      start_approval_workflow
        IMPORTING iv_order_id TYPE zorder_id
        RAISING   zcx_workflow_error.
ENDCLASS.

CLASS zcl_workflow_starter IMPLEMENTATION.
  METHOD start_approval_workflow.
    DATA: lv_workitem_id TYPE sww_wiid.

    " Prepare workflow container
    DATA(lt_container) = VALUE swcont(
      ( element = 'OrderId'
        value   = |{ iv_order_id }| )
      ( element = 'Approver'
        value   = sy-uname )
    ).

    " Start workflow
    CALL FUNCTION 'SAP_WAPI_START_WORKFLOW'
      EXPORTING
        task              = 'WS99900023'  " Workflow task ID
      TABLES
        input_container   = lt_container
      IMPORTING
        workitem_id       = lv_workitem_id
      EXCEPTIONS
        invalid_task      = 1
        workflow_not_found = 2
        OTHERS            = 3.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_workflow_error
        EXPORTING
          textid = zcx_workflow_error=>start_failed
          task_id = 'WS99900023'.
    ELSE.
      " Workflow started successfully
      MESSAGE |Workflow { lv_workitem_id } started| TYPE 'S'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

---

## Background Job Programming

### 1. Simple Background Job Scheduling

```abap
CLASS zcl_job_scheduler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      schedule_daily_report
        IMPORTING iv_start_date TYPE sy-datum
                  iv_start_time TYPE sy-uzeit
        RAISING   zcx_job_error.
ENDCLASS.

CLASS zcl_job_scheduler IMPLEMENTATION.
  METHOD schedule_daily_report.
    DATA: lv_jobname  TYPE tbtcjob-jobname,
          lv_jobcount TYPE tbtcjob-jobcount.

    lv_jobname = 'ZDAILY_SALES_REPORT'.

    " Open job
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = lv_jobname
      IMPORTING
        jobcount         = lv_jobcount
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_job_error
        EXPORTING
          textid = zcx_job_error=>job_open_failed.
    ENDIF.

    " Submit program to job
    SUBMIT zreport_daily_sales
      WITH s_date IN s_date_range
      WITH p_email = abap_true
      USER sy-uname
      VIA JOB lv_jobname NUMBER lv_jobcount
      AND RETURN.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_job_error
        EXPORTING
          textid = zcx_job_error=>submit_failed.
    ENDIF.

    " Schedule job
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = lv_jobcount
        jobname              = lv_jobname
        strtimmed            = abap_true
        sdlstrtdt            = iv_start_date
        sdlstrttm            = iv_start_time
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        OTHERS               = 5.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_job_error
        EXPORTING
          textid = zcx_job_error=>job_close_failed.
    ELSE.
      MESSAGE |Job { lv_jobname } scheduled successfully| TYPE 'S'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

### 2. Periodic Job Scheduling

```abap
CLASS zcl_periodic_job DEFINITION.
  PUBLIC SECTION.
    METHODS:
      schedule_monthly_job
        IMPORTING iv_program_name TYPE sy-repid
        RAISING   zcx_job_error.
ENDCLASS.

CLASS zcl_periodic_job IMPLEMENTATION.
  METHOD schedule_monthly_job.
    DATA: lv_jobname  TYPE tbtcjob-jobname,
          lv_jobcount TYPE tbtcjob-jobcount,
          lv_period   TYPE tbtcjob-period.

    lv_jobname = 'ZMONTHLY_AGGREGATION'.

    " Open job
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname  = lv_jobname
      IMPORTING
        jobcount = lv_jobcount.

    " Submit program
    SUBMIT (iv_program_name)
      USER sy-uname
      VIA JOB lv_jobname NUMBER lv_jobcount
      AND RETURN.

    " Create period (monthly on first day)
    CALL FUNCTION 'BP_CREATE_PERIOD'
      EXPORTING
        period_unit      = 'M'    " Month
        period_value     = 1      " Every 1 month
      IMPORTING
        period           = lv_period
      EXCEPTIONS
        invalid_input    = 1
        OTHERS           = 2.

    " Schedule with period
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount          = lv_jobcount
        jobname           = lv_jobname
        sdlstrtdt         = sy-datum
        sdlstrttm         = '010000'  " 01:00 AM
        periodic_jobs     = abap_true
        period            = lv_period
      EXCEPTIONS
        job_close_failed  = 1
        OTHERS            = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_job_error.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

### 3. Job with Variant

```abap
CLASS zcl_job_with_variant DEFINITION.
  PUBLIC SECTION.
    METHODS:
      schedule_report_with_variant
        IMPORTING iv_variant TYPE raldb-variant
        RAISING   zcx_job_error.
ENDCLASS.

CLASS zcl_job_with_variant IMPLEMENTATION.
  METHOD schedule_report_with_variant.
    DATA: lv_jobname  TYPE tbtcjob-jobname,
          lv_jobcount TYPE tbtcjob-jobcount.

    lv_jobname = 'ZSALES_ANALYSIS'.

    " Open job
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname  = lv_jobname
      IMPORTING
        jobcount = lv_jobcount.

    " Submit with variant
    SUBMIT zsales_analysis_report
      USING SELECTION-SET iv_variant
      USER sy-uname
      VIA JOB lv_jobname NUMBER lv_jobcount
      AND RETURN.

    " Close and start immediately
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = lv_jobcount
        jobname              = lv_jobname
        strtimmed            = abap_true
      EXCEPTIONS
        cant_start_immediate = 1
        OTHERS               = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_job_error.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

### 4. Event-Based Job Scheduling

```abap
CLASS zcl_event_based_job DEFINITION.
  PUBLIC SECTION.
    METHODS:
      schedule_on_event
        IMPORTING iv_event_id   TYPE tbtcm-eventid
                  iv_event_param TYPE tbtcm-eventparm
        RAISING   zcx_job_error.
ENDCLASS.

CLASS zcl_event_based_job IMPLEMENTATION.
  METHOD schedule_on_event.
    DATA: lv_jobname  TYPE tbtcjob-jobname,
          lv_jobcount TYPE tbtcjob-jobcount.

    lv_jobname = 'ZORDER_POST_PROCESSING'.

    " Open job
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname  = lv_jobname
      IMPORTING
        jobcount = lv_jobcount.

    " Submit program
    SUBMIT zorder_post_process
      USER sy-uname
      VIA JOB lv_jobname NUMBER lv_jobcount
      AND RETURN.

    " Schedule based on event
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount          = lv_jobcount
        jobname           = lv_jobname
        event_id          = iv_event_id
        event_param       = iv_event_param
      EXCEPTIONS
        job_close_failed  = 1
        OTHERS            = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_job_error.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" Raising an event to trigger the job
CLASS zcl_event_raiser DEFINITION.
  PUBLIC SECTION.
    METHODS:
      raise_event
        IMPORTING iv_event_id   TYPE tbtcm-eventid
                  iv_event_param TYPE tbtcm-eventparm.
ENDCLASS.

CLASS zcl_event_raiser IMPLEMENTATION.
  METHOD raise_event.
    CALL FUNCTION 'BP_EVENT_RAISE'
      EXPORTING
        eventid               = iv_event_id
        eventparm             = iv_event_param
      EXCEPTIONS
        bad_eventid           = 1
        eventid_does_not_exist = 2
        OTHERS                = 3.

    IF sy-subrc = 0.
      MESSAGE |Event { iv_event_id } raised| TYPE 'S'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

---

## Batch Processing

### 1. Parallel Processing with Package Size

```abap
CLASS zcl_batch_processor DEFINITION.
  PUBLIC SECTION.
    METHODS:
      process_large_dataset
        IMPORTING iv_package_size TYPE i DEFAULT 1000
        RAISING   zcx_processing_error.

  PRIVATE SECTION.
    METHODS:
      process_package
        IMPORTING it_package TYPE ztt_order_data
        RAISING   zcx_processing_error.
ENDCLASS.

CLASS zcl_batch_processor IMPLEMENTATION.
  METHOD process_large_dataset.
    DATA: lt_package TYPE ztt_order_data,
          lv_counter TYPE i.

    " Select data in packages
    SELECT *
      FROM zorders
      WHERE status = 'NEW'
      INTO TABLE @DATA(lt_orders)
      PACKAGE SIZE @iv_package_size.

      " Process each package
      TRY.
          process_package( lt_orders ).

          " Commit after each package
          COMMIT WORK.

          " Progress indicator
          lv_counter = lv_counter + lines( lt_orders ).
          cl_progress_indicator=>progress_indicate(
            i_text               = |Processing: { lv_counter } records|
            i_processed          = lv_counter
            i_total              = lv_counter
            i_output_immediately = abap_true
          ).

        CATCH zcx_processing_error INTO DATA(lx_error).
          " Rollback package on error
          ROLLBACK WORK.

          " Log error and continue with next package
          " ... error logging ...
      ENDTRY.

    ENDSELECT.
  ENDMETHOD.

  METHOD process_package.
    " Process each record in package
    LOOP AT it_package INTO DATA(ls_order).
      " Business logic here
      ls_order-status = 'PROCESSED'.
      ls_order-processed_date = sy-datum.
      ls_order-processed_time = sy-uzeit.

      MODIFY zorders FROM ls_order.
    ENDLOOP.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_processing_error.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

### 2. Parallel Processing Using RFC

```abap
" Server program (executed in parallel)
REPORT zprocess_orders_parallel.

PARAMETERS: p_order TYPE zorder_id.

START-OF-SELECTION.
  " Process single order
  DATA(lo_processor) = NEW zcl_order_processor( ).

  TRY.
      lo_processor->process_order( p_order ).
    CATCH zcx_processing_error INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.

" Parallel task manager
CLASS zcl_parallel_processor DEFINITION.
  PUBLIC SECTION.
    METHODS:
      process_orders_parallel
        IMPORTING it_orders TYPE ztt_order_id
        RAISING   zcx_processing_error.

  PRIVATE SECTION.
    DATA:
      mv_max_tasks     TYPE i VALUE 10,
      mv_running_tasks TYPE i,
      mt_results       TYPE TABLE OF zst_task_result.

    METHODS:
      start_task
        IMPORTING iv_order_id TYPE zorder_id
        RAISING   zcx_task_error,

      handle_task_complete
        IMPORTING iv_taskname TYPE string.
ENDCLASS.

CLASS zcl_parallel_processor IMPLEMENTATION.
  METHOD process_orders_parallel.
    DATA: lv_taskname TYPE string.

    LOOP AT it_orders INTO DATA(lv_order_id).
      " Wait if too many tasks running
      WHILE mv_running_tasks >= mv_max_tasks.
        WAIT UP TO 1 SECONDS.
      ENDWHILE.

      " Start new task
      TRY.
          start_task( lv_order_id ).
        CATCH zcx_task_error INTO DATA(lx_error).
          " Handle error
      ENDTRY.
    ENDLOOP.

    " Wait for all tasks to complete
    WAIT UNTIL mv_running_tasks = 0.
  ENDMETHOD.

  METHOD start_task.
    DATA: lv_taskname TYPE string.

    " Generate unique task name
    lv_taskname = |TASK_{ iv_order_id }_{ sy-datum }{ sy-uzeit }|.

    " Start asynchronous RFC call
    CALL FUNCTION 'ZPROCESS_ORDER_RFC'
      STARTING NEW TASK lv_taskname
      DESTINATION IN GROUP 'parallel_group'
      CALLING handle_task_complete ON END OF TASK
      EXPORTING
        iv_order_id           = iv_order_id
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        resource_failure      = 3.

    IF sy-subrc = 0.
      mv_running_tasks = mv_running_tasks + 1.
    ELSE.
      RAISE EXCEPTION TYPE zcx_task_error
        EXPORTING
          order_id = iv_order_id.
    ENDIF.
  ENDMETHOD.

  METHOD handle_task_complete.
    DATA: lv_result TYPE zst_task_result.

    " Receive results
    RECEIVE RESULTS FROM FUNCTION 'ZPROCESS_ORDER_RFC'
      IMPORTING
        ev_success = lv_result-success
        ev_message = lv_result-message.

    " Store results
    APPEND lv_result TO mt_results.

    " Decrease running task counter
    mv_running_tasks = mv_running_tasks - 1.
  ENDMETHOD.
ENDCLASS.
```

### 3. Batch Input Processing (BDC)

```abap
CLASS zcl_bdc_processor DEFINITION.
  PUBLIC SECTION.
    METHODS:
      create_customers_batch
        IMPORTING it_customers TYPE ztt_customer_data
        RAISING   zcx_bdc_error.

  PRIVATE SECTION.
    DATA: mt_bdcdata TYPE TABLE OF bdcdata.

    METHODS:
      add_bdc_field
        IMPORTING iv_program TYPE bdcdata-program
                  iv_dynpro  TYPE bdcdata-dynpro
                  iv_dynbegin TYPE bdcdata-dynbegin
                  iv_fnam    TYPE bdcdata-fnam
                  iv_fval    TYPE bdcdata-fval.
ENDCLASS.

CLASS zcl_bdc_processor IMPLEMENTATION.
  METHOD create_customers_batch.
    DATA: lt_messages TYPE TABLE OF bdcmsgcoll.

    LOOP AT it_customers INTO DATA(ls_customer).
      CLEAR mt_bdcdata.

      " Build BDC data for XD01 transaction
      add_bdc_field(
        iv_program  = 'SAPMF02D'
        iv_dynpro   = '0100'
        iv_dynbegin = 'X'
        iv_fnam     = ''
        iv_fval     = ''
      ).

      add_bdc_field(
        iv_program  = ''
        iv_dynpro   = ''
        iv_dynbegin = ''
        iv_fnam     = 'RF02D-KUNNR'
        iv_fval     = ls_customer-customer_id
      ).

      add_bdc_field(
        iv_program  = ''
        iv_dynpro   = ''
        iv_dynbegin = ''
        iv_fnam     = 'RF02D-NAME1'
        iv_fval     = ls_customer-name
      ).

      " Execute transaction
      CALL TRANSACTION 'XD01'
        USING mt_bdcdata
        MODE 'N'    " Background mode
        UPDATE 'S'  " Synchronous update
        MESSAGES INTO lt_messages.

      IF sy-subrc <> 0.
        " Handle errors
        LOOP AT lt_messages INTO DATA(ls_message)
          WHERE msgtyp = 'E' OR msgtyp = 'A'.

          " Log error
          " ...
        ENDLOOP.
      ENDIF.

      " Commit after each customer
      COMMIT WORK.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_bdc_field.
    APPEND VALUE #(
      program  = iv_program
      dynpro   = iv_dynpro
      dynbegin = iv_dynbegin
      fnam     = iv_fnam
      fval     = iv_fval
    ) TO mt_bdcdata.
  ENDMETHOD.
ENDCLASS.
```

---

## Job Monitoring and Error Handling

### 1. Job Status Monitoring

```abap
CLASS zcl_job_monitor DEFINITION.
  PUBLIC SECTION.
    METHODS:
      get_job_status
        IMPORTING iv_jobname       TYPE tbtcjob-jobname
                  iv_jobcount      TYPE tbtcjob-jobcount
        RETURNING VALUE(rv_status) TYPE tbtcjob-status
        RAISING   zcx_job_error,

      get_job_log
        IMPORTING iv_jobname    TYPE tbtcjob-jobname
                  iv_jobcount   TYPE tbtcjob-jobcount
        RETURNING VALUE(rt_log) TYPE TABLE OF tbtcp
        RAISING   zcx_job_error.
ENDCLASS.

CLASS zcl_job_monitor IMPLEMENTATION.
  METHOD get_job_status.
    SELECT SINGLE status
      FROM tbtco
      WHERE jobname  = @iv_jobname
        AND jobcount = @iv_jobcount
      INTO @rv_status.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_job_error
        EXPORTING
          textid   = zcx_job_error=>job_not_found
          job_name = iv_jobname.
    ENDIF.

    " Status values:
    " R - Running
    " F - Finished
    " A - Aborted
    " S - Scheduled
    " Y - Ready
  ENDMETHOD.

  METHOD get_job_log.
    SELECT *
      FROM tbtcp
      WHERE jobname  = @iv_jobname
        AND jobcount = @iv_jobcount
      INTO TABLE @rt_log.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_job_error
        EXPORTING
          textid   = zcx_job_error=>no_log_found
          job_name = iv_jobname.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

### 2. Error Handling and Logging

```abap
CLASS zcl_job_error_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      log_job_error
        IMPORTING iv_jobname  TYPE tbtcjob-jobname
                  iv_jobcount TYPE tbtcjob-jobcount
                  ix_error    TYPE REF TO cx_root,

      send_error_notification
        IMPORTING iv_jobname TYPE tbtcjob-jobname
                  iv_message TYPE string.
ENDCLASS.

CLASS zcl_job_error_handler IMPLEMENTATION.
  METHOD log_job_error.
    " Log to application log
    TRY.
        DATA(lo_log) = cl_bali_log=>create_with_header(
          header = cl_bali_header_setter=>create(
            object    = 'ZJOB'
            subobject = 'ERROR'
          )
        ).

        " Add exception to log
        DATA(lo_exception) = cl_bali_exception_setter=>create(
          severity  = if_bali_constants=>c_severity_error
          exception = ix_error
        ).

        lo_log->add_item( lo_exception ).

        " Save log
        cl_bali_log_db=>get_instance( )->save_log(
          log = lo_log
        ).

      CATCH cx_bali_runtime.
        " Fallback logging
        MESSAGE ix_error->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD send_error_notification.
    " Send email notification
    TRY.
        DATA(lo_send_request) = cl_bcs=>create_persistent( ).

        " Create message body
        DATA(lt_body) = VALUE bcsy_text(
          ( line = |Job { iv_jobname } failed| )
          ( line = |Error: { iv_message }| )
          ( line = |Time: { sy-datum } { sy-uzeit }| )
        ).

        DATA(lo_document) = cl_document_bcs=>create_document(
          i_type    = 'RAW'
          i_text    = lt_body
          i_subject = |Background Job Error: { iv_jobname }|
        ).

        lo_send_request->set_document( lo_document ).

        " Add recipient
        DATA(lo_recipient) = cl_cam_address_bcs=>create_internet_address(
          'admin@company.com'
        ).

        lo_send_request->add_recipient( lo_recipient ).

        " Send email
        lo_send_request->send( ).

        COMMIT WORK.

      CATCH cx_bcs INTO DATA(lx_bcs).
        " Log send error
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
```

---

## Best Practices

### 1. Job Design Principles

```abap
" ✅ DO: Use packages for large datasets
SELECT * FROM large_table
  PACKAGE SIZE 1000
  INTO TABLE @DATA(lt_package).
  " Process package
  COMMIT WORK.
ENDSELECT.

" ✅ DO: Implement restart capability
CLASS zcl_restartable_job DEFINITION.
  PRIVATE SECTION.
    METHODS:
      get_last_processed_id
        RETURNING VALUE(rv_id) TYPE zkey,

      save_checkpoint
        IMPORTING iv_id TYPE zkey.
ENDCLASS.

" ✅ DO: Add progress indicators
cl_progress_indicator=>progress_indicate(
  i_text               = |Processing { lv_count } of { lv_total }|
  i_processed          = lv_count
  i_total              = lv_total
  i_output_immediately = abap_true
).

" ✅ DO: Implement proper error handling
TRY.
    " Process batch
  CATCH cx_error INTO DATA(lx_error).
    " Log error
    " Send notification
    " Continue or abort based on severity
ENDTRY.
```

### 2. Performance Optimization

```abap
" ✅ DO: Use array operations
MODIFY ztable FROM TABLE lt_data.

" ❌ DON'T: Single record updates in loop
LOOP AT lt_data INTO DATA(ls_data).
  MODIFY ztable FROM ls_data.
ENDLOOP.

" ✅ DO: Commit in packages
DATA lv_package_counter TYPE i.
LOOP AT lt_large_table INTO DATA(ls_line).
  " Process
  lv_package_counter = lv_package_counter + 1.

  IF lv_package_counter MOD 1000 = 0.
    COMMIT WORK.
  ENDIF.
ENDLOOP.
COMMIT WORK.  " Final commit
```

### 3. Monitoring and Alerting

```abap
" ✅ DO: Log important milestones
MESSAGE |Started processing { lv_count } records| TYPE 'I'.

" ✅ DO: Track execution time
DATA: lv_start_time TYPE timestampl,
      lv_end_time   TYPE timestampl.

GET TIME STAMP FIELD lv_start_time.
" ... processing ...
GET TIME STAMP FIELD lv_end_time.

DATA(lv_duration) = cl_abap_tstmp=>subtract(
  tstmp1 = lv_end_time
  tstmp2 = lv_start_time
).

MESSAGE |Execution time: { lv_duration } seconds| TYPE 'I'.
```

---

**Last Updated:** December 2025

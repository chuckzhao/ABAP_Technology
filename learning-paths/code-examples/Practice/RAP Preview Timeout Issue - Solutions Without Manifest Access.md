# RAP Preview Timeout Issue - Solutions Without Manifest Access

## The Direct Answer

**Yes, the 30-second retrigger issue WILL happen with RAP Preview!**

The SAP generic Fiori Elements app used in the Preview has the same default 30-second timeout that any Fiori Elements app would have. Since you cannot modify SAP's built-in manifest.json, you cannot change this timeout setting directly in the Preview environment.

However, this doesn't mean you're stuck. Let me explain why this happens and then show you multiple solutions, both for the Preview scenario and for production use.

---

## Understanding Why It Happens in RAP Preview

When you click the Preview button in your Service Binding, here's what actually occurs behind the scenes:

### The Preview Flow

SAP launches its generic Fiori Elements application, which has a hardcoded manifest.json somewhere deep in the SAP Gateway infrastructure. That manifest contains default settings including a thirty-second timeout for OData requests. When your backend action takes longer than thirty seconds to complete, the frontend OData model assumes the request has failed or timed out. The model then automatically retries the request, which is exactly the behavior you observed earlier.

The challenge is that this manifest.json belongs to SAP's framework, not to your application. You have no direct access to modify it, which means you cannot simply increase the timeout value the way you would in an application you created yourself.

### Why This Matters for RAP Development

When you develop with RAP annotations, you're primarily working in the backend layer. Your CDS views, behavior definitions, and action implementations are all ABAP code running on the application server. If you have a complex action that involves sending emails, generating reports, or processing large datasets, it might legitimately take forty-five seconds or even two minutes to complete. The frontend timeout of thirty seconds becomes a problem because it doesn't align with the actual processing time your backend needs.

---

## Solution 1: Optimize Your Backend Performance (Preferred Long-Term Solution)

The most sustainable solution is to ensure your backend operations complete quickly enough that they don't hit the timeout threshold. This might sound like a limitation, but it actually represents good application design. Let me show you how to approach this.

### The Synchronous vs Asynchronous Pattern

Instead of making the user wait while a long-running process completes, you can split your operation into two parts: a quick acknowledgment followed by background processing.

**Current Pattern (Problematic):**

```abap
METHOD approve FOR MODIFY
  IMPORTING keys FOR ACTION SalesOrder~approve RESULT result.
  
  " This entire block runs synchronously while user waits
  
  " Step 1: Update status (fast - 1 second)
  MODIFY ENTITIES OF zi_salesorder IN LOCAL MODE
    ENTITY SalesOrder
      UPDATE FIELDS ( Status )
      WITH VALUE #( FOR key IN keys ( %tky = key-%tky Status = 'APPROVED' ) ).
  
  " Step 2: Send email notifications (slow - 15 seconds)
  LOOP AT keys INTO DATA(ls_key).
    " Email sending logic
    CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
      " ... email parameters ...
  ENDLOOP.
  
  " Step 3: Generate approval report (slow - 20 seconds)
  LOOP AT keys INTO DATA(ls_key).
    " Report generation logic
  ENDLOOP.
  
  " Step 4: Update related records (medium - 8 seconds)
  " ... more processing ...
  
  " Total time: 44 seconds → Triggers timeout and retry!
  
  " Return result
  READ ENTITIES OF zi_salesorder IN LOCAL MODE
    ENTITY SalesOrder ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(lt_result).
    
  result = VALUE #( FOR order IN lt_result ( %tky = order-%tky %param = order ) ).
ENDMETHOD.
```

**Improved Pattern (Asynchronous Background Processing):**

```abap
METHOD approve FOR MODIFY
  IMPORTING keys FOR ACTION SalesOrder~approve RESULT result.
  
  " Step 1: Quick status update only (fast - 1 second)
  MODIFY ENTITIES OF zi_salesorder IN LOCAL MODE
    ENTITY SalesOrder
      UPDATE FIELDS ( Status ApprovalInitiatedAt ApprovalInitiatedBy )
      WITH VALUE #( FOR key IN keys ( 
        %tky = key-%tky 
        Status = 'APPROVAL_PENDING'  " Intermediate status
        ApprovalInitiatedAt = cl_abap_context_info=>get_system_time( )
        ApprovalInitiatedBy = sy-uname
      ) ).
  
  " Step 2: Schedule background job for heavy processing (fast - 0.5 seconds)
  LOOP AT keys INTO DATA(ls_key).
    " Create background job entry
    DATA(lv_job_id) = lcl_background_processor=>schedule_approval_job(
      iv_sales_order = ls_key-SalesOrder
      iv_action = 'APPROVE'
    ).
    
    " Optionally store job ID for tracking
    MODIFY ENTITIES OF zi_salesorder IN LOCAL MODE
      ENTITY SalesOrder
        UPDATE FIELDS ( BackgroundJobId )
        WITH VALUE #( ( %tky = ls_key-%tky BackgroundJobId = lv_job_id ) ).
  ENDLOOP.
  
  " Total synchronous time: 1.5 seconds → No timeout!
  
  " The background job will handle:
  " - Email notifications
  " - Report generation  
  " - Related record updates
  " And when complete, update status to 'APPROVED'
  
  " Return result immediately
  READ ENTITIES OF zi_salesorder IN LOCAL MODE
    ENTITY SalesOrder ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(lt_result).
    
  result = VALUE #( FOR order IN lt_result ( %tky = order-%tky %param = order ) ).
ENDMETHOD.
```

### Background Job Implementation

You would create a helper class to manage the background processing:

```abap
CLASS lcl_background_processor DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS schedule_approval_job
      IMPORTING
        iv_sales_order TYPE vbeln_va
        iv_action      TYPE string
      RETURNING
        VALUE(rv_job_id) TYPE string.
        
  PRIVATE SECTION.
    CLASS-METHODS execute_approval_processing
      IMPORTING
        iv_sales_order TYPE vbeln_va.
ENDCLASS.

CLASS lcl_background_processor IMPLEMENTATION.
  
  METHOD schedule_approval_job.
    " Option 1: Use ABAP background job
    DATA: lv_jobname TYPE btcjob,
          lv_jobcount TYPE btcjobcnt.
    
    " Generate unique job name
    lv_jobname = |APPROVE_ORDER_{ iv_sales_order }|.
    
    " Open job
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname  = lv_jobname
      IMPORTING
        jobcount = lv_jobcount.
    
    " Submit program to run in background
    SUBMIT z_process_order_approval
      WITH p_order = iv_sales_order
      WITH p_action = iv_action
      VIA JOB lv_jobname NUMBER lv_jobcount
      AND RETURN.
    
    " Schedule job to start immediately
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount = lv_jobcount
        jobname  = lv_jobname
        strtimmed = 'X'.  " Start immediately
    
    " Return job identifier for tracking
    rv_job_id = |{ lv_jobname }_{ lv_jobcount }|.
    
    " Option 2: Use Application Job (for newer systems)
    " TRY.
    "   DATA(lo_application_job) = cl_apj_rt_api=>schedule_job(
    "     iv_job_template_name = 'Z_APPROVAL_TEMPLATE'
    "     iv_job_text = |Approve Order { iv_sales_order }|
    "     it_job_parameter_values = VALUE #(
    "       ( name = 'SALES_ORDER' t_value = VALUE #( ( sign = 'I' option = 'EQ' low = iv_sales_order ) ) )
    "     )
    "   ).
    "   rv_job_id = lo_application_job.
    " CATCH cx_apj_rt INTO DATA(lx_error).
    "   " Handle error
    " ENDTRY.
    
  ENDMETHOD.
  
  METHOD execute_approval_processing.
    " This runs in the background job
    
    " Step 1: Get order data
    SELECT SINGLE * FROM zsalesorder
      WHERE sales_order = @iv_sales_order
      INTO @DATA(ls_order).
    
    IF sy-subrc <> 0.
      " Log error and exit
      RETURN.
    ENDIF.
    
    " Step 2: Send email notifications (takes time, but user doesn't wait)
    TRY.
        " Email logic here
        CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
          " ... parameters ...
      CATCH cx_root INTO DATA(lx_email_error).
        " Log error but continue
    ENDTRY.
    
    " Step 3: Generate approval report (takes time, but user doesn't wait)
    TRY.
        " Report generation logic
      CATCH cx_root INTO DATA(lx_report_error).
        " Log error but continue
    ENDTRY.
    
    " Step 4: Update final status to APPROVED
    UPDATE zsalesorder
      SET status = 'APPROVED'
          approved_at = @( cl_abap_context_info=>get_system_time( ) )
          approved_by = @sy-uname
      WHERE sales_order = @iv_sales_order.
    
    COMMIT WORK.
    
    " Step 5: Optional - Send notification that processing is complete
    " (Could be email, workflow, or just status update that UI polls)
    
  ENDMETHOD.
  
ENDCLASS.
```

### User Experience with Asynchronous Processing

With this pattern, here's what the user experiences:

The user clicks the approve button in the Fiori app. Within one or two seconds, they see a success message that says "Approval initiated successfully. You will receive a notification when processing is complete." The order status changes to "Approval Pending" with a visual indicator showing that background processing is in progress. The user can continue working on other tasks immediately without waiting. Five minutes later, the background job completes all the heavy processing. The order status automatically updates to "Approved" and the user receives an email notification that the approval process is complete.

This approach provides a much better user experience and completely avoids the timeout issue because the synchronous part of the operation completes in under two seconds.

---

## Solution 2: Use Server-Side Timeout Configuration

While you cannot change the client-side timeout in the Preview manifest, you can configure server-side timeout settings that might help in certain scenarios.

### ICF Service Timeout

The Internet Communication Framework service that hosts your OData service has its own timeout settings that are independent of the client timeout.

**Transaction: SICF**

Navigate to the ICF service for your OData endpoint. The path typically looks like this: default_host → sap → opu → odata4 → sap → your_service_name.

Right-click on your service node and select "Change" to modify the service properties. Look for the timeout settings in the configuration. You can increase the timeout value from the default of sixty seconds to something higher like one hundred twenty or one hundred eighty seconds.

However, I need to be honest with you about the limitation here. Even if you increase the server-side timeout, the client-side timeout in the Fiori Elements framework will still trigger after thirty seconds. The server will continue processing your request, but the frontend will have already given up and retried. This can help in some edge cases but doesn't fully solve the problem.

### SAP Gateway Timeout Settings

**Transaction: /IWFND/V4_ADMIN**

Navigate to your service in the V4 Admin tool. Check the service-specific timeout settings. You might find options to configure how long the gateway waits for backend responses. Again, this helps ensure the server doesn't prematurely kill long-running operations, but it doesn't prevent the client-side retry behavior.

---

## Solution 3: Implement Idempotency in Your Actions

Since you cannot prevent the retry from happening in the Preview environment, the next best approach is to ensure that if a retry does occur, it doesn't cause problems. This is called making your action idempotent, which means that executing the same action multiple times has the same effect as executing it once.

### Idempotent Action Implementation

```abap
METHOD approve FOR MODIFY
  IMPORTING keys FOR ACTION SalesOrder~approve RESULT result.
  
  " Step 1: Read current state with lock
  READ ENTITIES OF zi_salesorder IN LOCAL MODE
    ENTITY SalesOrder
      FIELDS ( Status ApprovedAt ApprovedBy )
      WITH CORRESPONDING #( keys )
    RESULT DATA(lt_orders)
    FAILED DATA(lt_failed).
  
  " Step 2: Check if already approved (idempotency check)
  LOOP AT lt_orders INTO DATA(ls_order).
    IF ls_order-Status = 'APPROVED' AND ls_order-ApprovedAt IS NOT INITIAL.
      " Already approved - this is a retry
      " Don't process again, just return success
      APPEND VALUE #(
        %tky = ls_order-%tky
        %msg = new_message_with_text(
          severity = if_abap_behv_message=>severity-success
          text = |Order { ls_order-SalesOrder } was already approved at { ls_order-ApprovedAt }|
        )
      ) TO reported-salesorder.
      
      " Still return success, not error
      CONTINUE.
    ENDIF.
    
    " Step 3: Check if approval is in progress (within last 2 minutes)
    IF ls_order-Status = 'APPROVAL_PENDING' AND ls_order-ApprovalInitiatedAt IS NOT INITIAL.
      DATA(lv_seconds_elapsed) = cl_abap_tstmp=>subtract(
        tstmp1 = cl_abap_context_info=>get_system_time( )
        tstmp2 = ls_order-ApprovalInitiatedAt
      ).
      
      IF lv_seconds_elapsed < 120.  " Less than 2 minutes
        " Approval already in progress, likely a retry
        APPEND VALUE #(
          %tky = ls_order-%tky
          %msg = new_message_with_text(
            severity = if_abap_behv_message=>severity-information
            text = |Approval for order { ls_order-SalesOrder } is already in progress|
          )
        ) TO reported-salesorder.
        CONTINUE.
      ENDIF.
    ENDIF.
    
    " Step 4: Process approval (only if not already done)
    " ... your approval logic here ...
    
  ENDLOOP.
  
  " Return result
  READ ENTITIES OF zi_salesorder IN LOCAL MODE
    ENTITY SalesOrder ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(lt_result).
    
  result = VALUE #( FOR order IN lt_result ( %tky = order-%tky %param = order ) ).
ENDMETHOD.
```

### What Idempotency Achieves

With this implementation, if the frontend retries the approve action after thirty seconds, your backend code detects that the order was already approved or that approval is already in progress. Instead of processing the approval twice, it simply returns a success message indicating the current state. This prevents duplicate emails, duplicate reports, and duplicate database updates. The user sees a success message regardless of whether this was the first request or a retry, which is the correct behavior from their perspective.

---

## Solution 4: Create Your Own Custom Fiori Elements App

This is the most comprehensive solution and gives you full control over the timeout behavior. Since the Preview is just for development and testing anyway, for production use you should create your own Fiori application that consumes your RAP service.

### Step-by-Step Process

Open VS Code and launch the SAP Fiori Application Generator using the command palette. Choose "List Report Page" as your template type since you're building a Fiori Elements application. When asked for the data source, select "Connect to OData Service" and enter the URL of your published RAP service. The generator will download the metadata and show you the available entities. Select your main entity, configure navigation if needed, and provide your project details.

The generator creates a complete Fiori Elements application structure with your own manifest.json file. This file is now under your control, and you can modify it as needed.

### Customizing the Timeout

Open the generated manifest.json and locate the models section. Add or modify the timeout setting:

```json
{
  "sap.ui5": {
    "models": {
      "": {
        "dataSource": "mainService",
        "settings": {
          "synchronizationMode": "None",
          "operationMode": "Server",
          "autoExpandSelect": true,
          "timeout": 120000  // 120 seconds instead of default 30
        }
      }
    }
  }
}
```

Now when you deploy this application to your SAP system, it will use your custom timeout setting of one hundred twenty seconds instead of the default thirty seconds.

### Additional Customizations Available

Since you now have your own manifest, you can also configure many other aspects of the application that are not possible in the Preview:

You can change the table type from ResponsiveTable to GridTable if you prefer a different user interface. You can add custom actions and buttons that call your own controller extensions. You can configure variant management behavior to control how users save and share filter settings. You can add custom fragments for completely custom UI sections. You can control the initial load behavior and pagination settings.

All of these customizations are impossible in the Preview environment but become available once you create your own application.

---

## Solution 5: Use Inline Processing Indicators

Even if you cannot prevent the timeout, you can improve the user experience by showing progress during long-running operations. This requires creating your own app with custom extensions.

### Progress Dialog Implementation

In your custom Fiori Elements app, you can add a controller extension that shows a progress dialog during long-running actions:

```javascript
sap.ui.define([
    'sap/ui/core/mvc/ControllerExtension',
    'sap/m/BusyDialog',
    'sap/m/MessageToast'
], function(ControllerExtension, BusyDialog, MessageToast) {
    'use strict';

    return ControllerExtension.extend('com.mycompany.salesorder.ext.controller.ObjectPageExt', {
        
        onApprove: function(oBindingContext, aSelectedContexts) {
            // Create a custom busy dialog with message
            var oBusyDialog = new BusyDialog({
                title: "Approving Order",
                text: "Processing approval... This may take up to 2 minutes.\n\nPlease do not close this window.",
                showCancelButton: false
            });
            
            oBusyDialog.open();
            
            // Call the RAP action
            var oModel = this.base.getView().getModel();
            var oContext = oBindingContext || aSelectedContexts[0];
            
            // Execute the action with extended timeout expectation
            var oOperation = oModel.bindContext(
                oContext.getPath() + '/com.sap.service.approve(...)',
                oContext
            );
            
            oOperation.execute().then(function() {
                oBusyDialog.close();
                MessageToast.show("Order approved successfully!");
                
                // Refresh the data
                oModel.refresh();
                
            }).catch(function(oError) {
                oBusyDialog.close();
                
                // Check if it's a timeout error
                if (oError.canceled || oError.message.includes("timeout")) {
                    MessageToast.show(
                        "Approval is still processing in the background. " +
                        "Please refresh in a few moments to see the updated status."
                    );
                } else {
                    // Real error
                    sap.m.MessageBox.error("Approval failed: " + oError.message);
                }
            });
        }
    });
});
```

This provides better user feedback during long operations and handles timeout scenarios gracefully by informing the user that processing continues in the background.

---

## Recommended Approach - Decision Matrix

Let me help you decide which solution or combination of solutions works best for your situation.

### For Development and Testing (RAP Preview)

If you're just using the Preview for development and quick validation, implement idempotency in your actions as described in Solution Three. This ensures that even if retries happen, they don't cause problems. Additionally, optimize your action implementations to complete quickly, ideally under twenty seconds. For any operations that genuinely need more time, use the asynchronous background processing pattern from Solution One.

### For Production Deployment

Create your own Fiori Elements application in VS Code as described in Solution Four. This gives you a manifest.json that you can customize with appropriate timeout values. Deploy this application to your SAP system and add it to the Fiori Launchpad for your users. Continue to use the Preview during development, but always test with your custom app before releasing to production.

### For Complex Long-Running Operations

Implement the asynchronous pattern from Solution One regardless of which frontend approach you use. Quick acknowledgment with background processing provides the best user experience and eliminates timeout concerns entirely. Add progress monitoring capabilities so users can check the status of their long-running operations. Consider implementing notifications (email or in-app) when background processing completes.

---

## Summary

The thirty-second timeout issue absolutely can occur when using RAP annotations with the Preview function, and you cannot directly fix it by modifying the manifest because that manifest belongs to SAP's framework. However, you have multiple effective solutions available.

The best long-term approach combines backend optimization with frontend customization. On the backend side, implement asynchronous processing for long-running operations and ensure all actions are idempotent to handle retries gracefully. On the frontend side, create your own Fiori Elements application with a customized manifest that includes appropriate timeout settings for your use case.

For development purposes, the Preview is perfectly fine even with its limitations, as long as your actions are idempotent. For production use, always create your own application to gain full control over timeout behavior and other configuration options. This hybrid approach gives you the best of both worlds: the rapid development benefits of RAP annotations combined with the flexibility and control of a custom Fiori application.

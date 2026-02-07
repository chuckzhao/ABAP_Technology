# Complete Fiori App Tracing Guide - All Three Development Approaches

## Table of Contents
1. [Understanding the Three Development Paths](#understanding-the-three-development-paths)
2. [Approach 1: RAP with Fiori Elements Preview](#approach-1-rap-with-fiori-elements-preview)
3. [Approach 2: Fiori Elements via Application Generator](#approach-2-fiori-elements-via-application-generator)
4. [Approach 3: Freestyle SAPUI5](#approach-3-freestyle-sapui5)
5. [Quick Identification Guide](#quick-identification-guide)
6. [Common Tracing Techniques](#common-tracing-techniques)
7. [Troubleshooting Decision Tree](#troubleshooting-decision-tree)

---

## Understanding the Three Development Paths

Before tracing any Fiori application, you must first identify which development approach was used. Each approach has a fundamentally different architecture:

### **Approach 1: RAP with Fiori Elements Preview**
- **Backend-Driven**: App generated at runtime from CDS annotations
- **No Frontend Files**: Everything controlled by RAP behavior definitions
- **Location**: Files live in ADT (Eclipse), no VS Code frontend files
- **Deployment**: Accessed via `/sap/bc/adt/businessservices/.../preview`

### **Approach 2: Fiori Elements via Application Generator**
- **Template-Based**: SAP generates frontend files from templates
- **Frontend Files Exist**: `manifest.json`, `Component.js` in BSP repository
- **Location**: Files in both VS Code project AND SAP BSP repository
- **Deployment**: Deployed as BSP application via `npm run deploy`

### **Approach 3: Freestyle SAPUI5**
- **Custom Code**: Manually written JavaScript, views, controllers
- **Full Frontend Control**: Complete XML views, custom controllers
- **Location**: Files in both VS Code project AND SAP BSP repository
- **Deployment**: Deployed as BSP application via deploy tools

---

## Approach 1: RAP with Fiori Elements Preview

### **Step 1: Identify It's a RAP Preview App**

**In Browser:**
```
URL Pattern: 
https://host:port/sap/bc/adt/businessservices/odatav4/feap
?sap-ui-xx-viewCache=false&fePreviewModelId=YOUR_SERVICE
&sap-client=100&sap-language=EN
```

**Key Indicators:**
- URL contains `/sap/bc/adt/businessservices/`
- URL contains `fePreviewModelId=`
- No BSP application name in URL
- Opens directly from ADT Eclipse

### **Step 2: Trace Frontend to Backend**

#### **2.1 Browser Developer Tools**

**Open Network Tab:**
```
F12 → Network Tab → Filter: OData
```

**Find the OData Service Call:**
```
Request URL: 
https://host:port/sap/opu/odata4/sap/zi_travel_m/srvd_a2x/sap/zi_travel_m/0001/Travel

Request Headers:
sap-statistics-id: YOUR_TRACE_ID
```

**Get Service Technical Name:**
- Look for `/sap/opu/odata4/sap/[SERVICE_NAME]/`
- Service name format: `ZI_TRAVEL_M` or `ZUI_TRAVEL_O4`

#### **2.2 Find Service Definition in ADT**

**In Eclipse ADT:**
```
1. Press Ctrl+Shift+A (Open ABAP Development Object)
2. Search for: ZI_TRAVEL_M or ZUI_TRAVEL_O4
3. Filter by: Service Definition
4. Double-click to open
```

**Service Definition Content:**
```abap
@EndUserText.label: 'Travel Service'
define service ZUI_TRAVEL_O4 {
  expose ZC_TRAVEL_M as Travel;
  expose ZC_BOOKING_M as Booking;
}
```

#### **2.3 Trace to Projection View (C_View)**

**From Service Definition:**
```
expose ZC_TRAVEL_M as Travel;
          ↓
   This is the CDS Projection View
```

**Open Projection View:**
```
Ctrl+Shift+A → Search: ZC_TRAVEL_M
```

**Projection View Content:**
```abap
@EndUserText.label: 'Travel Projection'
@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
define root view entity ZC_TRAVEL_M
  provider contract transactional_query
  as projection on ZI_TRAVEL_M
{
  key TravelUUID,
      TravelID,
      AgencyID,
      CustomerID,
      BeginDate,
      EndDate,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      TotalPrice,
      CurrencyCode,
      Description,
      OverallStatus
}
```

#### **2.4 Find Metadata Extension (UI Annotations)**

**Right-click on ZC_TRAVEL_M → Navigate → Metadata Extension**

**Or manually search:**
```
Ctrl+Shift+A → Search: ZC_TRAVEL_M
Filter by: Metadata Extension
```

**Metadata Extension Content:**
```abap
@Metadata.layer: #CORE
@UI: {
  headerInfo: {
    typeName: 'Travel',
    typeNamePlural: 'Travels',
    title: { value: 'TravelID' }
  },
  presentationVariant: [{
    sortOrder: [{ by: 'TravelID', direction: #DESC }],
    visualizations: [{type: #AS_LINEITEM}]
  }]
}
annotate view ZC_TRAVEL_M with
{
  @UI.facet: [
    { id: 'Travel',
      purpose: #STANDARD,
      type: #IDENTIFICATION_REFERENCE,
      label: 'Travel Details',
      position: 10 }
  ]
  
  @UI.lineItem: [{ position: 10 }]
  @UI.identification: [{ position: 10 }]
  TravelID;
  
  @UI.lineItem: [{ position: 20 }]
  @UI.identification: [{ position: 20 }]
  AgencyID;
  
  @UI.lineItem: [{ position: 30 }]
  @UI.identification: [{ position: 30 }]
  CustomerID;
}
```

**What Controls What:**
- `@UI.lineItem` → Columns in List Report table
- `@UI.facet` → Sections in Object Page
- `@UI.identification` → Fields within sections
- `@UI.headerInfo` → Object Page header
- `@UI.selectionField` → Filters in filter bar

#### **2.5 Trace to Interface View (I_View)**

**From Projection View:**
```abap
define root view entity ZC_TRAVEL_M
  as projection on ZI_TRAVEL_M
                      ↓
            This is the Interface View
```

**Open Interface View:**
```
Ctrl+Shift+A → Search: ZI_TRAVEL_M
```

**Interface View Content:**
```abap
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Travel Interface View'
define root view entity ZI_TRAVEL_M
  as select from ztravel_xxx as Travel
  composition [0..*] of ZI_BOOKING_M as _Booking
  association [0..1] to /DMO/I_Agency as _Agency
    on $projection.AgencyID = _Agency.AgencyID
  association [0..1] to /DMO/I_Customer as _Customer
    on $projection.CustomerID = _Customer.CustomerID
{
  key travel_uuid as TravelUUID,
      travel_id as TravelID,
      agency_id as AgencyID,
      customer_id as CustomerID,
      begin_date as BeginDate,
      end_date as EndDate,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      total_price as TotalPrice,
      currency_code as CurrencyCode,
      description as Description,
      overall_status as OverallStatus,
      
      _Booking,
      _Agency,
      _Customer
}
```

#### **2.6 Find Database Table**

**From Interface View:**
```abap
define root view entity ZI_TRAVEL_M
  as select from ztravel_xxx as Travel
                    ↓
         This is the database table
```

**Open Table:**
```
Ctrl+Shift+A → Search: ZTRAVEL_XXX
Filter by: Database Table
```

**Table Structure:**
```abap
@EndUserText.label: 'Travel Data'
@AbapCatalog.enhancement.category: #NOT_EXTENSIBLE
define table ztravel_xxx {
  key client : abap.clnt;
  key travel_uuid : sysuuid_x16;
  travel_id : /dmo/travel_id;
  agency_id : /dmo/agency_id;
  customer_id : /dmo/customer_id;
  begin_date : /dmo/begin_date;
  end_date : /dmo/end_date;
  @Semantics.amount.currencyCode: 'ztravel_xxx.currency_code'
  total_price : /dmo/total_price;
  currency_code : /dmo/currency_code;
  description : /dmo/description;
  overall_status : /dmo/overall_status;
  created_by : abp_creation_user;
  created_at : abp_creation_tstmpl;
  last_changed_by : abp_locinst_lastchange_user;
  last_changed_at : abp_locinst_lastchange_tstmpl;
}
```

#### **2.7 Find Behavior Definition**

**Right-click on ZI_TRAVEL_M → Navigate → Behavior Definition**

**Or search:**
```
Ctrl+Shift+A → Search: ZI_TRAVEL_M
Filter by: Behavior Definition
```

**Behavior Definition Content:**
```abap
managed implementation in class zbp_i_travel_m unique;
strict ( 2 );
with draft;

define behavior for ZI_TRAVEL_M alias Travel
persistent table ztravel_xxx
draft table ztravel_d_xxx
lock master total etag LastChangedAt
authorization master ( instance )
etag master LocalLastChangedAt
{
  create;
  update;
  delete;
  
  association _Booking { create; with draft; }
  
  field ( readonly ) TravelUUID, TotalPrice;
  field ( readonly : update ) TravelID;
  field ( mandatory ) AgencyID, CustomerID, BeginDate, EndDate;
  
  determination setTravelNumber on save { create; }
  determination calculateTotalPrice on modify { field BookingFee, CurrencyCode; }
  
  validation validateCustomer on save { field CustomerID; create; update; }
  validation validateDates on save { field BeginDate, EndDate; create; update; }
  validation validateAgency on save { field AgencyID; create; update; }
  
  action ( features : instance ) acceptTravel result [1] $self;
  action ( features : instance ) rejectTravel result [1] $self;
  
  draft action Edit;
  draft action Activate;
  draft action Discard;
  draft action Resume;
  draft determine action Prepare;
  
  mapping for ztravel_xxx
  {
    TravelUUID = travel_uuid;
    TravelID = travel_id;
    AgencyID = agency_id;
    CustomerID = customer_id;
    BeginDate = begin_date;
    EndDate = end_date;
    TotalPrice = total_price;
    CurrencyCode = currency_code;
    Description = description;
    OverallStatus = overall_status;
  }
}
```

**Key Behavior Elements:**
- `create/update/delete` → CRUD operations
- `field (readonly)` → Fields user cannot edit
- `field (mandatory)` → Required fields
- `determination` → Auto-calculations, numbering
- `validation` → Data validation rules
- `action` → Custom business actions
- `draft action` → Draft handling
- `association` → Child entity operations

#### **2.8 Find Behavior Implementation Class**

**From Behavior Definition:**
```abap
managed implementation in class zbp_i_travel_m unique;
                                      ↓
                    This is the behavior implementation class
```

**Open Class:**
```
Ctrl+Shift+A → Search: ZBP_I_TRAVEL_M
Filter by: Class
```

**Behavior Implementation Class:**
```abap
CLASS zbp_i_travel_m DEFINITION
  PUBLIC
  ABSTRACT
  FINAL
  FOR BEHAVIOR OF zi_travel_m.
ENDCLASS.

CLASS zbp_i_travel_m IMPLEMENTATION.
ENDCLASS.
```

**Local Types (where real logic lives):**
```abap
CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Travel
      RESULT result.
      
    METHODS setTravelNumber FOR DETERMINE ON SAVE
      IMPORTING keys FOR Travel~setTravelNumber.
      
    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~calculateTotalPrice.
      
    METHODS validateCustomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateCustomer.
      
    METHODS validateDates FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateDates.
      
    METHODS validateAgency FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateAgency.
      
    METHODS acceptTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~acceptTravel
      RESULT result.
      
    METHODS rejectTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~rejectTravel
      RESULT result.
ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.

  METHOD setTravelNumber.
    " Auto-generate travel ID
    READ ENTITIES OF zi_travel_m IN LOCAL MODE
      ENTITY Travel
        FIELDS ( TravelID )
        WITH CORRESPONDING #( keys )
      RESULT DATA(travels).
      
    DELETE travels WHERE TravelID IS NOT INITIAL.
    CHECK travels IS NOT INITIAL.
    
    " Get next number
    TRY.
        cl_numberrange_runtime=>number_get(
          EXPORTING
            nr_range_nr = '01'
            object = 'ZTRAVEL'
            quantity = CONV #( lines( travels ) )
          IMPORTING
            number = DATA(number_range_key)
            returncode = DATA(number_range_return_code)
            returned_quantity = DATA(number_range_returned_quantity) ).
      CATCH cx_number_ranges INTO DATA(lx_number_ranges).
        " Handle error
    ENDTRY.
    
    " Assign numbers
    DATA lv_travel_id TYPE /dmo/travel_id VALUE '00000000'.
    LOOP AT travels INTO DATA(travel).
      lv_travel_id = number_range_key.
      number_range_key = number_range_key + 1.
      
      MODIFY ENTITIES OF zi_travel_m IN LOCAL MODE
        ENTITY Travel
          UPDATE FIELDS ( TravelID )
          WITH VALUE #( ( %tky = travel-%tky
                         TravelID = lv_travel_id ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD validateCustomer.
    " Validate customer exists
    READ ENTITIES OF zi_travel_m IN LOCAL MODE
      ENTITY Travel
        FIELDS ( CustomerID )
        WITH CORRESPONDING #( keys )
      RESULT DATA(travels).
      
    SELECT FROM /dmo/customer
      FIELDS customer_id
      FOR ALL ENTRIES IN @travels
      WHERE customer_id = @travels-CustomerID
      INTO TABLE @DATA(customers).
      
    LOOP AT travels INTO DATA(travel).
      IF NOT line_exists( customers[ customer_id = travel-CustomerID ] ).
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.
        APPEND VALUE #(
          %tky = travel-%tky
          %msg = new_message_with_text(
            severity = if_abap_behv_message=>severity-error
            text = |Customer { travel-CustomerID } does not exist| )
          %element-CustomerID = if_abap_behv=>mk-on
        ) TO reported-travel.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateDates.
    " Validate begin date < end date
    READ ENTITIES OF zi_travel_m IN LOCAL MODE
      ENTITY Travel
        FIELDS ( BeginDate EndDate )
        WITH CORRESPONDING #( keys )
      RESULT DATA(travels).
      
    LOOP AT travels INTO DATA(travel).
      IF travel-BeginDate >= travel-EndDate.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.
        APPEND VALUE #(
          %tky = travel-%tky
          %msg = new_message_with_text(
            severity = if_abap_behv_message=>severity-error
            text = 'Begin date must be before end date' )
          %element-BeginDate = if_abap_behv=>mk-on
          %element-EndDate = if_abap_behv=>mk-on
        ) TO reported-travel.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD acceptTravel.
    " Change status to accepted
    MODIFY ENTITIES OF zi_travel_m IN LOCAL MODE
      ENTITY Travel
        UPDATE FIELDS ( OverallStatus )
        WITH VALUE #( FOR key IN keys (
          %tky = key-%tky
          OverallStatus = 'A' ) ).
          
    READ ENTITIES OF zi_travel_m IN LOCAL MODE
      ENTITY Travel
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(travels).
      
    result = VALUE #( FOR travel IN travels (
      %tky = travel-%tky
      %param = travel ) ).
  ENDMETHOD.

  METHOD calculateTotalPrice.
    " Sum booking prices to calculate total
    READ ENTITIES OF zi_travel_m IN LOCAL MODE
      ENTITY Travel
        FIELDS ( CurrencyCode )
        WITH CORRESPONDING #( keys )
      RESULT DATA(travels)
      ENTITY Travel BY \_Booking
        FIELDS ( FlightPrice CurrencyCode )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bookings).
      
    LOOP AT travels INTO DATA(travel).
      DATA(lv_total) = CONV /dmo/total_price( 0 ).
      
      LOOP AT bookings INTO DATA(booking)
        WHERE TravelUUID = travel-TravelUUID.
        lv_total = lv_total + booking-FlightPrice.
      ENDLOOP.
      
      MODIFY ENTITIES OF zi_travel_m IN LOCAL MODE
        ENTITY Travel
          UPDATE FIELDS ( TotalPrice )
          WITH VALUE #( ( %tky = travel-%tky
                         TotalPrice = lv_total ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
```

### **Step 3: Complete Trace Summary for RAP**

**Execution Flow When User Clicks "Create":**

```
Browser (Fiori App)
    ↓
OData Request: POST /Travel
    ↓
Service Definition (ZUI_TRAVEL_O4)
    ↓
Projection View (ZC_TRAVEL_M) → Metadata Extension (UI Annotations)
    ↓
Interface View (ZI_TRAVEL_M)
    ↓
Behavior Definition (validates, determines, acts)
    ↓
Behavior Implementation Class (ZBP_I_TRAVEL_M)
    ↓ 
Local Handler Class (lhc_travel)
    ↓
Method: setTravelNumber, validateCustomer, validateDates, etc.
    ↓
Database Table (ZTRAVEL_XXX)
    ↓
Response back through same chain
    ↓
Browser displays result
```

**Files You Need to Find:**
1. ✅ **Service Definition**: `ZUI_TRAVEL_O4`
2. ✅ **Projection View**: `ZC_TRAVEL_M`
3. ✅ **Metadata Extension**: `ZC_TRAVEL_M` (UI annotations)
4. ✅ **Interface View**: `ZI_TRAVEL_M`
5. ✅ **Behavior Definition**: `ZI_TRAVEL_M`
6. ✅ **Behavior Implementation**: `ZBP_I_TRAVEL_M`
7. ✅ **Database Table**: `ZTRAVEL_XXX`

**Where to Find Them:**
- **All files**: ADT Eclipse
- **No VS Code files**: This approach has NO frontend files
- **No BSP repository**: Preview app is generated at runtime

---

## Approach 2: Fiori Elements via Application Generator

### **Step 1: Identify It's a Fiori Elements App**

**In Browser:**
```
URL Pattern:
https://host:port/sap/bc/ui5_ui5/sap/ztravel_app/index.html
?sap-client=100&sap-language=EN
```

**Key Indicators:**
- URL contains `/sap/bc/ui5_ui5/`
- Has a BSP application name (e.g., `ztravel_app`)
- No `fePreviewModelId` in URL
- Can find app in transaction `/n/UI5/UI5_REPOSITORY_LOAD`

### **Step 2: Find Frontend Files in SAP**

#### **2.1 Using Transaction SE80**

```
Transaction: /nSE80
→ Repository Browser
→ BSP Application
→ Search: ZTRAVEL_APP
```

**Files You'll Find:**
```
ZTRAVEL_APP/
├── webapp/
│   ├── manifest.json          ← Main app configuration
│   ├── Component.js            ← App bootstrap
│   ├── i18n/                   ← Translations
│   ├── localService/           ← Mock data
│   ├── test/                   ← Test files
│   └── index.html             ← Entry point
```

#### **2.2 Using Transaction /n/UI5/UI5_REPOSITORY_LOAD**

```
Transaction: /n/UI5/UI5_REPOSITORY_LOAD
→ Display mode
→ BSP Application: ZTRAVEL_APP
→ Click "Display"
```

**Navigate folder structure:**
```
webapp/ → manifest.json (double-click to view)
```

### **Step 3: Analyze manifest.json**

**Get manifest.json content from SE80 or /UI5/UI5_REPOSITORY_LOAD:**

```json
{
  "sap.app": {
    "id": "ztravel.app",
    "type": "application",
    "title": "{{appTitle}}",
    "description": "{{appDescription}}",
    "dataSources": {
      "mainService": {
        "uri": "/sap/opu/odata4/sap/zi_travel_m/srvd_a2x/sap/zi_travel_m/0001/",
        "type": "OData",
        "settings": {
          "odataVersion": "4.0",
          "localUri": "localService/metadata.xml"
        }
      }
    }
  },
  "sap.ui5": {
    "dependencies": {
      "libs": {
        "sap.fe.templates": {}
      }
    },
    "routing": {
      "config": {},
      "routes": [
        {
          "pattern": ":?query:",
          "name": "TravelList",
          "target": "TravelList"
        },
        {
          "pattern": "Travel({key}):?query:",
          "name": "TravelObjectPage",
          "target": "TravelObjectPage"
        }
      ],
      "targets": {
        "TravelList": {
          "type": "Component",
          "id": "TravelList",
          "name": "sap.fe.templates.ListReport",
          "options": {
            "settings": {
              "entitySet": "Travel",
              "navigation": {
                "Travel": {
                  "detail": {
                    "route": "TravelObjectPage"
                  }
                }
              }
            }
          }
        },
        "TravelObjectPage": {
          "type": "Component",
          "id": "TravelObjectPage",
          "name": "sap.fe.templates.ObjectPage",
          "options": {
            "settings": {
              "entitySet": "Travel"
            }
          }
        }
      }
    },
    "models": {
      "i18n": {
        "type": "sap.ui.model.resource.ResourceModel",
        "uri": "i18n/i18n.properties"
      },
      "": {
        "dataSource": "mainService",
        "preload": true,
        "settings": {
          "synchronizationMode": "None",
          "operationMode": "Server",
          "autoExpandSelect": true,
          "earlyRequests": true,
          "timeout": 120000
        }
      }
    }
  }
}
```

**Key Information from manifest.json:**

**1. OData Service URL:**
```json
"uri": "/sap/opu/odata4/sap/zi_travel_m/srvd_a2x/sap/zi_travel_m/0001/"
        ↓                       ↓
    OData V4 path      Service Name: ZI_TRAVEL_M
```

**2. Template Type:**
```json
"name": "sap.fe.templates.ListReport"  ← List Report template
"name": "sap.fe.templates.ObjectPage"  ← Object Page template
```

**3. Entity Set:**
```json
"entitySet": "Travel"  ← The entity being displayed
```

### **Step 4: Find VS Code Project Files**

**If you have access to the original VS Code project:**

```
Project Structure:
travel-app/
├── webapp/
│   ├── manifest.json          ← Same as in SAP
│   ├── Component.js
│   ├── i18n/
│   ├── annotations.xml        ← Local UI annotations (optional)
│   ├── ext/                   ← Extensions (custom code)
│   └── test/
├── ui5.yaml                   ← UI5 Tooling config
├── package.json               ← NPM dependencies
└── ui5-deploy.yaml           ← Deployment config
```

**Check package.json for deployment info:**
```json
{
  "name": "travel-app",
  "version": "0.0.1",
  "scripts": {
    "start": "fiori run",
    "deploy": "fiori deploy --config ui5-deploy.yaml",
    "build": "ui5 build"
  },
  "devDependencies": {
    "@sap/ux-ui5-tooling": "^1",
    "@sap/ux-specification": "^1",
    "@ui5/cli": "^3.0.0"
  }
}
```

**Check ui5-deploy.yaml for BSP app name:**
```yaml
specVersion: '3.0'
metadata:
  name: ztravel.app
type: application
builder:
  customTasks:
    - name: deploy-to-abap
      afterTask: generateCachebusterInfo
      configuration:
        target:
          destination: SAP_DEV
          bsp-application: ZTRAVEL_APP
          package: ZTRAVEL
          transport: DEVK900001
```

### **Step 5: Trace Backend (Same as RAP)**

**From manifest.json OData service:**
```
/sap/opu/odata4/sap/zi_travel_m/srvd_a2x/sap/zi_travel_m/0001/
                        ↓
            Service Name: ZI_TRAVEL_M
```

**Follow the same backend tracing as RAP Approach:**
1. Find Service Definition
2. Find Projection View (C_View)
3. Find Metadata Extension
4. Find Interface View (I_View)
5. Find Behavior Definition
6. Find Behavior Implementation Class
7. Find Database Table

### **Step 6: Browser to Backend Trace**

**Network Tab in Browser:**
```
F12 → Network → Filter: OData

Request:
GET /sap/opu/odata4/sap/zi_travel_m/srvd_a2x/sap/zi_travel_m/0001/Travel
```

**Trace in SAP Gateway:**
```
Transaction: /nST05
→ Activate Trace (SQL + RFC + Buffer)
→ Perform action in browser
→ Deactivate Trace
→ Display Trace
→ Filter by user/time
```

**Alternative: /IWFND/ERROR_LOG**
```
Transaction: /n/IWFND/ERROR_LOG
→ Shows all OData errors with stack traces
→ Filter by timestamp when you tested
```

### **Step 7: Complete Trace Summary for Fiori Elements**

**Execution Flow:**

```
Browser
    ↓
index.html (BSP: ZTRAVEL_APP)
    ↓
Component.js (bootstrap)
    ↓
manifest.json (configuration)
    ↓
SAP Fiori Elements Templates (sap.fe.templates.ListReport)
    ↓
OData Request: /sap/opu/odata4/sap/zi_travel_m/.../Travel
    ↓
[SAME BACKEND FLOW AS RAP]
Service Definition → Projection View → Interface View 
→ Behavior Definition → Behavior Implementation → Database Table
    ↓
OData Response
    ↓
Fiori Elements Template renders UI
    ↓
Browser displays
```

**Files You Need:**

**Frontend (in BSP):**
1. ✅ **BSP Application**: `ZTRAVEL_APP` (SE80)
2. ✅ **manifest.json**: Configuration
3. ✅ **Component.js**: Bootstrap
4. ✅ **annotations.xml**: Local UI annotations (optional)
5. ✅ **i18n/**: Translations

**Frontend (in VS Code if available):**
1. ✅ Same files as above
2. ✅ **package.json**: Dependencies
3. ✅ **ui5.yaml**: UI5 Tooling
4. ✅ **ui5-deploy.yaml**: Deployment config

**Backend (in ADT):**
1. ✅ Service Definition
2. ✅ Projection View + Metadata Extension
3. ✅ Interface View
4. ✅ Behavior Definition + Implementation
5. ✅ Database Table

---

## Approach 3: Freestyle SAPUI5

### **Step 1: Identify It's a Freestyle App**

**In Browser:**
```
URL Pattern:
https://host:port/sap/bc/ui5_ui5/sap/zcustom_app/index.html
```

**Key Indicators:**
- URL contains `/sap/bc/ui5_ui5/`
- Has custom BSP application name
- Can inspect source and see custom JavaScript controllers
- Has XML views (not template-based)

### **Step 2: Inspect Browser Source**

**F12 → Sources Tab:**
```
Look for folder structure:
sap/bc/ui5_ui5/sap/zcustom_app/
├── controller/
│   ├── Main.controller.js       ← Custom controller logic
│   ├── Detail.controller.js
│   └── BaseController.js
├── view/
│   ├── Main.view.xml            ← Custom XML views
│   └── Detail.view.xml
├── model/
│   └── formatter.js             ← Data formatting
├── manifest.json
└── Component.js
```

### **Step 3: Find Frontend Files in SAP**

**Transaction SE80:**
```
/nSE80
→ BSP Application
→ ZCUSTOM_APP
```

**File Structure:**
```
ZCUSTOM_APP/
├── webapp/
│   ├── controller/
│   │   ├── Main.controller.js
│   │   └── Detail.controller.js
│   ├── view/
│   │   ├── Main.view.xml
│   │   └── Detail.view.xml
│   ├── model/
│   │   ├── formatter.js
│   │   └── models.js
│   ├── css/
│   │   └── style.css
│   ├── i18n/
│   ├── manifest.json
│   ├── Component.js
│   └── index.html
```

### **Step 4: Analyze Main Controller**

**Example Main.controller.js:**
```javascript
sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageToast"
], function (Controller, JSONModel, MessageToast) {
    "use strict";

    return Controller.extend("zcustom.app.controller.Main", {

        onInit: function () {
            // Initialize view model
            var oViewModel = new JSONModel({
                busy: false,
                delay: 0
            });
            this.getView().setModel(oViewModel, "viewModel");
            
            // Load data
            this._loadData();
        },

        _loadData: function () {
            var oModel = this.getView().getModel();
            var that = this;
            
            this.getView().setBusy(true);
            
            // OData read
            oModel.read("/TravelSet", {
                success: function (oData) {
                    that.getView().setBusy(false);
                    MessageToast.show("Data loaded successfully");
                },
                error: function (oError) {
                    that.getView().setBusy(false);
                    MessageToast.show("Error loading data");
                }
            });
        },

        onSearchTravel: function (oEvent) {
            // Get search value
            var sQuery = oEvent.getParameter("query");
            
            // Build filters
            var aFilters = [];
            if (sQuery) {
                aFilters.push(new sap.ui.model.Filter({
                    filters: [
                        new sap.ui.model.Filter("TravelID", sap.ui.model.FilterOperator.Contains, sQuery),
                        new sap.ui.model.Filter("Description", sap.ui.model.FilterOperator.Contains, sQuery)
                    ],
                    and: false
                }));
            }
            
            // Apply filters
            var oList = this.byId("travelList");
            var oBinding = oList.getBinding("items");
            oBinding.filter(aFilters);
        },

        onTravelPress: function (oEvent) {
            // Navigate to detail
            var oItem = oEvent.getSource();
            var oContext = oItem.getBindingContext();
            var sTravelID = oContext.getProperty("TravelID");
            
            var oRouter = this.getOwnerComponent().getRouter();
            oRouter.navTo("detail", {
                travelId: sTravelID
            });
        },

        onCreateTravel: function () {
            // Open create dialog
            if (!this._oCreateDialog) {
                this._oCreateDialog = sap.ui.xmlfragment(
                    "zcustom.app.view.CreateTravelDialog",
                    this
                );
                this.getView().addDependent(this._oCreateDialog);
            }
            this._oCreateDialog.open();
        },

        onSaveTravel: function () {
            var oModel = this.getView().getModel();
            var oEntry = {
                AgencyID: "070001",
                CustomerID: "000001",
                BeginDate: new Date(),
                EndDate: new Date(),
                Description: "Test Travel"
            };
            
            var that = this;
            oModel.create("/TravelSet", oEntry, {
                success: function (oData) {
                    MessageToast.show("Travel created successfully");
                    that._oCreateDialog.close();
                    that._loadData();
                },
                error: function (oError) {
                    MessageToast.show("Error creating travel");
                }
            });
        },

        onDeleteTravel: function (oEvent) {
            var oContext = oEvent.getParameter("listItem").getBindingContext();
            var sPath = oContext.getPath();
            var oModel = this.getView().getModel();
            
            oModel.remove(sPath, {
                success: function () {
                    MessageToast.show("Travel deleted");
                },
                error: function () {
                    MessageToast.show("Error deleting travel");
                }
            });
        }
    });
});
```

**Key Points to Trace:**
1. `getView().getModel()` → OData model from manifest.json
2. `oModel.read("/TravelSet")` → OData entity set name
3. `oModel.create("/TravelSet", oEntry)` → Create operation
4. Navigation: `oRouter.navTo("detail")` → Route defined in manifest.json

### **Step 5: Analyze XML View**

**Example Main.view.xml:**
```xml
<mvc:View
    controllerName="zcustom.app.controller.Main"
    xmlns:mvc="sap.ui.core.mvc"
    xmlns="sap.m"
    displayBlock="true">
    
    <Page
        id="mainPage"
        title="{i18n>mainTitle}"
        showNavButton="false">
        
        <content>
            <SearchField
                id="searchField"
                search=".onSearchTravel"
                width="100%"
                placeholder="{i18n>searchPlaceholder}"/>
            
            <List
                id="travelList"
                items="{
                    path: '/TravelSet',
                    sorter: {
                        path: 'TravelID',
                        descending: true
                    }
                }"
                mode="Delete"
                delete=".onDeleteTravel">
                
                <items>
                    <ObjectListItem
                        type="Active"
                        press=".onTravelPress"
                        title="{TravelID}"
                        number="{
                            parts: [{path: 'TotalPrice'}, {path: 'CurrencyCode'}],
                            type: 'sap.ui.model.type.Currency',
                            formatOptions: {showMeasure: true}
                        }"
                        numberState="{
                            path: 'OverallStatus',
                            formatter: '.formatter.statusState'
                        }">
                        
                        <attributes>
                            <ObjectAttribute
                                text="{AgencyID}"/>
                            <ObjectAttribute
                                text="{CustomerID}"/>
                            <ObjectAttribute
                                text="{
                                    path: 'BeginDate',
                                    type: 'sap.ui.model.type.Date',
                                    formatOptions: {pattern: 'MMM dd, yyyy'}
                                }"/>
                        </attributes>
                        
                        <firstStatus>
                            <ObjectStatus
                                text="{
                                    path: 'OverallStatus',
                                    formatter: '.formatter.statusText'
                                }"
                                state="{
                                    path: 'OverallStatus',
                                    formatter: '.formatter.statusState'
                                }"/>
                        </firstStatus>
                    </ObjectListItem>
                </items>
            </List>
        </content>
        
        <footer>
            <Toolbar>
                <ToolbarSpacer/>
                <Button
                    text="{i18n>createButton}"
                    icon="sap-icon://add"
                    press=".onCreateTravel"/>
            </Toolbar>
        </footer>
    </Page>
</mvc:View>
```

**Key Points:**
1. `items="{path: '/TravelSet'}"` → Entity set binding
2. Property bindings: `{TravelID}`, `{TotalPrice}`, etc. → Field names
3. Formatters: `.formatter.statusState` → Custom logic in formatter.js
4. Event handlers: `press=".onTravelPress"` → Methods in controller

### **Step 6: Analyze manifest.json**

**Freestyle manifest.json:**
```json
{
  "sap.app": {
    "id": "zcustom.app",
    "type": "application",
    "title": "Custom Travel App",
    "dataSources": {
      "mainService": {
        "uri": "/sap/opu/odata/sap/ZTRAVEL_SRV/",
        "type": "OData",
        "settings": {
          "odataVersion": "2.0",
          "localUri": "localService/metadata.xml"
        }
      }
    }
  },
  "sap.ui5": {
    "rootView": {
      "viewName": "zcustom.app.view.Main",
      "type": "XML",
      "id": "main"
    },
    "routing": {
      "config": {
        "routerClass": "sap.m.routing.Router",
        "viewType": "XML",
        "viewPath": "zcustom.app.view",
        "controlId": "app",
        "controlAggregation": "pages",
        "async": true
      },
      "routes": [
        {
          "pattern": "",
          "name": "main",
          "target": "main"
        },
        {
          "pattern": "detail/{travelId}",
          "name": "detail",
          "target": "detail"
        }
      ],
      "targets": {
        "main": {
          "viewName": "Main",
          "viewId": "main"
        },
        "detail": {
          "viewName": "Detail",
          "viewId": "detail"
        }
      }
    },
    "models": {
      "i18n": {
        "type": "sap.ui.model.resource.ResourceModel",
        "settings": {
          "bundleName": "zcustom.app.i18n.i18n"
        }
      },
      "": {
        "dataSource": "mainService",
        "preload": true,
        "settings": {
          "defaultBindingMode": "TwoWay",
          "defaultCountMode": "Inline",
          "useBatch": true,
          "refreshAfterChange": false
        }
      }
    }
  }
}
```

**Key Information:**
1. **OData Service**: `/sap/opu/odata/sap/ZTRAVEL_SRV/`
2. **OData Version**: 2.0 (older)
3. **Root View**: `zcustom.app.view.Main`
4. **Routing**: Custom route patterns

### **Step 7: Find Backend OData Service**

**From manifest.json:**
```
"uri": "/sap/opu/odata/sap/ZTRAVEL_SRV/"
                           ↓
                Service Name: ZTRAVEL_SRV
```

#### **7.1 Find Service in Gateway**

**Transaction /nSEGW:**
```
SAP Gateway Service Builder
→ Search for: ZTRAVEL_SRV
→ Double-click to open
```

**Or Transaction /n/IWFND/MAINT_SERVICE:**
```
Activate and Maintain Services
→ Filter by: ZTRAVEL
→ Find: ZTRAVEL_SRV
→ Technical Service Name column shows technical name
```

#### **7.2 Analyze Service Structure**

**In SEGW, you'll see:**
```
Data Model:
├── Entity Types
│   ├── Travel
│   │   ├── Properties: TravelID, AgencyID, CustomerID, etc.
│   │   └── Key: TravelID
│   └── Booking
│       ├── Properties: BookingID, FlightPrice, etc.
│       └── Key: TravelID, BookingID
│
├── Entity Sets
│   ├── TravelSet → Entity Type: Travel
│   └── BookingSet → Entity Type: Booking
│
├── Associations
│   └── TravelToBooking: Travel (1) → Booking (*)
│
└── Function Imports
    ├── AcceptTravel
    └── RejectTravel
```

#### **7.3 Find Implementation Class**

**In SEGW:**
```
Service Implementation → Runtime Artifacts

You'll find:
- Data Provider Class (DPC): ZCL_ZTRAVEL_SRV_DPC_EXT
- Model Provider Class (MPC): ZCL_ZTRAVEL_SRV_MPC_EXT
```

**Open DPC class in SE24:**
```
Transaction: /nSE24
Class: ZCL_ZTRAVEL_SRV_DPC_EXT
```

**Example DPC Implementation:**
```abap
CLASS zcl_ztravel_srv_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_ztravel_srv_dpc
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS /iwbep/if_mgw_appl_srv_runtime~get_entityset
      REDEFINITION.
    METHODS /iwbep/if_mgw_appl_srv_runtime~get_entity
      REDEFINITION.
    METHODS /iwbep/if_mgw_appl_srv_runtime~create_entity
      REDEFINITION.
    METHODS /iwbep/if_mgw_appl_srv_runtime~update_entity
      REDEFINITION.
    METHODS /iwbep/if_mgw_appl_srv_runtime~delete_entity
      REDEFINITION.

  PRIVATE SECTION.
    METHODS travelset_get_entityset
      IMPORTING
        iv_entity_name TYPE string
        iv_entity_set_name TYPE string
        iv_source_name TYPE string
        it_filter_select_options TYPE /iwbep/t_mgw_select_option
        it_order TYPE /iwbep/t_mgw_sorting_order
        is_paging TYPE /iwbep/s_mgw_paging
        it_navigation_path TYPE /iwbep/t_mgw_navigation_path
        it_key_tab TYPE /iwbep/t_mgw_name_value_pair
        iv_filter_string TYPE string
        iv_search_string TYPE string
      EXPORTING
        et_entityset TYPE zcl_ztravel_srv_mpc=>tt_travel
        es_response_context TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_mgw_response_context
      RAISING
        /iwbep/cx_mgw_busi_exception
        /iwbep/cx_mgw_tech_exception.

    METHODS travelset_create_entity
      IMPORTING
        iv_entity_name TYPE string
        iv_entity_set_name TYPE string
        iv_source_name TYPE string
        io_data_provider TYPE REF TO /iwbep/if_mgw_entry_provider
        it_key_tab TYPE /iwbep/t_mgw_name_value_pair
        it_navigation_path TYPE /iwbep/t_mgw_navigation_path
        io_tech_request_context TYPE REF TO /iwbep/if_mgw_req_entity_c
      EXPORTING
        er_entity TYPE zcl_ztravel_srv_mpc=>ts_travel
      RAISING
        /iwbep/cx_mgw_busi_exception
        /iwbep/cx_mgw_tech_exception.
ENDCLASS.

CLASS zcl_ztravel_srv_dpc_ext IMPLEMENTATION.

  METHOD /iwbep/if_mgw_appl_srv_runtime~get_entityset.
    " Route to specific entity set handler
    CASE iv_entity_set_name.
      WHEN 'TravelSet'.
        travelset_get_entityset(
          EXPORTING
            iv_entity_name = iv_entity_name
            iv_entity_set_name = iv_entity_set_name
            iv_source_name = iv_source_name
            it_filter_select_options = it_filter_select_options
            it_order = it_order
            is_paging = is_paging
            it_navigation_path = it_navigation_path
            it_key_tab = it_key_tab
            iv_filter_string = iv_filter_string
            iv_search_string = iv_search_string
          IMPORTING
            et_entityset = er_entityset
            es_response_context = es_response_context ).
    ENDCASE.
  ENDMETHOD.

  METHOD travelset_get_entityset.
    " Read from database
    DATA: lt_travel TYPE STANDARD TABLE OF ztravel_xxx.
    
    SELECT * FROM ztravel_xxx
      INTO CORRESPONDING FIELDS OF TABLE @lt_travel.
      
    " Apply filters
    IF it_filter_select_options IS NOT INITIAL.
      " Process filters
    ENDIF.
    
    " Apply sorting
    IF it_order IS NOT INITIAL.
      " Process sorting
    ENDIF.
    
    " Apply paging
    IF is_paging-top IS NOT INITIAL.
      DELETE lt_travel FROM is_paging-top + 1.
    ENDIF.
    
    " Convert to OData format
    et_entityset = CORRESPONDING #( lt_travel ).
    
    " Set inline count
    es_response_context-inlinecount = lines( lt_travel ).
  ENDMETHOD.

  METHOD travelset_create_entity.
    " Get data from request
    DATA: ls_travel TYPE zcl_ztravel_srv_mpc=>ts_travel,
          ls_db_travel TYPE ztravel_xxx.
    
    io_data_provider->read_entry_data(
      IMPORTING
        es_data = ls_travel ).
        
    " Generate ID
    TRY.
        cl_numberrange_runtime=>number_get(
          EXPORTING
            nr_range_nr = '01'
            object = 'ZTRAVEL'
          IMPORTING
            number = ls_travel-travel_id ).
      CATCH cx_number_ranges.
        " Handle error
    ENDTRY.
    
    " Validate data
    IF ls_travel-customer_id IS INITIAL.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message = 'Customer ID is required'.
    ENDIF.
    
    " Save to database
    ls_db_travel = CORRESPONDING #( ls_travel ).
    ls_db_travel-created_by = sy-uname.
    ls_db_travel-created_at = cl_abap_tstmp=>utclong_current( ).
    
    INSERT ztravel_xxx FROM @ls_db_travel.
    
    IF sy-subrc = 0.
      COMMIT WORK.
      er_entity = ls_travel.
    ELSE.
      ROLLBACK WORK.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message = 'Error creating travel'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
```

#### **7.4 Find Database Tables**

**From DPC class, trace database operations:**
```abap
SELECT * FROM ztravel_xxx
              ↓
    This is the database table
```

**Or check in SEGW:**
```
Service Implementation → Map to Data Source
→ Shows mapped tables/structures
```

### **Step 8: Complete Trace Summary for Freestyle**

**Execution Flow:**

```
Browser
    ↓
index.html (BSP: ZCUSTOM_APP)
    ↓
Component.js (bootstrap)
    ↓
manifest.json (routing, models)
    ↓
Main.view.xml (XML view)
    ↓
Main.controller.js (event handlers)
    ↓
OData Model API call (read/create/update/delete)
    ↓
HTTP Request: GET /sap/opu/odata/sap/ZTRAVEL_SRV/TravelSet
    ↓
Gateway Runtime (/IWFND/)
    ↓
Service ZTRAVEL_SRV
    ↓
DPC Class: ZCL_ZTRAVEL_SRV_DPC_EXT
    ↓
Method: TRAVELSET_GET_ENTITYSET
    ↓
Database Table: ZTRAVEL_XXX
    ↓
Response back through same chain
    ↓
Main.controller.js processes response
    ↓
Main.view.xml updates binding
    ↓
Browser displays result
```

**Files You Need:**

**Frontend (in BSP: ZCUSTOM_APP):**
1. ✅ **manifest.json**: App configuration
2. ✅ **Component.js**: Bootstrap
3. ✅ **Main.view.xml**: Main view
4. ✅ **Detail.view.xml**: Detail view
5. ✅ **Main.controller.js**: Main controller
6. ✅ **Detail.controller.js**: Detail controller
7. ✅ **formatter.js**: Data formatters
8. ✅ **i18n/i18n.properties**: Translations

**Frontend (in VS Code if available):**
- Same as above

**Backend:**
1. ✅ **Gateway Service**: ZTRAVEL_SRV (SEGW)
2. ✅ **DPC Class**: ZCL_ZTRAVEL_SRV_DPC_EXT (SE24)
3. ✅ **MPC Class**: ZCL_ZTRAVEL_SRV_MPC_EXT (SE24)
4. ✅ **Database Tables**: ZTRAVEL_XXX (SE11)
5. ✅ **Function Modules**: Any custom logic (SE37)

---

## Quick Identification Guide

### **How to Quickly Identify Which Approach Was Used**

**Look at the URL:**

```
RAP Preview:
✓ https://.../ sap/bc/adt/businessservices /odatav4/feap?fePreviewModelId=...
         Contains: /adt/businessservices/ and fePreviewModelId

Fiori Elements:
✓ https://.../ sap/bc/ui5_ui5/sap/zapp_name /index.html
         Contains: /ui5_ui5/ and BSP app name

Freestyle:
✓ https://.../ sap/bc/ui5_ui5/sap/zcustom_app /index.html
         Contains: /ui5_ui5/ and BSP app name (same as Fiori Elements!)
```

**Differentiate Fiori Elements vs Freestyle:**

**1. Inspect manifest.json in browser source:**

```javascript
// Fiori Elements:
"routing": {
  "targets": {
    "TravelList": {
      "name": "sap.fe.templates.ListReport"  ← SAP template
    }
  }
}

// Freestyle:
"routing": {
  "targets": {
    "main": {
      "viewName": "Main"  ← Custom view name
    }
  }
}
```

**2. Check browser source folders:**

```
Fiori Elements:
✓ NO controller/ folder in source
✓ NO view/ folder in source
✓ Only manifest.json, Component.js, i18n/

Freestyle:
✓ Has controller/ folder with .controller.js files
✓ Has view/ folder with .view.xml files
✓ Custom code everywhere
```

**3. Look for sap.fe.templates:**

```
Fiori Elements:
✓ Uses sap.fe.templates.ListReport
✓ Uses sap.fe.templates.ObjectPage

Freestyle:
✓ Uses sap.m.routing.Router
✓ Uses sap.ui.core.mvc.Controller
```

---

## Common Tracing Techniques

### **Technique 1: Browser Network Tab**

**For ALL approaches:**

```
F12 → Network Tab → Filter: "odata" or "OData"

Look for:
- Request URL: Shows exact service and entity set
- Request Headers: sap-client, authorization
- Response: Shows actual data returned
- Timing: Performance analysis
```

**Example Network Request:**
```
Request URL: 
https://host:port/sap/opu/odata4/sap/zi_travel_m/srvd_a2x/sap/zi_travel_m/0001/Travel
                                       ↓                              ↓             ↓
                                Service Group               Service Name    Entity Set

Request Method: GET

Request Headers:
accept: application/json
sap-client: 100
sap-language: EN
authorization: Basic ...

Response:
{
  "@odata.context": "$metadata#Travel",
  "value": [
    {
      "TravelUUID": "...",
      "TravelID": "00000001",
      "AgencyID": "070001",
      ...
    }
  ]
}
```

### **Technique 2: SAP Gateway Error Log**

**Transaction /n/IWFND/ERROR_LOG:**

```
Shows all OData requests and errors:
- Timestamp
- User
- Service
- Entity Set
- HTTP Method
- Error Message
- Call Stack
```

**How to Use:**
1. Perform action in browser
2. Go to /IWFND/ERROR_LOG
3. Filter by your user and time
4. Double-click error to see full stack trace
5. Follow stack to find exact code location

### **Technique 3: SQL Trace**

**Transaction /nST05:**

```
1. Activate Trace
   - SQL Trace: ON
   - RFC Trace: ON (if needed)
   - Buffer Trace: OFF (unless needed)

2. Execute 1 (only 1!) request in browser

3. Deactivate Trace

4. Display Trace
   - Shows all SQL statements
   - Shows table accesses
   - Shows execution time
```

**Example Trace Output:**
```
SELECT * FROM ZTRAVEL_XXX
  WHERE TRAVEL_ID = '00000001'
  
Duration: 0.125 ms
Rows: 1
```

### **Technique 4: OData Service Metadata**

**For ALL OData-based approaches:**

**Get metadata URL from manifest.json:**
```
Service: /sap/opu/odata4/sap/zi_travel_m/srvd_a2x/sap/zi_travel_m/0001/

Metadata: /sap/opu/odata4/sap/zi_travel_m/srvd_a2x/sap/zi_travel_m/0001/$metadata
```

**Open in browser:**
```xml
<edmx:Edmx Version="4.0">
  <edmx:DataServices>
    <Schema Namespace="SAP">
      <EntityType Name="TravelType">
        <Key>
          <PropertyRef Name="TravelUUID"/>
        </Key>
        <Property Name="TravelUUID" Type="Edm.Guid" Nullable="false"/>
        <Property Name="TravelID" Type="Edm.String" MaxLength="8"/>
        <Property Name="AgencyID" Type="Edm.String" MaxLength="6"/>
        <Property Name="CustomerID" Type="Edm.String" MaxLength="6"/>
        <Property Name="BeginDate" Type="Edm.Date"/>
        <Property Name="EndDate" Type="Edm.Date"/>
        <Property Name="TotalPrice" Type="Edm.Decimal" Scale="2"/>
        <Property Name="CurrencyCode" Type="Edm.String" MaxLength="3"/>
        
        <NavigationProperty Name="_Booking" Type="Collection(SAP.BookingType)"/>
      </EntityType>
      
      <EntitySet Name="Travel" EntityType="SAP.TravelType">
        <NavigationPropertyBinding Path="_Booking" Target="Booking"/>
      </EntitySet>
    </Schema>
  </edmx:DataServices>
</edmx:Edmx>
```

**What you learn from metadata:**
- Entity types and their properties
- Data types and constraints
- Navigation properties (associations)
- Entity sets available
- Function imports (actions)

### **Technique 5: ADT Object Search**

**For RAP and Fiori Elements:**

```
Ctrl+Shift+A in Eclipse ADT

Search for object name
Filter by type:
- Service Definition
- CDS View
- Metadata Extension
- Behavior Definition
- Class
- Table
```

**Quick Navigation:**
```
Right-click on object → Navigate →
- Used By
- Where-Used List
- Related Objects
- Behavior Definition
- Metadata Extension
```

### **Technique 6: VS Code Search**

**For Fiori Elements and Freestyle:**

```
Ctrl+Shift+F (Search in files)

Search for:
- Entity set name: "TravelSet"
- OData service: "zi_travel_m"
- Controller method: "onTravelPress"
- i18n key: "mainTitle"
```

**Search in specific folders:**
```
controller/ → Event handlers
view/ → XML views
model/ → Formatters, models
i18n/ → Translations
```

---

## Troubleshooting Decision Tree

### **Problem: App Doesn't Load**

```
Is URL correct?
├─ NO → Check deployment, BSP app exists?
└─ YES
    ↓
    Check browser console (F12)
    ├─ 404 Not Found → BSP app not deployed or wrong URL
    ├─ 401 Unauthorized → Login required or no authorization
    ├─ 403 Forbidden → User lacks authorization
    ├─ 500 Internal Error → Check error log (/IWFND/ERROR_LOG)
    └─ JavaScript errors → Check Component.js, manifest.json syntax
```

### **Problem: Data Doesn't Display**

```
Is service call successful? (Network tab)
├─ NO → Check OData service
│   ├─ Service inactive? → /IWFND/MAINT_SERVICE
│   ├─ Authorization? → Check user roles (PFCG)
│   └─ Service error? → /IWFND/ERROR_LOG
└─ YES → Data returned but not shown
    ├─ Check binding path in XML view
    ├─ Check model name in manifest.json
    └─ Check property names match metadata
```

### **Problem: Create/Update Fails**

```
Check validation messages
├─ Frontend validation → Check controller code
└─ Backend validation → Trace to:
    ├─ RAP: Behavior implementation validation method
    ├─ Fiori Elements: Same as RAP
    └─ Freestyle: DPC class CREATE_ENTITY method
```

### **Problem: Can't Find Source Code**

```
What type of app? (Check URL)
├─ RAP Preview (/adt/businessservices/)
│   └─ ALL code in ADT Eclipse
│       └─ Search: Ctrl+Shift+A
│
├─ Fiori Elements (/ui5_ui5/ + templates in manifest)
│   ├─ Frontend: BSP app (SE80)
│   │   └─ Check manifest.json for service
│   └─ Backend: ADT Eclipse
│       └─ Follow service → views → behavior
│
└─ Freestyle (/ui5_ui5/ + custom controllers)
    ├─ Frontend: BSP app (SE80)
    │   └─ controller/, view/ folders
    └─ Backend: SEGW (Gateway Service Builder)
        └─ DPC class for implementation
```

---

## Summary Comparison Table

| Aspect | RAP Preview | Fiori Elements | Freestyle |
|--------|-------------|----------------|-----------|
| **URL Pattern** | `/sap/bc/adt/businessservices/...?fePreviewModelId=` | `/sap/bc/ui5_ui5/sap/app_name/` | `/sap/bc/ui5_ui5/sap/app_name/` |
| **Frontend Files** | ❌ None (runtime generated) | ✅ manifest.json, Component.js | ✅ Views, Controllers, Models |
| **UI Control** | Annotations in Metadata Extension | Annotations + manifest.json | Custom XML views |
| **Business Logic** | Behavior Definition + Implementation | Same as RAP | DPC class methods |
| **Development Tool** | ADT Eclipse only | VS Code + ADT | VS Code + ADT + SEGW |
| **OData Version** | V4 | V4 or V2 | V2 (usually) |
| **Deployment** | No deployment (preview) | `npm run deploy` to BSP | Deploy to BSP |
| **Customization** | Limited (annotations only) | Medium (extensions, actions) | Full (custom code) |
| **Complexity** | Low (annotations) | Medium (template + config) | High (full custom) |
| **Best For** | Quick prototypes, simple apps | Standard business apps | Complex custom requirements |

---

## Key Takeaways

### **RAP Preview:**
- **Fastest to develop**: Pure annotations
- **Hardest to trace**: Everything runtime-generated
- **All files in ADT**: No VS Code needed
- **No deployment**: Preview URL only

### **Fiori Elements:**
- **Template-based**: SAP generates frontend
- **Two locations**: VS Code project + BSP repository
- **Easy to deploy**: `npm run deploy`
- **Medium customization**: Extensions and actions

### **Freestyle:**
- **Full control**: Custom code everywhere
- **Two locations**: VS Code project + BSP repository
- **Most complex**: Manual coding required
- **Legacy OData**: Usually V2, older Gateway services

### **General Tracing Strategy:**

1. **Identify approach** (URL + source inspection)
2. **Find OData service** (manifest.json or Network tab)
3. **Trace service to backend** (Service Def → Views → Behavior)
4. **Find business logic** (Behavior class or DPC class)
5. **Locate database operations** (Tables in I_View or DPC)

---

## Quick Reference Commands

### **Eclipse ADT:**
```
Ctrl+Shift+A → Open object
Ctrl+F3 → Navigate to definition
Alt+← / Alt+→ → Navigate back/forward
F3 → Go to declaration
Ctrl+H → Search in workspace
```

### **VS Code:**
```
Ctrl+P → Quick file open
Ctrl+Shift+F → Search in files
F12 → Go to definition
Alt+← / Alt+→ → Navigate back/forward
Ctrl+` → Toggle terminal
```

### **Browser:**
```
F12 → Developer tools
Ctrl+Shift+C → Inspect element
Ctrl+Shift+I → Console
Ctrl+R → Reload
Ctrl+Shift+R → Hard reload
```

### **SAP Transactions:**
```
/nSE80 → BSP applications
/n/IWFND/ERROR_LOG → Gateway errors
/n/IWFND/MAINT_SERVICE → Service activation
/nSEGW → Gateway Service Builder
/nSE24 → Class Builder
/nSE11 → ABAP Dictionary
/nST05 → SQL Trace
```

---

This guide provides a complete roadmap for tracing any Fiori application back to its source code, regardless of which development approach was used. The key is first identifying which approach was used, then following the appropriate tracing path for that approach.

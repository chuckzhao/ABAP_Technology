# Three Ways to Create Fiori Apps - Complete Comparison

## Important Clarification: Frontend vs Backend

The three approaches are actually:

```
APPROACH 1: Fiori Elements (Frontend) + Any Backend
├── Frontend: VS Code creates minimal app
└── Backend: Can be RAP, Gateway, CAP, etc.

APPROACH 2: Freestyle SAPUI5 (Frontend) + Any Backend
├── Frontend: VS Code creates full custom app
└── Backend: Can be RAP, Gateway, CAP, etc.

APPROACH 3: RAP-First Development (Backend-Driven)
├── Backend: ADT/Eclipse creates CDS with @UI annotations
└── Frontend: Fiori Elements auto-generated from annotations
```

**Key Insight:** RAP with annotations is NOT a separate frontend creation method - it's a **backend development approach** that works WITH Fiori Elements!

---

## The Three Approaches - Detailed Breakdown

### APPROACH 1: Fiori Elements with VS Code

```
┌─────────────────────────────────────────────────────────┐
│                    APPROACH 1                            │
│              Fiori Elements (Template-Based)             │
└─────────────────────────────────────────────────────────┘

WHERE YOU WORK:
├── Frontend: VS Code (your local machine)
└── Backend: Already exists (OData service)

WHAT YOU CREATE:
├── manifest.json (40 lines)
├── Component.js (8 lines)
└── Optional: Custom extensions

TOTAL CODE YOU WRITE: ~50-200 lines

RESULT:
Frontend app that generates UI from backend metadata
```

#### Step-by-Step: Create Fiori Elements App

**1. Open VS Code**
```
Press F1 → "Fiori: Open Application Generator"
```

**2. Choose Template**
```
Template Type: SAP Fiori Elements
Template: List Report Page
```

**3. Configure Data Source**
```
Data Source: Connect to OData Service
Service URL: http://your-sap:8000/sap/opu/odata4/sap/zsd_salesorder/.../
```

**4. Select Entity**
```
Main Entity: SalesOrder
Navigation Entity: _Items
```

**5. Project Details**
```
Module name: salesorder-fe
Application title: Sales Order Display
Namespace: com.mycompany.salesorder
```

**6. Generated Files**
```
salesorder-fe/
├── webapp/
│   ├── manifest.json              ← 40 lines
│   ├── Component.js              ← 8 lines
│   ├── index.html               ← 15 lines
│   └── test/
│       └── flpSandbox.html
├── package.json
└── ui5.yaml
```

**manifest.json (What YOU get):**
```json
{
  "_version": "1.49.0",
  "sap.app": {
    "id": "com.mycompany.salesorder",
    "type": "application",
    "dataSources": {
      "mainService": {
        "uri": "/sap/opu/odata4/sap/zsd_salesorder/.../",
        "type": "OData",
        "settings": { "odataVersion": "4.0" }
      }
    }
  },
  "sap.ui5": {
    "dependencies": {
      "libs": {
        "sap.fe.templates": {}  // ← Fiori Elements!
      }
    },
    "routing": {
      "targets": {
        "SalesOrderList": {
          "type": "Component",
          "name": "sap.fe.templates.ListReport",  // ← Template
          "options": {
            "settings": {
              "contextPath": "/SalesOrder"
            }
          }
        }
      }
    }
  }
}
```

**Component.js (What YOU get):**
```javascript
sap.ui.define([
    "sap/fe/core/AppComponent"
], function (Component) {
    "use strict";
    return Component.extend("com.mycompany.salesorder.Component", {
        metadata: { manifest: "json" }
    });
});
```

**That's it! Total: ~50 lines**

**7. Test Locally**
```bash
npm install
npm start
# Opens http://localhost:8080
# Full app running!
```

**8. Deploy to SAP**
```bash
npm run deploy
# Uploads to SAP as BSP application
```

#### What Fiori Elements Generates at Runtime

**From your 50 lines, you get:**
- Complete List Report page
- Filter bar with all fields
- Table with sortable columns
- Object Page with sections
- Navigation
- Export, personalization, variants
- ~9,800 lines of equivalent functionality!

**Backend Requirements:**
- OData service must exist (RAP, Gateway, or CAP)
- Metadata must have @UI annotations (for Fiori Elements to read)

---

### APPROACH 2: Freestyle SAPUI5 with VS Code

```
┌─────────────────────────────────────────────────────────┐
│                    APPROACH 2                            │
│              Freestyle SAPUI5 (Custom-Coded)             │
└─────────────────────────────────────────────────────────┘

WHERE YOU WORK:
├── Frontend: VS Code (your local machine)
└── Backend: Already exists (OData service)

WHAT YOU CREATE:
├── manifest.json
├── Component.js
├── All controllers (JavaScript)
├── All views (XML)
├── Models, formatters, CSS
└── Every piece of UI logic

TOTAL CODE YOU WRITE: ~800-5,000 lines

RESULT:
Completely custom app with full control
```

#### Step-by-Step: Create Freestyle App

**1. Open VS Code**
```
Press F1 → "Fiori: Open Application Generator"
```

**2. Choose Template**
```
Template Type: SAPUI5 Freestyle
Template: SAPUI5 Application
```

**3. Configure Data Source**
```
Data Source: Connect to OData Service
Service URL: http://your-sap:8000/sap/opu/odata4/sap/zsd_salesorder/.../
```

**4. View Settings**
```
View name: Main
View type: XML
```

**5. Project Details**
```
Module name: salesorder-freestyle
Application title: Sales Order Display
Namespace: com.mycompany.salesorder
```

**6. Generated Files**
```
salesorder-freestyle/
├── webapp/
│   ├── manifest.json              ← Configuration
│   ├── Component.js              ← Component definition
│   ├── controller/
│   │   └── Main.controller.js   ← YOU write all logic here!
│   ├── view/
│   │   └── Main.view.xml        ← YOU design UI here!
│   ├── model/
│   │   └── models.js            ← YOU create models
│   └── css/
│       └── style.css            ← YOU write styles
├── package.json
└── ui5.yaml
```

**manifest.json:**
```json
{
  "_version": "1.49.0",
  "sap.app": {
    "id": "com.mycompany.salesorder.freestyle",
    "type": "application",
    "dataSources": {
      "mainService": {
        "uri": "/sap/opu/odata4/sap/zsd_salesorder/.../",
        "type": "OData",
        "settings": { "odataVersion": "4.0" }
      }
    }
  },
  "sap.ui5": {
    "rootView": {
      "viewName": "com.mycompany.salesorder.freestyle.view.Main",  // ← Your custom view
      "type": "XML",
      "async": true,
      "id": "Main"
    },
    "dependencies": {
      "libs": {
        "sap.m": {},           // ← Standard UI5 libraries
        "sap.ui.core": {},
        "sap.ui.layout": {}
        // NO sap.fe.templates!
      }
    },
    "models": {
      "": {
        "dataSource": "mainService",
        "preload": true
      }
    }
  }
}
```

**Main.view.xml (YOU must create this):**
```xml
<mvc:View
    controllerName="com.mycompany.salesorder.freestyle.controller.Main"
    xmlns:mvc="sap.ui.core.mvc"
    xmlns="sap.m">
    
    <Page title="Sales Order Display">
        <content>
            <!-- YOU design the entire UI! -->
            <Panel headerText="Search">
                <Input
                    id="orderInput"
                    placeholder="Enter Sales Order"
                    value="{viewModel>/orderNumber}" />
                <Button
                    text="Search"
                    press="onSearch" />
            </Panel>
            
            <Table
                id="ordersTable"
                items="{/SalesOrder}">
                <columns>
                    <Column><Text text="Order" /></Column>
                    <Column><Text text="Date" /></Column>
                    <Column><Text text="Customer" /></Column>
                </columns>
                <items>
                    <ColumnListItem>
                        <cells>
                            <Text text="{SalesOrder}" />
                            <Text text="{OrderDate}" />
                            <Text text="{CustomerName}" />
                        </cells>
                    </ColumnListItem>
                </items>
            </Table>
        </content>
    </Page>
</mvc:View>
```

**Main.controller.js (YOU must write all logic):**
```javascript
sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageBox"
], function (Controller, JSONModel, MessageBox) {
    "use strict";

    return Controller.extend("com.mycompany.salesorder.freestyle.controller.Main", {
        
        onInit: function () {
            // YOU initialize everything
            var oViewModel = new JSONModel({
                orderNumber: ""
            });
            this.getView().setModel(oViewModel, "viewModel");
        },

        onSearch: function () {
            // YOU write search logic
            var sOrderNumber = this.getView().getModel("viewModel").getProperty("/orderNumber");
            
            if (!sOrderNumber) {
                MessageBox.warning("Enter an order number");
                return;
            }
            
            this.getView().setBusy(true);
            
            var oModel = this.getView().getModel();
            var oBinding = this.byId("ordersTable").getBinding("items");
            
            // YOU implement filtering
            var aFilters = [
                new sap.ui.model.Filter("SalesOrder", sap.ui.model.FilterOperator.EQ, sOrderNumber)
            ];
            oBinding.filter(aFilters);
            
            this.getView().setBusy(false);
        },

        onOrderPress: function (oEvent) {
            // YOU implement navigation
            var oItem = oEvent.getSource();
            var sOrderNumber = oItem.getBindingContext().getProperty("SalesOrder");
            
            // Navigate to detail page (which YOU also must create)
            this.getOwnerComponent().getRouter().navTo("detail", {
                orderId: sOrderNumber
            });
        }
        
        // YOU write ALL other methods...
    });
});
```

**Total code YOU write: 800-5,000 lines** (depending on complexity)

**7. Test Locally**
```bash
npm install
npm start
# You see YOUR custom UI
```

**8. Deploy to SAP**
```bash
npm run deploy
```

#### Comparison

**Fiori Elements:**
- You write: 50 lines
- You get: Full app with features

**Freestyle:**
- You write: 800-5,000 lines
- You get: Exactly what you coded

---

### APPROACH 3: RAP-First Development (Backend-Driven)

```
┌─────────────────────────────────────────────────────────┐
│                    APPROACH 3                            │
│         RAP with Annotations (Backend-Driven)            │
└─────────────────────────────────────────────────────────┘

WHERE YOU WORK:
├── Backend: ADT/Eclipse (ABAP development)
└── Frontend: Auto-generated (no VS Code needed!)

WHAT YOU CREATE:
├── CDS Interface Views
├── CDS Projection Views with @UI annotations
├── Behavior Definitions
├── Behavior Implementations
└── Service Definitions

TOTAL CODE YOU WRITE: ~600-2,000 lines (ABAP)

RESULT:
Backend generates both data AND UI metadata
Frontend auto-generated from annotations
```

#### Key Difference

**Approaches 1 & 2:** You create frontend in VS Code, connect to existing backend
**Approach 3:** You create backend with UI annotations, frontend is auto-generated

#### Step-by-Step: RAP-First Development

**1. Open ADT (Eclipse)**
```
Eclipse with ABAP Development Tools
```

**2. Create CDS Interface View**

**File:** `ZI_SALESORDER.ddls`

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order - Interface'
define root view entity ZI_SALESORDER
  as select from vbak
{
  key vbeln as SalesOrder,
      erdat as OrderDate,
      bstnk as PurchaseOrderNumber,
      waerk as Currency,
      kunnr as Customer
}
```

**3. Create CDS Projection View with @UI Annotations**

**File:** `ZC_SALESORDER.ddls`

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order - Projection'
@Metadata.allowExtensions: true

@UI: {
  headerInfo: {
    typeName: 'Sales Order',
    typeNamePlural: 'Sales Orders',
    title: { value: 'SalesOrder' }
  },
  presentationVariant: [{
    sortOrder: [{ by: 'OrderDate', direction: #DESC }]
  }]
}

define root view entity ZC_SALESORDER
  provider contract transactional_query
  as projection on ZI_SALESORDER
{
      @UI.facet: [
        {
          id: 'HeaderInfo',
          purpose: #STANDARD,
          type: #IDENTIFICATION_REFERENCE,
          label: 'Order Details',
          position: 10
        }
      ]

      @UI: {
        lineItem: [{ position: 10, importance: #HIGH }],
        identification: [{ position: 10 }],
        selectionField: [{ position: 10 }]
      }
  key SalesOrder,

      @UI: {
        lineItem: [{ position: 20, importance: #HIGH }],
        identification: [{ position: 20 }],
        selectionField: [{ position: 20 }]
      }
      OrderDate,

      @UI: {
        lineItem: [{ position: 30, importance: #MEDIUM }],
        identification: [{ position: 30 }]
      }
      PurchaseOrderNumber,

      @UI: {
        lineItem: [{ position: 40 }],
        identification: [{ position: 40 }]
      }
      Currency,

      @UI: {
        identification: [{ position: 50 }]
      }
      Customer
}
```

**These @UI annotations define the UI!**

**4. Create Service Definition**

**File:** `ZSD_SALESORDER.srvd`

```abap
@EndUserText.label: 'Sales Order Service'
define service ZSD_SALESORDER {
  expose ZC_SALESORDER as SalesOrder;
}
```

**5. Create Service Binding**

**In ADT:**
```
Right-click on ZSD_SALESORDER → New Service Binding
Name: ZUI_SALESORDER
Binding Type: OData V4 - UI
Activate
Publish
```

**6. Preview - Fiori App Auto-Generated!**

**In Service Binding:**
```
Click "Preview" button
Select entity: SalesOrder
Browser opens with FULLY FUNCTIONAL Fiori app!
```

**NO VS Code! NO manifest.json creation! Just works!**

#### What Just Happened?

```
1. You wrote CDS views with @UI annotations (ABAP)
    ↓
2. Service Binding published OData service
    ↓
3. Metadata includes @UI annotations (XML)
    ↓
4. SAP Gateway automatically generates Fiori Elements app
    ↓
5. Browser loads the auto-generated app
    ↓
6. Fiori Elements reads metadata
    ↓
7. Generates UI from @UI annotations
    ↓
8. Working app with ZERO frontend code!
```

**Your @UI annotations = Frontend "code"!**

#### Adding RAP Behaviors

**7. Create Behavior Definition**

**File:** `ZI_SALESORDER.bdef`

```abap
managed implementation in class zbp_i_salesorder unique;
strict ( 2 );

define behavior for ZI_SALESORDER alias SalesOrder
persistent table zsalesorder
lock master
authorization master ( instance )
{
  create;
  update;
  delete;
  
  validation validateOrderDate on save { field OrderDate; }
  determination calculateTotal on modify { field Items; }
  
  action approve result [1] $self;
}
```

**8. Implement Behavior Class**

**File:** `ZBP_I_SALESORDER` (ABAP Class)

```abap
CLASS zbp_i_salesorder DEFINITION PUBLIC ABSTRACT FINAL 
  FOR BEHAVIOR OF zi_salesorder.
ENDCLASS.

CLASS zbp_i_salesorder IMPLEMENTATION.
ENDCLASS.

CLASS lhc_salesorder DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS validateOrderDate FOR VALIDATE ON SAVE
      IMPORTING keys FOR SalesOrder~validateOrderDate.
      
    METHODS approve FOR MODIFY
      IMPORTING keys FOR ACTION SalesOrder~approve RESULT result.
ENDCLASS.

CLASS lhc_salesorder IMPLEMENTATION.
  METHOD validateOrderDate.
    READ ENTITIES OF zi_salesorder IN LOCAL MODE
      ENTITY SalesOrder
        FIELDS ( OrderDate ) WITH CORRESPONDING #( keys )
      RESULT DATA(lt_orders).
      
    LOOP AT lt_orders INTO DATA(ls_order).
      IF ls_order-OrderDate < sy-datum.
        APPEND VALUE #( %tky = ls_order-%tky ) TO failed-salesorder.
        APPEND VALUE #(
          %tky = ls_order-%tky
          %msg = new_message_with_text(
            severity = if_abap_behv_message=>severity-error
            text = 'Order date cannot be in the past'
          )
        ) TO reported-salesorder.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD approve.
    MODIFY ENTITIES OF zi_salesorder IN LOCAL MODE
      ENTITY SalesOrder
        UPDATE FIELDS ( Status )
        WITH VALUE #( FOR key IN keys ( %tky = key-%tky Status = 'APPROVED' ) ).
        
    READ ENTITIES OF zi_salesorder IN LOCAL MODE
      ENTITY SalesOrder ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_orders).
      
    result = VALUE #( FOR order IN lt_orders ( %tky = order-%tky %param = order ) ).
  ENDMETHOD.
ENDCLASS.
```

**Now you have:**
- Auto-generated UI (from @UI annotations)
- Backend logic (validations, actions)
- Complete transactional app
- All without touching VS Code!

#### If You Want to Customize the Frontend

**Option 1: Add more @UI annotations** (in ADT)
```abap
@UI: {
  lineItem: [{ 
    position: 50, 
    importance: #HIGH,
    criticality: 'StatusCriticality'  // ← Add features via annotations
  }]
}
Status,
```

**Option 2: Create VS Code app that extends it**
```
VS Code → Create Fiori Elements app
Point to same OData service
Add custom extensions
Deploy as separate app
```

---

## Complete Comparison Matrix

| Aspect | Fiori Elements (VS Code) | Freestyle (VS Code) | RAP-First (ADT) |
|--------|-------------------------|---------------------|-----------------|
| **Primary Tool** | VS Code | VS Code | ADT/Eclipse |
| **Language** | JavaScript/JSON | JavaScript/XML | ABAP |
| **Code Location** | Frontend files | Frontend files | Backend CDS |
| **Lines of Code** | 50-200 | 800-5,000 | 600-2,000 |
| **UI Definition** | Via backend metadata | Via custom views | Via @UI annotations |
| **Flexibility** | Limited | Complete | Limited |
| **Development Speed** | ⚡⚡⚡ Very Fast | 🐢 Slow | ⚡⚡ Fast |
| **Deployment** | BSP to SAP | BSP to SAP | Built-in to ABAP |
| **When to Use** | Standard CRUD | Unique UX | RAP-based apps |
| **Backend Required** | Yes (existing) | Yes (existing) | You create it |
| **Offline Development** | ✅ Yes | ✅ Yes | ⚠️ Requires SAP connection |

---

## When to Use Which Approach

### Use Approach 1 (Fiori Elements + VS Code)

```
✅ You have existing OData service (RAP/Gateway/CAP)
✅ You need standard CRUD functionality
✅ You want fastest development
✅ You prefer frontend development
✅ UI fits List Report/Object Page pattern
```

**Example:** Display sales orders from existing SAP backend

### Use Approach 2 (Freestyle + VS Code)

```
✅ You have existing OData service
✅ You need completely custom UI
✅ Standard templates don't fit your needs
✅ You have strong UI5 expertise
✅ You need special visualizations
```

**Example:** Interactive warehouse map, custom dashboard

### Use Approach 3 (RAP-First)

```
✅ You're creating NEW application from scratch
✅ You're comfortable with ABAP
✅ You want backend and frontend in one stack
✅ You need transactional capabilities
✅ Standard Fiori UI is acceptable
```

**Example:** New transactional app for S/4HANA

---

## The Complete Development Paths

### Path 1: Backend Already Exists → Frontend with VS Code

```
Scenario: You have SAP system with OData service

Steps:
1. VS Code → Create Fiori Elements or Freestyle app
2. Point to existing OData service
3. Develop frontend locally
4. Deploy to SAP as BSP application
5. Users access via Fiori Launchpad

Tools: VS Code + SAP Fiori Tools extension
```

### Path 2: Nothing Exists → RAP-First Development

```
Scenario: Building new app from scratch in S/4HANA

Steps:
1. ADT → Create CDS views with @UI annotations
2. Create RAP behaviors
3. Create service definition and binding
4. Preview → Auto-generated Fiori app appears!
5. Optionally: Extend with VS Code if needed

Tools: ADT/Eclipse + (optional) VS Code
```

### Path 3: Hybrid Approach

```
Scenario: RAP backend + Custom frontend enhancements

Steps:
1. ADT → Create RAP backend with @UI annotations
2. Publish service
3. VS Code → Create Fiori Elements app pointing to RAP service
4. Add custom extensions in VS Code
5. Deploy to SAP

Tools: ADT/Eclipse + VS Code
```

---

## Practical Examples for Each Approach

### Example 1: Fiori Elements with VS Code

**Goal:** Display existing sales orders

**What you do:**
```bash
# VS Code
yo @sap/fiori
# Choose: List Report
# Point to: Existing OData service
# Time: 5 minutes
```

**What you get:**
- manifest.json (40 lines)
- Working app with filters, tables, export
- Deploy to SAP

### Example 2: Freestyle with VS Code

**Goal:** Custom sales dashboard with charts

**What you do:**
```bash
# VS Code
yo @sap/fiori
# Choose: SAPUI5 Freestyle
# Point to: Existing OData service
# Then: Write 2,000 lines of custom code
# Time: 2 weeks
```

**What you get:**
- Complete custom UI
- Charts, KPIs, custom interactions
- Full control

### Example 3: RAP-First

**Goal:** New employee onboarding app

**What you do:**
```abap
// ADT
// Create: CDS views (400 lines)
// Add: @UI annotations (200 lines)
// Create: Behaviors (300 lines)
// Total: 900 lines ABAP
// Time: 3-5 days
```

**What you get:**
- Complete app (backend + frontend)
- Auto-generated Fiori UI
- Transactional capabilities
- No frontend coding needed

---

## Summary: Three Distinct Approaches

### 1️⃣ **Fiori Elements with VS Code**
- **Where:** VS Code (frontend development)
- **What:** Minimal configuration files
- **Result:** Template-based UI consuming existing backend
- **Code:** 50-200 lines JavaScript/JSON

### 2️⃣ **Freestyle SAPUI5 with VS Code**
- **Where:** VS Code (frontend development)
- **What:** Complete custom app
- **Result:** Fully custom UI consuming existing backend
- **Code:** 800-5,000 lines JavaScript/XML

### 3️⃣ **RAP with Annotations (ADT)**
- **Where:** ADT/Eclipse (backend development)
- **What:** CDS views with @UI annotations
- **Result:** Auto-generated Fiori UI + backend logic
- **Code:** 600-2,000 lines ABAP

**Key Distinction:**
- Approaches 1 & 2: Frontend development (VS Code)
- Approach 3: Backend development (ADT) → Frontend auto-generated

**They can be combined:**
- RAP backend (ADT) + Fiori Elements frontend (auto or VS Code extended)
- Gateway backend (SE80) + Freestyle frontend (VS Code)
- CAP backend (VS Code) + Fiori Elements frontend (VS Code)

Now you have the complete picture of all three approaches! 🎉

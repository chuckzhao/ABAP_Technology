# RAP Annotations - What Really Gets Generated

## The Short Answer

**Does RAP generate manifest.json?**
- No, not in the way you might think
- RAP generates OData **metadata** (XML), not manifest files (JSON)
- When you click "Preview" in Service Binding, you're using SAP's **generic built-in Fiori Elements app**
- That generic app HAS a manifest.json, but it's SAP's, not yours

**Can it be changed?**
- The SAP generic manifest.json: No, you don't have access to change it
- But you CAN create your own app with your own manifest.json that consumes the RAP service

Let me explain what actually happens step by step.

---

## What RAP Actually Generates

### Step 1: You Write CDS with @UI Annotations

**Your ABAP code in ADT:**

```abap
@EndUserText.label: 'Sales Order - Projection'
@Metadata.allowExtensions: true

@UI: {
  headerInfo: {
    typeName: 'Sales Order',
    typeNamePlural: 'Sales Orders'
  }
}

define root view entity ZC_SALESORDER
  provider contract transactional_query
  as projection on ZI_SALESORDER
{
      @UI: {
        lineItem: [{ position: 10, importance: #HIGH }],
        identification: [{ position: 10 }],
        selectionField: [{ position: 10 }]
      }
  key SalesOrder,

      @UI: {
        lineItem: [{ position: 20, importance: #HIGH }],
        identification: [{ position: 20 }]
      }
      OrderDate,
      
      @UI: {
        lineItem: [{ position: 30 }]
      }
      Currency
}
```

**This is ABAP code in the backend. No JavaScript. No JSON. No manifest.json.**

### Step 2: Service Binding Publishes OData Service

When you create and publish a Service Binding (ZUI_SALESORDER), SAP Gateway creates an OData V4 service endpoint:

```
Service URL:
/sap/opu/odata4/sap/zui_salesorder/srvd/sap/zsd_salesorder/0001/
```

### Step 3: The OData Metadata Contains Your Annotations

When you call the metadata endpoint, you get XML that includes your @UI annotations:

```
GET /sap/opu/odata4/sap/zui_salesorder/srvd/sap/zsd_salesorder/0001/$metadata
```

**Response (XML, not JSON):**

```xml
<?xml version="1.0" encoding="utf-8"?>
<edmx:Edmx Version="4.0">
  <edmx:DataServices>
    <Schema Namespace="SAP">
      
      <!-- Your entity definition -->
      <EntityType Name="SalesOrderType">
        <Key>
          <PropertyRef Name="SalesOrder"/>
        </Key>
        
        <!-- Properties -->
        <Property Name="SalesOrder" Type="Edm.String" MaxLength="10"/>
        <Property Name="OrderDate" Type="Edm.Date"/>
        <Property Name="Currency" Type="Edm.String" MaxLength="3"/>
        
        <!-- UI Annotations embedded in metadata -->
        <Annotation Term="UI.HeaderInfo">
          <Record>
            <PropertyValue Property="TypeName" String="Sales Order"/>
            <PropertyValue Property="TypeNamePlural" String="Sales Orders"/>
          </Record>
        </Annotation>
        
        <Annotation Term="UI.LineItem">
          <Collection>
            <Record Type="UI.DataField">
              <PropertyValue Property="Value" Path="SalesOrder"/>
              <PropertyValue Property="Label" String="Sales Order"/>
              <Annotation Term="UI.Importance" EnumMember="UI.ImportanceType/High"/>
            </Record>
            <Record Type="UI.DataField">
              <PropertyValue Property="Value" Path="OrderDate"/>
              <PropertyValue Property="Label" String="Order Date"/>
              <Annotation Term="UI.Importance" EnumMember="UI.ImportanceType/High"/>
            </Record>
            <Record Type="UI.DataField">
              <PropertyValue Property="Value" Path="Currency"/>
              <PropertyValue Property="Label" String="Currency"/>
            </Record>
          </Collection>
        </Annotation>
        
        <Annotation Term="UI.SelectionFields">
          <Collection>
            <PropertyValue Property="PropertyPath" Path="SalesOrder"/>
          </Collection>
        </Annotation>
        
        <Annotation Term="UI.Identification">
          <Collection>
            <Record Type="UI.DataField">
              <PropertyValue Property="Value" Path="SalesOrder"/>
            </Record>
            <Record Type="UI.DataField">
              <PropertyValue Property="Value" Path="OrderDate"/>
            </Record>
          </Collection>
        </Annotation>
      </EntityType>
      
      <EntityContainer Name="EntityContainer">
        <EntitySet Name="SalesOrder" EntityType="SAP.SalesOrderType"/>
      </EntityContainer>
    </Schema>
  </edmx:DataServices>
</edmx:Edmx>
```

**This is what RAP generates: OData metadata XML with embedded UI annotations.**

**Still no manifest.json!**

---

## What Happens When You Click "Preview"

### The Behind-the-Scenes Process

When you click the "Preview" button in the Service Binding:

```
1. Service Binding opens a URL in your browser
   ↓
2. URL points to SAP's built-in generic Fiori Elements app
   ↓
3. That generic app has its own manifest.json (SAP's, not yours)
   ↓
4. The generic app is configured to read from your OData service
   ↓
5. It fetches the $metadata (the XML with annotations)
   ↓
6. Fiori Elements framework reads the UI annotations
   ↓
7. Generates the UI dynamically
   ↓
8. You see a working Fiori app!
```

### The Actual URL Structure

When you click Preview, the URL looks something like this:

```
https://your-sap-system.com/sap/bc/ui2/flp?
  sap-client=001&
  sap-ui-app-id-hint=com.sap.gateway.srvd.v4.zsd_salesorder.v0001.display&
  sap-ui-xx-viewCache=false#
  com.sap.gateway.srvd.v4.zsd_salesorder.v0001.display-manage
```

**Breaking this down:**

```
/sap/bc/ui2/flp
├── This is SAP's Fiori Launchpad
│
com.sap.gateway.srvd.v4.zsd_salesorder.v0001.display
├── This is SAP's auto-generated app ID
│
com.sap.gateway.srvd.v4.zsd_salesorder.v0001.display-manage
└── This is the app + intent
```

### Where is the manifest.json?

The manifest.json exists, but it's part of **SAP's generic Fiori Elements application**, not something you created or own. It's located somewhere deep in the SAP Gateway infrastructure:

```
Approximate location (you can't directly access it):
/sap/bc/ui5_ui5/sap/fie_template/  ← SAP's Fiori Elements template app

Inside that app:
├── manifest.json              ← SAP's generic manifest
├── Component.js              ← SAP's component
└── Other SAP framework files
```

**SAP's Generic manifest.json (conceptually):**

```json
{
  "sap.app": {
    "id": "com.sap.gateway.srvd.template",
    "type": "application",
    "title": "Generic Fiori Elements App"
  },
  "sap.ui5": {
    "dependencies": {
      "libs": {
        "sap.fe.templates": {}
      }
    },
    "routing": {
      "config": {
        "async": true
      },
      "routes": [
        {
          "pattern": "",
          "name": "ListReport",
          "target": "ListReport"
        }
      ],
      "targets": {
        "ListReport": {
          "type": "Component",
          "name": "sap.fe.templates.ListReport",
          "options": {
            "settings": {
              "contextPath": "{dynamically determined}",
              "entitySet": "{dynamically determined}"
            }
          }
        }
      }
    }
  }
}
```

**The key point:** The service URL, entity set, and other specifics are determined **dynamically at runtime** based on which Service Binding you're previewing.

---

## Can You Change This Manifest.json?

### Direct Answer: No

You **cannot** directly edit SAP's generic manifest.json that's used in the Preview function because:

1. It's part of SAP's framework code (not your application)
2. It's shared across all RAP service previews
3. It's deep in the system infrastructure
4. Modifying it would affect all RAP previews globally (bad idea!)

### But You Have Alternatives!

---

## Alternative 1: Create Your Own Fiori Elements App

Instead of using the Preview, create your own Fiori Elements app in VS Code that points to your RAP service. Then you get your own manifest.json that you can customize!

### How to Do This

**Step 1: Your RAP Service Exists**

You already have:
- CDS views with @UI annotations (in ADT)
- Published Service Binding
- OData service URL

**Step 2: Create Fiori Elements App in VS Code**

```bash
# In VS Code
Press F1 → "Fiori: Open Application Generator"

Template: List Report Page
Data Source: Connect to OData Service
Service URL: /sap/opu/odata4/sap/zui_salesorder/srvd/sap/zsd_salesorder/0001/
Entity: SalesOrder

Project name: salesorder-custom
```

**Generator creates YOUR manifest.json:**

```json
{
  "_version": "1.49.0",
  "sap.app": {
    "id": "com.mycompany.salesorder.custom",  // ← Your app ID
    "type": "application",
    "title": "Sales Order Display - Custom",
    "dataSources": {
      "mainService": {
        "uri": "/sap/opu/odata4/sap/zui_salesorder/srvd/sap/zsd_salesorder/0001/",  // ← Points to your RAP service
        "type": "OData",
        "settings": {
          "odataVersion": "4.0"
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
    "models": {
      "": {
        "dataSource": "mainService",
        "settings": {
          "timeout": 120000  // ← YOU can change this!
        }
      }
    },
    "routing": {
      "targets": {
        "SalesOrderList": {
          "type": "Component",
          "name": "sap.fe.templates.ListReport",
          "options": {
            "settings": {
              "contextPath": "/SalesOrder",
              "variantManagement": "Page",  // ← YOU can change this!
              "controlConfiguration": {
                "@com.sap.vocabularies.UI.v1.LineItem": {
                  "tableSettings": {
                    "type": "ResponsiveTable"  // ← YOU can change this!
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
```

**Now THIS manifest.json is YOURS and you can change it!**

**Step 3: Customize Your Manifest**

You can now add customizations that the RAP Preview doesn't support:

```json
{
  "sap.ui5": {
    "models": {
      "": {
        "settings": {
          "timeout": 180000,  // ← Increase timeout
          "earlyRequests": false  // ← Disable early requests
        }
      }
    },
    "routing": {
      "targets": {
        "SalesOrderList": {
          "options": {
            "settings": {
              "controlConfiguration": {
                "@com.sap.vocabularies.UI.v1.LineItem": {
                  "tableSettings": {
                    "type": "GridTable",  // ← Change table type
                    "selectionMode": "Multi"
                  },
                  "actions": {
                    "CustomExport": {  // ← Add custom button
                      "press": ".extension.custom.controller.onExport",
                      "text": "Custom Export"
                    }
                  }
                }
              }
            }
          }
        }
      }
    },
    "extends": {
      "extensions": {
        "sap.ui.controllerExtensions": {  // ← Add controller extensions
          "sap.fe.templates.ListReport.ListReportController": {
            "controllerName": "com.mycompany.salesorder.custom.ext.controller.ListReportExt"
          }
        }
      }
    }
  }
}
```

**Step 4: Deploy Your Custom App**

```bash
npm run deploy
```

Now you have:
- Your RAP backend (with @UI annotations)
- Your custom Fiori Elements app (with customizable manifest.json)
- Full control over frontend configuration

---

## Alternative 2: Metadata Extensions

If you want to keep using the RAP Preview but need more control over UI, use Metadata Extensions instead of editing manifest.json.

### Create Metadata Extension

**File:** `ZC_SALESORDER.ddlx` (Metadata Extension)

```abap
@Metadata.layer: #CORE
@UI: {
  headerInfo: {
    typeName: 'Sales Order',
    typeNamePlural: 'Sales Orders',
    title: { value: 'SalesOrder' },
    description: { value: 'PurchaseOrderNumber' }
  }
}
annotate view ZC_SALESORDER with
{
  @UI.facet: [
    {
      id: 'HeaderInfo',
      purpose: #STANDARD,
      type: #IDENTIFICATION_REFERENCE,
      label: 'Order Details',
      position: 10
    },
    {
      id: 'Items',
      purpose: #STANDARD,
      type: #LINEITEM_REFERENCE,
      label: 'Order Items',
      position: 20,
      targetElement: '_Items'
    }
  ]
  SalesOrder;
  
  @UI: {
    lineItem: [{ 
      position: 10, 
      importance: #HIGH,
      label: 'Order Number'
    }],
    selectionField: [{ position: 10 }]
  }
  SalesOrder;
  
  @UI: {
    lineItem: [{ 
      position: 20, 
      importance: #HIGH,
      criticality: 'StatusCriticality'  // ← Add dynamic coloring
    }],
    identification: [{ position: 20 }]
  }
  OrderDate;
}
```

**These annotations affect the Preview app without needing to change manifest.json!**

But you're still limited to what @UI annotations can express. For manifest-level settings (like timeout, table type preferences), you need your own app.

---

## The Complete Picture

### What RAP Generates

```
RAP (ADT):
CDS View with @UI annotations
    ↓
Service Binding publishes OData service
    ↓
OData Metadata (XML) with UI annotations
    ↓
    ├─→ Option 1: Preview uses SAP's generic app
    │   └─→ manifest.json: SAP's (can't change)
    │
    └─→ Option 2: Create your own app in VS Code
        └─→ manifest.json: YOURS (can change!)
```

### Comparison

| Aspect | RAP Preview (SAP Generic) | Custom App (VS Code) |
|--------|---------------------------|---------------------|
| **manifest.json exists?** | Yes, but it's SAP's | Yes, and it's yours |
| **Can you change it?** | No | Yes |
| **Who owns it?** | SAP Gateway framework | You |
| **Location** | SAP infrastructure | Your BSP application |
| **Customization** | Via @UI annotations only | Via manifest.json + extensions |
| **Setup effort** | Zero (automatic) | 5-10 minutes |
| **Deployment** | Built-in | Deploy to SAP |
| **Best for** | Quick preview/prototyping | Production apps |

---

## Practical Examples

### Example 1: You Want to Change Timeout

**Problem:** RAP Preview times out after 30 seconds

**Solution 1: Can't change SAP's manifest**
```
❌ You cannot modify the timeout in the Preview
```

**Solution 2: Create your own app**
```
✅ VS Code → Create Fiori Elements app
✅ Edit manifest.json → Add timeout: 120000
✅ Deploy to SAP
✅ Use your app instead of Preview
```

### Example 2: You Want to Change Table Type

**Problem:** RAP Preview uses ResponsiveTable, you want GridTable

**Solution 1: Can't change in Preview**
```
❌ Preview always uses default table type
```

**Solution 2: Create your own app**
```json
✅ In YOUR manifest.json:
{
  "controlConfiguration": {
    "@com.sap.vocabularies.UI.v1.LineItem": {
      "tableSettings": {
        "type": "GridTable"  // ← Change it!
      }
    }
  }
}
```

### Example 3: You Want to Add Custom Button

**Problem:** Need custom export functionality

**Solution 1: Can't add to Preview**
```
❌ Preview doesn't support custom actions
   (only what @UI annotations provide)
```

**Solution 2: Create your own app**
```json
✅ In YOUR manifest.json:
{
  "actions": {
    "CustomExport": {
      "press": ".extension.controller.onExport",
      "text": "My Custom Export"
    }
  }
}
```

---

## Best Practices

### Use RAP Preview For:
- ✅ Quick development/testing
- ✅ Validating @UI annotations
- ✅ Demos to stakeholders
- ✅ Prototyping

### Create Custom App (VS Code) For:
- ✅ Production applications
- ✅ Apps needing manifest-level customization
- ✅ Apps with custom extensions
- ✅ Apps requiring specific Fiori Launchpad integration

---

## The Development Workflow

### Recommended Approach

```
Phase 1: RAP Development (ADT)
├── Create CDS views with @UI annotations
├── Create behaviors, validations, actions
├── Publish service binding
└── Use Preview to validate

Phase 2: Decide
├── Is Preview good enough?
│   └── YES → Use Preview, done!
│
└── Need customization?
    └── YES → Create custom app in VS Code
        ├── Points to same RAP service
        ├── Customize manifest.json
        ├── Add extensions if needed
        └── Deploy to SAP
```

### The Hybrid Advantage

By combining RAP + Custom Fiori Elements App:

```
RAP (Backend):
✅ @UI annotations define base UI structure
✅ Behaviors, validations, actions
✅ Business logic

Custom App (Frontend):
✅ manifest.json customizations
✅ Timeout, table type, etc.
✅ Custom buttons, actions
✅ Controller extensions
✅ Custom fragments

Result:
✅ Best of both worlds!
✅ Backend-driven UI + Frontend customization
```

---

## Summary

### Does RAP generate manifest.json?

**Direct answer:** No, RAP generates OData metadata (XML) with embedded UI annotations.

**What you see in Preview:** SAP's generic Fiori Elements app reads that metadata and generates the UI. That app has a manifest.json, but it's SAP's framework file, not something RAP generated for you.

### Can it be changed?

**SAP's generic manifest.json:** No, you don't have access to modify it.

**Your own manifest.json:** Yes! Create a Fiori Elements app in VS Code that consumes your RAP service, then you get a customizable manifest.json.

### The Mental Model

Think of it this way:

```
RAP Annotations = Recipe ingredients list
    ↓
OData Metadata = Recipe card (with ingredient list)
    ↓
    ├─→ SAP Generic App = Restaurant's standard menu
    │   (You eat what they cook, can't change recipe)
    │
    └─→ Your Custom App = Your own kitchen
        (You follow the recipe but can adjust to taste!)
```

RAP gives you the data and UI structure (the recipe), but you need your own app (your own kitchen) to have full control over how it's presented!

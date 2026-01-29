# SAP Fiori - Complete Interview Guide

A comprehensive guide covering SAP Fiori concepts, architecture, and best practices for interview preparation.

---

## Table of Contents

1. [What is SAP Fiori](#1-what-is-sap-fiori)
2. [Fiori Design Principles](#2-fiori-design-principles)
3. [Fiori Architecture](#3-fiori-architecture)
4. [Fiori App Types](#4-fiori-app-types)
5. [SAPUI5 Framework](#5-sapui5-framework)
6. [Fiori Elements](#6-fiori-elements)
7. [Fiori Launchpad](#7-fiori-launchpad)
8. [OData Services](#8-odata-services)
9. [CDS Views for Fiori](#9-cds-views-for-fiori)
10. [RAP and Fiori](#10-rap-and-fiori)
11. [Fiori Tools and Development](#11-fiori-tools-and-development)
12. [Security and Authorization](#12-security-and-authorization)
13. [Performance Optimization](#13-performance-optimization)
14. [Common Interview Questions](#14-common-interview-questions)

---

## 1. What is SAP Fiori

### Definition
SAP Fiori is SAP's **user experience (UX) design system** that provides a set of design principles, tools, and guidelines for creating modern, intuitive, and responsive business applications.

### Key Characteristics
- **Role-based**: Apps designed for specific user roles
- **Responsive**: Works on desktop, tablet, and mobile
- **Simple**: Focus on key tasks, reduce complexity
- **Coherent**: Consistent design across all apps
- **Instant value**: Productive from first use

### Evolution
| Generation | Technology | Description |
|------------|------------|-------------|
| Fiori 1.0 | SAPUI5 | Initial release, transactional apps |
| Fiori 2.0 | SAPUI5 | Enhanced UX, Launchpad improvements |
| Fiori 3.0 | SAPUI5/Fiori Elements | Intelligent, conversational, integrated |

### Fiori vs SAPUI5
| Aspect | SAPUI5 | Fiori |
|--------|--------|-------|
| What | JavaScript UI framework | UX design system + apps |
| Focus | Technical implementation | User experience |
| Scope | Development toolkit | Complete solution |
| Relation | Foundation technology | Built on SAPUI5 |

---

## 2. Fiori Design Principles

### The Five Key Principles

#### 1. Role-Based
- Apps designed for specific user roles
- Show only relevant information
- Personalization options
- Example: Purchaser sees purchase orders, not sales orders

#### 2. Responsive
- Adapts to device screen size
- Desktop, tablet, mobile support
- Fluid layouts using FlexBox
- Breakpoints: Phone (<600px), Tablet (600-1024px), Desktop (>1024px)

#### 3. Simple
- 1-1-3 Rule: 1 user, 1 use case, 3 screens max
- Reduce clicks and navigation
- Progressive disclosure
- Clear visual hierarchy

#### 4. Coherent
- Consistent navigation patterns
- Uniform terminology
- Standard icons and colors
- Shared shell (Fiori Launchpad)

#### 5. Delightful
- Smooth animations
- Immediate feedback
- Intuitive interactions
- Aesthetically pleasing

### SAP Fiori Design Guidelines
- **Quartz Light Theme**: Default theme for S/4HANA
- **Horizon Theme**: Latest design evolution
- **Typography**: 72 font family
- **Color Palette**: Semantic colors for status indication
- **Spacing**: 16px base unit

---

## 3. Fiori Architecture

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Presentation Layer                        │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │   Browser   │  │   Mobile    │  │   Desktop   │         │
│  │  (SAPUI5)   │  │    App      │  │    App      │         │
│  └─────────────┘  └─────────────┘  └─────────────┘         │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│                   SAP Fiori Launchpad                        │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐   │
│  │  Tiles   │  │Navigation│  │  Shell   │  │  Plugins │   │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘   │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│                    Gateway Layer                             │
│  ┌─────────────────────────────────────────────────────┐   │
│  │              SAP Gateway / OData Services            │   │
│  │         (REST-based, JSON/XML, CRUD operations)      │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│                    Backend Layer                             │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐   │
│  │   ABAP   │  │   CDS    │  │   RAP    │  │   BAPI   │   │
│  │  Classes │  │  Views   │  │  BO      │  │  /RFC    │   │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘   │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│                    Database Layer                            │
│  ┌─────────────────────────────────────────────────────┐   │
│  │                   SAP HANA                           │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

### Communication Flow
1. User interacts with UI (SAPUI5)
2. UI sends OData request to Gateway
3. Gateway routes to backend service
4. Backend processes request (ABAP/RAP)
5. Data retrieved from HANA
6. Response sent back through layers
7. UI updates with new data

---

## 4. Fiori App Types

### 1. Transactional Apps
- **Purpose**: Create, update, delete business data
- **Examples**: Create Purchase Order, Manage Sales Orders
- **Characteristics**:
  - Full CRUD operations
  - Form-based input
  - Validation and error handling
  - Draft handling support

### 2. Analytical Apps
- **Purpose**: Analyze business data, KPIs, trends
- **Examples**: Sales Performance, Financial Overview
- **Characteristics**:
  - Charts and visualizations
  - Real-time analytics
  - Drill-down capabilities
  - Integration with Smart Business

### 3. Fact Sheet Apps
- **Purpose**: Display contextual information
- **Examples**: Customer Fact Sheet, Product Details
- **Characteristics**:
  - Read-only display
  - 360-degree view
  - Quick links to related objects
  - Overview pages

### 4. Fiori Elements App Types

| Template | Use Case | Features |
|----------|----------|----------|
| **List Report** | Browse and search list of items | Filter bar, table, actions |
| **Object Page** | View/edit single object details | Header, sections, facets |
| **Worklist** | Task-oriented item processing | Simple list with actions |
| **Overview Page** | Dashboard with multiple cards | KPIs, charts, lists |
| **Analytical List Page** | Combined analytics and list | Chart + table hybrid |

---

## 5. SAPUI5 Framework

### What is SAPUI5?
- JavaScript UI framework by SAP
- Based on jQuery, with enterprise features
- MVC architecture
- Rich control library
- Theming support

### Core Concepts

#### MVC Architecture
```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│    Model    │◄───►│  Controller │◄───►│    View     │
│  (Data)     │     │  (Logic)    │     │    (UI)     │
└─────────────┘     └─────────────┘     └─────────────┘
```

#### Model Types
| Model | Description | Use Case |
|-------|-------------|----------|
| **JSONModel** | Client-side JSON data | Local data, UI state |
| **ODataModel** | Server-side OData | Backend integration |
| **ResourceModel** | i18n texts | Translations |
| **XMLModel** | XML data | Legacy support |

#### Data Binding Types
| Type | Syntax | Description |
|------|--------|-------------|
| **Property Binding** | `{/path}` | Bind single value |
| **Aggregation Binding** | `{path: '/items'}` | Bind collection |
| **Expression Binding** | `{= ${field} > 0}` | Calculated values |
| **Composite Binding** | `{parts: [...]}` | Multiple sources |

#### Binding Modes
- **OneWay**: Model → View only
- **TwoWay**: Model ↔ View (default for OData)
- **OneTime**: Initial value only

### Key SAPUI5 Controls

#### Layout Controls
- `sap.m.Page` - Basic page structure
- `sap.m.App` - Application container
- `sap.m.SplitApp` - Master-detail layout
- `sap.ui.layout.form.SimpleForm` - Form layout
- `sap.ui.layout.Grid` - Responsive grid

#### Display Controls
- `sap.m.Text` - Plain text
- `sap.m.Label` - Form labels
- `sap.m.Title` - Headings
- `sap.m.ObjectStatus` - Status with color
- `sap.m.ObjectNumber` - Formatted numbers

#### Input Controls
- `sap.m.Input` - Text input
- `sap.m.Select` - Dropdown
- `sap.m.ComboBox` - Searchable dropdown
- `sap.m.DatePicker` - Date selection
- `sap.m.CheckBox` - Boolean input

#### List Controls
- `sap.m.List` - Simple list
- `sap.m.Table` - Responsive table
- `sap.ui.table.Table` - Grid table (large data)
- `sap.m.Tree` - Hierarchical data

### Component-Based Architecture

```javascript
// Component.js
sap.ui.define([
    "sap/ui/core/UIComponent"
], function(UIComponent) {
    return UIComponent.extend("my.app.Component", {
        metadata: {
            manifest: "json"  // descriptor
        },
        init: function() {
            UIComponent.prototype.init.apply(this, arguments);
            this.getRouter().initialize();
        }
    });
});
```

### Routing
- Define routes in manifest.json
- Pattern matching for URLs
- Target views for each route
- Navigation with `navTo()`

---

## 6. Fiori Elements

### What is Fiori Elements?
- **Template-based** UI development
- **Metadata-driven** rendering
- **Annotation-based** configuration
- Minimal or no JavaScript coding

### Benefits
- **Faster development**: 70% less code
- **Consistent UX**: SAP design guidelines built-in
- **Automatic updates**: Framework improvements apply automatically
- **Reduced maintenance**: Less custom code

### Architecture
```
┌─────────────────────────────────────────┐
│           Fiori Elements App            │
├─────────────────────────────────────────┤
│    UI Annotations (CDS/Local)           │
├─────────────────────────────────────────┤
│    Fiori Elements Templates             │
├─────────────────────────────────────────┤
│    OData Service (V2/V4)                │
├─────────────────────────────────────────┤
│    CDS Views + RAP                      │
└─────────────────────────────────────────┘
```

### Key UI Annotations

#### Header Information
```cds
@UI.headerInfo: {
  typeName: 'Sales Order',
  typeNamePlural: 'Sales Orders',
  title: { value: 'SalesOrderID' },
  description: { value: 'CustomerName' }
}
```

#### Line Item (Table Columns)
```cds
@UI.lineItem: [
  { position: 10, importance: #HIGH, label: 'Order ID' },
  { position: 20, importance: #MEDIUM }
]
```

#### Selection Fields (Filters)
```cds
@UI.selectionField: [{ position: 10 }]
```

#### Identification (Object Page Fields)
```cds
@UI.identification: [{ position: 10 }]
```

#### Facets (Object Page Sections)
```cds
@UI.facet: [
  {
    id: 'GeneralInfo',
    type: #IDENTIFICATION_REFERENCE,
    label: 'General Information',
    position: 10
  },
  {
    id: 'Items',
    type: #LINEITEM_REFERENCE,
    label: 'Items',
    position: 20,
    targetElement: '_Items'
  }
]
```

#### Field Groups
```cds
@UI.fieldGroup: [{ qualifier: 'OrderData', position: 10 }]
```

### Floorplans Comparison

| Floorplan | Best For | Key Features |
|-----------|----------|--------------|
| List Report | Browsing large datasets | Smart filter bar, table variants |
| Object Page | Viewing/editing single record | Header, sections, sub-objects |
| Worklist | Task processing | Simple list, quick actions |
| Overview Page | Executive dashboards | Cards, KPIs, charts |
| Analytical List Page | Data analysis | Visual filters, chart-table combo |

---

## 7. Fiori Launchpad

### What is Fiori Launchpad (FLP)?
- **Central entry point** for all Fiori apps
- **Shell** that hosts applications
- **Role-based** tile configuration
- **Personalization** support

### Key Components

#### Tiles
| Type | Description | Use Case |
|------|-------------|----------|
| **Static** | Fixed content | Navigation only |
| **Dynamic** | Real-time KPI | Show counts/amounts |
| **News** | Scrolling content | Announcements |
| **Custom** | Custom rendering | Special requirements |

#### Tile Configuration (Target Mapping)
- **Semantic Object**: Business entity (e.g., SalesOrder)
- **Action**: Operation (e.g., display, create)
- **Parameters**: Context data
- **Target**: App component or URL

#### Catalogs and Groups
- **Catalog**: Collection of available tiles (admin-defined)
- **Group**: User's arranged tiles (personalized)
- **Role**: Determines catalog access

### Launchpad Configuration

#### Backend (ABAP)
1. **Transaction /UI2/FLPD_CUST**: Configure catalogs/groups
2. **Transaction /UI2/SEMOBJ**: Define semantic objects
3. **Transaction PFCG**: Assign catalogs to roles

#### Business App Studio / Fiori Tools
- Launchpad sandbox for development
- Preview apps in FLP context
- Configure intent-based navigation

### Intent-Based Navigation
```
#SemanticObject-action?param1=value1&param2=value2

Example:
#SalesOrder-display?SalesOrderID=12345
```

### Cross-App Navigation
```javascript
// In controller
var oCrossAppNav = sap.ushell.Container.getService("CrossApplicationNavigation");
oCrossAppNav.toExternal({
    target: {
        semanticObject: "SalesOrder",
        action: "display"
    },
    params: {
        SalesOrderID: "12345"
    }
});
```

---

## 8. OData Services

### What is OData?
- **Open Data Protocol**
- RESTful API standard
- JSON or XML format
- CRUD operations via HTTP methods

### OData Versions in SAP
| Version | Characteristics | Use Case |
|---------|----------------|----------|
| **V2** | Mature, widely used | Classic Fiori apps |
| **V4** | Modern, richer features | New RAP-based apps |

### HTTP Methods
| Method | Operation | OData Equivalent |
|--------|-----------|------------------|
| GET | Read | Query/Read |
| POST | Create | Create |
| PUT | Full update | Update |
| PATCH | Partial update | Update |
| DELETE | Delete | Delete |

### OData Query Options
| Option | Purpose | Example |
|--------|---------|---------|
| `$filter` | Filter results | `$filter=Status eq 'A'` |
| `$select` | Choose fields | `$select=ID,Name` |
| `$expand` | Include associations | `$expand=to_Items` |
| `$orderby` | Sort results | `$orderby=Date desc` |
| `$top` | Limit results | `$top=10` |
| `$skip` | Pagination | `$skip=20` |
| `$count` | Include count | `$count=true` |

### Service Document
```
https://server/sap/opu/odata/sap/SERVICE_NAME/
```

### Metadata Document
```
https://server/sap/opu/odata/sap/SERVICE_NAME/$metadata
```

### SEGW Transaction (OData V2)
1. Create project
2. Import data model (CDS, DDIC, RFC)
3. Generate runtime objects
4. Implement CRUD methods
5. Register and activate service

### RAP Service Binding (OData V4)
1. Create CDS views
2. Define behavior
3. Create service definition
4. Create service binding
5. Activate

---

## 9. CDS Views for Fiori

### CDS Annotation Categories

#### UI Annotations
Control how data appears in Fiori Elements

```cds
@UI.headerInfo: { ... }
@UI.lineItem: [{ ... }]
@UI.selectionField: [{ ... }]
@UI.identification: [{ ... }]
@UI.facet: [{ ... }]
@UI.fieldGroup: [{ ... }]
@UI.chart: { ... }
@UI.presentationVariant: { ... }
@UI.selectionVariant: { ... }
```

#### Search Annotations
Enable search functionality

```cds
@Search.searchable: true
@Search.defaultSearchElement: true
@Search.fuzzinessThreshold: 0.8
```

#### Consumption Annotations
Configure value helps and filters

```cds
@Consumption.valueHelpDefinition: [{
  entity: { name: 'ZI_Customer', element: 'CustomerID' }
}]
@Consumption.filter: { selectionType: #SINGLE }
```

#### Analytics Annotations
For analytical scenarios

```cds
@Analytics.dataCategory: #CUBE
@Analytics.dimension: true
@DefaultAggregation: #SUM
```

### CDS View Layers for Fiori

```
┌─────────────────────────────────────┐
│     Consumption View (ZC_)          │  ← UI Annotations
│     Projection on Interface         │
├─────────────────────────────────────┤
│     Interface View (ZI_)            │  ← Reusable, stable
│     Business logic, associations    │
├─────────────────────────────────────┤
│     Basic View / Database           │  ← Raw data access
└─────────────────────────────────────┘
```

### Metadata Extension
Separate UI annotations from CDS view:

```cds
@Metadata.layer: #CUSTOMER
annotate view ZC_SalesOrder with {
  @UI.lineItem: [{ position: 10 }]
  SalesOrderID;

  @UI.lineItem: [{ position: 20 }]
  CustomerName;
}
```

---

## 10. RAP and Fiori

### RAP Overview
- **R**ESTful **A**BAP **P**rogramming Model
- Modern ABAP development model
- Native Fiori Elements support
- Transactional apps with draft

### RAP + Fiori Stack
```
┌─────────────────────────────────────┐
│         Fiori Elements UI           │
├─────────────────────────────────────┤
│         OData V4 Service            │
├─────────────────────────────────────┤
│      Service Binding (ODATA V4)     │
├─────────────────────────────────────┤
│      Service Definition             │
├─────────────────────────────────────┤
│      Behavior Definition            │
├─────────────────────────────────────┤
│      CDS Projection View (ZC_)      │
├─────────────────────────────────────┤
│      CDS Interface View (ZI_)       │
├─────────────────────────────────────┤
│      Database Table                 │
└─────────────────────────────────────┘
```

### Draft Handling
- Auto-save user input
- Exclusive edit locking
- Resume incomplete work
- Discard changes option

```cds
// Behavior Definition
managed implementation in class zbp_travel unique;
with draft;

define behavior for ZI_Travel alias Travel
draft table ztravel_draft
{
  draft action Edit;
  draft action Activate;
  draft action Discard;
  draft action Resume;
  draft determine action Prepare;
}
```

### Actions in Fiori
```cds
// Behavior Definition
action acceptTravel result [1] $self;

// CDS Annotation for Button
@UI.lineItem: [{
  type: #FOR_ACTION,
  dataAction: 'acceptTravel',
  label: 'Accept'
}]
```

---

## 11. Fiori Tools and Development

### Development Tools

#### SAP Business Application Studio
- Cloud-based IDE
- Fiori development workspace
- Built-in generators
- Integrated testing

#### Visual Studio Code + Fiori Tools
- SAP Fiori Tools extension pack
- Application Generator
- Service Modeler
- Guided Development

### Key Fiori Tools Features

| Tool | Purpose |
|------|---------|
| **Application Generator** | Create new Fiori apps |
| **Service Modeler** | Visualize OData services |
| **Guided Development** | Step-by-step feature addition |
| **Application Info** | View project structure |
| **XML Annotation LSP** | Annotation code completion |

### Project Structure
```
webapp/
├── Component.js           # App component
├── manifest.json          # App descriptor
├── i18n/
│   └── i18n.properties   # Translations
├── view/
│   └── *.view.xml        # Views
├── controller/
│   └── *.controller.js   # Controllers
├── model/
│   └── formatter.js      # Formatters
├── localService/         # Mock data
└── test/                 # Tests
```

### manifest.json Key Sections
```json
{
  "sap.app": {
    "id": "com.company.app",
    "dataSources": { }      // OData services
  },
  "sap.ui5": {
    "rootView": { },         // Main view
    "dependencies": { },     // Libraries
    "models": { },           // Data models
    "routing": { }           // Navigation
  }
}
```

### Testing Tools
- **OPA5**: Integration testing
- **QUnit**: Unit testing
- **Mock Server**: Offline development
- **UI5 Inspector**: Chrome extension

---

## 12. Security and Authorization

### Authentication
- **SAP Logon**: Traditional ABAP authentication
- **SAML 2.0**: Single sign-on
- **OAuth 2.0**: Token-based (BTP)
- **X.509 Certificates**: Client certificates

### Authorization Layers

#### Backend (ABAP)
- **PFCG Roles**: Traditional authorization
- **CDS Access Control**: Row-level security
- **RAP Authorization**: Instance/global auth

#### CDS Access Control
```cds
@AccessControl.authorizationCheck: #CHECK
define view entity ZI_SalesOrder as select from vbak {
  ...
}

// DCL file
@EndUserText.label: 'Sales Order Access'
@MappingRole: true
define role ZI_SalesOrder_AC {
  grant select on ZI_SalesOrder
    where (vkorg) = aspect pfcg_auth(V_VBAK_VKO, VKORG, ACTVT = '03');
}
```

#### Fiori Launchpad
- Catalog assignment to roles
- Tile visibility
- App restrictions

### CSRF Token
- Required for modifying operations
- Fetch with `X-CSRF-Token: Fetch`
- Send with subsequent requests

---

## 13. Performance Optimization

### Frontend Optimization

#### Loading Performance
- **Component Preload**: Bundle JS files
- **Manifest First**: Load descriptor early
- **Lazy Loading**: Load views on demand
- **Asynchronous Loading**: `async: true`

#### Runtime Performance
- **Batch Requests**: Group OData calls
- **Table Virtualization**: Load visible rows only
- **Debounce Search**: Delay filter requests
- **Caching**: Browser and OData cache

### Backend Optimization

#### OData Service
- Use `$select` to limit fields
- Use `$top` for pagination
- Implement search-help on demand
- Enable count only when needed

#### CDS/ABAP
- Push calculations to HANA
- Use associations efficiently
- Implement draft garbage collection
- Optimize authorization checks

### Performance Checklist
- [ ] Enable component preload
- [ ] Use async view loading
- [ ] Implement batch requests
- [ ] Optimize OData queries
- [ ] Enable GZIP compression
- [ ] Use CDN for UI5 library
- [ ] Minimize custom CSS/JS

---

## 14. Common Interview Questions

### Basic Questions

**Q: What is SAP Fiori?**
A: SAP Fiori is SAP's user experience design system providing design guidelines, tools, and a set of applications for creating modern, responsive, role-based business applications.

**Q: What are the Fiori design principles?**
A: Role-based, Responsive, Simple, Coherent, and Delightful.

**Q: What is the difference between SAPUI5 and Fiori?**
A: SAPUI5 is a JavaScript framework for building web applications. Fiori is a UX design system and collection of apps built on SAPUI5.

**Q: What are Fiori Elements?**
A: Template-based, annotation-driven approach to building Fiori apps with minimal coding, using CDS annotations for UI configuration.

### Technical Questions

**Q: Explain Fiori architecture.**
A: Three-tier: Presentation (SAPUI5/browser), Gateway (OData services), Backend (ABAP/HANA). Communication via REST/OData.

**Q: What is the Fiori Launchpad?**
A: Central entry point for Fiori apps, providing shell, tiles, navigation, personalization, and role-based access.

**Q: What OData versions does SAP support?**
A: OData V2 (mature, SEGW-based) and OData V4 (modern, RAP-based).

**Q: How do you implement draft handling?**
A: In RAP behavior definition: use `with draft`, define draft table, add draft actions (Edit, Activate, Discard, Resume, Prepare).

**Q: What are CDS annotations used for in Fiori?**
A: Configure UI appearance (lineItem, identification, facets), enable search, define value helps, set up analytics.

### Scenario Questions

**Q: How would you create a List Report app?**
A: 1) Create CDS views with UI annotations, 2) Define RAP behavior, 3) Create service definition/binding, 4) Use Fiori Tools generator, 5) Deploy to Launchpad.

**Q: How do you handle errors in Fiori apps?**
A: Backend: RAP messages (failed/reported), OData error responses. Frontend: MessageBox, MessageToast, MessageManager.

**Q: How do you optimize Fiori app performance?**
A: Frontend: preload, async loading, batch requests. Backend: selective $select, efficient CDS, HANA pushdown.

### Troubleshooting Questions

**Q: App not showing in Launchpad - what to check?**
A: 1) Target mapping configured, 2) Tile in catalog, 3) Catalog assigned to role, 4) User has role, 5) Service activated.

**Q: OData service returning 403 - what could be wrong?**
A: 1) Service not activated, 2) Missing authorization, 3) CSRF token issue, 4) ICF node inactive.

---

## Quick Reference Card

### Useful Transactions
| Transaction | Purpose |
|-------------|---------|
| /UI2/FLPD_CUST | Launchpad Designer |
| /UI2/SEMOBJ | Semantic Objects |
| SEGW | Gateway Service Builder |
| /IWFND/MAINT_SERVICE | Activate OData Service |
| PFCG | Role Maintenance |
| SICF | ICF Services |

### Important URLs
```
Fiori Launchpad:    /sap/bc/ui2/flp
OData Service:      /sap/opu/odata/sap/SERVICE_NAME/
Metadata:           /sap/opu/odata/sap/SERVICE_NAME/$metadata
UI5 Resources:      /sap/public/bc/ui5_ui5/
```

### Essential Annotations
```cds
@UI.headerInfo           // Object page header
@UI.lineItem            // Table columns
@UI.selectionField      // Filter bar fields
@UI.identification      // Object page fields
@UI.facet              // Object page sections
@Search.searchable     // Enable search
@Consumption.valueHelpDefinition  // F4 help
```

---

*Last updated: January 2026*
*Prepared for SAP Fiori Interview*

# SAP Fiori - Practical Code Samples

Hands-on code examples for practicing Fiori development.

---

## Table of Contents

1. [Complete Fiori Elements App (Backend)](#1-complete-fiori-elements-app-backend)
2. [UI Annotations Reference](#2-ui-annotations-reference)
3. [SAPUI5 Freestyle Examples](#3-sapui5-freestyle-examples)
4. [Controller Patterns](#4-controller-patterns)
5. [OData Operations](#5-odata-operations)
6. [Navigation Patterns](#6-navigation-patterns)
7. [Error Handling](#7-error-handling)
8. [Custom Extensions](#8-custom-extensions)

---

## 1. Complete Fiori Elements App (Backend)

### Step 1: Database Table

```abap
@EndUserText.label : 'Product Master'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table zproduct {
  key client            : abap.clnt not null;
  key product_id        : abap.char(10) not null;
  product_name          : abap.char(100);
  category              : abap.char(20);
  @Semantics.amount.currencyCode : 'zproduct.currency_code'
  price                 : abap.curr(15,2);
  currency_code         : abap.cuky;
  stock_quantity        : abap.quan(13,3);
  unit_of_measure       : abap.unit(3);
  status                : abap.char(1);
  @Semantics.user.createdBy : true
  created_by            : abp_creation_user;
  @Semantics.systemDateTime.createdAt : true
  created_at            : abp_creation_tstmpl;
  @Semantics.user.lastChangedBy : true
  last_changed_by       : abp_locinst_lastchange_user;
  @Semantics.systemDateTime.lastChangedAt : true
  last_changed_at       : abp_locinst_lastchange_tstmpl;
}
```

### Step 2: Interface CDS View (ZI_Product)

```abap
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Product Interface View'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZI_Product
  as select from zproduct
  association [0..*] to ZI_ProductReview as _Reviews
    on $projection.ProductId = _Reviews.ProductId
{
  key product_id       as ProductId,
      product_name     as ProductName,
      category         as Category,

      @Semantics.amount.currencyCode: 'CurrencyCode'
      price            as Price,

      @Semantics.currencyCode: true
      currency_code    as CurrencyCode,

      @Semantics.quantity.unitOfMeasure: 'UnitOfMeasure'
      stock_quantity   as StockQuantity,

      @Semantics.unitOfMeasure: true
      unit_of_measure  as UnitOfMeasure,

      status           as Status,

      @Semantics.user.createdBy: true
      created_by       as CreatedBy,

      @Semantics.systemDateTime.createdAt: true
      created_at       as CreatedAt,

      @Semantics.user.lastChangedBy: true
      last_changed_by  as LastChangedBy,

      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at  as LastChangedAt,

      // Virtual Elements
      case status
        when 'A' then 3  // Green
        when 'D' then 1  // Red
        else 2           // Yellow
      end              as StatusCriticality,

      case status
        when 'A' then 'Active'
        when 'D' then 'Discontinued'
        else 'Pending'
      end              as StatusText,

      // Association
      _Reviews
}
```

### Step 3: Consumption CDS View with UI Annotations (ZC_Product)

```abap
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Product - Fiori App'
@Metadata.allowExtensions: true

@UI: {
  headerInfo: {
    typeName: 'Product',
    typeNamePlural: 'Products',
    title: { type: #STANDARD, value: 'ProductName' },
    description: { type: #STANDARD, value: 'ProductId' },
    imageUrl: 'ProductImageUrl'
  },
  presentationVariant: [{
    sortOrder: [{ by: 'ProductId', direction: #ASC }],
    visualizations: [{ type: #AS_LINEITEM }]
  }]
}

@Search.searchable: true

define root view entity ZC_Product
  provider contract transactional_query
  as projection on ZI_Product
{
      // Header Facets and Object Page Sections
      @UI.facet: [
        // Header Facet - Quick View
        {
          id: 'HeaderPrice',
          type: #DATAPOINT_REFERENCE,
          targetQualifier: 'Price',
          purpose: #HEADER
        },
        {
          id: 'HeaderStatus',
          type: #DATAPOINT_REFERENCE,
          targetQualifier: 'Status',
          purpose: #HEADER
        },
        // General Information Section
        {
          id: 'GeneralInfo',
          type: #IDENTIFICATION_REFERENCE,
          label: 'General Information',
          position: 10
        },
        // Pricing Section
        {
          id: 'Pricing',
          type: #FIELDGROUP_REFERENCE,
          targetQualifier: 'PricingData',
          label: 'Pricing & Stock',
          position: 20
        },
        // Reviews Section (Sub-object)
        {
          id: 'Reviews',
          type: #LINEITEM_REFERENCE,
          targetElement: '_Reviews',
          label: 'Customer Reviews',
          position: 30
        },
        // Admin Data Section
        {
          id: 'AdminData',
          type: #FIELDGROUP_REFERENCE,
          targetQualifier: 'AdminData',
          label: 'Administrative Data',
          position: 40
        }
      ]

      // Key Field
      @UI: {
        lineItem: [{ position: 10, importance: #HIGH }],
        identification: [{ position: 10 }],
        selectionField: [{ position: 10 }]
      }
      @Search.defaultSearchElement: true
  key ProductId,

      // Product Name
      @UI: {
        lineItem: [{ position: 20, importance: #HIGH }],
        identification: [{ position: 20 }],
        selectionField: [{ position: 20 }]
      }
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      ProductName,

      // Category with Value Help
      @UI: {
        lineItem: [{ position: 30, importance: #MEDIUM }],
        identification: [{ position: 30 }],
        selectionField: [{ position: 30 }]
      }
      @Consumption.valueHelpDefinition: [{
        entity: { name: 'ZI_ProductCategory', element: 'CategoryId' },
        additionalBinding: [{ localElement: 'Category', element: 'CategoryId' }]
      }]
      Category,

      // Price with Data Point for Header
      @UI: {
        lineItem: [{ position: 40, importance: #HIGH }],
        identification: [{ position: 40 }],
        dataPoint: { qualifier: 'Price', title: 'Price' },
        fieldGroup: [{ qualifier: 'PricingData', position: 10 }]
      }
      Price,

      @UI.hidden: true
      CurrencyCode,

      // Stock Quantity
      @UI: {
        lineItem: [{ position: 50, importance: #MEDIUM }],
        fieldGroup: [{ qualifier: 'PricingData', position: 20 }]
      }
      StockQuantity,

      @UI.hidden: true
      UnitOfMeasure,

      // Status with Criticality
      @UI: {
        lineItem: [{
          position: 60,
          importance: #HIGH,
          criticality: 'StatusCriticality',
          criticalityRepresentation: #WITH_ICON
        }],
        identification: [{
          position: 50,
          criticality: 'StatusCriticality'
        }],
        selectionField: [{ position: 40 }],
        dataPoint: {
          qualifier: 'Status',
          title: 'Status',
          criticality: 'StatusCriticality'
        },
        textArrangement: #TEXT_ONLY
      }
      @Consumption.valueHelpDefinition: [{
        entity: { name: 'ZI_ProductStatusVH', element: 'Status' }
      }]
      Status,

      @UI.hidden: true
      StatusCriticality,

      StatusText,

      // Administrative Fields
      @UI.fieldGroup: [{ qualifier: 'AdminData', position: 10, label: 'Created By' }]
      CreatedBy,

      @UI.fieldGroup: [{ qualifier: 'AdminData', position: 20, label: 'Created On' }]
      CreatedAt,

      @UI.fieldGroup: [{ qualifier: 'AdminData', position: 30, label: 'Changed By' }]
      LastChangedBy,

      @UI.fieldGroup: [{ qualifier: 'AdminData', position: 40, label: 'Changed On' }]
      LastChangedAt,

      // Association
      _Reviews
}
```

### Step 4: Behavior Definition

```abap
managed implementation in class zbp_i_product unique;
strict ( 2 );
with draft;

define behavior for ZI_Product alias Product
persistent table zproduct
draft table zproduct_d
etag master LastChangedAt
lock master total etag LastChangedAt
authorization master ( instance )
{
  // Standard Operations
  create;
  update;
  delete;

  // Field Properties
  field ( readonly ) ProductId, CreatedBy, CreatedAt, LastChangedBy, LastChangedAt;
  field ( readonly : update ) ProductId;
  field ( mandatory ) ProductName, Category, Price, CurrencyCode;

  // Validations
  validation validatePrice on save { field Price; create; update; }
  validation validateCategory on save { field Category; create; update; }

  // Determinations
  determination setProductId on save { create; }
  determination setDefaultStatus on modify { create; }

  // Actions
  action ( features : instance ) activateProduct result [1] $self;
  action ( features : instance ) discontinueProduct result [1] $self;

  // Draft Actions
  draft action Edit;
  draft action Activate optimized;
  draft action Discard;
  draft action Resume;
  draft determine action Prepare
  {
    validation validatePrice;
    validation validateCategory;
  }

  // Field Mapping
  mapping for zproduct
  {
    ProductId = product_id;
    ProductName = product_name;
    Category = category;
    Price = price;
    CurrencyCode = currency_code;
    StockQuantity = stock_quantity;
    UnitOfMeasure = unit_of_measure;
    Status = status;
    CreatedBy = created_by;
    CreatedAt = created_at;
    LastChangedBy = last_changed_by;
    LastChangedAt = last_changed_at;
  }

  // Association to Reviews
  association _Reviews { create; with draft; }
}

// Behavior for child entity
define behavior for ZI_ProductReview alias Review
persistent table zproduct_review
draft table zproduct_rev_d
etag master LastChangedAt
lock dependent by _Product
authorization dependent by _Product
{
  update;
  delete;

  field ( readonly ) ProductId, ReviewId;
  field ( mandatory ) Rating, ReviewText;

  determination setReviewId on save { create; }

  mapping for zproduct_review
  {
    ProductId = product_id;
    ReviewId = review_id;
    Rating = rating;
    ReviewText = review_text;
    ReviewDate = review_date;
    LastChangedAt = last_changed_at;
  }

  association _Product { with draft; }
}
```

### Step 5: Behavior Implementation

```abap
CLASS lhc_product DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Product RESULT result.

    METHODS validatePrice FOR VALIDATE ON SAVE
      IMPORTING keys FOR Product~validatePrice.

    METHODS validateCategory FOR VALIDATE ON SAVE
      IMPORTING keys FOR Product~validateCategory.

    METHODS setProductId FOR DETERMINE ON SAVE
      IMPORTING keys FOR Product~setProductId.

    METHODS setDefaultStatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Product~setDefaultStatus.

    METHODS activateProduct FOR MODIFY
      IMPORTING keys FOR ACTION Product~activateProduct RESULT result.

    METHODS discontinueProduct FOR MODIFY
      IMPORTING keys FOR ACTION Product~discontinueProduct RESULT result.
ENDCLASS.

CLASS lhc_product IMPLEMENTATION.

  METHOD get_instance_features.
    " Read product status
    READ ENTITIES OF zi_product IN LOCAL MODE
      ENTITY Product
        FIELDS ( Status )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_products)
      FAILED failed.

    result = VALUE #( FOR product IN lt_products
      ( %tky = product-%tky
        " Enable/disable actions based on status
        %action-activateProduct = COND #(
          WHEN product-Status = 'A'
          THEN if_abap_behv=>fc-o-disabled
          ELSE if_abap_behv=>fc-o-enabled )
        %action-discontinueProduct = COND #(
          WHEN product-Status = 'D'
          THEN if_abap_behv=>fc-o-disabled
          ELSE if_abap_behv=>fc-o-enabled ) ) ).
  ENDMETHOD.

  METHOD validatePrice.
    READ ENTITIES OF zi_product IN LOCAL MODE
      ENTITY Product
        FIELDS ( Price )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_products).

    LOOP AT lt_products INTO DATA(ls_product).
      IF ls_product-Price <= 0.
        APPEND VALUE #( %tky = ls_product-%tky ) TO failed-product.
        APPEND VALUE #(
          %tky = ls_product-%tky
          %state_area = 'VALIDATE_PRICE'
          %msg = new_message_with_text(
            severity = if_abap_behv_message=>severity-error
            text = 'Price must be greater than zero' )
          %element-Price = if_abap_behv=>mk-on
        ) TO reported-product.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateCategory.
    READ ENTITIES OF zi_product IN LOCAL MODE
      ENTITY Product
        FIELDS ( Category )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_products).

    " Get valid categories
    SELECT category_id FROM zproduct_cat INTO TABLE @DATA(lt_categories).

    LOOP AT lt_products INTO DATA(ls_product).
      IF NOT line_exists( lt_categories[ category_id = ls_product-Category ] ).
        APPEND VALUE #( %tky = ls_product-%tky ) TO failed-product.
        APPEND VALUE #(
          %tky = ls_product-%tky
          %state_area = 'VALIDATE_CATEGORY'
          %msg = new_message_with_text(
            severity = if_abap_behv_message=>severity-error
            text = |Category { ls_product-Category } is not valid| )
          %element-Category = if_abap_behv=>mk-on
        ) TO reported-product.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD setProductId.
    READ ENTITIES OF zi_product IN LOCAL MODE
      ENTITY Product
        FIELDS ( ProductId )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_products).

    " Get next number
    SELECT MAX( product_id ) FROM zproduct INTO @DATA(lv_max_id).
    DATA(lv_next_id) = lv_max_id + 1.

    LOOP AT lt_products INTO DATA(ls_product) WHERE ProductId IS INITIAL.
      MODIFY ENTITIES OF zi_product IN LOCAL MODE
        ENTITY Product
          UPDATE FIELDS ( ProductId )
          WITH VALUE #( (
            %tky = ls_product-%tky
            ProductId = |PROD{ lv_next_id WIDTH = 6 ALIGN = RIGHT PAD = '0' }|
          ) ).
      lv_next_id = lv_next_id + 1.
    ENDLOOP.
  ENDMETHOD.

  METHOD setDefaultStatus.
    READ ENTITIES OF zi_product IN LOCAL MODE
      ENTITY Product
        FIELDS ( Status )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_products).

    MODIFY ENTITIES OF zi_product IN LOCAL MODE
      ENTITY Product
        UPDATE FIELDS ( Status )
        WITH VALUE #( FOR product IN lt_products
          WHERE ( Status IS INITIAL )
          ( %tky = product-%tky
            Status = 'P' ) ). " Pending
  ENDMETHOD.

  METHOD activateProduct.
    MODIFY ENTITIES OF zi_product IN LOCAL MODE
      ENTITY Product
        UPDATE FIELDS ( Status )
        WITH VALUE #( FOR key IN keys
          ( %tky = key-%tky
            Status = 'A' ) ).

    READ ENTITIES OF zi_product IN LOCAL MODE
      ENTITY Product
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_products).

    result = VALUE #( FOR product IN lt_products
      ( %tky = product-%tky
        %param = product ) ).
  ENDMETHOD.

  METHOD discontinueProduct.
    MODIFY ENTITIES OF zi_product IN LOCAL MODE
      ENTITY Product
        UPDATE FIELDS ( Status )
        WITH VALUE #( FOR key IN keys
          ( %tky = key-%tky
            Status = 'D' ) ).

    READ ENTITIES OF zi_product IN LOCAL MODE
      ENTITY Product
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_products).

    result = VALUE #( FOR product IN lt_products
      ( %tky = product-%tky
        %param = product ) ).
  ENDMETHOD.

ENDCLASS.
```

### Step 6: Service Definition

```abap
@EndUserText.label: 'Product Management Service'
define service ZUI_PRODUCT_O4 {
  expose ZC_Product as Product;
  expose ZC_ProductReview as ProductReview;
  expose ZI_ProductCategory as ProductCategory;
  expose ZI_ProductStatusVH as ProductStatus;
}
```

---

## 2. UI Annotations Reference

### Complete Annotation Examples

```abap
// Header Info - Object Page Header
@UI.headerInfo: {
  typeName: 'Sales Order',
  typeNamePlural: 'Sales Orders',
  title: { type: #STANDARD, value: 'SalesOrderID' },
  description: { type: #STANDARD, value: 'CustomerName' },
  imageUrl: 'ProductImageUrl',
  typeImageUrl: 'sap-icon://sales-order'
}

// Line Item - Table Columns
@UI.lineItem: [
  { position: 10, importance: #HIGH, label: 'Order ID' },
  { position: 20, type: #FOR_ACTION, dataAction: 'approve', label: 'Approve' },
  { position: 30, criticality: 'StatusCriticality', criticalityRepresentation: #WITH_ICON }
]

// Selection Field - Filter Bar
@UI.selectionField: [{ position: 10, exclude: false }]

// Identification - Object Page Fields
@UI.identification: [
  { position: 10, importance: #HIGH },
  { type: #FOR_ACTION, dataAction: 'cancel', label: 'Cancel Order' }
]

// Field Group - Grouping Fields
@UI.fieldGroup: [
  { qualifier: 'OrderDetails', position: 10, label: 'Order ID' },
  { qualifier: 'OrderDetails', position: 20, label: 'Customer' }
]

// Facets - Object Page Sections
@UI.facet: [
  // Header Facet (KPI)
  {
    id: 'HeaderAmount',
    purpose: #HEADER,
    type: #DATAPOINT_REFERENCE,
    targetQualifier: 'TotalAmount'
  },
  // Collection Facet (Group of sections)
  {
    id: 'OrderInfo',
    type: #COLLECTION,
    label: 'Order Information',
    position: 10
  },
  // Identification Reference (inside collection)
  {
    id: 'GeneralInfo',
    parentId: 'OrderInfo',
    type: #IDENTIFICATION_REFERENCE,
    label: 'General',
    position: 10
  },
  // Field Group Reference
  {
    id: 'Pricing',
    parentId: 'OrderInfo',
    type: #FIELDGROUP_REFERENCE,
    targetQualifier: 'PricingGroup',
    label: 'Pricing',
    position: 20
  },
  // Line Item Reference (Sub-object table)
  {
    id: 'Items',
    type: #LINEITEM_REFERENCE,
    targetElement: '_Items',
    label: 'Order Items',
    position: 20
  }
]

// Data Point - KPI Display
@UI.dataPoint: {
  qualifier: 'TotalAmount',
  title: 'Total Amount',
  description: 'Order Total',
  valueFormat: { numberOfFractionalDigits: 2 },
  criticality: 'AmountCriticality',
  trend: 'AmountTrend'
}

// Chart Annotation
@UI.chart: [{
  qualifier: 'SalesByMonth',
  chartType: #COLUMN,
  dimensions: ['Month'],
  measures: ['TotalSales'],
  title: 'Sales by Month'
}]

// Presentation Variant - Default Sort/Filter
@UI.presentationVariant: [{
  qualifier: 'Default',
  sortOrder: [{ by: 'CreatedAt', direction: #DESC }],
  visualizations: [{ type: #AS_LINEITEM }]
}]

// Selection Variant - Predefined Filters
@UI.selectionVariant: [{
  qualifier: 'OpenOrders',
  text: 'Open Orders',
  filter: 'Status eq ''O'''
}]

// Hidden - Hide from UI
@UI.hidden: true

// Text Arrangement
@UI.textArrangement: #TEXT_ONLY    // Show text only
@UI.textArrangement: #TEXT_FIRST   // Text (ID)
@UI.textArrangement: #TEXT_LAST    // ID (Text)
@UI.textArrangement: #TEXT_SEPARATE // Separate columns
```

### Action Button Annotations

```abap
// In Behavior Definition
action approveOrder result [1] $self;
action ( features : instance ) rejectOrder result [1] $self;

// In CDS View
@UI.lineItem: [{
  position: 100,
  type: #FOR_ACTION,
  dataAction: 'approveOrder',
  label: 'Approve',
  importance: #HIGH
}]

@UI.identification: [{
  position: 100,
  type: #FOR_ACTION,
  dataAction: 'rejectOrder',
  label: 'Reject'
}]
```

---

## 3. SAPUI5 Freestyle Examples

### 3.1 manifest.json (App Descriptor)

```json
{
  "_version": "1.42.0",
  "sap.app": {
    "id": "com.company.products",
    "type": "application",
    "i18n": "i18n/i18n.properties",
    "title": "{{appTitle}}",
    "description": "{{appDescription}}",
    "applicationVersion": {
      "version": "1.0.0"
    },
    "dataSources": {
      "mainService": {
        "uri": "/sap/opu/odata4/sap/zui_product_o4/srvd/sap/zui_product_o4/0001/",
        "type": "OData",
        "settings": {
          "odataVersion": "4.0",
          "localUri": "localService/metadata.xml"
        }
      }
    },
    "crossNavigation": {
      "inbounds": {
        "ProductManage": {
          "semanticObject": "Product",
          "action": "manage",
          "signature": {
            "parameters": {},
            "additionalParameters": "allowed"
          }
        }
      }
    }
  },
  "sap.ui": {
    "technology": "UI5",
    "icons": {
      "icon": "sap-icon://product"
    },
    "deviceTypes": {
      "desktop": true,
      "tablet": true,
      "phone": true
    }
  },
  "sap.ui5": {
    "rootView": {
      "viewName": "com.company.products.view.App",
      "type": "XML",
      "async": true,
      "id": "app"
    },
    "dependencies": {
      "minUI5Version": "1.108.0",
      "libs": {
        "sap.ui.core": {},
        "sap.m": {},
        "sap.ui.layout": {},
        "sap.uxap": {},
        "sap.ui.table": {}
      }
    },
    "contentDensities": {
      "compact": true,
      "cozy": true
    },
    "models": {
      "i18n": {
        "type": "sap.ui.model.resource.ResourceModel",
        "settings": {
          "bundleName": "com.company.products.i18n.i18n"
        }
      },
      "": {
        "dataSource": "mainService",
        "preload": true,
        "settings": {
          "operationMode": "Server",
          "autoExpandSelect": true,
          "earlyRequests": true
        }
      }
    },
    "routing": {
      "config": {
        "routerClass": "sap.m.routing.Router",
        "viewType": "XML",
        "viewPath": "com.company.products.view",
        "controlId": "app",
        "controlAggregation": "pages",
        "async": true,
        "bypassed": {
          "target": "notFound"
        }
      },
      "routes": [
        {
          "pattern": "",
          "name": "productList",
          "target": "productList"
        },
        {
          "pattern": "Product/{productId}",
          "name": "productDetail",
          "target": "productDetail"
        },
        {
          "pattern": "Product/{productId}/edit",
          "name": "productEdit",
          "target": "productEdit"
        }
      ],
      "targets": {
        "productList": {
          "viewName": "ProductList",
          "viewLevel": 1
        },
        "productDetail": {
          "viewName": "ProductDetail",
          "viewLevel": 2
        },
        "productEdit": {
          "viewName": "ProductEdit",
          "viewLevel": 3
        },
        "notFound": {
          "viewName": "NotFound",
          "viewLevel": 1
        }
      }
    }
  }
}
```

### 3.2 Component.js

```javascript
sap.ui.define([
    "sap/ui/core/UIComponent",
    "sap/ui/Device",
    "com/company/products/model/models"
], function (UIComponent, Device, models) {
    "use strict";

    return UIComponent.extend("com.company.products.Component", {
        metadata: {
            manifest: "json"
        },

        init: function () {
            // Call parent init
            UIComponent.prototype.init.apply(this, arguments);

            // Set device model
            this.setModel(models.createDeviceModel(), "device");

            // Initialize router
            this.getRouter().initialize();

            // Set content density
            this._applyContentDensity();
        },

        _applyContentDensity: function () {
            this.getContentDensityClass = function () {
                if (!this._sContentDensityClass) {
                    if (Device.support.touch) {
                        this._sContentDensityClass = "sapUiSizeCozy";
                    } else {
                        this._sContentDensityClass = "sapUiSizeCompact";
                    }
                }
                return this._sContentDensityClass;
            };
        },

        destroy: function () {
            UIComponent.prototype.destroy.apply(this, arguments);
        }
    });
});
```

### 3.3 List View (ProductList.view.xml)

```xml
<mvc:View
    controllerName="com.company.products.controller.ProductList"
    xmlns:mvc="sap.ui.core.mvc"
    xmlns="sap.m"
    xmlns:core="sap.ui.core"
    xmlns:f="sap.f"
    xmlns:fb="sap.ui.comp.filterbar"
    displayBlock="true">

    <f:DynamicPage id="dynamicPage" headerExpanded="true">
        <!-- Title -->
        <f:title>
            <f:DynamicPageTitle>
                <f:heading>
                    <Title text="{i18n>productListTitle}" />
                </f:heading>
                <f:actions>
                    <Button
                        id="createBtn"
                        text="{i18n>create}"
                        type="Emphasized"
                        icon="sap-icon://add"
                        press=".onCreateProduct" />
                    <Button
                        id="refreshBtn"
                        icon="sap-icon://refresh"
                        press=".onRefresh"
                        tooltip="{i18n>refresh}" />
                </f:actions>
            </f:DynamicPageTitle>
        </f:title>

        <!-- Header (Filter Bar) -->
        <f:header>
            <f:DynamicPageHeader pinnable="true">
                <f:content>
                    <fb:FilterBar
                        id="filterBar"
                        useToolbar="false"
                        filterBarExpanded="true"
                        showClearOnFB="true"
                        showRestoreOnFB="true"
                        search=".onSearch"
                        clear=".onClear">
                        <fb:filterGroupItems>
                            <fb:FilterGroupItem
                                name="ProductId"
                                label="{i18n>productId}"
                                groupName="Basic"
                                visibleInFilterBar="true">
                                <fb:control>
                                    <Input id="productIdFilter" />
                                </fb:control>
                            </fb:FilterGroupItem>
                            <fb:FilterGroupItem
                                name="ProductName"
                                label="{i18n>productName}"
                                groupName="Basic"
                                visibleInFilterBar="true">
                                <fb:control>
                                    <Input id="productNameFilter" />
                                </fb:control>
                            </fb:FilterGroupItem>
                            <fb:FilterGroupItem
                                name="Category"
                                label="{i18n>category}"
                                groupName="Basic"
                                visibleInFilterBar="true">
                                <fb:control>
                                    <ComboBox
                                        id="categoryFilter"
                                        items="{/ProductCategory}">
                                        <core:Item
                                            key="{CategoryId}"
                                            text="{CategoryName}" />
                                    </ComboBox>
                                </fb:control>
                            </fb:FilterGroupItem>
                            <fb:FilterGroupItem
                                name="Status"
                                label="{i18n>status}"
                                groupName="Basic"
                                visibleInFilterBar="true">
                                <fb:control>
                                    <MultiComboBox
                                        id="statusFilter"
                                        items="{/ProductStatus}">
                                        <core:Item
                                            key="{Status}"
                                            text="{StatusText}" />
                                    </MultiComboBox>
                                </fb:control>
                            </fb:FilterGroupItem>
                        </fb:filterGroupItems>
                    </fb:FilterBar>
                </f:content>
            </f:DynamicPageHeader>
        </f:header>

        <!-- Content (Table) -->
        <f:content>
            <Table
                id="productTable"
                growing="true"
                growingThreshold="20"
                growingScrollToLoad="true"
                mode="SingleSelectMaster"
                selectionChange=".onSelectionChange"
                items="{
                    path: '/Product',
                    parameters: {
                        $count: true,
                        $orderby: 'ProductId'
                    },
                    sorter: { path: 'ProductId' }
                }">

                <headerToolbar>
                    <OverflowToolbar>
                        <Title text="{i18n>products} ({= ${/Product/$count}})" />
                        <ToolbarSpacer />
                        <SearchField
                            id="searchField"
                            width="20rem"
                            search=".onQuickSearch"
                            placeholder="{i18n>searchPlaceholder}" />
                        <Button
                            id="sortBtn"
                            icon="sap-icon://sort"
                            press=".onOpenSortDialog"
                            tooltip="{i18n>sort}" />
                    </OverflowToolbar>
                </headerToolbar>

                <columns>
                    <Column width="10rem">
                        <Text text="{i18n>productId}" />
                    </Column>
                    <Column>
                        <Text text="{i18n>productName}" />
                    </Column>
                    <Column width="10rem">
                        <Text text="{i18n>category}" />
                    </Column>
                    <Column width="10rem" hAlign="End">
                        <Text text="{i18n>price}" />
                    </Column>
                    <Column width="8rem" hAlign="End">
                        <Text text="{i18n>stock}" />
                    </Column>
                    <Column width="8rem">
                        <Text text="{i18n>status}" />
                    </Column>
                </columns>

                <items>
                    <ColumnListItem type="Navigation" press=".onItemPress">
                        <cells>
                            <ObjectIdentifier
                                title="{ProductId}"
                                text="{CreatedAt}" />
                            <Text text="{ProductName}" />
                            <Text text="{Category}" />
                            <ObjectNumber
                                number="{
                                    path: 'Price',
                                    type: 'sap.ui.model.odata.type.Decimal',
                                    formatOptions: { minFractionDigits: 2, maxFractionDigits: 2 }
                                }"
                                unit="{CurrencyCode}" />
                            <ObjectNumber
                                number="{StockQuantity}"
                                unit="{UnitOfMeasure}" />
                            <ObjectStatus
                                text="{StatusText}"
                                state="{= ${StatusCriticality} === 3 ? 'Success' : (${StatusCriticality} === 1 ? 'Error' : 'Warning')}"
                                icon="{= ${StatusCriticality} === 3 ? 'sap-icon://status-positive' : (${StatusCriticality} === 1 ? 'sap-icon://status-negative' : 'sap-icon://status-critical')}" />
                        </cells>
                    </ColumnListItem>
                </items>
            </Table>
        </f:content>

        <!-- Footer -->
        <f:footer>
            <OverflowToolbar>
                <ToolbarSpacer />
                <Button
                    text="{i18n>delete}"
                    type="Reject"
                    enabled="{view>/deleteEnabled}"
                    press=".onDeleteProduct" />
            </OverflowToolbar>
        </f:footer>
    </f:DynamicPage>
</mvc:View>
```

---

## 4. Controller Patterns

### 4.1 List Controller

```javascript
sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel",
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator",
    "sap/ui/model/Sorter",
    "sap/m/MessageBox",
    "sap/m/MessageToast",
    "sap/ui/core/Fragment"
], function (Controller, JSONModel, Filter, FilterOperator, Sorter,
             MessageBox, MessageToast, Fragment) {
    "use strict";

    return Controller.extend("com.company.products.controller.ProductList", {

        onInit: function () {
            // View model for UI state
            const oViewModel = new JSONModel({
                busy: false,
                deleteEnabled: false,
                selectedProduct: null
            });
            this.getView().setModel(oViewModel, "view");

            // Store reference to table
            this._oTable = this.byId("productTable");

            // Attach route matched
            this.getOwnerComponent().getRouter()
                .getRoute("productList")
                .attachPatternMatched(this._onRouteMatched, this);
        },

        _onRouteMatched: function () {
            // Refresh data when navigating back
            this._oTable.getBinding("items").refresh();
        },

        // Create new product
        onCreateProduct: function () {
            this.getOwnerComponent().getRouter().navTo("productEdit", {
                productId: "new"
            });
        },

        // Navigate to detail
        onItemPress: function (oEvent) {
            const oItem = oEvent.getSource();
            const sProductId = oItem.getBindingContext().getProperty("ProductId");

            this.getOwnerComponent().getRouter().navTo("productDetail", {
                productId: sProductId
            });
        },

        // Selection change
        onSelectionChange: function (oEvent) {
            const oSelectedItem = oEvent.getParameter("listItem");
            const bSelected = oSelectedItem !== null;

            this.getView().getModel("view").setProperty("/deleteEnabled", bSelected);

            if (bSelected) {
                const sProductId = oSelectedItem.getBindingContext().getProperty("ProductId");
                this.getView().getModel("view").setProperty("/selectedProduct", sProductId);
            }
        },

        // Filter bar search
        onSearch: function () {
            const aFilters = this._createFilters();
            this._oTable.getBinding("items").filter(aFilters);
        },

        _createFilters: function () {
            const aFilters = [];

            // Product ID filter
            const sProductId = this.byId("productIdFilter").getValue();
            if (sProductId) {
                aFilters.push(new Filter("ProductId", FilterOperator.Contains, sProductId));
            }

            // Product Name filter
            const sProductName = this.byId("productNameFilter").getValue();
            if (sProductName) {
                aFilters.push(new Filter("ProductName", FilterOperator.Contains, sProductName));
            }

            // Category filter
            const sCategory = this.byId("categoryFilter").getSelectedKey();
            if (sCategory) {
                aFilters.push(new Filter("Category", FilterOperator.EQ, sCategory));
            }

            // Status filter (multi-select)
            const aStatus = this.byId("statusFilter").getSelectedKeys();
            if (aStatus.length > 0) {
                const aStatusFilters = aStatus.map(function (sStatus) {
                    return new Filter("Status", FilterOperator.EQ, sStatus);
                });
                aFilters.push(new Filter({
                    filters: aStatusFilters,
                    and: false
                }));
            }

            return aFilters;
        },

        // Quick search
        onQuickSearch: function (oEvent) {
            const sQuery = oEvent.getParameter("query");
            const aFilters = [];

            if (sQuery) {
                aFilters.push(new Filter({
                    filters: [
                        new Filter("ProductId", FilterOperator.Contains, sQuery),
                        new Filter("ProductName", FilterOperator.Contains, sQuery)
                    ],
                    and: false
                }));
            }

            this._oTable.getBinding("items").filter(aFilters);
        },

        // Clear filters
        onClear: function () {
            this.byId("productIdFilter").setValue("");
            this.byId("productNameFilter").setValue("");
            this.byId("categoryFilter").setSelectedKey("");
            this.byId("statusFilter").setSelectedKeys([]);
            this._oTable.getBinding("items").filter([]);
        },

        // Refresh
        onRefresh: function () {
            this._oTable.getBinding("items").refresh();
            MessageToast.show(this._getText("dataRefreshed"));
        },

        // Sort dialog
        onOpenSortDialog: function () {
            const oView = this.getView();

            if (!this._oSortDialog) {
                Fragment.load({
                    id: oView.getId(),
                    name: "com.company.products.view.fragments.SortDialog",
                    controller: this
                }).then(function (oDialog) {
                    this._oSortDialog = oDialog;
                    oView.addDependent(oDialog);
                    oDialog.open();
                }.bind(this));
            } else {
                this._oSortDialog.open();
            }
        },

        onSortConfirm: function (oEvent) {
            const mParams = oEvent.getParameters();
            const sSortPath = mParams.sortItem.getKey();
            const bDescending = mParams.sortDescending;

            this._oTable.getBinding("items").sort(new Sorter(sSortPath, bDescending));
        },

        // Delete product
        onDeleteProduct: function () {
            const sProductId = this.getView().getModel("view").getProperty("/selectedProduct");

            MessageBox.confirm(this._getText("confirmDelete", [sProductId]), {
                title: this._getText("delete"),
                onClose: function (sAction) {
                    if (sAction === MessageBox.Action.OK) {
                        this._deleteProduct(sProductId);
                    }
                }.bind(this)
            });
        },

        _deleteProduct: function (sProductId) {
            const oModel = this.getView().getModel();
            const oContext = oModel.bindContext("/Product('" + sProductId + "')");

            oContext.delete().then(function () {
                MessageToast.show(this._getText("deleteSuccess"));
                this._oTable.getBinding("items").refresh();
            }.bind(this)).catch(function (oError) {
                MessageBox.error(this._getText("deleteFailed"));
            }.bind(this));
        },

        // Helper: Get i18n text
        _getText: function (sKey, aArgs) {
            return this.getView().getModel("i18n").getResourceBundle().getText(sKey, aArgs);
        }
    });
});
```

---

## 5. OData Operations

### 5.1 CRUD Operations (OData V4)

```javascript
// READ - Single entity
onReadProduct: function (sProductId) {
    const oModel = this.getView().getModel();
    const oContext = oModel.bindContext("/Product('" + sProductId + "')");

    oContext.requestObject().then(function (oData) {
        console.log("Product:", oData);
    }).catch(function (oError) {
        MessageBox.error("Read failed: " + oError.message);
    });
},

// READ - Collection with expand
onReadProducts: function () {
    const oModel = this.getView().getModel();
    const oListBinding = oModel.bindList("/Product", null, null, null, {
        $expand: "_Reviews",
        $orderby: "ProductName",
        $top: 100
    });

    oListBinding.requestContexts().then(function (aContexts) {
        aContexts.forEach(function (oContext) {
            console.log(oContext.getObject());
        });
    });
},

// CREATE
onCreateProduct: function (oProductData) {
    const oModel = this.getView().getModel();
    const oListBinding = oModel.bindList("/Product");

    const oContext = oListBinding.create(oProductData);

    oContext.created().then(function () {
        MessageToast.show("Product created: " + oContext.getProperty("ProductId"));
    }).catch(function (oError) {
        MessageBox.error("Create failed: " + oError.message);
    });
},

// UPDATE
onUpdateProduct: function (sProductId, oChanges) {
    const oModel = this.getView().getModel();
    const sPath = "/Product('" + sProductId + "')";

    // Option 1: Using property binding
    Object.keys(oChanges).forEach(function (sProperty) {
        oModel.setProperty(sPath + "/" + sProperty, oChanges[sProperty]);
    });

    oModel.submitBatch("updateGroup").then(function () {
        MessageToast.show("Product updated");
    });

    // Option 2: Using context
    const oContext = oModel.bindContext(sPath);
    oContext.requestObject().then(function () {
        Object.keys(oChanges).forEach(function (sProperty) {
            oContext.setProperty(sProperty, oChanges[sProperty]);
        });
    });
},

// DELETE
onDeleteProduct: function (sProductId) {
    const oModel = this.getView().getModel();
    const oContext = oModel.bindContext("/Product('" + sProductId + "')");

    oContext.delete().then(function () {
        MessageToast.show("Product deleted");
    }).catch(function (oError) {
        MessageBox.error("Delete failed: " + oError.message);
    });
},

// BATCH Operations
onBatchUpdate: function (aProducts) {
    const oModel = this.getView().getModel();

    aProducts.forEach(function (oProduct) {
        const sPath = "/Product('" + oProduct.ProductId + "')";
        oModel.setProperty(sPath + "/Price", oProduct.Price);
    });

    // Submit all changes in batch
    oModel.submitBatch("batchGroup").then(function () {
        MessageToast.show("Batch update successful");
    }).catch(function (oError) {
        MessageBox.error("Batch update failed");
    });
},

// Call Action (Bound)
onCallAction: function (sProductId, sAction) {
    const oModel = this.getView().getModel();
    const oContext = oModel.bindContext(
        "/Product('" + sProductId + "')/" + sAction + "(...)"
    );

    oContext.execute().then(function () {
        MessageToast.show("Action executed successfully");
        oModel.refresh();
    }).catch(function (oError) {
        MessageBox.error("Action failed: " + oError.message);
    });
},

// Call Function (Unbound)
onCallFunction: function () {
    const oModel = this.getView().getModel();
    const oContext = oModel.bindContext("/GetStatistics(...)");

    oContext.setParameter("year", "2025");
    oContext.execute().then(function () {
        const oResult = oContext.getBoundContext().getObject();
        console.log("Statistics:", oResult);
    });
}
```

---

## 6. Navigation Patterns

### 6.1 Intent-Based Navigation

```javascript
// Navigate to another app
onNavigateToCustomer: function (sCustomerId) {
    const oCrossAppNav = sap.ushell.Container.getService("CrossApplicationNavigation");

    oCrossAppNav.toExternal({
        target: {
            semanticObject: "Customer",
            action: "display"
        },
        params: {
            CustomerId: sCustomerId
        }
    });
},

// Check if navigation target exists
onCheckNavigation: function () {
    const oCrossAppNav = sap.ushell.Container.getService("CrossApplicationNavigation");

    oCrossAppNav.isNavigationSupported([{
        target: {
            semanticObject: "Customer",
            action: "display"
        }
    }]).then(function (aSupported) {
        if (aSupported[0].supported) {
            // Navigation is supported
        }
    });
},

// Get startup parameters
onGetStartupParams: function () {
    const oComponent = this.getOwnerComponent();
    const oStartupParams = oComponent.getComponentData().startupParameters;

    if (oStartupParams.ProductId) {
        const sProductId = oStartupParams.ProductId[0];
        // Use the parameter
    }
}
```

### 6.2 Internal Navigation

```javascript
// Navigate with router
onNavToDetail: function (sProductId) {
    this.getOwnerComponent().getRouter().navTo("productDetail", {
        productId: sProductId
    });
},

// Navigate with query parameters
onNavToListFiltered: function () {
    this.getOwnerComponent().getRouter().navTo("productList", {}, {
        Category: "Electronics",
        Status: "A"
    });
},

// Navigate back
onNavBack: function () {
    const oHistory = sap.ui.core.routing.History.getInstance();
    const sPreviousHash = oHistory.getPreviousHash();

    if (sPreviousHash !== undefined) {
        window.history.go(-1);
    } else {
        this.getOwnerComponent().getRouter().navTo("productList", {}, true);
    }
}
```

---

## 7. Error Handling

### 7.1 Global Error Handler

```javascript
// In Component.js init()
init: function () {
    // ... other init code ...

    // Global error handler
    const oModel = this.getModel();
    oModel.attachEvent("requestFailed", this._onRequestFailed, this);
},

_onRequestFailed: function (oEvent) {
    const oResponse = oEvent.getParameter("response");
    let sMessage = "An error occurred";

    if (oResponse.responseText) {
        try {
            const oError = JSON.parse(oResponse.responseText);
            sMessage = oError.error.message.value || oError.error.message;
        } catch (e) {
            sMessage = oResponse.responseText;
        }
    }

    MessageBox.error(sMessage, {
        title: "Error (" + oResponse.statusCode + ")"
    });
}
```

### 7.2 Message Manager

```javascript
// Get Message Manager
const oMessageManager = sap.ui.getCore().getMessageManager();

// Register view for messages
onInit: function () {
    oMessageManager.registerObject(this.getView(), true);
},

// Add custom message
onAddMessage: function () {
    const oMessage = new sap.ui.core.message.Message({
        message: "Validation error on field",
        type: sap.ui.core.MessageType.Error,
        target: "/Product/ProductName",
        processor: this.getView().getModel()
    });
    oMessageManager.addMessages(oMessage);
},

// Clear messages
onClearMessages: function () {
    oMessageManager.removeAllMessages();
},

// Show message popover
onShowMessages: function (oEvent) {
    if (!this._oMessagePopover) {
        this._oMessagePopover = new sap.m.MessagePopover({
            items: {
                path: "message>/",
                template: new sap.m.MessageItem({
                    title: "{message>message}",
                    type: "{message>type}",
                    description: "{message>description}"
                })
            }
        });
        this._oMessagePopover.setModel(
            oMessageManager.getMessageModel(),
            "message"
        );
    }
    this._oMessagePopover.toggle(oEvent.getSource());
}
```

---

## 8. Custom Extensions

### 8.1 Custom Column in Fiori Elements

```javascript
// In manifest.json
"extends": {
    "extensions": {
        "sap.ui.viewExtensions": {
            "sap.suite.ui.generic.template.ListReport.view.ListReport": {
                "ResponsiveTableColumnsExtension|Product": {
                    "className": "sap.ui.core.Fragment",
                    "fragmentName": "com.company.products.ext.CustomColumn",
                    "type": "XML"
                }
            }
        }
    }
}

// CustomColumn.fragment.xml
<core:FragmentDefinition xmlns:core="sap.ui.core" xmlns="sap.m">
    <Column>
        <Text text="Custom" />
    </Column>
</core:FragmentDefinition>

// CustomColumnCell.fragment.xml
<core:FragmentDefinition xmlns:core="sap.ui.core" xmlns="sap.m">
    <Text text="{= ${Price} > 100 ? 'Premium' : 'Standard'}" />
</core:FragmentDefinition>
```

### 8.2 Custom Action in Fiori Elements

```javascript
// In manifest.json
"sap.ui.generic.app": {
    "pages": [{
        "entitySet": "Product",
        "component": {
            "name": "sap.suite.ui.generic.template.ListReport"
        },
        "pages": [{
            "entitySet": "Product",
            "component": {
                "name": "sap.suite.ui.generic.template.ObjectPage",
                "settings": {
                    "sections": {
                        "CustomSection": {
                            "type": "XMLFragment",
                            "fragmentName": "com.company.products.ext.CustomSection"
                        }
                    }
                }
            }
        }]
    }]
}

// Controller extension
sap.ui.define([
    "sap/ui/core/mvc/ControllerExtension"
], function (ControllerExtension) {
    "use strict";

    return ControllerExtension.extend("com.company.products.ext.ListReportExt", {
        override: {
            onInit: function () {
                // Custom initialization
            }
        },

        onCustomAction: function (oEvent) {
            const oContext = oEvent.getSource().getBindingContext();
            MessageToast.show("Custom action for: " + oContext.getProperty("ProductId"));
        }
    });
});
```

---

## Quick Reference

### Useful Code Snippets

```javascript
// Get i18n text
this.getView().getModel("i18n").getResourceBundle().getText("key");

// Get router
this.getOwnerComponent().getRouter();

// Get model
this.getView().getModel();
this.getView().getModel("modelName");

// Busy indicator
sap.ui.core.BusyIndicator.show(0);
sap.ui.core.BusyIndicator.hide();

// Format currency
sap.ui.core.format.NumberFormat.getCurrencyInstance().format(100, "USD");

// Format date
sap.ui.core.format.DateFormat.getDateInstance({pattern: "dd.MM.yyyy"}).format(new Date());

// Device detection
sap.ui.Device.system.phone
sap.ui.Device.system.tablet
sap.ui.Device.system.desktop
```

---

*Last updated: January 2026*
*Practice these examples in SAP Business Application Studio*

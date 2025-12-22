# Modern ABAP Syntax and CDS Views - Code Examples

## Part 1: Modern ABAP Syntax

### 1.1 Inline Declarations
```abap
" Old way
DATA: lv_customer TYPE kunnr,
      lt_orders TYPE TABLE OF vbak,
      ls_order TYPE vbak.

SELECT * FROM vbak INTO TABLE lt_orders.
LOOP AT lt_orders INTO ls_order.
  " process
ENDLOOP.

" Modern way
SELECT * FROM vbak INTO TABLE @DATA(lt_orders).
LOOP AT lt_orders INTO DATA(ls_order).
  " process
ENDLOOP.
```

### 1.2 Constructor Operators
```abap
" VALUE operator
DATA(lt_materials) = VALUE tt_matnr(
  ( '000000000000000001' )
  ( '000000000000000002' )
  ( '000000000000000003' )
).

" CORRESPONDING operator
DATA(ls_customer) = CORRESPONDING #( ls_kna1 ).

" NEW operator (for object creation)
DATA(lo_instance) = NEW zcl_my_class( iv_param = 'value' ).

" Conditional expressions
DATA(lv_status) = COND #( WHEN ls_order-status = 'A' THEN 'Active'
                           WHEN ls_order-status = 'C' THEN 'Completed'
                           ELSE 'Unknown' ).
```

### 1.3 String Templates
```abap
" Old way
CONCATENATE 'Order' lv_order 'for customer' lv_customer 
  INTO lv_message SEPARATED BY space.

" Modern way
DATA(lv_message) = |Order { lv_order } for customer { lv_customer }|.

" With formatting
DATA(lv_formatted) = |Amount: { lv_amount DECIMALS = 2 } { lv_currency }|.
DATA(lv_date) = |Date: { lv_datum DATE = USER }|.
```

### 1.4 Method Chaining and Functional Programming
```abap
DATA(lv_result) = cl_abap_typedescr=>describe_by_data( ls_data 
                  )->get_relative_name( ).

" Functional style iteration with REDUCE
DATA(lv_total) = REDUCE netwr( INIT sum = 0
                                FOR line IN lt_items
                                NEXT sum = sum + line-netwr ).

" FILTER operator
DATA(lt_high_value) = FILTER #( lt_items USING KEY primary_key 
                                WHERE netwr > 1000 ).
```

---

## Part 2: CDS Views Examples

### 2.1 Basic CDS View
```abap
@AbapCatalog.sqlViewName: 'ZCUSTOMER_V'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer Basic View'
define view Z_Customer_Basic
  as select from kna1
{
  key kunnr as CustomerId,
      name1 as CustomerName,
      ort01 as City,
      land1 as Country,
      stcd1 as TaxNumber
}
where kunnr <> ''
```

### 2.2 CDS View with Associations
```abap
@AbapCatalog.sqlViewName: 'ZSALESORDER_V'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order with Associations'
define view Z_SalesOrder
  as select from vbak
  association [0..1] to Z_Customer_Basic as _Customer 
    on $projection.SoldToParty = _Customer.CustomerId
  association [0..*] to Z_SalesOrderItem as _Items
    on $projection.SalesDocument = _Items.SalesDocument
{
  key vbeln as SalesDocument,
      erdat as CreatedOn,
      kunnr as SoldToParty,
      netwr as NetAmount,
      waerk as Currency,
      
      // Exposing associations
      _Customer,
      _Items
}
```

### 2.3 CDS View with Path Expressions
```abap
@AbapCatalog.sqlViewName: 'ZSALESORD_DET'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order Details'
define view Z_SalesOrder_Details
  as select from vbak
  association [0..1] to Z_Customer_Basic as _Customer 
    on $projection.SoldToParty = _Customer.CustomerId
{
  key vbeln as SalesDocument,
      erdat as CreatedOn,
      kunnr as SoldToParty,
      
      // Using path expression to access associated data
      _Customer.CustomerName,
      _Customer.City,
      _Customer.Country,
      
      netwr as NetAmount,
      waerk as Currency,
      
      _Customer
}
```

### 2.4 CDS View with Parameters
```abap
@AbapCatalog.sqlViewName: 'ZSALES_PARAM'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Orders by Date Range'
define view Z_SalesOrder_ByDate
  with parameters
    p_from_date : vbak.erdat,
    p_to_date   : vbak.erdat
  as select from vbak
{
  key vbeln as SalesDocument,
      erdat as CreatedOn,
      kunnr as SoldToParty,
      netwr as NetAmount,
      waerk as Currency
}
where erdat between :p_from_date and :p_to_date
```

### 2.5 CDS View with Calculations and CASE
```abap
@AbapCatalog.sqlViewName: 'ZSALES_CALC'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order with Calculations'
define view Z_SalesOrder_Calculated
  as select from vbak
{
  key vbeln as SalesDocument,
      erdat as CreatedOn,
      kunnr as SoldToParty,
      netwr as NetAmount,
      waerk as Currency,
      
      // Calculation
      cast(netwr as abap.dec(15,2)) * 1.19 as GrossAmount,
      
      // CASE statement for status
      case vbak.vbtyp
        when 'C' then 'Order'
        when 'G' then 'Contract'
        else 'Other'
      end as DocumentType,
      
      // Conditional calculation
      case 
        when netwr < 1000 then 1
        when netwr < 10000 then 2
        else 3
      end as PriceCategory
}
```

### 2.6 Analytical CDS View
```abap
@AbapCatalog.sqlViewName: 'ZSALES_CUBE'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Analysis Cube'
@Analytics.dataCategory: #CUBE
define view Z_Sales_Cube
  as select from vbak
  association [0..1] to Z_Customer_Basic as _Customer 
    on $projection.SoldToParty = _Customer.CustomerId
{
  @Analytics.dimension: true
  key vbeln as SalesDocument,
  
  @Analytics.dimension: true
  kunnr as SoldToParty,
  
  @Analytics.dimension: true
  _Customer.Country,
  
  @Analytics.dimension: true
  erdat as CreatedOn,
  
  @Semantics.currencyCode: true
  waerk as Currency,
  
  @DefaultAggregation: #SUM
  @Semantics.amount.currencyCode: 'Currency'
  netwr as NetAmount,
  
  @DefaultAggregation: #SUM
  cast(1 as abap.int4) as OrderCount,
  
  _Customer
}
```

### 2.7 CDS View with UI Annotations
```abap
@AbapCatalog.sqlViewName: 'ZSALES_UI'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order for UI'

@UI: {
  headerInfo: {
    typeName: 'Sales Order',
    typeNamePlural: 'Sales Orders',
    title: { type: #STANDARD, value: 'SalesDocument' }
  }
}

@Search.searchable: true
define view Z_SalesOrder_UI
  as select from vbak
  association [0..1] to Z_Customer_Basic as _Customer 
    on $projection.SoldToParty = _Customer.CustomerId
{
  @UI.facet: [
    { id: 'GeneralInfo',
      purpose: #STANDARD,
      type: #IDENTIFICATION_REFERENCE,
      label: 'General Information',
      position: 10 }
  ]
  
  @UI: {
    lineItem: [{ position: 10, importance: #HIGH }],
    identification: [{ position: 10 }],
    selectionField: [{ position: 10 }]
  }
  @Search.defaultSearchElement: true
  key vbeln as SalesDocument,
  
  @UI: {
    lineItem: [{ position: 20, importance: #HIGH }],
    identification: [{ position: 20 }],
    selectionField: [{ position: 20 }]
  }
  erdat as CreatedOn,
  
  @UI: {
    lineItem: [{ position: 30, importance: #HIGH }],
    identification: [{ position: 30 }]
  }
  @Consumption.valueHelpDefinition: [{
    entity: { name: 'Z_Customer_Basic', element: 'CustomerId' }
  }]
  kunnr as SoldToParty,
  
  @UI: {
    lineItem: [{ position: 40, importance: #HIGH }],
    identification: [{ position: 40 }]
  }
  @Semantics.amount.currencyCode: 'Currency'
  netwr as NetAmount,
  
  @Semantics.currencyCode: true
  waerk as Currency,
  
  _Customer
}
```

---

## Quick Reference

### CDS View Naming Conventions
- **I_**: Interface views (basic, reusable)
- **C_**: Consumption views (for UI/reporting)
- **P_**: Private views (internal use)
- **Z_** or **Y_**: Custom views

### Common Annotations
- `@AbapCatalog.sqlViewName` - Database view name (max 16 chars)
- `@EndUserText.label` - Description
- `@AccessControl.authorizationCheck` - Authorization check level
- `@Search.searchable` - Enable search
- `@UI.*` - UI rendering hints
- `@Semantics.*` - Semantic information (amount, quantity, etc.)
- `@Consumption.*` - Value helps and consumption behavior

### Association Cardinality
- `[1]` - Exactly one
- `[0..1]` - Zero or one
- `[1..*]` - One or more
- `[0..*]` - Zero or more

---

*Last updated: December 2025*

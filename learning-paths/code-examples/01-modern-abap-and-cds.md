# Modern ABAP Syntax and CDS Entity Views - Code Examples

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

## Part 2: CDS Entity Views (Modern Approach)

### 2.1 Basic CDS Entity View

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer Basic View'
define view entity ZI_Customer_Basic
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

**Key Differences from DDIC-based Views:**
- ✅ Uses `define view entity` (not `define view`)
- ✅ No `@AbapCatalog.sqlViewName` annotation (no DDIC view created)
- ✅ No `@AbapCatalog.compiler.compareFilter` (deprecated)
- ✅ Direct consumption without database view layer

### 2.2 CDS Entity View with Associations

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order with Associations'
define view entity ZI_SalesOrder
  as select from vbak
  association [0..1] to ZI_Customer_Basic as _Customer
    on $projection.SoldToParty = _Customer.CustomerId
  association [0..*] to ZI_SalesOrderItem as _Items
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

### 2.3 CDS Entity View with Path Expressions

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order Details'
define view entity ZI_SalesOrder_Details
  as select from vbak
  association [0..1] to ZI_Customer_Basic as _Customer
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

### 2.4 CDS Entity View with Parameters

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Orders by Date Range'
define view entity ZI_SalesOrder_ByDate
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

### 2.5 CDS Entity View with Calculations and CASE

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order with Calculations'
define view entity ZI_SalesOrder_Calculated
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

### 2.6 Analytical CDS Entity View (Cube)

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Analysis Cube'
@Analytics.dataCategory: #CUBE
define view entity ZI_Sales_Cube
  as select from vbak
  association [0..1] to ZI_Customer_Basic as _Customer
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

### 2.7 Consumption View (Projection) with UI Annotations

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order for UI'
@Metadata.allowExtensions: true

@UI: {
  headerInfo: {
    typeName: 'Sales Order',
    typeNamePlural: 'Sales Orders',
    title: { type: #STANDARD, value: 'SalesDocument' }
  }
}

@Search.searchable: true
define view entity ZC_SalesOrder
  as projection on ZI_SalesOrder
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
  key SalesDocument,

  @UI: {
    lineItem: [{ position: 20, importance: #HIGH }],
    identification: [{ position: 20 }],
    selectionField: [{ position: 20 }]
  }
  CreatedOn,

  @UI: {
    lineItem: [{ position: 30, importance: #HIGH }],
    identification: [{ position: 30 }]
  }
  @Consumption.valueHelpDefinition: [{
    entity: { name: 'ZI_Customer_Basic', element: 'CustomerId' }
  }]
  SoldToParty,

  @UI: {
    lineItem: [{ position: 40, importance: #HIGH }],
    identification: [{ position: 40 }]
  }
  NetAmount,

  Currency,

  /* Associations */
  _Customer,
  _Items
}
```

### 2.8 Transactional View with Draft

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Travel Booking - Transactional'
define view entity ZI_Travel
  as select from ztravel
  association [0..*] to ZI_Booking as _Booking
    on $projection.TravelId = _Booking.TravelId
{
  key travel_id     as TravelId,
      @Semantics.user.createdBy: true
      created_by    as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at    as CreatedAt,
      @Semantics.user.lastChangedBy: true
      changed_by    as ChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      changed_at    as ChangedAt,
      description   as Description,
      customer_id   as CustomerId,
      begin_date    as BeginDate,
      end_date      as EndDate,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      total_price   as TotalPrice,
      @Semantics.currencyCode: true
      currency_code as CurrencyCode,
      status        as Status,

      /* Associations */
      _Booking
}
```

### 2.9 Virtual Elements (Calculated at Runtime)

```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Product with Virtual Elements'
define view entity ZI_Product
  as select from mara
{
  key matnr      as ProductId,
      mtart      as ProductType,
      matkl      as ProductGroup,
      meins      as BaseUnit,

      // Virtual element - calculated at runtime by CDS view implementation
      @ObjectModel.virtualElement: true
      @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_PRODUCT_VIRTUAL'
      cast( '' as abap.char( 50 ) ) as ProductDescription,

      @ObjectModel.virtualElement: true
      @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_PRODUCT_VIRTUAL'
      cast( 0 as abap.int4 ) as StockLevel
}
```

---

## Part 3: Comparison - DDIC-based vs Entity Views

### DDIC-based CDS View (Old - Deprecated)
```abap
@AbapCatalog.sqlViewName: 'ZCUSTOMER_V'  // Creates database view
@AbapCatalog.compiler.compareFilter: true // Deprecated
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer'
define view Z_Customer
  as select from kna1
{
  key kunnr as CustomerId,
      name1 as CustomerName
}
```

### CDS Entity View (New - Recommended)
```abap
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer'
define view entity ZI_Customer  // No database view created
  as select from kna1
{
  key kunnr as CustomerId,
      name1 as CustomerName
}
```

### Key Advantages of CDS Entity Views:
1. ✅ **No DDIC layer** - Direct consumption, better performance
2. ✅ **Cleaner syntax** - No need for sqlViewName annotation
3. ✅ **Better tooling support** - Enhanced ADT features
4. ✅ **Future-proof** - SAP's recommended approach
5. ✅ **RAP compatibility** - Required for RAP-based applications
6. ✅ **Simplified deployment** - No transport of DDIC objects

---

## Quick Reference

### CDS Entity View Naming Conventions (VDM-Style)
- **ZI_**: Interface views (basic, reusable layer)
- **ZC_**: Consumption views (projection for UI/API)
- **ZP_**: Private views (internal use only)
- **ZE_**: Extension views (customer extensions)
- **ZR_**: Remote views (for remote APIs)

### Common Annotations
- `@EndUserText.label` - Description
- `@AccessControl.authorizationCheck` - Authorization check level
- `@Metadata.allowExtensions` - Allow metadata extensions
- `@Search.searchable` - Enable search
- `@UI.*` - UI rendering hints
- `@Semantics.*` - Semantic information (amount, quantity, user, date)
- `@Consumption.*` - Value helps and consumption behavior
- `@ObjectModel.*` - Object model settings (associations, virtual elements)
- `@Analytics.*` - Analytics annotations (for cubes/queries)

### Association Cardinality
- `[1]` - Exactly one
- `[0..1]` - Zero or one
- `[1..*]` - One or more
- `[0..*]` - Zero or more

### Semantic Annotations Examples
```abap
@Semantics.amount.currencyCode: 'Currency'
amount as Amount,

@Semantics.currencyCode: true
waers as Currency,

@Semantics.quantity.unitOfMeasure: 'Unit'
quantity as Quantity,

@Semantics.unitOfMeasure: true
meins as Unit,

@Semantics.user.createdBy: true
created_by as CreatedBy,

@Semantics.systemDateTime.createdAt: true
created_at as CreatedAt,

@Semantics.user.lastChangedBy: true
changed_by as ChangedBy,

@Semantics.systemDateTime.lastChangedAt: true
changed_at as ChangedAt
```

---

## Best Practices

### ✅ DO:
- Use `define view entity` for all new CDS views
- Follow VDM naming conventions (ZI_, ZC_)
- Use semantic annotations for amounts, currencies, dates
- Expose associations for flexibility
- Use projections (ZC_) for UI-specific requirements
- Enable draft for transactional applications

### ❌ DON'T:
- Use `define view` (DDIC-based) - it's deprecated
- Use `@AbapCatalog.sqlViewName` in new views
- Mix interface and consumption concerns in one view
- Hardcode values - use parameters instead
- Forget authorization checks in production

---

*Last updated: December 2025*
*Uses CDS Entity Views (SAP-recommended approach)*

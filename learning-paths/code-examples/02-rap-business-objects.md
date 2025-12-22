# RAP (ABAP RESTful Application Programming Model) - Code Examples

## Complete Travel Booking Application Example

### 1. Database Table
```abap
@EndUserText.label : 'Travel Booking'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table ztravel_xxx {
  key client      : abap.clnt not null;
  key travel_id   : /dmo/travel_id not null;
  agency_id       : /dmo/agency_id;
  customer_id     : /dmo/customer_id;
  begin_date      : /dmo/begin_date;
  end_date        : /dmo/end_date;
  @Semantics.amount.currencyCode : 'ztravel_xxx.currency_code'
  booking_fee     : /dmo/booking_fee;
  @Semantics.amount.currencyCode : 'ztravel_xxx.currency_code'
  total_price     : /dmo/total_price;
  currency_code   : /dmo/currency_code;
  description     : /dmo/description;
  overall_status  : /dmo/overall_status;
  created_by      : abp_creation_user;
  created_at      : abp_creation_tstmpl;
  last_changed_by : abp_locinst_lastchange_user;
  last_changed_at : abp_locinst_lastchange_tstmpl;
  local_last_changed_at : abp_lastchange_tstmpl;
}
```

### 2. Basic Interface CDS View
```abap
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Travel View - Basic Interface'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_Travel_XXX
  as select from ztravel_xxx
{
  key travel_id          as TravelId,
      agency_id          as AgencyId,
      customer_id        as CustomerId,
      begin_date         as BeginDate,
      end_date           as EndDate,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      booking_fee        as BookingFee,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      total_price        as TotalPrice,
      currency_code      as CurrencyCode,
      description        as Description,
      overall_status     as OverallStatus,
      created_by         as CreatedBy,
      created_at         as CreatedAt,
      last_changed_by    as LastChangedBy,
      last_changed_at    as LastChangedAt,
      local_last_changed_at as LocalLastChangedAt
}
```

### 3. Consumption CDS View (Projection)
```abap
@EndUserText.label: 'Travel Projection View'
@AccessControl.authorizationCheck: #NOT_REQUIRED

@UI: {
  headerInfo: {
    typeName: 'Travel',
    typeNamePlural: 'Travels',
    title: { type: #STANDARD, value: 'TravelId' }
  }
}

@Search.searchable: true
define root view entity ZC_Travel_XXX
  provider contract transactional_query
  as projection on ZI_Travel_XXX
{
  @UI.facet: [
    { id: 'Travel',
      purpose: #STANDARD,
      type: #IDENTIFICATION_REFERENCE,
      label: 'Travel',
      position: 10 }
  ]
  
  @UI: {
    lineItem: [{ position: 10, importance: #HIGH }],
    identification: [{ position: 10 }]
  }
  @Search.defaultSearchElement: true
  key TravelId,
  
  @UI: {
    lineItem: [{ position: 20, importance: #HIGH }],
    identification: [{ position: 20 }]
  }
  @Consumption.valueHelpDefinition: [{
    entity: { name: '/DMO/I_Agency', element: 'AgencyID' }
  }]
  AgencyId,
  
  @UI: {
    lineItem: [{ position: 30, importance: #HIGH }],
    identification: [{ position: 30 }]
  }
  @Consumption.valueHelpDefinition: [{
    entity: { name: '/DMO/I_Customer', element: 'CustomerID' }
  }]
  CustomerId,
  
  @UI: {
    lineItem: [{ position: 40 }],
    identification: [{ position: 40 }]
  }
  BeginDate,
  
  @UI: {
    lineItem: [{ position: 50 }],
    identification: [{ position: 50 }]
  }
  EndDate,
  
  @UI: {
    lineItem: [{ position: 60 }],
    identification: [{ position: 60 }]
  }
  BookingFee,
  
  @UI: {
    lineItem: [{ position: 70, importance: #HIGH }],
    identification: [{ position: 70 }]
  }
  TotalPrice,
  
  CurrencyCode,
  
  @UI: {
    lineItem: [{ position: 80 }],
    identification: [{ position: 80 }]
  }
  Description,
  
  @UI: {
    lineItem: [{ position: 90, importance: #HIGH, criticality: 'StatusCriticality' }],
    identification: [{ position: 90 }]
  }
  OverallStatus,
  
  // Virtual element for criticality
  case OverallStatus
    when 'O' then 2  // yellow
    when 'A' then 3  // green
    when 'X' then 1  // red
    else 0
  end as StatusCriticality,
  
  CreatedBy,
  CreatedAt,
  LastChangedBy,
  LastChangedAt,
  LocalLastChangedAt
}
```

### 4. Behavior Definition (Managed Scenario)
```abap
managed implementation in class zbp_i_travel_xxx unique;
strict ( 2 );

define behavior for ZI_Travel_XXX alias Travel
persistent table ztravel_xxx
draft table ztravel_xxx_d
lock master total etag LastChangedAt
authorization master ( instance )
{
  // Standard operations
  create;
  update;
  delete;
  
  // Field properties
  field ( readonly ) TravelId, CreatedBy, CreatedAt, LastChangedBy, LastChangedAt, LocalLastChangedAt;
  field ( mandatory ) AgencyId, CustomerId, BeginDate, EndDate;
  
  // Validations
  validation validateCustomer on save { field CustomerId; create; }
  validation validateDates on save { field BeginDate, EndDate; create; }
  validation validateAgency on save { field AgencyId; create; }
  
  // Determinations
  determination calculateTotalPrice on modify { field BookingFee; }
  determination setInitialStatus on modify { create; }
  
  // Actions
  action acceptTravel result [1] $self;
  action rejectTravel result [1] $self;
  
  // Draft handling
  draft action Edit;
  draft action Activate;
  draft action Discard;
  draft action Resume;
  draft determine action Prepare;
  
  mapping for ztravel_xxx
  {
    TravelId = travel_id;
    AgencyId = agency_id;
    CustomerId = customer_id;
    BeginDate = begin_date;
    EndDate = end_date;
    BookingFee = booking_fee;
    TotalPrice = total_price;
    CurrencyCode = currency_code;
    Description = description;
    OverallStatus = overall_status;
    CreatedBy = created_by;
    CreatedAt = created_at;
    LastChangedBy = last_changed_by;
    LastChangedAt = last_changed_at;
    LocalLastChangedAt = local_last_changed_at;
  }
}
```

### 5. Behavior Implementation Class
```abap
CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS validateCustomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateCustomer.
      
    METHODS validateDates FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateDates.
      
    METHODS validateAgency FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateAgency.
      
    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~calculateTotalPrice.
      
    METHODS setInitialStatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~setInitialStatus.
      
    METHODS acceptTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~acceptTravel RESULT result.
      
    METHODS rejectTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~rejectTravel RESULT result.
ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.

  METHOD validateCustomer.
    " Read travel data
    READ ENTITIES OF zi_travel_xxx IN LOCAL MODE
      ENTITY Travel
        FIELDS ( CustomerId )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels).
      
    " Check if customer exists
    SELECT FROM /dmo/customer
      FIELDS customer_id
      FOR ALL ENTRIES IN @lt_travels
      WHERE customer_id = @lt_travels-CustomerId
      INTO TABLE @DATA(lt_customers).
      
    " Raise error for invalid customers
    LOOP AT lt_travels INTO DATA(ls_travel).
      IF NOT line_exists( lt_customers[ customer_id = ls_travel-CustomerId ] ).
        APPEND VALUE #( %tky = ls_travel-%tky ) TO failed-travel.
        APPEND VALUE #(
          %tky = ls_travel-%tky
          %msg = new_message_with_text(
            severity = if_abap_behv_message=>severity-error
            text = |Customer { ls_travel-CustomerId } does not exist| )
          %element-CustomerId = if_abap_behv=>mk-on
        ) TO reported-travel.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateDates.
    READ ENTITIES OF zi_travel_xxx IN LOCAL MODE
      ENTITY Travel
        FIELDS ( BeginDate EndDate )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels).
      
    LOOP AT lt_travels INTO DATA(ls_travel).
      " Check if begin date is in the future
      IF ls_travel-BeginDate < cl_abap_context_info=>get_system_date( ).
        APPEND VALUE #( %tky = ls_travel-%tky ) TO failed-travel.
        APPEND VALUE #(
          %tky = ls_travel-%tky
          %msg = new_message_with_text(
            severity = if_abap_behv_message=>severity-error
            text = 'Begin date must be in the future' )
          %element-BeginDate = if_abap_behv=>mk-on
        ) TO reported-travel.
      ENDIF.
      
      " Check if end date is after begin date
      IF ls_travel-EndDate < ls_travel-BeginDate.
        APPEND VALUE #( %tky = ls_travel-%tky ) TO failed-travel.
        APPEND VALUE #(
          %tky = ls_travel-%tky
          %msg = new_message_with_text(
            severity = if_abap_behv_message=>severity-error
            text = 'End date must be after begin date' )
          %element-EndDate = if_abap_behv=>mk-on
          %element-BeginDate = if_abap_behv=>mk-on
        ) TO reported-travel.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateAgency.
    READ ENTITIES OF zi_travel_xxx IN LOCAL MODE
      ENTITY Travel
        FIELDS ( AgencyId )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels).
      
    SELECT FROM /dmo/agency
      FIELDS agency_id
      FOR ALL ENTRIES IN @lt_travels
      WHERE agency_id = @lt_travels-AgencyId
      INTO TABLE @DATA(lt_agencies).
      
    LOOP AT lt_travels INTO DATA(ls_travel).
      IF NOT line_exists( lt_agencies[ agency_id = ls_travel-AgencyId ] ).
        APPEND VALUE #( %tky = ls_travel-%tky ) TO failed-travel.
        APPEND VALUE #(
          %tky = ls_travel-%tky
          %msg = new_message_with_text(
            severity = if_abap_behv_message=>severity-error
            text = |Agency { ls_travel-AgencyId } does not exist| )
          %element-AgencyId = if_abap_behv=>mk-on
        ) TO reported-travel.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD calculateTotalPrice.
    READ ENTITIES OF zi_travel_xxx IN LOCAL MODE
      ENTITY Travel
        FIELDS ( BookingFee )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels).
      
    MODIFY ENTITIES OF zi_travel_xxx IN LOCAL MODE
      ENTITY Travel
        UPDATE FIELDS ( TotalPrice )
        WITH VALUE #( FOR travel IN lt_travels (
          %tky = travel-%tky
          TotalPrice = travel-BookingFee * '1.1' ) ).
  ENDMETHOD.

  METHOD setInitialStatus.
    READ ENTITIES OF zi_travel_xxx IN LOCAL MODE
      ENTITY Travel
        FIELDS ( OverallStatus )
        WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels).
      
    DELETE lt_travels WHERE OverallStatus IS NOT INITIAL.
    CHECK lt_travels IS NOT INITIAL.
    
    MODIFY ENTITIES OF zi_travel_xxx IN LOCAL MODE
      ENTITY Travel
        UPDATE FIELDS ( OverallStatus )
        WITH VALUE #( FOR travel IN lt_travels (
          %tky = travel-%tky
          OverallStatus = 'O' ) ). " Open
  ENDMETHOD.

  METHOD acceptTravel.
    MODIFY ENTITIES OF zi_travel_xxx IN LOCAL MODE
      ENTITY Travel
        UPDATE FIELDS ( OverallStatus )
        WITH VALUE #( FOR key IN keys (
          %tky = key-%tky
          OverallStatus = 'A' ) ). " Accepted
          
    READ ENTITIES OF zi_travel_xxx IN LOCAL MODE
      ENTITY Travel
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels).
      
    result = VALUE #( FOR travel IN lt_travels (
      %tky = travel-%tky
      %param = travel ) ).
  ENDMETHOD.

  METHOD rejectTravel.
    MODIFY ENTITIES OF zi_travel_xxx IN LOCAL MODE
      ENTITY Travel
        UPDATE FIELDS ( OverallStatus )
        WITH VALUE #( FOR key IN keys (
          %tky = key-%tky
          OverallStatus = 'X' ) ). " Rejected
          
    READ ENTITIES OF zi_travel_xxx IN LOCAL MODE
      ENTITY Travel
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels).
      
    result = VALUE #( FOR travel IN lt_travels (
      %tky = travel-%tky
      %param = travel ) ).
  ENDMETHOD.

ENDCLASS.
```

### 6. Service Definition
```abap
@EndUserText.label: 'Service Definition for Travel'
define service Z_UI_TRAVEL_XXX {
  expose ZC_Travel_XXX as Travel;
}
```

### 7. Service Binding

Create via ADT:
1. Right-click on Service Definition
2. New → Service Binding
3. Name: `Z_UI_TRAVEL_XXX_O2`
4. Binding Type: `OData V2 - UI`
5. Activate and publish

---

## RAP Key Concepts

### Managed vs Unmanaged

**Managed (Framework-managed):**
- Framework handles database operations
- Less code to write
- Automatic numbering, locking, transactions
- Use for new developments

**Unmanaged (Developer-managed):**
- You control all database operations
- More code, more control
- Use for wrapping existing logic/BAPIs

### Important RAP Keywords

- `%tky` - Transactional key (unique identifier)
- `%cid` - Content ID (for create operations)
- `%msg` - Message handling
- `%element` - Field-level message assignment
- `%control` - Field change tracking
- `%param` - Action parameter passing

### Common Patterns

**Reading entities:**
```abap
READ ENTITIES OF zi_travel_xxx IN LOCAL MODE
  ENTITY Travel
    FIELDS ( CustomerId AgencyId )
    WITH CORRESPONDING #( keys )
  RESULT DATA(lt_result).
```

**Modifying entities:**
```abap
MODIFY ENTITIES OF zi_travel_xxx IN LOCAL MODE
  ENTITY Travel
    UPDATE FIELDS ( OverallStatus )
    WITH VALUE #( ( %tky = ls_key-%tky
                    OverallStatus = 'A' ) ).
```

**Creating entities:**
```abap
MODIFY ENTITIES OF zi_travel_xxx IN LOCAL MODE
  ENTITY Travel
    CREATE FIELDS ( AgencyId CustomerId BeginDate EndDate )
    WITH VALUE #( ( %cid = 'CID_001'
                    AgencyId = '070001'
                    CustomerId = '000001'
                    BeginDate = '20260101'
                    EndDate = '20260110' ) )
  MAPPED DATA(ls_mapped).
```

---

*Last updated: December 2025*

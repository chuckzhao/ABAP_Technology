# ABAP Naming Conventions Guide

This guide provides comprehensive naming conventions for ABAP development, aligned with SAP best practices and Clean ABAP principles.

## Table of Contents
- [General Principles](#general-principles)
- [Database Objects](#database-objects)
- [ABAP Objects](#abap-objects)
- [Variables and Data Declarations](#variables-and-data-declarations)
- [CDS Views](#cds-views)
- [RAP Objects](#rap-objects)
- [Fiori/UI5 Elements](#fioriui5-elements)
- [Package and Namespace Guidelines](#package-and-namespace-guidelines)

---

## General Principles

### Naming Philosophy
- **Descriptive**: Names should clearly indicate purpose
- **Consistent**: Follow the same pattern throughout your project
- **Meaningful**: Avoid cryptic abbreviations
- **Searchable**: Use unique prefixes for easy searching
- **Length**: Balance between descriptive and concise (max 30 characters for most objects)

### Character Rules
- Use only **uppercase letters, numbers, and underscores**
- No special characters except underscore `_`
- Start with a letter (never a number)
- Use English language for all names

### Namespace Conventions
```abap
" Customer namespace (recommended for all custom objects)
Z* or Y*

" Example:
ZTABLE_NAME    " Z-prefix for customer development
YTABLE_NAME    " Y-prefix for customer development
```

---

## Database Objects

### Database Tables

#### Transparent Tables
```abap
" Pattern: Z[DOMAIN]_[OBJECT]_[TYPE]
" Type suffixes: none for master data, _T for transaction data

Examples:
ZTRAVEL_BOOKING        " Travel booking master data
ZTRAVEL_BOOKING_T      " Travel booking transactions
ZEMPLOYEE              " Employee master data
ZSALES_ORDER_T         " Sales order transactions
ZINVENTORY_ITEM        " Inventory items
```

#### Structure Tables
```abap
" Pattern: Z[OBJECT]_S

Examples:
ZADDRESS_S             " Address structure
ZCONTACT_S             " Contact information structure
ZPAYMENT_S             " Payment structure
```

#### Append Structures
```abap
" Pattern: ZA_[BASE_TABLE]

Examples:
ZA_MARA                " Append structure for MARA
ZA_VBAK                " Append structure for VBAK
```

#### Table Types
```abap
" Pattern: Z[OBJECT]_TT

Examples:
ZEMPLOYEE_TT           " Table type for employee
ZORDER_ITEM_TT         " Table type for order items
```

### Database Indexes
```abap
" Pattern: Z[TABLE]~[INDEX_NUMBER]

Examples:
ZTRAVEL~001            " Index 001 for ZTRAVEL table
ZEMPLOYEE~002          " Index 002 for ZEMPLOYEE table
```

### Views (Classic)
```abap
" Database View:     Z[OBJECT]_V
" Projection View:   Z[OBJECT]_P
" Maintenance View:  Z[OBJECT]_M

Examples:
ZTRAVEL_V              " Database view for travel data
ZEMPLOYEE_P            " Projection view for employee
ZPRODUCT_M             " Maintenance view for product
```

---

## ABAP Objects

### Classes

#### Global Classes
```abap
" Pattern: Z[CL|CM]_[DOMAIN]_[PURPOSE]
" CL = Class, CM = Class Manager

Examples:
ZCL_TRAVEL_MANAGER              " Travel management class
ZCL_EMPLOYEE_VALIDATOR          " Employee validation class
ZCL_INVOICE_PROCESSOR           " Invoice processing class
ZCL_HTTP_CLIENT_HELPER          " HTTP client helper class
ZCL_DATA_ACCESS_LAYER           " Data access layer class
```

#### Class Types by Purpose
```abap
" Business Logic:
ZCL_[DOMAIN]_MANAGER
ZCL_[DOMAIN]_PROCESSOR
ZCL_[DOMAIN]_CALCULATOR

" Data Access:
ZCL_[DOMAIN]_DAO
ZCL_[DOMAIN]_REPOSITORY

" Validation:
ZCL_[DOMAIN]_VALIDATOR
ZCL_[DOMAIN]_CHECKER

" Utilities:
ZCL_UTIL_[PURPOSE]
ZCL_HELPER_[PURPOSE]

" Exception Classes:
ZCX_[DOMAIN]_[ERROR_TYPE]
```

#### Local Classes (in includes)
```abap
" Pattern: lcl_[purpose]

Examples:
lcl_helper                      " Local helper class
lcl_data_provider               " Local data provider
lcl_calculator                  " Local calculator class
```

### Interfaces

#### Global Interfaces
```abap
" Pattern: ZIF_[DOMAIN]_[PURPOSE]

Examples:
ZIF_TRAVEL_PROCESSOR            " Travel processor interface
ZIF_EMPLOYEE_VALIDATOR          " Employee validator interface
ZIF_DATA_PROVIDER               " Data provider interface
ZIF_PERSISTENCE_LAYER           " Persistence layer interface
```

#### Local Interfaces
```abap
" Pattern: lif_[purpose]

Examples:
lif_validator                   " Local validator interface
lif_data_access                 " Local data access interface
```

### Methods

#### Method Naming
```abap
" Use verb-noun pattern
" Start with action verb

Examples (Public Methods):
get_employee_data              " Getter method
set_employee_salary            " Setter method
calculate_total_amount         " Calculation method
validate_input_data            " Validation method
process_order                  " Processing method
create_invoice                 " Creation method
delete_obsolete_records        " Deletion method
check_authorization            " Check method
is_valid                       " Boolean check (returns abap_bool)
has_errors                     " Boolean check

Examples (Private Methods):
" Prefix with underscore or use clear naming
_initialize_data
_validate_internal
_build_where_clause
prepare_result_set
filter_active_items
```

### Function Modules

#### Function Groups
```abap
" Pattern: Z[DOMAIN]_[PURPOSE]

Examples:
ZTRAVEL_UTILS                   " Travel utilities function group
ZEMPLOYEE_MGT                   " Employee management function group
ZINVOICE_PROC                   " Invoice processing function group
```

#### Function Modules
```abap
" Pattern: Z_[DOMAIN]_[ACTION]_[OBJECT]

Examples:
Z_TRAVEL_CREATE_BOOKING         " Create travel booking
Z_EMPLOYEE_GET_DETAILS          " Get employee details
Z_INVOICE_CALCULATE_TOTAL       " Calculate invoice total
Z_ORDER_VALIDATE_ITEMS          " Validate order items
Z_UTIL_CONVERT_DATE             " Utility: Convert date
```

### Programs (Reports)

#### Executable Programs
```abap
" Pattern: Z[DOMAIN]_R_[PURPOSE]
" R indicates Report

Examples:
ZTRAVEL_R_BOOKING_REPORT        " Travel booking report
ZEMPLOYEE_R_SALARY_LIST         " Employee salary list report
ZINVOICE_R_MONTHLY_SUMMARY      " Monthly invoice summary
```

#### Include Programs
```abap
" Pattern: Z[PROGRAM]_[TYPE]
" Types: TOP (declarations), F01 (forms), O01 (PBO), I01 (PAI)

Examples:
ZTRAVEL_R_BOOKING_TOP           " Top include (data declarations)
ZTRAVEL_R_BOOKING_F01           " Forms include
ZTRAVEL_R_BOOKING_O01           " PBO modules
ZTRAVEL_R_BOOKING_I01           " PAI modules
```

### Module Pool Programs
```abap
" Pattern: SAPMZ[DOMAIN]_[PURPOSE]

Examples:
SAPMZTRAVEL_BOOKING             " Travel booking module pool
SAPMZEMPLOYEE_MAINTENANCE       " Employee maintenance module pool
```

---

## Variables and Data Declarations

### Variable Prefixes (Hungarian Notation - Optional but Recommended)

#### Data Objects
```abap
" Local variables
DATA: lv_employee_id     TYPE zemp_id,          " Local variable
      lv_total_amount    TYPE p DECIMALS 2,     " Local variable
      lv_is_valid        TYPE abap_bool.        " Local boolean

" Global variables (class attributes)
DATA: gv_instance_count  TYPE i,                " Global variable
      gv_max_records     TYPE i.                " Global variable

" Constants
CONSTANTS: lc_max_retry  TYPE i VALUE 3,        " Local constant
           gc_status_ok  TYPE char1 VALUE 'S'.  " Global constant

" Field symbols
FIELD-SYMBOLS: <fs_employee> TYPE zemployee_s,  " Field symbol
               <fs_line>     TYPE any.          " Generic field symbol

" Data references
DATA: lr_employee        TYPE REF TO zemployee, " Local reference
      go_manager         TYPE REF TO zcl_manager. " Global object reference

" Tables (internal tables)
DATA: lt_employees       TYPE TABLE OF zemployee, " Local table
      gt_orders          TYPE TABLE OF zorder.    " Global table

" Work areas / Structures
DATA: ls_employee        TYPE zemployee,        " Local structure
      gs_config          TYPE zconfig.          " Global structure

" Parameters (in methods/forms)
METHODS calculate_total
  IMPORTING
    iv_order_id        TYPE zorder_id           " Import parameter (value)
    ir_config          TYPE REF TO zconfig      " Import parameter (reference)
  EXPORTING
    ev_total           TYPE p                   " Export parameter (value)
    et_items           TYPE zitem_tt            " Export parameter (table)
  CHANGING
    cv_status          TYPE zstatus             " Changing parameter (value)
    ct_log             TYPE zlog_tt             " Changing parameter (table)
  RETURNING
    VALUE(rv_result)   TYPE abap_bool.          " Returning parameter
```

#### Prefix Legend
| Prefix | Scope | Type | Example |
|--------|-------|------|---------|
| `lv_` | Local | Variable (single value) | `lv_counter` |
| `gv_` | Global | Variable (single value) | `gv_max_items` |
| `lc_` | Local | Constant | `lc_pi` |
| `gc_` | Global | Constant | `gc_version` |
| `ls_` | Local | Structure/Work Area | `ls_employee` |
| `gs_` | Global | Structure/Work Area | `gs_settings` |
| `lt_` | Local | Internal Table | `lt_orders` |
| `gt_` | Global | Internal Table | `gt_cache` |
| `lr_` | Local | Data Reference | `lr_data` |
| `gr_` | Global | Data Reference | `gr_instance` |
| `lo_` | Local | Object Reference | `lo_validator` |
| `go_` | Global | Object Reference | `go_manager` |
| `<fs_>` | N/A | Field Symbol | `<fs_item>` |
| `iv_` | Import | Value parameter | `iv_id` |
| `ir_` | Import | Reference parameter | `ir_object` |
| `ev_` | Export | Value parameter | `ev_result` |
| `er_` | Export | Reference parameter | `er_data` |
| `cv_` | Changing | Value parameter | `cv_status` |
| `cr_` | Changing | Reference parameter | `cr_config` |
| `rv_` | Returning | Value parameter | `rv_success` |

### Modern ABAP (No Prefixes - Clean ABAP Alternative)
```abap
" Clean ABAP style (preferred in modern development)
DATA employee_id     TYPE zemp_id.
DATA total_amount    TYPE p DECIMALS 2.
DATA is_valid        TYPE abap_bool.

" Use inline declarations
SELECT * FROM zemployees
  INTO TABLE @DATA(employees)    " Inline table declaration
  WHERE country = @country_code.

DATA(total) = calculate_sum( employees ). " Inline variable
```

---

## CDS Views

### CDS View Naming

#### Interface Views (Basic/Primitive)
```abap
" Pattern: Z[I|C]_[DOMAIN]_[OBJECT]
" I = Interface View, C = Consumption View

Examples:
ZI_TravelBooking                " Interface view for travel booking
ZI_Employee                     " Interface view for employee
ZI_SalesOrder                   " Interface view for sales order
ZI_ProductMaster                " Interface view for product master
```

#### Composite Interface Views
```abap
" Pattern: ZI_[DOMAIN]_[OBJECT]_[COMPOSITION]

Examples:
ZI_Travel_Booking_Header        " Header view
ZI_Travel_Booking_Item          " Item view
ZI_Order_with_Customer          " Order with customer data
```

#### Consumption/Projection Views
```abap
" Pattern: ZC_[DOMAIN]_[OBJECT]_[PURPOSE]

Examples:
ZC_TravelBooking                " Consumption view for travel booking
ZC_Employee_Manager             " Consumption view for managers
ZC_SalesOrder_Analytics         " Analytics consumption view
ZC_Product_Catalog              " Product catalog view
```

#### View Types by Purpose
```abap
" Basic Interface:        ZI_[Object]
" Composite Interface:    ZI_[Object]_[Composition]
" Consumption:            ZC_[Object]_[Purpose]
" Extension:              ZE_[Object]_[Extension]
" Private:                ZP_[Object]
" Remote:                 ZR_[Object]

Examples:
ZI_Travel                       " Basic interface
ZI_Travel_Booking               " Composite interface
ZC_Travel_Processor             " Consumption view
ZE_Travel_Custom                " Extension view
ZP_Travel_Helper                " Private view (internal use)
ZR_Travel_API                   " Remote-enabled view
```

### CDS Annotations Naming
```abap
@AbapCatalog.sqlViewName: 'ZVITRAVEL'        " Technical view name (max 16 chars)
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Travel Booking'
@VDM.viewType: #COMPOSITE                    " #BASIC, #COMPOSITE, #CONSUMPTION

" SQL View name pattern: ZV[I|C]_[SHORT_NAME]
Examples:
ZVI_TRAVEL                      " SQL view for ZI_Travel
ZVC_TRAVPROC                    " SQL view for ZC_Travel_Processor
```

---

## RAP Objects

### RAP Naming Conventions

#### Behavior Definitions
```abap
" Pattern: Z[I|C]_[OBJECT] (same as CDS view name)

Examples:
ZI_TravelBooking                " Behavior definition for interface view
ZC_TravelBooking                " Behavior definition for consumption view
```

#### Behavior Implementation Classes
```abap
" Pattern: ZBP_[I|C]_[OBJECT]
" BP = Behavior Pool

Examples:
ZBP_I_TravelBooking             " Behavior implementation for ZI_TravelBooking
ZBP_C_TravelBooking             " Behavior implementation for ZC_TravelBooking

" Handler classes (inside behavior pool)
CLASS lhc_travel DEFINITION.    " Local handler class for Travel entity
CLASS lsc_travel DEFINITION.    " Local saver class for Travel entity
```

#### Service Definitions
```abap
" Pattern: Z[UI|API]_[DOMAIN]_[VERSION]
" UI = Fiori app, API = external API

Examples:
ZUI_TRAVEL_O4                   " UI service for Travel (OData V4)
ZAPI_EMPLOYEE_V1                " API service for Employee (version 1)
ZUI_SALES_ORDER_O2              " UI service for Sales (OData V2)
```

#### Service Bindings
```abap
" Pattern: Z[UI|API]_[DOMAIN]_[PROTOCOL]_[VERSION]
" Protocol: O2 (OData V2), O4 (OData V4)

Examples:
ZUI_TRAVEL_O4_V1                " UI OData V4 service binding v1
ZAPI_EMPLOYEE_O4                " API OData V4 service binding
ZUI_SALES_O2                    " UI OData V2 service binding
```

#### Metadata Extensions
```abap
" Pattern: Z[I|C]_[OBJECT]_[PURPOSE]

Examples:
ZC_TravelBooking_Metadata       " Metadata extension for ZC_TravelBooking
ZC_Employee_UI                  " UI metadata extension
```

---

## Fiori/UI5 Elements

### BSP Applications
```abap
" Pattern: Z_[DOMAIN]_[APP_TYPE]

Examples:
Z_TRAVEL_MANAGE                 " Travel management app
Z_EMPLOYEE_LIST                 " Employee list app
Z_INVOICE_APPROVAL              " Invoice approval app
```

### OData Services (Gateway)
```abap
" Service Model: Z[DOMAIN]_[PURPOSE]_MDL
" Service Data:  Z[DOMAIN]_[PURPOSE]_DPC (Data Provider Class)
" Service Ext:   Z[DOMAIN]_[PURPOSE]_DPC_EXT

Examples:
ZTRAVEL_BOOKING_MDL             " Service model
ZTRAVEL_BOOKING_DPC             " Data provider class (generated)
ZTRAVEL_BOOKING_DPC_EXT         " Data provider extension (custom)
```

---

## Package and Namespace Guidelines

### Package Naming
```abap
" Pattern: Z[AREA]_[DOMAIN]_[LAYER]

Examples:
ZTRAVEL_CORE                    " Core travel functionality
ZTRAVEL_UI                      " Travel UI layer
ZTRAVEL_API                     " Travel API layer
ZEMPLOYEE_MGT                   " Employee management
ZCOMMON_UTILS                   " Common utilities

" Package hierarchy:
ZTRAVEL                         " Parent package
  ├── ZTRAVEL_CORE              " Business logic
  ├── ZTRAVEL_DATA              " Data model (tables, CDS)
  ├── ZTRAVEL_API               " Services and APIs
  └── ZTRAVEL_UI                " UI components
```

### Transport Organization
```abap
" Development package:   Z[PROJECT]_DEV
" Customizing package:   Z[PROJECT]_CUST
" Test package:          $TMP or Z[PROJECT]_TEST

Examples:
ZTRAVEL_DEV                     " Development objects
ZTRAVEL_CUST                    " Customizing/configuration
ZTRAVEL_TEST                    " Test objects (local)
```

---

## Best Practices Summary

### DO's ✅
- Use meaningful, descriptive names
- Follow SAP standard naming patterns
- Use consistent prefixes (Z or Y)
- Keep names under 30 characters when possible
- Use English language exclusively
- Document complex naming in code comments
- Use CamelCase for CDS views (modern style)
- Use snake_case for variables (modern ABAP)

### DON'Ts ❌
- Don't use cryptic abbreviations
- Don't mix naming conventions in one project
- Don't exceed SAP length limitations
- Don't use SAP namespace (A-X) for custom objects
- Don't use special characters except underscore
- Don't start names with numbers
- Don't use reserved ABAP keywords

---

## Quick Reference Table

| Object Type | Pattern | Example |
|-------------|---------|---------|
| **Database Table** | `Z[DOMAIN]_[OBJECT]` | `ZTRAVEL_BOOKING` |
| **Structure** | `Z[OBJECT]_S` | `ZEMPLOYEE_S` |
| **Table Type** | `Z[OBJECT]_TT` | `ZORDER_TT` |
| **Class** | `ZCL_[DOMAIN]_[PURPOSE]` | `ZCL_TRAVEL_MANAGER` |
| **Interface** | `ZIF_[DOMAIN]_[PURPOSE]` | `ZIF_DATA_PROVIDER` |
| **Exception** | `ZCX_[DOMAIN]_[ERROR]` | `ZCX_TRAVEL_NOT_FOUND` |
| **Function Module** | `Z_[DOMAIN]_[ACTION]` | `Z_TRAVEL_CREATE` |
| **Report** | `Z[DOMAIN]_R_[PURPOSE]` | `ZTRAVEL_R_REPORT` |
| **CDS Interface** | `ZI_[Object]` | `ZI_TravelBooking` |
| **CDS Consumption** | `ZC_[Object]` | `ZC_TravelBooking` |
| **Behavior Pool** | `ZBP_[I\|C]_[OBJECT]` | `ZBP_I_Travel` |
| **Service Definition** | `Z[UI\|API]_[DOMAIN]` | `ZUI_TRAVEL_O4` |
| **Package** | `Z[AREA]_[DOMAIN]` | `ZTRAVEL_CORE` |

---

## References
- [Clean ABAP Style Guide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md)
- SAP Naming Conventions for Development Objects
- ABAP RESTful Application Programming Model (RAP) Guidelines
- Virtual Data Model (VDM) Naming Conventions

---

**Last Updated:** December 2025
**Version:** 1.0

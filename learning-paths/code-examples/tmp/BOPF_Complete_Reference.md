# BOPF — Complete Developer Reference

A comprehensive reference for consuming, modifying, and understanding existing BOPF Business Objects in SAP S/4HANA and related modules (TM, EH&S, etc.). Covers full CRUD, Actions, the LUW lifecycle, behavior internals (Determinations, Validations), and tracing an end-to-end BO process.

---

## Table of Contents

1. [Naming Clarification](#naming-clarification)
2. [Architecture Overview](#architecture-overview)
3. [The Three Managers](#the-three-managers)
4. [The Constants Interface](#the-constants-interface)
5. [Exploring an Unknown BO](#exploring-an-unknown-bo)
   - [/BOBF/CONF_UI — Model Browser](#bobfconf_ui--model-browser)
   - [/BOBF/TEST_UI — Interactive Data Browser](#bobftest_ui--interactive-data-browser)
6. [READ Operations](#read-operations)
   - [Pattern 1: Query → Retrieve](#pattern-1-query--retrieve)
   - [Pattern 2: Convert Alternative Key → Retrieve](#pattern-2-convert-alternative-key--retrieve)
   - [Pattern 3: Retrieve by Association](#pattern-3-retrieve-by-association)
7. [WRITE Operations and the LUW Lifecycle](#write-operations-and-the-luw-lifecycle)
   - [The Interaction Phase vs. Save Phase](#the-interaction-phase-vs-save-phase)
   - [The Modification Structure](#the-modification-structure)
   - [Pattern 4: Create a Root Node Instance](#pattern-4-create-a-root-node-instance)
   - [Pattern 5: Create Root + Child in One Transaction](#pattern-5-create-root--child-in-one-transaction)
   - [Pattern 6: Update a Node Instance](#pattern-6-update-a-node-instance)
   - [Pattern 7: Delete a Node Instance](#pattern-7-delete-a-node-instance)
   - [Saving and Rollback](#saving-and-rollback)
8. [Executing Actions](#executing-actions)
   - [What Actions Are](#what-actions-are)
   - [Pattern 8: Execute an Action (Consumer)](#pattern-8-execute-an-action-consumer)
   - [Inside an Action Implementation](#inside-an-action-implementation)
9. [Understanding Determinations](#understanding-determinations)
   - [What They Are and When They Fire](#what-they-are-and-when-they-fire)
   - [Finding Determinations on an Existing BO](#finding-determinations-on-an-existing-bo)
   - [Inside a Determination Implementation](#inside-a-determination-implementation)
10. [Understanding Validations](#understanding-validations)
    - [Types of Validations](#types-of-validations)
    - [Triggering a Validation Manually](#triggering-a-validation-manually)
    - [Inside a Validation Implementation](#inside-a-validation-implementation)
11. [Tracing a Complete BO Process End-to-End](#tracing-a-complete-bo-process-end-to-end)
12. [Error Handling](#error-handling)
13. [Performance Patterns](#performance-patterns)
14. [Investigation Checklist](#investigation-checklist)
15. [Common Existing BOs](#common-existing-bos)
16. [Key Interfaces and Types Reference](#key-interfaces-and-types-reference)
17. [Transactions Summary](#transactions-summary)

---

## Naming Clarification

| Name | Meaning |
|------|---------|
| **BOPF** | Business Object Processing Framework — the conceptual/marketing name |
| **BOBF** | Business Object Building Framework — the actual SAP package namespace |

In code, transactions, and type names you will always see **BOBF** (e.g., `/BOBF/CONF_UI`, `/BOBF/IF_FRW_QUERY`, `sc_bo_key`). Both acronyms refer to the same framework.

---

## Architecture Overview

A BOPF Business Object (BO) is a **hierarchical tree of nodes**. Each node maps to a DDIC table. The framework manages the complete lifecycle — buffer, locks, transaction, authorization — so consumers interact exclusively through the API.

```
BO: /BOBF/EPM_SALES_ORDER
│
├── ROOT node          header data        /BOBF/D_SO_ROOT
│   ├── Alternative Key:  SALES_ORDER_ID  (human-readable identifier)
│   ├── Query:            SELECT_BY_ELEMENTS
│   ├── Actions:          MARK_AS_PAID, CANCEL_ORDER
│   ├── Determinations:   CALC_GROSS_AMOUNT  (fires after item change)
│   ├── Validations:      CHECK_DATES, CHECK_CUSTOMER
│   └── Association ──────────────────────────────────────────────────┐
│                                                                      ▼
├── ITEM node          line items         /BOBF/D_SO_ITEM             │
│   └── Association ──────────────────────────────────────────────────┤
│                                                                      ▼
└── CONTACT node       contact persons    /BOBF/D_SO_CONTACT
```

**Every table managed by BOPF has these mandatory key fields:**

```abap
DB_KEY   TYPE /BOBF/BOPF_KEY    " Internal GUID — primary key
ROOT_KEY TYPE /BOBF/BOPF_KEY    " GUID of the ROOT instance (same as DB_KEY for root)
PARENT_KEY TYPE /BOBF/BOPF_KEY  " GUID of the immediate parent node instance
```

Never SELECT directly from BOPF-managed tables in consumer code. Always use the API. Direct SELECTs bypass the buffer, bypass locks, and bypass determinations.

---

## The Three Managers

| Manager | Interface | Factory | Purpose |
|---------|-----------|---------|---------|
| **Service Manager** | `/BOBF/IF_TRA_SERVICE_MANAGER` | `/BOBF/CL_TRA_SERV_MGR_FACTORY` | All read and write operations: query, retrieve, modify, do_action, check_consistency |
| **Transaction Manager** | `/BOBF/IF_TRA_TRANSACTION_MGR` | `/BOBF/CL_TRA_TRANS_MGR_FACTORY` | Controls the LUW: save, cleanup (rollback) |
| **Configuration** | `/BOBF/IF_FRW_CONFIGURATION` | `/BOBF/CL_FRW_FACTORY` | Runtime BO metadata. Rarely needed in consumer code. |

**Key rule:** The Transaction Manager is a singleton per ABAP session — there is only one. The Service Manager is BO-specific — you get a different instance per BO key. In the same session, different service managers for different BOs all commit through the same transaction manager.

```abap
" Transaction manager — singleton, no BO key needed
DATA(lo_tmgr) = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

" Service manager — one per BO
DATA(lo_svc)  = /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
    /bobf/if_epm_sales_order_c=>sc_bo_key ).
```

---

## The Constants Interface

Every BOPF BO generates a constants interface. Pattern: `<namespace>IF_<BO_NAME>_C`

Examples:
- `/BOBF/EPM_SALES_ORDER` → `/BOBF/IF_EPM_SALES_ORDER_C`
- `/SCMTMS/TRQ` → `/SCMTMS/IF_TRQ_C`
- `/SCMTMS/TOR` → `/SCMTMS/IF_TOR_C`

**Never hardcode GUIDs or string literals.** Always use the constants interface.

```abap
" BO identity
sc_bo_key                                        " TYPE /BOBF/OBTYPE_KK

" Node keys
sc_node-root                                     " TYPE /BOBF/OBNODE_KK
sc_node-item
sc_node-contact

" Node attribute names (strings, used in query selection parameters)
sc_node_attribute-root-buyer_name                " TYPE /BOBF/ATTRIBUTE_KK
sc_node_attribute-root-gross_amount

" Query keys
sc_query-root-select_all                         " TYPE /BOBF/QUERY_KK
sc_query-root-select_by_elements

" Alternative key constants
sc_alternative_key-root-sales_order_id           " TYPE /BOBF/ALTKEY_KK

" Association constants
sc_association-root-item                         " TYPE /BOBF/ASSOC_KK
sc_association-item-contact

" Action constants
sc_action-root-mark_as_paid                      " TYPE /BOBF/ACTION_KK
sc_action-root-cancel_order
```

Open the constants interface directly in SE24 — it documents the entire BO in one place.

---

## Exploring an Unknown BO

### `/BOBF/CONF_UI` — Model Browser

Your starting point before writing any code. Shows the complete BO metadata.

**What to look at on each node:**

| Tab | What to capture |
|-----|----------------|
| **General** | Node structure type (DDIC structure → derive table type for `et_data`) |
| **Queries** | Query constant names (`sc_query-<node>-select_by_elements`, custom queries) |
| **Alternative Keys** | Key structure type, constant name → needed for `convert_altern_key()` |
| **Associations** | Association constant names, target node, cardinality |
| **Actions** | Action names, parameter structures, implementation class |
| **Determinations** | Determination names, trigger timing (After Modify / Finalize), implementation class |
| **Validations** | Validation names, type (Action / Consistency), implementation class |
| **Generated Objects** | Lists all generated artifacts including the constants interface name |

**Finding a BO when you don't know the name:**

- Module prefix wildcards: `/SCMTMS/*`, `/EHS/*`, `/APO/*`
- Known table: open in SE11 → check if a key field is `DB_KEY TYPE /BOBF/BOPF_KEY` → confirms BOPF management → search CONF_UI for BOs whose root table matches
- Browse all BOs: transaction `BOBX` lists everything in the system

---

### `/BOBF/TEST_UI` — Interactive Data Browser

Load, browse, and navigate real BO instances without writing code.

> **Launch with `/N/BOBF/TEST_UI`** — the `/N` prefix is mandatory.

**Workflow:**

```
1.  /N/BOBF/TEST_UI
2.  Enter BO name → lands on ROOT node, no data loaded yet
3.  Click Folder/Load icon → choose load method:

    ┌────────────────────────────────────────────────────────────┐
    │  By QUERY       Filter by field values (use most often)   │
    │  By KEY         If you have the raw hex GUID              │
    │  By ASSOCIATION Navigate from a parent you already loaded │
    └────────────────────────────────────────────────────────────┘

4.  By QUERY → select SELECT_BY_ELEMENTS → fill filter → Execute
5.  Right panel shows all field values for each returned instance
6.  To navigate children:
    → click child node in left tree
    → Folder icon → By ASSOCIATION
    → Child instances load for the selected parent

7.  Toolbar buttons let you:
    → Execute actions on a loaded instance
    → Trigger individual determinations and validations
    → Edit and save (DEV/QA only — do NOT use in Production)
```

**What TEST_UI tells you before you write code:**
- Exactly which node stores the field you care about
- The exact field name as it appears in the DDIC structure
- Which query to use and what filter parameters it accepts
- Which association connects parent to child
- What actions are available and what they do to a live instance

> ⚠️ Changes made and saved in TEST_UI write to the real database. In Production, use it read-only only.

---

## READ Operations

### Pattern 1: Query → Retrieve

Standard read. Use when filtering by field values but you don't have a GUID.

```abap
" ─── Setup ───────────────────────────────────────────────────────────────────
DATA(lo_svc) = /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
    /bobf/if_epm_sales_order_c=>sc_bo_key ).

DATA lt_sel  TYPE /bobf/t_frw_query_selparam.
DATA lt_key  TYPE /bobf/t_frw_key.
DATA lt_root TYPE /bobf/t_epm_so_root.    " ← concrete table type for this node
DATA lo_msg  TYPE REF TO /bobf/if_frw_message.

" ─── Step 1: Build selection parameters ─────────────────────────────────────
" Each entry mirrors a SELECT-OPTIONS row
APPEND VALUE #(
    attribute_name = /bobf/if_epm_sales_order_c=>sc_node_attribute-root-buyer_name
    sign           = 'I'
    option         = 'EQ'
    low            = 'ALFKI'
) TO lt_sel.

" Multiple criteria: AND logic within the same attribute, OR across instances
APPEND VALUE #(
    attribute_name = /bobf/if_epm_sales_order_c=>sc_node_attribute-root-currency_code
    sign           = 'I'
    option         = 'EQ'
    low            = 'EUR'
) TO lt_sel.

" ─── Step 2: Query — returns GUID key table only ─────────────────────────────
lo_svc->query(
    EXPORTING
        iv_query_key            = /bobf/if_epm_sales_order_c=>sc_query-root-select_by_elements
        it_selection_parameters = lt_sel
    IMPORTING
        et_key                  = lt_key
        eo_message              = lo_msg
).

" ─── Step 3: Check ───────────────────────────────────────────────────────────
IF lo_msg->check( ) = abap_true OR lt_key IS INITIAL.
    " check() returns abap_true if ERROR-level messages exist
    RETURN.
ENDIF.

" ─── Step 4: Retrieve actual data using the GUID keys ───────────────────────
lo_svc->retrieve(
    EXPORTING
        iv_node_key = /bobf/if_epm_sales_order_c=>sc_node-root
        it_key      = lt_key
    IMPORTING
        et_data     = lt_root
        eo_message  = lo_msg
).

" ─── Step 5: Use the data ────────────────────────────────────────────────────
LOOP AT lt_root ASSIGNING FIELD-SYMBOL(<ls_order>).
    WRITE: / <ls_order>-so_id, <ls_order>-buyer_name, <ls_order>-gross_amount.
ENDLOOP.
```

**Built-in query types available on every node:**

| Query Key | Behaviour |
|-----------|-----------|
| `sc_query-<node>-select_all` | Returns all instances. No filter. |
| `sc_query-<node>-select_by_elements` | Filters by attribute values via `it_selection_parameters`. |
| Custom queries | BO-specific, visible in `/BOBF/CONF_UI` → Queries tab. May have own filter structure. |

---

### Pattern 2: Convert Alternative Key → Retrieve

Use when starting from a business key (order number, document ID, material) rather than a GUID. This is the most common real-world starting point.

```abap
DATA(lo_svc)    = /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
    /bobf/if_epm_sales_order_c=>sc_bo_key ).

DATA lt_alt_key TYPE /bobf/t_epm_k_sales_order_id.  " ← alt key table type from SE11
DATA lt_key     TYPE /bobf/t_frw_key.
DATA lt_root    TYPE /bobf/t_epm_so_root.
DATA lo_msg     TYPE REF TO /bobf/if_frw_message.

" Populate the alt key table — one entry per business document you want to find
APPEND VALUE #( so_id = 'SO_000000001' ) TO lt_alt_key.

lo_svc->convert_altern_key(
    EXPORTING
        iv_node_key        = /bobf/if_epm_sales_order_c=>sc_node-root
        iv_altkey_key      = /bobf/if_epm_sales_order_c=>sc_alternative_key-root-sales_order_id
        it_key             = lt_alt_key
        iv_check_existence = abap_true    " returns empty lt_key if not found
    IMPORTING
        et_key             = lt_key
        eo_message         = lo_msg
).

IF lt_key IS INITIAL.
    " Document not found — handle gracefully
    RETURN.
ENDIF.

lo_svc->retrieve(
    EXPORTING
        iv_node_key = /bobf/if_epm_sales_order_c=>sc_node-root
        it_key      = lt_key
    IMPORTING
        et_data     = lt_root
        eo_message  = lo_msg
).
```

**Finding the alt key table type:** `/BOBF/CONF_UI` → node → Alternative Keys tab → note the key structure name → in SE11 the table type has a `T_` prefix in place of `S_`.

---

### Pattern 3: Retrieve by Association

Navigate from loaded parent instances to their children. Always prefer this over a separate child query — it uses the buffer and is mass-enabled.

```abap
" Assumes lt_root_keys already populated from query or convert_altern_key
DATA lt_items    TYPE /bobf/t_epm_so_item.
DATA lt_key_link TYPE /bobf/t_frw_key_link.
DATA lo_msg      TYPE REF TO /bobf/if_frw_message.

lo_svc->retrieve_by_association(
    EXPORTING
        iv_node_key    = /bobf/if_epm_sales_order_c=>sc_node-root
        it_key         = lt_root_keys         " ALL root keys — not one at a time
        iv_association = /bobf/if_epm_sales_order_c=>sc_association-root-item
        iv_fill_data   = abap_true            " populate et_data directly
    IMPORTING
        et_data        = lt_items
        et_key_link    = lt_key_link          " maps root key → child key
        eo_message     = lo_msg
).

" Use et_key_link to correlate items back to their root
" lt_key_link-source_key = parent ROOT instance DB_KEY
" lt_key_link-key        = child ITEM instance DB_KEY
LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ls_item>).
    READ TABLE lt_key_link WITH KEY key = <ls_item>-key
        ASSIGNING FIELD-SYMBOL(<ls_link>).
    " <ls_link>-source_key is the parent's DB_KEY
ENDLOOP.
```

**`iv_fill_data` flag:**

| Value | Behaviour |
|-------|-----------|
| `abap_true` | Populates `et_data` with full node data. Use for most reads. |
| `abap_false` | Returns only `et_key_link`. Use to filter before deciding to retrieve data. |

**The typing problem with `et_data`:** Both `retrieve()` and `retrieve_by_association()` accept `ANY TABLE`. The framework does not type-check at compile time. If you pass the wrong table type you get a runtime dump or silent garbage. Always verify the correct table type in SE11 before coding.

To find the table type: `/BOBF/CONF_UI` → node → General tab → note "Node Structure" (e.g., `/BOBF/S_EPM_SO_ROOT`) → in SE11 the table type is the same name with `S_` replaced by `T_` (e.g., `/BOBF/T_EPM_SO_ROOT`).

---

## WRITE Operations and the LUW Lifecycle

### The Interaction Phase vs. Save Phase

BOPF separates a transaction into two phases:

```
INTERACTION PHASE
─────────────────
Consumer calls modify() / do_action()
  │
  └─► Framework updates in-memory buffer
  └─► After-Modify Determinations fire automatically
  └─► Action Validations fire automatically
  └─► Changes accumulate — nothing written to DB yet

      (repeat as many modify/action calls as needed)

SAVE PHASE (triggered by lo_tmgr->save())
──────────────────────────────────────────
1. Framework calls SAVE_<node> framework actions
2. Finalize Determinations fire
3. Consistency Validations fire
4. If no validation blocks save:
   └─► Data written from buffer → Database (COMMIT WORK)
5. If any validation blocks save:
   └─► Save is REJECTED for ALL participating BOs
   └─► Consumer must call cleanup() to discard
```

**Critical:** One failed validation at save blocks the entire LUW — not just the failing BO. All BOs that participated in the transaction are rolled back together.

---

### The Modification Structure

All write operations (create, update, delete) go through a single `modify()` call that takes a table of `modifications`. Each row in the table is one change instruction.

```abap
" The modification table type
DATA lt_mod TYPE /bobf/t_frw_modification.

" One row in the table — /BOBF/S_FRW_MODIFICATION
" Key fields:
"   node         TYPE /BOBF/OBNODE_KK   — which node
"   change_mode  TYPE /BOBF/CHANGE_M    — CREATE / UPDATE / DELETE
"   key          TYPE /BOBF/BOPF_KEY    — GUID of the instance
"   data         TYPE REF TO data       — REF to the data structure (CREATE/UPDATE)
"   changed_fields TYPE /BOBF/T_FRWFIELDNAME — for UPDATE: list of fields changed (optional but important)
"   source_node  TYPE /BOBF/OBNODE_KK   — parent node (for CREATE of child)
"   source_key   TYPE /BOBF/BOPF_KEY    — parent GUID (for CREATE of child)
"   association  TYPE /BOBF/ASSOC_KK    — association used to attach child (for CREATE)
"   root_key     TYPE /BOBF/BOPF_KEY    — root instance GUID (for CREATE of child)

" Change mode constants in /BOBF/IF_FRW_C:
"   sc_modify_create  — create a new instance
"   sc_modify_update  — update an existing instance
"   sc_modify_delete  — delete an existing instance
```

---

### Pattern 4: Create a Root Node Instance

```abap
DATA(lo_tmgr) = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).
DATA(lo_svc)  = /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
    /bobf/if_epm_sales_order_c=>sc_bo_key ).

DATA lt_mod   TYPE /bobf/t_frw_modification.
DATA lo_chg   TYPE REF TO /bobf/if_tra_change.
DATA lo_msg   TYPE REF TO /bobf/if_frw_message.

" ─── Step 1: Generate a new GUID key ─────────────────────────────────────────
DATA(lv_new_key) = /bobf/cl_frw_factory=>get_new_key( ).

" ─── Step 2: Populate the node data structure ────────────────────────────────
DATA(ls_new_order) = VALUE /bobf/s_epm_so_root(
    key          = lv_new_key
    so_id        = 'SO_999999999'
    buyer_id     = 'C000000001'
    currency_code = 'USD'
    lifecycle_status = 'N'
).

" ─── Step 3: Build the modification entry ─────────────────────────────────────
APPEND VALUE #(
    node        = /bobf/if_epm_sales_order_c=>sc_node-root
    change_mode = /bobf/if_frw_c=>sc_modify_create
    key         = lv_new_key
    data        = REF #( ls_new_order )
) TO lt_mod.

" ─── Step 4: Submit modifications to buffer ──────────────────────────────────
lo_svc->modify(
    EXPORTING it_modification = lt_mod
    IMPORTING eo_change       = lo_chg
              eo_message      = lo_msg
).

IF lo_msg->check( ) = abap_true.
    " Error during modification — check messages
    lo_tmgr->cleanup( ).
    RETURN.
ENDIF.

" ─── Step 5: Save to database ────────────────────────────────────────────────
lo_tmgr->save(
    IMPORTING
        ev_rejected          = DATA(lv_rejected)
        et_rejecting_bo_key  = DATA(lt_rejecting_keys)
        eo_message           = lo_msg
).

IF lv_rejected = abap_true.
    " A validation blocked the save
    lo_tmgr->cleanup( ).
    " Handle: report error messages
ENDIF.
```

---

### Pattern 5: Create Root + Child in One Transaction

Build an order header and add an item in a single `modify()` call.

```abap
DATA(lo_tmgr)    = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).
DATA(lo_svc)     = /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
    /bobf/if_epm_sales_order_c=>sc_bo_key ).
DATA lt_mod      TYPE /bobf/t_frw_modification.

" ─── Root instance ───────────────────────────────────────────────────────────
DATA(lv_root_key) = /bobf/cl_frw_factory=>get_new_key( ).
DATA(ls_root)     = VALUE /bobf/s_epm_so_root(
    key           = lv_root_key
    so_id         = 'SO_888888888'
    buyer_id      = 'C000000002'
    currency_code = 'EUR'
).
APPEND VALUE #(
    node        = /bobf/if_epm_sales_order_c=>sc_node-root
    change_mode = /bobf/if_frw_c=>sc_modify_create
    key         = lv_root_key
    data        = REF #( ls_root )
) TO lt_mod.

" ─── Child item instance ─────────────────────────────────────────────────────
DATA(lv_item_key) = /bobf/cl_frw_factory=>get_new_key( ).
DATA(ls_item)     = VALUE /bobf/s_epm_so_item(
    key         = lv_item_key
    so_item_pos = '0000000010'
    product_id  = 'HT-1000'
    quantity    = 2
    unit        = 'EA'
).
APPEND VALUE #(
    node        = /bobf/if_epm_sales_order_c=>sc_node-item
    change_mode = /bobf/if_frw_c=>sc_modify_create
    key         = lv_item_key
    data        = REF #( ls_item )
    " These three fields link the item to its parent root:
    source_node = /bobf/if_epm_sales_order_c=>sc_node-root
    source_key  = lv_root_key
    root_key    = lv_root_key
    association = /bobf/if_epm_sales_order_c=>sc_association-root-item
) TO lt_mod.

" ─── Submit and save in one call ─────────────────────────────────────────────
lo_svc->modify(
    EXPORTING it_modification = lt_mod
    IMPORTING eo_message      = DATA(lo_msg)
).

IF lo_msg->check( ) = abap_true.
    lo_tmgr->cleanup( ). RETURN.
ENDIF.

lo_tmgr->save(
    IMPORTING ev_rejected = DATA(lv_rejected)
              eo_message  = lo_msg
).
IF lv_rejected = abap_true.
    lo_tmgr->cleanup( ).
ENDIF.
```

---

### Pattern 6: Update a Node Instance

You must first retrieve the instance to get the current key and data, then modify only the fields you want to change.

```abap
DATA(lo_tmgr) = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).
DATA(lo_svc)  = /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
    /bobf/if_epm_sales_order_c=>sc_bo_key ).
DATA lt_mod   TYPE /bobf/t_frw_modification.

" ─── Step 1: Retrieve current data (to get the GUID key) ─────────────────────
" (using convert_altern_key or query first — see READ patterns above)
" Assume lt_root is already populated with the instance to update

READ TABLE lt_root INDEX 1 ASSIGNING FIELD-SYMBOL(<ls_order>).
CHECK sy-subrc = 0.

" ─── Step 2: Modify the local copy ───────────────────────────────────────────
<ls_order>-note = 'Updated by Z-program'.
<ls_order>-billing_status = 'P'.

" ─── Step 3: Build update modification ───────────────────────────────────────
APPEND VALUE #(
    node        = /bobf/if_epm_sales_order_c=>sc_node-root
    change_mode = /bobf/if_frw_c=>sc_modify_update
    key         = <ls_order>-key
    data        = REF #( <ls_order> )
    " Optional but recommended: declare which fields actually changed
    " This helps the framework trigger only relevant determinations
    changed_fields = VALUE #(
        ( /bobf/if_epm_sales_order_c=>sc_node_attribute-root-note )
        ( /bobf/if_epm_sales_order_c=>sc_node_attribute-root-billing_status )
    )
) TO lt_mod.

" ─── Step 4: Modify and save ──────────────────────────────────────────────────
lo_svc->modify(
    EXPORTING it_modification = lt_mod
    IMPORTING eo_message      = DATA(lo_msg)
).

IF lo_msg->check( ) = abap_true.
    lo_tmgr->cleanup( ). RETURN.
ENDIF.

lo_tmgr->save(
    IMPORTING ev_rejected = DATA(lv_rejected)
              eo_message  = lo_msg
).
IF lv_rejected = abap_true.
    lo_tmgr->cleanup( ).
ENDIF.
```

**Why `changed_fields` matters:** When you don't specify it, the framework assumes all fields changed and fires every determination that is configured to trigger on any field of that node. Declaring the specific changed fields lets the framework run only the determinations that actually depend on those fields — significant performance difference in complex BOs.

---

### Pattern 7: Delete a Node Instance

```abap
" Retrieve current data first to get the key
" Assume ls_order is the instance to delete

APPEND VALUE #(
    node        = /bobf/if_epm_sales_order_c=>sc_node-root
    change_mode = /bobf/if_frw_c=>sc_modify_delete
    key         = ls_order-key
) TO lt_mod.

lo_svc->modify(
    EXPORTING it_modification = lt_mod
    IMPORTING eo_message      = DATA(lo_msg)
).

IF lo_msg->check( ) = abap_true.
    lo_tmgr->cleanup( ). RETURN.
ENDIF.

lo_tmgr->save(
    IMPORTING ev_rejected = DATA(lv_rejected)
              eo_message  = lo_msg
).
IF lv_rejected = abap_true.
    lo_tmgr->cleanup( ).
ENDIF.
```

Deleting a root instance **cascades automatically** to all child node instances for that root. The framework handles this via the configured associations — you do not need to explicitly delete child nodes first.

---

### Saving and Rollback

```abap
lo_tmgr->save(
    IMPORTING
        ev_rejected          = lv_rejected          " abap_true if save was blocked
        et_rejecting_bo_key  = lt_rejecting_keys     " which BO keys caused rejection
        eo_message           = lo_msg               " error messages
        eo_change            = lo_change            " what actually changed
).

IF lv_rejected = abap_true.
    " Save was blocked by a validation
    " et_rejecting_bo_key tells you which BO (if multiple BOs in the LUW) blocked it
    lo_tmgr->cleanup( ).   " rolls back ALL changes since last save
    " Report error to user using lo_msg
ENDIF.
```

**Save transaction patterns (via `iv_transaction_pattern`):**

| Constant | Behaviour |
|----------|-----------|
| `GC_TP_SAVE_AND_CONTINUE` (default) | Saves, creates a new transaction automatically. Normal usage. |
| `GC_TP_SAVE_AND_EXIT` | Saves and ends the BOPF session. Use when you're completely done. |

**`cleanup()` vs. not calling it:**

`cleanup()` discards all pending changes in the buffer since the last save. If you don't call it after a rejected save, the failed changes remain in the buffer and will be retried on the next `save()` call — usually not what you want. Always call `cleanup()` when handling a rejected save or an exception.

---

## Executing Actions

### What Actions Are

Actions are **explicitly-invoked, externally-visible operations** on a BO node. They are the API equivalent of a button in a Fiori app. Unlike determinations (which fire automatically), actions only run when a consumer explicitly calls `do_action()`.

Actions have:
- An implementation class implementing `/BOBF/IF_FRW_ACTION`
- An optional parameter structure (for input data from the caller)
- A cardinality: single instance, multiple instances, or no instance
- Optionally: action validations that check preconditions before the action runs

Standard examples: `MARK_AS_PAID`, `CANCEL_ORDER`, `SEND_TO_CARRIER` (`/SCMTMS/TOR` SEND_TOR action).

---

### Pattern 8: Execute an Action (Consumer)

```abap
DATA(lo_tmgr) = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).
DATA(lo_svc)  = /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
    /bobf/if_epm_sales_order_c=>sc_bo_key ).

" ─── Optional: build input parameters for the action ─────────────────────────
" Not all actions have parameters. Check in /BOBF/CONF_UI → Actions → parameter structure.
" If a parameter structure exists, instantiate and fill it:
DATA(ls_params) = VALUE /bobf/s_epm_a_mark_as_paid_p(
    payment_method = 'CC'
).

" ─── Execute the action on one or more instances ─────────────────────────────
lo_svc->do_action(
    EXPORTING
        iv_act_key    = /bobf/if_epm_sales_order_c=>sc_action-root-mark_as_paid
        it_key        = lt_root_keys           " keys of instances to act on (mass-enabled)
        is_parameters = REF #( ls_params )     " omit if action has no parameters
    IMPORTING
        et_failed_key = DATA(lt_failed_keys)   " instances for which action failed
        eo_message    = DATA(lo_msg)
        eo_change     = DATA(lo_change)
).

" ─── Check which instances succeeded vs. failed ───────────────────────────────
" et_failed_key contains keys of instances where the action was rejected
IF lt_failed_keys IS NOT INITIAL.
    " Some instances failed — log messages, continue with successful ones or abort
    lo_tmgr->cleanup( ).
    RETURN.
ENDIF.

" Action ran in the buffer — still need to save
lo_tmgr->save(
    IMPORTING ev_rejected = DATA(lv_rejected)
              eo_message  = lo_msg
).
IF lv_rejected = abap_true.
    lo_tmgr->cleanup( ).
ENDIF.
```

**Important distinction:** `do_action()` runs the action logic in the buffer (Interaction Phase). Nothing is written to the database until `save()` is called. An action may internally call `io_modify->create/update/delete` to change node data as part of its logic, and those changes also stay in the buffer until save.

**Real example — TM Freight Order SEND_TOR action:**

```abap
DATA(lo_svc) = /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
    /scmtms/if_tor_c=>sc_bo_key ).

lo_svc->do_action(
    EXPORTING
        iv_act_key    = /scmtms/if_tor_c=>sc_action-root-send_tor
        it_key        = lt_tor_keys
    IMPORTING
        et_failed_key = DATA(lt_failed)
        eo_message    = DATA(lo_msg)
).
```

---

### Inside an Action Implementation

When you find an existing action and need to understand what it does, open its implementation class in SE24 (visible from `/BOBF/CONF_UI` → Actions tab → class name). Every action class implements:

```abap
METHOD /bobf/if_frw_action~execute.
" Interface parameters available inside every action:
"
"   it_key          — keys of node instances the action is executing on (mass)
"   is_parameters   — REF TO the action's input parameter structure (if any)
"   io_read         — /BOBF/IF_FRW_READ    — read data from buffer (not DB)
"   io_modify       — /BOBF/IF_FRW_MODIFY  — modify data in buffer
"   et_failed_key   — output: which instances failed
"   eo_message      — output: messages
"   is_ctx          — context info (rarely used)

  " Reading data within action — use io_read, not the service manager
  DATA lt_root_data TYPE /bobf/t_epm_so_root.

  io_read->retrieve(
      EXPORTING
          iv_node    = /bobf/if_epm_sales_order_c=>sc_node-root
          it_key     = it_key
      IMPORTING
          et_data    = lt_root_data
  ).

  " Modifying data within action — use io_modify, not the service manager
  LOOP AT lt_root_data ASSIGNING FIELD-SYMBOL(<ls_order>).
      <ls_order>-payment_status = 'P'.
      io_modify->update(
          EXPORTING
              iv_node          = /bobf/if_epm_sales_order_c=>sc_node-root
              iv_key           = <ls_order>-key
              is_data          = REF #( <ls_order> )
              it_changed_fields = VALUE #(
                  ( /bobf/if_epm_sales_order_c=>sc_node_attribute-root-payment_status )
              )
      ).
  ENDLOOP.

ENDMETHOD.
```

**`io_read` vs. Service Manager inside action/determination/validation:**
- `io_read` reads from the **in-memory buffer** (includes pending modifications not yet saved)
- Service Manager `retrieve()` also hits the buffer for the current LUW, but carries extra overhead
- Always use `io_read` and `io_modify` when you're coding inside a behavior class (action, determination, validation)

---

## Understanding Determinations

### What They Are and When They Fire

Determinations are **automatic side-effects** — they fire without being called explicitly by the consumer. The framework triggers them based on configured conditions (which fields changed, which operation type).

Two trigger timings:

| Timing | When it fires | Typical use |
|--------|---------------|-------------|
| **After Modify** | Immediately after a `modify()` call changes the configured trigger fields | Calculate derived fields (e.g., item total after quantity change), populate transient display fields |
| **Finalize (Before Save)** | During the save phase, before consistency validations | Final aggregation, setting status fields, cross-node calculations that need the full picture |

**After Modify determinations can trigger other determinations** — if determination A updates field X, and determination B is triggered by changes to field X, then B fires automatically after A. The framework analyzes these **determination dependencies** and ensures correct execution order.

---

### Finding Determinations on an Existing BO

In `/BOBF/CONF_UI` → click a node → Determinations tab:

| Column | What to look for |
|--------|-----------------|
| Name | Determination identifier (constant in the constants interface) |
| Timing | After Modify or Finalize |
| Trigger Fields | Which node attributes trigger this determination |
| Implementation Class | The ABAP class — open in SE24 to read the logic |
| Dependencies | Which determinations must run before or after this one |

If you see unexpected field values appearing after a `modify()` call, a determination is computing them. Finding the determination that writes a field is often critical for debugging.

---

### Inside a Determination Implementation

```abap
METHOD /bobf/if_frw_determination~execute.
" Interface parameters:
"   it_modification   — what was just modified (keys + which fields changed)
"   io_read           — read from buffer
"   io_modify         — write back to buffer
"   et_failed_key     — output: instances where determination failed
"   eo_message        — output: messages

  " Determine which instances are affected
  DATA lt_changed_keys TYPE /bobf/t_frw_key.
  LOOP AT it_modification INTO DATA(ls_mod).
      APPEND VALUE #( key = ls_mod-key ) TO lt_changed_keys.
  ENDLOOP.

  " Read the data that triggered this determination
  DATA lt_items TYPE /bobf/t_epm_so_item.
  io_read->retrieve(
      EXPORTING
          iv_node = /bobf/if_epm_sales_order_c=>sc_node-item
          it_key  = lt_changed_keys
      IMPORTING
          et_data = lt_items
  ).

  " Calculate derived value — e.g., extend to net amount
  " Then navigate up to root to update the total
  DATA lt_root_link TYPE /bobf/t_frw_key_link.
  io_read->retrieve_by_association(
      EXPORTING
          iv_node        = /bobf/if_epm_sales_order_c=>sc_node-item
          it_key         = lt_changed_keys
          iv_association = /bobf/if_epm_sales_order_c=>sc_association-item-root   " navigate UP
          iv_fill_data   = abap_false
      IMPORTING
          et_key_link    = lt_root_link
  ).

  " ... aggregate and write back to root via io_modify->update()

ENDMETHOD.
```

---

## Understanding Validations

### Types of Validations

| Type | When it fires | Effect of failure |
|------|---------------|-------------------|
| **Action Validation** | Before a specific action or modify operation executes | The action/modify is **rejected** — not executed at all. `et_failed_key` is populated. |
| **Consistency Validation** | During the save phase | Save is **rejected** for the entire LUW. |

Both types implement the same interface (`/BOBF/IF_FRW_VALIDATION`) and appear under the Validations tab in `/BOBF/CONF_UI`.

**Consistency validations** run automatically at every save attempt. You cannot skip them.

**Action validations** run before a specific action or before any `modify()` on the node. A failed action validation means the operation was never applied to the buffer — no cleanup needed on your side.

---

### Triggering a Validation Manually

You can explicitly trigger consistency validations without saving — useful for "Check" buttons in UI scenarios.

```abap
lo_svc->check_consistency(
    EXPORTING
        it_node_key = VALUE #(
            ( /bobf/if_epm_sales_order_c=>sc_node-root )
        )
        it_key      = lt_root_keys
    IMPORTING
        eo_message  = DATA(lo_msg)
).

" Check if any errors were raised
IF lo_msg->check( ) = abap_true.
    " Validation found errors — report to user, don't save yet
ENDIF.
```

---

### Inside a Validation Implementation

```abap
METHOD /bobf/if_frw_validation~validate.
" Interface parameters:
"   it_key          — instances to validate (mass-enabled — never READ TABLE INDEX 1)
"   io_read         — read from buffer
"   et_failed_key   — output: instances that failed validation
"   eo_message      — output: error messages

  DATA lt_root TYPE /bobf/t_epm_so_root.
  io_read->retrieve(
      EXPORTING
          iv_node = /bobf/if_epm_sales_order_c=>sc_node-root
          it_key  = it_key
      IMPORTING
          et_data = lt_root
  ).

  LOOP AT lt_root ASSIGNING FIELD-SYMBOL(<ls_order>).
      " Check: begin date must be before end date
      IF <ls_order>-delivery_date < <ls_order>-created_at.
          APPEND VALUE #( key = <ls_order>-key ) TO et_failed_key.
          eo_message->add_message(
              EXPORTING
                  is_msg = VALUE #(
                      msgty = 'E'
                      msgid = 'Z_MY_MSGS'
                      msgno = '001'
                      msgv1 = <ls_order>-so_id
                  )
          ).
      ENDIF.
  ENDLOOP.

ENDMETHOD.
```

---

## Tracing a Complete BO Process End-to-End

When you encounter a BOPF-based Fiori app and need to understand what happens behind a user action, here is the systematic investigation path.

**Scenario:** User opens a Sales Order in Fiori, changes the quantity on an item, and clicks Save. A "Gross Amount" field updates automatically.

### Step 1 — Identify the BO

Identify the BO key from one of:
- `/BOBF/CONF_UI` wildcard search for likely namespace
- SE16 on the suspected database table → verify `DB_KEY TYPE /BOBF/BOPF_KEY` field exists
- `BOBX` transaction to browse all ~500 BOs in the system

### Step 2 — Map the Process in TEST_UI

1. Launch `/N/BOBF/TEST_UI`, enter the BO name
2. Load a real order instance using `SELECT_BY_ELEMENTS` with the order number
3. Navigate to the ITEM node via Association
4. Note the current `GROSS_AMOUNT` on the root
5. In TEST_UI, edit the item quantity → observe: does GROSS_AMOUNT change immediately?
   - If yes → an **After Modify determination** is computing it
   - If it changes only after clicking Save → a **Finalize determination** is computing it

### Step 3 — Find the Determination

In `/BOBF/CONF_UI` → ITEM node → Determinations tab:
- Look for a determination with trigger fields including `QUANTITY`
- Note its timing (After Modify = fires immediately; Finalize = fires at save)
- Open its implementation class in SE24

### Step 4 — Read the Implementation Class

In SE24, open the determination class → method `EXECUTE`:
- See which fields it reads (`io_read->retrieve` or `io_read->retrieve_by_association`)
- See which fields it writes (`io_modify->update` with `it_changed_fields`)
- Trace any further association navigations (e.g., rolling up item totals to root)

### Step 5 — Trace Determination Dependencies

Back in `/BOBF/CONF_UI` → Determinations tab → check "Dependencies" column:
- If the item-level determination writes `NET_AMOUNT`, check whether there is a root-level determination triggered by changes to `NET_AMOUNT` (e.g., one that aggregates all items to `GROSS_AMOUNT`)
- The chain: qty change → DET_ITEM_AMOUNT → NET_AMOUNT updated → DET_ROOT_TOTAL → GROSS_AMOUNT updated

### Step 6 — Find the Validation at Save

In `/BOBF/CONF_UI` → ROOT node → Validations tab → identify consistency validations:
- Open each implementation class → `validate` method
- Note what fields are checked and what errors are raised
- This tells you why certain saves fail with cryptic error messages

### Step 7 — Identify the Action Behind a Button

If a Fiori toolbar button triggers something:
- In `/BOBF/CONF_UI` → node → Actions tab
- Find the action whose name matches the button's behavior
- Open its implementation class → `execute` method
- Trace: what does it read, what does it modify, what child nodes does it create/delete

### Step 8 — Set a Breakpoint in the Implementation Class

To debug the live process:
1. Find the implementation class name from `/BOBF/CONF_UI`
2. Open in SE24 → set a breakpoint in the `EXECUTE` or `VALIDATE` method
3. Trigger the Fiori action that should invoke it
4. The debugger catches it and you can step through with real data in `it_key`, `io_read`, `io_modify`

```
Flow summary for "change item quantity → Save":

User changes QTY in Fiori
  │
  ├─ OData PATCH → BOPF service manager modify()
  │   └─► Buffer updated: ITEM.QTY changed
  │   └─► After-Modify fires: DET_CALC_ITEM_AMOUNT
  │         reads ITEM.QTY and ITEM.UNIT_PRICE
  │         writes ITEM.NET_AMOUNT
  │         └─► triggers DET_CALC_ROOT_TOTAL (depends on NET_AMOUNT change)
  │               reads all ITEM.NET_AMOUNT for this ROOT
  │               writes ROOT.GROSS_AMOUNT
  │
User clicks Save
  │
  └─ Transaction Manager save()
      │
      ├─► Finalize determinations run (e.g., DET_SET_STATUS)
      ├─► Consistency validations run (e.g., VAL_CHECK_DATES)
      │     if validation fails → save REJECTED → cleanup()
      │
      └─► If all validations pass → COMMIT WORK → DB updated
```

---

## Error Handling

### The Message Container

Every API call returns `eo_message TYPE REF TO /BOBF/IF_FRW_MESSAGE`. Always check it.

```abap
" Check for blocking errors
IF lo_msg->check( ) = abap_true.
    " ERROR-level messages exist — do not proceed
    lo_tmgr->cleanup( ).
ENDIF.

" Read all messages to report to user or log
DATA lt_messages TYPE /bobf/t_frw_message_k.
lo_msg->get_messages( IMPORTING et_message = lt_messages ).

LOOP AT lt_messages INTO DATA(ls_msg).
    " ls_msg-msgty = 'E'/'W'/'I'/'S'
    " ls_msg-msgid, ls_msg-msgno, ls_msg-msgv1..4 = standard T100 message fields
    MESSAGE ID ls_msg-msgid TYPE ls_msg-msgty NUMBER ls_msg-msgno
        WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
ENDLOOP.
```

### The `et_failed_key` Pattern

`do_action()` and validations return `et_failed_key` containing the GUIDs of instances that failed. This allows **partial success** in mass processing — some instances may succeed while others fail.

```abap
lo_svc->do_action(
    EXPORTING
        iv_act_key    = sc_action-root-mark_as_paid
        it_key        = lt_all_order_keys     " 50 orders passed in
    IMPORTING
        et_failed_key = DATA(lt_failed)       " 3 failed, 47 succeeded
        eo_message    = DATA(lo_msg)
).

" lt_all_order_keys MINUS lt_failed = successfully processed
DATA lt_succeeded TYPE /bobf/t_frw_key.
lt_succeeded = lt_all_order_keys.
DELETE lt_succeeded WHERE key IN ( SELECT key FROM TABLE lt_failed ).
```

### Exception Handling

BOPF API calls can raise `/BOBF/CX_FRW`. Wrap in TRY-CATCH in production code:

```abap
TRY.
    lo_svc->modify(
        EXPORTING it_modification = lt_mod
        IMPORTING eo_message      = lo_msg
    ).
    lo_tmgr->save(
        IMPORTING ev_rejected = lv_rejected
                  eo_message  = lo_msg
    ).
CATCH /bobf/cx_frw INTO DATA(lx_bopf).
    lo_tmgr->cleanup( ).
    " Log or re-raise
    MESSAGE lx_bopf->get_text( ) TYPE 'E'.
ENDTRY.
```

---

## Performance Patterns

### Mass-read everything upfront

The API is designed for mass operations. Never call in a loop with single keys.

```abap
" ❌ Wrong — one retrieve call per order in a loop
LOOP AT lt_orders ASSIGNING FIELD-SYMBOL(<order>).
    DATA(lt_single) = VALUE /bobf/t_frw_key( ( key = <order>-key ) ).
    lo_svc->retrieve_by_association( it_key = lt_single ... ).
ENDLOOP.

" ✅ Correct — pass all keys in one call
lo_svc->retrieve_by_association( it_key = lt_all_root_keys ... ).
```

### Declare `changed_fields` on UPDATE

Without it, the framework assumes all fields changed and fires all determinations. With it, only relevant determinations fire.

```abap
changed_fields = VALUE #(
    ( sc_node_attribute-root-billing_status )
    ( sc_node_attribute-root-note )
)
```

### Use `it_requested_attributes` on RETRIEVE

Limits which fields are populated, reducing buffer memory.

```abap
lo_svc->retrieve(
    EXPORTING
        iv_node_key             = sc_node-root
        it_key                  = lt_key
        it_requested_attributes = VALUE #(
            ( sc_node_attribute-root-so_id      )
            ( sc_node_attribute-root-buyer_name )
        )
    IMPORTING et_data = lt_root
).
```

### The BOPF Buffer

The framework maintains an in-memory buffer per LUW. Repeated reads of the same instances within the same LUW hit the buffer, not the database. This makes multiple `retrieve()` calls for the same keys cheap.

---

## Investigation Checklist

```
□ 1. /BOBF/CONF_UI
      → Enter BO name (use BOBX to browse if unknown)
      → Map node tree: ROOT + child nodes
      → For each relevant node:
           - Note DDIC structure name → derive table type for et_data
           - Note available queries (select_by_elements, custom)
           - Note alternative key structure + constant
           - Note association constants (to children, to parent, cross-BO)
           - Note action names + parameter structures + implementation classes
           - Note determination names + timing + trigger fields + implementation classes
           - Note validation names + type (action/consistency) + implementation classes

□ 2. /N/BOBF/TEST_UI
      → Load a real representative instance via SELECT_BY_ELEMENTS
      → Navigate every node via "By Association"
      → Identify which node holds the field you care about
      → Test: change a value → observe which fields auto-update → identifies determinations
      → Test: execute an action → observe what changes in the buffer
      → Note exact field names as they appear in the data

□ 3. SE24 → Open the constants interface (<namespace>IF_<BO>_C)
      → Note sc_bo_key, sc_node-*, sc_query-*, sc_alternative_key-*,
        sc_association-*, sc_action-*, sc_node_attribute-*

□ 4. SE11 → Open node structures
      → Confirm field names match TEST_UI observations
      → Note table types for et_data (S_ → T_ prefix)
      → For alt key: confirm key structure and table type

□ 5. SE24 → Open action/determination/validation implementation classes
      → Read EXECUTE / VALIDATE methods
      → Trace io_read and io_modify calls to understand data flow

□ 6. Write code
      → get_transaction_manager + get_service_manager
      → convert_altern_key() or query() to get GUIDs
      → retrieve() for node data
      → retrieve_by_association() for child nodes (mass)
      → modify() for create/update/delete
      → do_action() for actions
      → save() → handle lv_rejected → cleanup() if needed
      → Check eo_message->check() and et_failed_key after every relevant call
```

---

## Common Existing BOs

| Module | BO Name | Constants Interface | Notes |
|--------|---------|---------------------|-------|
| **EPM Demo — Sales Order** | `/BOBF/EPM_SALES_ORDER` | `/BOBF/IF_EPM_SALES_ORDER_C` | Best sandbox. Has root/item/contact, alt keys, associations, sample actions including MARK_AS_PAID. Safe to experiment on. |
| **EPM Demo — Sales Invoice** | `/BOBF/EPM_SALES_INVOICE` | `/BOBF/IF_EPM_SOI_CONSTANTS` | Has MARK_AS_PAID action. Good for action testing. |
| **TM Forwarding Order** | `/SCMTMS/TRQ` | `/SCMTMS/IF_TRQ_C` | Complex. Entry point for TM freight forwarding. Many nodes and cross-BO associations. |
| **TM Freight Order / Booking** | `/SCMTMS/TOR` | `/SCMTMS/IF_TOR_C` | Freight execution. SEND_TOR action frequently referenced. |
| **EH&S** | `/EHS/*` prefix | `/EHS/IF_*_C` | Multiple BOs for different EH&S domains. |
| **S/4HANA standard apps** | Various | Check BOBX | Use S4 Fiori Model Analyzer to identify BO behind any Fiori app. |

---

## Key Interfaces and Types Reference

### Factory Classes

```abap
" Get managers
/BOBF/CL_TRA_SERV_MGR_FACTORY=>get_service_manager( iv_bo_key )
/BOBF/CL_TRA_TRANS_MGR_FACTORY=>get_transaction_manager( )

" Generate a new GUID key
/BOBF/CL_FRW_FACTORY=>get_new_key( )    " Returns TYPE /BOBF/BOPF_KEY
```

### Service Manager Methods (`/BOBF/IF_TRA_SERVICE_MANAGER`)

```abap
->query( iv_query_key, it_selection_parameters → et_key, eo_message )
->retrieve( iv_node_key, it_key → et_data, eo_message )
->retrieve_by_association( iv_node_key, it_key, iv_association, iv_fill_data → et_data, et_key_link, eo_message )
->convert_altern_key( iv_node_key, iv_altkey_key, it_key → et_key, eo_message )
->modify( it_modification → eo_change, eo_message )
->do_action( iv_act_key, it_key, is_parameters → et_failed_key, eo_message, eo_change )
->check_consistency( it_node_key, it_key → eo_message )
```

### Transaction Manager Methods (`/BOBF/IF_TRA_TRANSACTION_MGR`)

```abap
->save( iv_transaction_pattern → ev_rejected, et_rejecting_bo_key, eo_message, eo_change )
->cleanup( )    " rollback all pending changes
```

### Behavior Class Interfaces (for action/determination/validation implementations)

```abap
/BOBF/IF_FRW_ACTION~execute( it_key, is_parameters, io_read, io_modify → et_failed_key, eo_message )
/BOBF/IF_FRW_DETERMINATION~execute( it_modification, io_read, io_modify → et_failed_key, eo_message )
/BOBF/IF_FRW_VALIDATION~validate( it_key, io_read → et_failed_key, eo_message )
```

### Common Types

```abap
/BOBF/T_FRW_KEY              " Table of GUIDs — used in all it_key parameters
/BOBF/S_FRW_KEY              " Single key entry: key TYPE /BOBF/BOPF_KEY
/BOBF/T_FRW_QUERY_SELPARAM   " Selection parameters table
/BOBF/S_FRW_QUERY_SELPARAM   " One selection row: attribute_name, sign, option, low, high
/BOBF/T_FRW_MODIFICATION     " Modification table for modify()
/BOBF/S_FRW_MODIFICATION     " One modification: node, change_mode, key, data, changed_fields, source_node, source_key, association, root_key
/BOBF/T_FRW_KEY_LINK         " Parent↔child mapping: source_key, key
/BOBF/BOPF_KEY               " Raw GUID type (= RAW 16)
/BOBF/IF_FRW_MESSAGE         " Message container
/BOBF/IF_FRW_READ            " Read interface used inside behavior classes
/BOBF/IF_FRW_MODIFY          " Modify interface used inside behavior classes
```

### Change Mode Constants (`/BOBF/IF_FRW_C`)

```abap
sc_modify_create   " 'C' — create a new instance
sc_modify_update   " 'U' — update an existing instance
sc_modify_delete   " 'D' — delete an existing instance
```

---

## Transactions Summary

| Transaction | Alias | Purpose |
|-------------|-------|---------|
| `/BOBF/CONF_UI` | — | Model browser. BO structure, nodes, types, behaviors, constants. |
| `/BOBF/TEST_UI` | `BOBT` | Interactive data browser. Load, navigate, trigger actions/determinations on real instances. |
| `BOBX` | — | List all BOs in the system (~500 in S/4HANA). Filter by package or namespace. |
| `/BOBF/CUST_UI` | — | Enhancement workbench. Add custom nodes/behaviors to existing BOs. |

---

*Last updated: 2026-03-29*  
*Status: Classic BOPF is in maintenance mode as of S/4HANA 2021. RAP is recommended for all new development. BOPF knowledge remains essential for enhancements and integrations on existing TM, EH&S, and legacy S/4HANA standard apps.*

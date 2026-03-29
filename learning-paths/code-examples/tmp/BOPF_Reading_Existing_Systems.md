# BOPF — Reading Existing Systems

A practical reference for navigating, exploring, and programmatically consuming existing BOPF Business Objects in SAP S/4HANA and related modules. Focused on brownfield/read scenarios, not building new BOs.

---

## Table of Contents

1. [Naming Clarification](#naming-clarification)
2. [Core Concepts Recap](#core-concepts-recap)
3. [Layer 1 — Explore the Model with Transactions](#layer-1--explore-the-model-with-transactions)
   - [/BOBF/CONF_UI — Model Browser](#bobfconf_ui--model-browser)
   - [/BOBF/TEST_UI — Interactive Data Browser](#bobftest_ui--interactive-data-browser)
4. [Layer 2 — The Programmatic API](#layer-2--the-programmatic-api)
   - [The Three Managers](#the-three-managers)
   - [The Constants Interface](#the-constants-interface)
   - [Pattern 1: Query → Retrieve](#pattern-1-query--retrieve)
   - [Pattern 2: Convert Alternative Key → Retrieve](#pattern-2-convert-alternative-key--retrieve)
   - [Pattern 3: Retrieve by Association](#pattern-3-retrieve-by-association)
   - [The Typing Problem](#the-typing-problem)
5. [Layer 3 — Finding Constants for an Unknown BO](#layer-3--finding-constants-for-an-unknown-bo)
6. [Investigation Checklist](#investigation-checklist)
7. [Common Existing BOs](#common-existing-bos)
8. [Performance Notes](#performance-notes)
9. [Key Interfaces and Classes Reference](#key-interfaces-and-classes-reference)

---

## Naming Clarification

| Name | Meaning |
|------|---------|
| **BOPF** | Business Object Processing Framework — the conceptual/marketing name |
| **BOBF** | Business Object Building Framework — the actual SAP package namespace |

In code, transactions, and type names you will always see **BOBF** (e.g., `/BOBF/CONF_UI`, `/BOBF/IF_FRW_QUERY`, `sc_bo_key`). Both acronyms refer to the same framework.

---

## Core Concepts Recap

A BOPF Business Object (BO) is a **hierarchical tree of nodes**. Each node:

- Maps to a DDIC database table (with `DB_KEY TYPE /BOBF/BOPF_KEY` as primary key — a GUID)
- Has **attributes** (the table columns / fields)
- Has **behaviors**: Actions, Determinations, Validations
- Has **Queries** to find instances
- Has **Alternative Keys** — human-readable identifiers (e.g., Sales Order number)
- Has **Associations** to navigate to child nodes or nodes in other BOs

The framework manages all buffer handling, locking, and transaction lifecycle automatically. As a consumer, you interact exclusively through the API — never directly via SELECT on BOPF-managed tables.

```
BO: /BOBF/EPM_SALES_ORDER
│
├── ROOT node          (header data — maps to /BOBF/D_SO_ROOT)
│   ├── Alt Key: SALES_ORDER_ID
│   ├── Query: SELECT_BY_ELEMENTS
│   └── Association → ITEM
│
└── ITEM node          (line items — maps to /BOBF/D_SO_ITEM)
    └── Association → CONTACT
```

---

## Layer 1 — Explore the Model with Transactions

Always explore before coding. These two transactions replace the need for internal documentation.

---

### `/BOBF/CONF_UI` — Model Browser

**Purpose:** Read-only view of the complete BO metadata model. Use this to map out nodes, find type names, and discover constants.

**What to examine on each node:**

| Tab | What to look for |
|-----|-----------------|
| **General** | Node structure type name (the DDIC structure your `et_data` must match) |
| **Queries** | Query constant names — you need `sc_query-<node>-select_by_elements` etc. |
| **Alternative Keys** | Key structure type, constant name → needed for `convert_altern_key()` |
| **Associations** | Association constant names → needed for `retrieve_by_association()` |
| **Generated Objects** | Lists all generated artifacts including the constants interface name |

**Tip — Finding a BO when you don't know the name:**

- If you know the module: search with wildcards e.g. `/SCMTMS/*`
- If you know a database table: open it in SE11 → check if one of its key fields is `DB_KEY TYPE /BOBF/BOPF_KEY`. If yes, it's BOPF-managed. Search `/BOBF/CONF_UI` for BOs whose root node table matches.
- Browse all ~500 BOs in an S/4HANA system: use transaction `BOBX`

---

### `/BOBF/TEST_UI` — Interactive Data Browser

**Purpose:** Load, browse, and navigate real BO instances without writing any code. Equivalent to a live debugger for data structure discovery.

> **Important:** Always launch with `/N/BOBF/TEST_UI` — the `/N` prefix is required. It will not open from within a running transaction otherwise.

**Step-by-step workflow:**

```
1. /N/BOBF/TEST_UI
2. Enter BO Name (e.g., /BOBF/EPM_SALES_ORDER)
   → Lands on ROOT node view. No data loaded yet.

3. Click the Folder/Load icon → choose load method:
   ┌─────────────────────────────────────────────────────┐
   │ By QUERY      ← USE THIS. Filter by field values.  │
   │               Best when you know a business key     │
   │               (order number, material, etc.)        │
   │                                                     │
   │ By KEY        ← Use if you have the raw GUID.      │
   │               Displayed in hex — rarely convenient  │
   │                                                     │
   │ By ASSOCIATION← Navigate from an already-loaded    │
   │               parent to its children                │
   └─────────────────────────────────────────────────────┘

4. Choose "By QUERY" → select SELECT_BY_ELEMENTS
   → Fill filter fields (e.g., order number, date)
   → Execute

5. Results appear in right panel
   → All field values visible for each instance

6. To navigate to child nodes:
   → Click child node in left tree
   → Click Folder icon → By ASSOCIATION
   → Child instances for the selected root instance load

7. Inspect every field, every node, cross-BO navigation
   → No code required
```

**What this tells you before you write code:**

- Which node holds the field you care about
- The exact field name as it appears in the DDIC structure
- Which query to use
- Which association to follow for child data

> ⚠️ **Production warning:** You can edit and save data from TEST_UI. Do not do this in Production. Use read-only mode — do not click "Test Edit" unless you intend to make changes in a non-production system.

---

## Layer 2 — The Programmatic API

### The Three Managers

All BOPF API interaction goes through these interfaces, obtained from factory classes:

| Manager | Interface | Factory Class | Purpose |
|---------|-----------|---------------|---------|
| **Service Manager** | `/BOBF/IF_TRA_SERVICE_MANAGER` | `/BOBF/CL_TRA_SERV_MGR_FACTORY` | Query, retrieve, retrieve by association. **All read operations.** |
| **Transaction Manager** | `/BOBF/IF_TRA_TRANSACTION_MGR` | `/BOBF/CL_TRA_TRANS_MGR_FACTORY` | Modify, save, rollback. Only needed for write operations. |
| **Configuration** | `/BOBF/IF_FRW_CONFIGURATION` | `/BOBF/CL_FRW_FACTORY` | Read BO metadata. Rarely needed in consumer code. |

**For read-only scenarios, you only need the Service Manager.**

```abap
" Get a service manager — always BO-specific
DATA(lo_svc) = /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
    <bo_constants_interface>=>sc_bo_key ).
```

The service manager instance is lightweight and can be created on demand. There is no need to cache it across calls.

---

### The Constants Interface

Every BOPF BO generates a constants interface named after the pattern:

```
<namespace>IF_<BO_NAME>_C
```

Examples:
- `/BOBF/EPM_SALES_ORDER` → `/BOBF/IF_EPM_SALES_ORDER_C`
- `/SCMTMS/TRQ` → `/SCMTMS/IF_TRQ_C`
- `/SCMTMS/TOR` → `/SCMTMS/IF_TOR_C`

**Never hardcode GUIDs or string literals for node keys, query keys, or association keys.** Always use the constants interface. It is the typed contract for the BO.

**Constants structure:**

```abap
" BO identity
sc_bo_key                                   " TYPE /BOBF/OBTYPE_KK — BO UUID

" Node keys
sc_node-root                                " TYPE /BOBF/OBNODE_KK
sc_node-item                                " (each child node has its own constant)

" Node attribute names (used in query selection parameters)
sc_node_attribute-root-buyer_name           " TYPE /BOBF/ATTRIBUTE_KK (= string)
sc_node_attribute-root-gross_amount

" Query keys
sc_query-root-select_by_elements            " TYPE /BOBF/QUERY_KK
sc_query-root-select_all

" Alternative key constants
sc_alternative_key-root-sales_order_id      " TYPE /BOBF/ALTKEY_KK

" Association constants
sc_association-root-item                    " TYPE /BOBF/ASSOC_KK
sc_association-item-contact
```

Open the constants interface directly in SE24 and browse it — it documents the entire BO shape in one place.

---

### Pattern 1: Query → Retrieve

The standard read pattern. Use when you know field values to filter on but do not have GUIDs.

```abap
" ─── Declarations ───────────────────────────────────────────────────────────
DATA(lo_svc)  = /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
    /bobf/if_epm_sales_order_c=>sc_bo_key ).

DATA lt_sel   TYPE /bobf/t_frw_query_selparam.
DATA lt_key   TYPE /bobf/t_frw_key.
DATA lt_root  TYPE /bobf/t_epm_so_root.        " ← concrete node table type
DATA lo_msg   TYPE REF TO /bobf/if_frw_message.

" ─── Step 1: Build selection parameters ─────────────────────────────────────
" Structure mirrors SELECT-OPTIONS: sign, option, low, high
APPEND VALUE #(
    attribute_name = /bobf/if_epm_sales_order_c=>sc_node_attribute-root-buyer_name
    sign           = 'I'
    option         = 'EQ'
    low            = 'ALFKI'
) TO lt_sel.

" Multiple criteria — just append more entries (AND logic within same attribute)
APPEND VALUE #(
    attribute_name = /bobf/if_epm_sales_order_c=>sc_node_attribute-root-currency_code
    sign           = 'I'
    option         = 'EQ'
    low            = 'EUR'
) TO lt_sel.

" ─── Step 2: Query — returns GUIDs only ─────────────────────────────────────
lo_svc->query(
    EXPORTING
        iv_query_key            = /bobf/if_epm_sales_order_c=>sc_query-root-select_by_elements
        it_selection_parameters = lt_sel
    IMPORTING
        et_key                  = lt_key
        eo_message              = lo_msg
).

" ─── Step 3: Check results ───────────────────────────────────────────────────
IF lo_msg->check( ) = abap_true OR lt_key IS INITIAL.
    " lo_msg->check() returns abap_true if there are ERROR messages
    " Handle: not found, or query failed
    RETURN.
ENDIF.

" ─── Step 4: Retrieve data ───────────────────────────────────────────────────
lo_svc->retrieve(
    EXPORTING
        iv_node_key = /bobf/if_epm_sales_order_c=>sc_node-root
        it_key      = lt_key
    IMPORTING
        et_data     = lt_root          " ← must match node's table type
        eo_message  = lo_msg
).

" ─── Step 5: Consume ─────────────────────────────────────────────────────────
LOOP AT lt_root ASSIGNING FIELD-SYMBOL(<ls_order>).
    WRITE: / <ls_order>-buyer_name, <ls_order>-gross_amount.
ENDLOOP.
```

**Built-in query types:**

| Query Key | Behaviour |
|-----------|-----------|
| `sc_query-<node>-select_all` | Returns all instances of the node. No filter. |
| `sc_query-<node>-select_by_elements` | Filters by attribute values via `it_selection_parameters`. |
| Custom queries | BO-specific queries with their own filter structures. Visible in `/BOBF/CONF_UI`. |

---

### Pattern 2: Convert Alternative Key → Retrieve

Use this when you start from a human-readable business key (order number, material number, document ID) rather than a GUID. This is the most common real-world starting point.

```abap
" ─── Declarations ───────────────────────────────────────────────────────────
DATA(lo_svc)    = /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
    /bobf/if_epm_sales_order_c=>sc_bo_key ).

DATA lt_alt_key TYPE /bobf/t_epm_k_sales_order_id.  " ← alt key table type from SE11
DATA lt_key     TYPE /bobf/t_frw_key.
DATA lt_root    TYPE /bobf/t_epm_so_root.
DATA lo_msg     TYPE REF TO /bobf/if_frw_message.

" ─── Step 1: Populate the alt key table ─────────────────────────────────────
" The alt key structure has one field: the business identifier
APPEND VALUE #( so_id = 'SO_000000001' ) TO lt_alt_key.

" ─── Step 2: Convert alt key → internal GUID key ────────────────────────────
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
    " Document not found
    RETURN.
ENDIF.

" ─── Step 3: Retrieve using internal key ────────────────────────────────────
lo_svc->retrieve(
    EXPORTING
        iv_node_key = /bobf/if_epm_sales_order_c=>sc_node-root
        it_key      = lt_key
    IMPORTING
        et_data     = lt_root
        eo_message  = lo_msg
).
```

**Finding the alt key table type:**
Open `/BOBF/CONF_UI` → node → Alternative Keys tab → note the key structure name → in SE11 the table type follows the same naming convention with a `T_` prefix, or search for `*K_<BO_short>*`.

---

### Pattern 3: Retrieve by Association

Navigate from parent node instances to their children. Always prefer this over issuing a separate query for child data — it uses the framework buffer and is mass-enabled.

```abap
" ─── Declarations ───────────────────────────────────────────────────────────
" Assumes lt_root_keys is already populated from a previous query/retrieve
DATA lt_items    TYPE /bobf/t_epm_so_item.      " ← child node table type
DATA lt_key_link TYPE /bobf/t_frw_key_link.     " maps root key → child key
DATA lo_msg      TYPE REF TO /bobf/if_frw_message.

" ─── Retrieve children for ALL root instances in one call ───────────────────
lo_svc->retrieve_by_association(
    EXPORTING
        iv_node_key    = /bobf/if_epm_sales_order_c=>sc_node-root
        it_key         = lt_root_keys             " ALL root keys — not one at a time
        iv_association = /bobf/if_epm_sales_order_c=>sc_association-root-item
        iv_fill_data   = abap_true                " populate et_data directly
    IMPORTING
        et_data        = lt_items
        et_key_link    = lt_key_link              " optional: root↔child mapping
        eo_message     = lo_msg
).

" ─── Use et_key_link to correlate items back to their root ──────────────────
" lt_key_link-source_key = root instance DB_KEY (GUID)
" lt_key_link-key        = item instance DB_KEY (GUID)

LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ls_item>).
    " Find the root key for this item
    READ TABLE lt_key_link WITH KEY key = <ls_item>-key
        ASSIGNING FIELD-SYMBOL(<ls_link>).
    IF sy-subrc = 0.
        " <ls_link>-source_key is the parent root's DB_KEY
    ENDIF.
ENDLOOP.
```

**`iv_fill_data` flag:**

| Value | Behaviour |
|-------|-----------|
| `abap_true` | Populates `et_data` with full node data. Use for most read scenarios. |
| `abap_false` | Only returns `et_key_link`. Use when you want to further filter before retrieving data. |

---

### The Typing Problem

`et_data` in both `retrieve()` and `retrieve_by_association()` is typed as `ANY TABLE` — completely generic. The framework accepts whatever table you pass in. If the type is wrong you get either a runtime type conflict dump or silent garbage data.

**You must find the correct concrete table type for each node.**

Convention: `<namespace>T_<structure_suffix>`

To find it systematically:
1. In `/BOBF/CONF_UI` → click the node → note the "Node Structure" field (a DDIC structure, e.g., `/BOBF/S_EPM_SO_ROOT`)
2. In SE11 → open that structure → the table type will be the same name with `S_` replaced by `T_` (e.g., `/BOBF/T_EPM_SO_ROOT`)
3. Verify in SE11 that the table type exists and its line type matches the structure

---

## Layer 3 — Finding Constants for an Unknown BO

When you encounter an unfamiliar BO in a project, use this sequence:

### Identify the BO name

**Path A — You know the module:**
Search in `/BOBF/CONF_UI` with wildcards: `/SCMTMS/*`, `/EHS/*`, etc.

**Path B — You know a database table:**
1. Open the table in SE11
2. Check if one key field is typed `DB_KEY TYPE /BOBF/BOPF_KEY` — if yes, it is BOPF-managed
3. In `/BOBF/CONF_UI`, search for BOs whose root node table matches

**Path C — Browse everything:**
Transaction `BOBX` lists all BOs in the system (~500 in S/4HANA). Filter by package or namespace.

### Find the constants interface

In SE24, search: `*IF_<BO_short_name>*_C`

Or from `/BOBF/CONF_UI` → Generated Objects tab → the constants interface is listed there with its full name.

### Find node data types

`/BOBF/CONF_UI` → click node → General tab → note "Node Structure" → go to SE11 → derive table type.

### Find alternative key types

`/BOBF/CONF_UI` → node → Alternative Keys tab → each entry shows the key structure → derive table type in SE11.

### Find association constants

`/BOBF/CONF_UI` → node → Associations tab → each entry shows the constant name and target node.

---

## Investigation Checklist

Use this sequence when starting work on any unfamiliar BOPF BO:

```
□ 1. /BOBF/CONF_UI
      → Enter BO name (use BOBX to browse if unknown)
      → Map the node tree: ROOT + all child nodes
      → For each relevant node: note structure type, queries, alt keys, associations

□ 2. /N/BOBF/TEST_UI
      → Load a real representative instance via SELECT_BY_ELEMENTS query
      → Navigate every node using "By Association"
      → Identify exactly which node holds the field you need
      → Note the exact field name as it appears in the data

□ 3. SE24 → Open the constants interface
      → Note sc_bo_key
      → Note sc_node-<nodename> for each node you need
      → Note sc_query-<node>-select_by_elements (and any custom queries)
      → Note sc_alternative_key-<node>-<keyname> if starting from business key
      → Note sc_association-<node>-<childnode> for navigation

□ 4. SE11
      → Open the node structure found in step 1
      → Confirm field names match what TEST_UI showed
      → Note the table type for et_data (structure name with T_ prefix)
      → For alt key: open key structure → note table type

□ 5. Write code
      → get_service_manager( sc_bo_key )
      → convert_altern_key() if starting from business key  -OR-
        query() with select_by_elements if filtering by attributes
      → retrieve() to get node data
      → retrieve_by_association() for child nodes — pass ALL parent keys at once
      → Check eo_message->check() after each API call
```

---

## Common Existing BOs

BOs you are likely to encounter in S/4HANA and related modules:

| Module | BO Name | Constants Interface | Notes |
|--------|---------|---------------------|-------|
| **EPM Demo** | `/BOBF/EPM_SALES_ORDER` | `/BOBF/IF_EPM_SALES_ORDER_C` | SAP's built-in demo BO. Best sandbox for learning API patterns. Has root, item, contact nodes and sample data. |
| **TM Forwarding Order** | `/SCMTMS/TRQ` | `/SCMTMS/IF_TRQ_C` | Complex. Many nodes. Entry point for TM freight forwarding. |
| **TM Freight Order / Booking** | `/SCMTMS/TOR` | `/SCMTMS/IF_TOR_C` | Freight execution in TM. |
| **EH&S** | `/EHS/*` prefix | `/EHS/IF_*_C` | Multiple BOs covering different EH&S domains. |
| **S/4HANA standard apps** | Various | Check `/BOBF/CONF_UI` | Use `BOBX` to browse. The S4 Fiori Model Analyzer tool can identify the BO behind any Fiori app. |

> **Start learning with `/BOBF/EPM_SALES_ORDER`.** It is safe (demo data, no business impact), well-structured (root → item → contact), has alternative keys and associations, and is fully explorable in TEST_UI.

---

## Performance Notes

### Mass-read everything upfront

The BOPF API is mass-enabled by design. Never call retrieve or retrieve_by_association in a loop with single keys.

```abap
" ❌ Wrong — single key per call in a loop
LOOP AT lt_all_orders ASSIGNING FIELD-SYMBOL(<order>).
    DATA(lt_single_key) = VALUE /bobf/t_frw_key( ( key = <order>-key ) ).
    lo_svc->retrieve_by_association(
        it_key = lt_single_key ... ).  " DB hit on every iteration
ENDLOOP.

" ✅ Correct — pass ALL keys in one call
lo_svc->retrieve_by_association(
    it_key = lt_all_root_keys ... ).   " Single buffered read for all instances
```

### Use `it_requested_attributes` to limit field retrieval

When you only need a subset of fields, pass the attribute names you want. This reduces buffer memory and can avoid unnecessary field population.

```abap
lo_svc->retrieve(
    EXPORTING
        iv_node_key             = /bobf/if_epm_sales_order_c=>sc_node-root
        it_key                  = lt_key
        it_requested_attributes = VALUE #(
            ( /bobf/if_epm_sales_order_c=>sc_node_attribute-root-buyer_name   )
            ( /bobf/if_epm_sales_order_c=>sc_node_attribute-root-gross_amount )
        )
    IMPORTING
        et_data    = lt_root
        eo_message = lo_msg
).
```

### The BOPF buffer

The framework maintains an in-memory buffer per LUW. Repeated reads of the same instance within the same LUW hit the buffer, not the database. This makes it safe to call retrieve multiple times for the same keys without performance penalty.

---

## Key Interfaces and Classes Reference

### Factory Classes (entry points)

```abap
/BOBF/CL_TRA_SERV_MGR_FACTORY=>get_service_manager( iv_bo_key )
/BOBF/CL_TRA_TRANS_MGR_FACTORY=>get_transaction_manager( )
/BOBF/CL_FRW_FACTORY=>get_new_key( )    " Generate a new GUID key
```

### Core Interfaces

```abap
/BOBF/IF_TRA_SERVICE_MANAGER     " All read operations
    ->query( )
    ->retrieve( )
    ->retrieve_by_association( )
    ->convert_altern_key( )

/BOBF/IF_TRA_TRANSACTION_MGR     " Write operations only
    ->modify( )
    ->save( )
    ->cleanup( )

/BOBF/IF_FRW_MESSAGE             " Message container returned by all API calls
    ->check( )                   " Returns abap_true if ERROR messages exist
    ->get_messages( )            " Retrieve individual messages
```

### Common Types

```abap
/BOBF/T_FRW_KEY                  " Table of GUIDs — returned by query(), used in retrieve()
/BOBF/S_FRW_KEY                  " Single GUID key entry
/BOBF/T_FRW_QUERY_SELPARAM       " Selection parameters table for query()
/BOBF/S_FRW_QUERY_SELPARAM       " Single selection parameter (sign, option, low, high, attribute_name)
/BOBF/T_FRW_KEY_LINK             " Root↔child key mapping from retrieve_by_association()
/BOBF/BOPF_KEY                   " Raw GUID type (= RAW 16)
```

### Constants Interface — Attribute Name Usage

Attribute names in `it_selection_parameters` must be passed as the string value of the constant, not as a variable reference. Use the `sc_node_attribute` component:

```abap
" Correct
attribute_name = /bobf/if_epm_sales_order_c=>sc_node_attribute-root-buyer_name

" This is a TYPE /BOBF/ATTRIBUTE_KK (character string)
" Its value is the DDIC field name, e.g., 'BUYER_NAME'
```

---

## Transactions Summary

| Transaction | Alias | Purpose |
|-------------|-------|---------|
| `/BOBF/CONF_UI` | — | Model browser. Explore BO structure, nodes, types, constants. |
| `/BOBF/TEST_UI` | `BOBT` | Interactive data browser. Load, navigate, and inspect real instances. |
| `BOBX` | — | List all BOs in the system. Browse by package or namespace. |
| `/BOBF/CUST_UI` | — | Enhancement workbench. Add custom nodes/behaviors to existing BOs. |

---

*Last updated: 2026-03-27*
*Reference: BOPF/BOBF — Classic Business Object Processing Framework (maintenance mode as of S/4HANA 2021; RAP is recommended for new development)*

# SPAU & SPDD: Code Remediation During S/4HANA Upgrades

A brownfield-focused reference for ABAP developers managing modification adjustments after SAP support package (SP) stacks, EHP upgrades, or S/4HANA system conversions.

---

## Table of Contents

1. [Overview: Why SPAU/SPDD Exist](#overview)
2. [SPDD – Dictionary Object Adjustments](#spdd)
3. [SPAU – Repository Object Adjustments](#spau)
4. [Upgrade / SP Stack Workflow](#workflow)
5. [Modification Types and Adjustment Actions](#modification-types)
6. [Key Adjustment Decisions](#key-decisions)
7. [Brownfield Realities and Common Pitfalls](#brownfield-realities)
8. [Batch Handling Strategies](#batch-handling)
9. [Z-Program Impacts from SAP Changes](#z-program-impacts)
10. [Testing After Remediation](#testing-after-remediation)
11. [Preventive Practices: Reducing Future SPAU Work](#preventive-practices)
12. [Quick Reference Cheat Sheet](#quick-reference)

---

## 1. Overview: Why SPAU/SPDD Exist {#overview}

When SAP delivers new support packages or upgrades, the import process compares the **original version** of SAP objects (the version at the time you made your modification) against the **new SAP version** being imported.

If you previously modified an SAP standard object (via modification, not enhancement), the import machinery:
1. Detects a conflict between the modified version you have and the new SAP version
2. Suspends the import for that object
3. Queues it in **SPDD** (DDIC objects) or **SPAU** (repository objects) for manual resolution

```
Timeline:

  Original SAP object           → You modified it (with modification key)
        ↓                               ↓
  SAP delivers new version    ←  Conflict detected by upgrade/SP import
        ↓
  Object lands in SPAU/SPDD queue
        ↓
  Developer must decide: keep mod? integrate? reset to SAP?
```

**The fundamental question in every SPAU/SPDD entry:**
> Has SAP's new delivery made my modification unnecessary, partially addressed, or conflicting? What is the correct merged result?

---

## 2. SPDD – Dictionary Object Adjustments {#spdd}

### What SPDD Handles

SPDD covers **ABAP Dictionary (DDIC) objects** that were modified before the upgrade:

| Object Type | Examples |
|---|---|
| Database tables | `VBAK`, `MARA`, custom field additions |
| Structures | `BAPIRET2`, communication structures |
| Data elements | Modified short texts, search helps |
| Domains | Modified fixed values |
| Views | Modified maintenance views |
| Type groups | Modified TYPE POOL objects |
| Table types | Modified table type definitions |

### When SPDD Runs

**SPDD must be completed BEFORE SPAU.** This is enforced by the upgrade framework. You cannot proceed to repository object adjustment until all DDIC conflicts are resolved.

Timing in upgrade phases:
```
Phase: PREP → SPDD runs here (before objects imported)
Phase: MAIN → SPAU runs here (after SPDD complete)
```

### SPDD Actions per Object

For each flagged DDIC object you have three choices:

**1. Reset to Original (Remove Modification)**
- Your changes are discarded
- SAP's new version is accepted as-is
- Use when: SAP delivered your enhancement natively, or the modification is no longer needed

**2. Adopt SAP Version (with manual merge)**
- The system shows a three-way diff: your version / original SAP / new SAP
- You manually integrate your changes into the new SAP version
- Use when: Your change is still needed AND SAP changed the same object

**3. Keep Your Version (Override SAP)**
- Your existing modified version is kept
- SAP's new delivery is ignored for this object
- Use when: You are 100% certain SAP's changes don't affect your requirements
- **Risk:** You may lose SAP bug fixes or new functionality

### SPDD Table Field Additions (Most Common Scenario)

The most frequent SPDD case in brownfield S/4HANA: customer added a Z-field to a SAP table (common pre-upgrade practice, now replaced by Append Structures).

```
SPDD Scenario: Z-field added directly to MARA

Problem:
  Your version of MARA: standard fields + ZMYFIELD (Char 10)
  SAP new MARA: standard fields + several new SAP fields + modified field lengths

SPDD will show conflict.

Best resolution:
  1. Check if ZMYFIELD already exists in SAP's version (unlikely but check)
  2. Accept SAP's new version
  3. Manually re-add your ZMYFIELD via append structure instead
  4. Mark original modification as "reset to SAP"

Going forward:
  Never add Z-fields directly to SAP tables — always use Append Structures (SE11 → Append)
  Append structures survive upgrades without SPDD conflicts
```

---

## 3. SPAU – Repository Object Adjustments {#spau}

### What SPAU Handles

SPAU covers all **repository objects** other than DDIC:

| Object Type | Examples |
|---|---|
| Programs / Includes | Modified SAP reports, function group includes |
| Function modules | Modified standard FMs (e.g., SD_SALESDOCUMENT_CREATE) |
| Classes / Methods | Modified standard ABAP OO classes |
| Subroutine pools | Modified SAPL* programs |
| Message classes | Modified message texts |
| Screens / Dynpros | Modified SAP screen layouts |
| GUI statuses / titles | Modified SAP menus/toolbars |
| Form routines | Modified SAP subroutines |
| Module pool programs | Modified transaction programs |

### SPAU Modification Categories

SAP marks each SPAU entry with a category indicating the nature of the conflict:

| Category | Meaning |
|---|---|
| **Not Modified** | Modification exists but no conflict — SAP version unchanged |
| **Modification** | Your version conflicts with new SAP version |
| **Repair** | You modified an SAP note fix that SAP now delivers standardly |
| **Cannot Be Adjusted** | Object locked or broken — needs manual intervention |

### SPAU Adjustment Options

**Option 1: Reset to Original / Adopt SAP Version**
- Removes your modification entirely
- Accepts SAP's new code
- Best when the modification was a workaround that SAP has now fixed properly

**Option 2: Adjust Manually**
- Opens a comparison editor (three-way diff)
- Your version | SAP original at time of modification | SAP new version
- You determine the merged result
- This is the most common and most time-consuming action

**Option 3: Keep Modification (Confirm Without Change)**
- Marks the entry as "adjusted" without actually merging
- Use only when you've verified the new SAP code doesn't conflict with yours
- **Important:** If SAP moved code around in the same routine, this can silently break things

---

## 4. Upgrade / SP Stack Workflow {#workflow}

### Standard Sequence

```
1. Pre-Analysis (Before Upgrade)
   ├── Run SPDD/SPAU analysis in sandbox first
   ├── Catalog all modified objects (SE95 → Modification Browser)
   ├── Estimate effort per object
   └── Assign objects to responsible developers

2. Sandbox / Dev System Upgrade
   ├── Complete SPDD (all DDIC objects)
   ├── Complete SPAU (all repository objects)
   ├── Unit test modified areas
   └── Document decisions made per object

3. QA / Integration System Upgrade
   ├── Apply same SPDD/SPAU decisions (can transport some)
   ├── Integration testing
   └── Regression test Z-programs that use modified objects

4. Production Upgrade
   ├── Minimal SPDD/SPAU (decisions already made)
   └── Monitor for issues post-go-live
```

### How to Access SPAU/SPDD

```
Transaction SE95:  Modification Browser (see all your modifications before upgrade)
Transaction SPDD:  During upgrade (DDIC objects)
Transaction SPAU:  During upgrade (repository objects)
Transaction SPAU_ENH: Enhanced SPAU with filtering options (newer releases)
```

### Pre-Upgrade Analysis with SE95

Before the upgrade, run SE95 to inventory all modified objects:

```
SE95 → Execute

Filter options:
  - By namespace (Z*, Y*)
  - By object type (programs, function modules, etc.)
  - By package
  - By modification date

Key columns to review:
  - Object type
  - Object name
  - Original author
  - Last changed
  - Has note? (SAP note applied as mod?)
```

Export the SE95 list to Excel. For each entry, assess:
- Is this modification still business-relevant?
- Has SAP delivered this functionality natively in the new release?
- Which developer should handle this?

---

## 5. Modification Types and Adjustment Actions {#modification-types}

### Type 1: SAP Note Applied as Modification

A common scenario — you applied an SAP note manually (not via SNOTE) and it required a modification key.

```
SPAU Resolution:
  If SAP note is now included in the new SP:
    → Reset to SAP (SAP now delivers it natively)
    → No merge needed

  If SAP note is NOT yet in the new SP:
    → Re-apply via SNOTE after upgrade
    → Or adjust manually to re-apply patch in new code

Check: In SNOTE, see if the note status changes to "obsolete" after upgrade
       This means SAP absorbed the fix into their baseline
```

### Type 2: Business Logic Added to Standard Program

You added custom processing to a standard SAP routine — e.g., added custom pricing logic inside MV45AFZZ or added exit calls to a standard include.

```abap
* Example: Your code added to a SAP include (BEFORE UPGRADE)
FORM USEREXIT_SAVE_DOCUMENT.
  " Standard SAP code that was here...

  " YOUR ADDITION - custom validation
  PERFORM Z_VALIDATE_CUSTOM_FIELDS.
  " END YOUR ADDITION

  " More SAP standard code...
ENDFORM.
```

```
SPAU Resolution:
  1. Open three-way comparison editor
  2. Identify: what changed in the SAP routine?
  3. Identify: where exactly is your code?
  4. Determine: does SAP's change conflict with your position?
  5. Merge: insert your code into the new SAP version at the correct position
  6. Verify: no duplicate logic (did SAP add similar functionality?)
```

### Type 3: Standard Include Replaced by Enhancement Framework

SAP sometimes moves logic from user exits (MV45AFZZ-style) to BAdIs between releases. If the include was deprecated:

```
SPAU Resolution:
  1. Note: SAP deprecates INCLUDE MV45AFZZ → recommends BAdI BADI_SD_SALES
  2. Your mod to MV45AFZZ is now in "legacy" territory
  3. Decision: 
     - Short term: Adjust mod in MV45AFZZ anyway (works but deprecated)
     - Correct path: Migrate logic to the BAdI (no more mod needed)
  
Recommendation: Use SPAU as opportunity to migrate away from modifications
toward enhancements (BAdIs, User Exits, Enhancement Spots)
```

### Type 4: Screen / Dynpro Modification

You added a field or changed screen layout of a SAP standard transaction.

```
SPDD/SPAU Resolution for screens:
  - Very high risk of conflicts
  - Screen Painter comparison is visual
  - SAP may have completely redesigned the screen
  - If SAP redesigned: may need to redo from scratch

Better long-term approach:
  - Screen BADIs for field additions
  - Customer fields via Customizing (Logistics → screen configuration)
  - Field extensibility frameworks (S/4HANA)
```

### Type 5: Message Class Modification

You changed SAP message texts (e.g., translated messages or modified message text for legal compliance).

```
SPAU Resolution:
  - Usually low risk
  - Compare old message text vs new SAP text
  - If SAP changed the text: decide which is correct
  - If your text is translation: generally safe to keep
  - If SAP fixed an error in message text: adopt SAP version
```

---

## 6. Key Adjustment Decisions {#key-decisions}

### Decision Framework per Object

For every SPAU/SPDD entry, work through this logic:

```
Step 1: Is this modification still needed?
  ├── YES → Continue to Step 2
  └── NO  → Reset to SAP (remove modification)

Step 2: Did SAP deliver equivalent functionality natively?
  ├── YES → Reset to SAP, implement via standard config or BAdI
  └── NO  → Continue to Step 3

Step 3: Does the new SAP version conflict with our modification?
  ├── NO CONFLICT → Confirm without change (verify the mod still works)
  └── CONFLICT    → Adjust manually (three-way merge)

Step 4: After merge, does the merged result make functional sense?
  ├── YES → Mark as adjusted, schedule testing
  └── NO  → Escalate to functional consultant for re-analysis
```

### The Three-Way Comparison Editor

When you select "Adjust Manually" in SPAU, the editor shows:

```
Left pane:    Your current version (the modified code)
Center pane:  SAP original (what SAP shipped when you made your modification)
Right pane:   SAP new version (what SAP is delivering now)

Your task: Produce a merged version that contains:
  - SAP's new logic/fixes
  - Your custom additions
  - No duplicate logic
  - No orphaned code
```

Practical tips for the merge editor:
- Start from SAP's right pane (new version) as your base
- Identify your custom code in the left pane
- Port your custom additions into the right pane baseline
- Check if SAP's new version already handles what your modification did

### Common Merge Mistakes

**Mistake 1: Keeping your version without reading SAP's changes**
```
Risk: SAP added a critical fix in the same routine
      Your "keep my version" discards the fix
      System may behave incorrectly after upgrade
```

**Mistake 2: Accepting SAP's new version without re-inserting your code**
```
Risk: Business logic silently disappears after upgrade
      No short dump — just missing behavior
      Discovered in production by end users
```

**Mistake 3: Double-inserting logic**
```
Risk: Your mod already calls Z_MY_FM
      You merge without noticing SAP's new version also calls equivalent logic
      Logic runs twice → duplicate postings, double emails, etc.
```

**Mistake 4: Ignoring changed method signatures**
```abap
" Before upgrade: SAP function module signature
CALL FUNCTION 'SOME_FM'
  EXPORTING
    I_VBELN = lv_vbeln.   " Only one parameter

" After upgrade: SAP added new mandatory parameter
CALL FUNCTION 'SOME_FM'
  EXPORTING
    I_VBELN  = lv_vbeln
    I_BUKRS  = lv_bukrs.  " NEW — your old code won't compile!

" Your Z-programs that call this FM will also break
" Even if SPAU is "done" for the FM itself
```

---

## 7. Brownfield Realities and Common Pitfalls {#brownfield-realities}

### Reality 1: SPAU is Cumulative Debt

Every modification you made = debt you pay at every upgrade. In a brownfield S/4HANA system with years of modifications, SPAU can have **hundreds of entries**. Prioritize ruthlessly.

```
High Priority (fix before QA system goes live):
  - Modifications to SD billing, tax, and financial posting routines
  - Modifications to anything touching legal compliance (CFDI, tax determination)
  - Modifications that touch integration points (IDocs, RFC)

Medium Priority:
  - Reporting and display modifications
  - User exits for additional validation

Low Priority (can defer):
  - Message text changes
  - Screen layout cosmetics
  - Unused programs
```

### Reality 2: Some Modifications Are "Zombie" Mods

Objects that were modified years ago for requirements that no longer exist. The developer is gone. The business process changed. But the modification is still flagged.

```
How to identify zombie modifications in SE95:
  - Last changed date: years ago
  - Package: an old project package, not active
  - Related business process: no longer used

Decision: Reset to SAP without hesitation.
But first: Ask functional team to confirm the process is truly inactive.
```

### Reality 3: SAP Notes Create Hidden SPAU Entries

When you implement SAP notes manually (not via SNOTE), or when SNOTE implements a note that requires a modification key, those show up in SPAU.

```
Problem: You may have 50 SAP note-related SPAU entries that are NOT real issues —
         they're simply notes that SAP has now absorbed into the new SP.

Quick resolution:
  1. Filter SPAU by entries that match SAP note patterns
  2. For each: check SNOTE to see if the note is now obsolete/included
  3. If included in new SP: Reset to SAP
  4. If not yet included: Re-apply via SNOTE after upgrade completes
```

### Reality 4: Implicit SPAU-Like Impacts on Z-Programs

Your Z-programs are NOT in SPAU (they're not SAP modifications). But if they call modified SAP FMs, use modified structures, or depend on SAP standard behavior you relied upon, they can silently break after SPAU is "done."

```
Example impact chain:
  SAP upgrades function module PRICING_COMPLETE
  Your SPAU adjustment handles the FM itself correctly
  
  BUT: Your Z-program ZSD_CUSTOM_PRICING_RPT calls PRICING_COMPLETE
       with the OLD parameter interface
  
  Result: Short dump in Z-program after upgrade
          SPAU showed "complete" but Z-programs break
```

See section 9 for systematic Z-program impact analysis.

### Reality 5: S/4HANA Compatibility Packages (SSCR)

In S/4HANA conversions, SAP added the concept of compatibility packages — some objects that existed in ECC are no longer modifiable the same way. Check:

```
Transaction SCPR20: Review which objects are now under "Customizing" control
                    vs. modification control

S/4HANA-specific:
  - Many classic enhancement spots now point to new BAdIs
  - Some modifications to SD/FI documents are replaced by Extensibility Framework
  - Check: SAP Readiness Check output for modification impact analysis
```

---

## 8. Batch Handling Strategies {#batch-handling}

### Grouping SPAU Entries for Efficiency

Don't work through SPAU entry by entry randomly. Group by:

```
Group 1: SAP Note modifications (likely mass-reset-to-SAP)
  → Handle in batch by checking SNOTE obsolescence

Group 2: Modifications by module (SD together, MM together, FI together)
  → Assign to module-specialist developers

Group 3: Modifications with "not modified" category
  → SAP didn't change these objects: quick confirm, low risk

Group 4: Modifications to deprecated objects
  → Objects SAP plans to retire: migrate away entirely

Group 5: High-risk modifications (SD billing, FI posting, tax)
  → Handle last, test most thoroughly
```

### SPAU_ENH: The Better Transaction (Newer Systems)

On newer S/4HANA releases, use `SPAU_ENH` instead of plain `SPAU`:

```
SPAU_ENH advantages:
  - Filter by object type, package, responsible developer
  - Transport-aware: see which transport contains the modification
  - Priority flags
  - Export object lists to file
  - Mass operations for low-risk entries
  - Shows "original language" indicator for translations
```

### Transport Strategy for SPAU Decisions

```
Option A: Each developer works in their own transport
  + Parallel work possible
  - Merge conflicts if developers adjust the same base objects
  - Many small transports to manage through landscape

Option B: One central "SPAU correction" transport per system
  + Clean transport management
  - Sequential bottleneck (developers wait for each other)

Recommended in practice:
  - One transport per module team
  - SD team → one transport
  - MM team → one transport
  - FI/CO team → one transport
  - Release in sequence: FI first (dependencies), then SD, then MM
```

---

## 9. Z-Program Impacts from SAP Changes {#z-program-impacts}

### What SAP Changes Can Break Z-Programs

Even if your Z-program has no SPAU entry (it's not a modification), SAP's upgrade can still break it:

| SAP Change | Z-Program Impact |
|---|---|
| FM interface change (new mandatory parameter) | Short dump: parameter missing |
| FM behavior change (different return codes) | Silent logic error |
| Structure field renamed or type changed | Short dump or wrong data |
| Table field removed or type changed | Short dump in SELECT |
| Standard class method signature change | Short dump in OO calls |
| Message class renumbering | Wrong messages displayed |
| Database table restructuring | Performance degradation or SELECT failure |
| Authorization object changes | Missing authorization checks |

### Systematic Impact Analysis

**Step 1: Identify Changed SAP Objects**

Use SE95 / SPAU list → extract object names → use WHERE-USED to find Z-program dependencies:

```abap
" Mass where-used analysis — run in SE30 or as background job
" Find all Z-programs using modified SAP function modules
SELECT * FROM TFDIR
  WHERE FUNCNAME IN (SELECT OBJECT_NAME FROM MOD_LIST_TABLE)  " your SPAU FM list
  INTO TABLE lt_fms.

" Then use RS_WHERE_USED_SIMPLE or similar to find callers
```

**Step 2: Review Changed FM Interfaces**

For each modified FM in SPAU, check if the interface changed:

```
SE37 → Open the FM
Compare: Old signature (SE95 / modification log) vs. new signature

Focus on:
  - New MANDATORY parameters (will break callers)
  - Removed parameters (will break callers)
  - Changed TYPES (may cause type conflicts)
  - New EXCEPTIONS (may be unhandled)
```

**Step 3: Automated Syntax Check After Upgrade**

After SPDD/SPAU is done and objects are imported:

```
SE80 → Mass syntax check:
  Program → Check → Extended Program Check → Mass variant
  Scope: All Z/Y programs in relevant packages

Or: Use ABAP Test Cockpit (ATC) in ADT for broader analysis

Or: Transaction SCII (extended syntax check, batch)
```

---

## 10. Testing After Remediation {#testing-after-remediation}

### Test Priorities

```
Priority 1 — Business-Critical Processes
  ├── Order-to-cash (SD): Create → deliver → invoice → accounting
  ├── Procure-to-pay (MM): PO → GR → IR → payment
  ├── Financial closing: Period-end postings, clearing
  └── Legal compliance: Tax, CFDI (if applicable), document output

Priority 2 — Processes Using Modified Objects
  ├── Every business process that calls a modified FM or uses a modified include
  └── Identified from Z-program impact analysis (Section 9)

Priority 3 — Interfaces and Integration
  ├── IDoc processing (inbound and outbound)
  ├── RFC calls from external systems
  └── Any APIs exposed to third parties

Priority 4 — Reports and Outputs
  ├── Key reports that use custom logic or modified data
  └── Print programs and form outputs
```

### Regression Testing After SPAU

For each adjusted modification, define a minimal test:

```
Modification: Custom code in USEREXIT_PRICING_PREPARE_TKOMP
Test: Create a sales order with the pricing conditions that trigger your custom logic
      Verify: correct price calculated
      Verify: no short dump
      Verify: correct accounting document after billing

Modification: Custom include in SD billing routine
Test: Complete full billing cycle for representative order types
      Verify: billing document created correctly
      Verify: accounting entries match expectations
      Verify: output (invoice PDF) generated correctly
```

### Post-Upgrade Monitoring

After go-live:
```
SM21: System log — watch for new dumps in first 48 hours
ST22: ABAP dump analysis — triage any new dumps immediately
SLG1: Application log — watch for unexpected error messages
SXMB_MONI: If using PI/PO — watch for interface failures
WE05/WE02: IDoc monitoring — watch for processing failures
```

---

## 11. Preventive Practices: Reducing Future SPAU Work {#preventive-practices}

### The Core Principle: Never Modify, Always Enhance

Every direct SAP modification creates future SPAU debt. Every enhancement (BAdI, Enhancement Spot, User Exit) does NOT.

```
INSTEAD OF:                          USE:
─────────────────────────────────    ────────────────────────────────────
Modifying standard include           BAdI or Enhancement Spot
Adding field to SAP table            Append Structure (SE11 Append)
Changing SAP screen layout           Screen BAdI / Fiori Extensibility
Modifying SAP function module        Wrapper Z-FM that calls SAP FM
Modifying SAP message texts          Customer message class or override
Modifying SAP report                 Z-report that reads same data
```

### Enhancement Framework Quick Reference

```abap
" Find available BAdIs for a transaction (SE18 / SE19)
" SE18: BAdI Builder (view definition)
" SE19: BAdI Implementation

" Find Enhancement Spots in source code
" SE84 → Repository Information System → Enhancements → Enhancement Spots

" Create implementation for a BAdI
SE19 → Create Implementation
  → Select BAdI Definition (e.g., BADI_SD_SALES)
  → Implement required methods
  → Activate
  → No modification key needed!

" Enhancement Spots — explicit spots in SAP code
ENHANCEMENT 0 Z_MY_ENHANCEMENT. "#EC CI_ENH_SPOT
  " Your code here
  PERFORM z_my_custom_logic.
END-ENHANCEMENT.
```

### S/4HANA Extensibility Framework (In-App Extensibility)

For S/4HANA on-premise, SAP provides extensibility without modifications:

```
Key Extensibility and Developer Extensibility:
  - Custom fields via SAP Fiori apps (no ABAP required)
  - Custom business logic via key user BAdIs
  - ABAP Developer Extensibility for more complex cases

Transactions:
  SPRO → Cross-Application Components → Extensibility
  BAS (Business Application Studio) for cloud extensibility
  ABAP Development Tools (ADT) for developer extensibility

Benefit: These survive upgrades without SPAU entries
```

### When Modifications Are Unavoidable

Sometimes you genuinely must modify (SAP left no enhancement point):

```
Best practices for unavoidable modifications:
  1. Document thoroughly in modification header
     " MODIFICATION: <Ticket#> <Date> <Developer>
     " REASON: No BAdI available for this exit point
     " REVIEW REQUIRED: Yes, at each SP upgrade
     " FUNCTIONAL OWNER: <Name>

  2. Keep the modification minimal
     " Don't add 200 lines to a SAP routine
     " Call a Z-method/FM instead:
     PERFORM z_my_isolated_logic.   " All your code is in Z-land

  3. Isolate your code with clear markers
     *--- BEGIN CUSTOMER MODIFICATION: <Ticket#> ---
     PERFORM z_custom_logic.
     *--- END CUSTOMER MODIFICATION: <Ticket#> ---

  4. Log in a central modification registry (SharePoint/Wiki)
     Object | Reason | Developer | Last Adjusted | Next Review Date
```

---

## 12. Quick Reference Cheat Sheet {#quick-reference}

### SPDD vs SPAU at a Glance

| Aspect | SPDD | SPAU |
|---|---|---|
| Objects | DDIC (tables, structures, domains) | Programs, FMs, classes, screens |
| Timing | Must run FIRST (before SPAU) | Runs after SPDD complete |
| Transaction | `SPDD` | `SPAU` / `SPAU_ENH` |
| Risk if wrong | Data model inconsistency | Logic errors, short dumps |
| Common scenario | Z-field added to SAP table | Custom code in SAP include |

### Decision Tree: Quick Reference

```
SPAU/SPDD Entry Arrives
        │
        ▼
Is the modification still needed by the business?
  NO  → Reset to SAP. Close.
  YES ↓
        ▼
Has SAP delivered this natively in the new release?
  YES → Reset to SAP. Implement via standard config or enhancement. Close.
  NO  ↓
        ▼
Did SAP change the same object/code area as your modification?
  NO  → Confirm without change. Schedule targeted test. Close.
  YES ↓
        ▼
Perform three-way manual merge.
Verify merged result is functionally correct.
Schedule thorough integration test.
```

### Key Transactions Summary

| Transaction | Purpose |
|---|---|
| `SE95` | Modification Browser — pre-upgrade inventory |
| `SPDD` | DDIC object adjustment (during upgrade) |
| `SPAU` | Repository object adjustment (during upgrade) |
| `SPAU_ENH` | Enhanced SPAU with filters (newer systems) |
| `SE18` | BAdI Definition Builder (find available BAdIs) |
| `SE19` | BAdI Implementation (implement BAdIs) |
| `SE84` | Repository Information System (find enhancement spots) |
| `SE37` | Function Module editor (check FM interface changes) |
| `ST22` | ABAP dump analysis (post-upgrade monitoring) |
| `SM21` | System log (post-upgrade monitoring) |
| `SNOTE` | SAP Note Assistant (manage note-based modifications) |
| `SCII` | Extended syntax check (mass check after upgrade) |

### Effort Estimation Guide

| Object Type | Typical Effort per Entry |
|---|---|
| Note-based modification (now in SP) | 5 min — reset to SAP |
| Non-conflicting modification | 10 min — confirm + test |
| Minor conflict (few lines) | 30 min — merge + test |
| Complex FM conflict | 2–4 hours — merge + integration test |
| Screen modification | 1–3 hours — visual comparison + test |
| DDIC table (append structure issue) | 30 min — restructure as append |
| Deprecated include migration | 4–8 hours — migrate to BAdI |

---

**Applies to:** SAP ECC 6.0 → S/4HANA conversions, S/4HANA SP upgrades, EHP implementations
**Object types:** All ABAP modification types managed under SSCR modification keys
**Transactions referenced:** SE95, SPDD, SPAU, SPAU_ENH, SE18, SE19, SE84, SNOTE, ST22, SM21, SCII


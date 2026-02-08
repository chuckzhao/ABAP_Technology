# OData URL Case Sensitivity Reference

A comprehensive guide to understanding case sensitivity in SAP OData URLs and HTTP URLs in general.

---

## Table of Contents

1. [Quick Answer](#quick-answer)
2. [Case Sensitivity Rules](#case-sensitivity-rules)
3. [Real-World Examples](#real-world-examples)
4. [Why Different Parts Have Different Rules](#why-different-parts-have-different-rules)
5. [URL Structure & Standards](#url-structure--standards)
6. [Complete Technical Flow](#complete-technical-flow)
7. [Historical Reasons](#historical-reasons)
8. [Real-World SAP Example](#real-world-sap-example)
9. [Why This Matters for Development](#why-this-matters-for-development)
10. [RFC References](#rfc-references)
11. [Summary Table](#summary-table)

---

## Quick Answer

**Yes, SAP OData URLs are case-sensitive for most components** (path, entity sets, properties, query parameters), but **not for protocol and domain**.

```
https://MySAP.com:8000/sap/opu/odata/Products?$filter=Price gt 10
│────┘ │────────┘ │──┘ │─────────────────┘ │─────────────────────┘
│      │          │    │                    └─ Case-Sensitive
│      │          │    └─ Case-Sensitive
│      │          └─ Case-Insensitive
│      └─ Case-Insensitive
└─ Case-Insensitive
```

---

## Case Sensitivity Rules

### ❌ Case-Insensitive Components

```
Protocol:     https:// = HTTPS:// = HtTpS://
Domain:       mysap.com = MYSAP.COM = MySap.Com
Port:         :8000 = :8000
```

### ✅ Case-Sensitive Components

```
Path:         /sap/opu/odata/sap/Z_PRODUCT_SRV/
              ≠ /SAP/OPU/ODATA/SAP/Z_PRODUCT_SRV/

Entity Sets:  /Products  ≠ /products  ≠ /PRODUCTS

Properties:   /Products(1)/ProductName
              ≠ /Products(1)/productname

Functions:    /GetProductsByCategory
              ≠ /getproductsbycategory

Query Options: ?$filter= ✅
              ?$Filter= ❌
              ?$FILTER= ❌
```

---

## Real-World Examples

### ✅ Correct URLs

```bash
# Correct entity set name (matches metadata)
https://mysap.com/sap/opu/odata/sap/Z_PRODUCT_SRV/Products

# Correct property name
https://mysap.com/sap/opu/odata/sap/Z_PRODUCT_SRV/Products(1)/ProductName

# Correct query options
https://mysap.com/sap/opu/odata/sap/Z_PRODUCT_SRV/Products?$filter=ProductID eq 1
```

### ❌ Common Mistakes

```bash
# Wrong: lowercase entity set
https://mysap.com/sap/opu/odata/sap/Z_PRODUCT_SRV/products
# Error: Resource not found

# Wrong: wrong case for property
https://mysap.com/sap/opu/odata/sap/Z_PRODUCT_SRV/Products(1)/productname
# Error: Property not found

# Wrong: uppercase query option
https://mysap.com/sap/opu/odata/sap/Z_PRODUCT_SRV/Products?$Filter=ProductID eq 1
# May fail or be ignored

# Wrong: mixed case in path
https://mysap.com/SAP/opu/ODATA/sap/Z_PRODUCT_SRV/Products
# May fail depending on server configuration
```

---

## Why Different Parts Have Different Rules

### 1. Scheme (Protocol) - Case-Insensitive

**Standard:** RFC 3986 (URI Generic Syntax)

**Why Insensitive:**
```
https://  = HTTPS:// = HtTpS://
```

**Reason:** 
- Defined by **RFC 3986** which explicitly states scheme is case-insensitive
- Historical convention from early internet days
- Schemes are identifiers (like keywords), not data
- Simplifies parsing and matching

**From RFC 3986:**
> "For resiliency, programs interpreting URI should treat scheme names case-insensitively."

---

### 2. Domain (Host) - Case-Insensitive

**Standard:** DNS (Domain Name System) - RFC 1035

**Why Insensitive:**
```
MySAP.com = mysap.com = MYSAP.COM
```

**Technical Reason:**

DNS was designed to be **case-insensitive** from the beginning (1983):

```
DNS Resolution Process:
─────────────────────

User types:     MySAP.com
Browser sends:  mysap.com  ← Converted to lowercase
DNS receives:   mysap.com  ← All DNS queries are lowercase
DNS returns:    IP address (e.g., 192.168.1.100)
```

**Why DNS is Case-Insensitive:**

1. **Simplicity** - No need to store multiple case variations
2. **Consistency** - One canonical form (lowercase)
3. **Historical** - Early internet systems were case-insensitive
4. **Practicality** - Users shouldn't have to remember exact capitalization

**Real Example:**
```bash
# All these resolve to the same IP
ping MySAP.com
ping mysap.com
ping MYSAP.COM
ping mYsAp.CoM

# DNS lookups are identical
nslookup MySAP.com  → 192.168.1.100
nslookup MYSAP.COM  → 192.168.1.100
```

---

### 3. Path - Case-Sensitive

**Standard:** HTTP servers decide (usually case-sensitive)

**Why Sensitive:**
```
/sap/opu/odata/Products  ≠  /SAP/OPU/ODATA/products
```

**Technical Reason:**

The path maps to the **server's resource structure**, which is often:

**Unix/Linux Filesystems (Case-Sensitive):**
```
/var/www/html/
├── Products.html       ← Different file
└── products.html       ← Different file
```

**Windows Filesystems (Case-Insensitive but Case-Preserving):**
```
C:\inetpub\wwwroot\
├── Products.html       ← Same file as products.html
└── products.html       ← (Windows sees these as identical)
```

**SAP Application Server:**
```
SAP URL Path:
/sap/opu/odata/sap/Z_PRODUCT_SRV/Products

Maps to:
ICF Node Hierarchy (Internet Communication Framework)
└── sap
    └── opu
        └── odata
            └── sap
                └── Z_PRODUCT_SRV  ← Case-sensitive!
                    └── Products   ← Entity set name
```

**Why Path MUST Be Case-Sensitive:**

1. **Resource Identification** - Different cases = different resources
2. **Operating System** - Unix/Linux are case-sensitive
3. **Flexibility** - Allows `/Products` and `/products` to be different resources
4. **RESTful Design** - Resources should be distinct and identifiable

---

### 4. Query String - Case-Sensitive

**Standard:** HTTP specification (server-defined)

**Why Sensitive:**
```
?$filter=ProductName  ≠  ?$Filter=productname
```

**Reason:**
- Query parameters are **data**, not identifiers
- Server application decides how to interpret them
- Case changes meaning: `name=John` ≠ `name=john`

---

## URL Structure & Standards

```
https://MySAP.com:8000/sap/opu/odata/Products?$filter=Price gt 10
│────┘ │────────┘ │──┘ │─────────────────┘ │─────────────────────┘
│      │          │    │                    └─ Query String
│      │          │    └─ Path
│      │          └─ Port
│      └─ Domain (Host)
└─ Scheme (Protocol)

Case-Insensitive: Scheme, Domain, Port
Case-Sensitive:   Path, Query String
```

---

## Complete Technical Flow

```
URL: https://MySAP.com/sap/opu/odata/Products?$filter=Name eq 'John'

┌─────────────────────────────────────────────────────────────┐
│ Step 1: Browser Parses URL                                  │
├─────────────────────────────────────────────────────────────┤
│ Scheme:   https    (normalized to lowercase)                │
│ Domain:   MySAP.com (passed as-is to DNS)                   │
│ Path:     /sap/opu/odata/Products (preserved exactly)       │
│ Query:    $filter=Name eq 'John' (preserved exactly)        │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ Step 2: DNS Resolution                                      │
├─────────────────────────────────────────────────────────────┤
│ DNS Query:  mysap.com (converted to lowercase)              │
│ Response:   192.168.1.100                                   │
│                                                              │
│ Note: DNS is ALWAYS case-insensitive                        │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ Step 3: HTTP Request to Server                              │
├─────────────────────────────────────────────────────────────┤
│ GET /sap/opu/odata/Products?$filter=Name eq 'John' HTTP/1.1 │
│ Host: MySAP.com                                             │
│                                                              │
│ Path sent EXACTLY as typed: /sap/opu/odata/Products        │
│ Query sent EXACTLY as typed: $filter=Name eq 'John'        │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ Step 4: SAP Server Processing                               │
├─────────────────────────────────────────────────────────────┤
│ ICF Handler matches path:                                   │
│   /sap/opu/odata/Products  ← Case-sensitive match!         │
│                                                              │
│ OData Service parses query:                                 │
│   $filter (must be lowercase)                               │
│   Name (must match entity property exactly)                 │
│                                                              │
│ If case mismatch → 404 or error                            │
└─────────────────────────────────────────────────────────────┘
```

---

## Historical Reasons

### Why DNS Is Case-Insensitive

**Early 1980s Design Decision:**

```
Problem in 1983:
────────────────
Different systems had different case conventions:
- Unix: case-sensitive
- VMS: uppercase only  
- DOS: case-insensitive

Solution:
─────────
Make DNS case-insensitive!
- Everyone converts to lowercase
- No confusion across systems
- Simple canonical form
```

**From RFC 1035 (DNS Specification, 1987):**
> "Domain names are case-insensitive. When matching or comparing domain names, the comparison must be case-insensitive."

---

### Why Paths Are Case-Sensitive

**Unix Filesystem Influence:**

```
Unix (1970s):
─────────────
/home/user/Documents  ← Different directory
/home/user/documents  ← Different directory

This design influenced:
- HTTP server design
- URL path handling
- RESTful APIs
```

**Web Server Behavior:**

```
Apache on Unix/Linux:
─────────────────────
/var/www/html/Products/  ← Case-sensitive
/var/www/html/products/  ← Different directory

IIS on Windows:
───────────────
C:\inetpub\Products\     ← Case-insensitive filesystem
C:\inetpub\products\     ← Same directory

But HTTP spec says:
───────────────────
URLs SHOULD be treated as case-sensitive
(even on Windows IIS)
```

---

## Real-World SAP Example

```
Complete URL Breakdown:
───────────────────────

https://sap.mycompany.com:8000/sap/opu/odata/sap/Z_PRODUCT_SRV/Products?$filter=ProductID eq 1

┌─────────────┬──────────────────┬────────────────────────────┐
│ Component   │ Case-Sensitive?  │ Standard/Reason            │
├─────────────┼──────────────────┼────────────────────────────┤
│ https       │ ❌ No            │ RFC 3986 (URI scheme)      │
│ sap.my...   │ ❌ No            │ RFC 1035 (DNS)             │
│ :8000       │ ❌ No            │ Port numbers               │
│ /sap/opu... │ ✅ Yes           │ Server resource mapping    │
│ Products    │ ✅ Yes           │ OData entity set           │
│ ?$filter    │ ✅ Yes           │ OData query parameter      │
│ ProductID   │ ✅ Yes           │ OData property name        │
└─────────────┴──────────────────┴────────────────────────────┘
```

---

## Why This Matters for Development

### In Your CDS View Definition

```abap
@EndUserText.label: 'Product - Consumption'
define root view entity ZC_PRODUCT
  as projection on ZI_PRODUCT
{
  key ProductID,      // Exact case matters!
      ProductName,    // This too!
      UnitPrice       // And this!
}
```

### In Service Definition

```abap
@EndUserText.label: 'Product Service'
define service Z_PRODUCT_SRV {
  expose ZC_PRODUCT as Products;  // "Products" is case-sensitive!
}
```

### In Your Fiori App

**manifest.json:**
```json
{
  "sap.app": {
    "dataSources": {
      "mainService": {
        "uri": "/sap/opu/odata/sap/Z_PRODUCT_SRV/",
        "type": "OData",
        "settings": {
          "odataVersion": "2.0"
        }
      }
    }
  }
}
```

**OData Queries Must Match:**
```javascript
// ✅ Correct - matches metadata exactly
oModel.read("/Products", {
  filters: [new Filter("ProductName", "EQ", "Chai")]
});

// ❌ Wrong - case mismatch
oModel.read("/products", {  // Will fail!
  filters: [new Filter("productname", "EQ", "Chai")]  // Will fail!
});
```

---

## Best Practices

### 1. Always Check $metadata

```bash
# Get exact entity and property names
https://mysap.com/sap/opu/odata/sap/Z_PRODUCT_SRV/$metadata
```

The metadata shows exact case:
```xml
<EntityType Name="Product">
  <Key>
    <PropertyRef Name="ProductID"/>  <!-- Use EXACTLY this case -->
  </Key>
  <Property Name="ProductID" Type="Edm.String"/>
  <Property Name="ProductName" Type="Edm.String"/>  <!-- Not productName! -->
  <Property Name="UnitPrice" Type="Edm.Decimal"/>
</EntityType>

<EntitySet Name="Products" EntityType="Z_PRODUCT_SRV.Product"/>
<!-- Use "Products" not "products" -->
```

### 2. Copy-Paste Entity/Property Names

Don't type them manually - copy from:
- $metadata
- Service binding in ADT
- OData service explorer

### 3. SAP Naming Conventions

**Standard SAP patterns:**
```
Entity Sets:     PascalCase     (Products, SalesOrders, CustomerSet)
Properties:      PascalCase     (ProductID, CustomerName, OrderDate)
Functions:       PascalCase     (GetProductsByCategory)
Service Path:    UPPERCASE      (/SAP/OPU/ODATA/SAP/...)
```

---

## Query Parameters Specifics

### Case-Sensitive Query Options

```bash
# ✅ Correct
?$filter=ProductName eq 'Chai'
?$select=ProductID,ProductName
?$orderby=UnitPrice desc
?$expand=Category
?$top=10
?$skip=20

# ❌ Wrong
?$Filter=ProductName eq 'Chai'    # Won't work
?$SELECT=ProductID,ProductName    # Won't work
?$OrderBy=UnitPrice desc          # Won't work
```

### Property Names in Queries

```bash
# ✅ Correct - exact case from metadata
?$filter=ProductName eq 'Chai' and UnitPrice gt 10

# ❌ Wrong - case mismatch
?$filter=productname eq 'Chai' and unitprice gt 10
```

---

## Troubleshooting Case Issues

### Error: "Resource not found"

**Problem:**
```bash
GET /sap/opu/odata/sap/Z_PRODUCT_SRV/products
404 Not Found
```

**Solution:**
```bash
# Check $metadata for exact entity set name
GET /sap/opu/odata/sap/Z_PRODUCT_SRV/$metadata

# Find: <EntitySet Name="Products" .../>
# Use:  /Products (not /products)
```

### Error: "Property not found"

**Problem:**
```bash
?$filter=productid eq 1
Property 'productid' not found
```

**Solution:**
```bash
# Use exact case from metadata: ProductID
?$filter=ProductID eq 1
```

---

## RFC References

**URL Components:**
- **RFC 3986** - URI Generic Syntax (scheme, path structure)
- **RFC 1035** - DNS Specification (domain names)
- **RFC 7230** - HTTP/1.1 Message Syntax (HTTP handling)

**Key Quote from RFC 3986:**
```
The scheme and host are case-insensitive and therefore should be 
normalized to lowercase.

Other components are case-sensitive unless specifically defined 
otherwise by the scheme.
```

---

## Summary Table

| Component | Case Rule | Defined By | Reason |
|-----------|-----------|------------|--------|
| Scheme | Insensitive | RFC 3986 | Protocol identifier |
| Domain | Insensitive | RFC 1035 (DNS) | Cross-system compatibility |
| Port | Insensitive | TCP/IP | Numeric value |
| Path | **Sensitive** | Server implementation | Resource mapping |
| Query | **Sensitive** | Application logic | Data parameters |

---

## The Key Insight

**Protocol & Network Layer** (scheme, domain) = Standards-based, case-insensitive

**Application Layer** (path, query) = Application-controlled, case-sensitive

This is why you can type `HTTPS://MYSAP.COM` but must use exact case for `/sap/opu/odata/Products`!

---

## Testing Case Sensitivity

### DNS Testing

```bash
# All equivalent for finding the server
curl https://sap.mycompany.com/$metadata
curl https://SAP.MYCOMPANY.COM/$metadata
curl https://SaP.MyCoMpAnY.cOm/$metadata

# All resolve to same IP, then make same request
```

### Path Must Match Exactly

```javascript
// ✅ Correct - matches service binding
const serviceUrl = "/sap/opu/odata/sap/Z_PRODUCT_SRV/";

// ❌ Wrong - case mismatch
const serviceUrl = "/SAP/OPU/ODATA/SAP/Z_PRODUCT_SRV/";
//                  ^^^^ Server may reject this
```

### Entity Sets Must Match Metadata

```abap
// In Service Definition:
define service Z_PRODUCT_SRV {
  expose ZC_PRODUCT as Products;  // "Products" with capital P
}

// In JavaScript:
oModel.read("/Products", { ... });  // Must match exactly
//            ^^^^^^^^ Not /products or /PRODUCTS
```

---

## Golden Rule

**Always use the exact case as defined in the $metadata document.**

**When in doubt, check the metadata!**

---

**Last Updated:** 2025-02-07  
**Version:** 1.0
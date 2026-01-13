# ABAP Technology Anki Decks

Flashcard decks for learning modern SAP ABAP syntax through spaced repetition.

## Available Decks

| Deck | Cards | Topics |
|------|-------|--------|
| 01-modern-abap-syntax.txt | 29 | Inline declarations, VALUE/CORRESPONDING/NEW operators, string templates, REDUCE, FILTER |
| 02-cds-views.txt | 35 | CDS entity views, associations, annotations, parameters, analytics |
| 03-rap-framework.txt | 34 | Behavior definitions, validations, determinations, actions, EML |
| 04-hana-optimization.txt | 21 | Code pushdown, AMDP, aggregations, JOINs, performance |
| 05-abap-unit-testing.txt | 28 | Test classes, assertions, mocks, CDS test environment |
| 06-oo-design-patterns.txt | 25 | Singleton, Factory, Strategy, Observer, SOLID principles |
| 07-exception-handling.txt | 23 | Custom exceptions, TRY-CATCH, CLEANUP, exception chaining |
| 08-rest-api-integration.txt | 27 | HTTP handlers, JSON serialization, external APIs, BAPIs |

**Total: 222 flashcards**

## How to Import into Anki

### Method 1: Import Text Files

1. Open Anki
2. Click **File** > **Import**
3. Select the `.txt` file you want to import
4. Configure import settings:
   - **Type**: Basic
   - **Deck**: Create new or select existing
   - **Fields separated by**: Tab
   - **Allow HTML in fields**: Yes (for code formatting)
5. Click **Import**

### Method 2: Create Deck First

1. In Anki, click **Create Deck**
2. Name it (e.g., "ABAP Modern Syntax")
3. Then import the file into that deck

## Recommended Study Order

1. **01-modern-abap-syntax** - Foundation for all modern ABAP
2. **02-cds-views** - Core Data Services basics
3. **03-rap-framework** - RESTful Application Programming Model
4. **04-hana-optimization** - Performance optimization
5. **05-abap-unit-testing** - Testing fundamentals
6. **06-oo-design-patterns** - Advanced OO concepts
7. **07-exception-handling** - Error handling patterns
8. **08-rest-api-integration** - Integration patterns

## Study Tips

- **New cards per day**: Start with 10-15 new cards
- **Review regularly**: Anki's spaced repetition works best with daily reviews
- **Practice in ADT**: After reviewing, try the syntax in Eclipse ADT
- **Mark difficult cards**: Use Anki's flag feature for cards you struggle with

## Card Format

Each card follows this pattern:
- **Front**: Question or syntax prompt
- **Back**: Answer with code example

Example:
```
Front: How do you use the VALUE operator to create an internal table?
Back: DATA(lt_materials) = VALUE tt_matnr( ( '000000000000000001' ) ( '000000000000000002' ) ).
```

## Customization

Feel free to:
- Add your own cards based on your learning
- Modify existing cards
- Create sub-decks for specific topics
- Add tags for filtering

## Contributing

Found an error or want to add more cards? Please submit a PR to the main repository.

---

*Generated from ABAP_Technology code examples*

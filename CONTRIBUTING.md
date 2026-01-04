# Contributing to ABAP Technology

Thank you for your interest in contributing to the ABAP Technology learning repository! This document provides guidelines and instructions for contributing.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [How Can I Contribute?](#how-can-i-contribute)
  - [Reporting Issues](#reporting-issues)
  - [Suggesting Enhancements](#suggesting-enhancements)
  - [Adding Code Examples](#adding-code-examples)
  - [Improving Documentation](#improving-documentation)
- [Style Guidelines](#style-guidelines)
  - [ABAP Code Style](#abap-code-style)
  - [Markdown Style](#markdown-style)
  - [Commit Messages](#commit-messages)
- [Pull Request Process](#pull-request-process)

## Code of Conduct

This project adheres to a [Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code. Please report unacceptable behavior by opening an issue.

## How Can I Contribute?

### Reporting Issues

Before creating an issue, please check if a similar issue already exists. When creating an issue:

1. Use a clear and descriptive title
2. Describe the issue in detail
3. Include relevant context (file paths, code snippets, etc.)
4. Suggest a solution if you have one

### Suggesting Enhancements

Enhancement suggestions are welcome! Please:

1. Use a clear title prefixed with `[Enhancement]`
2. Provide a detailed description of the proposed enhancement
3. Explain why this enhancement would be useful
4. Include examples if applicable

### Adding Code Examples

When contributing new code examples:

1. **Follow the existing structure**: Place examples in appropriate directories under `learning-paths/code-examples/`
2. **Use placeholder naming**: Use `XXX` as a placeholder for customer/project-specific prefixes
3. **Show both patterns**: Include both BAD (legacy) and GOOD (modern) approaches where applicable
4. **Include comments**: Add clear comments explaining the code
5. **Test your code**: Ensure examples are syntactically correct and follow Clean ABAP principles

#### Code Example Template

```markdown
## Topic Name

### Description
Brief explanation of what this example demonstrates.

### Bad Practice (Legacy Approach)
```abap
" Show what NOT to do
```

### Good Practice (Modern Approach)
```abap
" Show the recommended approach
```

### Key Points
- Point 1
- Point 2
```

### Improving Documentation

Documentation improvements are always welcome:

- Fix typos and grammatical errors
- Clarify unclear explanations
- Add missing information
- Update outdated content
- Improve formatting and structure

## Style Guidelines

### ABAP Code Style

Follow these ABAP coding standards:

1. **Use modern ABAP syntax**
   - Inline declarations (`DATA(lv_var)`)
   - Constructor operators (`VALUE`, `NEW`, `COND`, `CORRESPONDING`)
   - String templates (`` `text { variable }` ``)

2. **Follow Clean ABAP principles**
   - Meaningful variable and method names
   - Small, focused methods
   - Proper exception handling
   - Use interfaces for abstraction

3. **Naming conventions**
   - Follow the guidelines in [NAMING_CONVENTIONS.md](NAMING_CONVENTIONS.md)
   - Use `XXX` as placeholder for customer namespace

4. **Formatting**
   - Use Pretty Printer settings consistently
   - Indent with 2 spaces
   - Keep lines under 120 characters

### Markdown Style

1. **Use proper heading hierarchy**: Start with `#` for document title, then `##`, `###`, etc.
2. **Use fenced code blocks**: Always specify the language (```abap, ```json, etc.)
3. **Add blank lines**: Between sections and before/after code blocks
4. **Use consistent list markers**: Use `-` for unordered lists
5. **Validate links**: Ensure all links are working

### Commit Messages

Write clear and meaningful commit messages:

```
<type>: <subject>

<body>
```

**Types:**
- `feat`: New feature or example
- `fix`: Bug fix or correction
- `docs`: Documentation changes
- `style`: Formatting changes
- `refactor`: Code restructuring
- `test`: Adding tests

**Examples:**
```
feat: Add CDS view example for sales order analysis

docs: Update RAP documentation with behavior pool details

fix: Correct syntax error in singleton pattern example
```

## Pull Request Process

1. **Fork the repository** and create your branch from the main branch
2. **Make your changes** following the style guidelines
3. **Test your changes** to ensure they don't break existing content
4. **Update documentation** if needed
5. **Submit a pull request** with:
   - Clear title describing the change
   - Description of what was changed and why
   - Reference to any related issues

### PR Checklist

Before submitting a PR, ensure:

- [ ] Code examples are syntactically correct
- [ ] Markdown passes linting (no errors)
- [ ] Links are valid
- [ ] Changes follow the style guidelines
- [ ] Commit messages are clear and descriptive
- [ ] Documentation is updated if needed

## Questions?

If you have questions about contributing, feel free to open an issue with the `[Question]` prefix.

Thank you for contributing to ABAP Technology!

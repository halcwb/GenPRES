# W1: Project Structure & Governance - Missing Items Analysis

- [W1: Project Structure \& Governance - Missing Items Analysis](#w1-project-structure--governance---missing-items-analysis)
  - [Current Repository State Assessment](#current-repository-state-assessment)
    - [✅ Present](#-present)
      - [Root Level Files](#root-level-files)
      - [Documentation Structure (`docs/`)](#documentation-structure-docs)
        - [Design History \& Architecture (`docs/mdr/design-history/`)](#design-history--architecture-docsmdrdesign-history)
        - [Requirements (`docs/mdr/requirements/`)](#requirements-docsmdrrequirements)
        - [Risk Management (`docs/mdr/risk-analysis/`)](#risk-management-docsmdrrisk-analysis)
        - [Validation \& Testing (`docs/mdr/validation/`)](#validation--testing-docsmdrvalidation)
        - [Interface Specifications (`docs/mdr/interface/`)](#interface-specifications-docsmdrinterface)
        - [Post-Market Surveillance (`docs/mdr/post-market/`)](#post-market-surveillance-docsmdrpost-market)
        - [Usability Engineering (`docs/mdr/usability/`)](#usability-engineering-docsmdrusability)
        - [Other Documentation](#other-documentation)
    - [❌ Missing Items by Category](#-missing-items-by-category)
  - [1. Community Health Files](#1-community-health-files)
    - [Missing](#missing)
  - [2. Repository Structure \& Documentation](#2-repository-structure--documentation)
    - [Present](#present)
    - [Missing](#missing-1)
  - [3. Development Workflow \& Quality Gates](#3-development-workflow--quality-gates)
    - [Missing](#missing-2)
  - [4. CI/CD Enhancements](#4-cicd-enhancements)
    - [Missing](#missing-3)
  - [5. Issue \& Project Management](#5-issue--project-management)
    - [Missing](#missing-4)
  - [6. Versioning \& Release Management](#6-versioning--release-management)
    - [Missing](#missing-5)
  - [7. Code Quality \& Testing](#7-code-quality--testing)
    - [Missing](#missing-6)
  - [8. Documentation Infrastructure](#8-documentation-infrastructure)
    - [Missing](#missing-7)
  - [9. Medical Device Specific (Critical for GenPRES)](#9-medical-device-specific-critical-for-genpres)
    - [Present](#present-1)
    - [Missing or Needs Enhancement](#missing-or-needs-enhancement)
    - [Recommendations](#recommendations)
  - [10. Community \& Communication](#10-community--communication)
    - [Missing](#missing-8)
  - [11. License \& Legal](#11-license--legal)
    - [Missing](#missing-9)
  - [12. Accessibility \& Internationalization](#12-accessibility--internationalization)
    - [Missing](#missing-10)
  - [Priority Recommendations for W1 Workshop](#priority-recommendations-for-w1-workshop)
    - [🔴 High Priority (Must Have)](#-high-priority-must-have)
    - [🟡 Medium Priority (Should Have)](#-medium-priority-should-have)
    - [🟢 Lower Priority (Nice to Have)](#-lower-priority-nice-to-have)
  - [Workshop Deliverables](#workshop-deliverables)
    - [Expected Outputs from W1](#expected-outputs-from-w1)
  - [Recommended Folder Layout](#recommended-folder-layout)
    - [Proposed Repository Structure](#proposed-repository-structure)
    - [Priority Implementation Order](#priority-implementation-order)
      - [Phase 1: Critical Foundation (Week 1)](#phase-1-critical-foundation-week-1)
      - [Phase 2: Documentation Structure (Week 2)](#phase-2-documentation-structure-week-2)
      - [Phase 3: MDR Enhancements (Week 3)](#phase-3-mdr-enhancements-week-3)
      - [Phase 4: Automation \& Quality (Ongoing)](#phase-4-automation--quality-ongoing)
    - [File Naming Conventions](#file-naming-conventions)
    - [Cross-References](#cross-references)

## Current Repository State Assessment

### ✅ Present

#### Root Level Files

- **CONTRIBUTING.md** - Exists but may need enhancement for MDR compliance
- **LICENSE** - Present
- **README.md** - Present with basic setup instructions
- **.github/workflows/build.yml** - Basic build workflow for Windows, macOS, and Ubuntu
- **.github/ISSUE_TEMPLATE/** - Bug report and feature request templates
- **.github/PULL_REQUEST_TEMPLATE.md** - Basic pull request template
- **.github/copilot-instructions.md** - AI coding assistant instructions
- **.github/instructions/** - F# coding and commit message standards

#### Documentation Structure (`docs/`)

##### Design History & Architecture (`docs/mdr/design-history/`)

- ✅ **architecture.md** - High-level system architecture (SAFE Stack)
- ✅ **change-log.md** - Design change history
- ✅ **genpres_stateless_proposal.md** - Stateless session design
- ✅ **mailbox-processor-design-proposal.md** - MailboxProcessor architecture
- ✅ **genpres_resource_requirements.md** - Resource/sheet specifications
- ✅ **informedica-genform-lib.md** - GenForm library design
- ✅ **informedica-genorder-lib.md** - GenOrder library design
- ✅ **domain_constrained_option_solver_architecture.md** - Solver architecture
- ✅ **order_value_logic.md** - Order value semantics
- ✅ **state-of-affairs.md** - Current implementation status
- ✅ **ui-wireframes.md** - UI design mockups

##### Requirements (`docs/mdr/requirements/`)

- ✅ **user-requirements.md** - User requirements (UR-001 through UR-XXX)
- ✅ **software-requirements.md** - Software requirements specification
- ✅ **chemo_specific_requirements.md** - Chemotherapy-specific requirements
- ✅ **informedica.genunits.lib.requirements.md** - Units library requirements
- ✅ **traceability-matrix.xlsx** - Requirements traceability
- ✅ **genpres_traceability_matrix.xlsx** - GenPRES traceability

##### Risk Management (`docs/mdr/risk-analysis/`)

- ✅ **risk-management-plan.md** - ISO 14971 risk management plan
- ✅ **risk-management-report.md** - Risk management report
- ✅ **hazard-analysis.xlsx** - Hazard identification and analysis
- ✅ **genpres_hazard_analysis.xlsx** - GenPRES-specific hazards
- ✅ **genpres_hazard_control.xlsx** - Hazard control measures
- ✅ **risk-control-table.xlsx** - Risk control implementation
- ✅ **hazard_analysis.md** - Hazard analysis documentation

##### Validation & Testing (`docs/mdr/validation/`)

- ✅ **test-strategy.md** - Testing strategy document
- ✅ **unit-test-report.md** - Unit test results
- ✅ **integration-test-report.md** - Integration test results
- ✅ **usability-validation-report.md** - Usability testing results

##### Interface Specifications (`docs/mdr/interface/`)

- ✅ **genpres_interface_specification.md** - GenPRES API specification
- ✅ **treatmentplan-interface-specification.md** - Treatment plan interface
- ✅ **treatmentplan-interface-specification-FHIR-IHE-revision.md** - FHIR/IHE revision
- ✅ **merged_fhir_specification.md** - Merged FHIR specifications

##### Post-Market Surveillance (`docs/mdr/post-market/`)

- ✅ **feedback-log.md** - User feedback tracking
- ✅ **known-issues.md** - Known issues log
- ✅ **update-plan.md** - Update and maintenance plan
- ✅ **genpres_protocol_draft.md** - Clinical protocol draft

##### Usability Engineering (`docs/mdr/usability/`)

- ✅ **user-profile.md** - User profiles and personas
- ✅ **critical-tasks.md** - Critical task analysis
- ✅ **formative-testing.md** - Formative usability testing
- ✅ **summative-testing.md** - Summative usability testing

##### Other Documentation

- ✅ **docs/scenarios/** - Clinical scenario examples (Newborn, Infant, Child, Teenager, Adult)
- ✅ **docs/code-reviews/** - Code review documentation
- ✅ **docs/literature/** - Literature references
- ✅ **docs/data-extraction/** - Data extraction documentation
- ✅ **docs/genpres-production-plan-2026-v3.md** - Production plan with workshops
- ✅ **docs/mdr/mdr-regulations.md** - MDR regulations overview

### ❌ Missing Items by Category

**Note:** Many MDR-related documents exist but are in draft/incomplete state. Items marked as "missing" below are either completely absent or need significant enhancement for production readiness.

---

## 1. Community Health Files

### Missing

- ❌ **CODE_OF_CONDUCT.md** - Essential for establishing community standards
  - Recommended: Use [Contributor Covenant](https://www.contributor-covenant.org/)
  - Critical for professional medical software project
  
- ❌ **SECURITY.md** - Required for responsible vulnerability disclosure
  - Security reporting process
  - Disclosure timeline
  - Supported versions
  - Critical for medical device software
  
- ❌ **SUPPORT.md** - Guidance for users seeking help
  - Where to ask questions
  - Community channels
  - Issue vs discussion guidelines
  
- ❌ **GOVERNANCE.md** - Decision-making process and project leadership
  - Maintainer responsibilities
  - Decision-making process
  - Commit rights policy
  - Release authority

---

## 2. Repository Structure & Documentation

### Present

- ✅ **docs/mdr/design-history/architecture.md** - Comprehensive SAFE Stack architecture
- ✅ **docs/mdr/design-history/change-log.md** - Design history file exists
- ✅ **docs/scenarios/** - Clinical scenarios documented (6 age groups)
- ✅ **docs/mdr/interface/** - Interface specifications (FHIR, IHE)
- ✅ **docs/code-reviews/** - Some code review documentation

### Missing

- ❌ **ARCHITECTURE.md** (at root level) - Quick reference for contributors
  - Should link to detailed `docs/mdr/design-history/architecture.md`
  - High-level component diagram at root for discoverability
  - Quick start architecture overview
  
- ❌ **ROADMAP.md** - Public roadmap for transparency
  - Planned features beyond production plan
  - Release timeline and milestones
  - Community input process
  - Currently have genpres-production-plan-2026-v3.md but not user-facing ROADMAP
  
- ❌ **CHANGELOG.md** - Structured release notes at root
  - Alternative: Use GitHub Releases
  - Follow [Keep a Changelog](https://keepachangelog.com/) format
  - Currently have change-log.md in design-history (developer-focused) but need user-facing CHANGELOG
  
- ❌ **AUTHORS.md** or **CONTRIBUTORS.md** - Recognition file
  - List of contributors
  - Attribution for third-party code
  
- ❌ **docs/ADR/** - Architecture Decision Records directory
  - Template-based decision records (Why we chose X over Y)
  - Critical for MDR traceability
  - Separate from design-history (which is "what" not "why")
  
- ❌ **docs/api/** - Developer-focused API documentation
  - Auto-generated from code (FSharp.Formatting)
  - Integration guides for EHR developers
  - Currently have interface specs but need code-level API reference
  
- ❌ **MAINTAINERS.md** - List of maintainers and their responsibilities
  - Maintainer roster
  - Areas of responsibility
  - Contact information

---

## 3. Development Workflow & Quality Gates

### Missing

- ❌ **Pre-commit hooks** - Automated formatting/linting
  - **Fantomas** for F# formatting
  - **FSharpLint** for code quality
  - **Commit message validation** (conventional commits)
  - Implementation: Use [pre-commit](https://pre-commit.com/) or [Husky](https://typicode.github.io/husky/)
  
- ❌ **Branch protection rules** - Documentation and enforcement
  - Require PR reviews
  - Require status checks
  - Require linear history
  - Restrict force pushes
  
- ❌ **Enhanced PR template** - Should include checklist for:
  - [ ] Tests added/updated
  - [ ] Documentation updated
  - [ ] Breaking changes noted
  - [ ] MDR compliance consideration
  - [ ] Security implications reviewed
  
- ❌ **.editorconfig** - Consistent coding style across IDEs
  - Indentation rules
  - Line endings
  - Charset
  - Trim trailing whitespace
  
- ❌ **Formatting verification in CI** - Fantomas check
  - Fail build on formatting violations
  - Auto-format option for PRs
  
- ❌ **Linting in CI** - FSharpLint integration
  - Code quality checks
  - Code smell detection
  - Configurable rules

---

## 4. CI/CD Enhancements

**Current state:** Basic build workflow exists for multiple platforms

### Missing

- ❌ **Test coverage reporting** - Integration with Codecov or Coveralls
  - Minimum coverage thresholds
  - Coverage trends
  - PR coverage diff
  
- ❌ **Automated dependency updates** - Dependabot or Renovate
  - Security vulnerability alerts
  - Automated PR for updates
  - Grouped updates
  
- ❌ **Security scanning** - SAST tools
  - GitHub Code Scanning (CodeQL)
  - Dependency vulnerability scanning
  - Secret detection
  
- ❌ **Performance benchmarking** - BenchmarkDotNet in CI
  - Performance regression detection
  - Baseline comparisons
  - Critical for solver performance
  
- ❌ **Release automation** - Semantic versioning + automated releases
  - Automated version bumping
  - Changelog generation
  - GitHub Releases creation
  
- ❌ **Docker image publishing** - Automated container builds
  - Multi-arch images
  - Version tagging
  - Registry deployment
  
- ❌ **Documentation deployment** - Auto-deploy docs to GitHub Pages
  - FSharp.Formatting or similar
  - API documentation
  - User guides
  
- ❌ **Multi-stage CI** - Separate jobs for build/test/lint/security
  - Parallel execution
  - Fail fast
  - Clear job separation

---

## 5. Issue & Project Management

**Current state:** Basic issue templates exist

### Missing

- ❌ **Issue labels** - Standardized label taxonomy
  - **Priority:** critical, high, medium, low
  - **Type:** bug, feature, documentation, question
  - **Area:** solver, ui, api, domain, etc.
  - **Status:** needs-triage, in-progress, blocked
  - **Medical:** MDR-relevant, clinical-safety
  
- ❌ **Project boards** - For sprint/release planning
  - Kanban-style boards
  - Sprint planning
  - Release tracking
  
- ❌ **Milestone definitions** - Clear release planning
  - Version milestones
  - Feature milestones
  - Due dates
  
- ❌ **Issue triage process** - Documentation
  - Triage schedule
  - Triage criteria
  - Escalation process
  
- ❌ **Discussion forums** - GitHub Discussions enabled
  - Q&A category
  - Ideas/feature requests
  - General discussion

---

## 6. Versioning & Release Management

### Missing

- ❌ **Semantic versioning policy** - Clear versioning scheme
  - Major.Minor.Patch rules
  - Breaking change policy
  - Pre-release versioning
  
- ❌ **Release process documentation** - Step-by-step release guide
  - Release checklist
  - Testing requirements
  - Deployment steps
  
- ❌ **Deprecation policy** - How to handle breaking changes
  - Deprecation timeline
  - Migration guides
  - Sunset policy
  
- ❌ **MinVer or GitVersion** - Automated version from git tags
  - Git tag conventions
  - CI integration
  - Package versioning
  
- ❌ **Release notes template** - Structured changelog format
  - Breaking changes section
  - New features section
  - Bug fixes section
  - Dependencies updated
  
- ❌ **Package publishing** - NuGet packages for libraries
  - Automated publishing
  - Package metadata
  - Symbol packages

---

## 7. Code Quality & Testing

### Missing

- ❌ **Test coverage requirements** - Minimum coverage thresholds
  - Overall coverage target (e.g., 80%)
  - Critical path coverage (e.g., 95%)
  - PR coverage diff requirements
  
- ❌ **Property-based testing examples** - FsCheck patterns
  - Common property patterns
  - Generator examples
  - Integration with Expecto
  
- ❌ **Integration test suite** - End-to-end scenarios
  - Full workflow tests
  - EHR integration tests
  - Performance tests
  
- ❌ **Performance test suite** - BenchmarkDotNet baselines
  - Solver benchmarks
  - Unit conversion benchmarks
  - Baseline tracking
  
- ❌ **Mutation testing** - Stryker.NET for test quality
  - Mutation score thresholds
  - CI integration
  - Critical path focus
  
- ❌ **Code quality badges** - In README for transparency
  - Build status
  - Test coverage
  - Code quality score
  - Security score

---

## 8. Documentation Infrastructure

### Missing

- ❌ **API documentation generation** - FSharp.Formatting or similar
  - XML doc comments
  - Auto-generated reference docs
  - Code examples
  
- ❌ **User documentation** - Separate from developer docs
  - User guides
  - Tutorials
  - Clinical workflows
  
- ❌ **Contributing guide sections for:**
  - First-time contributors
  - Good first issues
  - Development workflow (branch strategy)
  - Code review process
  - Testing requirements
  
- ❌ **MDR compliance documentation structure** - As noted in CONTRIBUTING.md
  - Requirements traceability
  - Risk management
  - Verification and validation
  
- ❌ **Clinical safety documentation** - For medical device certification
  - Clinical evaluation
  - Safety analysis
  - Intended use

---

## 9. Medical Device Specific (Critical for GenPRES)

### Present

- ✅ **docs/mdr/risk-analysis/risk-management-plan.md** - ISO 14971 compliant plan
- ✅ **docs/mdr/risk-analysis/risk-management-report.md** - Risk management report
- ✅ **docs/mdr/risk-analysis/hazard-analysis.xlsx** - Hazard identification (multiple files)
- ✅ **docs/mdr/risk-analysis/risk-control-table.xlsx** - Risk control measures
- ✅ **docs/mdr/requirements/traceability-matrix.xlsx** - Requirements traceability (2 versions)
- ✅ **docs/mdr/post-market/** - Post-market surveillance structure
- ✅ **docs/mdr/validation/** - Validation documentation structure
- ✅ **docs/mdr/usability/** - Usability engineering files
- ✅ **docs/mdr/requirements/user-requirements.md** - User requirements (UR-XXX)
- ✅ **docs/mdr/requirements/software-requirements.md** - Software requirements
- ✅ **docs/mdr/requirements/chemo_specific_requirements.md** - Specialized requirements

### Missing or Needs Enhancement

- ⚠️ **Software Bill of Materials (SBOM)** - Needs automation
  - Currently manual tracking
  - Need CycloneDX or SPDX format generation
  - CI/CD integration for automatic SBOM generation
  - License information consolidated

**Proposal**: use the microsoft .net tool for SBOM generation: <https://github.com/microsoft/sbom-tool>
  
- ⚠️ **Traceability matrix** - Needs consolidation and automation
  - Two Excel files exist but need unified approach
  - Automated traceability from requirements → tests → code
  - Gap analysis reporting
  - Integration with test frameworks
  
- ⚠️ **Clinical evaluation documentation** - Structure incomplete
  - Literature review framework needed
  - Clinical data collection process
  - Benefit-risk analysis template
  - Post-market clinical follow-up (PMCF) plan
  
- ❌ **Adverse event reporting** - Process documentation missing
  - Event classification criteria
  - Reporting timeline and procedures
  - Investigation workflow
  - Competent authority notification process
  
- ⚠️ **Change control process** - Needs formalization
  - Change request template needed
  - Impact assessment framework
  - Approval workflow documentation
  - Verification requirements per change type
  - Currently informal process

### Recommendations

1. **Automate SBOM generation** - Priority: HIGH
   - Use tools like `dotnet list package` + formatting
   - Include in CI/CD pipeline
   - Track license compatibility

2. **Enhance traceability matrix** - Priority: HIGH
   - Move from Excel to database or structured format
   - Auto-link test results to requirements
   - Generate compliance reports

3. **Formalize change control** - Priority: MEDIUM
   - Create GitHub issue template for change requests
   - Define approval workflow in GOVERNANCE.md
   - Link to risk management re-assessment

4. **Clinical evaluation framework** - Priority: MEDIUM
   - Template for literature search
   - Clinical data logging structure
   - Periodic review schedule

5. **Adverse event process** - Priority: HIGH (regulatory requirement)
   - Define vigilance process
   - Training documentation
   - Contact points for reporting

---

## 10. Community & Communication

### Missing

- ❌ **Communication channels** - Discord/Slack/Matrix
  - Real-time chat
  - Developer channel
  - User support channel
  
- ❌ **Meeting notes** - If regular contributor meetings occur
  - Meeting schedule
  - Notes repository
  - Action items tracking
  
- ❌ **Developer mailing list** - For announcements
  - Release announcements
  - Breaking changes
  - Security advisories
  
- ❌ **Social media presence** - Twitter/Mastodon for updates
  - Project updates
  - Community highlights
  - Event announcements
  
- ❌ **Newsletter** - For stakeholders
  - Monthly updates
  - Feature highlights
  - Community news
  
- ❌ **Office hours** - Regular Q&A sessions
  - Scheduled sessions
  - Video calls
  - Open forum

---

## 11. License & Legal

**Current:** LICENSE exists

### Missing

- ❌ **CLA (Contributor License Agreement) or DCO (Developer Certificate of Origin)** - For IP clarity
  - CLA bot integration
  - DCO sign-off requirements
  - Legal clarity
  
- ❌ **NOTICE file** - Third-party attributions
  - Third-party licenses
  - Copyright notices
  - Patent notices
  
- ❌ **License compatibility check** - For dependencies
  - Automated scanning
  - Compatibility matrix
  - Policy enforcement
  
- ❌ **Export control statement** - If applicable for medical software
  - Export classification
  - Restrictions
  - Compliance statement
  
- ❌ **GDPR/PHI compliance statement** - For data handling
  - Data processing
  - Privacy policy
  - PHI handling guidelines

---

## 12. Accessibility & Internationalization

### Missing

- ❌ **Accessibility guidelines** - WCAG compliance for UI
  - WCAG 2.1 Level AA target
  - Keyboard navigation
  - Screen reader support
  - Color contrast
  
- ❌ **Internationalization (i18n) strategy** - Multi-language support
  - Translation framework
  - Supported languages
  - Resource management
  
- ❌ **Localization (l10n) process** - Translation workflow
  - Translation memory
  - Review process
  - Update workflow

---

## Priority Recommendations for W1 Workshop

### 🔴 High Priority (Must Have)

1. **CODE_OF_CONDUCT.md** - Use Contributor Covenant
2. **SECURITY.md** - Critical for medical software
3. **Pre-commit hooks** - Fantomas + commit message validation
4. **CI enhancements** - Add formatting/linting checks
5. **.editorconfig** - Consistent formatting
6. **Semantic versioning** - MinVer/GitVersion setup
7. **SBOM generation** - For medical device compliance
8. **Branch protection rules** - Documented and enforced

### 🟡 Medium Priority (Should Have)

9. **GOVERNANCE.md** - Decision-making transparency
10. **Enhanced PR template** - With MDR checklist
11. **Issue labels** - Standardized taxonomy
12. **Test coverage reporting** - Codecov integration
13. **Dependabot** - Automated dependency updates
14. **ARCHITECTURE.md** - System overview
15. **ADR directory** - Architecture decisions

### 🟢 Lower Priority (Nice to Have)

16. **ROADMAP.md** - Public planning
17. **GitHub Discussions** - Community forum
18. **Documentation site** - GitHub Pages
19. **CLA/DCO** - IP management
20. **Community channels** - Discord/Slack

---

## Workshop Deliverables

### Expected Outputs from W1

1. **Decision documents**
   - Monorepo vs multi-repo structure
   - CI/CD platform choice (GitHub Actions confirmed)
   - Documentation tooling
   - Code quality tools configuration

2. **Work packages created**
   - **WP-01:** Repository setup and structure
   - **WP-02:** CI/CD pipeline implementation
   - **WP-03:** Documentation framework setup
   - **WP-04:** Community health files
   - **WP-05:** Code quality automation

3. **Templates and configurations**
   - Enhanced PR template with MDR checklist
   - Issue label taxonomy
   - .editorconfig file
   - Pre-commit hook configuration
   - Fantomas configuration
   - FSharpLint rules

4. **Documentation**
   - CONTRIBUTING.md enhancements
   - CODE_OF_CONDUCT.md
   - SECURITY.md
   - GOVERNANCE.md
   - Branch protection policy

5. **Timeline and assignment**
   - Work package effort estimates
   - Developer skill matching
   - Dependency mapping
   - Implementation schedule

---

## Recommended Folder Layout

### Proposed Repository Structure

Below is the recommended folder structure showing where missing documents should be placed and references to existing documentation. Items marked with ❌ are missing, ⚠️ need enhancement, and ✅ already exist.

```text
GenPRES2/
│
├── 📄 README.md                              ✅ Exists - Entry point
├── 📄 LICENSE                                ✅ Exists - Open source license
├── 📄 CONTRIBUTING.md                        ⚠️ Enhance with MDR checklist
├── 📄 CODE_OF_CONDUCT.md                     ❌ ADD - Use Contributor Covenant
├── 📄 SECURITY.md                            ❌ ADD - Vulnerability disclosure
├── 📄 SUPPORT.md                             ❌ ADD - Getting help guide
├── 📄 GOVERNANCE.md                          ❌ ADD - Project governance model
├── 📄 MAINTAINERS.md                         ❌ ADD - Maintainer roster
├── 📄 ARCHITECTURE.md                        ❌ ADD - Quick architecture reference
│                                                      → Links to docs/mdr/design-history/architecture.md
├── 📄 ROADMAP.md                             ❌ ADD - Public roadmap
│                                                      → Links to docs/roadmap/genpres-production-plan-2026-v3.md
├── 📄 CHANGELOG.md                           ❌ ADD - User-facing release notes
│                                                      → Separate from docs/mdr/design-history/change-log.md
├── 📄 AUTHORS.md                             ❌ ADD - Contributors list
├── 📄 .editorconfig                          ❌ ADD - Editor configuration
├── 📄 .gitattributes                         ✅ Check if exists
├── 📄 paket.dependencies                     ✅ Exists
├── 📄 GenPres.sln                           ✅ Exists
│
├── 📁 .github/                               ✅ Exists
│   ├── 📄 copilot-instructions.md            ✅ Exists
│   ├── 📄 PULL_REQUEST_TEMPLATE.md           ⚠️ Enhance with MDR checklist
│   │
│   ├── 📁 workflows/                         ✅ Exists
│   │   ├── 📄 build.yml                      ✅ Exists - Basic build
│   │   ├── 📄 test.yml                       ❌ ADD - Separate test workflow
│   │   ├── 📄 format-check.yml               ❌ ADD - Fantomas check
│   │   ├── 📄 lint.yml                       ❌ ADD - FSharpLint check
│   │   ├── 📄 coverage.yml                   ❌ ADD - Test coverage
│   │   ├── 📄 security-scan.yml              ❌ ADD - CodeQL/dependency scan
│   │   ├── 📄 sbom-generate.yml              ❌ ADD - Auto-generate SBOM
│   │   └── 📄 release.yml                    ❌ ADD - Automated releases
│   │
│   ├── 📁 ISSUE_TEMPLATE/                    ✅ Exists
│   │   ├── 📄 bug_report.md                  ✅ Exists
│   │   ├── 📄 feature_request.md             ✅ Exists
│   │   ├── 📄 change_request.md              ❌ ADD - MDR change control
│   │   ├── 📄 adverse_event.md               ❌ ADD - Adverse event reporting
│   │   └── 📄 config.yml                     ❌ ADD - Issue routing config
│   │
│   └── 📁 instructions/                      ✅ Exists
│       ├── 📄 fsharp-coding.instructions.md  ✅ Exists
│       ├── 📄 commit-message.instructions.md ✅ Exists
│       └── 📄 mdr-compliance.instructions.md ❌ ADD - MDR-specific guidelines
│
├── 📁 docs/                                  ✅ Exists
│   │
│   ├── 📁 roadmap/                           ✅ Exists - Strategic planning & workshops
│   │   ├── 📄 genpres-production-plan-2026-v3.md ✅ Exists - Production roadmap
│   │   ├── 📄 w1-project-structure-and-governance.md ✅ This document
│   │   └── 📄 w2-through-w12.md              ❌ ADD - Future workshop docs
│   │
│   ├── 📁 adr/                               ❌ ADD - Architecture Decision Records
│   │   ├── 📄 0000-use-adr.md                ❌ ADD - ADR about using ADRs
│   │   ├── 📄 0001-safe-stack.md             ❌ ADD - Why SAFE Stack
│   │   ├── 📄 0002-bigrationals.md           ❌ ADD - Why BigRational for calculations
│   │   ├── 📄 0003-stateless-sessions.md     ❌ ADD - Stateless design decision
│   │   ├── 📄 0004-mailbox-processor.md      ❌ ADD - MailboxProcessor choice
│   │   └── 📄 template.md                    ❌ ADD - ADR template
│   │
│   ├── 📁 api/                               ❌ ADD - Developer API documentation
│   │   ├── 📄 index.md                       ❌ ADD - API overview
│   │   ├── 📄 getting-started.md             ❌ ADD - Integration guide
│   │   ├── 📄 authentication.md              ❌ ADD - Auth/session handling
│   │   ├── 📄 endpoints.md                   ❌ ADD - Endpoint reference
│   │   └── 📄 fsharp-interop.md              ❌ ADD - F# library usage
│   │
│   ├── 📁 guides/                            ❌ ADD - User and developer guides
│   │   ├── 📄 user-guide.md                  ❌ ADD - End user guide
│   │   ├── 📄 developer-guide.md             ❌ ADD - Developer onboarding
│   │   ├── 📄 contributor-guide.md           ❌ ADD - Contributing workflow
│   │   └── 📄 deployment-guide.md            ❌ ADD - Deployment instructions
│   │
│   ├── 📁 scenarios/                         ✅ Exists
│   │   ├── 📄 Newborn.md                     ✅ Exists
│   │   ├── 📄 Infant.md                      ✅ Exists
│   │   ├── 📄 Child.md                       ✅ Exists
│   │   ├── 📄 Teenager.md                    ✅ Exists
│   │   ├── 📄 Adult.md                       ✅ Exists
│   │   └── 📄 Toddler.md                     ✅ Exists
│   │
│   ├── 📁 code-reviews/                      ✅ Exists
│   │   ├── 📄 genpres-review.md              ✅ Exists
│   │   ├── 📄 parseTextItem-refactoring.md   ✅ Exists
│   │   └── 📄 solver-memoization.md          ✅ Exists
│   │
│   ├── 📁 literature/                        ✅ Exists
│   ├── 📁 data-extraction/                   ✅ Exists
│   │
│   └── 📁 mdr/                               ✅ Exists - Medical Device Regulation docs
│       ├── 📄 mdr-regulations.md             ✅ Exists
│       │
│       ├── 📁 design-history/                ✅ Exists
│       │   ├── 📄 architecture.md            ✅ Exists - Detailed architecture
│       │   ├── 📄 change-log.md              ✅ Exists - Design changes (developer)
│       │   ├── 📄 genpres_stateless_proposal.md ✅ Exists
│       │   ├── 📄 mailbox-processor-design-proposal.md ✅ Exists
│       │   ├── 📄 genpres_resource_requirements.md ✅ Exists
│       │   ├── 📄 informedica-genform-lib.md ✅ Exists
│       │   ├── 📄 informedica-genorder-lib.md ✅ Exists
│       │   ├── 📄 domain_constrained_option_solver_architecture.md ✅ Exists
│       │   ├── 📄 order_value_logic.md       ✅ Exists
│       │   ├── 📄 state-of-affairs.md        ✅ Exists
│       │   └── 📄 ui-wireframes.md           ✅ Exists
│       │
│       ├── 📁 requirements/                  ✅ Exists
│       │   ├── 📄 user-requirements.md       ✅ Exists (UR-XXX)
│       │   ├── 📄 software-requirements.md   ✅ Exists (SR-XXX)
│       │   ├── 📄 chemo_specific_requirements.md ✅ Exists
│       │   ├── 📄 informedica.genunits.lib.requirements.md ✅ Exists
│       │   ├── 📄 traceability-matrix.xlsx   ✅ Exists
│       │   ├── 📄 genpres_traceability_matrix.xlsx ✅ Exists
│       │   └── 📄 traceability-automated.json ❌ ADD - Automated traceability
│       │
│       ├── 📁 risk-analysis/                 ✅ Exists
│       │   ├── 📄 risk-management-plan.md    ✅ Exists (ISO 14971)
│       │   ├── 📄 risk-management-report.md  ✅ Exists
│       │   ├── 📄 hazard-analysis.xlsx       ✅ Exists
│       │   ├── 📄 genpres_hazard_analysis.xlsx ✅ Exists
│       │   ├── 📄 genpres_hazard_control.xlsx ✅ Exists
│       │   ├── 📄 risk-control-table.xlsx    ✅ Exists
│       │   └── 📄 hazard_analysis.md         ✅ Exists
│       │
│       ├── 📁 validation/                    ✅ Exists
│       │   ├── 📄 test-strategy.md           ✅ Exists
│       │   ├── 📄 unit-test-report.md        ✅ Exists
│       │   ├── 📄 integration-test-report.md ✅ Exists
│       │   ├── 📄 usability-validation-report.md ✅ Exists
│       │   ├── 📄 performance-validation.md  ❌ ADD - Performance benchmarks
│       │   └── 📄 security-validation.md     ❌ ADD - Security testing
│       │
│       ├── 📁 interface/                     ✅ Exists
│       │   ├── 📄 genpres_interface_specification.md ✅ Exists
│       │   ├── 📄 treatmentplan-interface-specification.md ✅ Exists
│       │   ├── 📄 treatmentplan-interface-specification-FHIR-IHE-revision.md ✅ Exists
│       │   └── 📄 merged_fhir_specification.md ✅ Exists
│       │
│       ├── 📁 post-market/                   ✅ Exists
│       │   ├── 📄 feedback-log.md            ✅ Exists
│       │   ├── 📄 known-issues.md            ✅ Exists
│       │   ├── 📄 update-plan.md             ✅ Exists
│       │   ├── 📄 genpres_protocol_draft.md  ✅ Exists
│       │   ├── 📄 adverse-event-procedure.md ❌ ADD - Adverse event process
│       │   ├── 📄 vigilance-report-template.md ❌ ADD - Vigilance reporting
│       │   └── 📄 post-market-clinical-followup.md ❌ ADD - PMCF plan
│       │
│       ├── 📁 usability/                     ✅ Exists
│       │   ├── 📄 user-profile.md            ✅ Exists
│       │   ├── 📄 critical-tasks.md          ✅ Exists
│       │   ├── 📄 formative-testing.md       ✅ Exists
│       │   └── 📄 summative-testing.md       ✅ Exists
│       │
│       ├── 📁 clinical-evaluation/           ❌ ADD - Clinical evaluation framework
│       │   ├── 📄 clinical-evaluation-plan.md ❌ ADD - Evaluation plan
│       │   ├── 📄 literature-review.md       ❌ ADD - Literature search
│       │   ├── 📄 clinical-data.md           ❌ ADD - Clinical data collection
│       │   └── 📄 benefit-risk-analysis.md   ❌ ADD - Benefit-risk assessment
│       │
│       └── 📁 change-control/                ❌ ADD - Change control process
│           ├── 📄 change-control-procedure.md ❌ ADD - Change process
│           ├── 📄 change-request-template.md ❌ ADD - Change request form
│           └── 📄 change-log.md              ❌ ADD - Change tracking log
│
├── 📁 src/                                   ✅ Exists
│   ├── 📁 Client/                            ✅ Exists
│   ├── 📁 Server/                            ✅ Exists
│   ├── 📁 Shared/                            ✅ Exists
│   └── 📁 Informedica.*.Lib/                 ✅ Exists (multiple libraries)
│
├── 📁 tests/                                 ✅ Exists
│   ├── 📁 Informedica.*.Tests/               ✅ Exists (multiple test projects)
│   └── 📄 coverage-report/                   ❌ ADD - Coverage output directory
│
├── 📁 benchmarks/                            ⚠️ Check - Performance benchmarks
│   └── 📁 Informedica.GenSolver.Benchmarks/  ❌ ADD - Solver benchmarks
│
├── 📁 artifacts/                             ❌ ADD - Build artifacts
│   ├── 📄 sbom.json                          ❌ ADD - Auto-generated SBOM
│   ├── 📄 sbom.xml                           ❌ ADD - SBOM (SPDX format)
│   └── 📄 licenses.txt                       ❌ ADD - License compilation
│
├── 📁 scripts/                               ⚠️ Check if exists
│   ├── 📄 setup-dev-env.sh                   ❌ ADD - Dev environment setup
│   ├── 📄 pre-commit-hook.sh                 ❌ ADD - Pre-commit checks
│   ├── 📄 generate-sbom.sh                   ❌ ADD - SBOM generation
│   └── 📄 run-all-tests.sh                   ❌ ADD - Test runner
│
└── 📁 tools/                                 ❌ ADD - Development tools
    ├── 📄 fantomas-config.json               ❌ ADD - Formatting config
    ├── 📄 fsharplint.json                    ❌ ADD - Linting config
    └── 📄 traceability-checker.fsx           ❌ ADD - Traceability automation
```

### Priority Implementation Order

#### Phase 1: Critical Foundation (Week 1)

```text
Root Level:
  ❌ CODE_OF_CONDUCT.md
  ❌ SECURITY.md
  ❌ GOVERNANCE.md
  ❌ .editorconfig

.github/:
  ⚠️ PULL_REQUEST_TEMPLATE.md (enhance)
  ❌ workflows/format-check.yml
  ❌ workflows/lint.yml
  ❌ ISSUE_TEMPLATE/change_request.md
```

#### Phase 2: Documentation Structure (Week 2)

```text
Root Level:
  ❌ ARCHITECTURE.md
  ❌ MAINTAINERS.md
  ❌ SUPPORT.md

docs/:
  ❌ adr/ (entire directory + first ADRs)
  ❌ api/index.md
  ❌ guides/contributor-guide.md
```

#### Phase 3: MDR Enhancements (Week 3)

```text
docs/mdr/:
  ❌ clinical-evaluation/ (entire directory)
  ❌ change-control/ (entire directory)
  ❌ post-market/adverse-event-procedure.md
  ❌ requirements/traceability-automated.json
```

#### Phase 4: Automation & Quality (Ongoing)

```text
.github/workflows/:
  ❌ test.yml
  ❌ coverage.yml
  ❌ security-scan.yml
  ❌ sbom-generate.yml
  ❌ release.yml

artifacts/:
  ❌ sbom.json (auto-generated)
  
scripts/:
  ❌ pre-commit-hook.sh
  ❌ generate-sbom.sh
```

### File Naming Conventions

- **Root community files**: UPPERCASE.md (e.g., `README.md`, `SECURITY.md`)
- **Documentation**: lowercase-with-hyphens.md (e.g., `user-guide.md`)
- **MDR documents**: lowercase-with-hyphens.md (e.g., `risk-management-plan.md`)
- **ADRs**: `NNNN-short-title.md` (e.g., `0001-use-safe-stack.md`)
- **Configuration**: lowercase or project-standard (e.g., `.editorconfig`, `fantomas-config.json`)

### Cross-References

Many missing root-level files should link to detailed MDR documentation:

- `ARCHITECTURE.md` → `docs/mdr/design-history/architecture.md`
- `ROADMAP.md` → `docs/roadmap/genpres-production-plan-2026-v3.md`
- `CHANGELOG.md` (user-facing) ≠ `docs/mdr/design-history/change-log.md` (developer)
- ADRs should reference design history files for detailed technical decisions


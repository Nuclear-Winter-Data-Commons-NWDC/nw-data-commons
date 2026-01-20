# Substantive Decision Logging Protocol

**Version:** 1.0
**Last Updated:** 2026-01-16
**Purpose:** Replicable protocol for tracking substantive methodological decisions in research projects using LLM assistants

---

## Overview

This protocol provides a systematic approach to documenting substantive methodological changes during research projects. It ensures PRISMA-S compliance, reproducibility, and transparent audit trails for peer review.

**Key Benefits:**
- Complete provenance tracking of all methodological decisions
- Automated version control for research protocols
- Evidence-based justification for protocol deviations
- Support for systematic review reporting standards (PRISMA-S, BIBLIO)
- Integration with LLM assistants (Claude Code, ChatGPT, etc.)

---

## Core Concepts

### What is a "Substantive" Decision?

A substantive decision is any methodological change that:

1. **Affects data collection scope or methods:**
   - Changes to inclusion/exclusion criteria
   - Addition or removal of search terms
   - Changes to database queries
   - Modifications to data extraction fields

2. **Impacts reproducibility or validity:**
   - Changes to validation thresholds
   - Modifications to analysis methods
   - Changes to quality assurance procedures
   - Adjustments to sampling/selection

3. **Represents protocol deviations:**
   - Solutions to unforeseen problems
   - Adaptations based on empirical findings
   - Methodological refinements from data
   - Changes from technical constraints

**Non-substantive changes** (not logged):
- Typo corrections
- Formatting improvements
- Clarifications without meaning changes
- Addition of examples/elaborations
- Reorganization without content changes

---

## Directory Structure

Create the following structure in your repository (if not already present):

```
protocol/
├── README.md                    # Protocol system documentation (this file adapted)
├── protocol.md                  # Living protocol (CURRENT VERSION)
├── protocol_v1.0.md            # Locked baseline (NEVER MODIFIED)
├── decision_log.json           # Machine-readable decision history
├── CHANGELOG.md                # Human-readable change summary
├── DEC-XXX_*.md                # Individual decision documents (optional but recommended)
└── versions/                   # Archived protocol versions
    ├── protocol_v1.1.md
    ├── protocol_v1.2.md
    └── ...
```

---

## File Specifications

### 1. `protocol.md` - Living Protocol

**Purpose:** The current, active version of your research protocol.

**Requirements:**
- Must include version number in header (e.g., `**Version:** 1.5`)
- Must include last updated date (e.g., `**Last Updated:** 2026-01-16`)
- Updated whenever substantive changes are made
- Version number increments with each logged decision
- Serves as working reference for all research activities

**Example header:**
```markdown
# Research Protocol

**Version:** 1.5
**Last Updated:** 2026-01-16
**Project:** [Your Project Name]
**Protocol Baseline:** 1.0 (2025-12-09)

## [Your protocol content...]
```

---

### 2. `protocol_v1.0.md` - Locked Baseline

**Purpose:** Original protocol snapshot that is NEVER modified.

**Requirements:**
- Created at project initialization
- Exact copy of initial `protocol.md`
- Serves as comparison baseline for tracking evolution
- Reference point for deviation analysis

**Action:** When implementing this protocol, copy your current `protocol.md` to `protocol_v1.0.md` and never modify the baseline.

---

### 3. `decision_log.json` - Machine-Readable History

**Purpose:** Structured record of all substantive decisions in queryable JSON format.

**Schema:**

```json
{
  "metadata": {
    "project": "Your Project Name",
    "protocol_baseline_version": "1.0",
    "protocol_current_version": "1.5",
    "created": "2025-12-09T14:10:00Z",
    "last_updated": "2026-01-16T13:18:50Z",
    "total_decisions": 5
  },
  "decisions": [
    {
      "id": "DEC-001",
      "timestamp": "2025-12-09T14:29:24Z",
      "stage": "Stage 3 - Term Discovery",
      "trigger": {
        "description": "What prompted this decision",
        "evidence": {
          "metric1": "value1",
          "metric2": "value2"
        }
      },
      "options_considered": [
        {"option": "Option A: Description"},
        {"option": "Option B: Description"},
        {"option": "Option C: Description"}
      ],
      "decision": "Option B - Short description",
      "rationale": "Why this decision was made. Include multiple reasons.",
      "protocol_impact": {
        "sections_modified": ["Section 3.2", "Section 4.1"],
        "version_change": "1.0 → 1.1",
        "methodology_change": "Brief summary of what changed"
      },
      "implementation": {
        "scripts_created": ["script1.py", "script2.R"],
        "dependencies_added": ["package1", "package2"],
        "other_details": "Any relevant implementation notes"
      }
    }
  ]
}
```

**Required fields:**
- `id`: Unique identifier (format: `DEC-XXX` where XXX is zero-padded number)
- `timestamp`: ISO 8601 format with timezone (UTC recommended)
- `stage`: Which phase of research this decision occurred in
- `trigger.description`: What prompted the decision
- `trigger.evidence`: Quantitative/qualitative data supporting need for decision
- `options_considered`: List of options evaluated (minimum 2)
- `decision`: Final decision made
- `rationale`: Justification for the decision
- `protocol_impact.sections_modified`: Which protocol sections are affected
- `protocol_impact.version_change`: Version transition (e.g., "1.3 → 1.4")

**Optional fields:**
- `implementation`: Details about how decision was implemented
- Additional custom fields relevant to your project

---

### 4. `CHANGELOG.md` - Human-Readable Summary

**Purpose:** Quick-reference summary of protocol evolution following [Keep a Changelog](https://keepachangelog.com/) format.

**Template:**

```markdown
# Protocol Changelog

All substantive changes to the research protocol are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project uses Semantic Versioning for protocol versions.

---

## [Unreleased]

---

## [1.2] - 2026-01-16

### Changed
- Brief description of what changed ([DEC-002])
  - **Decision:** Option B - Combined approach
  - **Rationale:** Key reason for decision
  - **Impact:** Protocol sections 3.2, 4.1

---

## [1.1] - 2025-12-10

### Added
- Description of what was added ([DEC-001])
  - **Decision:** Implement hybrid validation
  - **Rationale:** Most rigorous methodology
  - **Impact:** Protocol sections 3.3

---

## [1.0] - 2025-12-09

### Added
- Initial protocol baseline
- [List key components of initial protocol]

---

## Change Categories

- **Added:** New sections, methods, or procedures
- **Changed:** Modifications to existing methods
- **Deprecated:** Methods marked for future removal
- **Removed:** Deleted sections or methods
- **Fixed:** Corrections to errors in the protocol
```

---

### 5. Individual Decision Files (Optional but Recommended)

**Purpose:** Detailed standalone documentation for major decisions.

**Naming:** `DEC-XXX_Brief_Description.md`

**Template:**

```markdown
# DEC-XXX: Decision Title

**ID:** DEC-XXX
**Timestamp:** YYYY-MM-DD HH:MM:SS UTC
**Stage:** Research Phase
**Protocol Version Change:** X.Y → X.Z

---

## Trigger

[What prompted this decision? What problem or observation led to this?]

**Evidence:**
- Metric 1: Value
- Metric 2: Value
- Observation: Description

---

## Options Considered

### Option A: [Name]
- **Pros:** [Benefits]
- **Cons:** [Drawbacks]

### Option B: [Name]
- **Pros:** [Benefits]
- **Cons:** [Drawbacks]

### Option C: [Name]
- **Pros:** [Benefits]
- **Cons:** [Drawbacks]

---

## Decision

**[Option X - Brief description]**

---

## Rationale

[Detailed justification for why this option was selected]

1. Reason 1
2. Reason 2
3. Reason 3

---

## Implementation

### Scripts Created
- `script1.py` - Purpose
- `script2.R` - Purpose

### Dependencies Added
- package1
- package2

### Data Files Created/Modified
- `file1.csv` - Description
- `file2.json` - Description

### Configuration Changes
- [Any config modifications]

---

## Protocol Impact

**Sections Modified:**
- Section X.Y: Description
- Section A.B: Description

**Version Change:** X.Y → X.Z

**Methodology Change:** [Summary]

---

**Decision Status:** ✅ IMPLEMENTED AND VALIDATED
```

---

## Version Numbering

### Recommended Scheme: Semantic Versioning (MAJOR.MINOR)

```
MAJOR.MINOR
```

**Major version (X.0):** Fundamental methodological changes
- Examples: Changing databases, complete search strategy overhaul, fundamental redesign
- Rare, requires strong justification

**Minor version (1.X):** Substantive but incremental changes
- Examples: Adding search terms, modifying extraction methods, changing thresholds
- Most decisions fall into this category

**Patch version (1.1.X):** Optional - not used in reference implementation
- Reference implementation treats all logged changes as at least "minor"
- Could be used for documentation fixes if desired

### Alternative Schemes

You may adapt this to your needs:
- Date-based: `YYYY.MM.DD`
- Sequential: `v1`, `v2`, `v3`
- Git-style: SHA hashes with tags

**Key requirement:** Version numbers must be monotonically increasing and unambiguous.

---

## Workflow

### Step-by-Step Process

#### 1. Identify Substantive Decision

**Researcher or LLM assistant recognizes** a methodological choice that meets substantive criteria (see "What is a Substantive Decision?" above).

**Examples:**
- "We need to decide how to handle papers without author keywords"
- "The query is timing out - we need a new retrieval strategy"
- "Should we expand the seed set to include this newly discovered paper?"

---

#### 2. Flag for Logging

**Human researcher indicates** the decision should be logged.

**Method 1 - Explicit flag:**
```
User: "SUBSTANTIVE: We need to decide how to handle papers without keywords"
```

**Method 2 - LLM proactive identification:**
```
Assistant: "This appears to be a substantive methodological change that will
affect reproducibility. Should I log this decision in the protocol?"

User: "Yes, please log it."
```

---

#### 3. Propose Options

**LLM assistant or researcher** explicitly lists multiple options considered.

**Requirements:**
- Minimum 2 options (preferably 3-5)
- Each option should be clearly described
- Include pros/cons if helpful

**Example:**
```
Option A: Author keywords only (127 papers)
  - Pros: Simple, clean
  - Cons: Excludes 67% of corpus

Option B: Combined approach (author keywords + NLP mining)
  - Pros: Comprehensive coverage
  - Cons: More complex

Option C: Title/abstract mining for all papers
  - Pros: Uniform methodology
  - Cons: Discards expert-provided keywords
```

---

#### 4. Make Decision

**Researcher selects** one option with rationale.

**Example:**
```
User: "Let's go with Option B. We can't exclude foundational papers,
and author keywords provide expert validation where available."
```

---

#### 5. Record Decision

**LLM assistant or researcher** creates the decision record.

##### Manual Method (No Automation)

1. Generate next decision ID (check `decision_log.json` for count, increment)
2. Create entry in `decision_log.json` following schema above
3. Update `metadata.total_decisions` count
4. Update `metadata.protocol_current_version`
5. Update `metadata.last_updated` timestamp
6. Add entry to `CHANGELOG.md` under appropriate version
7. (Optional) Create individual decision file `DEC-XXX_Title.md`

##### Automated Method (With Scripting)

If you implement a logging utility (see Reference Implementation section), the assistant can call:

```python
from utils.protocol_logger import log_decision

log_decision(
    stage="Stage 3 - Term Discovery",
    trigger="67% of papers lack author keywords",
    evidence={
        "papers_with_keywords": 127,
        "total_papers": 389,
        "coverage_pct": 32.6
    },
    options=[
        "Option A: Author keywords only",
        "Option B: Combined approach",
        "Option C: Title/abstract mining only"
    ],
    decision="Option B - Combined approach",
    rationale="Cannot exclude foundational papers. Author keywords provide expert validation where available, NLP ensures comprehensive coverage.",
    protocol_sections=["Stage 3: Keyword Extraction"],
    implementation={
        "scripts_created": ["extract_keywords.py"],
        "dependencies_added": ["yake", "spacy"]
    },
    change_type="minor"  # or "major"
)
```

---

#### 6. Archive Protocol Version

**Before updating protocol.md:**

1. Copy current `protocol.md` to `versions/protocol_vX.Y.md` (where X.Y is OLD version)
2. This preserves point-in-time snapshot before changes

**Example:**
```bash
cp protocol/protocol.md protocol/versions/protocol_v1.3.md
```

---

#### 7. Update Protocol Document

**Modify `protocol.md`** to reflect the decision:

1. Update version number in header (increment according to change_type)
2. Update "Last Updated" date
3. Modify affected sections based on the decision
4. Add note referencing decision ID where appropriate

**Example:**
```markdown
### 3.2 Keyword Extraction

**Modified by:** [DEC-001]

For papers with author-provided keywords (127/389, 32.6%), extract keywords directly.
For papers without author keywords (262/389, 67.4%), apply NLP extraction using YAKE
and spaCy noun phrase chunking (see DEC-001 for rationale).
```

---

#### 8. Confirm and Continue

**LLM assistant** prints confirmation summary:

```
================================================================================
SUBSTANTIVE DECISION LOGGED: DEC-001
================================================================================
Stage: Stage 3 - Term Discovery
Trigger: 67% of papers lack author keywords
Decision: Option B - Combined approach
Protocol version: 1.0 → 1.1
Sections affected: Stage 3: Keyword Extraction
================================================================================
```

**Research continues** with updated protocol.

---

## LLM Assistant Integration

### Prompting Strategy for Claude Code / ChatGPT / Other Assistants

Include the following instructions in your project documentation or assistant system prompt:

```markdown
## Decision Logging Protocol

This project uses substantive decision logging. Follow this protocol:

### 1. Watch for "SUBSTANTIVE" Flag

When the user includes "SUBSTANTIVE" in their message, the next decision
should be logged to the protocol decision system.

### 2. Proactive Identification

If you identify a decision that meets substantive criteria (affects methodology,
reproducibility, or represents protocol deviation), pause and ask:

"This appears to be a substantive methodological change. Should I log this
decision to the protocol?"

### 3. Decision Logging Checklist

When logging a decision, ensure you collect:
- ✓ Stage/phase of research
- ✓ Trigger (what prompted the decision)
- ✓ Evidence (data supporting the need)
- ✓ Options considered (minimum 2)
- ✓ Final decision
- ✓ Rationale
- ✓ Protocol sections affected
- ✓ Implementation details

### 4. Regular Review

Every 3-5 exchanges, check if any unreported substantive changes occurred
and prompt user if logging needed.

### 5. Use Logging Utility (if available)

If `utils/protocol_logger.py` exists, use it to log decisions:

```python
from utils.protocol_logger import log_decision
log_decision(...)
```

Otherwise, manually update:
- protocol/decision_log.json
- protocol/CHANGELOG.md
- protocol/DEC-XXX_Title.md (optional)
- protocol/protocol.md (update version and content)
- Archive old version to protocol/versions/
```

---

## Reference Implementation

The reference implementation uses Python with a utility module that automates decision logging. This is **optional** but recommended for efficiency.

### Key Features

1. **Automatic ID generation** (DEC-001, DEC-002, etc.)
2. **Version incrementing** (MAJOR.MINOR)
3. **Protocol archiving** (copies old version to `versions/` before updating)
4. **Changelog updating** (appends entry to CHANGELOG.md)
5. **JSON validation** (ensures schema compliance)

### Implementation Location

**Reference:** See `utils/protocol_logger.py` in this repository

**Core function signature:**

```python
def log_decision(
    stage: str,                          # Research phase
    trigger: str,                        # What prompted decision
    evidence: Dict[str, Any],           # Supporting data
    options: List[str],                 # Options considered
    decision: str,                      # Final decision
    rationale: str,                     # Justification
    protocol_sections: List[str],       # Affected sections
    implementation: Optional[Dict] = None,  # Implementation details
    change_type: str = "minor"          # "major" or "minor"
) -> str:                               # Returns decision ID
```

### Adaptation to Other Languages

**R implementation:**
- Use `jsonlite` for JSON manipulation
- Use `glue` for string templating
- Use `lubridate` for timestamps

**JavaScript/TypeScript:**
- Use native JSON methods
- Use `date-fns` or `dayjs` for timestamps
- Use `fs` module for file operations

**Manual (no code):**
- Use text editor to manually update JSON
- Follow schema strictly
- Use online JSON validators to check syntax

---

## Quality Assurance

### Regular Checks

Perform these checks periodically:

1. **Version consistency:**
   - Does `protocol.md` version match `decision_log.json` metadata?
   - Are all versions in `CHANGELOG.md` accounted for?

2. **Archive completeness:**
   - For version X.Y in `protocol.md`, does `protocol_vX.(Y-1).md` exist in `versions/`?

3. **Decision ID sequence:**
   - Are decision IDs sequential without gaps?
   - Does highest DEC-XXX number match `total_decisions` in metadata?

4. **JSON validity:**
   - Run JSON through validator (e.g., `jsonlint`, online validators)
   - Ensure all required fields present for each decision

---

## Usage Scenarios

### Scenario 1: Research Protocol Evolution

**Context:** Systematic literature review where search strategy evolves based on findings.

**Application:**
- Log decisions when adding/removing search terms
- Track query refinement iterations
- Document validation threshold changes
- Maintain complete audit trail for PRISMA-S reporting

---

### Scenario 2: Data Analysis Pipeline Changes

**Context:** Computational analysis where preprocessing methods are refined.

**Application:**
- Log decisions about data cleaning approaches
- Track changes to statistical thresholds
- Document algorithm parameter tuning
- Support reproducibility and peer review

---

### Scenario 3: Multi-Database Integration

**Context:** Combining data from multiple sources with evolving deduplication strategies.

**Application:**
- Log decisions about database selection
- Track deduplication method evolution
- Document handling of edge cases
- Maintain provenance for merged datasets

---

## Benefits for Peer Review and Publication

### PRISMA-S Compliance

The decision log directly supports PRISMA-S (Preferred Reporting Items for Systematic Reviews - Search) requirements:

- **S1:** Documents search strategy development process
- **S2:** Provides rationale for database selection
- **S3:** Tracks query evolution with complete history
- **S4:** Documents validation against gold standard (seed papers)

### Reproducibility

- Complete audit trail allows independent replication
- All parameters and thresholds documented with justification
- Deviations from original protocol explicitly explained

### Methodological Transparency

- Demonstrates rigorous decision-making process
- Shows consideration of alternatives
- Provides evidence-based justification for choices
- Reduces appearance of p-hacking or data dredging

---

## Minimal Implementation Checklist

To implement this protocol in a new repository:

- [ ] Create `protocol/` directory
- [ ] Copy current protocol to `protocol/protocol_v1.0.md` (baseline)
- [ ] Copy current protocol to `protocol/protocol.md` (living version)
- [ ] Create `protocol/decision_log.json` with initial metadata
- [ ] Create `protocol/CHANGELOG.md` with version 1.0 entry
- [ ] Create `protocol/versions/` directory (empty initially)
- [ ] Add protocol documentation to project README
- [ ] (Optional) Create `protocol/README.md` explaining the system
- [ ] (Optional) Implement logging utility in preferred language
- [ ] (Optional) Add LLM assistant instructions to project documentation

---

## Maintenance

### When to Review

- **After each decision:** Verify log entry created correctly
- **Weekly:** Check version consistency across files
- **Before major milestones:** Audit complete decision trail
- **Before publication:** Generate decision summary for methodology section

### Archive Management

- Keep all versions in `versions/` directory (disk space is cheap)
- Never delete archived versions (breaks historical record)
- Consider compressing very old archives if storage becomes issue

### Long-Term Preservation

For publication and archival:
1. Export `decision_log.json` to CSV for long-term readability
2. Include both JSON and CSV in supplementary materials
3. Archive entire `protocol/` directory with manuscript submission
4. Consider depositing to repository (OSF, Zenodo, etc.)

---

## Troubleshooting

### "I forgot to log a decision"

**Solution:** Log it retroactively
1. Create decision entry with accurate timestamp (when it actually occurred)
2. Insert in chronological order in `decision_log.json`
3. Note in `implementation` that it was "logged retroactively"
4. Update version numbers for all subsequent decisions if needed

### "I made a mistake in a logged decision"

**Solution:** Log a correction
1. Create new decision documenting the correction
2. Reference original decision ID
3. Explain what was incorrect and why
4. Do NOT modify the original decision (preserves audit trail)

### "Decision IDs out of sequence"

**Solution:** Renumber if early in project, or continue with note
- Early project (< 5 decisions): Renumber and document in commit message
- Later project: Continue with note in README explaining gap
- Never re-use deleted decision IDs

---

## Advanced Extensions

### Integration with Git

Tag protocol versions in Git:
```bash
git tag -a protocol-v1.5 -m "Decision DEC-005: Query refinement"
```

### Automated Reporting

Generate summary reports from `decision_log.json`:
- Count decisions by stage
- Timeline visualization
- Decision type distribution
- Export to LaTeX table for manuscript

### Multi-Project Analysis

Compare decision patterns across projects:
- Common decision types
- Typical triggers
- Option selection patterns
- Inform future protocol design

---

## Summary

This protocol provides a rigorous, transparent, and reproducible approach to tracking methodological decisions in research projects. Key principles:

1. **Log substantive changes** (methodology, reproducibility, protocol deviations)
2. **Document alternatives** (always consider multiple options)
3. **Justify decisions** (evidence-based rationale)
4. **Version protocol** (automated or manual version control)
5. **Archive history** (never delete old versions)
6. **Integrate with LLMs** (leverage assistants for consistency)

By following this protocol, you ensure complete provenance tracking, support peer review requirements, and maintain methodological rigor throughout your research project.

---

## References

- **PRISMA-S:** Rethlefsen ML, et al. (2021). PRISMA-S: an extension to the PRISMA Statement for Reporting Literature Searches in Systematic Reviews. *Systematic Reviews*, 10(1), 39.
- **BIBLIO:** Montazeri A, et al. (2023). Bibliometric Literature Reviews: Reporting Standards. *Journal of Informetrics*.
- **Keep a Changelog:** https://keepachangelog.com/
- **Semantic Versioning:** https://semver.org/

---

**Protocol Version:** 1.0
**License:** CC0 1.0 Universal (Public Domain)
**Source Repository:** https://github.com/[your-repo] (adapt as needed)

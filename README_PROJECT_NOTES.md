# README Project - Session Notes

## Project Goal
Create a professional, publication-ready README for the nw-data-commons repository to support peer-reviewed publication.

## Decisions Made

### License
- **Type:** MIT License
- **Year:** 2025
- **Rationale:** Most permissive for research software; code is public while data access is controlled via OSF application process

### Authors & Maintainers
1. **William Faulkner** (Corresponding Author)
   - Email: william@fluxrme.com
   - Affiliation: Science Policy Research Unit, University of Sussex

2. **Victoria Garza**
   - Affiliation: Louisiana State University

3. **E. Kesse Asante**
   - Affiliation: Louisiana State University

### Citation Information
- **Preprint Title:** "Accessible Climate and Impact Model Output for Studying the Human and Environmental Impacts of Nuclear Conflict"
- **Lead Author:** Cheryl Harrison et al.
- **DOI:** https://doi.org/10.31223/X5XB20
- **EarthArXiv Link:** https://eartharxiv.org/repository/view/10406/
- **Preprint File:** `NW_Data_Harrison_preprint.pdf` (in repository)

### Data Sources
- **Primary Source:** OSF repository at https://osf.io/e28gq/
- **Note:** Some model outputs currently available; all will be available by publication
- **Access Model:** Data requires application with description of intended use

## Repository Information
- **GitHub URL:** https://github.com/wnfaulkner/nw-data-commons
- **Working Environment:** VSCode on WSL
- **Current Branch:** main
- **Languages:** Python, R, Jupyter notebooks

## Next Steps

### Immediate Decision Needed
Choose README approach:
- **Option A:** Enhance current README structure with publication-ready elements (citation, license, authors, better organization)
- **Option B:** Complete restructure following standard academic software repository format

### Proposed README Structure (for reference)
1. Title & Badges
2. Brief Description
3. Citation
4. Overview
5. Installation & Setup
6. Data Workflows
7. Usage
8. Repository Structure
9. Data Products
10. Contributing
11. License
12. Authors & Contact
13. Acknowledgments
14. References

### Remaining Tasks
- [ ] Finalize README structure approach
- [ ] Draft improved README sections
- [ ] Create MIT LICENSE file
- [ ] Create CITATION.cff file
- [ ] Review and finalize all documents

## Session History
- **Session 1 (2025-10-09):**
  - Discussed project goals
  - Reviewed license options
  - Read preprint PDF
  - Extracted author and citation information
  - Confirmed key details with user
  - Proposed README structures

## Files in Repository (relevant)
- `README.md` - Current README (to be enhanced)
- `NW_Data_Harrison_preprint.pdf` - Reference preprint
- `requirements.txt` - Python dependencies
- `renv.lock` - R environment
- `install_system_deps.sh` - System dependencies script
- Various R and Python scripts in `c_scripts/`

## Notes
- User prefers step-by-step approach with clear instructions
- Need to provide code in copy-paste friendly format
- Should ask clarifying questions to avoid common pitfalls
- Express uncertainty where present

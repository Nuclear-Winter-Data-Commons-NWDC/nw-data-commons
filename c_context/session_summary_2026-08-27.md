# Session Summary: 2026-08-27

## Session Start

**Date:** 2026-08-27
**Starting Commit:** e1874aa (Add Year 2 snapshot note to starvation dataset template; ignore __pycache__)
**Session Goal:** Produce a thorough reply to four external correspondents who raised
questions about zeros vs. missing data in the starvation dataset (and Xia et al. 2022
Table S8). Fix anything the investigation turns up.

**Note on names:** all individuals in this thread are referred to by role only (the project
PI, the external researcher, the student analyst, the source-paper lead author). See the
privacy task in `future_tasks.md`.

**Source materials:** `c_context/2026-08-27_external_data_queries/` — gitignored
(2 email-chain PDFs, `globsconfpap26.docx` draft paper, `table s8a.docx`).

---

## 0. Orientation — done

- `git pull` → already up to date; working tree clean.
- Read README, latest session summary (2026-07-15), `future_tasks.md`, `readme_template.md`.
- No `.claude/` directory in this repo.
- Extracted email chains + attachments to scratchpad (installed `pypdf`, `python-docx`
  into `.venv`).

### The ask, decomposed

| # | Question (asker) | Status |
|---|---|---|
| Q1 | Do the zeros in the Data Commons starvation data have the same ambiguity as Table S8 — i.e. 0 = "no deaths" and 0 = "no data"? (the PI → the student analyst) | investigated |
| Q2 | How can real zeros be distinguished from missing data? Australia/NZ 0s may be real; Barbados 0 is not credible. (the external researcher) | investigated |
| Q3 | How were small countries handled across the country-level data products? (the PI → Will) | open |
| Q4 | Are there ag remote-sensing datasets with finer grids? (the external researcher) | open |
| Q5 | Any other Data Commons datasets they plan to use / issues with them? (Will → group) | open (awaiting them) |
| Q6 | Bibliography on nuclear winter + climate-change collapse — scope? (Will → group) | open (awaiting them) |

---

## 1. Findings so far (from `data/starvation.json`, 7,584 rows, 158 countries)

**A. The Data Commons starvation dataset is a faithful transcription of the Xia et al.
(2022) supplementary tables — it inherits their limits, and adds no new ones.**
`b_scripts/3_standardize/starvation_cleaning.R` reshapes the published sheets to long
form, joins country metadata, and derives `pct.population.starving.2010`. No
zero/NA handling is applied anywhere.

**B. The binding constraint is *rounding*, not the climate-model grid.** Every value in
the dataset is a multiple of 0.1 million (100,000 people) — verified across all rows.
So the reporting floor is 50,000 people. This is a table-precision issue in the
published supplement, not a 1–2° grid-resolution issue.

**C. Country coverage is identical to Table S8** — 158 nations + a `Total` row that the
cleaning script drops. No small countries were lost in the Data Commons pipeline.

**D. The external researcher's Barbados case is answered by the data: Barbados is NOT missing.**
Under every *no-trade* sub-scenario, Barbados shows 0.1 M starving (a third of its
population) at *every* soot level, 5 Tg through 150 Tg. Its zeros appear only under the
*trade* assumption. So the model does contain Barbados; the trade-case zeros mean
"imports cover it," possibly plus sub-resolution rounding.

**E. A sharper test than "zero at 150 Tg": zero in the harshest sub-scenario**
(150 Tg, no trade, no livestock, 0% waste reduction), where the median country loses
94.2% of its population. Only **13** countries are zero there:

| Country | Pop 2010 | Max % starving still consistent with a 0.0 |
|---|---|---|
| Argentina | 40.4 M | 0.1% |
| Australia | 22.4 M | 0.2% |
| Haiti | 9.9 M | 0.5% |
| Paraguay | 6.5 M | 0.8% |
| Costa Rica | 4.7 M | 1.1% |
| Panama | 3.7 M | 1.4% |
| Uruguay | 3.4 M | 1.5% |
| Oman | 2.8 M | 1.8% |
| Gambia | 1.7 M | 2.9% |
| Guyana | 0.8 M | 6.2% |
| Solomon Islands | 0.5 M | 10.0% |
| Suriname | 0.5 M | 10.0% |
| Iceland | 0.3 M | 16.7% |

Argentina/Australia/Uruguay/Paraguay are large net food exporters — plausible true
zeros. Haiti, Oman, Iceland, Gambia are not plausible true zeros and are the strongest
missing-data candidates.

**F. Six countries are zero in ALL 48 scenario cells:** Australia, Haiti, Iceland,
Guyana, Solomon Islands, Suriname. "Zero everywhere, including the worst case" is the
cleanest available proxy for "no usable land points / no data."

**G. The PI's proposed rule — "drop any country with a zero in the 150 Tg case" — is too
blunt.** In the S8 slice (trade + partial livestock) that removes 28 of 158 countries,
including Argentina and Brazil-like exporters whose zeros are defensible. The external researcher is right.

**H. Two data-quality issues found incidentally (need verification, then fix):**
- `country.iso3` and `country.land.area.sq.km` are NULL for **Turkey** and
  **Taiwan, province of China** → a country-name join miss in the metadata table.
  These two countries silently drop out of any ISO3- or area-based analysis.
- `country.land.area.sq.km` looks like it may actually be **agricultural** land area,
  not total land area (Australia 3,588,950 km² vs. true 7,682,300; Iceland 18,720 vs.
  100,250; Suriname 840 vs. 163,820). Needs confirmation against the metadata source.

---

## 2. Files changed this session

- `c_context/session_summary_2026-08-27.md` — this file.
- (venv only, not committed) `pypdf`, `python-docx` installed into `.venv`.

---

## Open / next steps

See the todo list in section 3 below.


---

## 4. Verification against the authoritative source — BLOCKED (documented)

`.env` is a fresh template with no `OSF_TOKEN`, and `a_data/osf_data_current/{2_aggregated,3_standardized}/`
are empty, so the OSF standardized CSVs could not be pulled this session. All findings
below are derived from the dashboard JSONs in
`d_codesign_and_analysis/2026-02-24_analysis_dashboard/data/`, which are generated from
those CSVs by `generate_dashboard_data.py`. **Next session: regenerate an OSF token and
re-verify.** Locally available: agriculture_agmip, agriculture_clm, fish_catch, sea_ice,
starvation, uv (6 of 9). Not available locally: temperature, precipitation,
surface_solar_radiation — audited from cleaning code only.

---

## 5. Cross-dataset audit of small-country / missing-data handling — DONE

### 5.1 Zeros vs. NULLs: starvation is the sole offender

| Dataset | rows | NULL rate (main measures) | exact zeros |
|---|---|---|---|
| uv_radiation | 84,960 | 18.2% | 0 |
| agriculture_clm | 10,080 | 4.2–41.7% | 0 |
| agriculture_agmip | 4,290 | 6.3–74.1% | 0 (1 row) |
| fish_catch | 22,464 | 1.3% | 0 |
| sea_ice | 10,752 | 0% | 88.9% (genuine 0 m ice — correct) |
| **starvation** | **7,584** | **0%** | **38.1%** |

**Conclusion:** every other country-level product already encodes "no data" as NA and
reserves numbers for real estimates. Starvation is the only dataset that conflates them,
and it does so because it is a faithful transcription of the published Table S8 — our
pipeline adds no NA handling of its own (`starvation_cleaning.R` has none).

### 5.2 Country coverage varies widely by dataset

uv 236 · agriculture_clm 168 · **starvation 158** · agriculture_agmip 143.
(fish_catch is EEZ-level; sea_ice is port-level, 9 port countries.)

### 5.3 Which countries actually have no land grid points

Taking uv (full 236-country grid mapping) as the reference: exactly **43 countries are
all-NULL** — i.e. genuinely no land grid points at model resolution: American Samoa,
Bahamas, Bahrain, Barbados, Belize, Bhutan, Bonaire/St Eustatius/Saba, BIOT, British
Virgin Is., Cape Verde, Cayman Is., Christmas I., Cocos Is., Comoros, El Salvador,
Eswatini, Faroe Is., French Polynesia, Gambia, Kiribati, Kosovo, Maldives, Malta,
Marshall Is., Mayotte, Micronesia, Montenegro, N. Mariana Is., Palau, Rwanda, St Helena,
St Lucia, Samoa, Sao Tome & Principe, Seychelles, Slovakia, Taiwan, Tonga, Trinidad &
Tobago, Tuvalu, US Minor Outlying Is., US Virgin Is., Wallis & Futuna.

### 5.4 KEY RESULT — the starvation zeros are NOT a grid-resolution artefact

Of the 14 starvation countries that have **no land grid points** in uv, **13 still carry
non-zero starvation estimates**. Xia et al.'s food-security calculation runs off national
food-balance/calorie-supply data, not the climate grid, so small islands *do* get real
estimates. This refutes the working hypothesis in the email thread.

Their signature is diagnostic: Bahamas, Barbados, Cape Verde, Gambia, Maldives, Malta,
St Lucia, Trinidad & Tobago each show **exactly 24 of 48 cells non-zero** — every
*no-trade* cell non-zero, every *trade* cell zero. Their zeros are a **model result about
trade**, not missing data. Barbados: 0.0 under trade at every soot level; 0.1 M (a third
of its population) under no-trade at every soot level, 5 → 150 Tg.

### 5.5 Revised suspect list — very short

Only 6 countries are zero in all 48 cells: **Australia, Haiti, Iceland, Guyana, Solomon
Islands, Suriname** — all of which *do* have land grid points. Of these, Australia,
Guyana, Suriname (net food exporters), Iceland (fisheries; Xia includes marine catch) and
Solomon Islands (subsistence) are defensible true zeros. **Haiti is the one genuinely
implausible case.**

### 5.6 THE BIGGER PROBLEM — 80 countries are absent from starvation entirely

Table S8 as supplied contains 158 nations + a `Total` row (verified: 159 data rows across
its 4 docx tables; our 158 match it exactly, so nothing was lost in our pipeline). But
relative to the 236-country reference it omits — beyond small islands — a set of large,
mostly low-income states: **Democratic Republic of the Congo, Sudan, South Sudan, Somalia,
Syria, Libya, Burundi, Eritrea, Côte d'Ivoire, Papua New Guinea, Bhutan, Djibouti,
Eswatini, North Macedonia, Qatar, Singapore, Bahrain, Fiji, Comoros.** Roughly 200 million
people (2010), and disproportionately the food-insecure periphery the external researcher's paper is
about. For that use case this is a larger problem than the zeros. Note agriculture_clm
(same paper) *does* cover DR Congo, Sudan and Somalia — so the gap is specific to the
food-security model's country set.

### 5.7 Precision floor

Every starvation value is a multiple of 0.1 M (100,000 people) — verified across all
7,584 rows. Reporting floor = 50,000 people. Rounding alone explains zeros for large
countries but cannot explain a zero for a small country with a large implied share
(Iceland: a 0.0 is consistent with up to 16.7% of the population starving).

### 5.8 Incidental documentation/data defects found (NOT fixed — awaiting say-so)

1. `country.iso3` and `country.land.area.sq.km` are NULL for **Turkey** and
   **Taiwan, province of China** in starvation — a country-name join miss (the metadata
   table uses "Türkiye" / "Taiwan"). Both silently drop out of ISO3- or area-keyed joins.
2. `country.land.area.sq.km` appears to be **agricultural** land area, not total land
   area (Australia 3,588,950 km² vs. true 7,682,300; Iceland 18,720 vs. 100,250;
   Suriname 840 vs. 163,820). Needs confirming against the metadata source, then
   renaming or redocumenting.
3. `readme_template.md` Table S1 lists the starvation scenarios as "150, 47, 37, 27, 16,
   5, **0**" — but the dataset contains **no 0 Tg control** (150/47/37/27/16/5 only).
   Documentation error.

---

## 6. Answer to the external researcher's remote-sensing question (Q4)

The binding constraint is not observational resolution — it is (a) the ESM grid for
climate forcing and (b) the country set of the food-balance model. Finer ag data won't
by itself add the 80 missing countries. Practical leads, best first:

- **FAO Food Balance Sheets / FAOSTAT SUA** — national calorie supply for ~180 countries
  incl. small islands. This is the route that would actually extend coverage.
- **MapSPAM v1.1/v2 (10 km, 42 crops, 2010)** — FAO catalog.
- **GAEZ v4 (5 arc-min ≈ 9 km)** — FAO/IIASA.
- **GGCP10 (10 km, 2010–2020, maize/wheat/rice/soy)** — Nature Scientific Data (2024).
- MODIS vegetation indices at 250 m exist but are noted as poor for scattered/mixed
  small-island cropping.

---

## 7. Session status

- Repo changes so far: this session summary; one `.gitignore` line (explicitly requested);
  the email draft, which lands inside the now-ignored folder so it will not be committed.
- No data edits, no OSF pushes.
- Fix plan recorded for next session (see below).

## 8. Plan recorded for NEXT session

1. Regenerate OSF token; restore `.env`; sync `a_data/osf_data_current/`.
2. Re-verify every finding above against the authoritative
   `3_standardized/starvation_v2026-02-13.csv`.
3. Implement the agreed policy: **keep published values unchanged; add a documented
   `data.quality.flag` column** (`plausible-true-zero` / `below-reporting-threshold` /
   `suspected-no-data` / `not-estimated`) rather than overwriting zeros with NA.
4. Fix the Turkey/Taiwan join miss; confirm and correct the `country.land.area.sq.km`
   definition.
5. Correct the README Table S1 starvation scenario list (drop 0 Tg) and add notes on the
   0.1 M rounding floor, the trade-case zero semantics, and the 80 absent countries.
6. Re-upload to OSF and send a short follow-up email to the group.

---

## 3. Todo list

1. ✅ Orient: pull, scan repo, ingest `c_context/` + new question folder.
2. ✅ Create this session summary.
3. ✅ Clarifying questions asked and answered (see section 9).
4. ⛔ BLOCKED (no OSF token) — deferred to next session. Verify findings against the authoritative OSF file (`3_standardized/starvation_v2026-02-13.csv`),
   not just the dashboard JSON, and against the Xia et al. supplement PDF.
5. ✅ DONE (section 5) — Q3 audit how small countries / missing land points are handled across
   ALL country-level datasets (temperature, precipitation, solar, UV, agriculture_clm,
   agriculture_agmip), not just starvation.
6. 🗓 DEFERRED to next session by user decision (plan recorded, section 8). Decide + implement the zero/NA policy for starvation (recommendation: keep the
   published values, add a documented `data.quality.flag` column rather than silently
   overwriting zeros with NA).
7. 🗓 DEFERRED to next session. Fix the Turkey / Taiwan ISO3 + land-area join miss; confirm what
   `country.land.area.sq.km` actually measures and rename/redocument if needed.
8. 🗓 DEFERRED to next session. Update `readme_template.md` starvation notes (rounding floor, zero semantics,
   scenario-coverage caveat) and push any corrected files to OSF.
9. ✅ DONE. Reply drafted per user spec — **394 words** of email body + **397-word**
   technical detail section below the signature. Two formats, both in the (ignored)
   question folder: `reply_draft_2026-08-27.txt` (plain) and
   `reply_draft_2026-08-27.html` (Gmail paste — open in browser, Ctrl+A/Ctrl+C).
   HTML chosen over RTF because Gmail's composer is HTML natively; inline styles only,
   since Gmail strips classes and external CSS.
   Revision note: the UV paragraph was rewritten after the user flagged it as opaque —
   it now explains *why* UV is the control (UV maps gridded output straight onto
   countries, so a country with no land grid cell comes out blank → 43 of 236 = the
   empirical "grid misses this country" list; 13 of the 14 of those in the starvation
   table still have non-zero values → the zeros are not a resolution artefact).
10. ⬜ Final commit at end of session (user: "we will do a final commit at the end").
11. ✅ `.gitignore`: added `c_context/2026-08-27_external_data_queries/` —
    the folder holds private email correspondence and must not reach the public repo.
    Verified with `git check-ignore`; the folder was never tracked, so no history to scrub.

**Last Updated:** 2026-08-27 (session closed)


---

## 9. User direction received 2026-08-27

1. **No data updates this session** unless strictly necessary. Mention in the email that
   the fix is straightforward.
2. Agreed: `data.quality.flag` approach, not NA overwrites. **Shelve for this session**,
   record the plan, implement next session, notify the group in a follow-up email.
3. `.env` files are fresh templates with no tokens; tokens must be **regenerated next
   session**.
4. Full cross-dataset audit — done, used to sharpen the email.
5. Deliverable is a **plain-text email body only** (option i). Attachments/tables only if
   the email gets too long.
6. **Reiterate** the two open asks (other datasets of interest; bibliography scope).
7. Bibliography assembly is out of scope for this repo — user handles it elsewhere.
8. Remote-sensing question: quick search only; mention a lead if genuinely on-point.

**Standing constraint:** make no repo changes without explicit say-so (this session
summary excepted).


---

## 10. OPEN QUESTION FOR NEXT COMMIT

This session summary is NOT ignored and will be committed to a public repo. It names the
four correspondents and summarises private correspondence. Decide before committing
whether to sanitise it (e.g. initials only, drop the folder reference) or to gitignore it
alongside the source folder.


---

## 11. Correction log — claims retracted or revised during drafting (2026-08-27)

The user challenged two claims; both needed revision. Recorded so they are not
reintroduced.

**RETRACTED — the UV cross-reference as evidence.** I argued the starvation zeros were not
a grid artefact because 13 of the 14 starvation countries that are blank in `uv` still
carry non-zero values. Invalid: `uv` is one hop (a WACCM field mapped onto countries),
while starvation is three (ESM → CLM crop + fishery models → national food-balance
accounting). Different pipelines, different blind spots. `uv`'s blanks are not
starvation's blanks. Suggestive at best; it was presented as proof.

**REVISED — "80 countries absent".** The denominator was `uv`'s 236 entries, which include
territories (Bouvet Island, Christmas Island, …). Replaced with **"at least 19 sovereign
states"**, verified name-by-name against the printed 158-nation list, checking aliases
(DR Congo/Zaire, Cote d'Ivoire/Ivory Coast, Eswatini/Swaziland, North Macedonia). All 19
confirmed absent.

**NEW AND MORE IMPORTANT — 161 physically impossible zeros.** A monotonicity test
(starvation should not fall as soot rises) finds **30 countries with 161 cells that are
zero at a soot level where a milder scenario killed millions**: Philippines 0 at 27 Tg vs
10.5M lower down; Algeria 0 at 37 Tg vs 19.6M; Nigeria 0 at 47 Tg vs 8.3M; Morocco 0 at
16 Tg vs 6.9M. Distribution by level: 16 Tg 19 cells, 27 Tg 37, 37 Tg 50, 47 Tg 40,
150 Tg 15. **This — not the UV argument — is what rules out grid resolution as the sole
cause**, since these are large countries covering many grid cells. It also means the
defect is larger than section 5.5 implied: interior zeros are invisible to any filter
keyed on 150 Tg or on all-zero countries.

**PRECEDENT FOUND.** `session_summary_2026-02-18b.md:137` — in Feb 2026 the 47 Tg
surface-radiation run was found milder than 27 Tg, traced to the raw simulation files, and
a question to co-authors was drafted but **never resolved**. Only 25% of the impossible
zeros sit at 47 Tg, so this is not a complete explanation, but it is a related open
anomaly. No record anywhere in the repo of previously investigating or resolving the
starvation zeros, and no record of contacting the source-paper lead author about them.

**Consequence for the email:** the position is now "we don't know the cause; here is what
we can and cannot rule out; ask the source-paper lead author" — not a confident diagnosis.

**FACT CHECK — the student analyst did not ask the user for a meeting.** "Do you have a time to talk
about cleaning up the data?" was addressed to the PI, answering the PI's offer. The
meeting offer was removed from the draft.

## 12. Final email structure (approved shape)

Six consolidated, reordered questions with answers beneath each; the same six-heading
structure repeated below the signature as supporting detail for a conversation with the source-paper lead author.
Body **399 words**; detail section **407 words** (7 over the 400 budget — flagged to user).


---

## 13. Session close

**Delivered:** the reply draft (plain text + Gmail-ready HTML) in the gitignored
`c_context/2026-08-27_external_data_queries/`; the full cross-dataset audit (section 5);
the correction log (section 11).

**NOT done — carried to next session:**
1. Regenerate the OSF token, restore `.env`, sync `a_data/osf_data_current/`.
2. Re-verify every finding against the authoritative `3_standardized/starvation_v2026-02-13.csv`
   (this session's numbers come from the dashboard JSONs, which are generated from it).
3. Implement `data.quality.flag`; fix the Turkey/Taiwan ISO3 join miss; confirm what
   `country.land.area.sq.km` actually measures; correct the README's starvation scenario
   list (no 0 Tg control exists).
4. Re-upload to OSF and send the follow-up email.
5. Run the repo-wide PII audit recorded in `future_tasks.md` — including committed
   history, not just the working tree.

**Privacy actions taken this session:** the source folder was renamed to a neutral name
and gitignored (never tracked); this summary and `future_tasks.md` refer to people by role
only.

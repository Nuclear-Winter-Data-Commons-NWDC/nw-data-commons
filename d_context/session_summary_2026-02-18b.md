# Session Summary: 2026-02-18 (Part B — Meeting Prep)

## Session Start

**Date:** 2026-02-18 (second session block)
**Starting Commit:** da58caf (Update session summary 2026-02-18 with complete log)
**Session Goal:** Prepare for tomorrow's re-introduction meeting with Matt Korda & Ollie Stephenson (FAS). Tasks: research their work, design country report HTML, create meeting deck, diagnose known data issues, write session plan.

---

## Meeting Context

**Meeting:** Google Meet, ~60 min, tomorrow (2026-02-19)
**Attendees:** William Faulkner, Matt Korda (FAS Nuclear Information Project / SIPRI), Ollie Stephenson (FAS AI & Emerging Tech Policy)
**Prior meeting:** 2025-01-29 (DC, in-person). Follow-up has been scheduling emails only — assume both are largely fresh since that meeting.
**Overarching goal:** Re-introduce NWDC, build excitement as a platform for broader analysis, begin scheming on funding + institutional home. After meeting: hope they spread the word in their networks.

---

## Updates Since Jan 2025 Meeting (to communicate)
1. Added starvation dataset (user-facing)
2. Added downwelling shortwave radiation dataset (user-facing)
3. Smoothed out pipeline for adding/updating datasets (back end)
4. Created country reports HTML report — easily deployable as web page with PDF export
5. Paper is a preprint, now accepted for publication; revisions due Mar 1

---

## Profiles: Matt Korda & Ollie Stephenson

### Matt Korda
- **Role:** FAS Nuclear Information Project + Associate Senior Researcher at SIPRI
- **Core work:** Tracking/visualizing global nuclear arsenals (Nuclear Notebook in Bulletin of Atomic Scientists, SIPRI Yearbook), ICBM Information Project
- **Recent publications:**
  - "The Aftermath: The Expiration of New START" (Feb 2026)
  - "Inspections Without Inspectors" — remote-sensing-based arms control verification (Nov 2025)
  - "Planning for the Unthinkable: Targeting strategies of nuclear-armed states" (Jun 2025)
  - "The Two-Hundred Billion Dollar Boondoggle" — Sentinel ICBM costs (Jun 2025)
  - Day One Memos: "Removing Arbitrary Deployment Quotas" (Dec 2024), "Saving Billions on US Nuclear Deterrent" (Dec 2024)
  - Nuclear Notebooks: India (2018), North Korea (2022), US (2023), Israel (2021)
- **Listed research area:** Nuclear-Climate Nexus
- **Critical connection:** Matt is a co-author of Toon et al. 2019 — the primary source paper for soot injection scenarios 5, 16, 27, 37, 47 Tg in the NWDC dataset. He literally helped produce the conflict scenarios our data is built on.
- **From Jan 2025 meeting:** Requested country-level datasets; framed as "Do you want to know what happens to your country in a nuclear winter?"

### Ollie Stephenson
- **Role:** Associate Director, AI & Emerging Tech Policy, FAS
- **Background:** PhD + MS Geophysics (Caltech, deep learning for disaster response); BS + MS Physics (Cambridge); worked on Capitol Hill advising on S&T policy
- **Core work:** AI governance, national security + AI, responsible AI adoption
- **Relevant intersections:**
  - Spoke at PSR "AI and Nuclear Weapons" event (Apr 2025)
  - WIRED piece on Anthropic AI + nuclear weapons prevention (Oct 2025)
  - Geophysics PhD = can engage with climate model outputs at a technical level (unusual in policy audience)
  - Deep learning for disaster response = natural bridge to NWDC's potential AI-assisted analysis extensions
- **From Jan 2025 meeting:** Suggested funding sources: AllFed, Open Philanthropy, FLI; discussed FAS as funding pass-through

---

## Key Overlap Angles for the Meeting

### Matt
1. **Scenarios ↔ his targeting analysis:** The 5–150 Tg range maps directly to conflicts he analyzes. "Planning for the Unthinkable" (2025) examines targeting strategies — those target sets determine soot loads, which determine which NWDC rows to look at. Direct bridge from his weapons-side work to consequences-side NWDC data.
2. **New START expiration ↔ 47→150 Tg consequences:** What does the difference in country-level outcomes look like if arsenals grow unconstrained? NWDC can quantify the *stakes* of arms control failure.
3. **Nuclear Notebook countries ↔ country reports:** India, Pakistan, China, Russia, US, UK, France, Israel, N. Korea — all the countries Matt has characterized in detail have NWDC data. The country report could be a direct companion to his arsenal profiles.
4. **Cross-scenario question hook:** "How would India & Pakistan fare in a US-Russia conflict?" and "How would the US & Russia fare in an India-Pakistan conflict?" — these flip the usual perspective and are analytically interesting.

### Ollie
1. **Geophysics background:** Can engage technically; can discuss model structure, aggregation choices, scenario design
2. **AI + NWDC:** Potential future direction — AI-assisted country report generation at scale, automated analysis pipelines, natural language querying of the dataset
3. **Funding:** AllFed, Open Philanthropy, FLI — he offered to help; this meeting is a chance to get specific
4. **Institutional home:** FAS as pass-through discussed in Jan 2025; revisit concretely

---

## Deliverables Needed Before Meeting

### 1. Country Report HTML ✓ DONE
**Spec:**
- Two tabs/pages: (a) **Policy Dashboard** and (b) **Analyst Dashboard**
- **Policy Dashboard:**
  - Audience: policymakers, generalists
  - Country chooser, scenario multi-select
  - PDF export → one-pager
  - Sexy, clean visualizations
  - Expandable text briefly explaining each scenario (e.g. "5 Tg = limited India-Pakistan conflict")
- **Analyst Dashboard:**
  - Audience: external analysts, researchers
  - More detailed visualizations, more controls
  - More explanatory text: units, variable definitions, technical notes
- **Demo countries:** India, Pakistan, US, Russia (for cross-scenario questions above)
- **Format:** Interactive HTML (can be emailed or Google Drive shared before meeting)
- **Status:** First version built, `d_context/meeting_prep/country_report_v2026-02-18.html`
- **Priority for next session:** Iterate on content and formatting — this is top priority before meeting

### 2. Meeting Deck (PPTX) — NOT DONE
- New file in repo (separate from existing brief)
- Update all file/directory references to current structure
- Update dataset list to include starvation and downwelling shortwave radiation
- Keep consistent with existing visual style
- Location: `d_context/meeting_prep/`

### 3. Session Plan & Speaking Points — NOT DONE
- ~60 min agenda
- Key moments: introduce updates, demo country report, cross-scenario questions, funding/institutional discussion
- Framing: NWDC as platform → broader community → policy impact → nuclear risk reduction

---

## Data Issue Diagnoses (completed this session)

### Issue 1A: `surface.radiation.mean` missing from downwelling output — DIAGNOSED & FIXED

**Root cause:** `ImportCSVsFromDirectory()` (`00_utils_import.R:158-161`) strips `_v{YYYY-MM-DD}` from filenames before returning the named list. The indicator detection in `downwelling_shortwave_radiation_cleaning.R:49` used `grepl("mean_v", source_table_name)`, which relied on the `_v` from the version suffix being present. After stripping, `nw_targets_01_FSDS_country_mean_v2025-12-19` → `nw_targets_01_FSDS_country_mean`, and `mean_v` no longer matches. All mean files received `indicator = NA`, `pivot_wider` created a `NA` column, and `select(any_of(...))` silently dropped it.

**Fix applied:** `downwelling_shortwave_radiation_cleaning.R:49` changed from:
```r
grepl("mean_v", source_table_name) & !grepl("mean_(max|min|stdev)", source_table_name) ~ "surface.radiation.mean",
```
to:
```r
grepl("_mean$", source_table_name) ~ "surface.radiation.mean",  # matches after version suffix stripped by ImportCSVsFromDirectory
```
Fix verified: all input filenames now resolve to correct indicators.

**Note:** The current standardized output CSV (`downwelling_shortwave_radiation_v2026-02-13.csv`) does not include `surface.radiation.mean`. Pipeline must be re-run to regenerate this file with the fix applied.

### Issue 1B: 47 Tg scenario milder than 27 Tg — DIAGNOSED, SOURCE DATA ANOMALY

**Finding:** The anomaly exists in the **raw input files**, not introduced by the pipeline. Afghanistan, year 1, month 7 example:

| Scenario | Max radiation (W/m²) |
|---|---|
| 27 Tg (`nw_targets_03_FSDS_country_mean_max_v2025-12-19.csv`) | 274.4 |
| 47 Tg (`nw_targets_04_FSDS_country_mean_max_v2025-12-19.csv`) | 322.9 ← higher |

**Hypothesis:** The `targets_04` simulation may use a different target set or ensemble member than `targets_03`, resulting in less stratospheric soot lofting despite nominally higher Tg loading. Could also be stochastic variability in the model's stratospheric transport.

**Action taken:** Drafted Slack message to co-authors asking for clarification (see below). No data change made; flagged as pending co-author response. Should be noted in the data readme.

**Slack message drafted:**
> Hi all — flagging a data anomaly in the downwelling shortwave radiation dataset. In the standardized output, the 47 Tg scenario shows *higher* downwelling surface solar radiation than the 27 Tg scenario for many countries/months. Confirmed this is in the raw input files (not the pipeline). [Full message with table and precise questions written — see current session chat for copy-pasteable text.]

**Key questions for co-authors:**
1. Is this expected from the model runs — different target sets between `targets_03` and `targets_04`?
2. Could this be stochastic stratospheric transport variability between ensemble members?
3. Is `nw_targets_04` from the same paper (Toon et al. 2019) or a different simulation?
4. Is there a scenario description document mapping `targets_01–04` to conflict parameters?
5. Should we flag this in the readme or expect a corrected file?

### Issue 2: Starvation 150 Tg "~15 billion starving" — NOT A BUG

**Finding:** The ~15B figure resulted from summing all rows for 150 Tg across all combinations. The 150 Tg scenario has **18 combinations** (2 trade × 3 livestock × 3 food-waste-reduction) vs. **6 combinations** for other scenarios — the extra food-waste-reduction levels were only modeled at 150 Tg. Naive sum triple-counts countries relative to other scenarios.

**Actual values:** Per-combination maximum (no trade, no livestock, 0% food waste reduction, 150 Tg) = 5.57B total globally — physically plausible. No individual country exceeds its 2010 population in any single combination.

**Action:** No data fix needed. Should add a note to the readme/variables table explaining that 150 Tg has 18 combinations while other scenarios have 6.

---

## Code Changes (this session)

### `c_scripts/3_standardize/downwelling_shortwave_radiation_cleaning.R`
- Line 49: fixed indicator detection regex from `grepl("mean_v", ...)` to `grepl("_mean$", ...)`
- Reason: `ImportCSVsFromDirectory` strips `_v{date}` suffix; old pattern relied on that suffix

### `d_context/meeting_prep/country_report_v2026-02-18.html` (NEW FILE)
- Self-contained interactive HTML report (755 KB including embedded data)
- 193-country selector; 7 scenarios; Chart.js 4.x; dark theme
- **Policy Dashboard tab:** Stat cards (temp Δ, wheat Δ, % starving at 47 Tg), 4 charts (temp annual, starvation bar, wheat annual, crops summary bar at 15 years), expandable scenario descriptions, PDF export
- **Analyst Dashboard tab:** Temperature annual time series, 4 crop charts (wheat/corn/rice/soy annual), starvation summary table, country-2 comparison overlay, data source note
- Data embedded as compact JSON (740 KB): temperature (yearly averages), agriculture CLM (pct change by crop/year), starvation (% starving by scenario)

---

## Session Status

**Status:** Partial — country report v1 built, data issues diagnosed, cleaning fix applied. Meeting deck and session plan not started.

---

## Next Session Priorities (TOP PRIORITY ORDER)

1. **[TOP PRIORITY] Iterate on country report HTML content and formatting** — polish for meeting demo
   - Review charts, labels, color coding for clarity
   - Ensure scenario descriptions are prominent and readable
   - Test PDF export output (does it produce a clean one-pager?)
   - Consider: cross-scenario comparison framing ("India in US-Russia conflict")
   - Consider: adding precipitation data
2. **Create meeting deck PPTX** — new file, updated structure/dataset list
3. **Write session plan and speaking points** — 60-min agenda, key framing moments
4. **Re-run downwelling pipeline** to regenerate CSV with `surface.radiation.mean` fix applied, then push to OSF
5. **Send Slack message to co-authors** re: 47 Tg anomaly (drafted this session, ready to send)
6. **Add readme note** re: starvation 150 Tg combination structure (18 vs 6 combos)

---

**Last Updated:** 2026-02-18 (Part B/C complete)

# Dataset Readme

## Metadata

**Identifier:** https://osf.io/e28gq

**Creators:** Harrison, Cheryl, William Faulkner, Joshua Coupe, E. Kesse Asante, Charles Bardeen, Victoria Garza, Jonas Jägermeyr, Nicole S. Lovenduski, Alan Robock, Karen Rojas, Kim Scherrer, O. Brian Toon, and Lili Xia

**Dataset correspondence:** william@fluxrme.com

**Title:** Accessible Climate and Impact Model Output for Studying the Human and Environmental Impacts of Nuclear Conflict

**Publisher:** Open Science Framework

**Publication year:** 2025

**Resource type:** Dataset

---

## Datasets Included

This repository contains 9 standardized datasets derived from climate and impact models simulating nuclear conflict scenarios. All datasets are organized by:
- **Soot injection scenarios:** 0 Tg (control), 5 Tg, 16 Tg, 27 Tg, 37 Tg, 47 Tg, 150 Tg
- **Time dimension:** Years and months elapsed from conflict onset
- **Spatial dimension:** Country (most datasets), Exclusive Economic Zone or EEZ (fish catch), or port (sea ice)

**Important note on sources:** The **Source** field listed under each dataset below refers to the **analysis and discussion publication** where results from that dataset were analyzed and discussed (see Table S1 below). For some datasets (particularly agriculture, fish catch, starvation, UV, and sea ice), the Earth system simulation was performed for a different publication, but the specific impact analysis (e.g., crop yields, fishery changes) was conducted in the cited source. For temperature and precipitation datasets, the Earth system simulation reference and the analysis & discussion publication are the same.

**Current datasets (alphabetically):**

1. `agriculture_agmip_v2026-02-13.csv` - Crop yield changes (AGMIP model)
2. `agriculture_clm_v2026-02-13.csv` - Crop yield changes (CLM model)
3. `fish_catch_v2026-02-13.csv` - Marine fishery catch changes by EEZ
4. `precipitation_v2026-02-13.csv` - Precipitation rate and variability
5. `sea_ice_v2026-02-13.csv` - Sea ice extent by port
6. `starvation_v2026-02-13.csv` - Population starvation estimates
7. `surface_solar_radiation_v2026-02-20.csv` - Incoming solar radiation at surface (FSDS)
8. `temperature_v2026-02-13.csv` - Surface temperature and variability
9. `uv_radiation_v2026-02-13.csv` - UV radiation indices

### Table S1. Main Data Product - Dataset Dimensions & Associated Publications

Geographic and time unit columns denote the smallest (most disaggregated) unit available in the dataset. The climate forcing scenario represents the total mass of black carbon soot lofted into the stratosphere in teragrams (Tg). The penultimate column displays the publication for which the Earth system modeling was originally performed. For papers on temperature and precipitation, this column will be the same as the final column. For all other themes, additional sub-model components (e.g., for crops) were run using the outputs from the Earth systems model and the results discussed in separate papers. These updated analysis and discussion papers are shown in the final column.

| Theme | Dataset Tab/File Name | Geographic Unit | Time Unit | Climate Forcing Scenario (Tg) | Earth System Simulation Reference | Analysis & Discussion Publication |
|---|---|---|---|---|---|---|
| Temperature | temperature | Country | Month | 150, 47, 37, 27, 16, 5, 0 | Coupe et al. (2019), Toon et al. (2019) | Coupe et al. (2019), Toon et al. (2019) |
| Precipitation | precipitation | Country | Month | 150, 47, 37, 27, 16, 5, 0 | Coupe et al. (2019), Toon et al. (2019) | Coupe et al. (2019), Toon et al. (2019) |
| UV | uv_radiation | Country | Month | 150, 0 | Coupe et al. (2019) | Bardeen et al. (2021) |
| Surface Solar Radiation | surface_solar_radiation | Country | Month | 150, 37, 27, 16, 5, 0 | Coupe et al. (2019), Toon et al. (2019) | Coupe et al. (2019), Toon et al. (2019) |
| Agriculture | agriculture_agmip | Country | Year | 5 | Mills et al. (2014), Toon et al. (2019) | Jägermeyr et al. (2020) |
| Agriculture | agriculture_clm | Country | Year | 150, 47, 37, 27, 16, 5 | Coupe et al. (2019), Toon et al. (2019) | Xia et al. (2022) |
| Fish Catch | fish_catch | EEZ | Year | 150, 47, 37, 27, 16, 5, 0 | Coupe et al. (2019), Toon et al. (2019) | Scherrer et al. (2020) |
| Starvation | starvation | Country | Year 2 only | 150, 47, 37, 27, 16, 5, 0 | — | Xia et al. (2022) |
| Sea Ice | sea_ice | Port | Month | 150, 47, 37 | Coupe et al. (2019), Toon et al. (2019) | Harrison et al. (2022) |

---

## Dataset-Specific Notes

### agriculture_agmip

Crop yield percent changes modeled using the Agricultural Model Intercomparison and Improvement Project (AgMIP) framework. Includes corn, rice, wheat, and soya beans.

This dataset uses a multi-model ensemble approach (see variable 'cesm.model.configuration') to test whether after a regional nuclear conflict scenario (5 Tg of BC in the stratosphere) the changes in agricultural yields would be detectable, given the variabilities in models and model configurations. The projected changes in crop yields are averaged across six global process-based crop models (EPICBOKU, GEPIC, LPJmL, pDSSAT, PEPIC, and PROMET) from the Agriculture Model Intercomparison and Improvement Project (AgMIP; Rosenzweig et al. 2017) and two ESM simulations with different CESM model configurations (Toon et al. 2019; Mills et al. 2014). The ESM forcing for this dataset has been bias-corrected using observational data, so that it has high fidelity with historical agriculture timeseries.

**Source:** Jägermeyr, Jonas, Alan Robock, Joshua Elliott, Christoph Müller, Lili Xia, Nikolay Khabarov, Christian Folberth, Erwin Schmid, Wenfeng Liu, Florian Zabel, Sam S. Rabin, Michael J. Puma, Alison Heslin, James Franke, Ian Foster, Senthold Asseng, Charles G. Bardeen, Owen B. Toon, and Cynthia Rosenzweig. 2020. "A Regional Nuclear Conflict Would Compromise Global Food Security." _Proceedings of the National Academy of Sciences_ 117 (13): 7071–81. https://doi.org/10.1073/pnas.1919049117


### agriculture_clm

Crop yield percent changes modeled using the Community Land Model (CLM). Includes corn, rice, spring wheat, soy, and livestock pasture grass. This dataset includes all nuclear conflict scenarios and is best used to compare different levels of nuclear-conflict-driven cooling on impacts related to food availability.

**Source:** Xia, Lili, Alan Robock, Kim Scherrer, Harrison Coupe, Joshua Coupe, Samantha Stevenson, Charles G. Bardeen, Alan Robock, and Owen B. Toon. 2022. "Global Food Insecurity and Famine from Reduced Crop, Marine Fishery and Livestock Production Due to Climate Disruption from Nuclear War Soot Injection." _Nature Food_ 3 (8): 586–96. https://doi.org/10.1038/s43016-022-00573-0.

**Notes:**
- All `pct.change.harvest.yield.[crop]` indicators were generated during the research published in Xia et al. 2022 (cited above).
- All `avg.yield.[crop]` indicators represent a mean of the country's yield for that crop from 2000-2020. Data was sourced from "Food and Agriculture Organization of the United Nations Statistical Database (FAOSTAT)." n.d. Version Revision 2025-06-11. Accessed August 1, 2025. https://www.fao.org/faostat/en/#data. Taking the average of yields across years is important to smooth out year-to-year differences in crop yields due to normal climate variations. The simple 20-year mean yield does, however, obscure trends in crop yields resulting from changes in agronomic practices, demographics, the political context, etc., that may have been occurring in a given country during the 2000-2020 period.
- Crop `livestock.pasture.grass`: Grass leaf carbon is used to estimate pasture change, and simulated crop production change is used to estimate animal feed from grains. Average animal feed has ratio of 46% grass to 54% crops (Xia et al. 2022, Supplemental Materials). Benchmark yield data for pasture grass is not available in a worldwide standardized dataset from FAOSTAT or otherwise, and hence not included in this data.

### fish_catch

Marine fishery catch changes by Exclusive Economic Zone (EEZ). Includes mean catch per 1000 km², percent change in catch, and standard deviation of percent change.

**Source:** Scherrer, Kim J. N., Cheryl S. Harrison, Ryan F. Heneghan, Eric Galbraith, Charles G. Bardeen, Joshua Coupe, Jonas Jägermeyr, Nicole S. Lovenduski, Andrea Luna, Alan Robock, John Stevenson, Samantha Stevenson, Owen B. Toon, and Lili Xia. 2020. "Marine Wild-Capture Fisheries after Nuclear War." _Proceedings of the National Academy of Sciences_ 117 (47): 29748–58. https://doi.org/10.1073/pnas.2008256117


### precipitation

Country-level precipitation rate (mm/month) and standard deviation. Derived from CESM1(WACCM4) climate model simulations.

**Source:** Toon, Owen B., Charles G. Bardeen, and Alan Robock. 2019. "Rapidly Expanding Nuclear Arsenals in Pakistan and India Portend Regional and Global Catastrophe." _Science Advances_ 5 (10): eaay5478. https://doi.org/10.1126/sciadv.aay5478

### sea_ice

Sea ice extent by port location. Useful for analyzing impacts on shipping routes and port accessibility.

**Source:** Harrison, Cheryl S., Tyler Rohr, Alice DuVivier, et al. 2022. "A New Ocean State After Nuclear War." _AGU Advances_ 3 (4): e2021AV000610. https://doi.org/10.1029/2021AV000610.

### starvation

Country-level population starvation estimates. Includes multiple sub-scenarios for trade status (with trade / no trade), livestock availability (livestock / partial livestock / no livestock), and food waste reduction (0% / 50% / 100%).

All estimates represent conditions in the second year (Year 2) following the conflict — the period of peak food shortfall in Xia et al. (2022). The dataset is a single-year snapshot and therefore has no time column.

**Notes:**
- The 150 Tg scenario includes all 18 sub-scenario combinations (2 trade × 3 livestock × 3 food waste reduction levels).
- Other scenarios (5, 16, 27, 37, 47 Tg) include only 6 sub-scenario combinations (2 trade × 3 livestock × 1 food waste reduction level = 0% only).
- When aggregating across sub-scenarios, be careful not to double-count populations. Use only one sub-scenario combination per analysis.

**Source:** Xia, Lili, Alan Robock, Kim Scherrer, et al. 2022. "Global Food Insecurity and Famine from Reduced Crop, Marine Fishery and Livestock Production Due to Climate Disruption from Nuclear War Soot Injection." _Nature Food_ 3 (8): 586–96. https://doi.org/10.1038/s43016-022-00573-0.

### surface_solar_radiation

Incoming solar radiation at the Earth's surface (variable: FSDS). Includes mean, minimum, maximum, and standard deviation.

**Technical details:**
- **Variable name:** FSDS (Flux, Shortwave, Downwelling, Surface)
- **Accurate description:** Incoming solar flux / insolation / incoming solar radiation
- **Wavelength range:** 200–12,200 nm (includes >90% shortwave plus small longwave component)
- **Units:** W/m² (Watts per meter squared)
- **Model:** Calculated using RRTMG radiative transfer model within CESM1(WACCM4)

While commonly called "shortwave radiation," this variable actually integrates across all radiation wavelengths from 200 nm to 12,200 nm, thus including a small longwave component in addition to the dominant shortwave radiation.

**Available scenarios:** 0, 5, 16, 27, 37, 150 Tg (47 Tg source files not available)

**Source:** Toon, Owen B., Charles G. Bardeen, and Alan Robock. 2019. "Rapidly Expanding Nuclear Arsenals in Pakistan and India Portend Regional and Global Catastrophe." _Science Advances_ 5 (10): eaay5478. https://doi.org/10.1126/sciadv.aay5478

### temperature

Country-level surface temperature (°C) and standard deviation. Derived from CESM1(WACCM4) climate model simulations.

**Difference in simulation lengths:** With respect to the other soot injection scenarios, the 150 Tg case involves unique feedback loops with polar ice which prevent temperatures from recovering to pre-conflict levels. The greater length of the 150 Tg scenario simulation (28 years vs. 13 years for other scenarios and 19 years for the control scenario) represents an effort to better understand the newly stable climate state. For more, see: Harrison et al. 2022 (cited below).

**Source:** Toon, Owen B., Charles G. Bardeen, and Alan Robock. 2019. "Rapidly Expanding Nuclear Arsenals in Pakistan and India Portend Regional and Global Catastrophe." _Science Advances_ 5 (10): eaay5478. https://doi.org/10.1126/sciadv.aay5478

### uv_radiation

Country-level UV radiation indices (UVA, UVB, UV Index, UV Index Max). Changes in stratospheric ozone following nuclear conflict lead to increased UV radiation at the surface.

**Source:** Bardeen, Charles G., Douglas E. Kinnison, Owen B. Toon, et al. 2021. "Extreme Ozone Loss Following Nuclear War Results in Enhanced Surface Ultraviolet Radiation." _Journal of Geophysical Research: Atmospheres_ 126 (18): e2021JD035079. https://doi.org/10.1029/2021JD035079.

---

## Weighted Averages

When averaging multiple units of analysis in this data, it is important to think about simple vs. weighted averages. For example, if you want to see average temperature trends by region (groups of countries), a simple average will treat all countries as if they were the same size and could be misleading. A good rule of thumb is to use a weighted average when each data point represents a different-sized unit. Below is a table of potential ways to use weighted averages with this data.

| Dataset | Unit of Analysis | Variable(s) to Use for Weighting |
|---------|------------------|----------------------------------|
| agriculture_agmip | country | country.land.area.sq.km or country.agricultural.land.area.sq.km |
| agriculture_clm | country | country.land.area.sq.km or country.agricultural.land.area.sq.km |
| fish_catch | EEZ | eez.area.sq.km |
| precipitation | country | country.land.area.sq.km |
| sea_ice | port | N/A (discrete port locations) |
| starvation | country | country.population.2018 |
| surface_solar_radiation | country | country.land.area.sq.km |
| temperature | country | country.land.area.sq.km |
| uv_radiation | country | country.land.area.sq.km |

---

## Scenario Descriptions

All scenarios simulate the climate and environmental impacts following injection of black carbon soot into the stratosphere from firestorms ignited by nuclear detonations. The soot injection scenarios correspond to different conflict scales:

- **0 Tg (control):** No nuclear conflict; baseline climate conditions
- **5 Tg:** Small-scale regional conflict (e.g., limited India-Pakistan conflict)
- **16 Tg:** Moderate regional conflict
- **27 Tg:** Large regional conflict
- **37 Tg:** Very large regional conflict
- **47 Tg:** Near-global-scale conflict
- **150 Tg:** Full-scale US-Russia nuclear war

**Note:** Dataset availability varies by scenario. The 47 Tg scenario is currently unavailable for surface_solar_radiation due to missing source files. Most other datasets include all or most scenarios.

---

## Citations

### Analysis & Discussion Publications (Primary Sources)

These are the publications where data from each theme were analyzed and discussed (see Table S1 above).

Bardeen, Charles G., Douglas E. Kinnison, Owen B. Toon, Ryan P. Thornberry, Andrew W. Rollins, Pengfei Yu, Eric J. Jensen, Michael J. Mills, Brian M. Lazar, Charles H. Jackman, Samantha M. Carstens, David W. Fahey, and Ru-Shan Gao. 2021. "Extreme Ozone Loss Following Nuclear War Results in Enhanced Surface Ultraviolet Radiation." _Journal of Geophysical Research: Atmospheres_ 126 (18): e2021JD035079. https://doi.org/10.1029/2021JD035079

Coupe, Joshua, Charles G. Bardeen, Alan Robock, and Owen B. Toon. 2019. "Nuclear Winter Responses to Nuclear War Between the United States and Russia in the Whole Atmosphere Community Climate Model Version 4 and the Goddard Institute for Space Studies ModelE." _Journal of Geophysical Research: Atmospheres_ 124 (15): 8522–43. https://doi.org/10.1029/2019JD030509

Harrison, Cheryl S., Tyler Rohr, Alice DuVivier, Nicole S. Lovenduski, Joshua Coupe, Charles G. Bardeen, Samantha Stevenson, Owen B. Toon, and Alan Robock. 2022. "A New Ocean State After Nuclear War." _AGU Advances_ 3 (4): e2021AV000610. https://doi.org/10.1029/2021AV000610

Jägermeyr, Jonas, Alan Robock, Joshua Elliott, Christoph Müller, Lili Xia, Nikolay Khabarov, Christian Folberth, Erwin Schmid, Wenfeng Liu, Florian Zabel, Sam S. Rabin, Michael J. Puma, Alison Heslin, James Franke, Ian Foster, Senthold Asseng, Charles G. Bardeen, Owen B. Toon, and Cynthia Rosenzweig. 2020. "A Regional Nuclear Conflict Would Compromise Global Food Security." _Proceedings of the National Academy of Sciences_ 117 (13): 7071–81. https://doi.org/10.1073/pnas.1919049117

Scherrer, Kim J. N., Cheryl S. Harrison, Ryan F. Heneghan, Eric Galbraith, Charles G. Bardeen, Joshua Coupe, Jonas Jägermeyr, Nicole S. Lovenduski, Andrea Luna, Alan Robock, John Stevenson, Samantha Stevenson, Owen B. Toon, and Lili Xia. 2020. "Marine Wild-Capture Fisheries after Nuclear War." _Proceedings of the National Academy of Sciences_ 117 (47): 29748–58. https://doi.org/10.1073/pnas.2008256117

Toon, Owen B., Charles G. Bardeen, and Alan Robock. 2019. "Rapidly Expanding Nuclear Arsenals in Pakistan and India Portend Regional and Global Catastrophe." _Science Advances_ 5 (10): eaay5478. https://doi.org/10.1126/sciadv.aay5478

Xia, Lili, Alan Robock, Kim Scherrer, Harrison Coupe, Joshua Coupe, Samantha Stevenson, Charles G. Bardeen, and Owen B. Toon. 2022. "Global Food Insecurity and Famine from Reduced Crop, Marine Fishery and Livestock Production Due to Climate Disruption from Nuclear War Soot Injection." _Nature Food_ 3 (8): 586–96. https://doi.org/10.1038/s43016-022-00573-0

### Earth System Simulation References

These publications describe the original Earth system modeling simulations that provided forcing data for the impact analyses above.

Mills, Michael J., Owen B. Toon, Julia Lee-Taylor, and Alan Robock. 2014. "Multidecadal Global Cooling and Unprecedented Ozone Loss Following a Regional Nuclear Conflict." _Earth's Future_ 2 (4): 161–76. https://doi.org/10.1002/2013EF000205

Rosenzweig, Cynthia, Joshua Elliott, Delphine Deryng, Alex C. Ruane, Christoph Müller, Almut Arneth, Kenneth J. Boote, Christian Folberth, Michael Glotter, Nikolay Khabarov, Katja Neumann, Franziska Piontek, Thomas A. M. Pugh, Erwin Schmid, Elke Stehfest, Hong Yang, and James W. Jones. 2017. "Assessing Agricultural Risks of Climate Change in the 21st Century in a Global Gridded Crop Model Intercomparison." _Proceedings of the National Academy of Sciences_ 111 (9): 3268–73. https://doi.org/10.1073/pnas.1222463110

### This Dataset

_[Add citation for this dataset once published]_

---

**Last Updated:** 2026-02-20
**Version:** v2026-02-20

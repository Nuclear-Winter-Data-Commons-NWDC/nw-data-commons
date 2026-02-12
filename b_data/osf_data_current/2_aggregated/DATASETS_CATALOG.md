# Nuclear Winter Data Commons - Dataset Catalog

**Last Updated:** 2026-02-12

This catalog describes all available aggregated datasets in the Nuclear Winter Data Commons.

---

## How to Use This Catalog

**Dataset Organization:**
- Datasets are organized by scientific aspect (physical climate, biological impacts, human consequences)
- Each dataset has a unique identifier following the pattern: `{aspect}_{model}_{contributor}`
- Multiple datasets may exist for the same aspect using different models or from different contributors

**File Formats:**
- **xlsx**: Single Excel workbook with multiple sheets (one per scenario)
- **csv_multi**: Multiple CSV files, typically one per scenario

**Scenarios:**
- **5Tg**: Small-scale nuclear conflict (~100 weapons)
- **16Tg**: India-Pakistan regional war scenario
- **27Tg**: Medium-scale conflict
- **47Tg**: Large regional conflict
- **150Tg**: US-Russia full-scale war
- **control**: Baseline scenario with no nuclear conflict

---

## Physical Climate Impacts

### Temperature (CESM)
- **ID:** `temperature_cesm_harrison`
- **Model:** CESM (Community Earth System Model)
- **Contributor:** Harrison et al.
- **Unit of Analysis:** Country
- **Scenarios:** 5Tg, 16Tg, 27Tg, 47Tg, 150Tg
- **Variables:**
  - Surface temperature mean
  - Surface temperature standard deviation
- **Location:** `/3_aggregated/temperature_cesm_harrison/`
- **Status:** ✅ Active

### Precipitation (CESM)
- **ID:** `precipitation_cesm_harrison`
- **Model:** CESM
- **Contributor:** Harrison et al.
- **Unit of Analysis:** Country
- **Scenarios:** 5Tg, 16Tg, 27Tg, 47Tg, 150Tg
- **Variables:**
  - Precipitation rate
  - Precipitation standard deviation
- **Location:** `/3_aggregated/precipitation_cesm_harrison/`
- **Status:** ✅ Active

### UV Radiation (CESM-TUV)
- **ID:** `uv_radiation_cesm_coupe`
- **Model:** CESM with TUV radiative transfer model
- **Contributor:** Coupe et al.
- **Unit of Analysis:** Country
- **Scenarios:** 150Tg, control
- **Variables:**
  - UV index
  - UV index maximum
  - UVA, UVB, UVC
- **Location:** `/3_aggregated/uv_radiation_cesm_coupe/`
- **Status:** ✅ Active

### Sea Ice Thickness
- **ID:** `seaice_thickness_coupe`
- **Model:** CESM
- **Contributor:** Coupe et al.
- **Unit of Analysis:** Port location
- **Scenarios:** 150Tg
- **Variables:**
  - Ice thickness
  - Port accessibility
- **Location:** `/3_aggregated/seaice_thickness_coupe/`
- **Status:** ✅ Active
- **Notes:** Used for assessing maritime trade route impacts

### Downwelling Solar Flux at Surface
- **ID:** `downwelling_solar_flux_cesm_harrison`
- **Model:** CESM
- **Contributor:** Harrison et al.
- **Unit of Analysis:** Country
- **Scenarios:** 5Tg, 16Tg, 27Tg, 47Tg, 150Tg, control
- **Variables:**
  - FSDS mean
  - FSDS minimum
  - FSDS maximum
  - FSDS standard deviation
- **Location:** `/3_aggregated/downwelling_solar_flux_cesm_harrison/`
- **Status:** 🔄 Pending (being processed)

---

## Biological Impacts

### Agriculture (AgMIP)
- **ID:** `agriculture_agmip_jonas`
- **Model:** AgMIP multi-model ensemble
- **Contributor:** Jonas et al.
- **Unit of Analysis:** Country
- **Scenarios:** 5Tg only
- **Variables:**
  - Percent change in corn yield
  - Percent change in rice yield
  - Percent change in wheat yield
  - Percent change in soybean yield
- **Location:** `/3_aggregated/agriculture_agmip_jonas/`
- **Status:** ✅ Active

### Agriculture (CLM)
- **ID:** `agriculture_clm_harrison`
- **Model:** CLM (Community Land Model)
- **Contributor:** Harrison et al.
- **Unit of Analysis:** Country
- **Scenarios:** 5Tg, 16Tg, 27Tg, 47Tg, 150Tg
- **Variables:**
  - Percent change in corn yield
  - Percent change in rice yield
  - Percent change in spring wheat yield
  - Percent change in soy yield
  - Percent change in livestock pasture grass yield
- **Location:** `/3_aggregated/agriculture_clm_harrison/`
- **Status:** ✅ Active

### Fisheries Catch (DBEM v2)
- **ID:** `fisheries_dbem_kim_v2`
- **Model:** DBEM (Dynamic Bioclimate Envelope Model)
- **Contributor:** Kim et al.
- **Unit of Analysis:** Exclusive Economic Zone (EEZ)
- **Scenarios:** 5Tg, 16Tg, 27Tg, 47Tg, 150Tg
- **Variables:**
  - Mean catch per 1000 sq km
  - Mean percent catch change
  - Standard deviation of percent catch change
- **Location:** `/3_aggregated/fisheries_dbem_kim_v2/`
- **Format:** Multiple CSV files (one per scenario: `output_v2_BAU_*tg.csv`)
- **Status:** ✅ Active
- **Notes:** Replaces v1 which had outlier issues

### ~~Fisheries Catch (DBEM v1)~~ [DEPRECATED]
- **Status:** ⛔ Deprecated - Do not use
- **Reason:** Replaced by v2 due to data quality issues
- **Replacement:** Use `fisheries_dbem_kim_v2` instead

---

## Human Consequences

### Starvation Projections
- **ID:** `starvation_python_xia`
- **Model:** PYTHON food security model
- **Contributor:** Xia et al.
- **Unit of Analysis:** Country
- **Scenarios:** 5Tg, 16Tg, 27Tg, 47Tg, 150Tg
- **Variables:**
  - Number of people starving
  - Percent of population starving (2010 baseline)
  - Country population (2010)
- **Location:** `/3_aggregated/starvation_python_xia/`
- **Status:** ✅ Active
- **Notes:** Includes multiple trade and adaptation scenarios (baseline, optimized trade, livestock feed reduction, food waste reduction)

---

## Dataset Status Indicators

- ✅ **Active**: Current, quality-controlled dataset ready for use
- 🔄 **Pending**: Dataset is being processed or integrated
- ⛔ **Deprecated**: Superseded by newer version, do not use for new analyses
- 🚧 **In Progress**: Dataset is being developed or updated

---

## Adding New Datasets

If you are a contributor adding a new dataset:

1. **Choose a descriptive ID** following the pattern: `{aspect}_{model}_{contributor}`
   - Example: `cloud_cover_cesm_smith`

2. **Organize your files** in a directory named with your dataset ID

3. **Include a README** in your dataset directory explaining:
   - Data sources and methods
   - Variable definitions
   - Known limitations
   - Citation information

4. **Register your dataset** by contacting the data manager or submitting a pull request

---

## Questions or Issues?

For questions about specific datasets, consult the README files in each dataset directory or contact:
- **Data Manager:** William Faulkner (william@fluxrme.com)
- **GitHub Issues:** https://github.com/wnfaulkner/nw-data-commons/issues

---

## Citation

If you use data from this repository, please cite:

Harrison, C. et al. (2024). "Accessible Climate and Impact Model Output for Studying the Human and Environmental Impacts of Nuclear Conflict." *EarthArXiv*. https://doi.org/10.31223/X5XB20

Additional citations may be required for specific datasets - see individual dataset README files.

---

**Version:** 1.0
**Schema Version:** 1.0
**Last Updated:** 2026-02-12

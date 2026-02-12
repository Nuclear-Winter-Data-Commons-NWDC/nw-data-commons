# Dataset Readme

## Metadata

**Metadata**
**Identifier:** https://osf.io/e28gq
**Creators:** Harrison, Cheryl, William Faulkner, Joshua Coupe, E. Kesse Asante, Charles Bardeen, Victoria Garza, Jonas Jägermeyr, Nicole S. Lovenduski, Alan Robock, Karen Rojas, Kim Scherrer, O. Brian Toon, and Lili Xia
**Dataset correspondence:** william@fluxrme.com
**Title:** Accessible Climate and Impact Model Output for Studying the Human and Environmental Impacts of Nuclear Conflict
**Publisher:**
**Publication year:** 2025
**(Resource type):**

---

## Dataset-Specific Notes

### temperature

Difference in length of simulations: With respect to the other soot injection scenarios, the 150Tg case involves unique feedback loops with polar ice which prevent temperatures from recovering to pre-conflict levels. The greater length of the 150Tg scenario simulation (28 years vs. 13 years for the others and 19 years for the control scenario) represents an effort to better understand the newly stable climate state. For more, see: Harrison, Cheryl S., Tyler Rohr, Alice DuVivier, et al. 2022. “A New Ocean State After Nuclear War.” AGU Advances 3 (4): e2021AV000610. https://doi.org/10.1029/2021AV000610.

### precipitation

_No notes provided._

### uv

_No notes provided._

### agriculture.clm

- All 'pct.change.harvest.yield.[crop]' indicators were generated during the research published in Xia, Lili, Alan Robock, Kim Scherrer, et al. 2022. “Global Food Insecurity and Famine from Reduced Crop, Marine Fishery and Livestock Production Due to Climate Disruption from Nuclear War Soot Injection.” Nature Food 3 (8): 586–96. https://doi.org/10.1038/s43016-022-00573-0.

- All 'avg.yield.[crop]' indicators represent a mean of the country's yield for that crop from 2000-2020. Data was sourced from “Food and Agriculture Organization of the United Nations Statistical Database (FAOSTAT).” n.d. Version Revision 2025-06-11. Accessed August 1, 2025. https://www.fao.org/faostat/en/#data. Taking the average of yields across years is important to smooth out year-to-year differences in crop yields due to normal climate variations. The simple 20-year mean yield does, however, obscure trends in crop yields resulting from changes in agronomic practices, demographics, the political context, etc., that may have been ocurring in a given country during the 2000-2020 period.

- Crop 'livestock.pasture.grass': Grass leaf carbon is used to estimate pasture change, and simulated crop production change is used to estimate animal feed from grains. Average animal feed has ratio of 46% grass to 54% crops. (Xia, Lili, Alan Robock, Kim Scherrer, et al. 2022, Supplemental Materials). Benchmark yield data for pasture grass is not available in a worldwide standardized dataset from FAOSTAT or otherwise, and hence not included in this data.

### agriculture.agmip

_No notes provided._

### fish.catch

_No notes provided._

### sea.ice

_No notes provided._


---

## Weighted Averages

When averaging multiple units of analysis in this data, it is important to think about simple vs. weighted averages. For example, if you want to see average temperature trends by region (groups of countries), a simple average will treat all countries as if they were the same size and could be misleading. A good rule of thumb is to use a weighted average when each data point represents a different-sized unit. Below is a table of potential ways to use weighted averages with this data.


| Table Name | Unit of Analysis | Variable(s) to Use for Weighting |
|------------|------------------|----------------------------------|
| temperature | country | surface.temp |
| temperature | country | surface.temp |
| precipitation | country | precipitation.mm |
| precipitation | country | precipitation.mm |
| uv | country | uvindex |
| uv | country | uvindex |
| agriculture.clm | country | pct.change.harvest.yield |
| agriculture.clm | country | pct.change.harvest.yield |
| agriculture.agmip | country | pct.change.harvest.yield |
| fish.catch | EEZ | total.catch.tons |
| fish.catch | EEZ | total.catch.tons |
| sea.ice | port | sea.ice.extent |

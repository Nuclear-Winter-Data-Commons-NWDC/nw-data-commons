#!/usr/bin/env bash
# Sync aggregated data from OSF to local with _current naming convention
# This downloads all current datasets from OSF and saves them with _current suffix locally

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

PYTHON=".venv/bin/python3"
OSF_MGR="c_scripts/1_download_or_extract/osf_manager.py"

echo "Syncing aggregated data from OSF..."

# Download all current datasets
"$PYTHON" "$OSF_MGR" download --remote /3_aggregated/temperature/temperature.xlsx --local b_data/3_aggregated/temperature/temperature_current.xlsx --overwrite
"$PYTHON" "$OSF_MGR" download --remote /3_aggregated/precipitation/precipitation.xlsx --local b_data/3_aggregated/precipitation/precipitation_current.xlsx --overwrite
"$PYTHON" "$OSF_MGR" download --remote /3_aggregated/uv_radiation/uv_radiation.xlsx --local b_data/3_aggregated/uv_radiation/uv_radiation_current.xlsx --overwrite
"$PYTHON" "$OSF_MGR" download --remote /3_aggregated/agriculture_agmip/agriculture_agmip.xlsx --local b_data/3_aggregated/agriculture_agmip/agriculture_agmip_current.xlsx --overwrite
"$PYTHON" "$OSF_MGR" download --remote /3_aggregated/agriculture_clm/agriculture_clm.xlsx --local b_data/3_aggregated/agriculture_clm/agriculture_clm_current.xlsx --overwrite
"$PYTHON" "$OSF_MGR" download --remote /3_aggregated/sea_ice/sea_ice.xlsx --local b_data/3_aggregated/sea_ice/sea_ice_current.xlsx --overwrite
"$PYTHON" "$OSF_MGR" download --remote /3_aggregated/starvation/starvation.xlsx --local b_data/3_aggregated/starvation/starvation_current.xlsx --overwrite

# Download deprecated fisheries v1
"$PYTHON" "$OSF_MGR" download --remote /3_aggregated/fisheries_v1/fisheries_v1.xlsx --local b_data/3_aggregated/fisheries_v1/fisheries_v1_current.xlsx --overwrite

# Download new fisheries CSVs
mkdir -p b_data/3_aggregated/fisheries
for scenario in 5tg 16tg 27tg 47tg 150tg; do
  "$PYTHON" "$OSF_MGR" download --remote "/3_aggregated/fisheries/output_v2_BAU_${scenario}.csv" --local "b_data/3_aggregated/fisheries/output_v2_BAU_${scenario}.csv" --overwrite
done

echo "✓ Sync complete"

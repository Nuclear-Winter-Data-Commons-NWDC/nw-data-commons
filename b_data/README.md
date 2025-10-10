Data layout for Nuclear Winter Data Commons

- b_data/1_scenario_definitions: versioned CSV configs committed to Git (e.g., source.table.configs.tb.csv, countries.tb.csv, osf_manifest.json).
- b_data/2_model_outputs: raw model outputs fetched from OSF (not tracked in Git).
- b_data/3_aggregated: intermediate aggregated tables, built locally by scripts (not tracked in Git).
- b_data/4_standardized: final harmonized tables, built locally by scripts (not tracked in Git - except specific published versions).

Data are hosted on the Open Science Framework (OSF). To reproduce:
1) Obtain an OSF Personal Access Token (PAT) and set it as OSF_TOKEN in your environment (or in a local .env file).
2) Edit b_data/1_scenario_definitions/osf_manifest.json to point to the correct OSF project and remote paths.
3) Fetch data: python c_scripts/1_download_or_extract/osf_fetch.py --manifest b_data/1_scenario_definitions/osf_manifest.json

# LGBTQI+ Hate Crime Analysis in the United States (1991–2024)

**A data science portfolio project by Guilherme Arpi**

---

## Overview

This project analyses LGBTQI+-motivated hate crime incidents reported to the FBI between 1991 and 2024. The core question: *What drives hate crime patterns against LGBTQI+ individuals in the US, and does state-level legal protection actually reduce victimisation?*

The project covers the full data science lifecycle — pipeline engineering, exploratory analysis, statistical testing, and machine learning — using R, with outputs designed for Tableau visualisation.

---

## Key Findings

**1. A statistically significant long-term increase in *recorded* incidents** exists across the 34-year period (Mann-Kendall Z = 4.79, p < 0.001, Sen's slope = +27 incidents/year). The trend is stronger than the 1991–2019 subset (+16/year), driven by sharp increases in 2020–2024. How much of this reflects rising victimisation versus rising reporting cannot be separated with this data — see [Data Limitations](#data-limitations--read-this-before-the-numbers).

**2. Anti-Transgender hate crimes rose 1,557%** between 1991 and 2024 (CAGR 8.9%) — the fastest-growing subgroup in absolute terms (68 → 1,127 incidents). Anti-Bisexual incidents rose 3,800% (CAGR 11.7%), but from a base of **one** recorded incident. Anti-Gay (Male) and Anti-Lesbian both grew ~265%.

> ⚠️ These growth figures conflate rising incidence with rising *reporting and classification*. Gender identity was not a separate FBI bias category before 2013. See **[Data Limitations](#data-limitations--read-this-before-the-numbers)** before citing them.

**3. Income inequality is the strongest positive predictor** of incident rates across both normalisation tracks. States with higher Gini coefficients report significantly more LGBTQI+ hate crimes — Track 1 OLS: β = 17.6 per 100k total pop. (t = 15.9, p < 0.001); Track 2 OLS: β = 310 per 100k LGBT adults (t = 10.9, p < 0.001). Model fit is moderate (Track 1 R² = 0.28, n = 1,533; Track 2 R² = 0.28, n = 630).

State population size is the strongest predictor overall but operates in the opposite direction (Track 1 β = −0.39, t = −17.4): larger states report *lower* per-capita rates, consistent with reporting infrastructure and population-denominator effects rather than lower incidence. Random Forest feature importance ranks Gini first (23.0) ahead of log population (20.8), so inequality is the dominant signal once non-linearity is allowed.

**4. State hate crime laws show a counterintuitive pattern**: state-years *with* hate crime law coverage report higher rates in both tracks (Track 1: mean 0.773 vs. 0.357, Cohen's d = −0.44, p < 0.001, n = 807 vs. 726; Track 2: mean 14.42 vs. 10.35, d = −0.30, p < 0.001). The gradient holds across all three coverage tiers — median Track 1 rate rises 0.282 (no law) → 0.449 (sexual orientation only) → 0.557 (full protection).

This is a well-documented *reporting effect* — legal frameworks create accountability structures that improve classification and reporting of incidents that would otherwise go unrecorded. Because law coverage is modelled as a **time-varying panel** (each state enters a tier in the year it actually adopted), this comparison is within-state over time rather than a static cross-section.

**5. Seasonal variation is significant** (χ² = 12.7, df = 3, p = 0.005), with Summer being the peak season for LGBTQI+ hate crimes.

**6. The year trend disappears once inequality, population and law coverage are controlled for** — the YEAR term is non-significant in *both* tracks (Track 1: β = 0.0024, p = 0.31; Track 2: β = 0.119, p = 0.39). The raw upward trend in Finding 1 is therefore largely absorbed by structural covariates, and in Track 2 specifically by a growing openly-identifying LGBT population, rather than reflecting a rising per-capita victimisation rate.

**7. ML models identify Gini and population size as the most predictive features**, but perform modestly at predicting individual incident violence (Random Forest accuracy: 64.0%, ROC AUC: 0.320; Decision Tree: 60.4%, ROC AUC: 0.398; both tuned via 5-fold CV). ROC AUC below 0.5 indicates the models rank violent vs. non-violent incidents worse than chance — an honest negative result. Violence is an individual-incident property, and aggregate state-year socioeconomic features carry little signal about it. Feature importance is the useful output here, not predictive performance.

**8. K-Means clustering reveals one extreme outlier cluster** (n = 21 state-year observations): mean rate 7.66 per 100k, Gini 0.541, 73.6% violent — driven by DC's exceptionally high Gini and small population denominator. The remaining three clusters separate cleanly by Gini level and violent share.

---

## Data Limitations — Read This Before the Numbers

**This analysis measures *recorded* hate crime, not *actual* hate crime.** The gap between the two is large, and it has narrowed considerably over the 34-year window. Every growth figure above should be read with that in mind.

**1. Reporting propensity changed enormously between 1991 and 2024.**
An incident in 1991 and an identical incident in 2024 were not equally likely to be reported by the victim, recorded by the responding officer, or classified as bias-motivated by the agency. Over this period: the Hate Crime Statistics Act reporting infrastructure matured, police training on bias-crime identification expanded, LGBTQI+ people became far more willing to report to police, and advocacy organisations began actively assisting with reporting. **A rising line in these charts reflects some combination of rising incidence and rising visibility, and this analysis cannot separate the two.**

**2. Anti-Transgender incidents were not a separate FBI category before 2013.**
The headline "+1,557% since 1991" starts from a period when gender identity bias had no dedicated reporting code — such incidents were recorded under other categories, or not at all. The post-2013 series (CAGR 8.9%) is the more defensible figure, and even that is affected by improving classification. The same applies to Anti-Gender Non-Conforming.

**3. Growth rates from near-zero bases are not meaningful.**
Anti-Bisexual incidents rose "3,800%" — from **1 recorded incident in 1991 to 39 in 2024**. That is a statement about the recording system, not about bisexual people's safety. Percentage growth is reported here for completeness but should not be quoted without the underlying counts.

**4. UCR participation is voluntary and uneven.**
Agencies are not required to submit hate crime data, and many that do submit report zero incidents annually. Coverage improved over the period, which independently inflates the apparent trend. State-level comparisons are therefore partly comparisons of *reporting infrastructure*, which is exactly the mechanism behind the law-coverage finding (#4).

**5. The LGBT population denominator has the same problem.**
Track 2 divides incidents by self-identified LGBT adults (Williams Institute / Gallup). That denominator grew rapidly — but substantially because disclosure became safer, not only because the population changed. Since both numerator and denominator are inflated by increasing openness, the Track 2 rate is a **conservative** measure: a flat per-capita rate is compatible with either a real decline or a real increase in underlying victimisation. This is also why pre-2012 figures were deliberately left blank rather than backfilled.

**What this means for the findings**

| More defensible | Treat with caution |
|---|---|
| Cross-sectional relationships (inequality × rate, law coverage × rate) | Absolute growth percentages over the full window |
| Within-year comparisons between states | Any claim that victimisation *itself* rose ~X% |
| Post-2013 subgroup composition | Pre-2013 subgroup breakdowns |
| Seasonality (stable reporting conditions within a year) | Year-over-year change in early years |

The reporting effect is not a flaw to be corrected away — it *is* one of the findings. Finding 4 shows it directly: states that built legal accountability structures record higher rates precisely because they classify better. The honest reading of this dataset is that it measures the visibility of anti-LGBTQI+ violence as much as its incidence.

---

## Project Structure

```
LGBTQI-Hate-Crime-Analysis/
│
├── R/                           ← Analysis scripts + notebooks (same code, two formats)
│   ├── 01_data_pipeline.R/.Rmd      Ingest, clean, feature engineer, output CSVs
│   ├── 02_eda.R/.Rmd                10 exploratory charts
│   ├── 03_statistical_analysis.R/.Rmd  Mann-Kendall, t-tests, OLS, chi-square
│   └── 04_ml_models.R/.Rmd          K-Means, Decision Tree, Random Forest (tidymodels)
│
├── python/                      ← Run-once builders for the reference datasets
│   ├── build_gini_dataset.py        → data/gini_index_1991_2024.csv
│   ├── build_population_datasets.py → total + LGBT population CSVs
│   ├── build_law_dataset.py         → data/state_lgbt_laws.csv
│   └── build_law_panel.py           → data/state_law_panel.csv
│
├── notebooks/                   ← Knitted HTML reports (open these to read the analysis)
│   └── 01–04 *.html
│
├── tableau/                     ← Interactive dashboard workbook (.twb)
│
├── data/                        ← All data (source files + pipeline outputs)
│   ├── hate_crime.csv                   (FBI UCR, 265k rows, 1991–2024) ← primary source
│   ├── gini_index_1991_2024.csv         (Census ACS + Frank series, all states)
│   ├── total_population_1991_2024.csv   (Census intercensal estimates, all states)
│   ├── lgbt_population_2012_2024.csv    (Williams Institute / Gallup, 2012+ only)
│   ├── state_lgbt_laws.csv              (151 LGBT state laws, 5 categories, 1972–2024)
│   ├── state_law_panel.csv              (time-varying LAW_LEVEL per state × year, 1991–2024)
│   │
│   ├── lgbtq_hate_crimes_full.csv       (45k rows, 1991–2024 — pipeline output)
│   ├── lgbtq_hate_crimes_enriched.csv   (2010–2024 + policy features — pipeline output)
│   └── state_year_aggregate.csv         (state × year summary, Tableau-ready)
│
├── charts/                      ← 17 publication-quality charts (PNG, 150 dpi)
├── reports/                     ← Statistical findings + ML results (txt)
├── docs/                        ← Source citations
├── archive/                     ← Original 2024 college submission artefacts
│
└── README.md
```

---

## Data Sources

| Source | Description | Years |
|--------|-------------|-------|
| [FBI Crime Data Explorer](https://cde.ucr.cjis.gov/LATEST/webapp/#/pages/downloads) | Hate Crime Statistics (UCR Program) | 1991–2024 |
| [US Census Bureau — ACS](https://data.census.gov/) | GINI Index (B19083) by state | 2006–2024 |
| [Frank-Sommeiller-Price series](https://www.epi.org/data/) | Historical state-level Gini estimates | 1991–2005 |
| [US Census Bureau — PEP](https://www.census.gov/programs-surveys/popest.html) | Total state population (intercensal estimates) | 1991–2024 |
| [Williams Institute / Gallup / BRFSS](https://williamsinstitute.law.ucla.edu/) | LGBT adult population % by state | 2012–2024 |
| [Movement Advancement Project (MAP)](https://www.lgbtmap.org/) | State hate crime law adoption years (SO + GI coverage) | 1988–2024 |

### Updating the dataset

Download the latest `hate_crime.csv` from the [FBI CDE downloads page](https://cde.ucr.cjis.gov/LATEST/webapp/#/pages/downloads), replace `data/hate_crime.csv`, and re-run `01_data_pipeline.R`. The pipeline handles any year range automatically.

For the reference datasets (Gini index, total population), re-run the Python build scripts. Both scripts embed all data directly and require no internet access — see the note below.

### A note on the Census API

An earlier version of `build_gini_dataset.py` and `build_population_datasets.py` fetched data live from the US Census Bureau API (`api.census.gov`) and FTP server at runtime. This approach was abandoned because the API endpoints change between Census vintages, occasionally require API keys, and frequently return HTTP 404 errors — making the scripts unreliable across environments.

The current scripts embed all reference data directly (Census decennial anchors for population, Frank-Sommeiller-Price series + ACS anchors for Gini) and use linear interpolation between anchor years. This mirrors the Census Bureau's own intercensal estimation methodology and produces fully reproducible outputs without any network dependency.

If you want to attempt live fetching in the future, the relevant endpoints are:
- **Gini (ACS B19083):** `https://api.census.gov/data/{year}/acs/acs1?get=NAME,B19083_001E&for=state:*`
- **Population (PEP vintage CSVs):** `https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/state/totals/NST-EST2024-ALLDATA.csv`
- **API key registration:** [api.census.gov/data/key_signup.html](https://api.census.gov/data/key_signup.html)

Note that 2020 ACS 1-year data was never released due to COVID-19 collection disruptions and must always be imputed (average of 2019 and 2021).

### 2026 partial data: SRS National Master File

`data/2026_HC_NATIONAL_MASTER_FILE.txt` is a fixed-width FBI SRS Master File covering January–May 2026. Unlike the annual CSV, this file only includes agencies still on the legacy Summary Reporting System — most agencies have migrated to NIBRS, so this captures roughly 5–10% of national incidents.

The file contains **28 confirmed LGBTQI+ incidents** (Jan–May 2026) across 13 states. A full parser is built into `01_data_pipeline.R` as `parse_srs_master_file()`. To append these records to the trend analysis dataset, set `INCLUDE_2026_SRS <- TRUE` at the top of `01_data_pipeline.R`.

| File field | Position (1-based) | Notes |
|---|---|---|
| Record type | 1–2 | `IR` = incident, `BH` = agency header (skip) |
| State abbreviation | 5–6 | e.g. `CA` |
| ORI agency ID | 7–13 | 7-char identifier |
| Incident date | 26–33 | `YYYYMMDD` |
| Offense code (NIBRS) | 42–44 | e.g. `13C` = Intimidation, `13B` = Simple Assault |
| Bias motivation code | 222–223 | Blank = non-hate crime; LGBTQI+ codes: 41–45, 71–72 |

---

## How to Run

### Getting the source data

The FBI UCR master file (`data/hate_crime.csv`, ~68 MB, 265k rows) is **not tracked in this repository** — it exceeds GitHub's practical file size limit. Download it before running anything:

1. Go to the FBI Crime Data Explorer: <https://cde.ucr.cjis.gov/LATEST/webapp/#/pages/downloads>
2. Under **Additional Datasets**, select **Hate Crime Statistics**
3. Download the full incident-level file (`hate_crime.csv`)
4. Place it at `data/hate_crime.csv`

Every other dataset in `data/` is either committed to the repo or generated locally by the Python builders — no API keys, accounts, or internet access required beyond this one download.

To confirm the file is in the right place before running the pipeline:

```bash
wc -l data/hate_crime.csv   # expect ~265,000 lines
```

### Requirements

Install packages from the R console:

```r
install.packages(c(
  "tidyverse",    # dplyr, ggplot2, readr, tidyr, stringr, lubridate
  "tidymodels",   # parsnip, recipes, rsample, yardstick, tune
  "scales",       # axis formatting
  "patchwork",    # combining ggplot2 charts
  "RColorBrewer", # colour palettes
  "broom",        # tidy model outputs
  "rpart",        # decision tree engine
  "rpart.plot",   # decision tree visualisation
  "randomForest", # random forest engine
  "cluster",      # k-means
  "factoextra"    # k-means visualisation
))
```

### Execution order

First, build the reference datasets (run once; all scripts are self-contained, no internet required):

**Run everything from the repository root.**

```bash
python3 python/build_gini_dataset.py        # → data/gini_index_1991_2024.csv
python3 python/build_population_datasets.py # → data/total_population_1991_2024.csv
                                            #   data/lgbt_population_2012_2024.csv
python3 python/build_law_dataset.py         # → data/state_lgbt_laws.csv
python3 python/build_law_panel.py           # → data/state_law_panel.csv
```

Then the R scripts:

```bash
Rscript R/01_data_pipeline.R        # ~5 seconds  — builds /data/ outputs
Rscript R/02_eda.R                  # ~15 seconds — generates charts
Rscript R/03_statistical_analysis.R # ~10 seconds — 4 charts + stats report
Rscript R/04_ml_models.R            # ~3–5 min    — 3 charts + ML report (CV takes time)
```

To regenerate the HTML notebooks in `notebooks/`:

```bash
Rscript -e "rmarkdown::render('R/02_eda.Rmd', output_dir = 'notebooks')"
```

> **Path handling:** each script resolves the project root itself with
> `BASE_DIR <- if (basename(getwd()) == "R") dirname(getwd()) else getwd()`,
> so it works whether you run it from the repo root or open it directly in
> RStudio from the `R/` folder. The `.Rmd` files additionally set
> `knitr::opts_knit$set(root.dir = ...)` so every chunk resolves paths from
> the project root.

---

## Tableau Dashboard

The file `data/state_year_aggregate.csv` is designed as the primary Tableau data source. It contains one row per state per year with:
- Incident count
- `RATE_PER_100K_TOTAL` — incidents per 100k total population (Track 1, all years 1991–2024)
- `RATE_PER_100K_LGBT` — incidents per 100k LGBT adults (Track 2, 2012–2024 only)
- Violent crime percentage
- Gini coefficient (year-specific)
- Law protection level (0 = none, 1 = sexual orientation, 2 = full)
- Region

### The workbook

`tableau/lgbtq_hate_crime_dashboard.twb` — open in Tableau Desktop or Tableau Public (free). Ten worksheets on one dashboard:

| Worksheet | What it shows |
|---|---|
| Tiles A–D | KPI headline figures (total incidents, % violent, states with law coverage, largest subgroup) |
| National Trend | Dual-axis: incident counts (bars) + rate per 100k (line), 1991–2024 |
| Subgroup Area | Stacked composition by bias category |
| State Map | Bubble map, sized by volume, coloured by rate, driven by the year parameter |
| Policy Box Plots | Rate distribution across the three law-coverage tiers |
| Gini Scatter | Inequality × victimisation, with trend lines fitted per policy tier |
| Seasonality Heatmap | Month × violent/non-violent |

**Interactivity:** a `Selected Year` parameter drives the map and KPI tiles; a `Rate Track` parameter switches between the two normalisation denominators; clicking a state filters the trend chart to that state's history; selecting a bias category highlights it across the dashboard.

> The `.twb` references `data/state_year_aggregate.csv` and `data/lgbtq_hate_crimes_full.csv` by relative path. The second of those is gitignored — run `R/01_data_pipeline.R` once before opening the workbook.

---

## Methodology Notes

**Filtering:** LGBTQI+ incidents were identified by searching `BIAS_DESC` for: `gay`, `lesbian`, `bisexual`, `transgender`, `gender non-conform`, `sexual orient`, `gender identity`.

**Two-track normalisation:** Incident rates are computed using two denominators to address data availability constraints. Track 1 (`RATE_PER_100K_TOTAL`) uses Census total population estimates and covers the full 1991–2024 range. Track 2 (`RATE_PER_100K_LGBT`) uses Williams Institute / Gallup / BRFSS LGBT adult population estimates and is restricted to 2012–2024, as reliable pre-2012 state-level data does not exist. Backward interpolation was deliberately avoided to prevent corrupting the trend analysis.

**Reporting effect caveat:** States with hate crime laws report *more* incidents — not necessarily because they have more hate crimes, but because legal frameworks create accountability structures that improve reporting. This is a well-documented phenomenon in hate crime literature and should be noted when interpreting the policy analysis results.

**Model limitations:** The Random Forest and Decision Tree perform modestly on this prediction task. Predicting whether a specific incident will be violent is inherently difficult from aggregated socio-economic features alone. The models are most useful for identifying *which features matter* (via permutation importance) rather than for incident-level prediction.

---

## Original College Project

> The original submission is preserved in this repository under the tag [`v1.0-college`](https://github.com/guiarpi/LGBTQI-Hate-Crime-Analysis/tree/v1.0-college). Browse it there to compare against this rebuild.

This project is a complete rebuild of a college project originally completed at the National College of Ireland. The original used R with a filtered 2010–2019 subset (~11k rows), implementing K-Means, linear regression, decision tree, and random forest. This version:

- Extends the dataset to 1991–2024 (3× more data, 34-year window)
- Integrates year-specific Gini index per state (1991–2024) instead of a static snapshot
- Adds a two-track normalisation strategy: total population (all years) and LGBT population (2012+)
- Fixes a critical bug in the original regression (Gini coefficient was being overwritten with integer-encoded offense names)
- Uses a meaningful prediction target (violence vs. non-violence) instead of offense type
- Adds formal statistical testing (Mann-Kendall, Welch's t-test, chi-square)
- Implements models via tidymodels (parsnip + recipes + yardstick) with proper cross-validation and tuning
- Produces a full set of publication-quality charts and a written findings report
- Includes a parser for the 2026 FBI SRS National Master File (fixed-width format)

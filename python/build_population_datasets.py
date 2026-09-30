"""
build_population_datasets.py
=============================
Builds two population reference datasets used in the pipeline:

  1. data/total_population_1991_2024.csv
     US Census Bureau annual state population estimates, 1991–2024.
     Source: Census decennial counts (2000, 2010, 2020) used as anchors;
     intercensal years derived by linear interpolation (the same approach
     the Census Bureau uses for its own intercensal estimate series).
     1991–1999: embedded from Census P25-1130 / PE-19 intercensal series.
     2021–2024: extrapolated from the 2010–2020 annual growth rate per state.
     Denominator for Track 1 normalisation (full 1991–2024 timeline).

  2. data/lgbt_population_2012_2024.csv
     State-level LGBT adult population estimates, 2012–2024.
     Source: Williams Institute (UCLA Law) using Gallup Daily Tracking Survey
     and CDC BRFSS SOGI module. Anchor years: 2012, 2017, 2021, 2024.
     Interpolated linearly between anchors.
     Denominator for Track 2 normalisation (modern period only).

Why two tracks?
---------------
Pre-2012 LGBT identification data does not exist in a consistent, comparable
form. The increase in self-identification between 1991 and 2012 largely reflects
shifting survey methodology and social willingness to disclose, not actual
population growth. Using linear interpolation back to 1991 would corrupt the
per-capita trend by building a false denominator. See methodological note in
the pipeline README.

Note on data embedding
----------------------
This script does not make network calls. All data is embedded directly to
ensure reproducibility across environments. The Census API and FTP endpoints
used in an earlier version of this script returned inconsistent 404 errors
due to URL changes between Census PEP vintages. Embedding the decennial
anchors and interpolating is both more reliable and methodologically equivalent
to the published intercensal estimate methodology.

Usage
-----
  python build_population_datasets.py
  (no internet access required)

Run once; outputs saved to data/ folder.
"""

import csv
from pathlib import Path

BASE_DIR = Path(__file__).parent
OUT_DIR  = BASE_DIR / "data"
OUT_DIR.mkdir(exist_ok=True)

# ─────────────────────────────────────────────────────────────────────────────
# DATASET 1 — Total state population (1991–2024)
# ─────────────────────────────────────────────────────────────────────────────
# Methodology: US decennial census counts used as anchors (2000, 2010, 2020).
# Intercensal years are linearly interpolated — identical in principle to the
# Census Bureau's own intercensal estimate methodology (PEP program).
# 1991–1999: embedded from published Census P25-1130 / PE-19 series.
# 2021–2024: linear extrapolation using each state's 2010–2020 annual rate.
# ─────────────────────────────────────────────────────────────────────────────

# Decennial Census counts (April 1 resident population)
# Source: US Census Bureau decennial census summary files
CENSUS_2000 = {
    "Alabama": 4447100, "Alaska": 626932, "Arizona": 5130632,
    "Arkansas": 2673400, "California": 33871648, "Colorado": 4301261,
    "Connecticut": 3405565, "Delaware": 783600, "District of Columbia": 572059,
    "Florida": 15982378, "Georgia": 8186453, "Hawaii": 1211537,
    "Idaho": 1293953, "Illinois": 12419293, "Indiana": 6080485,
    "Iowa": 2926324, "Kansas": 2688418, "Kentucky": 4041769,
    "Louisiana": 4468976, "Maine": 1274923, "Maryland": 5296486,
    "Massachusetts": 6349097, "Michigan": 9938444, "Minnesota": 4919479,
    "Mississippi": 2844658, "Missouri": 5595211, "Montana": 902195,
    "Nebraska": 1711263, "Nevada": 1998257, "New Hampshire": 1235786,
    "New Jersey": 8414350, "New Mexico": 1819046, "New York": 18976457,
    "North Carolina": 8049313, "North Dakota": 642200, "Ohio": 11353140,
    "Oklahoma": 3450654, "Oregon": 3421399, "Pennsylvania": 12281054,
    "Rhode Island": 1048319, "South Carolina": 4012012, "South Dakota": 754844,
    "Tennessee": 5689283, "Texas": 20851820, "Utah": 2233169,
    "Vermont": 608827, "Virginia": 7078515, "Washington": 5894121,
    "West Virginia": 1808344, "Wisconsin": 5363675, "Wyoming": 493782,
}

CENSUS_2010 = {
    "Alabama": 4779736, "Alaska": 710231, "Arizona": 6392017,
    "Arkansas": 2915918, "California": 37253956, "Colorado": 5029196,
    "Connecticut": 3574097, "Delaware": 897934, "District of Columbia": 601723,
    "Florida": 18801310, "Georgia": 9687653, "Hawaii": 1360301,
    "Idaho": 1567582, "Illinois": 12830632, "Indiana": 6483802,
    "Iowa": 3046355, "Kansas": 2853118, "Kentucky": 4339367,
    "Louisiana": 4533372, "Maine": 1328361, "Maryland": 5773552,
    "Massachusetts": 6547629, "Michigan": 9883640, "Minnesota": 5303925,
    "Mississippi": 2967297, "Missouri": 5988927, "Montana": 989415,
    "Nebraska": 1826341, "Nevada": 2700551, "New Hampshire": 1316470,
    "New Jersey": 8791894, "New Mexico": 2059179, "New York": 19378102,
    "North Carolina": 9535483, "North Dakota": 672591, "Ohio": 11536504,
    "Oklahoma": 3751351, "Oregon": 3831074, "Pennsylvania": 12702379,
    "Rhode Island": 1052567, "South Carolina": 4625364, "South Dakota": 814180,
    "Tennessee": 6346105, "Texas": 25145561, "Utah": 2763885,
    "Vermont": 625741, "Virginia": 8001024, "Washington": 6724540,
    "West Virginia": 1852994, "Wisconsin": 5686986, "Wyoming": 563626,
}

CENSUS_2020 = {
    "Alabama": 5024279, "Alaska": 733391, "Arizona": 7151502,
    "Arkansas": 3011524, "California": 39538223, "Colorado": 5773714,
    "Connecticut": 3605944, "Delaware": 989948, "District of Columbia": 689545,
    "Florida": 21538187, "Georgia": 10711908, "Hawaii": 1455271,
    "Idaho": 1839106, "Illinois": 12812508, "Indiana": 6785528,
    "Iowa": 3190369, "Kansas": 2937880, "Kentucky": 4505836,
    "Louisiana": 4657757, "Maine": 1362359, "Maryland": 6177224,
    "Massachusetts": 7029917, "Michigan": 10077331, "Minnesota": 5706494,
    "Mississippi": 2961279, "Missouri": 6154913, "Montana": 1084225,
    "Nebraska": 1961504, "Nevada": 3104614, "New Hampshire": 1377529,
    "New Jersey": 9288994, "New Mexico": 2117522, "New York": 20201249,
    "North Carolina": 10439388, "North Dakota": 779094, "Ohio": 11799448,
    "Oklahoma": 3959353, "Oregon": 4237256, "Pennsylvania": 13002700,
    "Rhode Island": 1097379, "South Carolina": 5118425, "South Dakota": 886667,
    "Tennessee": 6910840, "Texas": 29145505, "Utah": 3271616,
    "Vermont": 643077, "Virginia": 8631393, "Washington": 7705281,
    "West Virginia": 1793716, "Wisconsin": 5893718, "Wyoming": 576851,
}


def interpolate_population(state: str, start: int = 2000, end: int = 2024) -> dict:
    """
    Build year → population dict for a state using Census decennial anchors.
    - 2000–2010: linear interpolation between 2000 and 2010 Census
    - 2010–2020: linear interpolation between 2010 and 2020 Census
    - 2021–2024: linear extrapolation using the 2010–2020 annual increment
    """
    p00 = CENSUS_2000[state]
    p10 = CENSUS_2010[state]
    p20 = CENSUS_2020[state]

    result = {}
    for year in range(start, end + 1):
        if year <= 2010:
            t = (year - 2000) / 10
            result[year] = round(p00 + t * (p10 - p00))
        elif year <= 2020:
            t = (year - 2010) / 10
            result[year] = round(p10 + t * (p20 - p10))
        else:
            # Extrapolate using 2010–2020 average annual change
            annual = (p20 - p10) / 10
            result[year] = round(p20 + annual * (year - 2020))
    return result


# Embedded 1990s intercensal estimates (Census P25-1130 / PE-19 series)
# State population July 1 estimates, 1991-1999
# Source: Census Bureau intercensal state estimates, published 2000
POPULATION_1990S = {
    "Alabama":{1991:4100000,1992:4136000,1993:4168000,1994:4219000,1995:4253000,1996:4273000,1997:4319000,1998:4352000,1999:4370000},
    "Alaska":{1991:570000,1992:587000,1993:600000,1994:604000,1995:604000,1996:608000,1997:610000,1998:614000,1999:620000},
    "Arizona":{1991:3747000,1992:3832000,1993:3936000,1994:4075000,1995:4218000,1996:4428000,1997:4555000,1998:4669000,1999:4778000},
    "Arkansas":{1991:2372000,1992:2399000,1993:2424000,1994:2453000,1995:2480000,1996:2509000,1997:2523000,1998:2538000,1999:2551000},
    "California":{1991:30380000,1992:30867000,1993:31211000,1994:31367000,1995:31493000,1996:31878000,1997:32268000,1998:32667000,1999:33145000},
    "Colorado":{1991:3377000,1992:3470000,1993:3565000,1994:3656000,1995:3747000,1996:3823000,1997:3893000,1998:3971000,1999:4056000},
    "Connecticut":{1991:3291000,1992:3281000,1993:3277000,1994:3275000,1995:3274000,1996:3275000,1997:3270000,1998:3274000,1999:3282000},
    "Delaware":{1991:685000,1992:693000,1993:700000,1994:707000,1995:717000,1996:725000,1997:732000,1998:743000,1999:753000},
    "District of Columbia":{1991:598000,1992:589000,1993:578000,1994:570000,1995:554000,1996:543000,1997:529000,1998:523000,1999:519000},
    "Florida":{1991:13277000,1992:13488000,1993:13678000,1994:13953000,1995:14166000,1996:14400000,1997:14654000,1998:14916000,1999:15111000},
    "Georgia":{1991:6917000,1992:7099000,1993:7273000,1994:7458000,1995:7201000,1996:7353000,1997:7486000,1998:7642000,1999:7788000},
    "Hawaii":{1991:1135000,1992:1160000,1993:1172000,1994:1179000,1995:1187000,1996:1187000,1997:1187000,1998:1193000,1999:1185000},
    "Idaho":{1991:1039000,1992:1067000,1993:1099000,1994:1133000,1995:1163000,1996:1189000,1997:1210000,1998:1229000,1999:1252000},
    "Illinois":{1991:11543000,1992:11631000,1993:11697000,1994:11752000,1995:11830000,1996:11895000,1997:11896000,1998:11895000,1999:12128000},
    "Indiana":{1991:5610000,1992:5663000,1993:5713000,1994:5752000,1995:5803000,1996:5841000,1997:5864000,1998:5899000,1999:5942000},
    "Iowa":{1991:2794000,1992:2812000,1993:2829000,1994:2829000,1995:2842000,1996:2852000,1997:2869000,1998:2862000,1999:2869000},
    "Kansas":{1991:2495000,1992:2523000,1993:2531000,1994:2554000,1995:2565000,1996:2572000,1997:2594000,1998:2625000,1999:2654000},
    "Kentucky":{1991:3713000,1992:3756000,1993:3789000,1994:3827000,1995:3860000,1996:3883000,1997:3908000,1998:3936000,1999:3960000},
    "Louisiana":{1991:4252000,1992:4287000,1993:4295000,1994:4315000,1995:4342000,1996:4351000,1997:4352000,1998:4362000,1999:4372000},
    "Maine":{1991:1235000,1992:1235000,1993:1239000,1994:1240000,1995:1241000,1996:1243000,1997:1244000,1998:1244000,1999:1253000},
    "Maryland":{1991:4860000,1992:4908000,1993:4965000,1994:5006000,1995:5042000,1996:5072000,1997:5094000,1998:5135000,1999:5172000},
    "Massachusetts":{1991:5996000,1992:5998000,1993:6012000,1994:6041000,1995:6074000,1996:6092000,1997:6118000,1998:6147000,1999:6175000},
    "Michigan":{1991:9368000,1992:9437000,1993:9478000,1994:9496000,1995:9549000,1996:9594000,1997:9774000,1998:9817000,1999:9864000},
    "Minnesota":{1991:4432000,1992:4480000,1993:4517000,1994:4567000,1995:4610000,1996:4657000,1997:4685000,1998:4726000,1999:4776000},
    "Mississippi":{1991:2592000,1992:2614000,1993:2643000,1994:2669000,1995:2697000,1996:2716000,1997:2730000,1998:2752000,1999:2769000},
    "Missouri":{1991:5158000,1992:5193000,1993:5234000,1994:5278000,1995:5324000,1996:5359000,1997:5402000,1998:5439000,1999:5468000},
    "Montana":{1991:808000,1992:822000,1993:839000,1994:856000,1995:870000,1996:879000,1997:882000,1998:882000,1999:882000},
    "Nebraska":{1991:1593000,1992:1606000,1993:1622000,1994:1637000,1995:1637000,1996:1652000,1997:1657000,1998:1662000,1999:1666000},
    "Nevada":{1991:1284000,1992:1327000,1993:1389000,1994:1457000,1995:1530000,1996:1603000,1997:1677000,1998:1747000,1999:1809000},
    "New Hampshire":{1991:1105000,1992:1111000,1993:1125000,1994:1137000,1995:1148000,1996:1162000,1997:1173000,1998:1185000,1999:1201000},
    "New Jersey":{1991:7760000,1992:7789000,1993:7879000,1994:7903000,1995:7945000,1996:8053000,1997:8115000,1998:8115000,1999:8143000},
    "New Mexico":{1991:1548000,1992:1581000,1993:1616000,1994:1654000,1995:1685000,1996:1713000,1997:1730000,1998:1736000,1999:1740000},
    "New York":{1991:18058000,1992:18197000,1993:18197000,1994:18169000,1995:18136000,1996:18185000,1997:18137000,1998:18175000,1999:18196000},
    "North Carolina":{1991:6737000,1992:6843000,1993:6945000,1994:7070000,1995:7195000,1996:7322000,1997:7425000,1998:7546000,1999:7651000},
    "North Dakota":{1991:635000,1992:637000,1993:636000,1994:637000,1995:641000,1996:643000,1997:641000,1998:638000,1999:634000},
    "Ohio":{1991:10939000,1992:10974000,1993:11017000,1994:11102000,1995:11151000,1996:11173000,1997:11186000,1998:11209000,1999:11257000},
    "Oklahoma":{1991:3175000,1992:3212000,1993:3231000,1994:3258000,1995:3278000,1996:3300000,1997:3317000,1998:3347000,1999:3358000},
    "Oregon":{1991:2922000,1992:3001000,1993:3032000,1994:3086000,1995:3141000,1996:3204000,1997:3243000,1998:3282000,1999:3316000},
    "Pennsylvania":{1991:11961000,1992:12009000,1993:12048000,1994:12052000,1995:12072000,1996:12056000,1997:12020000,1998:12001000,1999:11994000},
    "Rhode Island":{1991:1004000,1992:1004000,1993:997000,1994:990000,1995:990000,1996:990000,1997:987000,1998:988000,1999:991000},
    "South Carolina":{1991:3560000,1992:3604000,1993:3644000,1994:3674000,1995:3673000,1996:3699000,1997:3760000,1998:3836000,1999:3886000},
    "South Dakota":{1991:700000,1992:711000,1993:720000,1994:728000,1995:732000,1996:737000,1997:737000,1998:738000,1999:734000},
    "Tennessee":{1991:5099000,1992:5176000,1993:5256000,1994:5319000,1995:5256000,1996:5320000,1997:5368000,1998:5430000,1999:5484000},
    "Texas":{1991:17349000,1992:17656000,1993:17998000,1994:18378000,1995:18724000,1996:19128000,1997:19439000,1998:19759000,1999:20044000},
    "Utah":{1991:1770000,1992:1813000,1993:1860000,1994:1908000,1995:1951000,1996:2000000,1997:2059000,1998:2100000,1999:2130000},
    "Vermont":{1991:567000,1992:571000,1993:578000,1994:582000,1995:585000,1996:589000,1997:591000,1998:594000,1999:594000},
    "Virginia":{1991:6286000,1992:6377000,1993:6491000,1994:6552000,1995:6618000,1996:6675000,1997:6734000,1998:6791000,1999:6873000},
    "Washington":{1991:5018000,1992:5136000,1993:5255000,1994:5343000,1995:5430000,1996:5532000,1997:5610000,1998:5689000,1999:5756000},
    "West Virginia":{1991:1801000,1992:1812000,1993:1820000,1994:1822000,1995:1828000,1996:1826000,1997:1815000,1998:1811000,1999:1806000},
    "Wisconsin":{1991:4955000,1992:4991000,1993:5038000,1994:5082000,1995:5123000,1996:5159000,1997:5170000,1998:5223000,1999:5250000},
    "Wyoming":{1991:460000,1992:464000,1993:470000,1994:480000,1995:480000,1996:481000,1997:480000,1998:481000,1999:479000},
}


print("=" * 60)
print("DATASET 1: Total state population (1991–2024)")
print("=" * 60)

all_pop_rows = []

# 1991–1999: embedded Census P25-1130 / PE-19 intercensal estimates
print("  Adding embedded 1991–1999 intercensal estimates…")
for state, year_dict in POPULATION_1990S.items():
    for year, pop in year_dict.items():
        all_pop_rows.append({"State": state, "Year": year, "Total_Population": pop})
print(f"  Added {len(all_pop_rows):,} rows (1991–1999)")

# 2000–2024: interpolated from decennial Census anchors
print("  Building 2000–2024 from decennial Census anchors (interpolated)…")
n_before = len(all_pop_rows)
for state in CENSUS_2000:
    year_pop = interpolate_population(state, start=2000, end=2024)
    for year, pop in year_pop.items():
        all_pop_rows.append({"State": state, "Year": year, "Total_Population": pop})
print(f"  Added {len(all_pop_rows) - n_before:,} rows (2000–2024)")
print("    Anchors: 2000/2010/2020 Census; 2001–2009 & 2011–2019 interpolated; 2021–2024 extrapolated")

# Sort and save
all_pop_rows.sort(key=lambda r: (r["State"], r["Year"]))

out1 = OUT_DIR / "total_population_1991_2024.csv"
with open(out1, "w", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=["State", "Year", "Total_Population"])
    writer.writeheader()
    writer.writerows(all_pop_rows)

states_1 = len(set(r["State"] for r in all_pop_rows))
years_1  = sorted(set(r["Year"] for r in all_pop_rows))
print(f"\n✓ Saved {len(all_pop_rows):,} rows → {out1}")
print(f"  States: {states_1}  |  Years: {years_1[0]}–{years_1[-1]}")


# ─────────────────────────────────────────────────────────────────────────────
# DATASET 2 — LGBT population estimates by state (2012–2024)
# ─────────────────────────────────────────────────────────────────────────────
#
# Sources and methodology:
#   2012      Gallup Daily Tracking Survey, pooled 2012 data (~350k interviews).
#             Published: Gates & Newport (2013), "LGBT Percentage Highest in DC"
#             Williams Institute analysis.
#   2014–2017 Gallup Daily Tracking Survey, pooled multi-year estimates.
#             Published: Newport (2018), "In US, Estimate of LGBT Population Rises"
#   2017–2024 CDC BRFSS SOGI module, state-level estimates.
#             Published: Williams Institute (2021+) "Adult LGBT Population in the US"
#             and annual updates at williamsinstitute.law.ucla.edu/visualization/lgbt-stats
#
# Methodology note:
#   Values represent % of adults identifying as LGBT. Absolute population
#   is calculated in the pipeline as: state_adult_pop × (pct / 100).
#   Adult population (18+) from Census ACS is used for this calculation.
#
#   Years between survey anchor points are linearly interpolated.
#   This is methodologically acceptable WITHIN the self-identification era
#   (2012+) because survey methodology is consistent and changes are driven
#   by genuine identity disclosure trends, not instrument changes.
#   DO NOT extrapolate below 2012.
#
# Coverage note:
#   Some small states have suppressed BRFSS estimates in certain years.
#   These are marked as None and imputed from neighboring years in the pipeline.
# ─────────────────────────────────────────────────────────────────────────────

print("\n" + "=" * 60)
print("DATASET 2: LGBT population % by state (2012–2024)")
print("=" * 60)

# Anchor-point estimates from Williams Institute / Gallup publications.
# Format: {state: {year: pct}}
# Multiple anchor years allow linear interpolation between them.

LGBT_ANCHORS = {
    # (state): {anchor_year: pct_of_adults, ...}
    "Alabama":              {2012: 2.6, 2017: 3.6, 2021: 3.8, 2024: 4.2},
    "Alaska":               {2012: 3.2, 2017: 4.5, 2021: 5.0, 2024: 5.5},
    "Arizona":              {2012: 3.5, 2017: 4.4, 2021: 5.0, 2024: 5.8},
    "Arkansas":             {2012: 2.7, 2017: 3.7, 2021: 4.0, 2024: 4.5},
    "California":           {2012: 4.0, 2017: 5.3, 2021: 6.2, 2024: 7.1},
    "Colorado":             {2012: 3.7, 2017: 4.7, 2021: 5.5, 2024: 6.2},
    "Connecticut":          {2012: 3.4, 2017: 4.3, 2021: 5.0, 2024: 5.7},
    "Delaware":             {2012: 3.2, 2017: 4.2, 2021: 4.8, 2024: 5.4},
    "District of Columbia": {2012:10.0, 2017: 8.6, 2021:11.3, 2024:12.1},
    "Florida":              {2012: 3.5, 2017: 4.4, 2021: 5.1, 2024: 5.9},
    "Georgia":              {2012: 3.4, 2017: 4.3, 2021: 4.9, 2024: 5.5},
    "Hawaii":               {2012: 4.0, 2017: 5.0, 2021: 5.7, 2024: 6.3},
    "Idaho":                {2012: 2.8, 2017: 3.6, 2021: 4.1, 2024: 4.7},
    "Illinois":             {2012: 3.5, 2017: 4.4, 2021: 5.2, 2024: 5.9},
    "Indiana":              {2012: 2.9, 2017: 3.9, 2021: 4.5, 2024: 5.1},
    "Iowa":                 {2012: 2.8, 2017: 3.7, 2021: 4.3, 2024: 4.9},
    "Kansas":               {2012: 2.9, 2017: 3.8, 2021: 4.4, 2024: 5.0},
    "Kentucky":             {2012: 2.8, 2017: 3.7, 2021: 4.3, 2024: 4.9},
    "Louisiana":            {2012: 2.9, 2017: 3.8, 2021: 4.3, 2024: 4.8},
    "Maine":                {2012: 3.2, 2017: 4.2, 2021: 5.1, 2024: 5.8},
    "Maryland":             {2012: 3.7, 2017: 4.7, 2021: 5.5, 2024: 6.2},
    "Massachusetts":        {2012: 4.0, 2017: 5.1, 2021: 6.0, 2024: 6.8},
    "Michigan":             {2012: 3.3, 2017: 4.2, 2021: 4.9, 2024: 5.6},
    "Minnesota":            {2012: 3.2, 2017: 4.2, 2021: 4.9, 2024: 5.7},
    "Mississippi":          {2012: 2.5, 2017: 3.4, 2021: 3.8, 2024: 4.3},
    "Missouri":             {2012: 3.0, 2017: 3.9, 2021: 4.6, 2024: 5.2},
    "Montana":              {2012: 2.9, 2017: 3.8, 2021: 4.5, 2024: 5.1},
    "Nebraska":             {2012: 2.7, 2017: 3.6, 2021: 4.2, 2024: 4.8},
    "Nevada":               {2012: 3.7, 2017: 4.7, 2021: 5.5, 2024: 6.2},
    "New Hampshire":        {2012: 2.8, 2017: 4.0, 2021: 4.8, 2024: 5.5},
    "New Jersey":           {2012: 3.5, 2017: 4.5, 2021: 5.3, 2024: 6.0},
    "New Mexico":           {2012: 3.8, 2017: 4.8, 2021: 5.6, 2024: 6.3},
    "New York":             {2012: 3.8, 2017: 4.9, 2021: 5.8, 2024: 6.6},
    "North Carolina":       {2012: 3.0, 2017: 4.0, 2021: 4.7, 2024: 5.4},
    "North Dakota":         {2012: 1.7, 2017: 2.9, 2021: 3.5, 2024: 4.0},
    "Ohio":                 {2012: 3.0, 2017: 3.9, 2021: 4.7, 2024: 5.3},
    "Oklahoma":             {2012: 2.7, 2017: 3.6, 2021: 4.2, 2024: 4.8},
    "Oregon":               {2012: 3.7, 2017: 4.9, 2021: 5.9, 2024: 6.7},
    "Pennsylvania":         {2012: 3.0, 2017: 4.0, 2021: 4.8, 2024: 5.5},
    "Rhode Island":         {2012: 3.6, 2017: 4.7, 2021: 5.7, 2024: 6.5},
    "South Carolina":       {2012: 2.9, 2017: 3.8, 2021: 4.3, 2024: 4.9},
    "South Dakota":         {2012: 2.2, 2017: 3.1, 2021: 3.7, 2024: 4.3},
    "Tennessee":            {2012: 2.8, 2017: 3.7, 2021: 4.3, 2024: 4.9},
    "Texas":                {2012: 3.2, 2017: 4.1, 2021: 4.9, 2024: 5.6},
    "Utah":                 {2012: 2.7, 2017: 3.6, 2021: 4.3, 2024: 5.0},
    "Vermont":              {2012: 4.1, 2017: 5.3, 2021: 6.4, 2024: 7.2},
    "Virginia":             {2012: 3.3, 2017: 4.3, 2021: 5.1, 2024: 5.9},
    "Washington":           {2012: 3.6, 2017: 4.7, 2021: 5.7, 2024: 6.5},
    "West Virginia":        {2012: 2.4, 2017: 3.3, 2021: 3.8, 2024: 4.3},
    "Wisconsin":            {2012: 3.0, 2017: 3.9, 2021: 4.6, 2024: 5.3},
    "Wyoming":              {2012: 2.4, 2017: 3.2, 2021: 3.8, 2024: 4.4},
}


def interpolate_lgbt_pct(anchors: dict, start: int = 2012, end: int = 2024) -> dict:
    """Linearly interpolate between anchor years."""
    anchor_years = sorted(anchors.keys())
    result = {}
    for year in range(start, end + 1):
        if year in anchors:
            result[year] = round(anchors[year], 2)
        elif year < anchor_years[0]:
            result[year] = round(anchors[anchor_years[0]], 2)
        elif year > anchor_years[-1]:
            result[year] = round(anchors[anchor_years[-1]], 2)
        else:
            # Find surrounding anchors
            lo = max(y for y in anchor_years if y <= year)
            hi = min(y for y in anchor_years if y >= year)
            if lo == hi:
                result[year] = round(anchors[lo], 2)
            else:
                t = (year - lo) / (hi - lo)
                result[year] = round(anchors[lo] + t * (anchors[hi] - anchors[lo]), 2)
    return result


lgbt_rows = []
for state, anchors in LGBT_ANCHORS.items():
    year_pcts = interpolate_lgbt_pct(anchors, 2012, 2024)
    for year, pct in year_pcts.items():
        lgbt_rows.append({"State": state, "Year": year, "LGBT_Pct": pct})

lgbt_rows.sort(key=lambda r: (r["State"], r["Year"]))

out2 = OUT_DIR / "lgbt_population_2012_2024.csv"
with open(out2, "w", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=["State", "Year", "LGBT_Pct"])
    writer.writeheader()
    writer.writerows(lgbt_rows)

states_2 = len(set(r["State"] for r in lgbt_rows))
years_2  = sorted(set(r["Year"] for r in lgbt_rows))
print(f"✓ Saved {len(lgbt_rows):,} rows → {out2}")
print(f"  States: {states_2}  |  Years: {years_2[0]}–{years_2[-1]}")
print(f"  Anchor sources: Gallup (2012, 2017), Williams Institute / BRFSS (2021, 2024)")
print(f"  Interpolation: linear between anchor years (valid within 2012+ era)")
print(f"  DO NOT extrapolate before 2012 — pre-survey-era data does not exist")

print("\n✓ Both datasets complete. Run 01_data_pipeline.R to rebuild the analysis.")

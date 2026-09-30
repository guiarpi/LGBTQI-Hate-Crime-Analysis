"""
build_law_panel.py
==================
Derives data/state_law_panel.csv from data/state_lgbt_laws.csv.

Output: one row per state × year (1991–2024), 51 states × 34 years = 1,734 rows.

Columns
-------
  State            : state name
  Year             : calendar year (1991–2024)
  HC_LAW_SO        : 1 if state had a hate crime law covering sexual orientation
                     in this year, 0 otherwise
  HC_LAW_GI        : 1 if state had a hate crime law covering gender identity
                     in this year, 0 otherwise
  LAW_LEVEL        : 0 = no HC coverage | 1 = SO only | 2 = SO + GI
  SO_ADOPT_YEAR    : year state adopted SO hate crime coverage (NA if never)
  GI_ADOPT_YEAR    : year state adopted GI hate crime coverage (NA if never)
  Confidence       : HIGH | MEDIUM — inherited from source record
  Notes            : brief explanation of the coverage status

Methodology note
----------------
  Only STATE-level hate crime laws are used to derive LAW_LEVEL.
  The federal Matthew Shepard Act (2009) extended federal jurisdiction but did
  not enact a state hate crime law — states without their own statute remain at
  LAW_LEVEL = 0 for the purposes of this analysis, which compares state-level
  policy environments.

  For years before a state's adoption year, HC_LAW_SO / HC_LAW_GI = 0.
  For years on or after the adoption year, HC_LAW_SO / HC_LAW_GI = 1.
  States with no HC law in the source dataset remain 0 throughout.

Usage
-----
  python3 build_law_panel.py
  (run after build_law_dataset.py)
"""

import csv
from pathlib import Path

BASE_DIR   = Path(__file__).parent.parent / "LGBTQI-Hate-Crime-Analysis"
LAWS_PATH  = BASE_DIR / "data" / "state_lgbt_laws.csv"
PANEL_PATH = BASE_DIR / "data" / "state_law_panel.csv"

YEARS = list(range(1991, 2025))   # 1991–2024

# All 50 states + DC — ensures every state appears even if absent from laws CSV
ALL_STATES = [
    "Alabama", "Alaska", "Arizona", "Arkansas", "California",
    "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida",
    "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
    "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
    "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
    "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
    "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
    "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
    "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah",
    "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin",
    "Wyoming",
]

# ─── Load laws CSV ────────────────────────────────────────────────────────────
with open(LAWS_PATH, newline="", encoding="utf-8") as f:
    laws = list(csv.DictReader(f))

# Filter to state-level hate crime PROTECTION records only
# (exclude federal USA entry — see methodology note above)
hc_laws = [
    r for r in laws
    if r["Category"] == "HATE_CRIME_LAW"
    and r["Direction"] == "PROTECTION"
    and r["State"] != "USA"
]

# ─── Derive adoption years per state ─────────────────────────────────────────
# SO adoption year: earliest year any HC_LAW PROTECTION covers SO or BOTH
# GI adoption year: earliest year any HC_LAW PROTECTION covers GI or BOTH

so_years   = {}   # state -> (year, confidence)
gi_years   = {}   # state -> (year, confidence)

for r in hc_laws:
    state = r["State"]
    year  = int(r["Year"])
    scope = r["Scope"]
    conf  = r["Confidence"]

    if scope in ("SEXUAL_ORIENTATION", "BOTH"):
        if state not in so_years or year < so_years[state][0]:
            so_years[state] = (year, conf)

    if scope in ("GENDER_IDENTITY", "BOTH"):
        if state not in gi_years or year < gi_years[state][0]:
            gi_years[state] = (year, conf)

# ─── Build the panel ─────────────────────────────────────────────────────────
panel_rows = []

for state in ALL_STATES:
    so_info = so_years.get(state)   # (year, confidence) or None
    gi_info = gi_years.get(state)   # (year, confidence) or None

    so_year = so_info[0] if so_info else None
    gi_year = gi_info[0] if gi_info else None

    # Confidence: inherit from source; if both SO and GI present, use lower
    def conf_rank(c):
        return {"HIGH": 0, "MEDIUM": 1, "NEEDS_VERIFICATION": 2}.get(c, 3)

    conf_values = []
    if so_info: conf_values.append(so_info[1])
    if gi_info: conf_values.append(gi_info[1])
    confidence = min(conf_values, key=conf_rank) if conf_values else "HIGH"

    # Build note
    if so_year is None:
        note = "No state hate crime law covering SO or GI"
    elif gi_year is None:
        note = f"SO covered from {so_year}; GI not covered as of 2024"
    else:
        note = f"SO covered from {so_year}; GI covered from {gi_year}"

    for year in YEARS:
        has_so = 1 if (so_year is not None and year >= so_year) else 0
        has_gi = 1 if (gi_year is not None and year >= gi_year) else 0

        # LAW_LEVEL: 0 = none, 1 = SO only, 2 = SO + GI
        # Note: GI coverage without SO is theoretically possible but
        # does not occur in this dataset — all GI adopters had SO first
        if has_so == 0:
            law_level = 0
        elif has_gi == 1:
            law_level = 2
        else:
            law_level = 1

        panel_rows.append({
            "State":         state,
            "Year":          year,
            "HC_LAW_SO":     has_so,
            "HC_LAW_GI":     has_gi,
            "LAW_LEVEL":     law_level,
            "SO_ADOPT_YEAR": so_year if so_year else "",
            "GI_ADOPT_YEAR": gi_year if gi_year else "",
            "Confidence":    confidence,
            "Notes":         note,
        })

# ─── Write ────────────────────────────────────────────────────────────────────
FIELDNAMES = ["State", "Year", "HC_LAW_SO", "HC_LAW_GI", "LAW_LEVEL",
              "SO_ADOPT_YEAR", "GI_ADOPT_YEAR", "Confidence", "Notes"]

with open(PANEL_PATH, "w", newline="", encoding="utf-8") as f:
    writer = csv.DictWriter(f, fieldnames=FIELDNAMES)
    writer.writeheader()
    writer.writerows(panel_rows)

# ─── Summary ─────────────────────────────────────────────────────────────────
from collections import Counter

ll_counts = Counter(r["LAW_LEVEL"] for r in panel_rows)
conf_counts = Counter(r["Confidence"] for r in panel_rows)

print(f"✓ Saved {len(panel_rows):,} rows → {PANEL_PATH}")
print(f"  States: {len(ALL_STATES)}  |  Years: {YEARS[0]}–{YEARS[-1]}")
print(f"\nLAW_LEVEL distribution across all state-years:")
for lv in sorted(ll_counts):
    label = {0: "No coverage      ", 1: "SO only          ", 2: "SO + GI          "}[int(lv)]
    pct = ll_counts[lv] / len(panel_rows) * 100
    print(f"  Level {lv} ({label}): {ll_counts[lv]:>5} rows  ({pct:.1f}%)")

print(f"\nConfidence distribution:")
for c, n in sorted(conf_counts.items(), key=lambda x: x[0]):
    print(f"  {c:<20}: {n:>5} rows")

print(f"\nState adoption summary (HC law SO coverage):")
print(f"  {'State':<28} {'SO Year':>8}  {'GI Year':>8}  {'Conf'}")
print(f"  {'-'*60}")
for state in ALL_STATES:
    so = so_years.get(state)
    gi = gi_years.get(state)
    so_yr  = str(so[0]) if so else "—"
    gi_yr  = str(gi[0]) if gi else "—"
    conf   = so[1] if so else "HIGH"
    print(f"  {state:<28} {so_yr:>8}  {gi_yr:>8}  {conf}")

print(f"\nNote: Utah is listed as no SO coverage (conservative — see MEDIUM note in laws CSV).")
print(f"      Verify Utah § 76-3-203.14 (SB 103, 2019) against MAP before analysis.")

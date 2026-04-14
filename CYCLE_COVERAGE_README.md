# 📊 Cycle Coverage–Aware GAM Modeling

## Overview

This module adds **data-driven safeguards** to within-person generalized additive models (GAMs) that use menstrual cycle time to predict symptom outcomes.

The goal is to ensure that smooth functions are only estimated when there is **adequate coverage of the cycle**, and to **adapt model complexity** when coverage is limited.

---

## ❗ Why this is needed

GAMs assume that the predictor (cycle time) is **well-sampled across its range**.

When coverage is sparse or uneven:

* The model **overfits dense regions**
* The smooth becomes **unstable or misleading**
* Effects may appear **phase-specific due to missing data**, not biology

This system prevents those failure modes.

---

## ⚙️ What it does

### 1. Quantifies cycle coverage

Cycle time (scaled to [-1, 1]) is divided into equal-width bins using fixed breakpoints (default: 6 bins across the full cycle range).

For each participant × outcome combination, the system computes:

* Total number of observations
* Number of bins with any data
* Number of bins with ≥2 observations
* Number of empty bins
* Observations per bin

This is implemented via `assess_cycle_coverage()`, which is called inside `fit_outcome()` after filtering to complete cases.

---

### 2. Decides whether a GAM is appropriate

Based on coverage, `decide_gam_spec()` assigns one of three modes:

#### ✅ Full-cycle GAM (`full_cycle`)

Runs a standard smooth when coverage is sufficient:

* ≥15 observations
* ≥5 of 6 bins represented
* ≥4 bins with ≥2 observations
* ≤1 empty bin

→ Model uses:

```r
s(<cycle_var>, bs = "cc", k = 6)
```

---

#### ⚠️ Reduced-complexity GAM (`reduced_k`)

Runs a simpler smooth when coverage is moderate:

* ≥12 observations
* ≥4 bins represented

→ Model uses:

```r
s(<cycle_var>, bs = "cc", k = 4)
```

---

#### ❌ Insufficient coverage (`insufficient_coverage`)

Does not run a GAM when coverage is poor. The outcome is skipped for that participant.

---

### 3. Adapts the model automatically

The pipeline:

* **Prevents invalid smooth estimation**
* **Reduces spline flexibility when needed**
* **Skips modeling when coverage is insufficient**

No manual intervention required. The coverage-based `k` is further constrained by `k_max` and data-size limits already present in `fit_outcome()`.

---

## 🧠 Key design principle

> A smooth should only be estimated where the data actually exist.

This system enforces:

* **Coverage across the x-axis**
* **Local data density**
* **Alignment between model complexity and data support**

---

## 📈 Output

For each participant × outcome:

* Coverage summary printed via `message()` (bin counts, totals, mode)
* Modeling decision recorded in `coverage_mode` column of the significance tibble
* GAM fitted only when justified

Diagnostic messages emitted during fitting:

* `[DRSP_1] Total N: 28, Bins present: 6, Bins with ≥2: 6, Empty bins: 0, Mode: full_cycle`
* `Skipping GAM for CSS_Inatt: insufficient cycle coverage`

---

## 🔬 Interpretation guidance

* **Full-cycle** models (`k = 6`) reflect **cycle-wide dynamics** with standard flexibility
* **Reduced** models (`k = 4`) reflect **coarse trends only** — interpret with caution
* **Skipped** models indicate **insufficient data to estimate a trajectory**

The `coverage_mode` column in output CSVs allows filtering and flagging during downstream analysis.

---

## 🧩 Integration

These functions are defined in both:

* `ADHDCYCLE_5_single_GAMs.Rmd`
* `ADHDCYCLE_5b_single_GAMs.Rmd`

They integrate directly into the existing `fit_outcome()` function and work with the existing `mgcv::bam()` + tidyverse pipeline.

No changes were made to:

* Cycle time computation
* Outcome variable definitions
* Plotting logic
* Existing cycle-half coverage gate (in 5b)

The bin-based coverage check complements the existing per-half observation count gate: the half-gate runs at the participant level to skip participants entirely, while the bin-based check runs per-outcome inside `fit_outcome()` to adapt or skip individual models.

---

## 🎯 Bottom line

This system ensures that:

* GAMs are only run when **statistically defensible**
* Model complexity matches **data reality**
* Cycle effects are **not driven by missingness artifacts**

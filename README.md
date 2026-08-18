# Digitization, Competition, and Bank Stability

Panel evidence from 34 listed European banks across 18 countries, 2015–2024.

This repository contains a hand-collected panel dataset assembled from published
annual reports, combined with Eurostat digitization indicators, together with the
full cleaning, construction and estimation code used in my master's thesis in
Economics at the University of Oslo (2026, grade A).

---

## Research question

<!-- FILL IN: 2–3 sentences. What did you set out to test, and why does it matter?
     Example shape: "Digital adoption in retail banking has accelerated across
     Europe since 2015. This thesis asks whether higher digitization is associated
     with greater or lesser bank stability, and whether that relationship depends
     on the competitive intensity of the national banking market." -->

## Data

**Bank financials** — hand-collected from the published annual reports of 34 listed
European banks in 18 countries, covering 2015–2024. Every figure was extracted
manually and entered into a bank-year panel.

**Digitization indicators** — Eurostat.
<!-- FILL IN: name the exact dataset and indicator code you used, e.g.
     "isoc_ci_ac_i — individuals using the internet for internet banking". -->

**Sample** — <!-- FILL IN: e.g. "340 bank-year observations; unbalanced panel" -->

See [`data/codebook.md`](data/codebook.md) for every variable definition, unit and
source, and [`data/sources.csv`](data/sources.csv) for the bank-level source list.

## Method

<!-- FILL IN: 3–5 sentences. Your dependent variable, main regressors, controls,
     and the specification. Say why fixed effects, and why wild cluster bootstrap
     (few clusters relative to observations). -->

Estimation in R using <!-- FILL IN: e.g. plm, fixest, sandwich, fwildclusterboot -->.

## Repository structure

```
data/raw/          original hand-collected CSV files, unmodified
data/processed/    merged and cleaned bank-year panel
data/codebook.md   variable definitions, units and sources
data/sources.csv   bank list with country, period and source reference
R/                 numbered analysis scripts, run in order
output/            tables and figures produced by the scripts
```

## Reproducing the analysis

1. Clone or download this repository.
2. Open the project in R or RStudio.
3. Install the required packages:

```r
install.packages(c("REPLACE", "WITH", "YOUR", "PACKAGES"))
```

4. Run the scripts in `R/` in numerical order. Each writes its output to `output/`.

Analysis was run on R <!-- FILL IN version, e.g. 4.4.1 -->.

## Main findings

<!-- FILL IN: 2–3 sentences in plain language. State the direction and rough
     magnitude of the main effect, and one robustness result. This is the section
     most readers will actually read — no equations, no jargon. -->

## Limitations

<!-- FILL IN: 2–3 honest lines. Sample size, survivorship among listed banks,
     measurement of digitization, identification. Naming these is a strength,
     not a weakness — it is also what an interviewer will ask about. -->

## Licence

Code is released under the MIT Licence (see `LICENSE`).
The dataset is released under CC BY 4.0 (see `LICENSE-data.md`).
If you use the data, please cite this repository.

## Author

Mohit Singh Thind — MSc Economics, University of Oslo
Supervisor: Jin Cao
[linkedin.com/in/mohit-singh-thind](https://www.linkedin.com/in/mohit-singh-thind)

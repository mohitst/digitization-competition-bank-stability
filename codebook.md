# Codebook

Every variable in the processed panel, with its definition, unit and source.
One row per variable. Keep names in `snake_case` and identical to the column
names in the CSV files.

## Identifiers

| Variable | Definition | Unit | Source |
|---|---|---|---|
| `bank_id` | Unique bank identifier | — | Assigned |
| `bank_name` | Legal name of the institution | — | Annual report |
| `country` | Country of incorporation, ISO 3166-1 alpha-2 | — | Annual report |
| `year` | Fiscal year | Year | Annual report |

## Dependent variable

| Variable | Definition | Unit | Source |
|---|---|---|---|
| `FILL_IN` | e.g. Z-score, computed as (ROA + equity/assets) / sd(ROA) | Index | Own calculation from annual report figures |

## Main regressors

| Variable | Definition | Unit | Source |
|---|---|---|---|
| `FILL_IN` | Digitization indicator | % of individuals | Eurostat, table code FILL_IN |
| `FILL_IN` | Competition measure, e.g. Lerner index or HHI | Index | Own calculation / FILL_IN |

## Controls

| Variable | Definition | Unit | Source |
|---|---|---|---|
| `FILL_IN` | e.g. Total assets, log | log EUR m | Annual report |
| `FILL_IN` | e.g. Non-performing loan ratio | % | Annual report |
| `FILL_IN` | e.g. Cost-to-income ratio | % | Annual report |

---

## Construction notes

Document every judgement call. These are the questions a reader — or an
interviewer — will ask, and having answered them in writing is a strength.

- **Accounting standards.** <!-- FILL IN: were all banks IFRS, or did some report
     under local GAAP? How did you handle differences? -->
- **Currency.** <!-- FILL IN: did you convert to EUR? At which rate, on which
     date, from which source? -->
- **Missing values.** <!-- FILL IN: which bank-years are missing and why. Were
     they left blank, interpolated, or dropped? -->
- **Fiscal year alignment.** <!-- FILL IN: any bank whose fiscal year does not
     match the calendar year, and how it was treated. -->
- **Restatements.** <!-- FILL IN: where a later report restated an earlier figure,
     which version did you use? -->
- **Sample selection.** <!-- FILL IN: how the 34 banks were chosen, and which
     candidates were excluded and why. -->

# ═══════════════════════════════════════════════════════════════════════════════
# Masterdata.R
# Digitization, Competition, and Bank Stability:
# Panel Evidence from European Banks, 2015–2024
# ═══════════════════════════════════════════════════════════════════════════════


# ─────────────────────────────────────────────────────────────────────────────
# Section 1: Libraries, paths, seeds, and shared helpers
# ─────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)
library(fwildclusterboot)
library(knitr)
library(kableExtra)
library(janitor)

# Root paths — set these once for the entire script
root_dir <- "~/OneDrive - Universitetet i Oslo/UiO/Master Thesis/Data"
misc_dir <- "~/OneDrive - Universitetet i Oslo/UiO/Master Thesis/Miscellaneous fintech data"
fig_dir  <- file.path(root_dir, "Figures")
dir.create(fig_dir, showWarnings = FALSE)

# Reproducibility seed for wild cluster bootstrap
set.seed(1)
dqrng::dqset.seed(1)

# ── Shared plot theme ────────────────────────────────────────────────────────
# Used by all four thesis figures to ensure consistent styling
thesis_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(colour = "grey92"),
    axis.line         = element_line(colour = "grey40", linewidth = 0.4),
    axis.ticks        = element_line(colour = "grey40", linewidth = 0.3),
    plot.title        = element_text(size = 13, face = "bold",
                                     margin = margin(b = 6)),
    plot.subtitle     = element_text(size = 10, colour = "grey40",
                                     margin = margin(b = 10)),
    plot.caption      = element_text(size = 8,  colour = "grey50",
                                     margin = margin(t = 8)),
    legend.position   = "bottom",
    legend.title      = element_blank(),
    legend.text       = element_text(size = 9),
    strip.text        = element_text(size = 10, face = "bold"),
    strip.background  = element_blank()
  )

# ── Raw-data helpers ─────────────────────────────────────────────────────────

# Detects whether a CSV uses comma or semicolon as delimiter
detect_delim <- function(file) {
  header <- readLines(file, n = 1, warn = FALSE)
  if (str_count(header, ";") > str_count(header, ",")) ";" else ","
}

# Reads one bank CSV safely, keeping only the columns we need
read_one <- function(f) {
  delim <- detect_delim(f)
  df <- read_delim(
    f,
    delim       = delim,
    col_types   = cols(.default = col_character()),
    show_col_types = FALSE,
    na          = c("", "NA", "N/A")
  )
  df %>% select(any_of(keep_cols))
}

# Parses level-format numbers that may use EU (6.771,00) or US (6,771.00)
# thousands-separator conventions
parse_level_number <- function(x) {
  if (is.na(x) || x == "") return(NA_real_)
  x <- str_trim(x)
  has_comma <- str_detect(x, ",")
  has_dot   <- str_detect(x, "\\.")

  if (has_comma && has_dot) {
    last_comma <- max(str_locate_all(x, ",")[[1]][, 1])
    last_dot   <- max(str_locate_all(x, "\\.")[[1]][, 1])
    if (last_dot > last_comma) {
      return(as.numeric(str_replace_all(x, ",", "")))
    } else {
      return(as.numeric(str_replace_all(str_replace_all(x, "\\.", ""), ",", ".")))
    }
  }
  if (has_dot && !has_comma) {
    if (str_detect(x, "\\.[0-9]{3}$")) {
      return(as.numeric(str_replace_all(x, "\\.", "")))
    } else {
      return(as.numeric(x))
    }
  }
  if (has_comma && !has_dot) {
    if (str_detect(x, ",[0-9]{3}$")) {
      return(as.numeric(str_replace_all(x, ",", "")))
    } else {
      return(as.numeric(str_replace_all(x, ",", ".")))
    }
  }
  as.numeric(x)
}

# Parses BIS numeric values that use commas as thousands separators and
# "-" as a missing-value marker
to_num <- function(x) {
  x <- str_squish(as.character(x))
  x <- na_if(x, "-")
  x <- na_if(x, "")
  x <- str_replace_all(x, ",", "")
  suppressWarnings(as.numeric(x))
}

# ── Eurostat reader helpers ──────────────────────────────────────────────────

# Reads Eurostat digital-indicator CSVs (skip 9 metadata rows; geo in column 1)
read_eurostat_csv <- function(path, value_name) {
  raw <- read_csv(path, skip = 9, show_col_types = FALSE,
                  col_types = cols(.default = col_character()))
  first_col <- names(raw)[1]
  year_cols <- names(raw)[str_detect(names(raw), "^[0-9]{4}$")]
  raw %>%
    select(geo = all_of(first_col), all_of(year_cols)) %>%
    filter(!is.na(geo), geo != "", geo != "GEO (Labels)") %>%
    pivot_longer(all_of(year_cols), names_to = "year", values_to = value_name) %>%
    mutate(
      year            = as.integer(year),
      !!value_name   := na_if(.data[[value_name]], ":"),
      !!value_name   := na_if(.data[[value_name]], " :"),
      !!value_name   := na_if(.data[[value_name]], ""),
      !!value_name   := as.numeric(.data[[value_name]])
    )
}

# Reads Eurostat macro CSVs whose header layout is less predictable
# (auto-detects the header row and delimiter)
read_eurostat_flexible <- function(path, value_name) {
  lines      <- readLines(path, n = 300, warn = FALSE)
  header_idx <- which(
    str_detect(lines, "\\b201[5-9]\\b|\\b202[0-4]\\b") &
      str_detect(tolower(lines), "geo|time|country")
  )[1]
  if (is.na(header_idx)) {
    header_idx <- which(str_detect(lines, "\\b201[5-9]\\b|\\b202[0-4]\\b"))[1]
  }
  if (is.na(header_idx)) stop("Could not find header row with years in: ", path)

  header_line <- lines[header_idx]
  delims      <- c(";", ",", "\t")
  counts      <- sapply(delims, function(d) str_count(header_line, fixed(d)))
  delim       <- delims[which.max(counts)]

  raw <- read_delim(path, delim = delim, skip = header_idx - 1,
                    col_types = cols(.default = col_character()),
                    show_col_types = FALSE, na = c("", "NA", "N/A", ":"))

  geo_col  <- names(raw)[1]
  year_cols <- names(raw)[str_detect(names(raw), "^\\d{4}")]
  if (length(year_cols) == 0)
    stop("No year columns detected in: ", path, "\nHeader: ", header_line)

  raw %>%
    select(geo = all_of(geo_col), all_of(year_cols)) %>%
    filter(!is.na(geo), geo != "",
           !str_detect(geo, "European Union|Euro area")) %>%
    pivot_longer(all_of(year_cols), names_to = "year", values_to = value_name) %>%
    mutate(
      year    = as.integer(str_extract(year, "^\\d{4}")),
      val_txt = str_squish(.data[[value_name]]),
      val_txt = na_if(val_txt, ":"),
      val_txt = str_replace_all(val_txt, "[^0-9\\-\\.,]", ""),
      val_txt = str_replace_all(val_txt, ",", "."),
      !!value_name := as.numeric(val_txt)
    ) %>%
    select(geo, year, all_of(value_name))
}

# Reads Eurostat CSVs for figure production; keeps full country names rather
# than ISO codes, and strips Eurostat flags from values
read_eurostat_clean <- function(path, value_name) {
  raw <- read_csv(path, skip = 9, show_col_types = FALSE,
                  col_types = cols(.default = col_character()))
  first_col <- names(raw)[1]
  year_cols <- names(raw)[str_detect(names(raw), "^[0-9]{4}$")]
  raw %>%
    select(country_name = all_of(first_col), all_of(year_cols)) %>%
    filter(
      !is.na(country_name), country_name != "",
      country_name != "GEO (Labels)",
      !str_detect(country_name, "European Union|Euro area"),
      !country_name %in% c(":", "b", "e", "Observation flags:", "Special value", "p")
    ) %>%
    pivot_longer(all_of(year_cols), names_to = "year", values_to = value_name) %>%
    mutate(
      year           = as.integer(year),
      !!value_name  := str_remove_all(.data[[value_name]], "[a-zA-Z\\s]+$"),
      !!value_name  := na_if(.data[[value_name]], ":"),
      !!value_name  := na_if(.data[[value_name]], ""),
      !!value_name  := as.numeric(.data[[value_name]])
    ) %>%
    mutate(country_lower = str_to_lower(str_squish(country_name)))
}

# ── Summary statistics helpers ───────────────────────────────────────────────

# Computes N, mean, SD, min, median, and max for one variable
summarise_var <- function(data, varname, label, unit = "") {
  x <- data[[varname]]
  x <- x[!is.na(x)]
  tibble(Variable = label, Unit = unit, N = length(x),
         Mean = mean(x), SD = sd(x),
         Min = min(x), Median = median(x), Max = max(x))
}

# Formats a summary panel to an appropriate number of decimal places:
# 4 dp for small ratios (SD < 0.1), 2 dp for larger percentages
fmt_panel <- function(panel) {
  panel %>%
    mutate(
      dp     = if_else(abs(SD) < 0.1, 4, 2),
      Mean   = mapply(function(x, d) formatC(x, digits = d, format = "f"), Mean,   dp),
      SD     = mapply(function(x, d) formatC(x, digits = d, format = "f"), SD,     dp),
      Min    = mapply(function(x, d) formatC(x, digits = d, format = "f"), Min,    dp),
      Median = mapply(function(x, d) formatC(x, digits = d, format = "f"), Median, dp),
      Max    = mapply(function(x, d) formatC(x, digits = d, format = "f"), Max,    dp)
    ) %>%
    select(-dp)
}

# Prints a formatted summary panel to the console
print_panel <- function(panel_fmt, title, note = NULL) {
  cat(title, "\n")
  cat(strrep("-", 88), "\n")
  cat(sprintf("%-36s %5s %9s %9s %9s %9s %9s\n",
              "Variable", "N", "Mean", "SD", "Min", "Median", "Max"))
  cat(strrep("-", 88), "\n")
  for (i in seq_len(nrow(panel_fmt))) {
    r <- panel_fmt[i, ]
    cat(sprintf("%-36s %5d %9s %9s %9s %9s %9s\n",
                r$Variable, r$N, r$Mean, r$SD, r$Min, r$Median, r$Max))
  }
  if (!is.null(note)) cat(note, "\n")
  cat("\n")
}

# Adds significance stars based on wild bootstrap p-values
stars_fn <- function(p) {
  case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
}

# Formats a number to 4 significant figures (used in result tables)
fmt <- function(x, digits = 4) formatC(x, digits = digits, format = "g")


# ─────────────────────────────────────────────────────────────────────────────
# Section 2: Data loading and cleaning
# ─────────────────────────────────────────────────────────────────────────────

# Columns to retain from each bank's raw CSV
keep_cols <- c(
  "Bank", "Fiscal year", "Currency", "Units", "Variable",
  "Value", "Statement", "Exact line-item label in report",
  "Page number", "Section/Table name"
)

# ── Step A: Read and stack all bank CSVs ────────────────────────────────────
files       <- list.files(root_dir, pattern = "\\.csv$",
                          full.names = TRUE, recursive = TRUE)
master_long <- map_dfr(files, read_one)

# ── Step B: Drop rows missing any required field ────────────────────────────
master_long <- master_long %>%
  filter(
    !is.na(Bank), Bank != "",
    !is.na(`Fiscal year`), `Fiscal year` != "",
    !is.na(Variable), Variable != "",
    !is.na(Value), Value != ""
  )

# ── Step C: Standardize bank names (fix typos and merge duplicates) ─────────
master_long <- master_long %>%
  mutate(
    Bank = str_squish(Bank),
    Bank = case_when(
      Bank %in% c("ABN AMRO Bank N.V.", "ABN AMRO Group N.V.") ~ "ABN AMRO",
      Bank %in% c("Danske Bank", "Danske Bank Group")          ~ "Danske Bank",
      Bank %in% c("Eurobank Ergasias S.A.", "Eurobank S.A.")   ~ "Eurobank",
      Bank == "SpareBank 1 Sør-Norge"                          ~ "SpareBank 1 SR-Bank",
      Bank == "ntesa Sanpaolo"                                  ~ NA_character_,
      TRUE ~ Bank
    )
  ) %>%
  filter(!is.na(Bank))

# ── Step D: Tag asset quality rows and record their original label ───────────
master_long <- master_long %>%
  mutate(
    label_lower = str_to_lower(coalesce(`Exact line-item label in report`, "")),
    var_lower   = str_to_lower(coalesce(Variable, "")),

    is_asset_quality_row = str_detect(
      var_lower, "npl|npe|non-performing|stage\\s*3|impaired"
    ),

    asset_quality_type = case_when(
      is_asset_quality_row & str_detect(label_lower, "stage\\s*3") ~ "Stage3",
      is_asset_quality_row & (str_detect(label_lower, "\\bnpe\\b") |
                                str_detect(label_lower, "non-performing exposure")) ~ "NPE",
      is_asset_quality_row & (str_detect(label_lower, "\\bnpl\\b") |
                                str_detect(label_lower, "non-performing loan")) ~ "NPL",
      is_asset_quality_row & str_detect(label_lower, "apm") &
        str_detect(label_lower, "impaired") ~ "Impaired(APM)",
      is_asset_quality_row & str_detect(label_lower, "impaired") ~ "Impaired",
      is_asset_quality_row ~ "Other",
      TRUE ~ NA_character_
    ),

    asset_quality_ratio_label = if_else(
      is_asset_quality_row,
      `Exact line-item label in report`,
      NA_character_
    )
  ) %>%
  select(-label_lower, -var_lower)

# ── Step E: Standardize variable names to bridge split column labels ─────────
# Some banks label the same item differently across years; this maps them to a
# consistent name so pivot_wider produces a single column per concept
master_long <- master_long %>%
  mutate(
    var_lower    = str_to_lower(str_squish(Variable)),
    Variable_std = case_when(
      var_lower == "debt securities issued" ~ "Debt securities issued / wholesale funding",
      str_detect(var_lower, "debt securities issued") &
        str_detect(var_lower, "wholesale") ~ "Debt securities issued / wholesale funding",
      var_lower == "impairment losses" ~ "Impairment losses / credit loss expense",
      str_detect(var_lower, "impairment") & str_detect(var_lower, "credit") &
        str_detect(var_lower, "loss") ~ "Impairment losses / credit loss expense",
      str_detect(var_lower, "total operating income") ~ "Total operating income",
      str_detect(var_lower, "total income")           ~ "Total operating income",
      str_detect(var_lower, "net operating income")   ~ "Total operating income",
      str_detect(var_lower, "loan loss allowance")    ~ "Loan loss allowance",
      str_detect(var_lower, "loans") & str_detect(var_lower, "customers") &
        str_detect(var_lower, "gross") ~ "Loans and advances to customers (gross)",
      var_lower == "loans to customers" ~ "Loans and advances to customers (gross)",
      TRUE ~ Variable
    )
  ) %>%
  select(-var_lower)

# ── Step F: Build wide panel (one row per bank-year) ─────────────────────────

# Keep asset quality rows separately to merge back after widening
aq <- master_long %>%
  filter(is_asset_quality_row == TRUE) %>%
  select(Bank, `Fiscal year`,
         asset_quality_value = Value,
         asset_quality_type,
         asset_quality_ratio_label) %>%
  group_by(Bank, `Fiscal year`) %>%
  slice(1) %>%
  ungroup()

wide_main <- master_long %>%
  filter(is_asset_quality_row != TRUE) %>%
  select(Bank, `Fiscal year`, Variable_std, Value) %>%
  group_by(Bank, `Fiscal year`, Variable_std) %>%
  summarise(Value = first(na.omit(Value)), .groups = "drop") %>%
  pivot_wider(names_from = Variable_std, values_from = Value)

panel_wide <- wide_main %>%
  left_join(aq, by = c("Bank", "Fiscal year")) %>%
  select(-any_of("NA")) %>%
  select(where(~ !all(is.na(.))))

cat("Banks in long dataset:", n_distinct(master_long$Bank), "\n")
cat("Rows in long dataset:", nrow(master_long), "\n")
cat("Rows in wide panel:", nrow(panel_wide), "\n")

# ── Diagnostics: missingness and unit consistency ────────────────────────────

master_long %>%
  summarise(
    n_rows     = n(),
    n_banks    = n_distinct(Bank),
    n_bank_na  = sum(is.na(Bank) | Bank == ""),
    n_var_na   = sum(is.na(Variable) | Variable == ""),
    n_value_na = sum(is.na(Value) | Value == "")
  ) %>%
  print()

master_long %>%
  count(Units, sort = TRUE) %>%
  print(n = Inf)

master_long %>%
  filter(is.na(Units) | Units == "") %>%
  count(Variable_std, sort = TRUE) %>%
  print(n = 50)


# ─────────────────────────────────────────────────────────────────────────────
# Section 3: Variable construction
# ─────────────────────────────────────────────────────────────────────────────

# ── Step G: Convert raw text values to numeric (standardized to millions) ────

df_num <- master_long %>%
  mutate(
    # Map the many unit labels in the raw data to four canonical categories
    units_lower = str_to_lower(str_squish(coalesce(Units, ""))),
    units_std   = case_when(
      str_detect(units_lower, "%") |
        str_detect(units_lower, "per cent")                         ~ "percent",
      str_detect(units_lower, "billion") |
        str_detect(units_lower, "\\bbn\\b") |
        str_detect(units_lower, "€bn")                             ~ "billion",
      str_detect(units_lower, "thousand") |
        str_detect(units_lower, "'000") |
        str_detect(units_lower, "000")                             ~ "thousand",
      str_detect(units_lower, "million") |
        str_detect(units_lower, "€m") |
        str_detect(units_lower, "\\bm\\b") |
        str_detect(units_lower, "sekm") |
        str_detect(units_lower, "dkkm") |
        str_detect(units_lower, "nok millions")                    ~ "million",
      TRUE ~ "unknown"
    ),

    # Clean value text before parsing
    value_txt = str_squish(coalesce(Value, "")),
    value_txt = if_else(str_to_lower(value_txt) %in%
                          c("not found", "n/a", "na"), NA_character_, value_txt),
    # Convert bracketed numbers to negative (accounting convention)
    value_txt = case_when(
      str_detect(value_txt, "^\\(.*\\)$") ~
        paste0("-", str_replace_all(value_txt, "[\\(\\)]", "")),
      TRUE ~ value_txt
    ),
    # PKO Bank 2016 patch: level values were entered with spurious decimal places
    value_txt = case_when(
      Bank == "PKO Bank Polski SA Group" &
        `Fiscal year` == "2016" &
        units_std %in% c("thousand", "million", "billion") ~
        str_replace(value_txt, "([\\.,][0-9]+)$", ""),
      TRUE ~ value_txt
    ),
    value_txt = str_replace_all(value_txt, "%", ""),
    value_txt = str_replace_all(value_txt, "[–—−]", "-"),
    value_txt = str_replace_all(value_txt, "[^0-9\\-\\.,]", ""),

    # Parse to numeric; percentages use simple decimal conversion, levels use
    # parse_level_number which handles both EU and US separator conventions
    value_num = case_when(
      units_std == "percent"                          ~
        as.numeric(str_replace_all(value_txt, ",", ".")),
      units_std %in% c("thousand", "million", "billion") ~
        mapply(parse_level_number, value_txt),
      TRUE ~ NA_real_
    ),

    # Convert all level values to millions within their original currency
    value_million = case_when(
      units_std == "thousand" ~ value_num / 1000,
      units_std == "million"  ~ value_num,
      units_std == "billion"  ~ value_num * 1000,
      units_std == "percent"  ~ value_num,
      TRUE ~ NA_real_
    ),

    # Force known cost items (expenses) to positive values; banks sometimes
    # report these with a negative sign, which would distort ratios
    var_std_lower  = str_to_lower(str_squish(coalesce(Variable_std, Variable, ""))),
    is_cost_item   = str_detect(
      var_std_lower, "interest expense|operating expenses|fee.*expense"
    ),
    value_million_std = case_when(
      units_std != "percent" & is_cost_item ~ abs(value_million),
      TRUE ~ value_million
    )
  ) %>%
  select(-units_lower, -var_std_lower)

# Collapse to a single authoritative numeric column (value_final) with a unit label
df_num <- df_num %>%
  mutate(
    value_final = case_when(
      units_std == "percent"                              ~ value_num,
      units_std %in% c("thousand", "million", "billion") ~ value_million_std,
      TRUE ~ NA_real_
    ),
    value_final_unit = case_when(
      units_std == "percent"                              ~ "%",
      units_std %in% c("thousand", "million", "billion") ~ "million",
      TRUE ~ NA_character_
    )
  )

# ── Step H: Build wide numeric panel ────────────────────────────────────────

aq_num <- df_num %>%
  filter(is_asset_quality_row == TRUE) %>%
  select(Bank, `Fiscal year`,
         asset_quality_value = value_final,
         asset_quality_type,
         asset_quality_ratio_label) %>%
  group_by(Bank, `Fiscal year`) %>%
  slice(1) %>%
  ungroup()

wide_main_num <- df_num %>%
  filter(is_asset_quality_row != TRUE) %>%
  select(Bank, `Fiscal year`, Variable_std, value_final) %>%
  group_by(Bank, `Fiscal year`, Variable_std) %>%
  summarise(value_final = first(na.omit(value_final)), .groups = "drop") %>%
  pivot_wider(names_from = Variable_std, values_from = value_final)

panel_wide_numeric <- wide_main_num %>%
  left_join(aq_num, by = c("Bank", "Fiscal year")) %>%
  select(-any_of("NA")) %>%
  select(where(~ !all(is.na(.))))

write_csv(panel_wide_numeric, file.path(root_dir, "panel_wide_numeric.csv"))

# ── Step I: Compute core bank-level ratios ───────────────────────────────────

panel <- read_csv(
  file.path(root_dir, "panel_wide_numeric.csv"),
  col_types = cols(.default = col_double(),
                   Bank = col_character(),
                   `Fiscal year` = col_character(),
                   asset_quality_type = col_character(),
                   asset_quality_ratio_label = col_character())
) %>%
  mutate(year = as.integer(`Fiscal year`)) %>%
  arrange(Bank, year)

panel_vars <- panel %>%
  group_by(Bank) %>%
  mutate(
    roa            = `Net profit` / `Total assets`,
    margin_proxy   = `Net interest income` / `Total assets`,
    cost_to_income = `Operating expenses` / `Total operating income`,

    loans_to_assets    = `Loans and advances to customers (gross)` / `Total assets`,
    deposits_to_assets = `Customer deposits` / `Total assets`,
    loan_to_deposit    = `Loans and advances to customers (gross)` / `Customer deposits`,

    # Cost of risk: provisions scaled by gross loans (Louzis et al., 2012)
    cost_of_risk = `Impairment losses / credit loss expense` /
      `Loans and advances to customers (gross)`,

    log_assets = log(`Total assets`),

    # Year-on-year loan growth, computed within bank
    loan_growth = (`Loans and advances to customers (gross)` /
                     lag(`Loans and advances to customers (gross)`)) - 1
  ) %>%
  ungroup()

write_csv(panel_vars, file.path(root_dir, "panel_wide_regression_ready.csv"))

# ── Step J: Assign countries, merge internet banking adoption ────────────────

panel <- read_csv(file.path(root_dir, "panel_wide_regression_ready.csv"),
                  col_types = cols(.default = col_guess()))

# Hand-coded country mapping for the 34 sample banks
bank_map <- panel %>%
  distinct(Bank) %>%
  arrange(Bank) %>%
  mutate(
    country = c(
      "Netherlands", "Ireland", "Spain", "France", "Romania", "Portugal",
      "Spain", "Cyprus", "Ireland", "Belgium", "Spain", "Germany", "France",
      "Norway", "Denmark", "Germany", "Austria", "Greece", "Netherlands",
      "Italy", "Denmark", "Belgium", "Greece", "Finland", "Finland",
      "Hungary", "Poland", "Sweden", "France", "Norway", "Norway",
      "Sweden", "Sweden", "Italy"
    ),
    eurostat_geo = c(
      "NL", "IE", "ES", "FR", "RO", "PT", "ES", "CY", "IE", "BE", "ES",
      "DE", "FR", "NO", "DK", "DE", "AT", "GR", "NL", "IT", "DK", "BE",
      "GR", "FI", "FI", "HU", "PL", "SE", "FR", "NO", "NO", "SE", "SE", "IT"
    )
  )

write_csv(bank_map, file.path(root_dir, "bank_country_map_template.csv"))

# Merge internet banking adoption (Eurostat digital indicator)
internet_banking <- read_eurostat_csv(
  file.path(misc_dir, "Individuals using the internet for internet banking.csv"),
  "internet_banking"
) %>%
  filter(!str_detect(geo, "European Union|Euro area"))

panel2 <- panel %>%
  left_join(bank_map %>% select(Bank, country), by = "Bank") %>%
  left_join(internet_banking, by = c("country" = "geo", "year" = "year"))

panel2 %>%
  summarise(share_missing_internet_banking = mean(is.na(internet_banking))) %>%
  print()

write_csv(panel2, file.path(root_dir, "panel_with_internet_banking.csv"))

# ── Step K: Build the composite digitization index (Panel 3) ─────────────────

internet_frequent <- read_eurostat_csv(
  file.path(misc_dir, "Individuals frequently using the internet.csv"),
  "internet_frequent"
) %>% filter(!str_detect(geo, "European Union|Euro area"))

# Internet purchases stitched across two Eurostat vintages
ibuy <- read_eurostat_csv(
  file.path(misc_dir, "Internet purchases by individuals (2015-2019).csv"),
  "internet_purchases"
) %>%
  filter(!str_detect(geo, "European Union|Euro area"), year <= 2019)

ib20 <- read_eurostat_csv(
  file.path(misc_dir, "Internet purchases by individuals (2020-2024).csv"),
  "internet_purchases"
) %>%
  filter(!str_detect(geo, "European Union|Euro area"), year >= 2020)

internet_purchases <- bind_rows(ibuy, ib20) %>%
  mutate(post2020 = as.integer(year >= 2020))

panel3 <- panel2 %>%
  left_join(internet_frequent,  by = c("country" = "geo", "year" = "year")) %>%
  left_join(internet_purchases, by = c("country" = "geo", "year" = "year"))

# Year-standardized z-scores combined into composite indices
panel3 <- panel3 %>%
  group_by(year) %>%
  mutate(
    z_banking   = as.numeric(scale(internet_banking)),
    z_freq      = as.numeric(scale(internet_frequent)),
    z_purchases = as.numeric(scale(internet_purchases)),
    # Two-component index: banking adoption + general internet frequency
    digital_index_2 = rowMeans(cbind(z_banking, z_freq), na.rm = FALSE),
    # Three-component index: adds internet purchases as a demand-side proxy
    digital_index_3 = rowMeans(cbind(z_banking, z_freq, z_purchases), na.rm = FALSE)
  ) %>%
  ungroup()

panel3 %>%
  summarise(
    miss_banking   = mean(is.na(internet_banking)),
    miss_freq      = mean(is.na(internet_frequent)),
    miss_purchases = mean(is.na(internet_purchases)),
    miss_index2    = mean(is.na(digital_index_2)),
    miss_index3    = mean(is.na(digital_index_3))
  ) %>% print()

write_csv(panel3, file.path(root_dir, "panel3_banking_freq_purchases.csv"))

# ── Step L: Merge macro controls (GDP, unemployment, inflation, policy rate) ──

gdp_growth   <- read_eurostat_flexible(
  file.path(misc_dir, "Real GDP growth rate - volume.csv"), "gdp_growth"
)
unemployment <- read_eurostat_flexible(
  file.path(misc_dir, "Total unemployment rate.csv"), "unemployment"
)
inflation    <- read_eurostat_flexible(
  file.path(misc_dir, "Inflation rate.csv"), "inflation"
)

panel3 <- read_csv(file.path(root_dir, "panel3_banking_freq_purchases.csv"),
                   col_types = cols(.default = col_guess()))

panel3_macro <- panel3 %>%
  left_join(gdp_growth,   by = c("country" = "geo", "year" = "year")) %>%
  left_join(unemployment, by = c("country" = "geo", "year" = "year")) %>%
  left_join(inflation,    by = c("country" = "geo", "year" = "year"))

panel3_macro %>%
  summarise(
    miss_gdp   = mean(is.na(gdp_growth)),
    miss_unemp = mean(is.na(unemployment)),
    miss_infl  = mean(is.na(inflation))
  ) %>% print()

# Norges Bank policy rate (annual, key policy rate = KPRA/SD series)
norges_bank <- read_delim(
  file.path(misc_dir, "IR.csv"), delim = ";",
  col_types = cols(.default = col_character()), show_col_types = FALSE
)

nb_policy_annual <- norges_bank %>%
  filter(FREQ == "A", INSTRUMENT_TYPE == "KPRA", TENOR == "SD") %>%
  transmute(
    year           = as.integer(TIME_PERIOD),
    nb_policy_rate = as.numeric(OBS_VALUE)
  ) %>%
  filter(year >= 2015, year <= 2024) %>%
  distinct(year, .keep_all = TRUE)

# ECB deposit facility rate (daily observations → annual average)
ecb <- read_csv(file.path(misc_dir, "ECB rates.csv"),
                col_types = cols(.default = col_guess()))

dep_col <- "Deposit facility - date of changes (raw data) - Level (FM.D.U2.EUR.4F.KR.DFR.LEV)"

ecb_annual <- ecb %>%
  transmute(
    date    = as.Date(DATE),
    year    = as.integer(format(as.Date(DATE), "%Y")),
    dep_raw = .data[[dep_col]]
  ) %>%
  arrange(date) %>%
  mutate(
    dep_raw  = str_squish(as.character(dep_raw)),
    dep_raw  = na_if(dep_raw, ""),
    dep_raw  = str_replace_all(dep_raw, ",", "."),
    dep_raw  = str_replace_all(dep_raw, "[^0-9\\-\\.]", ""),
    dep_rate = as.numeric(dep_raw)
  ) %>%
  # Carry the prevailing rate forward between ECB announcement dates
  fill(dep_rate, .direction = "down") %>%
  filter(year >= 2015, year <= 2024) %>%
  group_by(year) %>%
  summarise(ecb_deposit_rate = mean(dep_rate, na.rm = TRUE), .groups = "drop")

# Euro area country list used to assign the ECB rate to non-Norwegian banks
euro_area <- c(
  "Austria", "Belgium", "Cyprus", "Estonia", "Finland", "France", "Germany",
  "Greece", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
  "Netherlands", "Portugal", "Slovakia", "Slovenia", "Spain"
)

panel3_final <- panel3_macro %>%
  left_join(nb_policy_annual, by = "year") %>%
  left_join(ecb_annual,       by = "year") %>%
  mutate(
    policy_rate = case_when(
      country == "Norway" ~ nb_policy_rate,
      TRUE                ~ ecb_deposit_rate
    )
  ) %>%
  select(-nb_policy_rate, -ecb_deposit_rate)

panel3_final %>%
  group_by(country) %>%
  summarise(miss_policy = mean(is.na(policy_rate))) %>%
  arrange(desc(miss_policy)) %>%
  print(n = Inf)

write_csv(panel3_final, file.path(root_dir, "panel3_with_macros_and_policy.csv"))


# ─────────────────────────────────────────────────────────────────────────────
# Section 4: Summary statistics (Table 1)
# ─────────────────────────────────────────────────────────────────────────────

df <- read_csv(
  file.path(root_dir, "panel3_with_macros_and_policy.csv"),
  col_types = cols(.default = col_guess())
) %>%
  mutate(year = as.integer(year))

cat("Panel dimensions:", nrow(df), "rows |",
    n_distinct(df$Bank), "banks |",
    n_distinct(df$year), "years |",
    n_distinct(df$country), "countries\n\n")

# Panel A: bank-level outcome and control variables
panel_a <- bind_rows(
  summarise_var(df, "roa",                 "Return on assets (ROA)",      "%"),
  summarise_var(df, "margin_proxy",        "NII margin",                  "%"),
  summarise_var(df, "cost_to_income",      "Cost-to-income ratio",        "%"),
  summarise_var(df, "loan_growth",         "Loan growth (YoY)",           "%"),
  summarise_var(df, "cost_of_risk",        "Cost of risk",                "%"),
  summarise_var(df, "asset_quality_value", "Asset quality ratio (a)",     "%"),
  summarise_var(df, "log_assets",          "Log total assets",            "log mn"),
  summarise_var(df, "loan_to_deposit",     "Loan-to-deposit ratio",       "ratio"),
  summarise_var(df, "CET1 ratio",          "CET1 ratio",                  "%")
)

# Panel B: country-year digitization and macro variables
# N here reflects unique country-year observations to avoid double-counting
country_year <- df %>% distinct(country, year, .keep_all = TRUE)
cat("Unique country-year observations:", nrow(country_year), "\n\n")

panel_b <- bind_rows(
  summarise_var(country_year, "internet_banking", "Internet banking adoption", "%"),
  summarise_var(country_year, "digital_index_2",  "Digital index (z-score)",   "index"),
  summarise_var(country_year, "gdp_growth",        "GDP growth",               "%"),
  summarise_var(country_year, "unemployment",      "Unemployment rate",         "%"),
  summarise_var(country_year, "inflation",         "Inflation rate (HICP)",     "%"),
  summarise_var(country_year, "policy_rate",       "Policy rate",               "%")
)

panel_a_fmt <- fmt_panel(panel_a)
panel_b_fmt <- fmt_panel(panel_b)

cat("================================================================\n")
cat("TABLE 1: SUMMARY STATISTICS\n")
cat("Sample: 34 listed European banks, 18 countries, 2015-2024\n")
cat("================================================================\n\n")

print_panel(
  panel_a_fmt,
  "Panel A: Bank-level variables (N = bank-year observations)",
  "(a) Asset quality combines NPL, NPE, Stage 3, and impaired loan ratios."
)
print_panel(panel_b_fmt,
            "Panel B: Country-year variables (N = unique country-year observations)")

# Missingness check — any variable exceeding 10% missing needs a note in the
# Data section
cat("Missingness (% of bank-year observations):\n")
cat(strrep("-", 45), "\n")

vars_check <- c(
  "roa", "margin_proxy", "cost_to_income", "loan_growth",
  "cost_of_risk", "asset_quality_value",
  "log_assets", "loan_to_deposit", "CET1 ratio",
  "internet_banking", "gdp_growth", "unemployment", "inflation", "policy_rate"
)

df %>%
  summarise(across(all_of(vars_check), ~ mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") %>%
  mutate(
    display = paste0(round(pct_missing * 100, 1), "%"),
    flag    = if_else(pct_missing > 0.10, " <- mention in Data section", "")
  ) %>%
  select(variable, display, flag) %>%
  print(n = Inf)

# Breakdown of how asset quality is measured across the sample
cat("\nAsset quality definition breakdown:\n")
cat(strrep("-", 50), "\n")
df %>%
  filter(!is.na(asset_quality_value)) %>%
  count(asset_quality_type, sort = TRUE) %>%
  mutate(
    pct        = paste0(round(n / sum(n) * 100, 1), "%"),
    definition = case_when(
      asset_quality_type == "NPL"      ~ "Non-performing loan ratio",
      asset_quality_type == "NPE"      ~ "Non-performing exposure ratio",
      asset_quality_type == "Stage3"   ~ "IFRS 9 Stage 3 ratio",
      asset_quality_type == "Impaired" ~ "Impaired loans ratio",
      TRUE                             ~ asset_quality_type
    )
  ) %>%
  select(asset_quality_type, definition, n, pct) %>%
  print()

# Effective sample sizes for each regression (complete cases across all controls)
df %>%
  summarise(
    n_margin = sum(!is.na(margin_proxy) & !is.na(internet_banking) &
                     !is.na(log_assets) & !is.na(loan_to_deposit) &
                     !is.na(`CET1 ratio`) & !is.na(gdp_growth) &
                     !is.na(unemployment) & !is.na(inflation) &
                     !is.na(policy_rate) & !is.na(cost_of_risk)),
    n_loang  = sum(!is.na(loan_growth) & !is.na(internet_banking) &
                     !is.na(log_assets) & !is.na(loan_to_deposit) &
                     !is.na(`CET1 ratio`) & !is.na(gdp_growth) &
                     !is.na(unemployment) & !is.na(inflation) &
                     !is.na(policy_rate) & !is.na(cost_of_risk)),
    n_aq     = sum(!is.na(asset_quality_value) & !is.na(asset_quality_type) &
                     !is.na(internet_banking) & !is.na(log_assets) &
                     !is.na(loan_to_deposit) & !is.na(`CET1 ratio`) &
                     !is.na(gdp_growth) & !is.na(unemployment) &
                     !is.na(inflation) & !is.na(policy_rate)),
    n_cor    = sum(!is.na(cost_of_risk) & !is.na(internet_banking) &
                     !is.na(log_assets) & !is.na(loan_to_deposit) &
                     !is.na(`CET1 ratio`) & !is.na(gdp_growth) &
                     !is.na(unemployment) & !is.na(inflation) &
                     !is.na(policy_rate)),
    n_roa    = sum(!is.na(roa) & !is.na(internet_banking) &
                     !is.na(log_assets) & !is.na(loan_to_deposit) &
                     !is.na(`CET1 ratio`) & !is.na(gdp_growth) &
                     !is.na(unemployment) & !is.na(inflation) &
                     !is.na(policy_rate) & !is.na(cost_of_risk)),
    n_cti    = sum(!is.na(cost_to_income) & !is.na(internet_banking) &
                     !is.na(log_assets) & !is.na(loan_to_deposit) &
                     !is.na(`CET1 ratio`) & !is.na(gdp_growth) &
                     !is.na(unemployment) & !is.na(inflation) &
                     !is.na(policy_rate) & !is.na(cost_of_risk))
  ) %>%
  print()

# Save summary table for Word/LaTeX import
bind_rows(
  panel_a_fmt %>% mutate(Panel = "A: Bank-level"),
  panel_b_fmt %>% mutate(Panel = "B: Country-year")
) %>%
  select(Panel, Variable, Unit, N, Mean, SD, Min, Median, Max) %>%
  write_csv(file.path(root_dir, "table1_summary_statistics.csv"))


# ─────────────────────────────────────────────────────────────────────────────
# Section 5: Main regressions
# ─────────────────────────────────────────────────────────────────────────────

df <- read_csv(
  file.path(root_dir, "panel3_with_macros_and_policy.csv"),
  col_types = cols(.default = col_guess())
) %>%
  mutate(
    year               = as.integer(year),
    Bank               = factor(Bank),
    year_fe            = factor(year),
    asset_quality_type = factor(asset_quality_type)
  )

cat("Panel loaded:", nrow(df), "rows |", n_distinct(df$Bank), "banks |",
    n_distinct(df$year), "years |", n_distinct(df$country), "countries\n\n")

# ── 5a: feols with bank-clustered SE (main table display) ───────────────────
# All models include bank + year fixed effects and the full macro control set.
# cost_of_risk is excluded from controls when it is the dependent variable.

cat("Running feols models...\n")

m_margin <- feols(
  margin_proxy ~ internet_banking + log_assets + loan_to_deposit + cost_of_risk +
    `CET1 ratio` + gdp_growth + unemployment + inflation + policy_rate | Bank + year,
  data = df, cluster = ~Bank
)

m_loang <- feols(
  loan_growth ~ internet_banking + log_assets + loan_to_deposit + cost_of_risk +
    `CET1 ratio` + gdp_growth + unemployment + inflation + policy_rate | Bank + year,
  data = df, cluster = ~Bank
)

# Asset quality feols: asset_quality_type absorbed as a factor within the FE
m_aq_feols <- feols(
  asset_quality_value ~ internet_banking + log_assets + loan_to_deposit +
    `CET1 ratio` + gdp_growth + unemployment + inflation + policy_rate +
    asset_quality_type | Bank + year,
  data = df, cluster = ~Bank
)

m_cor <- feols(
  cost_of_risk ~ internet_banking + log_assets + loan_to_deposit +
    `CET1 ratio` + gdp_growth + unemployment + inflation + policy_rate | Bank + year,
  data = df, cluster = ~Bank
)

# Secondary outcomes
m_roa <- feols(
  roa ~ internet_banking + log_assets + loan_to_deposit + cost_of_risk +
    `CET1 ratio` + gdp_growth + unemployment + inflation + policy_rate | Bank + year,
  data = df, cluster = ~Bank
)

m_cti <- feols(
  cost_to_income ~ internet_banking + log_assets + loan_to_deposit + cost_of_risk +
    `CET1 ratio` + gdp_growth + unemployment + inflation + policy_rate | Bank + year,
  data = df, cluster = ~Bank
)

cat("\n--- feols results: main outcomes (bank-clustered SE) ---\n")
etable(m_margin, m_loang, m_aq_feols, m_cor,
       headers = c("Margin", "Loan growth", "Asset quality", "Cost of risk"))

cat("\n--- feols results: all six outcomes ---\n")
etable(m_margin, m_loang, m_aq_feols, m_cor, m_roa, m_cti,
       headers = c("Margin", "Loan growth", "Asset quality",
                   "Cost of risk", "ROA", "Cost-to-income"))

# ROA comparison: main internet_banking spec vs composite digital index
m_roa_idx <- feols(
  roa ~ digital_index_3 + log_assets + loan_to_deposit + cost_of_risk +
    `CET1 ratio` + gdp_growth + unemployment + inflation + policy_rate | Bank + year,
  data = df, cluster = ~Bank
)

cat("\n--- ROA: internet_banking vs digital_index_3 ---\n")
etable(m_roa, m_roa_idx)

# ── 5b: Wild cluster bootstrap (country-clustered, B = 9999) ────────────────
# internet_banking varies at the country-year level, so country-clustered
# inference is the correct standard (Cameron & Miller, 2015). With 18 clusters
# we use the Rademacher wild bootstrap rather than asymptotic cluster-SE.

cat("\nRunning wild cluster bootstrap (B=9999, clustered by country)...\n")
cat("This takes a few minutes — do not close R.\n\n")

# Helper for standard outcomes (no asset_quality_type control)
run_boot_standard <- function(df, yvar, B = 9999) {
  base_vars <- c(yvar, "internet_banking", "log_assets", "loan_to_deposit",
                 "CET1 ratio", "gdp_growth", "unemployment", "inflation",
                 "policy_rate", "country", "Bank", "year_fe")
  if (yvar != "cost_of_risk") base_vars <- c(base_vars, "cost_of_risk")

  d <- df %>% select(all_of(base_vars)) %>% drop_na()

  rhs <- if (yvar == "cost_of_risk") {
    "internet_banking + log_assets + loan_to_deposit + `CET1 ratio` +
     gdp_growth + unemployment + inflation + policy_rate + Bank + year_fe"
  } else {
    "internet_banking + log_assets + loan_to_deposit + cost_of_risk + `CET1 ratio` +
     gdp_growth + unemployment + inflation + policy_rate + Bank + year_fe"
  }

  m  <- lm(as.formula(paste0("`", yvar, "` ~ ", rhs)), data = d)
  bt <- boottest(m, param = "internet_banking",
                 clustid = ~country, B = B, type = "rademacher")

  list(outcome     = yvar,
       n_obs       = nrow(d),
       n_countries = length(unique(d$country)),
       coef        = coef(m)["internet_banking"],
       se_ols      = sqrt(diag(vcov(m)))["internet_banking"],
       bt_pval     = bt$p_val,
       bt_ci_low   = bt$conf_int[1],
       bt_ci_high  = bt$conf_int[2],
       bt_obj      = bt)
}

# Separate helper for asset quality — adds asset_quality_type dummies to
# control for the heterogeneous measurement definitions across banks
run_boot_aq <- function(df, B = 9999) {
  vars_needed <- c(
    "asset_quality_value", "asset_quality_type",
    "internet_banking", "log_assets", "loan_to_deposit", "CET1 ratio",
    "gdp_growth", "unemployment", "inflation", "policy_rate",
    "country", "Bank", "year_fe"
  )
  d <- df %>% select(all_of(vars_needed)) %>% drop_na()

  m <- lm(
    asset_quality_value ~ internet_banking + log_assets + loan_to_deposit +
      `CET1 ratio` + gdp_growth + unemployment + inflation + policy_rate +
      asset_quality_type + Bank + year_fe,
    data = d
  )
  bt <- boottest(m, param = "internet_banking",
                 clustid = ~country, B = B, type = "rademacher")

  list(outcome     = "asset_quality_value",
       n_obs       = nrow(d),
       n_countries = length(unique(d$country)),
       coef        = coef(m)["internet_banking"],
       se_ols      = sqrt(diag(vcov(m)))["internet_banking"],
       bt_pval     = bt$p_val,
       bt_ci_low   = bt$conf_int[1],
       bt_ci_high  = bt$conf_int[2],
       bt_obj      = bt)
}

cat("Running: margin_proxy...\n"); res_margin <- run_boot_standard(df, "margin_proxy")
cat("Running: loan_growth...\n");  res_loang  <- run_boot_standard(df, "loan_growth")
cat("Running: asset_quality_value (with type dummies)...\n"); res_aq <- run_boot_aq(df)
cat("Running: cost_of_risk...\n"); res_cor    <- run_boot_standard(df, "cost_of_risk")
cat("Running: roa (appendix)...\n");             res_roa <- run_boot_standard(df, "roa")
cat("Running: cost_to_income (appendix)...\n");  res_cti <- run_boot_standard(df, "cost_to_income")
cat("\nAll bootstrap runs complete.\n")

# ── 5c: Collect and save bootstrap results ───────────────────────────────────

boot_summary <- tibble(
  outcome     = c("margin_proxy", "loan_growth",
                  "asset_quality_value", "cost_of_risk"),
  framework   = c("Competition", "Competition", "Stability", "Stability"),
  n_obs       = c(res_margin$n_obs,  res_loang$n_obs,
                  res_aq$n_obs,      res_cor$n_obs),
  n_countries = c(res_margin$n_countries, res_loang$n_countries,
                  res_aq$n_countries,     res_cor$n_countries),
  coef_ib     = c(res_margin$coef,   res_loang$coef,
                  res_aq$coef,       res_cor$coef),
  se_ols      = c(res_margin$se_ols, res_loang$se_ols,
                  res_aq$se_ols,     res_cor$se_ols),
  wb_pval     = c(res_margin$bt_pval, res_loang$bt_pval,
                  res_aq$bt_pval,    res_cor$bt_pval),
  wb_ci_low   = c(res_margin$bt_ci_low,  res_loang$bt_ci_low,
                  res_aq$bt_ci_low,     res_cor$bt_ci_low),
  wb_ci_high  = c(res_margin$bt_ci_high, res_loang$bt_ci_high,
                  res_aq$bt_ci_high,    res_cor$bt_ci_high)
) %>%
  mutate(sig_stars = stars_fn(wb_pval))

secondary_summary <- tibble(
  outcome     = c("roa", "cost_to_income"),
  framework   = c("Profitability", "Efficiency"),
  n_obs       = c(res_roa$n_obs,    res_cti$n_obs),
  n_countries = c(res_roa$n_countries, res_cti$n_countries),
  coef_ib     = c(res_roa$coef,     res_cti$coef),
  se_ols      = c(res_roa$se_ols,   res_cti$se_ols),
  wb_pval     = c(res_roa$bt_pval,  res_cti$bt_pval),
  wb_ci_low   = c(res_roa$bt_ci_low,  res_cti$bt_ci_low),
  wb_ci_high  = c(res_roa$bt_ci_high, res_cti$bt_ci_high)
) %>%
  mutate(sig_stars = stars_fn(wb_pval))

feols_summary <- tibble(
  outcome           = c("margin_proxy", "loan_growth",
                        "asset_quality_value", "cost_of_risk"),
  coef_feols        = c(coef(m_margin)["internet_banking"],
                        coef(m_loang)["internet_banking"],
                        coef(m_aq_feols)["internet_banking"],
                        coef(m_cor)["internet_banking"]),
  se_bank_cluster   = c(se(m_margin)["internet_banking"],
                        se(m_loang)["internet_banking"],
                        se(m_aq_feols)["internet_banking"],
                        se(m_cor)["internet_banking"]),
  pval_bank_cluster = c(pvalue(m_margin)["internet_banking"],
                        pvalue(m_loang)["internet_banking"],
                        pvalue(m_aq_feols)["internet_banking"],
                        pvalue(m_cor)["internet_banking"])
)

write_csv(boot_summary,      file.path(root_dir, "wildboot_results_main_outcomes.csv"))
write_csv(secondary_summary, file.path(root_dir, "wildboot_results_secondary_outcomes.csv"))
write_csv(feols_summary,     file.path(root_dir, "feols_results_main_outcomes.csv"))
cat("Bootstrap results saved.\n")

# ── 5d: Print clean summary table ───────────────────────────────────────────

cat("\n")
cat("=======================================================================\n")
cat("  WILD BOOTSTRAP RESULTS — internet_banking coefficient\n")
cat("  Clustered by country (18 clusters), B=9999, Rademacher weights\n")
cat("=======================================================================\n\n")

cat("--- Competition outcomes ---\n")
for (r in list(res_margin, res_loang)) {
  s <- boot_summary[boot_summary$outcome == r$outcome, ]
  cat(sprintf("%-25s  Coef: %7.4f  WB p: %.3f %s  CI: [%.4f, %.4f]  N=%d\n",
              r$outcome, r$coef, r$bt_pval, s$sig_stars,
              r$bt_ci_low, r$bt_ci_high, r$n_obs))
}

cat("\n--- Stability outcomes ---\n")
for (r in list(res_aq, res_cor)) {
  s <- boot_summary[boot_summary$outcome == r$outcome, ]
  cat(sprintf("%-25s  Coef: %7.4f  WB p: %.3f %s  CI: [%.4f, %.4f]  N=%d\n",
              r$outcome, r$coef, r$bt_pval, s$sig_stars,
              r$bt_ci_low, r$bt_ci_high, r$n_obs))
}

cat("\n--- Secondary outcomes (appendix) ---\n")
for (r in list(res_roa, res_cti)) {
  s <- secondary_summary[secondary_summary$outcome == r$outcome, ]
  cat(sprintf("%-25s  Coef: %7.4f  WB p: %.3f %s  CI: [%.4f, %.4f]  N=%d\n",
              r$outcome, r$coef, r$bt_pval, s$sig_stars,
              r$bt_ci_low, r$bt_ci_high, r$n_obs))
}
cat("\n* p<0.10  ** p<0.05  *** p<0.01  (wild bootstrap, country clusters)\n")
cat("=======================================================================\n")

# ── 5e: Publication-style Table 2 ───────────────────────────────────────────

boot_main  <- read_csv(file.path(root_dir, "wildboot_results_main_outcomes.csv"),
                       show_col_types = FALSE)
boot_sec   <- read_csv(file.path(root_dir, "wildboot_results_secondary_outcomes.csv"),
                       show_col_types = FALSE)
feols_main <- read_csv(file.path(root_dir, "feols_results_main_outcomes.csv"),
                       show_col_types = FALSE)

boot_all  <- bind_rows(boot_main, boot_sec)
feols_all <- bind_rows(
  feols_main,
  tibble(
    outcome           = c("roa", "cost_to_income"),
    coef_feols        = c(boot_sec$coef_ib[1], boot_sec$coef_ib[2]),
    se_bank_cluster   = c(boot_sec$se_ols[1],  boot_sec$se_ols[2]),
    pval_bank_cluster = c(NA_real_, NA_real_)
  )
)

order_vec <- c("NII margin", "Loan growth", "Asset quality",
               "Cost of risk", "ROA", "Cost-to-income")

table_flat <- boot_all %>%
  left_join(feols_all, by = "outcome") %>%
  mutate(
    outcome_label = case_when(
      outcome == "margin_proxy"        ~ "NII margin",
      outcome == "loan_growth"         ~ "Loan growth",
      outcome == "asset_quality_value" ~ "Asset quality",
      outcome == "cost_of_risk"        ~ "Cost of risk",
      outcome == "roa"                 ~ "ROA",
      outcome == "cost_to_income"      ~ "Cost-to-income"
    ),
    coef_rounded  = round(coef_feols,       6),
    se_rounded    = round(se_bank_cluster,   6),
    wb_pval_round = round(wb_pval,           3),
    wb_ci_low_r   = round(wb_ci_low,         6),
    wb_ci_high_r  = round(wb_ci_high,        6),
    sig_stars_wb  = stars_fn(wb_pval)
  ) %>%
  select(framework, outcome_label,
         coef_rounded, se_rounded,
         wb_pval_round, sig_stars_wb,
         wb_ci_low_r, wb_ci_high_r,
         n_obs, n_countries)

write_csv(table_flat, file.path(root_dir, "table2_main_results_flat.csv"))

kable_input <- table_flat %>%
  mutate(
    Coefficient        = paste0(formatC(coef_rounded, format = "e", digits = 3),
                                sig_stars_wb),
    `SE (bank clust.)` = formatC(se_rounded,    format = "e", digits = 3),
    `WB p-value`       = formatC(wb_pval_round, format = "f", digits = 3),
    `WB 95% CI`        = paste0("[", formatC(wb_ci_low_r,  format = "e", digits = 2),
                                ", ", formatC(wb_ci_high_r, format = "e", digits = 2), "]"),
    N = n_obs
  ) %>%
  select(Group = framework, Outcome = outcome_label,
         Coefficient, `SE (bank clust.)`, `WB p-value`, `WB 95% CI`, N) %>%
  mutate(Outcome = factor(Outcome, levels = order_vec)) %>%
  arrange(Outcome)

kable_out <- kable_input %>%
  kable(format = "pipe",
        align  = c("l", "l", "r", "r", "r", "r", "r"),
        caption = "Table 2: Effect of digitization on bank outcomes") %>%
  kable_styling(full_width = FALSE) %>%
  pack_rows("Competition outcomes", 1, 2) %>%
  pack_rows("Stability outcomes",   3, 4) %>%
  pack_rows("Secondary outcomes",   5, 6)

print(kable_out)
save_kable(kable_out, file = file.path(root_dir, "table2_main_results.html"))


# ─────────────────────────────────────────────────────────────────────────────
# Section 6: Robustness checks
# ─────────────────────────────────────────────────────────────────────────────

# ── 6a: Digital index swap (replace internet_banking with digital_index_2) ───

run_boot_index <- function(df, yvar, B = 9999) {
  base_vars <- c(yvar, "digital_index_2", "log_assets", "loan_to_deposit",
                 "CET1 ratio", "gdp_growth", "unemployment", "inflation",
                 "policy_rate", "country", "Bank", "year")
  if (yvar != "cost_of_risk") base_vars <- c(base_vars, "cost_of_risk")

  d <- df %>% select(all_of(base_vars)) %>% drop_na()

  rhs <- if (yvar == "cost_of_risk") {
    "digital_index_2 + log_assets + loan_to_deposit + `CET1 ratio` +
     gdp_growth + unemployment + inflation + policy_rate + Bank + factor(year)"
  } else {
    "digital_index_2 + log_assets + loan_to_deposit + cost_of_risk + `CET1 ratio` +
     gdp_growth + unemployment + inflation + policy_rate + Bank + factor(year)"
  }

  m  <- lm(as.formula(paste0("`", yvar, "` ~ ", rhs)), data = d)
  bt <- boottest(m, param = "digital_index_2",
                 clustid = ~country, B = B, type = "rademacher")

  list(outcome     = yvar,
       n_obs       = nrow(d),
       n_countries = length(unique(d$country)),
       coef        = coef(m)["digital_index_2"],
       bt_pval     = bt$p_val,
       bt_ci_low   = bt$conf_int[1],
       bt_ci_high  = bt$conf_int[2])
}

cat("\nRunning index swap: margin_proxy...\n")
idx_margin <- run_boot_index(df, "margin_proxy")
cat("Running index swap: loan_growth...\n")
idx_loang  <- run_boot_index(df, "loan_growth")
cat("Running index swap: asset_quality_value...\n")
idx_aq     <- run_boot_index(df, "asset_quality_value")
cat("Running index swap: cost_of_risk...\n")
idx_cor    <- run_boot_index(df, "cost_of_risk")

cat("\n--- Index swap results ---\n")
cat(sprintf("%-25s  %10s  %8s  %10s  %8s\n",
            "Outcome", "Main coef", "Main WB p", "Index coef", "Index WB p"))
cat(strrep("-", 65), "\n")

outcomes_order <- c("margin_proxy", "loan_growth", "asset_quality_value", "cost_of_risk")
main_coefs     <- c(res_margin$coef,    res_loang$coef,
                    res_aq$coef,        res_cor$coef)
main_pvals     <- c(res_margin$bt_pval, res_loang$bt_pval,
                    res_aq$bt_pval,     res_cor$bt_pval)
idx_results    <- list(idx_margin, idx_loang, idx_aq, idx_cor)

for (i in seq_along(outcomes_order)) {
  cat(sprintf("%-25s  %10.6f  %8.3f  %10.6f  %8.3f\n",
              outcomes_order[i], main_coefs[i], main_pvals[i],
              idx_results[[i]]$coef, idx_results[[i]]$bt_pval))
}

# ── 6b: Pre/post-2020 split (structural break around COVID-19) ───────────────
# Reparameterizes digitization into separate pre- and post-2020 slopes to test
# whether the digitization effect shifted during the pandemic period

df_pp <- df %>%
  mutate(
    post2020 = as.integer(year >= 2020),
    ib_pre   = internet_banking * (1 - post2020),
    ib_post  = internet_banking * post2020
  )

run_prepost <- function(d, yvar, B = 9999) {
  base_ctrl <- c("log_assets", "loan_to_deposit", "CET1 ratio",
                 "gdp_growth", "unemployment", "inflation", "policy_rate")
  if (yvar != "cost_of_risk") base_ctrl <- c(base_ctrl, "cost_of_risk")

  split_vars <- c(yvar, "ib_pre", "ib_post", base_ctrl, "country", "Bank", "year")
  ds <- d %>% select(all_of(split_vars)) %>% drop_na()

  controls_str <- paste(
    c(base_ctrl[base_ctrl != "CET1 ratio"], "`CET1 ratio`"),
    collapse = " + "
  )

  m_split  <- lm(
    as.formula(paste0("`", yvar, "` ~ ib_pre + ib_post + ",
                      controls_str, " + Bank + factor(year)")),
    data = ds
  )
  bt_pre  <- boottest(m_split, param = "ib_pre",
                      clustid = ~country, B = B, type = "rademacher")
  bt_post <- boottest(m_split, param = "ib_post",
                      clustid = ~country, B = B, type = "rademacher")

  int_vars <- c(yvar, "internet_banking", "post2020", base_ctrl,
                "country", "Bank", "year")
  di    <- d %>% select(all_of(int_vars)) %>% drop_na()
  m_int <- lm(
    as.formula(paste0("`", yvar, "` ~ internet_banking * post2020 + ",
                      controls_str, " + Bank + factor(year)")),
    data = di
  )
  bt_inter <- boottest(m_int, param = "internet_banking:post2020",
                       clustid = ~country, B = B, type = "rademacher")

  list(outcome      = yvar,
       coef_pre    = coef(m_split)["ib_pre"],
       pval_pre    = bt_pre$p_val,
       coef_post   = coef(m_split)["ib_post"],
       pval_post   = bt_post$p_val,
       pval_inter  = bt_inter$p_val,
       n_obs       = nrow(ds),
       n_countries = length(unique(ds$country)))
}

cat("\nRunning pre/post 2020: margin_proxy...\n")
pp_margin <- run_prepost(df_pp, "margin_proxy")
cat("Running pre/post 2020: cost_of_risk...\n")
pp_cor    <- run_prepost(df_pp, "cost_of_risk")

# Pre/post 2020 for asset quality — uses the split-slope parameterization
df_aq_int2 <- df_pp %>%
  select(asset_quality_value, asset_quality_type,
         ib_pre, ib_post,
         log_assets, loan_to_deposit, `CET1 ratio`,
         gdp_growth, unemployment, inflation, policy_rate,
         country, Bank, year_fe) %>%
  drop_na()

m_aq_split_lm <- lm(
  asset_quality_value ~ ib_pre + ib_post +
    log_assets + loan_to_deposit + `CET1 ratio` +
    gdp_growth + unemployment + inflation + policy_rate +
    asset_quality_type + Bank + year_fe,
  data = df_aq_int2
)

bt_pre  <- boottest(m_aq_split_lm, param = "ib_pre",
                    clustid = ~country, B = 9999, type = "rademacher")
bt_post <- boottest(m_aq_split_lm, param = "ib_post",
                    clustid = ~country, B = 9999, type = "rademacher")

# Asset quality interaction model (tests whether the slope changed after 2020)
df_aq_int <- df %>%
  mutate(post2020 = as.integer(year >= 2020)) %>%
  select(asset_quality_value, asset_quality_type,
         internet_banking, post2020,
         log_assets, loan_to_deposit, `CET1 ratio`,
         gdp_growth, unemployment, inflation, policy_rate,
         country, Bank, year_fe) %>%
  drop_na()

m_aq_post_lm <- lm(
  asset_quality_value ~ internet_banking * post2020 +
    log_assets + loan_to_deposit + `CET1 ratio` +
    gdp_growth + unemployment + inflation + policy_rate +
    asset_quality_type + Bank + year_fe,
  data = df_aq_int
)

# Verify the interaction term is named correctly before bootstrapping
names(coef(m_aq_post_lm))

# bt_aq_pre tests the main internet_banking coefficient when post2020 = 0
# (pre-2020 digitization effect in the interaction parameterization)
bt_aq_pre <- boottest(m_aq_post_lm, param = "internet_banking",
                      clustid = ~country, B = 9999, type = "rademacher")

# bt_aq_inter tests whether the slope shifted after 2020
bt_aq_inter <- boottest(m_aq_post_lm, param = "internet_banking:post2020",
                        clustid = ~country, B = 9999, type = "rademacher")

cat("\n--- Pre/post 2020 results ---\n")
cat(sprintf("%-22s  %10s  %8s  %10s  %8s  %12s\n",
            "Outcome", "Pre coef", "Pre WB p",
            "Post coef", "Post WB p", "Interaction p"))
cat(strrep("-", 75), "\n")

for (r in list(pp_margin, pp_cor)) {
  cat(sprintf("%-22s  %10.6f  %8.3f%s  %10.6f  %8.3f%s  %12.3f%s\n",
              r$outcome,
              r$coef_pre,  r$pval_pre,  stars_fn(r$pval_pre),
              r$coef_post, r$pval_post, stars_fn(r$pval_post),
              r$pval_inter, stars_fn(r$pval_inter)))
}

cat("\nasset_quality_value:\n")
cat(sprintf("  Pre coef:  %.6f  WB p: %.3f%s\n",
            coef(m_aq_split_lm)["ib_pre"],
            bt_pre$p_val, stars_fn(bt_pre$p_val)))
cat(sprintf("  Post coef: %.6f  WB p: %.3f%s\n",
            coef(m_aq_split_lm)["ib_post"],
            bt_post$p_val, stars_fn(bt_post$p_val)))
cat(sprintf("  Interaction WB p: %.3f%s\n",
            bt_aq_inter$p_val, stars_fn(bt_aq_inter$p_val)))

# Full bootstrap objects (for CI extraction or further inspection)
cat("\nFull bootstrap objects — asset quality pre/post 2020:\n")
bt_aq_pre
bt_aq_inter
bt_pre
bt_post

# Raw pre- and post-2020 coefficients
coef(m_aq_split_lm)["ib_pre"]
coef(m_aq_split_lm)["ib_post"]

# ── 6c: Asset quality subsamples by measurement type ────────────────────────
# Tests whether the headline asset quality result holds within each individual
# NPL definition (NPL, Stage 3, NPE) rather than pooling them with type dummies

run_aq_subsample <- function(type_keep, B = 9999) {
  d <- df %>%
    filter(asset_quality_type == type_keep) %>%
    select(asset_quality_value,
           internet_banking, log_assets, loan_to_deposit, `CET1 ratio`,
           gdp_growth, unemployment, inflation, policy_rate,
           country, Bank, year_fe) %>%
    drop_na()

  m  <- lm(
    asset_quality_value ~ internet_banking + log_assets + loan_to_deposit +
      `CET1 ratio` + gdp_growth + unemployment + inflation + policy_rate +
      Bank + year_fe,
    data = d
  )
  bt <- boottest(m, param = "internet_banking",
                 clustid = ~country, B = B, type = "webb")

  list(type = type_keep, n = nrow(d),
       countries = length(unique(d$country)), bt = bt)
}

res_npl    <- run_aq_subsample("NPL")
res_stage3 <- run_aq_subsample("Stage3")
res_npe    <- run_aq_subsample("NPE")

cat("\nNPL subsample — N:", res_npl$n, "| Countries:", res_npl$countries, "\n")
res_npl$bt
cat("Stage3 subsample — N:", res_stage3$n, "| Countries:", res_stage3$countries, "\n")
res_stage3$bt
cat("NPE subsample — N:", res_npe$n, "| Countries:", res_npe$countries, "\n")
res_npe$bt


# ─────────────────────────────────────────────────────────────────────────────
# Section 7: Mechanism analysis — BIS fintech data
# ─────────────────────────────────────────────────────────────────────────────
# Tests whether higher internet banking adoption at the country level predicts
# greater fintech credit activity (BIS alternative finance data, 2015–2019).
# This provides evidence that the digitization measure captures genuine
# competitive pressure from new entrants, not merely a general income effect.

# ── 7a: Load and clean BIS data ─────────────────────────────────────────────

bis <- read_csv(
  file.path(misc_dir, "BIS data.csv"),
  col_types = cols(.default = col_character()), show_col_types = FALSE
)

bis0 <- bis %>%
  mutate(across(everything(), as.character)) %>%
  select(where(~ !all(is.na(.))))

# The real header row is row 2 in this file
hdr <- bis0[2, ] %>% unlist(use.names = FALSE) %>% str_squish()
hdr[hdr == ""] <- NA_character_
hdr[is.na(hdr)] <- paste0("junk_", which(is.na(hdr)))

bis1 <- bis0[-c(1, 2), ]
names(bis1) <- hdr
bis1 <- bis1 %>%
  clean_names() %>%
  select(where(~ !all(is.na(.))))

bis_clean_raw <- bis1 %>%
  mutate(
    year                 = as.integer(year),
    iso2                 = iso2,
    country              = country_name,
    bigtech_credit_usd_m = to_num(big_tech_credit_usd_mn),
    fintech_credit_usd_m = to_num(fintech_credit_usd_mn),
    altcredit_usd_m      = to_num(total_alternative_credit_usd_mn),
    bigtech_credit_pc_usd = to_num(big_tech_credit_per_capita_usd),
    fintech_credit_pc_usd = to_num(fintech_credit_per_capita_usd),
    domestic_credit_usd_m = to_num(total_domestic_credit_by_financial_sector_usd_mn)
  ) %>%
  select(year, iso2, country,
         bigtech_credit_usd_m, fintech_credit_usd_m, altcredit_usd_m,
         bigtech_credit_pc_usd, fintech_credit_pc_usd,
         domestic_credit_usd_m) %>%
  filter(!is.na(year), !is.na(iso2), !is.na(country)) %>%
  arrange(country, year) %>%
  # Fintech shares are valid only through 2018 — the 2019 denominator is missing
  mutate(
    fintech_share   = if_else(year <= 2018 & !is.na(domestic_credit_usd_m) &
                                domestic_credit_usd_m > 0,
                              fintech_credit_usd_m / domestic_credit_usd_m, NA_real_),
    bigtech_share   = if_else(year <= 2018 & !is.na(domestic_credit_usd_m) &
                                domestic_credit_usd_m > 0,
                              bigtech_credit_usd_m / domestic_credit_usd_m, NA_real_),
    altcredit_share = if_else(year <= 2018 & !is.na(domestic_credit_usd_m) &
                                domestic_credit_usd_m > 0,
                              altcredit_usd_m / domestic_credit_usd_m, NA_real_)
  )

write_csv(bis_clean_raw, file.path(root_dir, "bis_fintech_bigtech_clean.csv"))

# ── 7b: Translate Eurostat country names to ISO2 codes for BIS merge ─────────
# Eurostat stores countries as full names; BIS uses ISO2 codes

name_to_iso2 <- tribble(
  ~geo_name,                ~iso2,
  "Austria",                "AT",  "Belgium",   "BE",  "Bulgaria",  "BG",
  "Croatia",                "HR",  "Cyprus",    "CY",  "Czechia",   "CZ",
  "Czech Republic",         "CZ",  "Denmark",   "DK",  "Estonia",   "EE",
  "Finland",                "FI",  "France",    "FR",  "Germany",   "DE",
  "Greece",                 "EL",  "Hungary",   "HU",  "Iceland",   "IS",
  "Ireland",                "IE",  "Italy",     "IT",  "Latvia",    "LV",
  "Liechtenstein",          "LI",  "Lithuania", "LT",  "Luxembourg","LU",
  "Malta",                  "MT",  "Netherlands","NL", "Norway",    "NO",
  "Poland",                 "PL",  "Portugal",  "PT",  "Romania",   "RO",
  "Slovakia",               "SK",  "Slovenia",  "SI",  "Spain",     "ES",
  "Sweden",                 "SE",  "Switzerland","CH", "Türkiye",   "TR",
  "Turkey",                 "TR",  "United Kingdom","GB","Albania",  "AL",
  "Bosnia and Herzegovina", "BA",  "Kosovo*",   "XK",  "Montenegro","ME",
  "North Macedonia",        "MK",  "Serbia",    "RS"
)

internet_banking_raw <- read_eurostat_csv(
  file.path(misc_dir, "Individuals using the internet for internet banking.csv"),
  "internet_banking"
) %>%
  filter(year >= 2015, year <= 2019)

internet_banking_iso2 <- internet_banking_raw %>%
  filter(!geo %in% c(":", "b", "e", "Observation flags:", "Special value")) %>%
  filter(!str_detect(geo, "^[a-z]$")) %>%
  left_join(name_to_iso2, by = c("geo" = "geo_name")) %>%
  # Eurostat uses "EL" for Greece; BIS uses "GR"
  mutate(iso2 = case_when(
    geo == "EL"  ~ "GR",
    !is.na(iso2) ~ iso2,
    TRUE         ~ NA_character_
  )) %>%
  filter(!is.na(iso2)) %>%
  select(iso2, year, internet_banking)

# ── 7c: Check overlap and merge ─────────────────────────────────────────────

bis_clean <- read_csv(file.path(root_dir, "bis_fintech_bigtech_clean.csv"),
                      col_types = cols(.default = col_guess())) %>%
  mutate(year = as.integer(year)) %>%
  filter(iso2 != "TOT")

bis_codes  <- bis_clean %>%
  filter(year >= 2015, year <= 2019) %>%
  distinct(iso2) %>% pull(iso2) %>% sort()
euro_codes <- internet_banking_iso2 %>%
  distinct(iso2) %>% pull(iso2) %>% sort()
overlap    <- intersect(bis_codes, euro_codes)

cat("Countries in BIS not in Eurostat (excluded):\n")
print(setdiff(bis_codes, euro_codes))
cat("\nCountries that will merge (", length(overlap), "):\n")
bis_clean %>%
  filter(iso2 %in% overlap) %>%
  distinct(iso2, country) %>%
  arrange(iso2) %>%
  print(n = Inf)

bis_europe <- bis_clean %>%
  filter(iso2 %in% overlap, year >= 2015, year <= 2019)

bis_merged <- bis_europe %>%
  left_join(internet_banking_iso2, by = c("iso2", "year"))

bis_model <- bis_merged %>%
  filter(!is.na(internet_banking)) %>%
  mutate(
    # log(1+x) handles zero-fintech-credit countries and compresses the UK outlier
    log_fintech_pc  = log(1 + coalesce(fintech_credit_pc_usd, 0)),
    log_fintech_vol = log(1 + coalesce(fintech_credit_usd_m,  0)),
    iso2_fe         = factor(iso2),
    year_fe         = factor(year)
  )

cat("\nMechanism dataset: N =", nrow(bis_model),
    "| Countries:", n_distinct(bis_model$iso2), "\n\n")

# ── 7d: BIS mechanism regressions ───────────────────────────────────────────

cat("=== BIS mechanism regressions ===\n\n")

# Model 1: log fintech per capita with country + year FE (within-country variation)
m_bis_pc <- feols(
  log_fintech_pc ~ internet_banking | iso2_fe + year_fe,
  data = bis_model, cluster = ~iso2
)

# Model 2: log fintech volume (robustness to the per-capita scaling)
m_bis_vol <- feols(
  log_fintech_vol ~ internet_banking | iso2_fe + year_fe,
  data = bis_model, cluster = ~iso2
)

# Model 3: fintech credit share of total domestic credit (2015–2018 only)
m_bis_share <- feols(
  fintech_share ~ internet_banking | iso2_fe + year_fe,
  data = bis_model %>% filter(!is.na(fintech_share)),
  cluster = ~iso2
)

# Model 4: year FE only — uses cross-country variation to test whether more
# digitized countries also host more fintech activity
m_bis_cross <- feols(
  log_fintech_pc ~ internet_banking | year_fe,
  data = bis_model, cluster = ~iso2
)

etable(
  m_bis_pc, m_bis_vol, m_bis_share, m_bis_cross,
  headers = c("Log fintech PC\n(ctry+yr FE)",
              "Log fintech vol\n(ctry+yr FE)",
              "Fintech share\n(2015-18)",
              "Log fintech PC\n(yr FE only)"),
  digits = 4
)

# Wild bootstrap for Model 4 (cross-country spec, where inference is sharpest)
bis_m4_lm <- lm(
  log_fintech_pc ~ internet_banking + factor(year_fe),
  data = bis_model
)

bt_bis_m4 <- boottest(
  bis_m4_lm, param = "internet_banking",
  clustid = ~iso2, B = 9999, type = "rademacher"
)

cat("\n--- BIS Model 4 wild bootstrap ---\n")
cat("Coefficient:     ", round(coef(bis_m4_lm)["internet_banking"], 5), "\n")
cat("Bootstrap p:     ", round(bt_bis_m4$p_val, 4), "\n")
cat("Bootstrap 95 CI: ", round(bt_bis_m4$conf_int, 5), "\n")
cat("N countries:     ", length(unique(bis_model$iso2)), "\n")
cat("N obs:           ", nobs(bis_m4_lm), "\n")

bis_results <- tibble(
  model   = c("Log fintech PC (ctry+yr FE)", "Log fintech vol (ctry+yr FE)",
              "Fintech share (2015-18)",      "Log fintech PC (yr FE only)"),
  n_obs   = c(nobs(m_bis_pc), nobs(m_bis_vol),
              nobs(m_bis_share), nobs(m_bis_cross)),
  coef_ib = c(coef(m_bis_pc)["internet_banking"],
              coef(m_bis_vol)["internet_banking"],
              coef(m_bis_share)["internet_banking"],
              coef(m_bis_cross)["internet_banking"]),
  se      = c(se(m_bis_pc)["internet_banking"],
              se(m_bis_vol)["internet_banking"],
              se(m_bis_share)["internet_banking"],
              se(m_bis_cross)["internet_banking"]),
  pval    = c(pvalue(m_bis_pc)["internet_banking"],
              pvalue(m_bis_vol)["internet_banking"],
              pvalue(m_bis_share)["internet_banking"],
              pvalue(m_bis_cross)["internet_banking"])
) %>%
  mutate(stars = stars_fn(pval))

print(bis_results)
write_csv(bis_model,   file.path(root_dir, "bis_model_data.csv"))
write_csv(bis_results, file.path(root_dir, "bis_mechanism_results.csv"))


# ─────────────────────────────────────────────────────────────────────────────
# Section 8: Figure generation
# ─────────────────────────────────────────────────────────────────────────────

# The 18 countries represented in the sample (exactly as they appear in Eurostat)
bank_countries_18 <- c(
  "Austria", "Belgium", "Cyprus", "Denmark", "Finland", "France",
  "Germany", "Greece", "Hungary", "Ireland", "Italy", "Netherlands",
  "Norway", "Poland", "Portugal", "Romania", "Spain", "Sweden"
)

# Colorblind-friendly palette (Okabe-Ito) for group-level distinctions
okabe_ito <- c(
  "1. Competition" = "#E69F00",
  "2. Stability"   = "#0072B2",
  "3. Secondary"   = "#999999"
)

# ── Figure 1: Internet banking adoption trends across Europe, 2015–2024 ──────
# Highlights the 18 sample countries; all others shown in grey for context

cat("Building Figure 1...\n")

ib_full <- read_eurostat_clean(
  file.path(misc_dir,
            "Individuals using the internet for internet banking.csv"),
  "internet_banking"
) %>%
  filter(year >= 2015, year <= 2024, !is.na(internet_banking))

ib_plot <- ib_full %>%
  mutate(highlight = country_name %in% bank_countries_18)

# ISO2 labels for end-of-line annotation (avoids legend clutter)
short_label <- c(
  "Austria" = "AT", "Belgium" = "BE", "Cyprus" = "CY", "Denmark" = "DK",
  "Finland" = "FI", "France" = "FR", "Germany" = "DE", "Greece" = "GR",
  "Hungary" = "HU", "Ireland" = "IE", "Italy" = "IT", "Netherlands" = "NL",
  "Norway" = "NO", "Poland" = "PL", "Portugal" = "PT", "Romania" = "RO",
  "Spain" = "ES", "Sweden" = "SE"
)

labels_fig1 <- ib_plot %>%
  filter(highlight) %>%
  group_by(country_name) %>%
  filter(year == max(year[!is.na(internet_banking)])) %>%
  ungroup() %>%
  mutate(lbl = short_label[country_name])

fig1 <- ggplot() +
  geom_line(
    data    = ib_plot %>% filter(!highlight),
    aes(x = year, y = internet_banking, group = country_name),
    colour  = "grey80", linewidth = 0.35, alpha = 0.7
  ) +
  geom_line(
    data      = ib_plot %>% filter(highlight),
    aes(x     = year, y = internet_banking,
        group = country_name, colour = country_name),
    linewidth = 0.85
  ) +
  geom_text(
    data  = labels_fig1,
    aes(x = year + 0.15, y = internet_banking,
        label = lbl, colour = country_name),
    size  = 2.6, hjust = 0, show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = 2015:2024,
    limits = c(2015, 2026),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_colour_viridis_d(option = "turbo", begin = 0.1, end = 0.9) +
  labs(
    title    = "Internet banking adoption across European countries, 2015–2024",
    subtitle = paste0("Share of individuals using internet banking (%).",
                      " Coloured lines: 18 sample-bank countries."),
    x        = NULL,
    y        = "Internet banking adoption (%)",
    caption  = "Source: Eurostat [tin00099]. Grey lines: other European countries."
  ) +
  thesis_theme +
  theme(
    legend.position = "none",
    plot.margin     = margin(8, 20, 8, 8)
  )

ggsave(file.path(fig_dir, "fig1_digitization_trends.png"),
       fig1, width = 9, height = 5.5, dpi = 300)
cat("  Saved fig1_digitization_trends.png\n")

# ── Figure 2: Standardized coefficient plot across all outcomes ──────────────
# Raw coefficients are not comparable across outcomes measured in different
# units. Multiplying each by SD(internet_banking)/SD(outcome) gives a
# "beta coefficient" — how many SDs the outcome moves per 1 SD in adoption

cat("Building Figure 2...\n")

df_fig <- read_csv(file.path(root_dir, "panel3_with_macros_and_policy.csv"),
                   col_types = cols(.default = col_guess())) %>%
  mutate(year = as.integer(year))

sd_ib <- sd(df_fig$internet_banking, na.rm = TRUE)

outcome_sds <- tibble(
  outcome    = c("margin_proxy", "loan_growth", "asset_quality_value",
                 "cost_of_risk", "roa", "cost_to_income"),
  sd_outcome = c(
    sd(df_fig$margin_proxy,        na.rm = TRUE),
    sd(df_fig$loan_growth,         na.rm = TRUE),
    sd(df_fig$asset_quality_value, na.rm = TRUE),
    sd(df_fig$cost_of_risk,        na.rm = TRUE),
    sd(df_fig$roa,                 na.rm = TRUE),
    sd(df_fig$cost_to_income,      na.rm = TRUE)
  )
)

feols_sec <- secondary_summary %>%
  transmute(outcome,
            coef_feols        = coef_ib,
            se_bank_cluster   = se_ols,
            pval_bank_cluster = wb_pval)

coef_data <- bind_rows(boot_main, boot_sec) %>%
  left_join(bind_rows(feols_summary, feols_sec), by = "outcome") %>%
  left_join(outcome_sds, by = "outcome") %>%
  mutate(
    std_factor  = sd_ib / sd_outcome,
    coef_std    = coalesce(coef_feols, coef_ib) * std_factor,
    ci_low_std  = wb_ci_low  * std_factor,
    ci_high_std = wb_ci_high * std_factor,
    significant = wb_pval < 0.10,
    outcome_label = case_when(
      outcome == "margin_proxy"        ~ "NII margin",
      outcome == "loan_growth"         ~ "Loan growth",
      outcome == "asset_quality_value" ~ "Asset quality",
      outcome == "cost_of_risk"        ~ "Cost of risk",
      outcome == "roa"                 ~ "ROA",
      outcome == "cost_to_income"      ~ "Cost-to-income"
    ),
    group = case_when(
      outcome %in% c("margin_proxy", "loan_growth")         ~ "1. Competition",
      outcome %in% c("asset_quality_value", "cost_of_risk") ~ "2. Stability",
      TRUE                                                   ~ "3. Secondary"
    ),
    plot_order = case_when(
      outcome == "margin_proxy"        ~ 6,
      outcome == "loan_growth"         ~ 5,
      outcome == "asset_quality_value" ~ 4,
      outcome == "cost_of_risk"        ~ 3,
      outcome == "roa"                 ~ 2,
      outcome == "cost_to_income"      ~ 1
    )
  ) %>%
  mutate(
    outcome_label = fct_reorder(outcome_label, plot_order),
    group         = factor(group, levels = c("1. Competition",
                                             "2. Stability",
                                             "3. Secondary"))
  )

fig2_std <- ggplot(coef_data,
                   aes(x      = coef_std,
                       y      = outcome_label,
                       colour = group,
                       alpha  = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "grey40", linewidth = 0.5) +
  geom_errorbarh(
    aes(xmin = ci_low_std, xmax = ci_high_std),
    height = 0.25, linewidth = 0.75
  ) +
  geom_point(size = 3.5) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.4), guide = "none") +
  scale_colour_manual(
    values = okabe_ito,
    labels = c("Competition", "Stability", "Secondary")
  ) +
  facet_grid(
    group ~ .,
    scales   = "free_y",
    space    = "free_y",
    labeller = labeller(group = c(
      "1. Competition" = "Competition",
      "2. Stability"   = "Stability",
      "3. Secondary"   = "Secondary"
    ))
  ) +
  labs(
    title    = "Effect of digitization on bank outcomes",
    subtitle = paste0(
      "Standardized coefficients: effect of a 1 SD increase in internet banking\n",
      "adoption, expressed in SD units of each outcome. Whiskers: wild bootstrap 95% CI.\n",
      "Faded estimates: wild bootstrap p > 0.10."
    ),
    x       = "Standardized coefficient (SD units)",
    y       = NULL,
    caption = paste0(
      "Bank and year fixed effects. Controls: log assets, loan-to-deposit, CET1 ratio,\n",
      "GDP growth, unemployment, inflation, policy rate. ",
      "Wild bootstrap clustered by country (B = 9,999).\n",
      "Standardization: coefficient × SD(internet banking) / SD(outcome)."
    )
  ) +
  thesis_theme +
  theme(
    legend.position = "none",
    plot.margin     = margin(8, 16, 8, 8)
  )

ggsave(file.path(fig_dir, "fig2_coefficient_plot_standardized.png"),
       fig2_std, width = 8, height = 6, dpi = 300)
cat("  Saved fig2_coefficient_plot_standardized.png\n")

# ── Figure 3: Binned scatter — digitization vs asset quality (residualized) ──
# Residualizes both variables on bank FE, year FE, and controls to isolate the
# within-bank conditional relationship; bins into deciles to reduce noise

cat("Building Figure 3...\n")

df_fig3 <- read_csv(file.path(root_dir, "panel3_with_macros_and_policy.csv"),
                    col_types = cols(.default = col_guess())) %>%
  mutate(year = as.integer(year),
         asset_quality_type = as.factor(asset_quality_type))

resid_data <- df_fig3 %>%
  select(Bank, year, internet_banking, asset_quality_value,
         asset_quality_type, log_assets, loan_to_deposit,
         `CET1 ratio`, gdp_growth, unemployment, inflation, policy_rate) %>%
  drop_na() %>%
  mutate(row_id = row_number())

m_ib <- feols(
  internet_banking ~ log_assets + loan_to_deposit + `CET1 ratio` +
    gdp_growth + unemployment + inflation + policy_rate | Bank + year,
  data = resid_data
)

m_aq_r <- feols(
  asset_quality_value ~ log_assets + loan_to_deposit + `CET1 ratio` +
    gdp_growth + unemployment + inflation + policy_rate +
    asset_quality_type | Bank + year,
  data = resid_data
)

# Safe residual assignment: obs() returns the exact row indices feols used,
# preventing the index-mismatch bug that arises from internal row dropping
resid_data$ib_resid <- NA_real_
resid_data$aq_resid <- NA_real_
resid_data$ib_resid[obs(m_ib)]   <- resid(m_ib)
resid_data$aq_resid[obs(m_aq_r)] <- resid(m_aq_r)

resid_plot <- resid_data %>%
  filter(!is.na(ib_resid), !is.na(aq_resid))

cat("Rows with both residuals:", nrow(resid_plot), "\n")
check_slope <- lm(aq_resid ~ ib_resid, data = resid_plot)
cat("Slope check (should be ~-0.528):", round(coef(check_slope)[2], 3), "\n\n")

n_bins <- 10
resid_binned <- resid_plot %>%
  mutate(
    ib_bin = cut(
      ib_resid,
      breaks         = quantile(ib_resid,
                                probs = seq(0, 1, length.out = n_bins + 1),
                                na.rm = TRUE),
      include.lowest = TRUE,
      labels         = FALSE
    )
  ) %>%
  group_by(ib_bin) %>%
  summarise(
    ib_mean = mean(ib_resid, na.rm = TRUE),
    aq_mean = mean(aq_resid, na.rm = TRUE),
    n       = n(),
    .groups = "drop"
  )

fig3 <- ggplot(resid_binned, aes(x = ib_mean, y = aq_mean)) +
  geom_smooth(
    method    = "lm", formula = y ~ x,
    colour    = "#0072B2", fill = "#AED6F1",
    linewidth = 0.8, alpha = 0.25
  ) +
  geom_point(aes(size = n), colour = "#0072B2", alpha = 0.85) +
  scale_size_continuous(range = c(2, 6), guide = "none") +
  geom_hline(yintercept = 0, linetype = "dotted",
             colour = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dotted",
             colour = "grey50", linewidth = 0.4) +
  labs(
    title    = "Digitization and bank asset quality (conditional relationship)",
    subtitle = paste0(
      "Binned scatter after partialling out bank FE, year FE, and controls.\n",
      "Each point = one bin of bank-year observations. ",
      "Point size proportional to bin size."
    ),
    x       = "Internet banking adoption (residualized, pp)",
    y       = "Asset quality ratio (residualized, pp)",
    caption = paste0(
      "Lower y-axis values indicate better asset quality (lower NPL/impairment ratio).\n",
      "Slope corresponds to the regression coefficient of −0.528 ",
      "(wild bootstrap p = 0.003)."
    )
  ) +
  thesis_theme +
  theme(plot.margin = margin(8, 16, 8, 8))

ggsave(file.path(fig_dir, "fig3_binned_scatter_aq.png"),
       fig3, width = 7.5, height = 5.5, dpi = 300)
cat("  Saved fig3_binned_scatter_aq.png\n")

# ── Figure 4: BIS scatter — country-average digitization vs fintech credit ───
# Macro-level cross-country scatter for the mechanism section; 4 of the 18
# sample countries are absent from the BIS data (Cyprus, Greece, Hungary,
# Romania) and are noted in the figure caption

cat("Building Figure 4...\n")

bis_raw <- read_csv(
  file.path(misc_dir, "BIS data.csv"),
  skip = 2, col_types = cols(.default = col_character()),
  show_col_types = FALSE
) %>%
  rename(year = 1, iso2 = 2, country_name = 3) %>%
  mutate(
    year          = as.integer(str_squish(year)),
    country_name  = str_squish(country_name),
    country_lower = str_to_lower(country_name)
  ) %>%
  filter(!is.na(year), country_name != "Country name",
         country_name != "Total", country_name != "") %>%
  filter(year >= 2015, year <= 2019)

ft_pc_col <- names(bis_raw)[str_detect(
  str_to_lower(names(bis_raw)), "fintech.*per capita|per capita.*fintech"
)][1]

bis_clean_fig4 <- bis_raw %>%
  mutate(fintech_pc = to_num(.data[[ft_pc_col]])) %>%
  select(year, country_name, country_lower, fintech_pc)

ib_data <- read_eurostat_clean(
  file.path(misc_dir,
            "Individuals using the internet for internet banking.csv"),
  "internet_banking"
) %>%
  filter(year >= 2015, year <= 2019, !is.na(internet_banking))

bis_merged_fig4 <- bis_clean_fig4 %>%
  left_join(ib_data %>% select(country_lower, year, internet_banking),
            by = c("country_lower", "year")) %>%
  filter(!is.na(internet_banking), !is.na(fintech_pc))

iso2_lookup <- c(
  "austria" = "AT", "belgium" = "BE", "denmark" = "DK", "finland" = "FI",
  "france" = "FR",  "germany" = "DE", "ireland" = "IE", "italy" = "IT",
  "netherlands" = "NL", "norway" = "NO", "poland" = "PL", "portugal" = "PT",
  "spain" = "ES",   "sweden" = "SE",  "united kingdom" = "GB", "estonia" = "EE",
  "latvia" = "LV",  "lithuania" = "LT", "switzerland" = "CH", "turkey" = "TR"
)

bis_scatter <- bis_merged_fig4 %>%
  group_by(country_name, country_lower) %>%
  summarise(
    avg_ib    = mean(internet_banking, na.rm = TRUE),
    avg_ft_pc = mean(fintech_pc,       na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(
    is_bank_country = str_to_title(country_lower) %in% bank_countries_18,
    lbl = case_when(
      is_bank_country ~ coalesce(iso2_lookup[country_lower],
                                 str_sub(country_name, 1, 2)),
      TRUE            ~ NA_character_
    )
  )

cat("  Countries in scatter:", nrow(bis_scatter), "\n")
cat("  Bank countries present:", sum(bis_scatter$is_bank_country), "of 18\n")

fig4 <- ggplot(bis_scatter, aes(x = avg_ib, y = avg_ft_pc)) +
  geom_smooth(method = "lm", formula = y ~ x,
              colour = "#E69F00", fill = "#FAD7A0",
              linewidth = 0.8, alpha = 0.25) +
  geom_point(
    data   = bis_scatter %>% filter(!is_bank_country),
    colour = "grey70", size = 1.8, alpha = 0.7
  ) +
  geom_point(
    data   = bis_scatter %>% filter(is_bank_country),
    colour = "#E69F00", size = 3, alpha = 0.9
  ) +
  geom_text(
    data    = bis_scatter %>% filter(is_bank_country, !is.na(lbl)),
    aes(label = lbl),
    size    = 2.8, colour = "grey25",
    nudge_y = max(bis_scatter$avg_ft_pc, na.rm = TRUE) * 0.03,
    nudge_x = 0.5
  ) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0("$", round(x, 0))) +
  labs(
    title    = "Digitization and fintech credit penetration across Europe",
    subtitle = paste0("Country averages, 2015–2019. ",
                      "Orange: 14 sample-bank countries present in BIS data. ",
                      "Grey: other BIS countries."),
    x        = "Internet banking adoption (%, average 2015–2019)",
    y        = "Fintech credit per capita (USD, average 2015–2019)",
    caption  = paste0(
      "Source: Eurostat [tin00099]; BIS (Cornelli et al., 2020).\n",
      "Fitted line from OLS with year fixed effects. ",
      "Cyprus, Greece, Hungary and Romania absent from BIS data."
    )
  ) +
  thesis_theme +
  theme(plot.margin = margin(8, 16, 8, 8))

ggsave(file.path(fig_dir, "fig4_bis_scatter.png"),
       fig4, width = 7.5, height = 5.5, dpi = 300)
cat("  Saved fig4_bis_scatter.png\n")

cat("\nAll figures saved to:", fig_dir, "\n")

fig1
fig2_std
fig3
fig4

#############################################
#  MULTI-COUNTRY FISCAL-RATES ANALYSIS
#  Replicate US analysis for: Germany, France, UK
#############################################

#############
# SETUP
#############
# SELECT COUNTRY: "us", "germany", "france", "uk"
COUNTRY <- "france"  # Country-specific script

# Create paths
DATA_DIR   <- file.path("data", tolower(COUNTRY))
OUT_DIR    <- file.path("output", tolower(COUNTRY))

# Create output directory if it doesn't exist
if (!dir.exists(OUT_DIR)) {
  dir.create(OUT_DIR, recursive = TRUE)
}

cat(sprintf("\n========== ANALYSIS FOR %s ==========\n", toupper(COUNTRY)))
cat(sprintf("Data directory: %s\n", DATA_DIR))
cat(sprintf("Output directory: %s\n\n", OUT_DIR))

#############
# LIBRARIES
#############
library(readr)
library(readxl)
library(data.table)
library(dplyr)
library(lubridate)
library(sandwich)
library(lmtest)
library(ggplot2)
library(patchwork)
library(scales)

#############
# SANITY HELPERS
#############
print_rows <- function(name, dt) {
  cat(sprintf("[sanity] %-24s rows=%d cols=%d\n", name, nrow(dt), ncol(dt)))
}

print_date_na <- function(name, dt) {
  na_n <- sum(is.na(dt$DATE))
  cat(sprintf("[sanity] %-24s DATE_NA=%d/%d\n", name, na_n, nrow(dt)))
}

print_complete_cases <- function(name, dt, cols) {
  use_cols <- cols[cols %in% names(dt)]
  cc_n <- sum(complete.cases(dt[, ..use_cols]))
  cat(sprintf("[sanity] %-24s complete_cases=%d/%d (vars=%d)\n", name, cc_n, nrow(dt), length(use_cols)))
}

read_csv_auto <- function(path, required = TRUE) {
  if (!file.exists(path)) {
    msg <- sprintf("Missing file: %s", path)
    if (required) stop(msg)
    cat(sprintf("Warning: %s\n", msg))
    return(NULL)
  }
  first_line <- readLines(path, n = 1, warn = FALSE)
  delim <- if (grepl(";", first_line) && !grepl(",", first_line)) ";" else ","
  dt <- suppressMessages(read_delim(path, delim = delim, show_col_types = FALSE, trim_ws = TRUE, progress = FALSE))
  dt <- as.data.table(dt)
  drop_idx <- which((startsWith(names(dt), "...") | names(dt) == "") & sapply(dt, function(x) all(is.na(x))))
  if (length(drop_idx) > 0) dt[, (drop_idx) := NULL]
  dt
}

pick_col <- function(dt, candidates) {
  cols <- names(dt)
  hit <- candidates[candidates %in% cols]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

require_col <- function(dt, candidates, label) {
  col <- pick_col(dt, candidates)
  if (is.na(col)) {
    stop(sprintf("%s is missing required column. Tried: %s", label, paste(candidates, collapse = ", ")))
  }
  col
}

parse_date_flex <- function(x) {
  as.Date(parse_date_time(as.character(x), orders = c("Y-m-d", "d/m/Y", "m/d/Y", "d-b-Y", "d-b-y", "Y/m/d"), quiet = TRUE))
}

num_vec <- function(x) suppressWarnings(as.numeric(x))

#############
# DATA UPLOAD
#############
cat("Loading data...\n")

# Fiscal variables
debt_5y_with_gdp    <- read_csv_auto(file.path(DATA_DIR, "independent_variables", "debt_5y_with_gdp.csv"))
deficit_5y_with_gdp <- read_csv_auto(file.path(DATA_DIR, "independent_variables", "deficit_5y_with_gdp.csv"))

# Interest rate variables
gsw_forward_rates <- read_csv(file.path(DATA_DIR, "dependent_variables", "gsw_forward_rates.csv"))
long_rate         <- read_csv(file.path(DATA_DIR, "dependent_variables", "long_rate.csv"))

# Control variables
EBP          <- read_csv_auto(file.path(DATA_DIR, "control_variables", "EBP.csv"), required = FALSE)
TBILL        <- read_csv_auto(file.path(DATA_DIR, "control_variables", "ST_rate.csv"), required = FALSE)
UNPOP        <- read_csv_auto(file.path(DATA_DIR, "control_variables", "UNPOP.csv"), required = FALSE)
REC          <- read_csv_auto(file.path(DATA_DIR, "control_variables", "REC.csv"), required = FALSE)
FDHBFIN      <- read_csv_auto(file.path(DATA_DIR, "control_variables", "FDHBFIN.csv"), required = FALSE)
GFDEBTN      <- read_csv_auto(file.path(DATA_DIR, "control_variables", "GFDEBTN.csv"), required = FALSE)

cat("Data loaded successfully.\n\n")
cat("[sanity] Loaded dataset sizes:\n")
print_rows("debt_5y_with_gdp", debt_5y_with_gdp)
print_rows("deficit_5y_with_gdp", deficit_5y_with_gdp)
print_rows("gsw_forward_rates", gsw_forward_rates)
print_rows("long_rate", long_rate)
if (!is.null(TBILL)) print_rows("TBILL", TBILL)
if (!is.null(UNPOP)) print_rows("UNPOP", UNPOP)
if (!is.null(REC)) print_rows("REC", REC)
if (!is.null(FDHBFIN)) print_rows("FDHBFIN", FDHBFIN)
if (!is.null(GFDEBTN)) print_rows("GFDEBTN", GFDEBTN)
if (!is.null(EBP)) print_rows("EBP", EBP)
cat("\n")

#############
# PREP DATES
#############
cat("Preparing dates...\n")

setDT(debt_5y_with_gdp)
setDT(deficit_5y_with_gdp)
setDT(gsw_forward_rates)
setDT(long_rate)
if (!is.null(TBILL)) setDT(TBILL)
if (!is.null(UNPOP)) setDT(UNPOP)
if (!is.null(REC)) setDT(REC)
if (!is.null(FDHBFIN)) setDT(FDHBFIN)
if (!is.null(GFDEBTN)) setDT(GFDEBTN)

if (!is.null(EBP)) setDT(EBP)

# Map heterogeneous fiscal column names
debt_date_col  <- require_col(debt_5y_with_gdp, c("baseline_date", "observation_date", "DATE", "date"), "debt_5y_with_gdp")
def_date_col   <- require_col(deficit_5y_with_gdp, c("baseline_date", "observation_date", "DATE", "date"), "deficit_5y_with_gdp")
debt_value_col <- require_col(debt_5y_with_gdp, c("debt_pct_GDP", "value"), "debt_5y_with_gdp")
def_value_col  <- require_col(deficit_5y_with_gdp, c("deficit_pct_GDP", "value"), "deficit_5y_with_gdp")

# Parse dates (supports old country files and new UK-style fiscal updates)
debt_5y_with_gdp[,    DATE := parse_date_flex(get(debt_date_col))]
deficit_5y_with_gdp[, DATE := parse_date_flex(get(def_date_col))]
gsw_forward_rates[,   DATE := as.Date(observation_date)]
long_rate[,           DATE := as.Date(observation_date)]
tbill_date_col <- if (!is.null(TBILL)) pick_col(TBILL, c("observation_date", "DATE", "date")) else NA_character_
tbill_val_col  <- if (!is.null(TBILL)) pick_col(TBILL, c("TB3MS", "value")) else NA_character_
unpop_year_col <- if (!is.null(UNPOP)) pick_col(UNPOP, c("Time", "year", "YEAR")) else NA_character_
unpop_val_col  <- if (!is.null(UNPOP)) pick_col(UNPOP, c("Value", "value", "UNPOP")) else NA_character_
rec_date_col   <- if (!is.null(REC)) pick_col(REC, c("observation_date", "DATE", "date")) else NA_character_
rec_val_col    <- if (!is.null(REC)) pick_col(REC, c("USREC", "REC", "value")) else NA_character_
fdh_date_col   <- if (!is.null(FDHBFIN)) pick_col(FDHBFIN, c("observation_date", "DATE", "date")) else NA_character_
fdh_val_col    <- if (!is.null(FDHBFIN)) pick_col(FDHBFIN, c("FDHBFIN", "value")) else NA_character_
gfd_date_col   <- if (!is.null(GFDEBTN)) pick_col(GFDEBTN, c("observation_date", "DATE", "date")) else NA_character_
gfd_val_col    <- if (!is.null(GFDEBTN)) pick_col(GFDEBTN, c("GFDEBTN", "value")) else NA_character_
ebp_date_col   <- if (!is.null(EBP)) pick_col(EBP, c("date", "DATE", "observation_date")) else NA_character_
ebp_val_col    <- if (!is.null(EBP)) pick_col(EBP, c("ebp", "value")) else NA_character_

if (!is.null(TBILL) && !is.na(tbill_date_col)) TBILL[, DATE := parse_date_flex(get(tbill_date_col))]
if (!is.null(UNPOP) && !is.na(unpop_year_col)) UNPOP[, DATE := as.Date(paste0(get(unpop_year_col), "-01-01"))]
if (!is.null(REC) && !is.na(rec_date_col)) REC[, DATE := parse_date_flex(get(rec_date_col))]

if (!is.null(EBP) && !is.na(ebp_date_col)) EBP[, DATE := parse_date_flex(get(ebp_date_col))]
if (!is.null(FDHBFIN) && !is.na(fdh_date_col)) FDHBFIN[, DATE := parse_date_flex(get(fdh_date_col))]
if (!is.null(GFDEBTN) && !is.na(gfd_date_col)) GFDEBTN[, DATE := parse_date_flex(get(gfd_date_col))]

# Population growth: annual data
if (!is.null(UNPOP) && !is.na(unpop_year_col) && !is.na(unpop_val_col)) {
  unpop <- UNPOP[, .(year = as.integer(get(unpop_year_col)), pop_growth = num_vec(get(unpop_val_col)))]
} else {
  unpop <- data.table(year = integer(), pop_growth = numeric())
}

cat("Dates prepared.\n\n")
cat("[sanity] DATE parse checks:\n")
print_date_na("debt_5y_with_gdp", debt_5y_with_gdp)
print_date_na("deficit_5y_with_gdp", deficit_5y_with_gdp)
print_date_na("gsw_forward_rates", gsw_forward_rates)
print_date_na("long_rate", long_rate)
if (!is.null(TBILL) && "DATE" %in% names(TBILL)) print_date_na("TBILL", TBILL)
if (!is.null(REC) && "DATE" %in% names(REC)) print_date_na("REC", REC)
if (!is.null(FDHBFIN) && "DATE" %in% names(FDHBFIN)) print_date_na("FDHBFIN", FDHBFIN)
if (!is.null(GFDEBTN) && "DATE" %in% names(GFDEBTN)) print_date_na("GFDEBTN", GFDEBTN)
if (!is.null(EBP)) print_date_na("EBP", EBP)
cat("\n")

##################
# BUILD CONTROLS
##################
cat("Building control variables...\n")

# 1. Short-term rate (country-specific)
rates <- long_rate[, .(DATE, rate_long = num_vec(value))]
base_dates <- rates$DATE

strate <- if (!is.null(TBILL) && !is.na(tbill_val_col) && "DATE" %in% names(TBILL)) {
  TBILL[, .(DATE, st_rate = num_vec(get(tbill_val_col)))]
} else {
  data.table(DATE = base_dates, st_rate = NA_real_)
}

# 2. Risk indicator (EBP or similar)
risk_ind <- if (!is.null(EBP) && !is.na(ebp_val_col) && "DATE" %in% names(EBP)) {
  EBP[, .(DATE, risk_ind = num_vec(get(ebp_val_col)))]
} else {
  data.table(DATE = base_dates, risk_ind = NA_real_)
}

# 3. Recession dummy
rec <- if (!is.null(REC) && !is.na(rec_val_col) && "DATE" %in% names(REC)) {
  REC[, .(DATE, recession = num_vec(get(rec_val_col)))]
} else {
  data.table(DATE = base_dates, recession = NA_real_)
}

# 4. Foreign holdings share (if available)
# foreign holdings / total debt * 100
if (!is.null(FDHBFIN) && !is.null(GFDEBTN) && !is.na(fdh_val_col) && !is.na(gfd_val_col) &&
    "DATE" %in% names(FDHBFIN) && "DATE" %in% names(GFDEBTN)) {
  foreign_hold <- FDHBFIN[, .(DATE, fdh = num_vec(get(fdh_val_col)))][GFDEBTN[, .(DATE, gfd = num_vec(get(gfd_val_col)))], on = "DATE", nomatch = 0]
  foreign_hold[, foreign_holdings := (fdh / (gfd / 1000)) * 100]
  foreign_hold <- foreign_hold[, .(DATE, foreign_holdings)]
} else {
  foreign_hold <- data.table(DATE = base_dates, foreign_holdings = NA_real_)
}

# 5. Population growth lookup
pop_lookup <- function(cbo_years) {
  sapply(cbo_years, function(y) {
    val <- unpop[year == y, pop_growth]
    if(length(val) == 0) NA_real_ else val[1]
  })
}

# Forward rates if available
if ("fwd_5_10y" %in% names(gsw_forward_rates)) {
  fwd <- gsw_forward_rates[, .(DATE, fwd_5_10y, fwd_10_15y)]
} else {
  fwd <- NULL
}

cat("Control variables built.\n\n")
cat("[sanity] Control series checks:\n")
print_rows("strate", strate)
print_rows("risk_ind", risk_ind)
print_rows("rec", rec)
print_rows("foreign_hold", foreign_hold)
print_rows("rates", rates)
if (!is.null(fwd)) print_rows("fwd", fwd)
cat(sprintf("[sanity] foreign_holdings NA=%d/%d\n", sum(is.na(foreign_hold$foreign_holdings)), nrow(foreign_hold)))
cat(sprintf("[sanity] risk_ind NA=%d/%d\n\n", sum(is.na(risk_ind$risk_ind)), nrow(risk_ind)))

#################
# MERGE FUNCTION
#################
join_nearest <- function(base, rate_dt, cols) {
  rate_sub <- rate_dt[, c("DATE", cols), with = FALSE]
  rate_sub <- rate_sub[!is.na(get(cols[1]))]
  if (nrow(rate_sub) == 0) {
    out <- copy(base)
    for (cc in cols) out[, (cc) := NA_real_]
    return(out)
  }
  rate_sub[base, on = "DATE", roll = "nearest"]
}

merge_all <- function(cbo_dt) {
  base <- data.table(DATE = cbo_dt$DATE)
  
  # Join rate_long
  out <- join_nearest(base, rates, "rate_long")
  
  # Join forward rates if available
  if (!is.null(fwd)) {
    out <- join_nearest(out, fwd, c("fwd_5_10y", "fwd_10_15y"))
  }
  
  # Join control variables
  out <- join_nearest(out, strate,    "st_rate")
  out <- join_nearest(out, risk_ind,  "risk_ind")
  out <- join_nearest(out, rec,       "recession")
  out <- join_nearest(out, foreign_hold, "foreign_holdings")
  
  # Population growth only; fiscal inputs are already expressed in % of GDP.
  out[, pop_growth := pop_lookup(as.integer(format(DATE, "%Y")))]
  
  out
}

################
# BUILD REG DFS
################
cat("Building regression datasets...\n")

add_trend <- function(dt) {
  dt[, t  := as.numeric(format(DATE, "%Y")) - 1976]
  dt[, t2 := t^2]
  dt
}

# --- DEFICIT dataset ---
df_deficit <- merge_all(deficit_5y_with_gdp)
df_deficit[, fiscal_balance := num_vec(deficit_5y_with_gdp[match(df_deficit$DATE, deficit_5y_with_gdp$DATE), get(def_value_col)])]
add_trend(df_deficit)

# --- DEBT dataset ---
df_debt <- merge_all(debt_5y_with_gdp)
df_debt[, debt := num_vec(debt_5y_with_gdp[match(df_debt$DATE, debt_5y_with_gdp$DATE), get(debt_value_col)])]
add_trend(df_debt)

cat("Regression datasets built.\n\n")
cat("[sanity] Regression DF checks:\n")
print_rows("df_deficit", df_deficit)
print_rows("df_debt", df_debt)
print_complete_cases("df_deficit M4 vars", df_deficit,
                     c("rate_long", "fwd_5_10y", "fwd_10_15y",
                       "fiscal_balance", "t", "st_rate", "foreign_holdings",
                       "risk_ind", "recession", "pop_growth"))
print_complete_cases("df_debt M4 vars", df_debt,
                     c("rate_long", "fwd_5_10y", "fwd_10_15y",
                       "debt", "t", "st_rate", "foreign_holdings",
                       "risk_ind", "recession", "pop_growth"))
cat("\n")

# --- Sanity check ---
cat("\n--- Fiscal input sanity check ---\n")
cat("Fiscal inputs are treated as already expressed in % of GDP.\n")
cat("deficit summary:\n"); print(summary(df_deficit$fiscal_balance))
cat("debt summary:\n");    print(summary(df_debt$debt))

sanity_dt <- rbind(
  df_deficit[, .(DATE, value = fiscal_balance, dataset = "Deficit (% GDP)")],
  df_debt[,    .(DATE, value = debt, dataset = "Debt (% GDP)")]
)

p_sanity <- ggplot(sanity_dt, aes(x = DATE, y = value, color = dataset)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = c("Deficit (% GDP)" = "#c0392b", "Debt (% GDP)" = "#1f3a5f")) +
  labs(title = sprintf("Fiscal input sanity check: %s", toupper(COUNTRY)),
       x = NULL, y = "% of GDP", color = NULL) +
  theme_bw(base_size = 10)

ggsave(file.path(OUT_DIR, "sanity_fiscal_inputs.png"), p_sanity,
       width = 8, height = 3.5, dpi = 150, bg = "white")
cat(sprintf("Sanity plot saved: %s\n", file.path(OUT_DIR, "sanity_fiscal_inputs.png")))

##############
# REGRESSION
##############
cat("\n========== REGRESSION ANALYSIS ==========\n")

# Determine which y variables are available; order: fwd rates → long rate
y_vars <- c()
if ("fwd_5_10y" %in% names(df_deficit)) y_vars <- c(y_vars, "fwd_5_10y", "fwd_10_15y")
y_vars <- c(y_vars, "rate_long")

y_labels <- c(
  fwd_5_10y  = "Fwd:5-10y",
  fwd_10_15y = "Fwd:10-15y",
  rate_long  = "LongRate"
)

run_reg_full <- function(df, y_var, x_var, controls) {
  ctrl <- controls

  if (nchar(ctrl) > 0) {
    requested <- trimws(unlist(strsplit(ctrl, "\\+")))
    requested <- requested[nchar(requested) > 0]
    available <- requested[requested %in% names(df)]
    usable <- available[sapply(available, function(v) {
      vv <- df[[v]]
      sum(!is.na(vv)) >= 5 && length(unique(vv[!is.na(vv)])) > 1
    })]
    ctrl <- paste(usable, collapse = " + ")
  }
  
  fml <- as.formula(paste(y_var, "~", x_var,
                          if(ctrl != "") paste("+", ctrl) else ""))
  fit <- tryCatch(
    lm(fml, data = df, na.action = na.omit),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(c(coef = NA_real_, se = NA_real_, pval = NA_real_, r2 = NA_real_, dw = NA_real_))
  }
  n   <- nobs(fit)
  
  # Skip if no valid observations
  if (n == 0) {
    return(c(coef = NA_real_, se = NA_real_, pval = NA_real_, r2 = NA_real_, dw = NA_real_))
  }
  
  lag <- floor(4 * (n/100)^(2/9))
  ct  <- coeftest(fit, vcov = NeweyWest(fit, lag = lag, prewhite = FALSE))
  dw  <- unname(dwtest(fit)$statistic)
  
  c(coef = ct[x_var, "Estimate"],
    se   = ct[x_var, "Std. Error"],
    pval = ct[x_var, "Pr(>|t|)"],
    r2   = summary(fit)$r.squared,
    dw   = dw)
}

print_block_full <- function(res, label) {
  # Handle case where res contains NAs (failed regression)
  if (all(is.na(res["coef",]))) {
    cat(sprintf("%-20s", label), "All NAs (no valid observations)\n")
    return(invisible(NULL))
  }
  
  cat(sprintf("%-20s", label),
      paste(sprintf("%12s", sprintf("%.3f%s", res["coef",],
                                    ifelse(res["pval",] < 0.01, "***", ifelse(res["pval",] < 0.05, "**",
                                                                               ifelse(res["pval",] < 0.10, "*", ""))))), collapse = ""), "\n")
  cat(sprintf("%-20s", ""),
      paste(sprintf("%12s", sprintf("(%.3f)", res["se",])), collapse = ""), "\n")
  cat(sprintf("%-20s", "  R²"),
      paste(sprintf("%12s", sprintf("%.3f", res["r2",])), collapse = ""), "\n")
  cat(sprintf("%-20s", "  DW"),
      paste(sprintf("%12s", sprintf("%.3f", res["dw",])), collapse = ""), "\n")
}

# Sequential specifications (all ordering combinations)
specs_sequential <- list(
  # Ordering 1: st_rate -> trend -> controls
  "O1.M1 baseline"           = "",
  "O1.M2 +strate"            = "st_rate",
  "O1.M3 +strate+trend"      = "st_rate + t",
  "O1.M4 +strate+trend+ctrl" = "st_rate + t + foreign_holdings + risk_ind + recession + pop_growth",
  
  # Ordering 2: trend -> st_rate -> controls
  "O2.M2 +trend"             = "t",
  "O2.M3 +trend+strate"      = "t + st_rate",
  "O2.M4 +trend+strate+ctrl" = "t + st_rate + foreign_holdings + risk_ind + recession + pop_growth",
  
  # Ordering 3: controls -> st_rate -> trend
  "O3.M2 +ctrl"              = "foreign_holdings + risk_ind + recession + pop_growth",
  "O3.M3 +ctrl+strate"       = "foreign_holdings + risk_ind + recession + pop_growth + st_rate",
  "O3.M4 +ctrl+strate+trend" = "foreign_holdings + risk_ind + recession + pop_growth + st_rate + t",
  
  # Ordering 4: trend -> controls -> st_rate
  "O4.M2 +trend"             = "t",
  "O4.M3 +trend+ctrl"        = "t + foreign_holdings + risk_ind + recession + pop_growth",
  "O4.M4 +trend+ctrl+strate" = "t + foreign_holdings + risk_ind + recession + pop_growth + st_rate",
  
  # Ordering 5: st_rate -> controls -> trend
  "O5.M2 +strate"            = "st_rate",
  "O5.M3 +strate+ctrl"       = "st_rate + foreign_holdings + risk_ind + recession + pop_growth",
  "O5.M4 +strate+ctrl+trend" = "st_rate + foreign_holdings + risk_ind + recession + pop_growth + t",
  
  # Ordering 6: controls -> trend -> st_rate
  "O6.M2 +ctrl"              = "foreign_holdings + risk_ind + recession + pop_growth",
  "O6.M3 +ctrl+trend"        = "foreign_holdings + risk_ind + recession + pop_growth + t",
  "O6.M4 +ctrl+trend+strate" = "foreign_holdings + risk_ind + recession + pop_growth + t + st_rate"
)

cat("\n========== SEQUENTIAL REGRESSION ANALYSIS ==========")
for(spec_name in names(specs_sequential)) {
  controls <- specs_sequential[[spec_name]]
  cat("\n\n========== MODEL:", spec_name, "==========\n")
  cat(sprintf("%-20s", ""), paste(sprintf("%12s", y_labels[y_vars]), collapse = ""), "\n")
  
  debt_res <- sapply(y_vars, function(y) run_reg_full(df_debt,    y, "debt",           controls))
  def_res  <- sapply(y_vars, function(y) run_reg_full(df_deficit, y, "fiscal_balance", controls))
  
  print_block_full(debt_res, "Debt")
  print_block_full(def_res,  "Fiscal Balance")
}

# Main specifications
specs_quad <- list(
  "M1: baseline"          = "",
  "M2: +trend"            = "t",
  "M3: +trend+strate"     = "t + st_rate",
  "M4: +all controls"     = "t + st_rate + risk_ind + recession + pop_growth + foreign_holdings",
  "M5: +quadratic trend"  = "t + t2 + st_rate + risk_ind + recession + pop_growth + foreign_holdings"
)

for(spec_name in names(specs_quad)) {
  controls <- specs_quad[[spec_name]]
  cat("\n========== MODEL:", spec_name, "==========\n")
  cat(sprintf("%-20s", ""), paste(sprintf("%12s", y_labels[y_vars]), collapse = ""), "\n")
  
  debt_res <- sapply(y_vars, function(y) run_reg_full(df_debt,    y, "debt",           controls))
  def_res  <- sapply(y_vars, function(y) run_reg_full(df_deficit, y, "fiscal_balance", controls))
  
  print_block_full(debt_res, "Debt")
  print_block_full(def_res,  "Fiscal Balance")
}

cat("\n========== ANALYSIS COMPLETE ==========\n")
cat(sprintf("Results saved to: %s\n", OUT_DIR))

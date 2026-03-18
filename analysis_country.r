#############################################
#  MULTI-COUNTRY FISCAL-RATES ANALYSIS
#  Replicate US analysis for: Germany, France, UK
#############################################

#############
# SETUP
#############
# SELECT COUNTRY: "us", "germany", "france", "uk"
COUNTRY <- "us"  # ← MODIFY THIS FOR EACH RUN

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

#############
# DATA UPLOAD
#############
cat("Loading data...\n")

# Fiscal variables
debt_5y_with_gdp    <- read_csv(file.path(DATA_DIR, "independent_variables", "debt_5y_with_gdp.csv"))
deficit_5y_with_gdp <- read_csv(file.path(DATA_DIR, "independent_variables", "deficit_5y_with_gdp.csv"))

# Interest rate variables
gsw_forward_rates <- read_csv(file.path(DATA_DIR, "dependent_variables", "gsw_forward_rates.csv"))
long_rate         <- read_csv(file.path(DATA_DIR, "dependent_variables", "long_rate.csv"))
ACMTermPremium    <- tryCatch(
  read_excel(file.path(DATA_DIR, "dependent_variables", "ACMTermPremium.xls")),
  error = function(e) {
    cat("Warning: ACMTermPremium not found or not applicable.\n")
    NULL
  }
)

# Control variables
EBP          <- tryCatch(read_csv(file.path(DATA_DIR, "control_variables", "EBP.csv")), error = function(e) NULL)
EXPINF5YR    <- read_csv(file.path(DATA_DIR, "control_variables", "EXPINF5YR.csv"))
TBILL        <- read_csv(file.path(DATA_DIR, "control_variables", "ST_rate.csv"))
UNPOP        <- read_csv(file.path(DATA_DIR, "control_variables", "UNPOP.csv"))
REC          <- read_csv(file.path(DATA_DIR, "control_variables", "REC.csv"))
FDHBFIN      <- read_csv(file.path(DATA_DIR, "control_variables", "FDHBFIN.csv"))
GFDEBTN      <- read_csv(file.path(DATA_DIR, "control_variables", "GFDEBTN.csv"))
GDP_ACTUAL   <- read_csv(file.path(DATA_DIR, "control_variables", "GDP.csv"))

cat("Data loaded successfully.\n\n")
cat("[sanity] Loaded dataset sizes:\n")
print_rows("debt_5y_with_gdp", debt_5y_with_gdp)
print_rows("deficit_5y_with_gdp", deficit_5y_with_gdp)
print_rows("gsw_forward_rates", gsw_forward_rates)
print_rows("long_rate", long_rate)
print_rows("EXPINF5YR", EXPINF5YR)
print_rows("TBILL", TBILL)
print_rows("UNPOP", UNPOP)
print_rows("REC", REC)
print_rows("FDHBFIN", FDHBFIN)
print_rows("GFDEBTN", GFDEBTN)
print_rows("GDP_ACTUAL", GDP_ACTUAL)
if (!is.null(ACMTermPremium)) print_rows("ACMTermPremium", ACMTermPremium)
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
setDT(EXPINF5YR)
setDT(TBILL)
setDT(UNPOP)
setDT(REC)
setDT(FDHBFIN)
setDT(GFDEBTN)
setDT(GDP_ACTUAL)

if (!is.null(ACMTermPremium)) setDT(ACMTermPremium)
if (!is.null(EBP)) setDT(EBP)

# Parse dates (adjust format strings as needed for each country)
debt_5y_with_gdp[,    DATE := as.Date(baseline_date, format = "%d/%m/%Y")]
deficit_5y_with_gdp[, DATE := as.Date(baseline_date, format = "%Y-%m-%d")]
gsw_forward_rates[,   DATE := as.Date(observation_date)]
long_rate[,           DATE := as.Date(observation_date)]
EXPINF5YR[,           DATE := as.Date(observation_date)]
TBILL[,               DATE := as.Date(observation_date)]
UNPOP[,               DATE := as.Date(as.character(Time), format = "%Y")]
REC[,                 DATE := as.Date(observation_date)]
GDP_ACTUAL[,          DATE := as.Date(observation_date)]

if (!is.null(ACMTermPremium)) ACMTermPremium[, DATE := as.Date(DATE, format = "%d-%b-%Y")]
if (!is.null(EBP)) EBP[, DATE := as.Date(date)]
FDHBFIN[, DATE := as.Date(observation_date)]
GFDEBTN[, DATE := as.Date(observation_date)]

# Population growth: annual data
unpop <- UNPOP[, .(year = as.integer(Time), pop_growth = Value)]

cat("Dates prepared.\n\n")
cat("[sanity] DATE parse checks:\n")
print_date_na("debt_5y_with_gdp", debt_5y_with_gdp)
print_date_na("deficit_5y_with_gdp", deficit_5y_with_gdp)
print_date_na("gsw_forward_rates", gsw_forward_rates)
print_date_na("long_rate", long_rate)
print_date_na("EXPINF5YR", EXPINF5YR)
print_date_na("TBILL", TBILL)
print_date_na("REC", REC)
print_date_na("FDHBFIN", FDHBFIN)
print_date_na("GFDEBTN", GFDEBTN)
print_date_na("GDP_ACTUAL", GDP_ACTUAL)
if (!is.null(ACMTermPremium)) print_date_na("ACMTermPremium", ACMTermPremium)
if (!is.null(EBP)) print_date_na("EBP", EBP)
cat("\n")

##################
# BUILD CONTROLS
##################
cat("Building control variables...\n")

# 1. Short-term rate (country-specific)
strate <- TBILL[, .(DATE, st_rate = TB3MS)]

# 2. Risk indicator (EBP or similar)
risk_ind <- if (!is.null(EBP)) {
  EBP[, .(DATE, risk_ind = ebp)]
} else {
  data.table(DATE = strate$DATE, risk_ind = NA_real_)
}

# 3. Recession dummy
rec <- REC[, .(DATE, recession = USREC)]

# 4. Foreign holdings share (if available)
# foreign holdings / total debt * 100
foreign_hold <- FDHBFIN[GFDEBTN, on = "DATE", nomatch = 0]
foreign_hold[, foreign_holdings := (FDHBFIN / (GFDEBTN / 1000)) * 100]
foreign_hold <- foreign_hold[, .(DATE, foreign_holdings)]

# 5. Annual actual GDP
GDP_ACTUAL[, fyear := as.integer(format(DATE, "%Y"))]
gdp_annual <- GDP_ACTUAL[, .(actual_gdp = mean(GDP, na.rm = TRUE)), by = .(year = fyear)]
setkey(gdp_annual, year)

# 6. Population growth lookup
pop_lookup <- function(cbo_years) {
  sapply(cbo_years, function(y) {
    val <- unpop[year == y, pop_growth]
    if(length(val) == 0) NA_real_ else val[1]
  })
}

# 7. Inflation lookup: find nearest inflation expectation to CBO release date
inf_lookup <- function(dates) {
  sapply(dates, function(d) {
    idx <- which.min(abs(EXPINF5YR$DATE - d))
    EXPINF5YR$EXPINF5YR[idx]
  })
}

# Rate series for dependent variables
rates <- long_rate[, .(DATE, rate_long = DGS10)]

if (!is.null(ACMTermPremium)) {
  ACMTermPremium[, DATE := as.Date(DATE, format = "%d-%b-%Y")]
  acm_tp <- ACMTermPremium[, .(DATE, tprem5 = ACMTP05, tprem10 = ACMTP10)]
  rates <- rates[acm_tp, on = "DATE", roll = "nearest"]
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
print_rows("gdp_annual", gdp_annual)
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
  rate_sub[base, on = "DATE", roll = "nearest"]
}

merge_all <- function(cbo_dt) {
  base <- data.table(DATE = cbo_dt$DATE)
  
  # Join rate_long
  out <- join_nearest(base, rates, "rate_long")
  
  # Join tprem if available
  if ("tprem10" %in% names(rates)) {
    out <- join_nearest(out, rates[, .(DATE, tprem5, tprem10)], c("tprem5", "tprem10"))
  }
  
  # Join forward rates if available
  if (!is.null(fwd)) {
    out <- join_nearest(out, fwd, c("fwd_5_10y", "fwd_10_15y"))
  }
  
  # Join control variables
  out <- join_nearest(out, strate,    "st_rate")
  out <- join_nearest(out, risk_ind,  "risk_ind")
  out <- join_nearest(out, rec,       "recession")
  out <- join_nearest(out, foreign_hold, "foreign_holdings")
  
  # Population and GDP growth
  out[, pop_growth := pop_lookup(as.integer(format(DATE, "%Y")))]
  out[, proj_gdp   := cbo_dt$GDP]
  out[, inflation  := inf_lookup(DATE)]
  
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
df_deficit[, fiscal_balance := deficit_5y_with_gdp[match(df_deficit$DATE, deficit_5y_with_gdp$DATE), deficit_pct_GDP]]

df_deficit[, release_year := as.integer(format(DATE, "%Y"))]
df_deficit[gdp_annual, on = .(release_year = year), actual_gdp := i.actual_gdp]
if (anyNA(df_deficit$actual_gdp))
  warning(sprintf("%s deficit: %d CBO release year(s) unmatched in GDP",
                  toupper(COUNTRY), sum(is.na(df_deficit$actual_gdp))))
df_deficit[, gdp_growth := ((proj_gdp / actual_gdp)^(0.2) - 1) * 100 - inflation]
df_deficit[, c("release_year", "actual_gdp", "proj_gdp", "inflation") := NULL]
add_trend(df_deficit)

# --- DEBT dataset ---
df_debt <- merge_all(debt_5y_with_gdp)
df_debt[, debt := debt_5y_with_gdp[match(df_debt$DATE, debt_5y_with_gdp$DATE), debt_pct_GDP]]

df_debt[, release_year := as.integer(format(DATE, "%Y"))]
df_debt[gdp_annual, on = .(release_year = year), actual_gdp := i.actual_gdp]
if (anyNA(df_debt$actual_gdp))
  warning(sprintf("%s debt: %d CBO release year(s) unmatched in GDP",
                  toupper(COUNTRY), sum(is.na(df_debt$actual_gdp))))
df_debt[, gdp_growth := ((proj_gdp / actual_gdp)^(0.2) - 1) * 100 - inflation]
df_debt[, c("release_year", "actual_gdp", "proj_gdp", "inflation") := NULL]
add_trend(df_debt)

cat("Regression datasets built.\n\n")
cat("[sanity] Regression DF checks:\n")
print_rows("df_deficit", df_deficit)
print_rows("df_debt", df_debt)
print_complete_cases("df_deficit M4 vars", df_deficit,
                     c("rate_long", "fwd_5_10y", "fwd_10_15y", "tprem5", "tprem10",
                       "fiscal_balance", "t", "st_rate", "gdp_growth", "foreign_holdings",
                       "risk_ind", "recession", "pop_growth"))
print_complete_cases("df_debt M4 vars", df_debt,
                     c("rate_long", "fwd_5_10y", "fwd_10_15y", "tprem5", "tprem10",
                       "debt", "t", "st_rate", "gdp_growth", "foreign_holdings",
                       "risk_ind", "recession", "pop_growth"))
cat("\n")

# --- Sanity check ---
cat("\n--- GDP growth sanity check ---\n")
cat("df_deficit:\n"); print(summary(df_deficit$gdp_growth))
cat("df_debt:\n");    print(summary(df_debt$gdp_growth))

sanity_dt <- rbind(
  df_deficit[, .(DATE, gdp_growth, dataset = "Deficit")],
  df_debt[,    .(DATE, gdp_growth, dataset = "Debt")]
)

p_sanity <- ggplot(sanity_dt, aes(x = DATE, y = gdp_growth, color = dataset)) +
  geom_line(linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("Deficit" = "#c0392b", "Debt" = "#1f3a5f")) +
  labs(title = sprintf("GDP growth sanity check: %s", toupper(COUNTRY)),
       x = NULL, y = "% per year", color = NULL) +
  theme_bw(base_size = 10)

ggsave(file.path(OUT_DIR, "sanity_gdp_growth.png"), p_sanity,
       width = 8, height = 3.5, dpi = 150, bg = "white")
cat(sprintf("Sanity plot saved: %s\n", file.path(OUT_DIR, "sanity_gdp_growth.png")))

##############
# REGRESSION
##############
cat("\n========== REGRESSION ANALYSIS ==========\n")

# Determine which y variables are available; order: fwd rates → long rate → term premia
y_vars <- c()
if ("fwd_5_10y" %in% names(df_deficit)) y_vars <- c(y_vars, "fwd_5_10y", "fwd_10_15y")
y_vars <- c(y_vars, "rate_long")
if ("tprem10" %in% names(df_deficit)) y_vars <- c(y_vars, "tprem5", "tprem10")

y_labels <- c(
  fwd_5_10y  = "Fwd:5-10y",
  fwd_10_15y = "Fwd:10-15y",
  rate_long  = "LongRate",
  tprem5     = "TPrem:5y",
  tprem10    = "TPrem:10y"
)

run_reg_full <- function(df, y_var, x_var, controls) {
  ctrl <- controls
  
  fml <- as.formula(paste(y_var, "~", x_var,
                          if(ctrl != "") paste("+", ctrl) else ""))
  fit <- lm(fml, data = df, na.action = na.omit)
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
  "O1.M4 +strate+trend+ctrl" = "st_rate + t + gdp_growth + foreign_holdings + risk_ind + recession + pop_growth",
  
  # Ordering 2: trend -> st_rate -> controls
  "O2.M2 +trend"             = "t",
  "O2.M3 +trend+strate"      = "t + st_rate",
  "O2.M4 +trend+strate+ctrl" = "t + st_rate + gdp_growth + foreign_holdings + risk_ind + recession + pop_growth",
  
  # Ordering 3: controls -> st_rate -> trend
  "O3.M2 +ctrl"              = "gdp_growth + foreign_holdings + risk_ind + recession + pop_growth",
  "O3.M3 +ctrl+strate"       = "gdp_growth + foreign_holdings + risk_ind + recession + pop_growth + st_rate",
  "O3.M4 +ctrl+strate+trend" = "gdp_growth + foreign_holdings + risk_ind + recession + pop_growth + st_rate + t",
  
  # Ordering 4: trend -> controls -> st_rate
  "O4.M2 +trend"             = "t",
  "O4.M3 +trend+ctrl"        = "t + gdp_growth + foreign_holdings + risk_ind + recession + pop_growth",
  "O4.M4 +trend+ctrl+strate" = "t + gdp_growth + foreign_holdings + risk_ind + recession + pop_growth + st_rate",
  
  # Ordering 5: st_rate -> controls -> trend
  "O5.M2 +strate"            = "st_rate",
  "O5.M3 +strate+ctrl"       = "st_rate + gdp_growth + foreign_holdings + risk_ind + recession + pop_growth",
  "O5.M4 +strate+ctrl+trend" = "st_rate + gdp_growth + foreign_holdings + risk_ind + recession + pop_growth + t",
  
  # Ordering 6: controls -> trend -> st_rate
  "O6.M2 +ctrl"              = "gdp_growth + foreign_holdings + risk_ind + recession + pop_growth",
  "O6.M3 +ctrl+trend"        = "gdp_growth + foreign_holdings + risk_ind + recession + pop_growth + t",
  "O6.M4 +ctrl+trend+strate" = "gdp_growth + foreign_holdings + risk_ind + recession + pop_growth + t + st_rate"
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
  "M4: +all controls"     = "t + st_rate + gdp_growth + risk_ind + recession + pop_growth",
  "M5: +quadratic trend"  = "t + t2 + st_rate + gdp_growth + risk_ind + recession + pop_growth"
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

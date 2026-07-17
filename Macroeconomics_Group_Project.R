# Set working directory and call libraries
#----------------------------------------------------------------------------

# Please set working directory to respective folder with all requisite datasets
# on user's computer


setwd("C:/Users/sohom/OneDrive/Desktop/Courses/Macroeconometrics/Macroeconometrics Project/Macroeconometrics Project Data")

# Call necessary libraries

library(readxl)
library(openxlsx)
library(tseries)
library(dplyr)
library(lubridate)
library(vars)

# ── Read "Sheet 3" from the new dataset ──────────────────────────────────────
# Row 11 = date headers; Row 17 = "Euro area - 19 countries (2015-2022)" IP index values
# Values are in even columns (2, 4, 6, ...); odd columns are flags

s3 <- read_excel("sts_inpr_m__custom_21392270_spreadsheet.xlsx",
                 sheet     = "Sheet 3",
                 col_names = FALSE)

date_row <- as.character(s3[11, ])
ip_row   <- as.numeric(s3[17, ])

# Even columns hold the numeric values
value_cols <- seq(2, ncol(s3), by = 2)
dates_all  <- date_row[value_cols]
ip_all     <- ip_row[value_cols]

# Include 2014 (needed as the 12-month lag for 2015 YoY)
mask      <- dates_all >= "2014-01" & dates_all <= "2022-12"
dates_sub <- dates_all[mask]
ip_sub    <- ip_all[mask]

# ── Year-on-year growth: 100 × (ln(IP_t) − ln(IP_{t−12})) ───────────────────
# Indices 1-12 = 2014 (used only as lags); indices 13-108 = 2015-01 to 2022-12
yoy_vals <- rep(NA_real_, length(ip_sub))
for (t in 13:length(ip_sub)) {
  if (!is.na(ip_sub[t]) && !is.na(ip_sub[t - 12]) &&
      ip_sub[t] > 0 && ip_sub[t - 12] > 0) {
    yoy_vals[t] <- 100 * (log(ip_sub[t]) - log(ip_sub[t - 12]))
  }
}

# ── Store results as a data frame (2015-03 to 2022-12 only) ──────────────────
yoy <- data.frame(
  date           = dates_sub[13:length(dates_sub)],
  ip             = ip_sub[13:length(ip_sub)],
  yoy_growth_pct = round(yoy_vals[13:length(yoy_vals)], 3)
)
yoy <- yoy[yoy$date >= "2015-03", ]

# ── Read APP breakdown: Public sector purchase programme (2015-03 to 2022-12) ──
# Column F = "Public sector purchase programme" under "Monthly net purchases at book value"
app_raw <- read.csv(
  "APP_breakdown_history.csv",
  skip = 3,
  header = FALSE,
  stringsAsFactors = FALSE
)

# Forward-fill the year column (only populated for first month of each year)
year_val <- NA_character_
for (i in seq_len(nrow(app_raw))) {
  if (nzchar(app_raw$V1[i])) {
    year_val <- app_raw$V1[i]
  } else {
    app_raw$V1[i] <- year_val
  }
}

# Remove footer/empty rows and build date strings
app_raw <- app_raw[nzchar(app_raw$V2) & !is.na(app_raw$V2), ]
app_raw$month_num <- match(app_raw$V2, month.name)
app_raw$date      <- sprintf("%s-%02d", app_raw$V1, app_raw$month_num)

# Store 2015-03 to 2022-12 as psp
mask_psp <- app_raw$date >= "2015-03" & app_raw$date <= "2022-12"
psp <- data.frame(
  date = app_raw$date[mask_psp],
  pspp = as.numeric(app_raw$V6[mask_psp])
)

# ── Read ECB unemployment rate (age 15–74, Euro area) ────────────────────────
# Column 3 = unemployment rate; column 1 = end-of-month date for filtering
ecb_raw <- read.csv("ECB Data Portal_20260504212455.csv", stringsAsFactors = FALSE)
ecb_raw$ym <- substr(ecb_raw[[1]], 1, 7)  # extract "YYYY-MM"

mask_u <- ecb_raw$ym >= "2015-03" & ecb_raw$ym <= "2022-12"
unemployment <- data.frame(
  date       = ecb_raw$ym[mask_u],
  unemp_rate = as.numeric(ecb_raw[[3]][mask_u])
)

# ── Read HICP inflation: Euro area, Sheet 4 of HICP spreadsheet ──────────────
# Row 9 = date headers; Row 12 = Euro area (EA11-1999 ... EA21-2026)
# Values are in even columns (odd columns are flags)
s4 <- read_excel("prc_hicp_minr__custom_21272002_spreadsheet.xlsx",
                 sheet     = "Sheet 4",
                 col_names = FALSE)

date_row_s4    <- as.character(s4[9, ])
ea_row         <- as.numeric(s4[12, ])
value_cols_s4  <- seq(2, ncol(s4), by = 2)
dates_s4       <- date_row_s4[value_cols_s4]
mask_inf <- !is.na(dates_s4) & dates_s4 >= "2015-03" & dates_s4 <= "2022-12"
inflation <- data.frame(
  date      = dates_s4[mask_inf],
  hicp_rate = ea_row[value_cols_s4][mask_inf]
)


# ── Read ECB deposit facility rate (daily → end-of-month) ────────────────────
ir_raw <- read_excel("ECB Data Portal_20260510213858.xlsx", col_names = FALSE)[[1]]
ir_df  <- read.csv(text = paste(ir_raw, collapse = "\n"), stringsAsFactors = FALSE)

interest_r <- ir_df |>
  mutate(
    date     = as.Date(DATE),
    rate     = Deposit.facility...date.of.changes..raw.data....Level..FM.D.U2.EUR.4F.KR.DFR.LEV.,
    year_mon = format(date, "%Y-%m")
  ) |>
  filter(year_mon >= "2015-03" & year_mon <= "2022-12") |>
  group_by(year_mon) |>
  slice_max(date, n = 1) |>
  ungroup() |>
  dplyr::select(date = year_mon, interest_rate = rate)


# ── First-difference psp, unemployment, and inflation ────────────────────────
psp          <- psp |> mutate(pspp       = pspp       - lag(pspp))       |> filter(!is.na(pspp))
unemployment <- unemployment |> mutate(unemp_rate = unemp_rate - lag(unemp_rate)) |> filter(!is.na(unemp_rate))
inflation    <- inflation |> mutate(hicp_rate  = hicp_rate  - lag(hicp_rate))  |> filter(!is.na(hicp_rate))
interest_r   <- interest_r |> mutate(interest_rate = interest_rate - lag(interest_rate)) |> filter(!is.na(interest_rate))

#Run Non-Stationarity Tests

#ADF Tests:

test.adf.yoy <- adf.test(yoy$yoy_growth_pct)
test.adf.psp <- adf.test(psp$pspp)
test.adf.unemployment <- adf.test(unemployment$unemp_rate)
test.adf.inflation <- adf.test(inflation$hicp_rate)
test.adf.interest_rate <- adf.test(interest_r$interest_rate)

#KPSS Tests:

test.kpss.yoy <- kpss.test(yoy$yoy_growth_pct)
test.kpss.psp <- kpss.test(psp$pspp)
test.kpss.unemployment <- kpss.test(unemployment$unemp_rate)
test.kpss.inflation <- kpss.test(inflation$hicp_rate)
test.kpss.interest_rate <- kpss.test(interest_r$interest_rate)


# Print ADF results
test.adf.yoy
test.adf.psp
test.adf.unemployment
test.adf.inflation
test.adf.interest_rate

# Print KPSS results
test.kpss.yoy
test.kpss.psp
test.kpss.unemployment
test.kpss.inflation
test.kpss.interest_rate

# Plot Month-on-Month Change of Inflation and Interest for visual inspection,
# as they remain non-stationary even after first-differences

png("differenced_inflation.png", width=800, height=600)
plot(inflation$hicp_rate, type="l", 
     main="Differenced Inflation",
     xlab="Months Elapsed Since 2015-04",
     ylab="Month-on-Month Change")
dev.off()

png("differenced_interest_rate.png", width=800, height=600)
plot(interest_r$interest_rate, type="l", 
     main="Differenced Interest Rate",
     xlab="Months Elapsed Since 2015-04",
     ylab="Month-on-Month Change")
dev.off()

# Run Phillips-Perron Test to Confirm non-stationarity

test.pp.inflation <- pp.test(inflation$hicp_rate)
test.pp.inflation

test.pp.interest <- pp.test(interest_r$interest_rate)
test.pp.interest

test.pp.yoy <- pp.test(yoy$yoy_growth_pct)
test.pp.yoy

# Drop First row from Industrial Production "yoy" dF 
# to align with other dFs:

yoy <- yoy[-1, ]

# All should have 93 rows

nrow(yoy)
nrow(psp)
nrow(unemployment)
nrow(inflation)
nrow(interest_r)

var_data <- cbind(
  psp$pspp,
  interest_r$interest_rate,
  yoy$yoy_growth_pct,
  unemployment$unemp_rate,
  inflation$hicp_rate
)

colnames(var_data) <- c("psp", "interest_rate", "yoy", "unemployment", "inflation")

# Combine all variables into single matrix for VAR Estimation:

var_data <- cbind(
  psp$pspp,
  interest_r$interest_rate,
  yoy$yoy_growth_pct,
  unemployment$unemp_rate,
  inflation$hicp_rate
)

colnames(var_data) <- c("psp", "interest_rate", "yoy", "unemployment", "inflation")


# Lag Selection:

lag_selection <- VARselect(var_data, lag.max=12, type="const")
lag_selection$selection
lag_selection$criteria

# Estimate VAR(2) according to information criteria:

estimated.var <- VAR(var_data, p=2, type="const")
roots(estimated.var)

# VAR(2) yields roots greater than 1, so re-estimate with VAR(1)

estimated.var1 <- VAR(var_data, p=1, type="const")
roots(estimated.var1)
summary(estimated.var1)

# Granger Causality Tests:

causality(estimated.var1, cause="psp")
causality(estimated.var1, cause="interest_rate")
causality(estimated.var1, cause="yoy")
causality(estimated.var1, cause="unemployment")
causality(estimated.var1, cause="inflation")



# Impulse Response Functions (IRFs):
#--------------------------------------------------

set.seed(123)  # necessary for consistency in bootstrapped CIs

# Interest rate response to PSP shock
irf_interest <- irf(estimated.var1, 
                    impulse="psp", 
                    response="interest_rate",
                    n.ahead=24, boot=TRUE, ci=0.95, runs=500)
png("irf_interest_rate.png", width=800, height=600)
plot(irf_interest)
dev.off()

# YOY response to PSP shock
irf_yoy <- irf(estimated.var1, 
               impulse="psp", 
               response="yoy",
               n.ahead=24, boot=TRUE, ci=0.95, runs=500)
png("irf_yoy.png", width=800, height=600)
plot(irf_yoy)
dev.off()

# Unemployment response to PSP shock
irf_unemployment <- irf(estimated.var1, 
                        impulse="psp", 
                        response="unemployment",
                        n.ahead=24, boot=TRUE, ci=0.95, runs=500)
png("irf_unemployment.png", width=800, height=600)
plot(irf_unemployment)
dev.off()

# Inflation response to PSP shock
irf_inflation <- irf(estimated.var1, 
                     impulse="psp", 
                     response="inflation",
                     n.ahead=24, boot=TRUE, ci=0.95, runs=500)
png("irf_inflation.png", width=800, height=600)
plot(irf_inflation)
dev.off()

# PSP own shock IRF for validity check of transitory nature of shock
irf_psp <- irf(estimated.var1, 
               impulse="psp", 
               response="psp",
               n.ahead=24, boot=TRUE, ci=0.95, runs=500)
png("irf_psp.png", width=800, height=600)
plot(irf_psp)
dev.off()

# Rescue Clean QE Shock from VAR Residuals
#---------------------------------------------

# Get residuals from the VAR
var_residuals <- residuals(estimated.var1)

# The PSP residuals are the clean QE shock
qe_shock <- var_residuals[, "psp"]

# Plot and Save
png("qe_shocks.png", width=800, height=600)
plot(qe_shock, type="l", 
     main="Clean QE Shock",
     xlab="Months Elapsed Since 2015-05",
     ylab="Unexpected PSP Purchases")
dev.off()

# Add dates and save as dF
qe_shock_df <- data.frame(
  date = psp$date[2:nrow(psp)],
  qe_shock = qe_shock
)

# Check QE shocks for autocorrelation

png("acf_qe_shock.png", width=800, height=600)
acf(qe_shock, main="ACF of Clean QE Shock")
dev.off()

png("pacf_qe_shock.png", width=800, height=600)
pacf(qe_shock, main="PACF of Clean QE Shock")
dev.off()

# Plotting QE clean shocks against financial variables
#-------------------------------------------------------

# ── Extract financial variables from EA-MPD dataset ──────────────────────────
# Sheet: "Press Release Window"; dates stored as Excel serial numbers.
# We aggregate to monthly frequency (summing changes across all ECB meetings
# within a month) and left-join onto qe_shock_df (2015-04 to 2022-12).

eampd_raw <- read_excel("Dataset_EA-MPD.xlsx", sheet = "Press Release Window")

eampd_monthly <- eampd_raw |>
  mutate(
    date_parsed = as.Date(as.numeric(date), origin = "1899-12-30"),
    year_mon    = format(date_parsed, "%Y-%m")
  ) |>
  filter(
    !is.na(date_parsed),
    date_parsed >= as.Date("2015-05-01"),
    date_parsed <= as.Date("2022-12-31")
  ) |>
  dplyr::select(year_mon, OIS_2Y, OIS_10Y, DE10Y, IT10Y, EURUSD) |>
  group_by(year_mon) |>
  summarise(
    across(c(OIS_2Y, OIS_10Y, DE10Y, IT10Y, EURUSD),
           \(x) sum(x, na.rm = TRUE)),
    .groups = "drop"
  )

# Merge onto QE shock dates; months with no ECB meeting will have NA
eampd <- left_join(qe_shock_df, eampd_monthly, by = c("date" = "year_mon"))

# Helper: fit lm on complete cases and overlay regression line + 95% CI band
add_reg_ci <- function(x, y) {
  df_tmp <- data.frame(x = x, y = y)[complete.cases(data.frame(x, y)), ]
  fit    <- lm(y ~ x, data = df_tmp)
  x_seq  <- seq(min(df_tmp$x), max(df_tmp$x), length.out = 300)
  pred   <- predict(fit, newdata = data.frame(x = x_seq), interval = "confidence")
  polygon(
    c(x_seq, rev(x_seq)),
    c(pred[, "lwr"], rev(pred[, "upr"])),
    col = adjustcolor("red", alpha.f = 0.15), border = NA
  )
  abline(fit, col = "red", lwd = 2)
}

png("scatter_qe_financial.png", width=1200, height=1000)
par(mfrow=c(2,2))

# 1. QE shock vs OIS 2Y
plot(qe_shock_df$qe_shock, eampd$OIS_2Y,
     xlab="QE Shock (PSP residual)",
     ylab="OIS 2Y Change (bps)",
     main="QE Surprise vs OIS 2Y",
     pch=16, col="steelblue")
add_reg_ci(qe_shock_df$qe_shock, eampd$OIS_2Y)

# 2. QE shock vs German Bund 10Y
plot(qe_shock_df$qe_shock, eampd$DE10Y,
     xlab="QE Shock (PSP residual)",
     ylab="German Bund 10Y Change (bps)",
     main="QE Surprise vs German Bund Yield",
     pch=16, col="steelblue")
add_reg_ci(qe_shock_df$qe_shock, eampd$DE10Y)

# 3. QE shock vs Italian 10Y
plot(qe_shock_df$qe_shock, eampd$IT10Y,
     xlab="QE Shock (PSP residual)",
     ylab="Italian 10Y Yield Change (bps)",
     main="QE Surprise vs Italian Yield",
     pch=16, col="steelblue")
add_reg_ci(qe_shock_df$qe_shock, eampd$IT10Y)

# 4. QE shock vs EUR/USD
plot(qe_shock_df$qe_shock, eampd$EURUSD,
     xlab="QE Shock (PSP residual)",
     ylab="EUR/USD Change",
     main="QE Surprise vs EUR/USD",
     pch=16, col="steelblue")
add_reg_ci(qe_shock_df$qe_shock, eampd$EURUSD)

dev.off()

# Plot correlation of QE clean shocks with inflation
inflation_df <- data.frame(
  date = inflation$date,
  hicp_rate = inflation$hicp_rate
)

merged_inflation <- left_join(qe_shock_df, inflation_df, by="date")

png("scatter_qe_inflation.png", width=800, height=600)
plot(merged_inflation$qe_shock, merged_inflation$hicp_rate,
     xlab="QE Shock (PSP residual)",
     ylab="Inflation change (month-on-month)",
     main="QE Shock vs Inflation",
     pch=16, col="steelblue")
add_reg_ci(merged_inflation$qe_shock, merged_inflation$hicp_rate)
dev.off()

# Build enhanced correlation table with t-statistics and significance stars
#---------------------------------------------------------------------------

# Helper: run cor.test on complete cases
corr_test <- function(x, y) {
  idx <- complete.cases(x, y)
  cor.test(x[idx], y[idx])
}

sig_stars <- function(p) {
  dplyr::case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE     ~ ""
  )
}

ct_ois2y  <- corr_test(eampd$qe_shock,              eampd$OIS_2Y)
ct_de10y  <- corr_test(eampd$qe_shock,              eampd$DE10Y)
ct_it10y  <- corr_test(eampd$qe_shock,              eampd$IT10Y)
ct_eurusd <- corr_test(eampd$qe_shock,              eampd$EURUSD)
ct_hicp   <- corr_test(merged_inflation$qe_shock,   merged_inflation$hicp_rate)

cor_table <- data.frame(
  Variable    = c("OIS 2Y", "German Bund 10Y", "Italian 10Y", "EUR/USD", "HICP Inflation"),
  Correlation = round(c(ct_ois2y$estimate,  ct_de10y$estimate,
                        ct_it10y$estimate,  ct_eurusd$estimate,
                        ct_hicp$estimate),  3),
  t_statistic = round(c(ct_ois2y$statistic, ct_de10y$statistic,
                        ct_it10y$statistic, ct_eurusd$statistic,
                        ct_hicp$statistic), 3),
  Sig         = sig_stars(c(ct_ois2y$p.value,  ct_de10y$p.value,
                             ct_it10y$p.value,  ct_eurusd$p.value,
                             ct_hicp$p.value))
)

# Note: *** p<0.01  ** p<0.05  * p<0.10
write.csv(cor_table, "correlation_table.csv", row.names = FALSE)
print(cor_table)



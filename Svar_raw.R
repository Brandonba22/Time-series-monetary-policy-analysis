#------------------
# loading packages
#------------------
library(tinytex)
library(modelsummary)
library(svars)
library(dplyr)
library(magrittr)
library(tidyquant)
library(timetk)
library(tibbletime)
library(tsibble)
library(vars)         
library(broom)
library(stargazer)
library(listviewer)
library(ggplot2)
library(ggfortify)
library(qqplotr)
library(scales)
library(plotly)
library(dplyr)
library(tidyr)
library(purrr)
library(knitr)
library(patchwork)

#-----------------
#loading the data
#-----------------
# getting economic data
macro_data <- tq_get(c("FEDFUNDS", "UNRATE", "PCEPILFE"), 
                     get = "economic.data",
                     from = "1990-01-01", to = "2024-12-31") %>%
  mutate(monthyear = as.yearmon(date))

# Get Monthly S&P 500 Prices 
sp500_raw <- tq_get("^GSPC", get = "stock.prices", 
                    from = "1990-01-01", to = "2024-12-31") %>%
  select(date, adjusted) %>%
  mutate(monthyear = as.yearmon(date)) %>%
  group_by(monthyear) %>%
  summarise(SP500 = mean(adjusted), .groups = "drop")
# Reshape Economic Data to Wide Format 
macro_wide <- macro_data %>%
  select(monthyear, symbol, price)
macro_wide <- macro_data %>%
  select(monthyear, symbol, price) %>%
  pivot_wider(names_from = symbol, values_from = price)
# Merge & Transform 
combined <- macro_wide %>%
  inner_join(sp500_raw, by = "monthyear") %>%
  arrange(monthyear) %>%
  mutate(log_sp500 = log(SP500),log_pce = log(PCEPILFE),
         dlrSP500 = 100 * ((log_sp500 - lag(log_sp500)) - (log_pce - lag(log_pce))),  # real return
         INFL = 100 * (log_pce - lag(log_pce)),                                       # core PCE inflation
         FF = FEDFUNDS                                                                # interest rate
  ) %>%
  select(monthyear, FF, dlrSP500, UNRATE, INFL)%>%
  drop_na()

#  S&P 500 Index Level
q1 <- ggplot(sp500_raw, aes(x = monthyear)) +
  geom_line(aes(y = SP500), color = "gold") +
  labs(title = "S&P 500 Index Level", y = "Index Level", x = "Date")

#  Unemployment Rate
q2 <- ggplot(macro_wide, aes(x = monthyear)) +
  geom_line(aes(y = UNRATE), color = "purple") +
  labs(title = "Unemployment Rate", y = "Percent", x = "Date")

# Federal Funds Rate
q3 <- ggplot(macro_wide, aes(x = monthyear)) +
  geom_line(aes(y = FEDFUNDS), color = "orange") +
  labs(title = "Federal Funds Rate", y = "Percent", x = "Date")

#Core PCE Inflation Index
q4 <- ggplot(macro_wide, aes(x = monthyear)) +
  geom_line(aes(y = PCEPILFE), color = "red") +
  labs(title = "Core PCE Inflation Index", y = "Index Level", x = "Date")

# Combine plots 
(q1 | q2) / (q3 | q4)

# checking the to dataframe 
head(combined, 6) %>%
  kable(caption = "Table 1: Sample of Combined Time Series Data", digits = 2)

#----------------------------
# Checking if stationary
#----------------------------
p1 <- ggplot(combined, aes(x = monthyear)) +
  geom_line(aes(y = UNRATE), color = "gold") +
  labs(title = "Unemployment Rate", y = "Level", x = "Date")

p2 <- ggplot(combined, aes(x = monthyear)) +
  geom_line(aes(y = dlrSP500), color = "purple") +
  labs(title = "S&P 500 Real Return", y = "Level", x = "Date")

p3 <- ggplot(combined, aes(x = monthyear)) +
  geom_line(aes(y = FF), color = "orange") +
  labs(title = "Federal Funds Rate", y = "Level", x = "Date")

p4 <- ggplot(combined, aes(x = monthyear)) +
  geom_line(aes(y = INFL), color = "red") +
  labs(title = "Core PCE Inflation", y = "Level", x = "Date")

(p1 | p2) / (p3 | p4)

#-------------
# ADF test 
#-------------
# federal funds rate (non-stationary)
adf_ff <- ur.df(combined$FF, type = "drift", selectlags = "AIC") 
summary(adf_ff)
# inflation (stationary)
adf_INFL <- ur.df(combined$INFL, type = "drift", selectlags = "AIC") 
summary(adf_INFL)
# unemployment (stationary)
adf_uem <- ur.df(combined$UNRATE, type = "drift", selectlags = "AIC") 
summary(adf_uem)
# Sp500 (stationary)
adf_500 <- ur.df(combined$dlrSP500, type = "drift", selectlags = "AIC") 
summary(adf_500)

# Create a summary table
adf_summary <- tibble::tibble(
  Variable = c("FF", "dlrSP500", "UNRATE", "INFL"),
  Test_Statistic = c(-2.488, -13.701, -3.355, -9.569),
  `1% Critical` = rep(-3.44, 4),
  `5% Critical` = rep(-2.87, 4),
  `10% Critical` = rep(-2.57, 4),
  Stationary = c("No", "Yes", "Yes", "Yes")
)
# Print the table
kable(adf_summary, caption = "ADF Test Results: Stationarity of Variables", digits = 3)

#-------------------
# var
#-------------------
# Create ts object
Y <- ts(combined[, c("FF", "dlrSP500", "UNRATE", "INFL")], start = c(1990, 1), frequency = 12)

# Use VARselect to determine optimal lags
vselect <- vars::VARselect(Y, lag.max = 8, type = "const")
print(vselect$selection)

# Estimate the VAR model using aic
optimal_lag <- vselect$selection["AIC(n)"]

v_model <- vars::VAR(Y, p = optimal_lag, type = "const")

#----------------------------
# Impulse response functions
#----------------------------
# Generate impulse response functions from FF shock
irf_results <- vars::irf(v_model, impulse = "FF", response = c("dlrSP500", "UNRATE", "INFL"), 
                         n.ahead = 24, boot = TRUE)

# Plot the IRFs
plot(irf_results)

#---------------------------
#  Granger 
#---------------------------
# Granger test: Does FF help predict other variables?
g_ff     <- causality(v_model, cause = "FF")$Granger
g_sp500  <- causality(v_model, cause = "dlrSP500")$Granger
g_unrate <- causality(v_model, cause = "UNRATE")$Granger
g_infl   <- causality(v_model, cause = "INFL")$Granger
#granger table 
granger_results <- tibble::tibble(
  Cause         = c("FF", "dlrSP500", "UNRATE", "INFL"),
  F_Statistic   = c(g_ff$statistic, g_sp500$statistic, g_unrate$statistic, g_infl$statistic),
  DF1           = c(g_ff$parameter[1], g_sp500$parameter[1], g_unrate$parameter[1], g_infl$parameter[1]),
  DF2           = c(g_ff$parameter[2], g_sp500$parameter[2], g_unrate$parameter[2], g_infl$parameter[2]),
  P_Value       = c(g_ff$p.value, g_sp500$p.value, g_unrate$p.value, g_infl$p.value),
  Granger_Causes = c(
    ifelse(g_ff$p.value < 0.05, "Yes", "No"),
    ifelse(g_sp500$p.value < 0.05, "Yes", "No"),
    ifelse(g_unrate$p.value < 0.05, "Yes", "No"),
    ifelse(g_infl$p.value < 0.05, "Yes", "No")
  )
)

#-----------------------------
#   svar
#-----------------------------
# Estimate SVAR using Cholesky decomposition (recursive identification)
#colnames(Y)
svar_model <- svars::id.chol(v_model)
#irf for svar
# Generate impulse responses from FF only
irf_ff <- irf(svar_model, impulse = "FF", 
              response = c("dlrSP500", "UNRATE", "INFL"), 
              n.ahead = 24, boot = TRUE)
plot(irf_ff)

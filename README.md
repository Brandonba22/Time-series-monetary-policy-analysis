# Time-series-monetary-policy-analysis
This repository contains an applied time series and structural analysis of U.S. monetary policy transmission using R. 
# Monetary Policy Analysis Using SVAR in R

This project conducts a structural time series analysis of U.S. monetary policy using a Structural Vector Autoregression (SVAR) model. The analysis investigates how changes in the federal funds rate influence macroeconomic variables such as unemployment and core inflation.

## Objective

To empirically evaluate the transmission of monetary policy shocks using recent U.S. economic data, and visualize the short-run dynamic effects on inflation and unemployment.

## Features

- Fetches macroeconomic data from FRED via `tidyquant` (`FEDFUNDS`, `UNRATE`, `PCEPILFE`)
- Transforms and structures monthly time series data for SVAR modeling
- Estimates and interprets impulse response functions (IRFs)
- Visualizes interest rate shock effects using `ggplot2`
- Clean `.R` file for replication, or reporting.

## Repository Contents
## Libraries Used

- `tidyquant`, `timetk` – Data collection and transformation
- `zoo`, `lubridate`, `dplyr` – Time handling and manipulation
- `vars`, `urca` – Vector autoregression and stationarity testing
- `ggplot2` – Visualization
- `RDM file` – Report Composition

## Data Source

All macroeconomic data was sourced from the [Federal Reserve Economic Database (FRED)](https://fred.stlouisfed.org/), covering the period from January 1990 to December 2024.

All financial market data was sourced from Yahoo Finance using the tidyquant package in R, covering the period from January 1990 to December 2024.

## Potential Extensions

- Add forecast error variance decomposition (FEVD)
- Include S&P 500 or other financial market indicators
- Compare Cholesky and sign-restricted identification schemes

## Author

Brandon Arnold  
University of Houston – M.A. Applied Economics 

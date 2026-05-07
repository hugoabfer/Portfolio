# Multivariate Volatility Analysis in ESG Portfolios

This project is the core technical component of my **Undergraduate Thesis (TFG)**. The research focuses on the dynamic relationship between assets that follow **ESG (Environmental, Social, and Governance)** criteria.

The goal was to move beyond static correlation matrices, which often fail to capture how assets behave during market stress. By using a **DCC-GARCH** (Dynamic Conditional Correlation) approach, I modeled how the ties between sustainable investments change over time, providing a more accurate framework for risk management and portfolio optimization in the ESG space.

## Project Scope
* **Context:** Research conducted at the University of Santiago de Compostela (USC).
* **Objective:** Measuring time-varying correlations between ESG-compliant stocks/indices to improve VaR (Value at Risk) estimates.
* **Methodology:** 1. Filtered returns using univariate GARCH(1,1) processes.
  2. Estimated the dynamic correlation structure using the DCC specification.
  3. Analyzed the impact of market shocks on the stability of ESG portfolios.

## Tech Stack & Libraries
The analysis was performed entirely in **R**. Key libraries used:
* `rugarch` - For univariate volatility modeling.
* `rmgarch` - For the multivariate DCC specification.
* `quantmod` & `PerformanceAnalytics` - For financial data retrieval and return analysis.

## Key Findings
The model successfully captured "volatility clustering" and showed that correlations between ESG assets are not constant. They tend to spike during market downturns, a crucial insight for investors looking for true diversification within sustainable finance.

## How to use
1. Load the `DCC-GARCH.r` script in RStudio.
2. Ensure you have the required packages installed: `install.packages(c("rugarch", "rmgarch", "quantmod"))`.
3. The script is set up to pull historical data, estimate the model, and output the dynamic correlation plots.

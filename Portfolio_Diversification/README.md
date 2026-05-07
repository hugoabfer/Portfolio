# Portfolio Allocation Models

This script compares three different asset allocation strategies. I wrote this to test empirically how mathematical optimization performs against a simple equally weighted approach.

## Strategies implemented
* **Naive (1/N):** The baseline. Equal weights for all assets.
* **Markowitz (Mean-Variance):** Uses `scipy.optimize` to maximize the Sharpe ratio based on historical returns and the covariance matrix.
* **Risk Parity:** Adjusts the weights so that every single asset contributes the exact same amount of volatility to the overall portfolio. 

## Requirements
You will need standard data science libraries to run the code. If you want to pull live data, `yfinance` is also required.
```bash
pip install pandas numpy scipy matplotlib yfinance

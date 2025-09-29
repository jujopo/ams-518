# fetch_prices.py
"""
Fetch historical stock price data using yfinance and save to CSV.

Usage:
    python fetch_prices.py --tickers AAPL MSFT TSLA --period 5y --interval 1d

Arguments:
    --tickers   List of ticker symbols (space-separated).
    --period    Length of history (e.g., 1y, 5y, max).
    --interval  Data frequency (e.g., 1d, 1h, 5m).
"""

import argparse
import os
import yfinance as yf
import pandas as pd


def fetch_and_save(ticker: str, period: str, interval: str, out_dir: str = "data/raw/"):
    """Fetch price data for one ticker and save to CSV."""
    print(f"Fetching {ticker} ({period}, {interval})...")
    
    try:
        data = yf.download(ticker, period=period, interval=interval, auto_adjust=True)
        
        if data.empty:
            print(f"⚠️ No data returned for {ticker}. Skipping.")
            return

        os.makedirs(out_dir, exist_ok=True)
        file_path = os.path.join(out_dir, f"{ticker}_{period}_{interval}.csv")
        data.to_csv(file_path)
        print(f"✅ Saved {ticker} data to {file_path}")

    except Exception as e:
        print(f"❌ Error fetching {ticker}: {e}")


def main():
    parser = argparse.ArgumentParser(description="Download stock price data.")
    parser.add_argument("--tickers", nargs="+", required=True, help="List of stock tickers")
    parser.add_argument("--period", default="5y", help="History length (e.g., 1y, 5y, max)")
    parser.add_argument("--interval", default="1d", help="Frequency (e.g., 1d, 1h, 5m)")
    parser.add_argument("--out", default="data/raw/", help="Output directory for CSV files")

    args = parser.parse_args()

    for ticker in args.tickers:
        fetch_and_save(ticker, args.period, args.interval, args.out)


if __name__ == "__main__":
    main()

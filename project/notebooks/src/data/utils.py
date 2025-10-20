import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import scipy.stats
from scipy.stats import norm, t, probplot

def timeframe_selector(data: dict, stocks: list, timeframe: str, freq: str):
    '''
    Selects a desired timeframe from our data format
    
    Inputs: 
        data (dict): data to analyze
        stocks (list): list of stock tickers
        timeframe (str): '10y', '5y', '730d', '60d', '8d'
        freq (str): '1d', '1h', '5m', '1m'
    '''
    specific_data = {}
    for key in data.keys():
        ticker, period, frequency = key.split("_")[:3]
        if (ticker in stocks) and (period == timeframe) and (frequency == freq):
            specific_data[key] = data[key]
            
    return specific_data

def close_prices_df_generator(data: dict):
    df = pd.DataFrame()
    for key in data.keys():
        df[key.split('_')[0]] = data[key]['Close']

    return df

def compute_returns(df: pd.DataFrame) -> pd.Series:
    '''Computes the returns based on the period of the dataframe'''
    df_returns = pd.DataFrame()

    for ticker in df.columns:
        df_returns[f'{ticker}'] = df[f'{ticker}'].pct_change()
    
    df_returns.dropna(inplace=True)
    return df_returns

def descriptive_statistics(data: pd.DataFrame):
    '''Computes the descriptive statistics for the dataframe containing the stocks'''

    statistics = {}
    for ticker in data.columns:
        series = data[f'{ticker}']
        stats_d = {
            'n_obs': int(series.shape[0]),
            'mean': float(series.mean()),
            'median': float(series.median()), 
            'std': float(series.std()),
            "skewness": float(series.skew()),
            'kurtosis': float(series.kurtosis()),   # Fisher (0 for normal)
            'min': float(series.min()),
            '5%': float(series.quantile(0.05)),
            '25%': float(series.quantile(0.25)),
            '75%': float(series.quantile(0.75)),
            '95%': float(series.quantile(0.95)),
            'max': float(series.max())
        }
        statistics[f'{ticker}'] = stats_d
    
    return statistics

def generate_histograms(data: pd.DataFrame, stats: dict, num_points=50):
    for ticker in data.columns:
        std = stats[f'{ticker}']['std'] # Extract std
        sigma = std ** 2                # Compute variance
        mean = stats[f'{ticker}']['mean']   # Extract mean
        min_range = mean - 3 * std      # Define ranges for plotting
        max_range = mean + 3 * std
        x_range = np.linspace(min_range, max_range, num_points)
        # Compute PDF values
        pdf_values = (1 / np.sqrt(2 * np.pi * sigma)) * np.exp(-(x_range - mean)**2 / (2 * sigma))
        # Find a t distribution that fits the data
        params = t.fit(data[f'{ticker}'])   # Params is a tuple containing df, loc and scale
        t_dist = t.pdf(x_range, *params)
            
        # Plot the histogram, the normal fit and the t fit
        plt.figure(figsize=(9,4), dpi=200)
        plt.hist(data[f'{ticker}'], bins=40, edgecolor='black', density=True, 
                 range=(stats[f'{ticker}']['min'], stats[f'{ticker}']['max']))
        plt.plot(x_range, pdf_values, color='black', linewidth=2, 
                 label=f'Normal distribution fit')
        plt.plot(x_range, t_dist, color='purple', linewidth=2, 
                 label='t distribution fit')
        plt.title(f'Returns distribution for {ticker}')
        plt.xlabel('Value')
        plt.ylabel('Density')
        plt.legend()
        plt.show()

def generate_qq_plots(data: pd.DataFrame):
    """
    Generates Normal and Student's t Q-Q plots for each stock ticker in the DataFrame.
    """
    for ticker in data.columns:
        ticker_data = data[ticker].dropna()

        # --- Create a figure with two subplots, side-by-side ---
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))
        fig.suptitle(f'Q-Q Plots for {ticker}', fontsize=16)

        # --- 1. Normal Q-Q Plot (Left) ---
        probplot(ticker_data, dist="norm", plot=ax1)
        ax1.set_title("Normal Distribution Q-Q Plot")
        ax1.set_xlabel("Theoretical Quantiles (Normal)")
        ax1.set_ylabel("Sample Quantiles")

        # --- 2. Student's t Q-Q Plot (Right) ---
        # First, we need to fit the t-distribution to get its parameters
        df, loc, scale = t.fit(ticker_data)
        
        # Now, create the plot using the fitted parameters
        probplot(ticker_data, dist=t, sparams=(df, loc, scale), plot=ax2)
        ax2.set_title(f"Student's t-Distribution Q-Q Plot, df = {df}")
        ax2.set_xlabel("Theoretical Quantiles (Student's t)")
        ax2.set_ylabel("") # Hide y-label for cleaner look

        plt.tight_layout(rect=[0, 0.03, 1, 0.95]) # Adjust layout to make space for suptitle
        plt.show()
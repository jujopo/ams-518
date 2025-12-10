import requests
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import scipy.stats
from scipy.stats import norm, t, probplot
from datetime import datetime, timedelta

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

def generate_histograms(data: pd.DataFrame, stats: dict, num_points=50, figsize=(8, 4), dpi=100):
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
        plt.figure(figsize=figsize, dpi=dpi)
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
        
def generate_histograms_with_residuals(data: pd.DataFrame, num_points=50, figsize=(10, 8)) :
    for ticker in data.columns:
        ticker_data = data[ticker].dropna()
        
        # Create a figure with two subplots, stacked vertically
        # sharex=True links the x-axis of both plots for easier comparison
        fig, ax = plt.subplots(
            2, 1, 
            figsize=figsize, 
            sharex=True, 
            gridspec_kw={'height_ratios': [3, 1]} # Make the top plot taller
        )

        # Top plot
        
        # Get the histogram data (bar heights and bin edges)
        # We need these values to calculate the residuals later
        counts, bin_edges, _ = ax[0].hist(
            ticker_data, 
            bins=num_points, 
            density=True, 
            edgecolor='black', 
            alpha=0.6, 
            label='Daily Returns'
        )
        
        # Calculate the center of each histogram bin for plotting
        bin_centers = (bin_edges[:-1] + bin_edges[1:]) / 2

        # Fit and plot the Normal distribution
        mean, std_dev = norm.fit(ticker_data)
        normal_pdf = norm.pdf(bin_centers, mean, std_dev)
        ax[0].plot(bin_centers, normal_pdf, 'orange', linewidth=2, label='Normal Distribution Fit')

        # Fit and plot student's t distribution
        df, loc, scale = t.fit(ticker_data)
        t_pdf = t.pdf(bin_centers, df, loc, scale)
        ax[0].plot(bin_centers, t_pdf, 'r--', linewidth=2, label="Student's t-Distribution Fit")

        ax[0].set_title(f'Distribution and Residuals for {ticker}')
        ax[0].set_ylabel("Probability Density")
        ax[0].legend()
        
        # Bottom plot: residuals

        # Calculate the residuals
        normal_residuals = counts - normal_pdf
        t_residuals = counts - t_pdf

        # Plot the residuals as bars
        bar_width = bin_edges[1] - bin_edges[0]
        ax[1].bar(bin_centers, normal_residuals, width=bar_width, alpha=0.6, label='Normal Fit Residuals')
        ax[1].bar(bin_centers, t_residuals, width=bar_width, alpha=0.6, label="Student's t Residuals")
        
        # Add a horizontal line at zero for reference
        ax[1].axhline(0, color='black', linestyle='--')

        ax[1].set_xlabel("Daily Returns")
        ax[1].set_ylabel("Residual (Actual - Fit)")
        ax[1].legend()

        plt.tight_layout() # Adjusts plot to prevent labels from overlapping
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
        
def get_historical_market_cap(api_key, symbol, start_date=None, end_date=None):
        """
        Extract historical market capitalization for a given stock symbol
        
        Args:
            symbol (str): Stock ticker symbol (e.g., 'AAPL', 'GOOGL')
            start_date (str): Start date in 'YYYY-MM-DD' format (optional)
            end_date (str): End date in 'YYYY-MM-DD' format (optional)
        """
        
        # If no dates provided, get last 30 days
        if not end_date:
            end_date = datetime.now().strftime('%Y-%m-%d')
        if not start_date:
            start_date = (datetime.now() - timedelta(days=30)).strftime('%Y-%m-%d')
        
        # Endpoint for historical market cap
        url = f"https://financialmodelingprep.com/stable/historical-market-capitalization?symbol={symbol}"
        
        params = {
            'from': start_date,
            'to': end_date,
            'apikey': api_key
        }
        
        try:
            response = requests.get(url, params=params)
            response.raise_for_status()
            
            data = response.json()
            
            if not data:
                print(f"No data found for {symbol}")
                return None
            
            # Convert to DataFrame
            df = pd.DataFrame(data)
            
            # Convert date and format market cap
            df['date'] = pd.to_datetime(df['date'])
            df['marketCap'] = pd.to_numeric(df['marketCap'])
            
            # Sort by date
            df = df.sort_values('date')
            
            return df
            
        except requests.exceptions.RequestException as e:
            print(f"Error fetching data: {e}")
            return None
        
def performance_metrics(weights, returns: pd.DataFrame, daily_rf_rate: pd.DataFrame, timeframe) -> tuple:
    """
    Calculates key performance metrics for a given portfolio.
    
    Args:
    weights (np.array): Series of asset weights (N x 1)
    returns (pd.DataFrame): DataFrame of assets returns (T x N)
    daily_rf_rate (pd.DataFrame): DataFrame of daily risk free rate DTB3 (T x 1)
    timeframe (tuple): Tuple containing the start and end date of the analysis
    
    Returns:
    dict: A dictionary containing the wealth series and key metrics.
    """
    # Timeframe window
    start_date = timeframe[0]
    end_date = timeframe[1]
    
    # Isolate the desired return data
    stock_returns = returns.loc[start_date:end_date]

    # Calculate portfolio returns
    portfolio_ret = stock_returns @ weights
    portfolio_ret = pd.DataFrame(portfolio_ret, columns=['return'])

    # Calculate Wealth index
    wealth_index = (1 + portfolio_ret).cumprod()

    # Merge datasets aligning them by date
    portfolio_rf = pd.merge(portfolio_ret,
         daily_rf_rate,
         right_on='observation_date',
         how='inner',
         left_index=True)
    
    # Excess returns
    portfolio_rf['excess_ret'] = portfolio_rf['return'] - portfolio_rf['risk_free']
    excess_returns = portfolio_rf['excess_ret']

    # Calculate annualized sharpe ratio from excess returns
    mean_excess_return = excess_returns.mean()
    std_excess_return = excess_returns.std()

    # Avoid division by zero if std is 0
    if std_excess_return == 0:
        annualized_sharpe = 0.0
    else:
        daily_sharpe = mean_excess_return / std_excess_return
        annualized_sharpe = daily_sharpe * np.sqrt(252)
     
    # Calculate Max Drawdown
    running_peak = wealth_index.cummax()
    drawdown = (wealth_index - running_peak) / running_peak
    max_drawdown = drawdown.min()
    
    # VaR and CVaR (95% Confidence)
    confidence_level_95 = 0.05
    var_95 = np.percentile(portfolio_ret, confidence_level_95 * 100)
    cvar_95 = portfolio_ret[portfolio_ret <= var_95].mean()

    # VaR and CVaR (99% Confidence)
    confidence_level_99 = 0.01
    var_99 = np.percentile(portfolio_ret, confidence_level_99 * 100)
    cvar_99 = portfolio_ret[portfolio_ret <= var_99].mean()

    return ({
        "wealth": wealth_index,
        "total_return": wealth_index.iloc[-1] - 1,
        "annualized_sharpe": annualized_sharpe,
        "drawdown": drawdown,
        "max_drawdown": max_drawdown,
        "portfolio_daily_returns": portfolio_ret,
        "var_95": var_95,
        "cvar_95": cvar_95,
        "var_99": var_99,
        "cvar_99": cvar_99
    }, portfolio_rf)
    
def performance_metrics_index(returns: pd.DataFrame, daily_rf_rate: pd.DataFrame, timeframe) -> tuple:
    """
    Calculates key performance metrics for a given portfolio.
    
    Args:
    returns (pd.DataFrame): DataFrame of assets returns (T x N)
    daily_rf_rate (pd.DataFrame): DataFrame of daily risk free rate DTB3 (T x 1)
    timeframe (tuple): Tuple containing the start and end date of the analysis
    
    Returns:
    dict: A dictionary containing the wealth series and key metrics.
    """
    # Timeframe window
    start_date = timeframe[0]
    end_date = timeframe[1]
    
    # Isolate the desired return data
    index_returns = returns.loc[start_date:end_date]
    index_returns = pd.DataFrame(index_returns)
    index_returns.columns = ['return']

    # Calculate Wealth index
    wealth_index = (1 + index_returns).cumprod()

    # Merge datasets aligning them by date
    portfolio_rf = pd.merge(index_returns,
         daily_rf_rate,
         right_on='observation_date',
         how='inner',
         left_index=True)
    
    # Excess returns
    portfolio_rf['excess_ret'] = portfolio_rf['return'] - portfolio_rf['risk_free']
    excess_returns = portfolio_rf['excess_ret']

    # Calculate annualized sharpe ratio from excess returns
    mean_excess_return = excess_returns.mean()
    std_excess_return = excess_returns.std()

    # Avoid division by zero if std is 0
    if std_excess_return == 0:
        annualized_sharpe = 0.0
    else:
        daily_sharpe = mean_excess_return / std_excess_return
        annualized_sharpe = daily_sharpe * np.sqrt(252)
     
    # Calculate Max Drawdown
    running_peak = wealth_index.cummax()
    drawdown = (wealth_index - running_peak) / running_peak
    max_drawdown = drawdown.min()
    
    # VaR and CVaR (95% Confidence)
    confidence_level_95 = 0.05
    var_95 = np.percentile(index_returns, confidence_level_95 * 100)
    cvar_95 = index_returns[index_returns <= var_95].mean()

    # VaR and CVaR (99% Confidence)
    confidence_level_99 = 0.01
    var_99 = np.percentile(index_returns, confidence_level_99 * 100)
    cvar_99 = index_returns[index_returns <= var_99].mean()

    return ({
        "wealth": wealth_index,
        "total_return": wealth_index.iloc[-1] - 1,
        "annualized_sharpe": annualized_sharpe,
        "drawdown": drawdown,
        "max_drawdown": max_drawdown,
        "var_95": var_95,
        "cvar_95": cvar_95,
        "var_99": var_99,
        "cvar_99": cvar_99
    }, portfolio_rf)
    
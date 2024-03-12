# Project 1: task1


# this script requires the quantmod- and the tidyverse-package to run!
required <- c("quantmod", "tidyverse", "patchwork")
for (pkg in required) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# - Obtaining historical stock data ----

# define the 5 stocks making up the portfolio
tickers <- c("AAPL", "GOOG", "AMZN", "MSFT", "TSLA")

# define historical data window
to <- as.Date("2024-03-04")
from <- to - years(5) # 5 years of price data

# create list containing daily returns for each stock in portfolio
stock_daily_returns <- tickers %>% 
  lapply(\(x) getSymbols(x, from = from, to = to, auto.assign = FALSE)[, 4]) %>% # get close prices from yahoo
  lapply(\(x) ((x / lag(x)) - 1)) # calculate returns
names(stock_daily_returns) <- tickers # update names

# create variable with portfolio daily returns
portfolio_daily_returns <- do.call(merge, stock_daily_returns) # merge lists
colnames(portfolio_daily_returns) <- tickers # update column names

# remove (if any) dates missing close price 
portfolio_daily_returns <- 
  portfolio_daily_returns[apply(portfolio_daily_returns, 1, \(x) all(!is.na(x))),]

# convert daily returns to data frame
portfolio_daily_returns <- data.frame(portfolio_daily_returns)

# - Monte Carlo Simulation of Weights ----

# set simulation parameters
set.seed(0)
n_its <-  100000 # the number of simulations to run

# generate portfolio weights
weights <- matrix(nrow = n_its, ncol = length(tickers)) # matrix to append weights to
for (i in seq_len(n_its)) {
  x <- runif(n = length(tickers)) # obtain weights from the uniform distribution
  x <- x / sum(x) # normalize the weight to sum to one
  weights[i,] <- x # append weights to weight matrix
}
weights <- data.frame(weights) # convert from matrix to data frame
colnames(weights) <- tickers # update column names
weights <- weights %>% mutate(sim = row_number()) # add column identifying from which sim the weights is obtained


# - Compute Portfolio Expected Return, Risk and Sharpe-Ratio ----

# create a portfolio data frame
portfolio <- data.frame(sim = seq(1, n_its), exp = NA, sd = NA, sharpe = NA)

# compute the expected return of each stock in the portfolio
stock_exp_return <- colMeans(portfolio_daily_returns)

# compute expected returns
for (i in seq_len(nrow(portfolio))) {
  exp <- sum(stock_exp_return * as.numeric(weights[i,1:5])) # computes the expected return with a simulated set of weights
  portfolio$exp[i] <- exp # appends the exp return to the portfolio data frame
}

# compute the portfolio covariance matrix
portfolio_cov <- cov(portfolio_daily_returns)

# compute the portfolio sd
for (i in seq_len(nrow(portfolio))) {
  x <- portfolio_cov %*% t(weights[i,1:5]) # matrix product of transposed weights and portfolio cov
  var <- as.numeric(weights[i,1:5]) %*% x  # matrix product of the previous and the weights
  sd <- sqrt(var) # square variance to obtain sd
  portfolio$sd[i] <- sd # append sd to portfolio data frame
}

# determine the risk free rate
# source: https://ycharts.com/indicators/10_year_treasury_rate
rfr <- (1 + .0422)^(1/250) - 1 # 10-year US treasury bill had an annual rate of 4.22% on the day we did the computing
                               # calculation assumes 250 trading days in a year
          
# compute the sharpe-ratio
for (i in seq_len(nrow(portfolio))){
  portfolio$sharpe[i] <- (portfolio$exp[i] - rfr) / portfolio$sd[i] # compute and append to portfolio data frame
}


# - Sharpe-Maximizing Weights ----

# determine the weights that maximized the sharpe
x <- which.max(portfolio$sharpe) # determine the sim with max sharpe
opt_weights <- weights[x,] # obtain weights

# determine the exp and sd of the optimal weights
portfolio[x,]
exp_opt <- portfolio[x,2] # used to highlight portfolio in efficient frontier
sd_opt <- portfolio[x,3]  # -|-


# - Efficient Frontier ----

# plot the efficient frontier
eff_front <- ggplot(aes(x = sd*100, y = exp*100), data = portfolio) +
  geom_point(alpha = .1, size = .1) +
  geom_point(aes(x = sd_opt*100, y = exp_opt*100, col = "Max Sharpe"), shape = 16) +
  labs(
    title = "Efficient Frontier",
    y = "E[r]",
    x = "σ"
  ) +
  scale_color_manual(name = NULL, values = "red") +
  theme_minimal() +
  theme(legend.position = "top")


# - Histogram of Sharp-Ratio Calculations ----
histogram <- ggplot(aes(x = sharpe), data = portfolio) +
  geom_histogram() +
  labs(
    title = "Distribution of Sharpe-Ratios",
    x = "Sharpe-Ratio",
    y = "Frequency"
  ) +
  theme_minimal()

# Save plots
# ggsave("plot_task1.png", plot = wrap_plots(eff_front, histogram), width = 10, height = 3)  

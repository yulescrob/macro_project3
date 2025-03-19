#Project 3 - Business Fluctuations and Firm-Level Responses to Economic Crises
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lmtest)
library(tidyverse)
library(lubridate)

#Part 1: Cross-Country Business Cycle Analysis
data <- read_csv("cycles_data.csv")

#first differences for all economic variables
df_diff <- data %>%
  mutate(
    GDP_Canada_diff = gdp_can-lag(gdp_can),
    GDP_USA_diff = gdp_usa-lag(gdp_usa),
    GDP_Germany_diff = gdp_ger-lag(gdp_ger),
    
    Unemployment_Canada_diff = unem_can-lag(unem_can),
    Unemployment_USA_diff = unem_usa-lag(unem_usa),
    Unemployment_Germany_diff = unem_ger-lag(unem_ger),
    
    Expend_Canada_diff = exp_can-lag(exp_can),
    Expend_USA_diff = exp_usa-lag(exp_usa),
    Expend_Germany_diff = exp_ger-lag(exp_ger),
    
    Bond_Canada_diff = bond_can-lag(bond_can),
    Bond_USA_diff = bond_usa-lag(bond_usa),
    Bond_Germany_diff = bond_ger-lag(bond_ger)
  )

#standard deviations
volatility <- df_diff %>%
  summarise(
    GDP_Canada_vol = sd(GDP_Canada_diff, na.rm = TRUE),
    GDP_USA_vol = sd(GDP_USA_diff, na.rm = TRUE),
    GDP_Germany_vol = sd(GDP_Germany_diff, na.rm = TRUE),
    
    Unemployment_Canada_vol = sd(Unemployment_Canada_diff, na.rm = TRUE),
    Unemployment_USA_vol = sd(Unemployment_USA_diff, na.rm = TRUE),
    Unemployment_Germany_vol = sd(Unemployment_Germany_diff, na.rm = TRUE),
    
    Expend_Canada_vol = sd(Expend_Canada_diff, na.rm = TRUE),
    Expend_USA_vol = sd(Expend_USA_diff, na.rm = TRUE),
    Expend_Germany_vol = sd(Expend_Germany_diff, na.rm = TRUE),
    
    Bond_Canada_vol = sd(Bond_Canada_diff, na.rm = TRUE),
    Bond_USA_vol = sd(Bond_USA_diff, na.rm = TRUE),
    Bond_Germany_vol = sd(Bond_Germany_diff, na.rm = TRUE)
  )

# print volatility measures
print(volatility)

# correlations between GDP and other variables
correlations <- df_diff %>%
  summarise(
    Unemployment_Canada_corr = cor(GDP_Canada_diff, Unemployment_Canada_diff, use = "complete.obs"),
    Unemployment_USA_corr = cor(GDP_USA_diff, Unemployment_USA_diff, use = "complete.obs"),
    Unemployment_Germany_corr = cor(GDP_Germany_diff, Unemployment_Germany_diff, use = "complete.obs"),
    
    Expend_Canada_corr = cor(GDP_Canada_diff, Expend_Canada_diff, use = "complete.obs"),
    Expend_USA_corr = cor(GDP_USA_diff, Expend_USA_diff, use = "complete.obs"),
    Expend_Germany_corr = cor(GDP_Germany_diff, Expend_Germany_diff, use = "complete.obs"),
    
    Bond_Canada_corr = cor(GDP_Canada_diff, Bond_Canada_diff, use = "complete.obs"),
    Bond_USA_corr = cor(GDP_USA_diff, Bond_USA_diff, use = "complete.obs"),
    Bond_Germany_corr = cor(GDP_Germany_diff, Bond_Germany_diff, use = "complete.obs")
  )

# print correlations
print(correlations)

# clean data for visualization
correlations_clean <- correlations %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Correlation") %>%
  separate(Variable, into = c("Indicator", "Country"), sep = "_", extra = "merge")

# Plot correlation results
ggplot(correlations_clean, aes(x = Indicator, y = Correlation, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Correlation with GDP (Cyclicality)", y = "Correlation Coefficient") +
  geom_hline(yintercept = 0, linetype = "dashed")


# cross-correlations between GDP time series of different countries
# removed missing values from GDP data before using ccf()
df_clean <- df_diff %>% drop_na(GDP_Canada_diff, GDP_USA_diff, GDP_Germany_diff)

# Compute cross-correlations, ACF
ccf(df_clean$GDP_Canada_diff, df_clean$GDP_USA_diff, main="Canada vs USA GDP Cross-Correlation")
ccf(df_clean$GDP_Canada_diff, df_clean$GDP_Germany_diff, main="Canada vs Germany GDP Cross-Correlation")
ccf(df_clean$GDP_USA_diff, df_clean$GDP_Germany_diff, main="USA vs Germany GDP Cross-Correlation")



#Part 2: Event Study – The Global Financial Crisis and One Other Recession

# Load the dataset
data <- read.csv("GERdat.csv")

# Convert observation date to date format
data$observation_date <- as.Date(data$observation_date, format="%m/%d/%y")

# Define recession periods
gfc_period <- data %>% filter(observation_date >= "2007-10-01" & observation_date <= "2009-09-30")
covid_period <- data %>% filter(observation_date >= "2020-01-01" & observation_date <= "2020-06-30")

# Define pre-recession and post-recession periods
pre_gfc <- data %>% filter(observation_date >= "2004-01-01" & observation_date < "2007-10-01")
post_gfc <- data %>% filter(observation_date > "2009-09-30" & observation_date <= "2012-12-31")

pre_covid <- data %>% filter(observation_date >= "2017-01-01" & observation_date < "2020-01-01")
post_covid <- data %>% filter(observation_date > "2020-06-30" & observation_date <= "2022-12-31")

# Plot GDP 
ggplot(data, aes(x=observation_date, y=GDP)) +
  geom_line(color="blue") +
  geom_vline(xintercept=as.Date(c("2008-01-01", "2009-09-30")), linetype="dashed", color="red") +
  geom_vline(xintercept=as.Date(c("2020-01-01", "2020-06-30")), linetype="dashed", color="green") +
  labs(title="GDP Trends Before, During, and After Recessions", x="Year", y="GDP") +
  theme_minimal()

# Plot Unemployment
ggplot(data, aes(x=observation_date, y=Unemployment)) +
  geom_line(color="darkred") +
  geom_vline(xintercept=as.Date(c("2008-01-01", "2009-09-30")), linetype="dashed", color="red") +
  geom_vline(xintercept=as.Date(c("2020-01-01", "2020-06-30")), linetype="dashed", color="green") +
  labs(title="Unemployment Rate Before, During, and After Recessions", x="Year", y="Unemployment Rate (%)") +
  theme_minimal()

#Summary Statistics for GDP and Unemployment During Crises
gfc_summary <- gfc_period %>% summarize(
  avg_GDP = mean(GDP, na.rm = TRUE),
  min_GDP = min(GDP, na.rm = TRUE),
  max_GDP = max(GDP, na.rm = TRUE),
  avg_Unemployment = mean(Unemployment, na.rm = TRUE),
  min_Unemployment = min(Unemployment, na.rm = TRUE),
  max_Unemployment = max(Unemployment, na.rm = TRUE)
)

covid_summary <- covid_period %>% summarize(
  avg_GDP = mean(GDP, na.rm = TRUE),
  min_GDP = min(GDP, na.rm = TRUE),
  max_GDP = max(GDP, na.rm = TRUE),
  avg_Unemployment = mean(Unemployment, na.rm = TRUE),
  min_Unemployment = min(Unemployment, na.rm = TRUE),
  max_Unemployment = max(Unemployment, na.rm = TRUE)
)

print(gfc_summary)

print(covid_summary)

# Plot 10 Year yield
ggplot(data, aes(x=observation_date, y=X10YearYield)) +
  geom_line(color="purple") +
  geom_vline(xintercept=as.Date(c("2008-01-01", "2009-09-30")), linetype="dashed", color="red") +
  geom_vline(xintercept=as.Date(c("2020-01-01", "2020-06-30")), linetype="dashed", color="green") +
  labs(title="10-Year Government Bond Yield Trends", x="Year", y="Yield (%)") +
  theme_minimal()

# Plot CPI inflation
ggplot(data, aes(x=observation_date, y=CPI)) +
  geom_line(color="orange") +
  geom_vline(xintercept=as.Date(c("2008-01-01", "2009-09-30")), linetype="dashed", color="red") +
  geom_vline(xintercept=as.Date(c("2020-01-01", "2020-06-30")), linetype="dashed", color="green") +
  labs(title="Consumer Price Index (CPI) Trends", x="Year", y="CPI") +
  theme_minimal()


#Part 3: Firm-Level Responses Using WRDS Compustat Data
# Load data
compustat_revenue <- read.csv("compustat revenue.csv")
compustat_ratios <- read.csv("ratios compustat.csv")

# Convert date columns to date format
compustat_revenue$qdate <- as.Date(compustat_revenue$qdate, format="%m/%d/%y")
compustat_ratios$qdate <- as.Date(compustat_ratios$qdate, format="%m/%d/%y")

# Filter for companies
companies <- c("PG", "MS", "KR")
df_revenue <- compustat_revenue %>% filter(TICKER %in% companies)
df_ratios <- compustat_ratios %>% filter(TICKER %in% companies)

# Compute revenue growth for each company
df_revenue <- df_revenue %>%
  arrange(TICKER, qdate) %>%
  group_by(TICKER) %>%
  mutate(revenue_growth = (revtq - lag(revtq)) / lag(revtq) * 100) %>%
  ungroup()

# Define recession period (2007 Q4 – 2009 Q3)
recession_start <- as.Date("2007-10-01")
recession_end <- as.Date("2009-09-30")

#Plot variable trends over time

#Revenue Growth
ggplot(df_revenue, aes(x = qdate, y = revenue_growth, color = TICKER)) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.numeric(recession_start), linetype = "dotted", color = "red") +
  geom_vline(xintercept = as.numeric(recession_end), linetype = "dotted", color = "red") +
  labs(title = "Revenue Growth Over Time for PG, MS, and KR",
       x = "Year", y = "Revenue Growth (%)") +
  theme_minimal()

#Revenue
ggplot(df_revenue, aes(x = qdate, y = revtq, color = TICKER)) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.numeric(recession_start), linetype = "dotted", color = "red") +
  geom_vline(xintercept = as.numeric(recession_end), linetype = "dotted", color = "red") +
  labs(title = "Revenue Over Time for PG, MS, and KR",
       x = "Year", y = "Revenue") +
  theme_minimal()

#Function to plot financial ratios for each company
plot_ratio <- function(data, ratio_var, title, y_label) {
  ggplot(data, aes(x = qdate, y = .data[[ratio_var]], color = TICKER)) +
    geom_line(size = 1) +
    geom_vline(xintercept = as.numeric(recession_start), linetype = "dotted", color = "red") +
    geom_vline(xintercept = as.numeric(recession_end), linetype = "dotted", color = "red") +
    labs(title = title, x = "Year", y = y_label) +
    theme_minimal()
}

# plots for ratio data
plot_ratio(compustat_ratios, "aftret_invcapx", "Return on Invested Capital Over Time", "Return on Invested Capital")
plot_ratio(compustat_ratios, "de_ratio", "Debt-to-Equity Ratio Over Time", "Debt-to-Equity Ratio")
plot_ratio(compustat_ratios, "roa", "Return on Assets (ROA) Over Time", "ROA (%)")

#summary stats to company data across companies and time lines
#assigning periods
compustat_ratios <- compustat_ratios %>%
  mutate(GFC_Period = case_when(
    qdate < as.Date("2007-10-01") ~ "Pre-GFC",
    qdate >= as.Date("2007-10-01") & qdate <= as.Date("2009-09-30") ~ "During-GFC",
    qdate > as.Date("2009-09-30") ~ "Post-GFC"
  ))
compustat_revenue <- compustat_revenue %>%
  mutate(GFC_Period = case_when(
    qdate < as.Date("2007-10-01") ~ "Pre-GFC",
    qdate >= as.Date("2007-10-01") & qdate <= as.Date("2009-09-30") ~ "During-GFC",
    qdate > as.Date("2009-09-30") ~ "Post-GFC"
  ))
#summary statistics by company and period
summary_stats <- compustat_ratios %>%
  group_by(TICKER, GFC_Period) %>%
  summarise(
    avg_investment_intensity = mean(aftret_invcapx, na.rm = TRUE),
    avg_debt_to_equity = mean(de_ratio, na.rm = TRUE),
    avg_return_on_assets = mean(roa, na.rm = TRUE)
  ) %>%
  ungroup()
print(summary_stats)

summary_stats2 <- compustat_revenue %>%
  group_by(TICKER, GFC_Period) %>%
  summarise(
    avgqrtlyrev = mean(revtq, na.rm = TRUE)
  ) %>%
  ungroup()
print(summary_stats2)
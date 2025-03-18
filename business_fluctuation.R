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
debteq_roe_roa <- read.csv("debteq_roe_roa.csv")
fundamentals_quarterly <- read.csv("other dta.csv")  # Renaming other data

# Convert to Date format
debteq_roe_roa$qdate <- as.Date(debteq_roe_roa$qdate, format="%m/%d/%y")
fundamentals_quarterly$datadate <- as.Date(fundamentals_quarterly$datadate, format="%m/%d/%y")

# Filter data to include only up to 2011 so there isn;t too mich post GFC data
debteq_roe_roa <- debteq_roe_roa %>% filter(qdate <= "2011-12-31")
fundamentals_quarterly <- fundamentals_quarterly %>% filter(datadate <= "2011-12-31")

# Merge datasets on matching dates because data was downloaded form 2 different queries
firm_data <- merge(debteq_roe_roa, fundamentals_quarterly, by.x = "qdate", by.y = "datadate", all = TRUE)

# Compute Firm-Level Indicators
firm_data <- firm_data %>%
  arrange(TICKER, qdate) %>%
  group_by(TICKER) %>%
  mutate(
    revenue_growth = (revtq - lag(revtq)) / lag(revtq) * 100,  # Revenue Growth %
    investment_intensity = capxy / atq * 100,                  # Investment as % of Assets
    return_on_assets = roa                                     # Return on Assets (ROA)
  ) %>%
  ungroup()

# Define GFC period (2007 Q4 – 2009 Q3)
gfc_period <- firm_data %>%
  filter(qdate >= "2007-10-01" & qdate <= "2009-09-30")

# Summary Statistics Before, During, and After GFC
summary_stats <- firm_data %>%
  mutate(period = case_when(
    qdate < "2007-10-01" ~ "Pre-GFC",
    qdate >= "2007-10-01" & qdate <= "2009-09-30" ~ "GFC",
    qdate > "2009-09-30" ~ "Post-GFC"
  )) %>%
  group_by(period) %>%
  summarize(
    avg_revenue_growth = mean(revenue_growth, na.rm = TRUE),
    avg_investment_intensity = mean(investment_intensity, na.rm = TRUE),
    avg_debt_to_equity = mean(de_ratio, na.rm = TRUE),
    avg_return_on_assets = mean(return_on_assets, na.rm = TRUE)
  )

print(summary_stats)

# Visualizing variable trends over time

# Define recession start and end dates for graph visualization
recession_start <- as.Date("2007-10-01")
recession_end <- as.Date("2009-09-30")

# Revenue Growth 
ggplot(firm_data, aes(x = qdate, y = revenue_growth)) +
  geom_line(color = "blue") +
  geom_vline(xintercept = as.numeric(recession_start), linetype = "dotted", color = "red") +
  geom_vline(xintercept = as.numeric(recession_end), linetype = "dotted", color = "red") +
  labs(title = "Revenue Growth Over Time", x = "Year", y = "Revenue Growth (%)") +
  theme_minimal()

# Investment Intensity
ggplot(firm_data, aes(x = qdate, y = investment_intensity)) +
  geom_line(color = "green") +
  geom_vline(xintercept = as.numeric(recession_start), linetype = "dotted", color = "red") +
  geom_vline(xintercept = as.numeric(recession_end), linetype = "dotted", color = "red") +
  labs(title = "Investment Intensity Over Time", x = "Year", y = "Investment as % of Assets") +
  theme_minimal()

# DE Ratio
ggplot(firm_data, aes(x = qdate, y = de_ratio)) +
  geom_line(color = "red") +
  geom_vline(xintercept = as.numeric(recession_start), linetype = "dotted", color = "black") +
  geom_vline(xintercept = as.numeric(recession_end), linetype = "dotted", color = "black") +
  labs(title = "Debt-to-Equity Ratio Over Time", x = "Year", y = "Debt-to-Equity Ratio") +
  theme_minimal()

# ROA
ggplot(firm_data, aes(x = qdate, y = return_on_assets)) +
  geom_line(color = "purple") +
  geom_vline(xintercept = as.numeric(recession_start), linetype = "dotted", color = "black") +
  geom_vline(xintercept = as.numeric(recession_end), linetype = "dotted", color = "black") +
  labs(title = "Return on Assets (ROA) Over Time", x = "Year", y = "ROA (%)") +
  theme_minimal()

#Revenue
ggplot(firm_data, aes(x = qdate, y = revtq)) +
  geom_line(color = "blue") +
  geom_vline(xintercept = as.numeric(recession_start), linetype = "dotted", color = "red") +
  geom_vline(xintercept = as.numeric(recession_end), linetype = "dotted", color = "red") +
  labs(title = "Revenue Over Time", x = "Year", y = "Revenue") +
  theme_minimal()


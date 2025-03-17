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

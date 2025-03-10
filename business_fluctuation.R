#Project 3 - Business Fluctuations and Firm-Level Responses to Economic Crises
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lmtest)

data <- read_csv("~/OneDrive - University of Cincinnati/Macro/Project 3/cycles_data.csv")

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
    Trade_Germany_vol = sd(Expend_Germany_diff, na.rm = TRUE),
    
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



# cross-correlations between GDP time series of different countries
# removed missing values from GDP data before using ccf()
df_clean <- df_diff %>% drop_na(GDP_Canada_diff, GDP_USA_diff, GDP_Germany_diff)

# Compute cross-correlations 
ccf(df_clean$GDP_Canada_diff, df_clean$GDP_USA_diff, main="Canada vs USA GDP Cross-Correlation")
ccf(df_clean$GDP_Canada_diff, df_clean$GDP_Germany_diff, main="Canada vs Germany GDP Cross-Correlation")
ccf(df_clean$GDP_USA_diff, df_clean$GDP_Germany_diff, main="USA vs Germany GDP Cross-Correlation")


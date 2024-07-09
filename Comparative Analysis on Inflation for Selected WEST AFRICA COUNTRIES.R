library(tidyverse)
library(ggplot2)

# Data Importation
Inflation = read.csv("C:/Users/hp/Downloads/Inflation_Across_Countries.csv")
GDP = read.csv("C:/Users/hp/Downloads/GDP Across Countries.csv")
Population = read.csv("C:/Users/hp/Downloads/Population_Across_Countries.csv")

view(Inflation)
view(GDP)
view(Population)

# Data Cleaning 
# un pivoting GDP and Population column to match Inflation table for effective Data Modelling 
GDP2 = GDP %>% # convert columns to rows for population table. 
  pivot_longer(cols = starts_with("X20"),
               names_to = "Year", 
               values_to = "GDP_per_capita_growth")

population2 = Population %>%  # convert columns to rows for population table
  pivot_longer(cols = starts_with("X20"),
               names_to = "Year",
               values_to = "Percentage Annual Population Growth")


# Data Wrangling 
# 1: Sub-setting selected countries 
# 2: Check for inconsistencies 
# 3: Check for null values 
# 4: Dealing with datatypes 
# 5: Remove unwanted columns


# Filter tables according to selected countries
GDP2_filtered <- GDP2 %>% # filter for GDP
  filter(Country.Name %in% c("Cameroon", "Chad", "Gambia, The", "Guinea-Bissau", "Liberia", "Mali", "Nigeria"))
view(GDP2_filtered)

population2_filtered <- population2 %>% # filter for population
  filter(Country.Name %in% c("Cameroon", "Chad", "Gambia, The", "Guinea-Bissau", "Liberia", "Mali", "Nigeria"))

inflation_filtered <- Inflation %>% # filter for inflation
  filter(country %in% c("Cameroon", "Chad", "Gambia, The", "Guinea-Bissau", "Liberia", "Mali", "Nigeria")) 


# Deal with inconsistencies in the data set
# Remove x from the year column for both gdp and population column using gsub
GDP2_filtered$Year = gsub("^X", "", GDP2_filtered$Year)
population2_filtered$Year = gsub("^X", "", population2_filtered$Year)


# Remove irrelevant columns using select function
GDP2_filtered=GDP2_filtered %>%
  select(-Indicator.Name)

population2_filtered=population2_filtered %>%
  select(-Indicator.Name, -Indicator.Code)



# clean column headers
GDP2_filtered <- clean_names(GDP2_filtered)
population2_filtered <-  clean_names(population2_filtered)
inflation_filtered <- clean_names(inflation_filtered)

# check columns
view(GDP2_filtered)
view(population2_filtered)
view(inflation_filtered)

# Dealing with the year column in the GDP and Population
GDP2_filtered$year <- ymd(paste0(GDP2_filtered$year, "-01-01"))
GDP2_filtered$year <- year(GDP2_filtered$year)

population2_filtered$year <- ymd(paste0(population2_filtered$year, "-01-01"))
population2_filtered$year <- year(population2_filtered$year)

inflation_filtered$date <- trimws(inflation_filtered$date)
str(inflation_filtered)


inflation_filtered$date <-  as.Date(inflation_filtered$date, format = "%d/%m/%Y")
inflation_filtered$year <- year(inflation_filtered$date)
inflation_filtered$month <- factor(month(inflation_filtered$date), 
                                   levels = 1:12, 
                                   labels = month.abb[1:12])
inflation_filtered$day <- day(inflation_filtered$date)

# drop date and day column as they are not needed
inflation_filtered <- inflation_filtered%>%
  select(-date, -day)

#fill null values for the inflation column: open,high,low,close:using interpolate
inflation_filtered <-  inflation_filtered %>%
  filter(!is.na(Open)) %>%
  filter(!is.na(High)) %>%
  filter(!is.na(Low)) %>%
  filter(!is.na(Close)) %>%
  filter(!is.na(Inflation)) %>%

# Objective 1: Understanding inflation trend across selected countries
# price movement analysis: Price Fluctuation
# Correlation between stock market price fluctuation and inflation
# Identify periods of highly and low inflation
# Volatility of Inflation: How much inflation fluctuate month to month
# "Inflation" refers to the percentage change in the general price level of goods and services over a specific period.

# Show overall inflation by countries 
ggplot(inflation_filtered) +
aes(x = country, y = inflation) +
geom_bar(stat = "summary", fun = "mean", 
fill = "#112446") +
labs(title = "Mean Distribution of Inflation by Countries") +
theme_minimal()


# Understand the Inflation by year to gain insight into period of high and low inflation
inflatio_Trend <-  ggplot(inflation_filtered, aes(x = year, y = inflation, color = country)) +
                   geom_smooth(method = "loess", se = FALSE, linewidth = 0.5) +
                  labs(title = "Inflation Trend Across Years Shown for Selected Countries") +
                  facet_wrap(~ country, scales = "free_y")
inflatio_Trend
ggplotly(inflatio_Trend)

# calculate price movement : Engineer a feature. 
inflation_filtered$monthly_stock_price_change <-  inflation_filtered$close - inflation_filtered$open


# Correlation between stock market price fluctuation and inflation
ggplot(inflation_filtered) +
  aes(x = monthly_stock_price_change, y = inflation, fill = country, colour = country) +
  geom_point(size = 1.05, shape = "bullet") +
  geom_jitter() +
  geom_smooth(method = lm, se = FALSE) +
  scale_fill_viridis_d(option = "cividis",
                       direction = -1) +
  scale_color_viridis_d(option = "cividis", direction = -1) +
  facet_wrap(~ country, scales = "free_y")
  theme_minimal()


# carry out hypothesis test on relationship between price fluctuation and inflation 
# Null hypothesis: There is no significant impact of price fluctuation on Inflation
# alpha = 0.05
summary(lm(inflation_filtered$inflation~inflation_filtered$monthly_stock_price_change))

# with a p value of 0.000000009917, we reject the null hypothesis, and there is a significant impact of stock market price fluctuation on Inflation

# Considering other parameters, and checking relationships between parameters
# Understanding GDP across years for different countries 
esquisse::esquisser(GDP2_filtered)

library(ggplot2)

ggplot(GDP2_filtered) +
 aes(x = year, y = gdp_per_capita_growth, colour = country_name) +
 geom_line() +
 scale_color_hue(direction = 1) +
  labs(title = "GDP Across Years Shown for Selected Countries") +
  facet_wrap(~ country_name, scales = "free_y")

# Investigating impact of GDP on Inflation

inflation_filtered3 <-  inflation_filtered %>% filter(month == "Dec") # filter the inflation dataset for December: Since its a cumulative one
view(inflation_filtered3)

#inflation_filtered3 <- na.omit(inflation_filtered3)


GDP2_filtered <- GDP2_filtered %>% rename(country = country_name)

GDP_inflation2 <-  merge(inflation_filtered3, GDP2_filtered, by = c("country", "year"))
view(GDP_inflation2)

# check correlation between inflation and GDP
ggplot(GDP_inflation2) +
  aes(x = monthly_stock_price_change, y = inflation, fill = country, colour = country) +
  geom_point(size = 1.05, shape = "bullet") +
  geom_jitter() +
  geom_smooth(method = lm, se = FALSE) +
  scale_fill_viridis_d(option = "cividis",
                       direction = -1) +
  scale_color_viridis_d(option = "cividis", direction = -1) +
  facet_wrap(~ country, scales = "free_y") +
  labs(title = "Correlation Between GDP and Inflation for Different Countries")
theme_minimal()

# check correlation for Nigeria
GDP_inflation3 <-  GDP_inflation2 %>% filter(country == "Nigeria")
GDP_inflation3 <- na.omit(GDP_inflation3)
view(GDP_inflation3)
cor(GDP_inflation3$inflation, GDP_inflation3$gdp_per_capita_growth) # calculates correlation coeff for Nigeria


# carry out hypothesis testing on GDP and Inflation
GDP_inflation4 <- na.omit(GDP_inflation2)
view(GDP_inflation4)

summary(lm(GDP_inflation4$inflation~GDP_inflation4$gdp_per_capita_growth))

# Analysis on Population
esquisse::esquisser(population2_filtered)

library(ggplot2)

ggplot(population2_filtered) +
 aes(x = country_name, y = percentage_annual_population_growth) +
 geom_bar(stat = "summary", 
 fun = "mean", fill = "#112446") +
 coord_polar(theta = "x") +
  labs(title = "Understanding Overall Population Growth Rate Across Countries")
 theme_minimal()
 
# Population growth rate trend by countries
 ggplot(population2_filtered) +
   aes(x = year, y = percentage_annual_population_growth, colour = country_name) +
   geom_line() +
   scale_color_hue(direction = 1) +
   labs(title = "Annual Population Growth Rate Across Years Shown for Selected Countries") +
   facet_wrap(~ country_name, scales = "free_y")

# Relationship Between Population and Inflation
 population2_filtered <- population2_filtered %>% rename(country = country_name)
 
 population3 <-  merge(inflation_filtered3, population2_filtered, by = c("country", "year"))
 view(population3)
 
 population4 <- na.omit(population3)
view(population4) 

ggplot(population4) +
  aes(x = percentage_annual_population_growth, y = inflation, fill = country, colour = country) +
  geom_point(size = 1.05, shape = "bullet") +
  geom_jitter() +
  geom_smooth(method = lm, se = FALSE) +
  scale_fill_viridis_d(option = "cividis",
                       direction = -1) +
  scale_color_viridis_d(option = "cividis", direction = -1) +
  facet_wrap(~ country, scales = "free_y") +
  labs(title = "Correlation Between Population Growth Rate and Inflation for Different Countries")
theme_minimal()

cor(population4$inflation, population4$percentage_annual_population_growth) # calculates correlation coeff for Nigeria

summary(lm(population4$inflation~population4$percentage_annual_population_growth))

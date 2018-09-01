library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

# Input -------------------------------------------------------------------
data = read.csv('gun-violence-data_01-2013_03-2018.csv')


# Understand data ---------------------------------------------------------
glimpse(data)

count_unique = data.frame()

for (i in 1:ncol(data)) {
  count_unique[i,1] = sum(!is.na(data[,i]))
  count_unique[i,2] = round(sum(!is.na(data[,i])) / nrow(data) * 100,3)
  count_unique[i,3] = length(unique(data[,i]))
}
rownames(count_unique) = colnames(data)
colnames(count_unique) = c("Total records", "% populated", "Unique values")

?month
# Date --------------------------------------------------------------------
data$date = as.Date(data$date)
dqrDate = data %>%
  group_by(date=round_date(date,'month')) %>%
  summarize(count = n()) %>%
  arrange(desc(date))

head(dqrDate)
tail(dqrDate)
ggplot(dqrDate,aes(date,count)) +
  geom_line(group=1)
  
# State -------------------------------------------------------------------
dqrState = data %>%
  group_by(state) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

head(dqrState,20)
tail(dqrState,20)

# City/County -------------------------------------------------------------
dqrCity = data %>%
  group_by(city_or_county) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

head(dqrCity,20)
tail(dqrCity,20)

# Address -----------------------------------------------------------------
dqrAddress = data %>%
  group_by(address) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

head(dqrAddress,20)
tail(dqrAddress,20)

data %>%
  filter(address == '2375 International Pkwy') %>%
  arrange(desc(date)) %>%
  select(date)

# number of killed --------------------------------------------------------
data %>%
  ggplot(aes(x=1,y=n_killed)) +
  geom_boxplot()

# number of injured -------------------------------------------------------


# gun stolen --------------------------------------------------------------
dqrAddress = data %>%
  group_by(address) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

head(dqrAddress,20)
tail(dqrAddress,20)

# gun type ----------------------------------------------------------------
dqrGunType = data %>%
  group_by(gun_type) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

head(dqrGunType,20)
tail(dqrGunType,20)

# incident characteristics ------------------------------------------------


# location description ----------------------------------------------------


# number of guns involved -------------------------------------------------


# participant age ---------------------------------------------------------


# participant age group ---------------------------------------------------


# participant gender ------------------------------------------------------


# participant name --------------------------------------------------------


# participant relationship ------------------------------------------------


# participant status ------------------------------------------------------


# participant  type -------------------------------------------------------





# Strategy:
# 1. Explore + understand the data
# 2. Experiment + create raw charts
# 3. Theme-tify all charts
# 4. Structure + add wordings

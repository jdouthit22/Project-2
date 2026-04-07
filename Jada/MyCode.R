library(dplyr)
library(tidyverse)
library(ggplot2)
SCD = read.csv("SyntheticClinicalData copy.csv")
Hi

Race = SCD$race
print(Race)


Pie Chart

mean(SCD$bmi)


White = filter(SCD, Race == "White")


#Bar chart, show percentage of total patients vs. BMI (background)
Race = select(SCD, race, bmi)
Race_table = table(SCD$race)
unique(SCD$race)
Race_table = table(SCD$race)
view(Race_table)

#convert to percentages
race_data <- SCD %>%
  count(race) %>%
  mutate(
    percent = (n / sum(n)) * 100)

race_data |>
  ggplot(aes(y = race, x = percent, fill = race)) +
  geom_col()



#percentage of sex 


#plotting the ages of patients (scatterplots)
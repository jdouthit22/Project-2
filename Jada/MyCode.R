library(dplyr)
library(tidyverse)
library(ggplot2)
SCD = read.csv("/Volumes/T9/Schoolwork/Project-2/SyntheticClinicalData copy.csv")


Race = SCD$race
print(Race)


#Pie Chart

mean(SCD$bmi)


White = filter(SCD, Race == "White")


#Bar chart, show percentage   of total patients vs. BMI (background)
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


#percentage of sex & smoking status
sex_table = table(SCD$gender)
view(sex_table)
race_new |>
  count(smoking_status, gender) |>
  ggplot(aes(x = smoking_status, y = n, 
             fill = gender)) +
  geom_col(position = "fill")

unique(SCD$gender)



#plotting the ages of patients (scatterplots)
race_new <- mutate(SCD,
                   age = 2026 - birth_year)

Race = select(race_new, race, age)


race_new$age_group <- cut(
  race_new$age,
  breaks = seq(0, 100, by = 10),
  right = FALSE
)

#get different races
unique(SCD$race)

#Line chart Comparison WvB
race_new |>
  filter(race %in% c(
    "White","Black or African American")) |>
  ggplot(aes(x = age, y = bmi, color = race)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "BMI vs Age by Race",
    x = "Age",
    y = "Average BMI"
  ) +
  theme(legend.position = "bottom")

#All races
race_new |>
  filter(race %in% c(
    "White","Black or African American",
    "Asian",
    "White",
    "Two or More Races",
    "American Indian or Alaska Native",
    "Unknown/Other",
    "Native Hawaiian or Pacific Islander")) |>
  ggplot(aes(x = age, y = bmi, color = race)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  labs(
    title = "BMI vs Age by Race",
    x = "Age",
    y = "Average BMI"
  ) +
  theme(legend.position = "bottom")


library(dplyr)
library(tidyverse)
library(ggplot2)
SCD = read.csv("/Volumes/T9/Schoolwork/Project-2/SyntheticClinicalData copy.csv")


Race = SCD$race
print(Race)


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
unique(SCD$gender)

sex_table = table(SCD$gender)
view(sex_table)
race_new |>
  count(smoking_status, gender) |>
  ggplot(aes(x = smoking_status, y = n, 
             fill = gender)) +
  geom_col() +
  labs(
    title = "Gender vs. Smoking Status",
    x = "Smoking Status",
    y = "count"
  ) +
  theme(legend.position = "bottom")




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

#highest and lowest by race

# White
# 51.7
# 68
Highest_White <- race_new %>%
  select(race,bmi, age) %>%
  filter(race == "White") %>%
  arrange(desc(bmi)) %>%
  head(1)

# race: White
# bmi: 12.8
# age: 55
Lowest_White <- race_new %>%
  select(race,bmi, age) %>%
  filter(race == "White") %>%
  arrange(bmi) %>%
  head(1)

# race: Black or African American
# bmi: 51.7
# age:82
Highest_Black <- race_new %>%
  select(race,bmi, age) %>%
  filter(race == "Black or African American") %>%
  arrange(desc(bmi)) %>%
  head(1)


# race: Black or African American
# bmi: 12.6
# age: 57
Lowest_Black <- race_new %>%
  select(race,bmi, age) %>%
  filter(race == "Black or African American") %>%
  arrange(bmi) %>%
  head(1)


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


library(GGally)
race_new |> 
  select(age, bmi) |> 
  ggpairs()

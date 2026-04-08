library(dplyr)
library(tidyverse)
library(ggplot2)
SCD = read.csv("/Volumes/T9/Schoolwork/Project-2/SyntheticClinicalData copy.csv")


Race = SCD$race
print(Race)


#Bar chart, show percentage   of total patients vs. BMI (background)
unique(SCD$race)
Race_table = table(SCD$race)

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
                   current_age = 2026 - birth_year)

#Line chart Comparison WvB
race_new |>
  filter(race %in% c(
    "White","Black or African American")) |>
  ggplot(aes(x = age_at_visit, y = bmi, color = race)) +
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
# 61
Highest_White <- race_new %>%
  select(race,bmi, age_at_visit) %>%
  filter(race == "White") %>%
  arrange(desc(bmi)) %>%
  head(1)

print(Highest_White)

# race: White
# bmi: 12.8
# age: 50
Lowest_White <- race_new %>%
  select(race,bmi, age_at_visit) %>%
  filter(race == "White") %>%
  arrange(bmi) %>%
  head(1)

print(Lowest_White)

# race: Black or African American
# bmi: 51.7
# age: 77
Highest_Black <- race_new %>%
  select(race,bmi, age_at_visit) %>%
  filter(race == "Black or African American") %>%
  arrange(desc(bmi)) %>%
  head(1)

print(Highest_Black)

# race: Black or African American
# bmi: 12.6
# age: 53
Lowest_Black <- race_new %>%
  select(race,bmi, age_at_visit) %>%
  filter(race == "Black or African American") %>%
  arrange(bmi) %>%
  head(1)

print(Lowest_Black)
#61 years old whites
WW <- race_new %>%
  filter(race == "White",
         smoking_status == "Never",
         age_at_visit == 61)

Alcohol <- WW %>%
  count(alcohol_use) %>%
  mutate(prop = n / sum(n),
         percent = prop * 100)

ggplot(Alcohol, aes(x = "", y = prop, fill = factor(alcohol_use))) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Alcohol Use Amongst 68 Years Old Whites",
    fill = "Alcohol Use"
  ) +
  theme_void()



PA <- WW %>%
  count(physical_activity_level) %>%
  mutate(prop = n / sum(n),
         percent = prop * 100)


WW |>
  count(alcohol_use, physical_activity_level) |>
  ggplot(aes(x = alcohol_use, y = n, 
             fill = physical_activity_level)) + 
  geom_col(position = "dodge")+
  labs(
    title = "Physical Activity by Alcohol Use",
    x = "Alcohol Use",
    y = "Proportion",
    fill = "Physical Activity"
  ) +
  theme_minimal()

#50 years old whites
W50 <- race_new %>%
  filter(race == "White",
         smoking_status == "Never",
         age_at_visit == 50)

A50 <- W50 %>%
  count(alcohol_use) %>%
  mutate(prop = n / sum(n),
         percent = prop * 100)

PA50 <- W50 %>%
  count(physical_activity_level) %>%
  mutate(prop = n / sum(n),
         percent = prop * 100)


W50 |>
  count(alcohol_use, physical_activity_level) |>
  ggplot(aes(x = alcohol_use, y = n, 
             fill = physical_activity_level)) + 
  geom_col(position = "dodge")+
  labs(
    title = "Physical Activity by Alcohol Use",
    x = "Alcohol Use",
    y = "Proportion",
    fill = "Physical Activity"
  ) +
  theme_minimal()


nrow(WW)


race_new

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

#Profession by pain score
# reorder changes order of bar chart, not arrange
pp_data <- SCD %>%
  group_by(profession) %>%
  summarise(avg_pain = mean(patient_reported_pain_score, na.rm = TRUE)) %>%
  arrange(desc(avg_pain))

ggplot(plot_data, aes(x = reorder(profession, avg_pain), y = avg_pain, fill = avg_pain)) +
  geom_col() +
  scale_fill_gradient(low = "lightblue", high = "darkred") +
  labs(
    title = "Average Pain Score for Top 5 Professions",
    x = "Profession",
    y = "Average Pain Score"
  ) +
  coord_flip()

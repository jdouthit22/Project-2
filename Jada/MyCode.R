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

Current_smokers <- race_new %>%
  filter(smoking_status = "current")

race_new %>%
  filter(smoking_status == "Current") %>%
  ggplot(aes(x = age, y = bmi)) +
  geom_point(color = "darkred", size = 4, alpha = 0.5) +
  geom_smooth(method = "lm", linewidth = 2)



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

#68 years old whites
WW <- race_new %>%
  filter(race == "White",
         smoking_status == "Never",
         age == 68)

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
WW$physical_activity_level

PA <- WW %>%
  count(physical_activity_level) %>%
  mutate(prop = n / sum(n),
         percent = prop * 100)

ggplot(PA, aes(x = "", y = prop, fill = factor(physical_activity_level))) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Physical Activity Amongst 68 Years Old Whites",
    fill = "Physical Activity"
  ) +
  theme_void()

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

#55 years old whites
W55 <- race_new %>%
  filter(race == "White",
         smoking_status == "Never",
         age == 55)

A55 <- W55 %>%
  count(alcohol_use) %>%
  mutate(prop = n / sum(n),
         percent = prop * 100)

ggplot(A55, aes(x = "", y = prop, fill = factor(alcohol_use))) +
  geom_col (width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Alcohol Use Amongst 55 Years Old Whites",
    fill = "Alcohol Use"
  ) +
  theme_void()
WW$physical_activity_level

PA55 <- W55 %>%
  count(physical_activity_level) %>%
  mutate(prop = n / sum(n),
         percent = prop * 100)

ggplot(PA55, aes(x = "", y = prop, fill = factor(physical_activity_level))) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Physical Activity Amongst 55 Years Old Whites",
    fill = "Physical Activity"
  ) +
  theme_void()

W55 |>
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


library(GGally)
race_new |> 
  select(age, bmi) |> 
  ggpairs()

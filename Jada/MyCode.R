library(dplyr)
library(tidyverse)
SCD = read.csv("SyntheticClinicalData copy.csv")
Hi

Race = SCD$race
print(Race)


Pie Chart

mean(SCD$bmi)


White = filter(SCD, Race == "White")


#Pie chart, show percentage of total patients vs. BMI (background)
Race = select(SCD, race, bmi)
Race_table = table(SCD$race)
unique(SCD$race)
Race_table = table(SCD$race)
view(Race_table)

plot_data <-Race %>% filter(szip5 %in% c("27801", "27886")) %>% count(szip5, writ) %>% group_by(szip5) %>% mutate(prop = n / sum(n), label = scales::percent(prop))

my_plot <- plot_data %>%
  ggplot(aes(x = "", y = prop, fill = writ)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ szip5) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(
    fill = "Writ of Possession",
    title = "Writ of Possession Distribution by ZIP Code"
  ) +
  theme_void() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 20)),
    plot.title.position = "plot"
  )



#percentage of sex 


#plotting the ages of patients (scatterplots)
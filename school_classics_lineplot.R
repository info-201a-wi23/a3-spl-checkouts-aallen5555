source("summary.R")
library("dplyr")
library("stringr")
library("ggplot2")

#sort high school classics (hamlet, macbeth, romeo and juliet)
school_df <- spl_df %>% filter(StandardizedTitle %in% c("hamlet", "macbeth", "romeo and juliet"))

school_df_2020_2023 <- school_df %>% filter(CheckoutYear %in% c(2020, 2021, 2022, 2023))

school_checkouts_per_month <- school_df_2020_2023 %>% group_by(date, StandardizedTitle) %>% summarize(sum_checkouts = sum(Checkouts))

ggplot(data = school_checkouts_per_month) +
  geom_line(
    mapping = aes(x = date, y = sum_checkouts, color = StandardizedTitle)
  ) +
  labs(
    title = "High School Classics during Covid",
    x = "Date",
    y = "Number of Checkouts", 
    color = "Title" 
  ) +
  scale_color_brewer(palette = "Accent")

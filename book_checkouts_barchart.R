source("summary.R")
library("dplyr")
library("stringr")
library("ggplot2")

#sort all plays
plays_df <- spl_df %>% filter(StandardizedTitle %in% c("all's well that ends well", "as you like it","love's labor's lost", "measure for measure", "the merchant of venice", "the merry wives of windsor", "a midsummer night's dream", "much ado about nothing", "pericles", "the taming of the shrew", "the tempest", "twelfth night", "the two gentlemen of verona", "the winter’s tale", "antony and cleopatra", "coriolanus", "cymbeline", "hamlet", "julius caesar", "king lear", "macbeth", "othello", "romeo and juliet", "timon of athens", "titus andronicus", "troilus and cressida"))

#find sum
total_plays <- plays_df %>% group_by(StandardizedTitle) %>% summarize(sum_checkouts = sum(Checkouts))

#bar plot
ggplot(data = total_plays) +
  geom_col(
    mapping = aes(x = sum_checkouts, y = reorder(StandardizedTitle, -sum_checkouts), fill = sum_checkouts)
  ) +
  labs(
    title = "Checkouts per Play (Comedy and Tragedy)",
    x = "Checkouts",
    y = "Play Title",
    fill = "Checkouts"
  )

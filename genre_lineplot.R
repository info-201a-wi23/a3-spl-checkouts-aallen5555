source("summary.R")
library("dplyr")
library("stringr")
library("ggplot2")

#sort into tragedy, comedy, history, other
spl_df <- spl_df %>% mutate(Genre = StandardizedTitle)

spl_df$Genre[str_detect(spl_df$Genre, "all's well that ends well|as you like it|comedy|comedies|love's labor's lost|measure for measure|the merchant of venice|the merry wives of windsor|a midsummer night's dream|much ado about nothing|pericles|the taming of the shrew|the tempest|twelfth night|the two gentlemen of verona|the winter’s tale")] <- "comedy"

spl_df$Genre[str_detect(spl_df$Genre, "antony and cleopatra|coriolanus|cymbeline|hamlet|julius caesar|king lear|macbeth|othello|romeo and juliet|timon of athens|titus andronicus|troilus and cressida|tragedy|tragedies")] <- "tragedy"

spl_df$Genre[str_detect(spl_df$Genre, "henry v|henry iv,  part 1|henry iv,  part 2|henry viii|king henry iv|william shakespeare's henry iv|henry iv, part 1 and 2|henry vi, part 3|henry vi, part 2|henry vi, part 1|king john|richard ii|richard iii")] <- "history"

spl_df$Genre[str_detect(spl_df$Genre, "the complete works|shakescenes|winter song|poems and sonnets|to sleep perchance to dream|shakespeare's insults|the reduced shakespeare company radio show|one hundred and eleven shakespeare monologues|bruce chatwin|the winter's tale|polnoe sobranie|sochinenii v des︠i︡ati tomakh|romances|a dictionary of quotations from shakespeare|all the world's a stage|the riverside shakespeare|in tasmania|the yale shakespeare|quennell|longman|shakespeare|knaves, knights, & kings|something rich and strange|the vision of elena silves|be thou now persuaded|the annotated shakespeare|speak the speech|sonety|soliloq|the two noble kinsmen|sonetos|comedias|a player's handbook of short scenes, selected and arr. by samuel selden|make 'em move|priscilla|yolo juliet|shashibiya|enrique iv|vivat rex|seven classic plays|dreams|fade out")] <- "other"

#sort high school classics (hamlet, macbeth, romeo and juliet)

genre_df <- spl_df %>% group_by(date, Genre) %>% summarize(sum_checkouts = sum(Checkouts))

ggplot(data = genre_df) +
  geom_line(
    mapping = aes(x = date, y = sum_checkouts, color = Genre)
  ) +
  labs(
    title = "Genre over time",
    x = "Date",
    y = "Number of Checkouts", 
    color = "Genre" 
  ) +
  scale_color_brewer(palette = "Dark2")
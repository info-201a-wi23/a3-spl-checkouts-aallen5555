library("dplyr")
library("stringr")

#read in csv
spl_df <- read.csv("shakespeare_checkouts.csv")

#combine month and year and read as dates into new column
spl_df <- spl_df %>% mutate(date = as.Date(paste0(CheckoutYear, "-", CheckoutMonth, "-01")))

#create a new dataframe of all total checkouts per month
checkouts_per_month <- spl_df %>% group_by(date) %>% summarize(sum_checkouts = sum(Checkouts))

#months with most and least total shakespeare checkouts
most_total_checkouts <- checkouts_per_month %>% summarize(max_checkouts = max(sum_checkouts))

least_total_checkouts <- checkouts_per_month %>% summarize(min_checkouts = min(sum_checkouts))

#create a new dataframe of all total checkouts by medium
checkouts_per_material <- spl_df %>% group_by(MaterialType) %>% summarize(sum_checkouts = sum(Checkouts))

#pull the max medium
most_checkouts_medium <- checkouts_per_material %>% summarize(max_checkouts = max(sum_checkouts))





#calculating most checked out book

#absolute mess in here, need to sort into unifying title

spl_df <- spl_df %>% mutate(StandardizedTitle = tolower(trimws(gsub("\\/.*|\\(.*|\\[.*|\\:.*", "", Title))))

#these are compilations, so this prevents them getting lumped in with other titles
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "classic bbc radio shakespeare, comedies")] <- "classic bbc radio shakespeare, comedies"
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "classic bbc radio shakespeare, tragedies")] <- "classic bbc radio shakespeare, tragedies"
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "classic bbc radio shakespeare, romances")] <- "classic bbc radio shakespeare, romances"

#sorting each title

#midsummer nights dream
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "midsummer|midsommer")] <- "a midsummer night's dream"

#alls well that ends well
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "well that")] <- "all's well that ends well"

#antony and cleopatra
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "antony and cleopatra")] <- "antony and cleopatra"

#as you like it
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "as you like it")] <- "as you like it"

#coriolanus
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "coriolanus")] <- "coriolanus"

#cymbeline
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "cymbeline")] <- "cymbeline"

#hamlet
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "hamlet|hămlét")] <- "hamlet"

#henry iv,  part 1
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "henry iv, part i|henry iv  part 1|henry iv, part 1|henry iv, part one|henry the fourth, part i|henry the fourth, part one|the first part of king henry iv|the first part of the history of henry iv|the history of henry iv, part 1|henry iv. part 1|the first part of king henry the fourth")] <- "henry iv,  part 1"

#henry iv,  part 2
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "henry iv, part ii|henry iv  part 2|henry iv, part 2|henry iv, part two|henry the fourth, part ii|henry the fourth, part two|henry iv. part two|the second part of king henry iv|the second part of the history of henry iv|the second part of king henry the fourth")] <- "henry iv,  part 2"

#henry iv, part 1 and 2
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "henry iv. parts one and two")] <- "henry iv, part 1 and 2"

#henry iv: shadow of succession
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "henry iv: shadow|henry iv : the shadow")] <- "henry iv: shadow of succession"

#henry v
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "henry v|henry the fift")] <- "henry v"

#henry vi, part 1
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "henry vi part one|henry vi, part 1|	
henry vi, part one|henry vi. part one|the first part of king henry vi|the first part of king henry the sixth")] <- "henry vi, part 1"

#henry vi, part 2
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "henry vi part two|henry vi, part 2|	
henry vi, part two|henry vi. part two|the second part of king henry vi|the second part of king henry the sixth")] <- "henry vi, part 2"

#henry vi, part 3
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "henry vi part three|henry vi, part 3|	
henry vi, part three|henry vi. part three|the third part of king henry vi|the third part of king henry the sixth")] <- "henry vi, part 3"

#henry viii
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "henry viii|henry the eighth")] <- "henry viii"

#julius caesar
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "caesar|césar")] <- "julius caesar"

#king john
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "king john")] <- "king john"

#king lear
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "king lear|el rey lear")] <- "king lear"

#love's labor's lost
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "love's labor's lost|love's labour's lost")] <- "love's labor's lost"

#macbeth
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "macbeth")] <- "macbeth"

#measure for measure
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "measure for measure")] <- "measure for measure"

#the merchant of venice
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "merchant")] <- "the merchant of venice"

#the merry wives of windsor
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "windsor")] <- "the merry wives of windsor"

#much ado about nothing
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "nothing")] <- "much ado about nothing"

#othello
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "othello")] <- "othello"

#pericles
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "pericles")] <- "pericles"

#richard ii
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "richard ii|richard the second")] <- "richard ii"

#richard iii
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "richard iii|richard the third")] <- "richard iii"

#romeo and juliet
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "romeo and juliet|romeo & juliet|romeo y julieta")] <- "romeo and juliet"

#the taming of the shrew
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "shrew")] <- "the taming of the shrew"

#the tempest
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "tempest")] <- "the tempest"

#the comedy of errors
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "errors")] <- "the comedy of errors"

#timon of athens
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "athens")] <- "timon of athens"

#titus andronicus
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "titus")] <- "titus andronicus"

#troilus and cressida
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "troilus")] <- "troilus and cressida"

#twelfth night
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "twelfth")] <- "twelfth night"

#the two gentlemen of verona
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "verona")] <- "the two gentlemen of verona"

#the winter's tale
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "the winter")] <- "the winter's tale"

#the complete
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "the work|complete work|complete play")] <- "the complete works"

#poems and sonnets
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "poems|sonnets")] <- "poems and sonnets"

#a new variorum edition of shakespeare
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "a new variorum edition of shakespeare")] <- "a new variorum edition of shakespeare"

#an art edition of the most popular dramas of shakespeare
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "an art edition of the most popular dramas of shakespeare")] <- "an art edition of the most popular dramas of shakespeare"

#the mercury shakespeare
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "the mercury shakespeare")] <- "the mercury shakespeare"

#the comedies
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "the comedies")] <- "the comedies"

#the tragedies
spl_df$StandardizedTitle[str_detect(spl_df$StandardizedTitle, "the tragedies")] <- "the tragedies"





#done!
#now continuing on, filter by BOOK
books_df <- spl_df %>% filter(MaterialType == "BOOK")

#find sum of each book checkout
checkouts_per_book <- books_df %>% group_by(StandardizedTitle) %>% summarize(sum_checkouts = sum(Checkouts))

#find most checked out book
most_checkouts_book <- checkouts_per_book %>% summarize(max_checkouts = max(sum_checkouts))

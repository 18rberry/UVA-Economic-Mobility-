library(rvest)
library(dplyr)
library(tidyr)
library(readr)

score_card <- read_csv("~/git/TestDSPG/dspg20uvaEM/EM_gates/data/Final Scorecard - Sheet1.csv")
score_card
domain.scorecard <- function(domain) {
  score_card <- filter(score_card, Domain == domain)
  score_card <- score_card %>%
    group_by(Subdomain) %>%
    summarize(oregon_score = mean(Oregon), iowa_score = mean(Iowa), virginia_score = mean(Virginia))
  mean_row <- score_card %>%
    summarise_if(is.numeric, mean)
  mean_row <- mean_row[1 , ]

  score_card %>%
    add_row(mean_row)
}

housing <- domain.scorecard('Taxation')
housing

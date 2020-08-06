library(ggplot2)
library(dplyr)
library(readr)
library(rlist)
library(gridExtra)
library(grid)

setwd('~/git/TestDSPG/dspg20uvaEM/EM_gates/WWW')
source("~/git/TestDSPG/dspg20uvaEM/EM_gates/data/theme_SDAD.R")
source("~/git/TestDSPG/dspg20uvaEM/EM_gates/data/Colorblind_Palette.R")
score_card <- read_csv("~/git/dspg20uvaEM/EM_gates/data/Final Scorecard - Sheet2.csv")
score_card

schoolclimate_scores <- c(.4,.2,1,.875,.5,.875,1,.57,1,.67,.33,1,1,.5,1,1,.5,0,1,0,1)
childhoodeducation_scores <- c(.7,.25,.75,.6,.8,.2,.6,.4,1,1,1,.5,0,0,.5,1,1,1)
afford_scores <- c(1,.67,.67,1,.8,1,0,.5,1)
workforce_scores <- c(1, .67,.67,1,1,0,.5,.25,.75)

heatmap.for.subdomain <-function(domain, subdomain, scores) {
  subdomain_card <- score_card %>%
    filter(Domain == domain & Subdomain == subdomain)
  print(subdomain_card)
  question_list <- subdomain_card$Question
  nrows <- nrow(subdomain_card)
  print(nrows)
  new_dimension <- rep(subdomain, 3 * nrows)
  new_questions <- factor(c(1: nrows), labels = question_list)
  new_questions <- rep(new_questions, rep(3, nrows))
  States <- rep(c("VA", "IA", "OR"), nrows)
  States<-ordered(States, levels=c("VA","IA","OR"))
  #will need to manually input the scores
  Scores <- scores
  Scores<-ifelse(Scores == 1,"1", "<1")
  VOTE<-data.frame(new_dimension, new_questions, States, Scores)
  intercept <- nrows + 0.5

  ggplot(VOTE, aes(y=new_questions, x=States, fill=factor(Scores))) +
    geom_tile(color="grey90", lwd=1) +
    coord_equal() +
    theme_SDAD() +
    ggplot2::annotate("text", x=rep(c(1:3), length(unique(new_questions))),
                      y=rep(c(1:length(unique(new_questions))), each=3),
                      label=Scores) +
    scale_fill_manual(values=cbPalette[c(3,2,1)]) +
    xlab("") + ylab("") +
    geom_hline(yintercept= intercept, color=cbPalette[1], lwd=4) +
    ggplot2::annotate("text", x=2, y= intercept,label= subdomain, fontface=2, color="black", size = 2) +
    theme(axis.text.y=element_text(size=10, hjust=1.0, colour="#2a2a2b"),
          axis.text.x=element_text(size=13, hjust=0.5, colour="#2a2a2b"),
          legend.position="none")
}

png("edu_heat_schoolclimate.png", width = 600, height = 400)
schoolclimate <- heatmap.for.subdomain("Education", "School Climate", schoolclimate_scores)
schoolclimate
dev.off()


png("edu_heat_childhoodeducation.png", width = 600, height = 400)
childhoodeducation <- heatmap.for.subdomain("Education", "Early Childhood Education", childhoodeducation_scores)
childhoodeducation
dev.off()

png("edu_heat_afford.png", width = 600, height = 400)
afford <- heatmap.for.subdomain("Education", "Post-Secondary Affordability Policy",
                                  afford_scores)
afford
dev.off()


png("edu_heat_workforce.png", width = 600, height = 400)
workforce <- heatmap.for.subdomain("Education", "Workforce Development Policy", workforce_scores)
workforce
dev.off()

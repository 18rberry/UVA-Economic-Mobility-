library(ggplot2)
library(dplyr)
library(readr)
library(rlist)
library(gridExtra)
library(grid)

setwd('~/git/dspg20uvaEM/EM_gates/WWW')
source("~/git/dspg20uvaEM/EM_gates/data/theme_SDAD.R")
source("~/git/dspg20uvaEM/EM_gates/data/Colorblind_Palette.R")

score_card <- read_csv("~/git/dspg20uvaEM/EM_gates/data/Final Scorecard - Sheet1.csv")


wage_scores<-  c(0, 0, 0, 0, 0, 0)
org_scores<- c(0, 0, 1, 0, 1, 1, 0, 0, 1) 
protect_scores<- c( 0, 0, 1,
                     0,  0, 1,
                    0, 0, 0, 
                    0, 0, 1,
                    1, 1, 1,
                    0, 0,  1,
                     0, 0,1,
                    0, 1, 1,
                    0, 0, 1,
                     0, 0,1,
                    1, 1, 1,
                    1, 1, 1)



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
  Scores<-ifelse(Scores==1,"Yes", "No")
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
    ggplot2::annotate("text", x=2, y= intercept,label= subdomain, fontface=2, color="black", size = 4) +
    theme(axis.text.y=element_text(size=10, hjust=1.0, colour="#2a2a2b"),
          axis.text.x=element_text(size=13, hjust=0.5, colour="#2a2a2b"),
          legend.position="none")
}

png("emp_heat_wage.png", width = 900, height = 400)
credits <- heatmap.for.subdomain("Employment", "Wage", wage_scores)
credits
dev.off()


png("emp_heat_org.png", width = 600, height = 400)
wealth <- heatmap.for.subdomain("Employment", "Organizing", org_scores)
wealth
dev.off()

png("emp_heat_protect.png", width = 700, height = 400)
business <- heatmap.for.subdomain("Employment", "Protections",
                                  protect_scores)
business
dev.off()



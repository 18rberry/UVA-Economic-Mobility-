library(ggplot2)
library(dplyr)
library(readr)
library(rlist)
library(gridExtra)
library(grid)

setwd('~/git/TestDSPG/dspg20uvaEM/EM_gates/WWW')
source("~/git/TestDSPG/dspg20uvaEM/EM_gates/data/theme_SDAD.R")
source("~/git/TestDSPG/dspg20uvaEM/EM_gates/data/Colorblind_Palette.R")
score_card <- read_csv("~/git/TestDSPG/dspg20uvaEM/EM_gates/data/Final Scorecard - Sheet1.csv")

#order of scores is left to right (VA, IA, OR) but backwards in the sense that you start with the last policy's scores (for each state)
# and then zig zag upwards
credit_scores <- c(1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1)
wealth_scores <- c(1, 1, 1, 0, 0, 1, 0, 1, 0)
business_scores <- c(1, 1, 1, 0, 0, 1, 1, 0, 1)
gini_scores <- c(1, 1, 1, 0, 0, 0)

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
    ggplot2::annotate("text", x=2, y= intercept,label= subdomain, fontface=2, color="black", size = 2) +
    theme(axis.text.y=element_text(size=10, hjust=1.0, colour="#2a2a2b"),
          axis.text.x=element_text(size=13, hjust=0.5, colour="#2a2a2b"),
          legend.position="none")
}

# png("tax_heat_credits.png", width = 600, height = 400)
credits <- heatmap.for.subdomain("Taxation", "Tax Credits", credit_scores)
credits
# dev.off()


# png("tax_heat_wealth.png", width = 600, height = 400)
wealth <- heatmap.for.subdomain("Taxation", "Taxes on Wealth", wealth_scores)
wealth
# dev.off()

# png("tax_heat_business.png", width = 600, height = 400)
business <- heatmap.for.subdomain("Taxation", "Taxes Related to Business and Corporations",
                                  business_scores)
business
# dev.off()


# png("tax_heat_gini.png", width = 600, height = 400)
gini <- heatmap.for.subdomain("Taxation", "Gini Index", gini_scores)
gini
# dev.off()

#taxation heat map
taxation_card <- score_card %>%
  filter(Domain == "Taxation")
taxation_card$Subdomain
question_list <- taxation_card$Question
subdomain_list <- c("Tax Credits", "Taxes on Wealth", "Taxes Related to Business and Corporations", "Gini Index")
Dimensions<-rep(subdomain_list,
                c(3*4, 3*3, 3*3, 3*2))
Questions <-factor(c(1:12), labels= question_list)
Questions<-rep(Questions, rep(3, 12)) #3=number of states, 9=number of questions
States<-rep(c("VA","IA","OR"), 12) #9=number of questions
States<-ordered(States, levels=c("VA","IA","OR"))
Scores <- c(1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1,
            0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0)
Scores<-ifelse(Scores==1,"Yes", "No")
VOTE<-data.frame(Dimensions, Questions, States, Scores)
ggplot(VOTE, aes(y=Questions, x=States, fill=factor(Scores))) +
  geom_tile(color="grey90", lwd=1) +
  coord_equal() +
  theme_SDAD() +
  ggplot2::annotate("text", x=rep(c(1:3), length(unique(Questions))),
                    y=rep(c(1:length(unique(Questions))), each=3),
                    label=Scores) +
  scale_fill_manual(values=cbPalette[c(3,2,1)]) +
  xlab("") + ylab("") +
  #The horizontal lines the separate the policy domains (you will need to change the intercept)
  geom_hline(yintercept=4.5, color=cbPalette[1], lwd=4) +
  geom_hline(yintercept=7.5, color=cbPalette[1], lwd=4) +
  geom_hline(yintercept=10.5, color=cbPalette[1], lwd=4) +
  geom_hline(yintercept=12.5, color=cbPalette[1], lwd=4) +
  #Title of the policy domains (you will need to change y)
  ggplot2::annotate("text", x=2, y=12.5,label= subdomain_list[4], fontface=2, color="black", size = 2) +
  ggplot2::annotate("text", x=2, y=10.5,label= subdomain_list[3], fontface=2, color="black", size = 2) +
  ggplot2::annotate("text", x=2, y=7.5,label= subdomain_list[2], fontface=2, color="black", size = 2) +
  ggplot2::annotate("text", x=2, y=4.5,label= subdomain_list[1], fontface=2, color="black", size = 2) +
  theme(axis.text.y=element_text(size=10, hjust=1.0, colour="#2a2a2b"),
        axis.text.x=element_text(size=13, hjust=0.5, colour="#2a2a2b"),
        legend.position="none")



composite_card <- read_csv("~/git/TestDSPG/dspg20uvaEM/EM_gates/data/Composite Scorecard - Sheet2.csv")
composite_card
composite_card <- composite_card %>%
  filter(Domain == "Taxation")
myTable <- tableGrob(composite_card,
                     rows = NULL,
                     theme = ttheme_default(core = list(bg_params = list(fill = "grey99"))))
grid.draw(myTable)

library(ggplot2)

setwd("~/git/dspg20uvaEM/EM_gates/plots/Output_images")

#source("theme_SDAD.R")
#source("Colorblind_Palette.R")

source("~/git/dspg20uvaEM/EM_gates/data/theme_SDAD.R")
source("~/git/dspg20uvaEM/EM_gates/data/Colorblind_Palette.R")

Dimensions<-rep(c("Assistance", "Financial", "Development"), c(3*8, 3*9, 3*9)) #3=number of states*number of dimension questions
#When you put your questions in the first question will be on the bottom row of the heat map.
#First create a numeric vector the length of the number of questions and then assign the questions to the numeric vector,
#so they will appear in the correct order. You need to use the factor to assign the questions to the numbers.
Questions<-factor(c(1:26), labels=c(
  "State offers tax breaks for long-term homeowners?",
  "State offers incentives to construct low income housing?",
  "Is transportation diversity considered in development plans?",
  "Is rural housing emphasized in development plans?",
  "Is economic diversity emphasized in development plans?",
  "Is racial diversity emphasized in development plans?",
  "Protect mobile home rights when considering construction?",
  "Are luxury home builds blocked in at-risk neighborhoods?",
  "Space restrictions on housing developments?",
  "Does lower income housing have lower tax rates?",
  "Offer tax deductions for second homes?",
  "Refundable credit in place of a tax deduction?",
  "Tax exemption for disabled members?",
  "Tax exemption for veterans?",
  "Is there a low rent housing tax exemption?",
  "Is state a recipient of national housing trust fund grant?",
  "Is state a recipient of the federal HOME block grant?",
  "Is state a recipient of community dev block grant (CDBG)?",
  "Home mortgage interest deductions?",
  "Offer home price discounts for first responders?",
  "Offers for home price discounts for educators?",
  "Rent aid for the elderly population?",
  "Government offered home loan programs?",
  "Loan assistance programs for the disabled?",
  "Loan assistance programs for veterans?",
  "Loan assistance program for 1st time owners?"
))
Questions<-rep(Questions, rep(3, 26)) #3=number of states, 9=number of questions
States<-rep(c("VA","IA","OR"), 26) #9=number of questions
States<-ordered(States, levels=c("VA","IA","OR"))
Scores<-c(1,1,1,0,1,1,0,1,0,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,0,1,0,1,1,1,1,1,0,1,0,1,1,1,0,1,0,1,0,0,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,1,1,1,1,0,1,1,1,1,0,0,0)
Scores<-ifelse(Scores==1,"Yes", "No")
VOTE<-data.frame(Dimensions, Questions, States, Scores)

png("housing_heat_map.png", width = 800, height = 800)

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
  geom_hline(yintercept=9.5, color=cbPalette[1], lwd=4) +
  geom_hline(yintercept=18.5, color=cbPalette[1], lwd=4) +
  geom_hline(yintercept=26.5, color=cbPalette[1], lwd=4) +
  #Title of the policy domains (you will need to change y)
  ggplot2::annotate("text", x=2, y=26.5,label="Assistance", fontface=2, color="black") +
  ggplot2::annotate("text", x=2, y=18.5,label="Financial", fontface=2, color="black") +
  ggplot2::annotate("text", x=2, y=9.5,label="Development", fontface=2, color="black") +
  theme(axis.text.y=element_text(size=10, hjust=1.0, colour="#2a2a2b"),
        axis.text.x=element_text(size=13, hjust=0.5, colour="#2a2a2b"),
        legend.position="none")

#Instructions for Exporting
#Save the heatmap as a .png file
#make sure you check maintain aspect ratio
#the height should be 70*the number of rows in the heatmap

dev.off()

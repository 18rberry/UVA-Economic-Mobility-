library(ggplot2)

#setwd("~/Documents/DSPG/EM project")
source("theme_SDAD.R")
source("Colorblind_Palette.R")

# source("~/git/dspg20uvaEM/EM_gates/data/theme_SDAD.R")
# source("~/git/dspg20uvaEM/EM_gates/data/Colorblind_Palette.R")

#Read in data ----------------------------------------------------------
data<- read_excel("~/git/dspg20uvaEM/EM_gates/data/em_master_data_final.xlsx")

#slice full data set to seaparate employment data
emp <- data %>%
  slice(49:65)
emp <- gather(emp, "state", 'score', c(4:6))
tail(emp)

colnames(emp)


Dimensions<-rep(c("Wage", "Organizing", "Protections"), c(3*2, 3*3, 3*12)) #3=number of states*number of dimension questions
#When you put your questions in the first question will be on the bottom row of the heat map.
#First create a numeric vector the length of the number of questions and then assign the questions to the numeric vector,
#so they will appear in the correct order. You need to use the factor to assign the questions to the numbers.

emp$questions

Questions<-factor(c(1:17), labels=c(
  "Does the state allow localities to pass minimum wage laws?",
  "Is the state minimum wage a living wage for a family of three (2 adults and 1 child) in a majority of the counties/cities?",
  "Does the state allow union activity (no Right-to-Work law)?",
  "Is there collective bargaining for teachers, police, and fire fighters?",
  "Does the state have a Project Labor Agreement?",
  "Does the state have paid sick leave?",
  "Does the state have paid family leave?",
  "Does the state require private sector pregnant worker accomodation?",
  "Does the state have private sector right to pump policies?",
  "Does the state have a basic equal pay mandate?",
  "Does the state have a \"No Pay Secrecy\" mandate?",
  "Does the state have a \"No Private Sector Salary History\" mandate? (asking for salary history not allowed)",
  "Does the state have sexual harassment law?",
  "Does unemployment insurance cover family care reasons?",
  "Does the State Have an Employment Rights Law for Victims of Domestic Violence?",
  "Does the state have an OSHA-approved plan that covers private workplaces?",
  "Does the state have an OSHA-approved plan that covers state/local government workplaces?"
))
Questions<-rep(Questions, rep(3, 17)) #3=number of states, 17=number of questions
States<-rep(c("VA","IA","OR"), 17) # 17 = number of questions
States<-ordered(States, levels=c("VA","IA","OR"))

Scores<-emp$score
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
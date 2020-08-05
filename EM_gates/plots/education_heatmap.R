library(NLP)
library(ggplot2)

#Read in data ----------------------------------------------------------
education <-read.csv("~/git/dspg20uvaEM/EM_gates/data/Education.csv")

head(education)

#get length of education
nrow(education)
education$X.POLICY.QUESTIONS.

#AUTOMATE - unique values in dimension columns
#dataframe$columnname
#Lara/Tasfia
Dimensions<-rep(c("Voter Registration", "Voting Accessibility"), c(3*5, 3*4)) #3=number of states*number of dimension questions
#When you put your questions in the first question will be on the bottom row of the heat map.
#First create a numeric vector the length of the number of questions and then assign the questions to the numeric vector,
#so they will appear in the correct order. You need to use the factor to assign the questions to the numbers.


#OLD CODE
#AUTOMATE - unique function for question category
#factor(c(1:nrow(df))),labels = c(unique function or iterating over a loop?) or count function
#Martha
#Questions<-factor(c(1:9), labels=c("Does the state have automatic voter\nregistration at the DMV?",
                                   #"Does the state have automatic voter registration\nat agencies other than the DMV?",
                                   #"Does the state have online registration?",
                                   #"Does the state have Election Day\nvoter registration?",
                                   #"Is there pre-registration for individuals\n<18 years of age?",
                                   #"Does the state allow voting with no\nvoter ID documents?",
                                   #"Does the state have automatic no\nexcuse absentee/mail-in voting?",
                                   #"Does the state have 28 days or\nmore of early voting?",
                                   #"Does the state restore a felon's right to\nvote without a Governor's pardon?"))

#NEW CODE TO REPLACE OLD CODE AND GET QUESTIONS
Questions <- education$X.POLICY.QUESTIONS.
Questions

#ALSO EDITED THIS FOR NROW INSTEAD OF 9
#Questions<-rep(Questions, rep(3, nrow(df))
Questions<-rep(Questions, rep(3, nrow(education))) #3=number of states, 9=number of questions
#keep abbrevation
States<-rep(c("VA","IA","OR"), nrow(education)) #9=number of questions

#
States<-ordered(States, levels=c("VA","IA","OR"))

#write a function, write it as functional form
#function  {ifelse }
#dataframe -> read in scores
#Policing['Iowa'].apply(function)
#If Iowa == 1
#Reading in scores :)
# Vatsala
Scores<-c(1,0,1,0,0,0,1,1,1,0,1,0,1,1,1,0,0,1,0,0,1,1,1,1,0,0,1)
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
  geom_hline(yintercept=5.5, color=cbPalette[1], lwd=4) +
  geom_hline(yintercept=9.5, color=cbPalette[1], lwd=4) +
  #Title of the policy domains (you will need to change y)
  ggplot2::annotate("text", x=2, y=9.5,label="Voting Accessibility", fontface=2, color="black") +
  ggplot2::annotate("text", x=2, y=9.5,label="Voting Accessibility", fontface=2, color="black") +
  ggplot2::annotate("text", x=2, y=5.5,label="Voter Registration", fontface=2, color="black") +
  ggplot2::annotate("text", x=2, y=5.5,label="Voter Registration", fontface=2, color="black") +
  theme(axis.text.y=element_text(size=10, hjust=1.0, colour="#2a2a2b"),
        axis.text.x=element_text(size=13, hjust=0.5, colour="#2a2a2b"),
        legend.position="none")

#Instructions for Exporting
#Save the heatmap as a .png file
#make sure you check maintain aspect ratio
#the height should be 70*the number of rows in the heatmap

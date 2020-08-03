library(ggplot2)
library(readxl)

#setwd("~/Documents/DSPG/EM project")

source("~/git/dspg20uvaEM/EM_gates/data/theme_SDAD.R")
source("~/git/dspg20uvaEM/EM_gates/data/Colorblind_Palette.R")

#Read in data ----------------------------------------------------------
data<- read_excel("~/git/dspg20uvaEM/EM_gates/data/em_master_data_final.xlsx")

#filter full data set to separate employment data
emp <- data %>%
  filter(domain == 'Employment')
#emp<- emp[nrow(emp):1,]

#emp_melt <- gather(emp, "state", 'score', c(4:6))

df = emp #_melt[with(emp_melt, order(sub_domain, questions)),]

#reverse the order of rows
df<- df[nrow(df):1,]
#When you put your questions in, the questions will start at the bottom and go up on the heat map.


Dimensions<-rep(unique(df$sub_domain), c(3*2, 3*3, 3*12)) #3=number of states*number of dimension questions

Questions<-factor(c(1:17), labels= unique(df$questions))
Questions<-rep(Questions, rep(3, 17)) #3=number of states, 17=number of questions
States<-rep(c("VA","IA","OR"), 17) # 17 = number of questions
States<-ordered(States, levels=c("VA","IA","OR"))

Scores<-c(0,
          0,
          0,
          0,
          0,
          0,
          1,
          0,
          0,
          1,
          1,
          0,
          1,
          0,
          0,
          1,
          0,
          0,
          1,
          0,
          0,
          0,
          0,
          0,
          1,
          0,
          0,
          1,
          1,
          1,
          1,
          0,
          0,
          1,
          0,
          0,
          1,
          1,
          0,
          1,
          0,
          0,
          1,
          0,
          0,
          1,
          1,
          1,
          1,
          1,
          1)

#reverse the order of scores to match questions
Scores<- Scores[length(Scores):1]

Scores<-ifelse(Scores==1,"Yes", "No")
plot_data<-data.frame(Dimensions, Questions, States, Scores)


png("employment_heatmap.png", width = 800, height = 800)

ggplot(plot_data, aes(y=Questions, x=States, fill=factor(Scores))) +
  geom_tile(color="grey90", lwd=1) + 
  coord_equal() +
  theme_SDAD() +
  ggplot2::annotate("text", x=rep(c(1:3), length(unique(Questions))),
                    y=rep(c(1:length(unique(Questions))), each=3),
                    label=Scores) +
  scale_fill_manual(values=cbPalette[c(3,2,1)]) +
  xlab("") + ylab("") +
  #The horizontal lines the separate the policy domains (you will need to change the intercept)
  geom_hline(yintercept=12.5, color=cbPalette[1], lwd=4) +
  geom_hline(yintercept=15.5, color=cbPalette[1], lwd=4) +
  geom_hline(yintercept=17.5, color=cbPalette[1], lwd=4) +
  #Title of the policy domains (you will need to change y)
  ggplot2::annotate("text", x=2, y=17.5,label="Wage", fontface=2, color="black",size = 2) +
  ggplot2::annotate("text", x=2, y=15.5,label="Organizing", fontface=2, color="black",size = 2) +
  ggplot2::annotate("text", x=2, y=12.5,label="Protections", fontface=2, color="black",size = 2) + #
  theme(axis.text.y=element_text(size=10, hjust=1.0, colour="#2a2a2b"),
        axis.text.x=element_text(size=13, hjust=0.5, colour="#2a2a2b"),
        legend.position="none")

dev.off()



#Instructions for Exporting
#Save the heatmap as a .png file
#make sure you check maintain aspect ratio
#the height should be 70*the number of rows in the heatmap
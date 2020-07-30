####Distributions education

library(readr)
library(dplyr)
setwd("~/Documents/DSPG/EM project")

edu_3states <- read_csv("edu_3states.csv")

#pop3states <- read_csv("~/Documents/Census/Population/productDownload_2020-07-21T223855/pop3states.csv")
#dat2019 <- edu_3states %>% filter(year== "2019")

#melt data base
library(reshape2)
edu <- melt(data = edu_3states, id.vars = c("domain", "dimension", "subcategory"), measure.vars = c("Oregon", "Virginia", "Iowa"))

#averages by subcategory
length(unique(edu$subcategory))

edu_av<- edu %>% group_by(subcategory) %>% summarize(mean=mean(value))
edu_av2<- edu %>% group_by(variable,subcategory) %>% summarize(mean=mean(value))

edu_d <- ggplot(edu_av2, aes(x=variable, y= mean , fill = variable))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = 3)+
  geom_point(position = position_jitter(width = .15), size = .5)+
  
  #geom_boxplot(aes(x = as.numeric(state)+0.15, y = pcw), outlier.shape = 1 , alpha = 0.3, width = .1, colour = "BLACK") +
  
  geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=1, notch=FALSE, width = .1, colour = "BLACK", fill="transparent") +
  ylab('Percentage')+xlab('')+
  coord_flip()+ 
  theme_cowplot()+
  guides(fill = FALSE)+
  ggtitle('Averages of policy presence per state ') +
  theme(
    panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "gray"),
    axis.line = element_line(colour = "transparent")
  ) 

edu_d



#############

edu_new <- ggplot(edu_av2, aes(x=variable, y= mean , fill = variable))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = 3)+
  geom_point(position = position_jitter(width = .15), size = .5)+
  
  #geom_boxplot(aes(x = as.numeric(state)+0.15, y = pcw), outlier.shape = 1 , alpha = 0.3, width = .1, colour = "BLACK") +
  
  geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=1, notch=FALSE, width = .1, colour = "BLACK", fill="transparent") +
  geom_text(aes(label=subcategory), size=2)+
  ylab('Percentage')+xlab('')+
  coord_flip()+ 
  theme_cowplot()+
  guides(fill = FALSE)+
  ggtitle('Averages of policy presence per state ') +
  theme(
    panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "gray"),
    axis.line = element_line(colour = "transparent")
  ) 

edu_new






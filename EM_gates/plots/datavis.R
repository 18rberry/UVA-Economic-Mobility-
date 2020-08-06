#Read data
df <- read.csv("Education.csv")
head(df)

subdomaindf <- read.csv("subdomain.csv")
head(subdomaindf)

subcategorydf <- read.csv("subcategory.csv")
head(subcategorydf)

# load library
library(ggplot2)
ggplot(subdomaindf,aes(Subdomain,Score, colour = State))+ geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ facet_grid(State ~ .)



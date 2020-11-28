#opening the survey data file
library(plyr)
library(dplyr)
dataname <- read.csv(file.choose())
surveyquestion  <- read.csv(file.choose())
summary(dataname)

#searching for missing observations
missing.observation <- sum(is.na(dataname$Country))
missing.observation

#top 5 countries are USA, India, UK, Germany, Canada respectively.
          
data1<- dataname[dataname$Country=="United Stated" | dataname$Country=="India"
                 | dataname$Country=="Germany" | dataname$Country=="Canada" |dataname$Country=="France", ]

df<- dataname
y <- group_by(df,Country) %>% summarise(Number = n()) %>% arrange(desc = 20) %>% ungroup() %>% top_n(5)
x<-group_by(df,Country) %>% summarise(Number = n()) %>% arrange(desc = 20) %>% ungroup() 

plot(x)
pie(x$Number,x$Country)
plot(y)
pie(y$Number,y$Country)

barplot(x$Number, xlab=x$Country)
barplot(y$Number, ylab=y$Country, horiz=TRUE)

#Percentages of the top 5 country respondends 
x$percent<- ((x$Number)/sum(x$Number))*100
x
x$percent <-ifelse(x$percent<=1,0,x$percent)
z <- subset(x,x$percent!=0,)
z
barplot(z$percent, xlab=z$Country,col="blue")

newdf<- dataname[dataname$Country=="United States" | dataname$Country=="India" |
                   dataname$Country=="United Kingdom" |dataname$Country=="Germany" |dataname$Country=="Canada",]
summary(newdf)

usa<-dataname[dataname$Country=="United States",]

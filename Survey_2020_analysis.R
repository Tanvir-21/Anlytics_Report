#necessary_library
library(dplyr)
library(highcharter)
library(tidyverse)
library(RColorBrewer)
library(pacman)
library(ggplot2)
library(treemapify)
library(ggpubr)

#opening survery data file 
survey_data <- read.csv(file.choose())
survey_schema  <- read.csv(file.choose())
summary(survey_data)

#Finding all the countries with respondents
total_countries <- survey_data %>%
  group_by(Country)%>% 
  summarise(Number=n())%>% 
  arrange(desc=Number)%>% 
  ungroup()%>% 
  mutate(Country = reorder(Country, Number))%>% 
  select(Country, Number)

#Finding the countries with top respondents
top_countries <- survey_data %>%
  group_by(Country)%>% 
  summarise(Number=n())%>% 
  arrange(desc=Number)%>% 
  ungroup()%>% 
  mutate(Country = reorder(Country, Number))%>% 
  top_n(20, wt = Number)


########################################## plotting ###############################################################

# Set highcharter options
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))

# Load the world Map data
data(worldgeojson, package = "highcharter")

total_countries_plot <- highchart() %>%
  hc_add_series_map(
    worldgeojson, total_countries, value = "Number", joinBy = c('name','Country'),
    name = "Number of Respondents"
  )  %>% 
  hc_colorAxis(stops = color_stops()) %>% 
  hc_title(text = "World Map") %>% 
  hc_subtitle(text = "Total respondent country in 2020")

#plotting the treemap 

colourCount = length(unique(top_countries$Country)) #get the unique color 
getPalette = colorRampPalette(brewer.pal(9, "PuOr")) #choose the color palette

#visualize using ggplot
ggplot(top_countries, aes(area=Number, fill=factor(Country), label=Country))+ 
  geom_treemap(show.legend=F) + 
  geom_treemap_text(color="black", place="centre") + 
  scale_fill_manual(values=getPalette(colourCount)) + labs(title="TreeMap from top 20 Countries with most respondents")


usa.responses<-survey_data[survey_data$Country=="United States",]
india.responses<-survey_data[survey_data$Country=="India", ]
difference.responses <-survey_data[survey_data$Country=="India"|survey_data$Country=="United States", ]

#Barplot of percent of responses according to country
options(repr.plot.width=9, repr.plot.height=6) #setting height and width of the plot
fillcolor1 <- "#FE8358"
fillcolor2 <- "#F1FB92"

line.length <- nrow(survey_data)

percentage_topcountries <- survey_data %>% 
  group_by(Country) %>% 
  summarise(Counting=round(n()/line.length * 100, 2)) %>% 
  arrange(desc(Counting)) %>% 
  ungroup() %>% 
  mutate(country = reorder(Country, Counting)) %>% 
  head(20)

percentage_topcountries_plot <- ggplot(percentage_topcountries, aes(x = country, y = Counting)) + 
  geom_bar(stat = "identity", fill = fillcolor1)+
  geom_text(aes(x = Country, y = 0.01, label = paste0("(", Counting, "% )", sep = "")),
            hjust = -7, vjust = 1, size = 3, color = "black", fontface = "bold")+
  scale_y_continuous()+
  labs(x = "Country", y = "Percent", title = "Percent of respondent")+
  coord_flip()

percentage_topcountries_plot

#developer by profession question response world plot

counting_developer_by_branch <- survey_data %>% 
  filter(!is.na(MainBranch)) %>% 
  group_by(MainBranch) %>% 
  count()

ggplot(survey_data,aes(y=factor(MainBranch)))+  
  geom_bar(stat="count", width=0.7, fill=fillcolor1)+
  labs(title= "Professions of Respondents",
       y="Profession Criteria",
       x="Response") + theme_minimal()


#coding as hobby question world plot
ggplot(survey_data,aes(y=factor(Hobbyist)))+  geom_bar(stat="count", width=0.7, fill="steelblue")+
  labs(title= "Coding as a hobby: Trait among respondents",
       y="Hobby",
       x="Response") + theme_minimal()

#Finding employment Type 
survey_data %>%
  filter(!is.na(Employment)) %>% 
  group_by(Employment) %>% 
  count() 







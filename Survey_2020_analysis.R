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
employment_type <- survey_data %>%
  filter(!is.na(Employment)) %>% 
  group_by(Employment) %>% 
  count() 

#Female to Male Ratio 
##Female to Male Ration - Better Countries

Female_male_ratio_plot <- survey_data %>% 
  group_by(Country) %>% 
  mutate(count = n()) %>% 
  filter(count > 200) %>% 
  filter(Gender %in% c('Man', 'Woman')) %>% 
  group_by(Country, Gender) %>% 
  count() %>% 
  spread(Gender,n) %>% 
  mutate(Female_to_male_ratio = Woman / Man) %>% 
  arrange(desc(Female_to_male_ratio)) %>% 
  head(50) %>% 
  hchart('column', hcaes(x = 'Country', y = 'Female_to_male_ratio')) %>% 
  hc_title(text = "Top 50 countries ordered with better female to male ratio") %>% 
  hc_colors(c("steelblue", "darkgray")) %>% 
  hc_xAxis(title = list(text = "Countries"))

  Female_male_ratio_plot
  
#Male-female ratio among respondents 

male_female_percentage <- survey_data %>% 
  filter(!is.na(Gender)) %>% 
  select(Gender) %>% 
  mutate(Gender = str_split(Gender, pattern = ";")) %>% 
  unnest(Gender) %>% 
  group_by(Gender) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  mutate(percentage = round((Count/sum(Count))*100)) %>% 
  arrange(desc(percentage))
  

# Formal Education 

# Which of the following best describes your main field of study (aka 'major')?

TotalNoofRows <- nrow(survey_data)

plotUndergradEducation <- function(survey_data, TotalNoofRows)
{
  survey_data %>% 
    filter(!is.na(survey_data$UndergradMajor)) %>% 
    select(UndergradMajor) %>% 
    group_by(UndergradMajor) %>% 
    summarise(Count = n()/TotalNoofRows) %>% 
    arrange(desc(Count)) %>% 
    ungroup() %>% 
    mutate(UndergradMajor = reorder(UndergradMajor,Count)) %>% 
    head(10) %>% 
    ggplot(aes(x = UndergradMajor, y = Count))+
    geom_bar(stat = 'identity', fill = fillcolor2)+
    geom_text(aes(x = UndergradMajor,y = .01, label = paste0("( ",round(Count*100,2)," %)",sep="")),
              hjust = -3, vjust = .5, size= 4, color = 'black', fontface = 'bold')+
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      x = 'Undergrad Major',
      y = 'Percentage',
      title = 'Undergraduate Major Percentage'
    )+
    coord_flip()+
    theme_bw()
}


plotUndergradEducation(survey_data, TotalNoofRows)

# Developer Desire Next Year 
## Platform Desire Next Year
plotPlatformDesire <- function(survey_data,TotalNoofRows) {
  survey_data %>%
    filter(!is.na(PlatformDesireNextYear)) %>%
    select(PlatformDesireNextYear) %>%
    mutate(PlatformDesireNextYear = str_split(PlatformDesireNextYear, pattern = ";")) %>%
    unnest(PlatformDesireNextYear) %>%
    group_by(PlatformDesireNextYear) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(TotalCount =  sum(Count)) %>%
    mutate(Count =  Count/TotalCount) %>%
    mutate(PlatformDesireNextYear = reorder(PlatformDesireNextYear,Count)) %>%
    head(10) %>%
    
    ggplot(aes(x = PlatformDesireNextYear,y = Count)) +
    geom_bar(stat='identity',fill= fillcolor2) +
    geom_text(aes(x = PlatformDesireNextYear,y = .01, label = paste0("( ",round(Count*100,2)," %)",sep="")),
              hjust= -3, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = 'PlatformDesireNextYear', 
         y='Percentage', 
         title = 'PlatformDesireNextYear and Percentage') +
    coord_flip() +
    theme_bw()
}

plotPlatformDesire(survey_data,TotalNoofRows)

#Language worked with this 
plotPlatformDesire <- function(survey_data,TotalNoofRows) {
  survey_data %>%
    filter(!is.na(LanguageWorkedWith)) %>%
    select(LanguageWorkedWith) %>%
    mutate(LanguageWorkedWith = str_split(LanguageWorkedWith, pattern = ";")) %>%
    unnest(LanguageWorkedWith) %>%
    group_by(LanguageWorkedWith) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(TotalCount =  sum(Count)) %>%
    mutate(Count =  Count/TotalCount) %>%
    mutate(LanguageWorkedWith = reorder(LanguageWorkedWith,Count)) %>%
    head(10) %>%
    
    ggplot(aes(x = LanguageWorkedWith,y = Count)) +
    geom_bar(stat='identity',fill= fillcolor2) +
    geom_text(aes(x = LanguageWorkedWith,y = .01, label = paste0("( ",round(Count*100,2)," %)",sep="")),
              hjust= -3, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = 'Language Worked With', 
         y='Percentage', 
         title = 'Language worked with this year and Percentage') +
    coord_flip() +
    theme_bw()
}

plotPlatformDesire(survey_data,TotalNoofRows)

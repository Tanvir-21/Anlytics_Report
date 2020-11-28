#necessary_library
library(dplyr)
library(highcharter)
library(tidyverse)
library(RColorBrewer)
library(pacman)
library(ggplot2)
library(treemapify)

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

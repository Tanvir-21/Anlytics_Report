
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


#Barplot of responses according to country
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

#Salary analysis by developer type 

##worldwide
salary_by_developer_type_worldwide<- survey_data %>% 
  filter(!is.na(DevType)) %>% 
  select(DevType, ConvertedComp) %>% 
  mutate(DevType = str_split(DevType, pattern = ";")) %>% 
  unnest(DevType) %>% 
  group_by(DevType) %>% 
  summarise(median_salary = median(ConvertedComp, na.rm = TRUE)) %>% 
  arrange(desc(median_salary)) %>% 
  ungroup() %>% 
  mutate(DevType = reorder(DevType, median_salary))

salary_by_developer_type_worldwide_plot <- ggplot(data = salary_by_developer_type_worldwide,
                                                  aes(x = DevType, y = median_salary))+
  geom_bar(stat = 'identity', fill = fillcolor2)+
  geom_text(aes(x = DevType, y = 1, label = paste0("( ",round(median_salary/1e3)," K)",sep="")),
            hjust = -3, vjust = 0.5, size = 3, color = 'black', fontface = 'bold')+
  labs(
    x = 'DevType',
    y = 'median_salary',
    title = "Median Salary according to developer type Worldwide"
  )+
  coord_flip()+
  theme_classic()

survey_schema %>% 
  filter(Column == "YearsCodePro") %>% 
  select(QuestionText)

## Salary Analysis Dev Types - Years of experiences 
#with yearcodepro
salary_by_dev_type <- survey_data %>% 
  select(DevType, YearsCodePro, ConvertedComp) %>% 
  mutate(DevType = str_split(DevType, pattern = ";")) %>% 
  unnest(DevType) %>% 
  group_by(DevType, YearsCodePro) %>% 
  summarise(median_salary = median(ConvertedComp, na.rm = TRUE)) %>% 
  arrange(desc(median_salary)) %>% 
  ungroup() %>% 
  mutate(YearsCodePro = as.character(YearsCodePro),
         DevType = as.character(DevType)) %>% 
  mutate(DevType_YearsCodePro = paste(DevType,YearsCodePro)) %>%
  mutate(DevType_YearsCodePro =str_replace(DevType_YearsCodePro,"NA","")) %>%
  mutate(DevType_YearsCodePro = reorder(DevType_YearsCodePro, median_salary)) %>% 
  head(10)


##########################################BANGLADESH ###############################################################


#dataframe of bd
bd <-survey_data[survey_data$Country=="Bangladesh",]

#Salary analysis by developer type 
salary_by_developer_type_Bangladesh <- survey_data %>% 
  filter(Country == "Bangladesh") %>% 
  filter(!is.na(DevType)) %>% 
  select(DevType, ConvertedComp) %>% 
  mutate(DevType = str_split(DevType, pattern = ";")) %>% 
  unnest(DevType) %>% 
  group_by(DevType) %>% 
  summarise(median_salary = median(ConvertedComp, na.rm = TRUE)) %>% 
  arrange(desc(median_salary)) %>% 
  ungroup() %>% 
  mutate(DevType = reorder(DevType, median_salary))

salary_by_developer_type_Bangladesh_plot <- ggplot(data = salary_by_developer_type_Bangladesh,
                                                   aes(x = DevType, y = median_salary))+
  geom_bar(stat = 'identity', fill = fillcolor2)+
  geom_text(aes(x = DevType, y = 1, label = paste0("( ",round(median_salary/1e3)," K)",sep="")),
            hjust = -3, vjust = 0.5, size = 3, color = 'black', fontface = 'bold')+
  labs(
    x = 'DevType',
    y = 'median_salary',
    title = "Median Salary according to developer type in Bangladesh"
  )+
  coord_flip()+
  theme_classic()

#developer by profession 
profession.bd <- bd %>% filter(!is.na(MainBranch))
profession.bd.plot <-ggplot(profession.bd,aes(y=factor(MainBranch)))+  geom_bar(stat="count", width=0.7, fill="LightGreen")+
  labs(title= "Professions of Respondents from Bangladesh",
       y="Profession Criteria",
       x="Response") + theme_minimal()
profession.bd.plot 


#coding as hobby
hobby.bd <- bd %>% filter(!is.na(Hobbyist))
hobby.bd.plot <- ggplot(hobby.bd,aes(y=factor(Hobbyist)))+  geom_bar(stat="count", width=0.7, fill="LightGreen")+
  labs(title= "Coding as a hobby: Trait among respondents from Bangladesh",
       y="Hobby",
       x="Response") + theme_minimal()
hobby.bd.plot


#employment status
employment.bd <- bd %>% filter(!is.na(Employment))
  employment.bd.plot <-  ggplot(employment.bd,aes(y=factor(Employment)))+  geom_bar(stat="count", width=0.7, fill="LightGreen")+
  labs(title= "Employment status of Respondents from Bangladesh",
       y="Employment Status",
       x="Response") + theme_minimal()
employment.bd.plot

#formal education
edulevel.bd <- bd %>% filter(!is.na(EdLevel))
edulevel.bd.plot <- ggplot(edulevel.bd,aes(y=factor(EdLevel)))+  geom_bar(stat="count", width=0.7, fill="LightGreen")+
  labs(title= "Education Level of Respondents from Bangladesh",
       y="Education Level",
       x="Response") + theme_minimal()
edulevel.bd.plot


#undergrad 
undergrad.bd <- bd %>% filter(!is.na(UndergradMajor))
undergrad.bd.plot <- ggplot(undergrad.bd,aes(y=factor(UndergradMajor)))+  geom_bar(stat="count", width=0.7, fill="LightGreen")+
  labs(title= "Primary Field of Study among Respondents from Bangladesh",
       y="Primary Field of Study",
       x="Response") + theme_minimal()
undergrad.bd.plot 

#importance of formal education in the eyes of Respondents
importanceofedu.bd  <- bd %>% filter(!is.na(NEWEdImpt))
importanceofedu.bd.plot<- ggplot(importanceofedu.bd,aes(y=factor(NEWEdImpt)))+  geom_bar(stat="count", width=0.7, fill="LightGreen")+
  labs(title= "Importance of formal education in the eyes of Respondentss from Bangladesh",
       y="Importance",
       x="Response") + theme_minimal()
importanceofedu.bd.plot

#Age of responder during first code

age.bd<- bd %>% filter(!is.na(Age1stCode)) 
age.bd.plot <- ggplot(age.bd,aes(y=Age1stCode))+  geom_bar(stat="Count", width=0.7, fill="LightGreen")+
  
  labs(title= "Age of responder during first code from Bangladesh",
       y="Age during first code",
       x="Response") + theme_minimal()
age.bd.plot

#total years of coding 
YearsCode.bd <- bd  %>% filter(!is.na(YearsCode)) 

YearsOfCoding.bd.plot <- ggplot(YearsCode.bd,aes(y=YearsCode))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Total years of coding of responder from Bangladesh",
       y="Total Years of Code",
       x="Response") + theme_minimal()
YearsOfCoding.bd.plot


#coding professionally as part of job
YearsCodeProf.bd <- bd  %>% filter(!is.na(YearsCodePro))

YearsOfCodingprof.bd.plot <- ggplot(YearsCodeProf.bd,aes(y=YearsCodePro))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Total years of coding professionally of responder from Bangladesh",
       y="Total Years of Code professionaly",
       x="Response") + theme_minimal()
YearsOfCodingprof.bd.plot


#job satisfaction

Jobsatifsaction.bd <- bd  %>% filter(!is.na(JobSat))

Jobsatifsaction.bd.plot <- ggplot(Jobsatifsaction.bd,aes(y=JobSat))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Job Satisfaction of responder from Bangladesh",
       y="Level of Satisfaction",
       x="Response") + theme_minimal()
Jobsatifsaction.bd.plot



#Organization size of the responders employing company
OrgSize.bd <- bd  %>% filter(!is.na(OrgSize))
OrgSize.bd.plot <- ggplot(OrgSize.bd,aes(y=OrgSize))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Organization size of the responders' employing company from Bangladesh",
       y="Size of the company",
       x="Response") + theme_minimal()
OrgSize.bd.plot

#currency used by responder

Currency.bd <- bd  %>% filter(!is.na(CurrencySymbol))
Currency.bd.plot <- ggplot(Currency.bd,aes(y=CurrencySymbol))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Currency used by responders from Bangladesh",
       y="Currency",
       x="Response") + theme_minimal()
Currency.bd.plot

#Work hours per week of responders from Bangladesh
WorkWeekHrs.bd <- bd  %>% filter(!is.na(WorkWeekHrs))
WorkWeekHrs.bd.plot <- ggplot(WorkWeekHrs.bd,aes(y=WorkWeekHrs))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Work hours per week of responders from Bangladesh",
       y="Work hour per week",
       x="Response") + theme_minimal()
WorkWeekHrs.bd.plot

#Overtime dony by responders from Bangladesh
overtime.bd <- bd  %>% filter(!is.na(NEWOvertime))
overtime.bd.plot <- ggplot(overtime.bd,aes(y=NEWOvertime))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Overtime dony by responders from Bangladesh",
       y="Amount",
       x="Response") + theme_minimal()
overtime.bd.plot


#Onboarding process evaluation by responders
onboarding.bd <- bd  %>% filter(!is.na(NEWOnboardGood))
onboarding.bd.plot <- ggplot(onboarding.bd,aes(y=NEWOnboardGood))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Onboarding process evaluation by responders from Bangladesh",
       y="Criteria",
       x="Response") + theme_minimal()
onboarding.bd.plot


#dedicated DevOps person in responders' company from Bangladesh
NEWDevOps.bd <- bd  %>% filter(!is.na(NEWDevOps))
NEWDevOps.bd.plot <- ggplot(NEWDevOps.bd,aes(y=NEWDevOps))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "dedicated DevOps person in responders' company from Bangladesh",
       y="Yes/No",
       x="Response") + theme_minimal()
NEWDevOps.bd.plot



#importance if the practice of DevOps to scaling software development

NEWDevOpsimportance.bd <- bd  %>% filter(!is.na(NEWDevOpsImpt))
NEWDevOpsimportance.bd.plot <- ggplot(NEWDevOpsimportance.bd,aes(y=NEWDevOpsImpt))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "importance of the practice of DevOps to scaling software development",
       y="Importance of the practice of DevOps",
       x="Response") + theme_minimal()
NEWDevOpsimportance.bd.plot


#current job seeking status

jobseek.bd <- bd  %>% filter(!is.na(JobSeek))
jobseek.bd.plot <- ggplot(jobseek.bd,aes(y=JobSeek))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Current Job Seeking Status of Respondents of Bangladesh",
       y="Status",
       x="Response") + theme_minimal()
jobseek.bd.plot

#Frequency of learning new language/framework 
NEWLearn.bd <- bd  %>% filter(!is.na(NEWLearn))
NEWLearn.bd.plot <- ggplot(NEWLearn.bd,aes(y=NEWLearn))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Frequency of learning new language/framework of Respondents of Bangladesh",
       y="Frequency",
       x="Response") + theme_minimal()
NEWLearn.bd.plot

#Operating system used by the Respondents
OS.bd<- bd  %>% filter(!is.na(OpSys))
OS.bd.plot <- ggplot(OS.bd,aes(y=OpSys))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Operating system used by the Respondents of Bangladesh",
       y="Frequency",
       x="Response") + theme_minimal()
OS.bd.plot

#influence of respondent in organization
influence.bd<- bd  %>% filter(!is.na(PurchaseWhat))
influence.bd.plot <- ggplot(influence.bd,aes(y=PurchaseWhat))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Influence of respondent in organization",
       y="Influence",
       x="Response") + theme_minimal()
influence.bd.plot


#influence of respondent in organization
prevVisitLink.bd<- bd  %>% filter(!is.na(NEWPurpleLink))
prevVisitLink.bd.plot <- ggplot(prevVisitLink.bd,aes(y=NEWPurpleLink))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Previously visited link induced response",
       y="Response",
       x="Count") + theme_minimal()
prevVisitLink.bd.plot

#frequency of stackoverflow visit

SOVisitFreq.bd<- bd  %>% filter(!is.na(SOVisitFreq))
SOVisitFreq.bd.plot <- ggplot(SOVisitFreq.bd,aes(y=SOVisitFreq))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Frequency of stackoverflow visit Among respondents",
       y="Frequency",
       x="Response") + theme_minimal()
SOVisitFreq.bd.plot

#Existence of stackoverflow account
SOAccount.bd<-bd  %>% filter(!is.na(SOAccount))
SOAccount.bd.plot <- ggplot(SOAccount.bd,aes(y=SOAccount))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Existence of stackoverflow account Among respondents",
       y="Frequency",
       x="Response") + theme_minimal()
SOAccount.bd.plot


#Frequency of participating in SO Q/A
frequencyQA.bd<-bd  %>% filter(!is.na(SOPartFreq))
frequencyQA.bd.plot <- ggplot(frequencyQA.bd,aes(y=SOPartFreq))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Frequency of participating in stackoverflow Q/A Among respondents",
       y="Frequency",
       x="Response") + theme_minimal()
frequencyQA.bd.plot

#Community response in SO

CommResponse.bd<-bd  %>% filter(!is.na(SOComm))
CommResponse.bd.plot <- ggplot(CommResponse.bd,aes(y=SOComm))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Community response of stackoverflow Among respondents",
       y="Frequency",
       x="Response") + theme_minimal()
CommResponse.bd.plot

#member of online developer community

CommResponseOnline.bd<-bd  %>% filter(!is.na(NEWOtherComms))
CommResponseOnline.bd.plot <- ggplot(CommResponseOnline.bd,aes(y=NEWOtherComms))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Community response in other online developer community Among respondents",
       y="Member of other online developer comminity",
       x="Response") + theme_minimal()
CommResponseOnline.bd.plot

#comparison of welcome feeling in SO

Comparisonchange.bd<-bd  %>% filter(!is.na(WelcomeChange))
Comparisonchange.bd.plot <- ggplot(Comparisonchange.bd,aes(y=WelcomeChange))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "comparison of welcome feeling in stackoverflow Among respondents",
       y="comparison of welcome feeling",
       x="Response") + theme_minimal()
Comparisonchange.bd.plot


#OFFtopic relaxations on StackOverflow

OffTopic.bd<-bd  %>% filter(!is.na(NEWOffTopic))
OffTopic.bd.plot <- ggplot(OffTopic.bd,aes(y=NEWOffTopic))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Should OFFtopic relaxations be increased in StackOverflow",
       y="Options",
       x="Response") + theme_minimal()
OffTopic.bd.plot


#age of responder
age.bd<-bd  %>% filter(!is.na(Age))
age.bd.plot <- ggplot(OffTopic.bd,aes(y=Age))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Age of respondent",
       y="age",
       x="Response") + theme_minimal()
age.bd.plot


#transgender among respondents
Trans.bd<-bd  %>% filter(!is.na(Trans))
Trans.bd.plot <- ggplot(Trans.bd,aes(y=Trans))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Transgender among respondent",
       y="Yes/No",
       x="Response") + theme_minimal()
Trans.bd.plot

#survey length
SurveyLength.bd<-bd  %>% filter(!is.na(SurveyLength))
SurveyLength.bd.plot <- ggplot(SurveyLength.bd,aes(y=SurveyLength))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Survey Length Query",
       y="Criteria",
       x="Response") + theme_minimal()
SurveyLength.bd.plot

#survey difficulty
SurveyEase.bd<-bd  %>% filter(!is.na(SurveyEase))
SurveyEase.bd.plot <- ggplot(SurveyEase.bd,aes(y=SurveyEase))+  geom_bar(stat="Count", width=.5, fill="LightGreen")+
  labs(title= "Survey Ease Query",
       y="Criteria",
       x="Response") + theme_minimal()
SurveyEase.bd.plot





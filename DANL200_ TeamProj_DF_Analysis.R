sleephlth <- read.csv("~/SUNY Geneseo/Classes/Fall 2023/DANL 200 (Team Pr)/dan-noone.github.io/Sleep_health_and_lifestyle_dataset.csv")
library(tidyverse)

##Summary Stats
slphlth_sum <- skimr::skim(sleephlth)
slphlth_sum

##Relationship between BMI and Sleep Duration
View(sleephlth)
ggplot(data = sleephlth)+
  geom_bar(mapping = aes(x = BMI.Category, y = stat(prop), group = 1)) #Bar chart depicting the proportion of each 
                                                                          #BMI category in DF

sh_bmi_sd <- sleephlth %>% 
  select(BMI.Category, Sleep.Duration) %>% 
  group_by(BMI.Category) %>% 
  summarise(n = n(),
            mean_sd = round(mean(Sleep.Duration), digits = 2)) %>% 
  mutate(prop_BMI = round(n/sum(n),digits = 2)) %>% 
  arrange(-n)
View(sh_bmi_sd)

##RL btwn SD and Sleep Disorder

#````
sh_sdis_sd <- sleephlth %>% 
  select(Sleep.Disorder, Sleep.Duration) %>% 
  group_by(Sleep.Disorder) %>% 
  summarise(n = n(),
            mean_sd = round(mean(Sleep.Duration),digits = 2)) %>% 
  arrange(-n) %>% 
  mutate(prop_SDis = round(n/sum(n), digits = 2))
View(sh_sdis_sd) # DF grouped by sleep disorder showing the frequency of each sleep disorder (or none), 
                  # the avg sleep duration for each, and the proportion of each sleep disorder in the DF
#````

#````
ggplot(data = sleephlth)+
  geom_histogram(mapping = aes(x = Sleep.Duration), bins = 25)+
  facet_wrap(.~Sleep.Disorder) #Histogram depicting the distribution of sleep duration for each sleep disorder (or none)
#````

#RL btwn Stress Level and Sleep Duration and Quality of Sleep

ggplot(data = sleephlth,
       mapping = aes(x = Sleep.Duration, y = Stress.Level))+
         geom_point(aes(alpha = 0.5))+
  geom_smooth(se = FALSE)+
  geom_smooth(method = lm, color = 'red', se = FALSE) 

ggplot(data = sleephlth,
       mapping = aes(x = Quality.of.Sleep, y = Stress.Level))+
  geom_point(aes(alpha = 0.5))+
  geom_smooth(se = FALSE)+
  geom_smooth(method = lm, color = 'red', se = FALSE) 



#RL btwn Physical Activity and Sleep Duration and Sleep Quality

ggplot(data = sleephlth,
       mapping = aes(x = Physical.Activity.Level, y = Sleep.Duration))+
  geom_point()+
  geom_smooth(se = F)+
  geom_smooth(method = lm, color = 'red')

ggplot(data = sleephlth,
       mapping = aes(x = Physical.Activity.Level, y = Quality.of.Sleep))+
  geom_point()+
  geom_smooth(se = F)+
  geom_smooth(method = lm, color = 'red')

#Sleep Duration & Occupation

ggplot(data = sleephlth,
       mapping = aes(x = Sleep.Duration))+
  geom_histogram(bins = 21)
       

ggplot(data = sleephlth,
       mapping = aes(x = Sleep.Duration))+
  geom_histogram(bins = 21)+
  facet_wrap(.~Occupation)

# Activity-R
#1 Install Package for Data manipulation and management

install.packages("tidyverse", dependencies=TRUE)
install.packages("ggplot2")
install.packages("modeldata")

#2 Load Package to Library/console


library(tidyverse)
library(dplyr)
library(ggplot2)
library(magrittr)
#3 View the imported data table in R Tab to work on.

view(COVID_DATA_Module_2)

#4 use summary functions to display basic statistical data of the COVID DATA TABLE 
summary(COVID_DATA_Module_2)

COVID_DATA_Module_2<- rename(COVID_DATA_Module_2,
                      "Deaths" = "Deaths - cumulative total per 100,000 population",
                      )
COVID_DATA_Module_2<- rename(COVID_DATA_Module_2,
                      "Total_Vaccine_Administered" = "Total Vaccine administered"
                      )

#5 add a new column with High vs Low vaccination rates based on the mean value of Deaths
COVID_DATA_Module_2 <- COVID_DATA_Module_2 %>%
  mutate(vaccination_rates = case_when(
    Deaths < 277.54 ~ 'High',
    Deaths >= 277.54 ~ 'Low'
  ))

#Check updated data table
view(COVID_DATA_Module_2)

#Split Data into Low and High vaccination rate tables

Vaccine_Rate_Data <- 
  split(COVID_DATA_Module_2,COVID_DATA_Module_2 $vaccination_rates)

#Calculate the Standard Deviation & Mean of the two formed groups
COVID_DATA_MODULE_2_VRATE<- COVID_DATA_Module_2 %>% 
  group_by(vaccination_rates) %>% 
  summarise(mean = mean(Deaths),sd = sd(Deaths))
 
print(COVID_DATA_MODULE_2_VRATE)

#Bar Graph Plot of Deaths - Cumulative total per 100,000 population

COVID_DATA_MODULE_2_VRATE %>% 
  ggplot(aes(vaccination_rates, mean)) +
  geom_bar(stat="identity", position="dodge", width = 1, aes(fill= vaccination_rates)) +
  geom_errorbar(aes(ymin= as.numeric(mean-sd), ymax= as.numeric(mean+sd)), width=0.1) +
  geom_point()+
  geom_label(aes(label = paste(round(mean, 1), "\ub1", round(sd))),nudge_x=-0.035,nudge_y = 15,
             size = 4,label.size = 0, label.r = unit(0, "pt"), fill = "#ebebeb") +
  labs(y = "Average Deaths - cumulative total per 100,000 population",
       x = "Low vs. High Vaccination Rates",
       fill="Vaccination Rates")

ANOVA<- aov(Deaths~vaccination_rates,COVID_DATA_Module_2)
summary(ANOVA)
View(ANOVA)
 

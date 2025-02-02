COVID_DATA_Module_2 %>% 
  ggplot(aes(Total_Vaccine_Administered, Deaths)) +
  geom_point(aes(colour=vaccination_rates))  +
  geom_smooth(method="lm") +
  scale_y_continuous(breaks = seq(0,600, by = 100))+
  scale_x_continuous(breaks = seq(0,250, by = 50)) +
  labs(y = "Deaths-cumulative total 
  per 100,000 population",
       x = "Total vaccine doses administered
       per 100 population")
        

Regression<- lm(Total_Vaccine_Administered ~ Deaths,
   COVID_DATA_Module_2)
summary(Regression)
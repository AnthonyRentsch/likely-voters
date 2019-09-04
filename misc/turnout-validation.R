# turnout validation plot for model broken out by different variables
# predictions is created in national_models.Rmd, either in logistic regression or random forest section
predictions %>% 
  # mutate(education = as.character(education)) %>% 
  # mutate(education = replace(education,education=='1','No HS')) %>% 
  # mutate(education = replace(education,education=='2','High School Graduate')) %>% 
  # mutate(education = replace(education,education=='3','Some College')) %>% 
  # mutate(education = replace(education,education=='4','2-Year')) %>% 
  # mutate(education = replace(education,education=='5','4-Year')) %>% 
  # mutate(education = replace(education,education=='6','Post-Grad')) %>% 
  mutate(race = as.character(race)) %>%
  mutate(race = replace(race,race=='1','White')) %>%
  mutate(race = replace(race,race=='2','Black')) %>%
  mutate(race = replace(race,race=='3','Hispanic')) %>%
  mutate(race = replace(race,race=='4','Asian')) %>%
  mutate(race = replace(race,race=='5','Native American')) %>%
  mutate(race = replace(race,race=='6','Other')) %>%
  # mutate(age = replace(age,age %in% 18:29,'18-29')) %>% 
  # mutate(age = replace(age,age %in% 30:39,'30-39')) %>% 
  # mutate(age = replace(age,age %in% 40:49,'40-49')) %>% 
  # mutate(age = replace(age,age %in% 50:64,'50-64')) %>% 
  # mutate(age = replace(age,age %in% 65:99,'65+')) %>% 
  group_by(race, voteprop_bucket = round(Voted*100, -1)) %>%
  summarise(turnout = mean(validated=='Voted'), num = n()) %>%
  ggplot(aes(x = voteprop_bucket)) +
  geom_smooth(aes(y = turnout*100), alpha = 0.5, se = F) +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = 'black') +
  coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
  scale_x_continuous(breaks = seq(0,100,by=10), labels = seq(0,100,by=10)) +
  facet_wrap(~race) +
  scale_color_brewer(palette = "Paired") +
  labs(x='Vote propensity bucket',y='Turnout') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), legend.title = element_blank())

# plot margin among validated voters by different variables
tmp <- pooled %>%
  mutate(age = replace(age,age %in% 18:29,'18-29')) %>% 
  mutate(age = replace(age,age %in% 30:39,'30-39')) %>% 
  mutate(age = replace(age,age %in% 40:49,'40-49')) %>% 
  mutate(age = replace(age,age %in% 50:64,'50-64')) %>% 
  mutate(age = replace(age,age %in% 65:99,'65+')) %>% 
  filter(year == 2016, !choice %in% c("I Won't Vote In This Election"), !is.na(choice), validated == 'Voted') %>%
  group_by(age, choice) %>%
  summarise(n = sum(weight)) %>%
  mutate(vote_share = n/sum(n))
tmp <- tmp %>%
  mutate(margin = 100*(vote_share - lag(vote_share, default=first(vote_share)))) %>%
  filter(choice == "Hillary Clinton (Democrat)") %>%
  select(age, margin)
tmp <- tmp %>% ungroup() %>% 
  # mutate(education = as.character(education)) %>% 
  # mutate(education = replace(education,education=='1','No HS')) %>% 
  # mutate(education = replace(education,education=='2','High School Graduate')) %>% 
  # mutate(education = replace(education,education=='3','Some College')) %>% 
  # mutate(education = replace(education,education=='4','2-Year')) %>% 
  # mutate(education = replace(education,education=='5','4-Year')) %>% 
  # mutate(education = replace(education,education=='6','Post-Grad'))
  # mutate(race = as.character(race)) %>% 
  # mutate(race = replace(race,race=='1','White')) %>% 
  # mutate(race = replace(race,race=='2','Black')) %>% 
  # mutate(race = replace(race,race=='3','Hispanic')) %>% 
  # mutate(race = replace(race,race=='4','Asian')) %>% 
  # mutate(race = replace(race,race=='5','Native American')) %>% 
  # mutate(race = replace(race,race=='6','Other'))

pal <- c("#FF0000", "#0000FF")
ggplot(data = tmp) +
  geom_col(aes(x = age, y = margin, fill = (margin >0))) +
  scale_fill_manual(values = pal) +
  labs(title = "", x = "", y = "Margin among validated voters") +
  guides(fill = FALSE)
  
  
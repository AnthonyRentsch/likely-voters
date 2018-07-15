# turnout validation plot for model broken out by education
# predictions is created in national_models.Rmd, either in logistic regression or random forest section
predictions %>% 
  mutate(education = as.character(education)) %>% 
  mutate(education = replace(education,education=='1','No HS')) %>% 
  mutate(education = replace(education,education=='2','High School Graduate')) %>% 
  mutate(education = replace(education,education=='3','Some College')) %>% 
  mutate(education = replace(education,education=='4','2-Year')) %>% 
  mutate(education = replace(education,education=='5','4-Year')) %>% 
  mutate(education = replace(education,education=='6','Post-Grad')) %>% 
  group_by(education, voteprop_bucket = round(response*100, -1)) %>%
  summarise(turnout = mean(validated=='Voted'), n = n()) %>%
  ggplot(aes(x = voteprop_bucket, y = turnout*100)) +
  geom_smooth(colour = '#0000FF', alpha = 0.5, se = F) +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = 'black') +
  coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
  scale_x_continuous(breaks = seq(0,100,by=10), labels = seq(0,100,by=10)) +
  facet_wrap(~education) +
  labs(x='Vote propensity bucket',y='Turnout')

# plot margin among validated voters by education
tmp <- pooled %>%
  filter(year == 2016, !choice %in% c("I Won't Vote In This Election"), !is.na(choice), validated == 'Voted') %>%
  group_by(education, choice) %>%
  summarise(n = sum(weight)) %>%
  mutate(vote_share = n/sum(n))
tmp <- tmp %>%
  mutate(margin = 100*(vote_share - lag(vote_share, default=first(vote_share)))) %>%
  filter(choice == "Hillary Clinton (Democrat)") %>%
  select(education, margin)
tmp <- tmp %>% ungroup() %>% 
  mutate(education = as.character(education)) %>% 
  mutate(education = replace(education,education=='1','No HS')) %>% 
  mutate(education = replace(education,education=='2','High School Graduate')) %>% 
  mutate(education = replace(education,education=='3','Some College')) %>% 
  mutate(education = replace(education,education=='4','2-Year')) %>% 
  mutate(education = replace(education,education=='5','4-Year')) %>% 
  mutate(education = replace(education,education=='6','Post-Grad'))

tmp$education <- factor(tmp$education, levels = c('No HS','High School Graduate','Some College',
                                                  '2-Year','4-Year','Post-Grad'))
pal <- c("#FF0000", "#0000FF")
ggplot(data = tmp) +
  geom_col(aes(x = education, y = margin, fill = (margin >0))) +
  scale_fill_manual(values = pal) +
  labs(title = "", x = "", y = "Margin among validated voters") +
  guides(fill = FALSE)
  
  
# take predictions df from either logistic regression or random forests section of national_models.Rmd
# replicate plot from Chapter 4 re: how much turnout could affect Obama vote
# dissertation here: https://academiccommons.columbia.edu/catalog/ac:177212	

turnout_rates <- seq(from = 0.01, to = 1, by = 0.01)

# don't calculate margin, calculate candidate vote shares
vote_choice_logreg <- function(turnout){
  predictions %>% 
    top_n(round(nrow(predictions)*turnout,0), wt = response) %>% 
    filter(choice %in% c("Donald Trump (Republican)","Hillary Clinton (Democrat)",
                         "Gary Johnson (Libertarian)","Jill Stein (Green)","Other",
                         "I'm Not Sure")) %>% 
    group_by(choice) %>% 
    summarise(n = sum(weight)) %>% 
    mutate(vote_share = round(n/sum(n)*100,2)) %>% 
    select(-n)
}

vote_share_turnout <- data.frame()
i <- 1
while(i <= length(turnout_rates)){
  temp <- vote_choice_logreg(turnout_rates[i])
  vote_share_turnout[i,1] <- temp$vote_share[temp$choice=='Hillary Clinton (Democrat)'] 
  vote_share_turnout[i,2] <- temp$vote_share[temp$choice=='Donald Trump (Republican)']
  vote_share_turnout[i,3] <- i
  i = i +1
}

# margin weighted by propensity score
vote_choice_logreg_weighted <- predictions %>% 
    filter(choice %in% c("Donald Trump (Republican)","Hillary Clinton (Democrat)",
                         "Gary Johnson (Libertarian)","Jill Stein (Green)","Other",
                         "I'm Not Sure")) %>% 
    group_by(choice) %>% 
    summarise(n = sum(weight*response)) %>% 
    mutate(vote_share = round(n/sum(n)*100,2)) %>% 
    select(-n)

weighted_vote_share <- data.frame()
weighted_vote_share[1,1] <- vote_choice_logreg_weighted$vote_share[temp$choice=='Hillary Clinton (Democrat)']
weighted_vote_share[1,2] <- vote_choice_logreg_weighted$vote_share[temp$choice=='Donald Trump (Republican)']

# visualize
vote_share_turnout <- vote_share_turnout %>% rename(clinton = V1, trump = V2, i = V3)
weighted_vote_share <- weighted_vote_share %>% rename(clinton = V1, trump = V2)

ggplot() +
  geom_rect(aes(xmin = 40, xmax = 60, ymin = 0, ymax = 100), fill = 'grey', colour = NA, alpha = 0.4) +
  geom_point(data = vote_share_turnout, 
             aes(x = turnout_rates[i]*100, y = clinton), colour = "blue", alpha = 0.75) +
  geom_hline(yintercept = weighted_vote_share$clinton, 
             lty = 2, size = 1.2, col = "blue", alpha = 0.3) +
  geom_point(data = vote_share_turnout, 
             aes(x = turnout_rates[i]*100, y = trump), colour = "red", alpha = 0.75) +
  geom_hline(yintercept = weighted_vote_share$trump, 
             lty = 2, size = 1.2, col = "red", alpha = 0.3) +
  coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
  labs(title = "Logistic regression: Perry-Gallup + demographic variables",
       x = "Turnout (%)",
       y = "Candidate vote share (%)") + 
  theme(plot.title = element_text(hjust = 0.5))

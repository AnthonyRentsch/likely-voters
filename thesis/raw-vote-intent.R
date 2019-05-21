
#create sheet with proportion responding "definitely will vote" and "already voted" by race and state

#2016
intent16 <- cces16 %>% group_by(state_abbreviation, racial_group) %>% 
  summarise(will_vote_already_voted = sum(CC16_364 == 1|CC16_364 == 3)/sum(!is.na(CC16_364)))
  
write.csv(intent16, file = "/Users/anthonyrentsch/Desktop/UMass/thesis/data/dataviz-charts-tables/voteintent16.csv")

#2012
intent12 <- cces12 %>% group_by(state_abbreviation, racial_group) %>% 
  filter(!is.na(CC354)) %>% 
  summarise(will_vote_already_voted = sum(CC354 == 1|CC354 == 3)/sum(!is.na(CC354)))
  
write.csv(intent12, file = "/Users/anthonyrentsch/Desktop/UMass/thesis/data/dataviz-charts-tables/voteintent12.csv")

#combine both lists into one
intent <- left_join(intent16, intent12, by = c("state_abbreviation","racial_group"))
names(intent) <- c("state_abbreviation","racial_group","x16","x12")

#calculate difference in proportions saying they will definitely vote or have already voted
intent$change <- intent$x16 - intent$x12

#visualize change in self-reported vote intetion for various racial groups
intent %>% ggplot(aes(state_abbreviation, change)) + 
  geom_point(aes(colour = change > 0)) +
  scale_colour_manual(name = 'PC1 > 0', values = setNames(c('blue','red'),c(T, F))) +
  geom_hline(aes(yintercept=0)) +
  labs(title = "Change in self-reported vote intention from 2012 to 2016", x = "", 
       y = "Change in proportion reporting they will definitely vote or have already voted") + 
  facet_wrap(~racial_group) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.position="none",
        plot.title = element_text(face="bold", hjust = 0.5))

#reorganize the Catalist and CCES data and join together to look at change in turnout v. reported intentions
catalist_new <- melt(catalistturnout)
names(catalist_new) <- c("State","state_abbreviation","variable","value")

intent$variable[intent$racial_group=="White"] <- "Change1216_White"
intent$variable[intent$racial_group=="Black"] <- "Change1216_Black"
intent$variable[intent$racial_group=="Hispanic"] <- "Change1216_Latino"
intent$variable[intent$racial_group=="Asian"] <- "Change1216_Asian"

turnout_intent <- left_join(intent, catalist_new, by = c("state_abbreviation","variable"))
turnout_intent$change <- turnout_intent$change * 100

turnout_intent1216 <- turnout_intent[, c(1,2,5,8)]
names(turnout_intent1216) <- c("state","race","intention_change","turnout_change")
turnout_intent1216 <- turnout_intent1216[!is.na(turnout_intent1216$turnout_change),]

#table comparing intent and Catalist turnout by state and racial group
write.csv(turnout_intent1216, 
          file = "/Users/anthonyrentsch/Desktop/UMass/thesis/data/dataviz-charts-tables/turnout_intent1216.csv",
          row.names = F)

# I would say that, after a quick glance, this does not seem to reveal much. 
# There are some cases that seem to provide strong evidence that self-reports could have predicted turnout - 
# in Wisconsin, for example, self-reported vote intention among black respondents dropped by 17% 
# while black turnout dropped by 12%. 
# In others, though, it was more muddled - 
# like in Michigan, where self-reports of black respondents indicated that they would vote at a higher rate, 
# but their turnout ended up dropping 12% between 2012 and 2016.
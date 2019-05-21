### This file will include all analyses for the main text of the paper
### Must run load.R before running this file

########################################################################
########################## 2016 Predictions ############################
########################################################################

# Create function for vote intention
validation_by_intent <- function(intention){
  willvote <- pooled %>% 
    filter(year == 2016, intent %in% intention)
  wontvote <- pooled %>% filter(year == 2016) %>% 
    anti_join(willvote, by = 'case_id')
  
  pred_voters <- willvote %>% 
    count(validated = (validated == 'Voted')) %>% 
    mutate(percent = round((n/sum(n))*100,2)) %>% 
    mutate(validated = replace(validated, validated == "FALSE", "No")) %>% 
    mutate(validated = replace(validated, validated == "TRUE", "Yes")) %>% 
    select(-n)
  
  pred_nonvoters <- wontvote %>% 
    count(validated = (validated == 'Voted')) %>% 
    mutate(percent = round((n/sum(n))*100,2)) %>% 
    mutate(validated = replace(validated, validated == "FALSE", "No")) %>% 
    mutate(validated = replace(validated, validated == "TRUE", "Yes")) %>% 
    select(-n)
  
  left_join(pred_voters, pred_nonvoters, by = "validated", suffix = c("_v","_nv")) %>% 
    rename(voters = percent_v, nonvoters = percent_nv)
}

# Graph True Positives/True Negatives based on vote intention question

# those who say they will definitely vote or have voted already
mod1 <- validation_by_intent(c(1,3))
# those who say they will definitely vote, have voted already, or will probably vote
mod2 <- validation_by_intent(c(1,2,3))
# those who say they will definitely vote, have voted already, will probably vote, or who are undecided
mod3 <- validation_by_intent(c(1,2,3,5))
# all respondents in the sample
mod4 <- validation_by_intent(c(1,2,3,4,5))

mods <- list(mod1, mod2, mod3, mod4)

df <- data.frame(model = c("Already voted + will definitely vote", "Already voted + will definitely or probably vote", "Already voted + will definitely or probably vote + undecided", "All respondents"),
                 true_positive = vector(length = 4),
                 #false_positive = vector(length = 5),
                 #false_negative = vector(length = 5),
                 true_negative = vector(length = 4))
i <- 1
while(i <= length(mods)){
  df$true_positive[i] <- mods[[i]]$voters[mods[[i]]$validated == "Yes"]
  #df$false_positive[i] <- mods[[i]]$voters[mods[[i]]$validated == "No"]
  #df$false_negative[i] <- mods[[i]]$nonvoters[mods[[i]]$validated == "Yes"]
  df$true_negative[i] <- mods[[i]]$nonvoters[mods[[i]]$validated == "No"]
  i = i +1
}

df$model <- factor(df$model, levels = df$model)

pdf(file="~/Dropbox/LikelyVoters/Journal article/Data/Figures/basic_cutoff.pdf", width=7, height=5)
df %>% gather(type, value, true_positive:true_negative) %>%
  mutate(type = replace(type, type == 'true_positive', 'True positive'),
         type = replace(type, type == 'true_negative', 'True negative')) %>% 
  ggplot(aes(factor(model), value, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "", title = "") +
  scale_fill_grey() +
  theme(legend.title=element_blank(), legend.position="bottom") + coord_flip()
dev.off()

# Create election predictions based on vote intention question

vote_choice_intent <- function(intention){
  pooled %>% 
    filter(year == 2016, intent %in% intention) %>% 
    filter(choice %in% c("Donald Trump (Republican)","Hillary Clinton (Democrat)",
                         "Gary Johnson (Libertarian)","Jill Stein (Green)","Other",
                         "I'm Not Sure")) %>% 
    group_by(choice) %>% 
    summarise(n = sum(weight)) %>% 
    mutate(vote_share = round(100*n/sum(n),2)) %>% 
    select(-n)
}
mod1 <- vote_choice_intent(c(1,3))
mod2 <- vote_choice_intent(c(1,2,3))
mod3 <- vote_choice_intent(c(1,2,3,5))
mod4 <- vote_choice_intent(c(1,2,3,4,5))

mods <- list(mod1, mod2, mod3, mod4)

df <- data.frame(model = c("Already voted + will definitely vote", "Already voted + will definitely or probably vote", "Already voted + will definitely or probably vote + undecided", "All respondents"),
                 margin = vector(length = 4))

i <- 1
while(i <= length(mods)){
  df$margin[i] = mods[[i]]$vote_share[mods[[i]]$choice == "Hillary Clinton (Democrat)"] - mods[[i]]$vote_share[mods[[i]]$choice == "Donald Trump (Republican)"]
  i = i + 1
}

df$model <- factor(df$model, levels = df$model)
df$bias <- df$margin - validated_margin16

fig1 <- df %>%  
  ggplot() +
  geom_col(aes(model, margin)) +
  coord_flip() +
  geom_hline(yintercept = 2.9, lty = 2, size = 1, colour = "red") +
  labs(x = "", y = "Clinton's predicted margin of victory")
pdf(file="~/Dropbox/LikelyVoters/Journal article/Data/Figures/basic_cutoff_margin.pdf", width=7, height=5)
fig1
dev.off()


# Use Perry Gallup with cutoffs 

validation_by_pg <- function(index) {
  willvote <- pooled %>% 
    filter(year == 2016, perry_gallup %in% index)
  wontvote <- pooled %>% filter(year == 2016) %>% 
    anti_join(willvote, by = "case_id")
  
  pred_voters <- willvote %>% 
    count(validated = (validated == 'Voted')) %>% 
    mutate(percent = round((n/sum(n))*100,2)) %>% 
    mutate(validated = replace(validated, validated == "FALSE", "No")) %>% 
    mutate(validated = replace(validated, validated == "TRUE", "Yes")) %>% 
    select(-n)
  
  pred_nonvoters <- wontvote %>% 
    count(validated = (validated == 'Voted')) %>% 
    mutate(percent = round((n/sum(n))*100,2)) %>% 
    mutate(validated = replace(validated, validated == "FALSE", "No")) %>% 
    mutate(validated = replace(validated, validated == "TRUE", "Yes")) %>% 
    select(-n)
  
  left_join(pred_voters, pred_nonvoters, by = "validated", suffix = c("_v","_nv")) %>% 
    rename(voters = percent_v, nonvoters = percent_nv)
}
mod1 <- validation_by_pg(6)
mod2 <- validation_by_pg(c(6,5))
mod3 <- validation_by_pg(c(6,5,4))
mod4 <- validation_by_pg(c(6,5,4,3))
mod5 <- validation_by_pg(c(6,5,4,3,2))
mod6 <- validation_by_pg(c(6,5,4,3,2,1))

mods <- list(mod1, mod2, mod3, mod4, mod5, mod6)

df <- data.frame(model = c("6s","6s and 5s","6s, 5s, and 4s","6s, 5s, 4s, and 3s", 
                           "6s, 5s, 4s, 3s, and 2s","All"),
                 true_positive = vector(length = 6),
                 true_negative = vector(length = 6))
i <- 1
while(i <= length(mods)){
  df$true_positive[i] <- mods[[i]]$voters[mods[[i]]$validated == "Yes"]
  df$true_negative[i] <- mods[[i]]$nonvoters[mods[[i]]$validated == "No"]
  i = i +1
}

df$model <- factor(df$model, levels = df$model)

pdf(file="~/Dropbox/LikelyVoters/Journal article/Data/Figures/pg_cutoff.pdf", width=7, height=5)

df %>% gather(type, value, true_positive:true_negative) %>%
  mutate(type = replace(type, type == 'true_positive', 'True positive rate'),
         type = replace(type, type == 'true_negative', 'True negative rate')) %>% 
  ggplot(aes(factor(model), value, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "", title = "") + 
  scale_fill_grey() +
  theme(legend.title=element_blank())
dev.off()

vote_choice_pg <- function(index){
  pooled %>% 
    filter(year == 2016, perry_gallup %in% index) %>% 
    filter(choice %in% c("Donald Trump (Republican)","Hillary Clinton (Democrat)",
                         "Gary Johnson (Libertarian)","Jill Stein (Green)","Other",
                         "I'm Not Sure")) %>% 
    group_by(choice) %>% 
    summarise(n = sum(weight)) %>% 
    mutate(vote_share = round(n/sum(n)*100,2)) %>% 
    select(-n)
}

mod1 <- vote_choice_pg(6)
mod2 <- vote_choice_pg(c(6,5))
mod3 <- vote_choice_pg(c(6,5,4))
mod4 <- vote_choice_pg(c(6,5,4,3))
mod5 <- vote_choice_pg(c(6,5,4,3,2))
mod6 <- vote_choice_pg(c(6,5,4,3,2,1))

mods <- list(mod1, mod2, mod3, mod4, mod5, mod6)

df <- data.frame(model = c("6s","6s and 5s","6s, 5s, and 4s","6s, 5s, 4s, and 3s", 
                           "6s, 5s, 4s, 3s, and 2s","All"),
                 margin = vector(length = 6))

i <- 1
while(i <= length(mods)){
  df$margin[i] = mods[[i]]$vote_share[mods[[i]]$choice == "Hillary Clinton (Democrat)"] - mods[[i]]$vote_share[mods[[i]]$choice == "Donald Trump (Republican)"]
  i = i + 1
}

df$model <- factor(df$model, levels = df$model)
df$bias <- df$margin - validated_margin16

fig5 <- df %>%  
  ggplot() +
  geom_col(aes(model, margin)) +
  coord_flip() +
  labs(x = "", y = "Clinton's predicted margin of victory") +
  geom_hline(yintercept = validated_margin16, lty = 2, size = 1, colour = "red")
pdf(file="~/Dropbox/LikelyVoters/Journal article/Data/Figures/pg_cutoff_margin.pdf", width=7, height=5)
fig5
dev.off()

# Just show how many of each group there are
weighted2016 <- svydesign(id=~1,data=subset(pooled, year=="2016"), weights=~weight)
prop.table(svytable(~perry_gallup, weighted2016))

######### Probabilistic models

####### Perry Gallup Probabilistic

# create training set 
train <- pooled %>% filter(year %in% c(2008,2010,2012,2014))
test <- pooled %>% filter(year == 2016)

# create eligible variable
# train
train$eligible[train$year %in% c(2008, 2012) & train$age < 22] <- 0
train$eligible[train$year %in% c(2008, 2012) & train$age >= 22] <- 1
train$eligible[train$year %in% c(2010, 2014) & train$age < 20] <- 0
train$eligible[train$year %in% c(2010, 2014) & train$age >= 20] <- 1
# test
test$eligible[test$age < 22] <- 0
test$eligible[test$age >= 22] <- 1

# run model
formula <- as.formula(validated ~ vote_history + intent + interest + registration + 
                        eligible)
set.seed(111)
model <- randomForest(formula, data = train, importance=TRUE)

pdf(file="~/Dropbox/LikelyVoters/Journal article/Data/Figures/vip_pg.pdf", width=5, height=4, onefile = F)
varImpPlot(model, main="Perry Gallup Index Only", type=1)
dev.off()

# apply the models to test data
predictions <- cbind(test, predict(model, newdata = test, type = "prob"))
predictions <- as.data.frame(predictions)

# Print implied turnout
mean(predictions$Voted*predictions$weight)

# Histogram of propensity scores
hist1 <- ggplot(predictions) +
  geom_histogram(aes(x = Voted), position = "identity", bins = 20) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(title = paste0("Perry-Gallup Only"),
       x = "Vote propensity score")


# margin weighted by propensity score
margin <- predictions %>% 
    filter(choice %in% c("Donald Trump (Republican)","Hillary Clinton (Democrat)",
                         "Gary Johnson (Libertarian)","Jill Stein (Green)","Other",
                         "I'm Not Sure")) %>% 
    group_by(choice) %>% 
    summarise(n = sum(weight*Voted)) %>% 
    mutate(vote_share = round(n/sum(n)*100,2)) %>% 
    select(-n)

# Print Dem Bias
print((margin$vote_share[margin$choice=='Hillary Clinton (Democrat)'] - margin$vote_share[margin$choice=='Donald Trump (Republican)'])-validated_margin16)

# Graph of prediction accuracy by vote propensity
fig12a <- predictions %>% 
  group_by(voteprop_bucket = round(Voted*100, -1)) %>% # **
  summarise(turnout = mean(validated=='Voted'), num = n()) %>%
  ggplot(aes(x = voteprop_bucket)) +
  geom_smooth(aes(y = turnout*100), colour = '#0000FF', alpha = 0.5, se = F) +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = 'black') +
  coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
  scale_x_continuous(breaks = seq(0,100,by=10), labels = seq(0,100,by=10)) +
  labs(x='Vote propensity bucket',y='Turnout', title='Perry-Gallup Only') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())

## ROC curve
predictions_roc <- cbind(test, predict(model, newdata = test, type = "prob"))
predictions_roc$validated <- ifelse(predictions_roc$validated == 'Voted', 1, 0)
auc(predictions_roc$validated, predictions_roc$Voted)
roc_pg16 <- roc(predictions_roc$validated, predictions_roc$Voted)

# resaving predictions df to use for simulations later
predictions_sim <- predictions


####### Perry Gallup + Demos

# create training set 
train <- pooled %>% filter(year %in% c(2008,2010,2012,2014))
test <- pooled %>% filter(year == 2016)

# run model
formula <- as.formula(validated ~ vote_history + intent + interest + registration + 
                        gender + age + race + education + income_new + 
                        partisan_strength )
set.seed(111)
model <- randomForest(formula, data = train, importance=TRUE)

pdf(file="~/Dropbox/LikelyVoters/Journal article/Data/Figures/vip_demos.pdf", width=5, height=4, onefile = F)
varImpPlot(model, main = "Perry Gallup + Demographics", type=1)
dev.off()

# apply the models to test data
predictions <- cbind(test, predict(model, newdata = test, type = "prob"))
predictions <- as.data.frame(predictions)

# Implied turnout
mean(predictions$Voted*predictions$weight)

# Histogram of predictions
hist2 <- ggplot(predictions) +
  geom_histogram(aes(x = Voted), position = "identity", bins = 20) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 40000)) +
  labs(title = paste0("Perry-Gallup + Demographics"),
       x = "Vote propensity score")


# margin weighted by propensity score
margin <- predictions %>% 
  filter(choice %in% c("Donald Trump (Republican)","Hillary Clinton (Democrat)",
                       "Gary Johnson (Libertarian)","Jill Stein (Green)","Other",
                       "I'm Not Sure")) %>% 
  group_by(choice) %>% 
  summarise(n = sum(weight*Voted)) %>% 
  mutate(vote_share = round(n/sum(n)*100,2)) %>% 
  select(-n)

# Print Dem Bias
print((margin$vote_share[margin$choice=='Hillary Clinton (Democrat)'] - margin$vote_share[margin$choice=='Donald Trump (Republican)'])-validated_margin16)

# Graph of prediction accuracy by vote propensity
fig12b <- predictions %>% 
  group_by(voteprop_bucket = round(Voted*100, -1)) %>% # **
  summarise(turnout = mean(validated=='Voted'), num = n()) %>%
  ggplot(aes(x = voteprop_bucket)) +
  geom_smooth(aes(y = turnout*100), colour = '#0000FF', alpha = 0.5, se = F) +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = 'black') +
  coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
  scale_x_continuous(breaks = seq(0,100,by=10), labels = seq(0,100,by=10)) +
  labs(x='Vote propensity bucket',y='Turnout', title='Perry-Gallup + Demographics') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())

## ROC curve
predictions_roc <- cbind(test, predict(model, newdata = test, type = "prob"))
predictions_roc$validated <- ifelse(predictions_roc$validated == 'Voted', 1, 0)
auc(predictions_roc$validated, predictions_roc$Voted)
roc_pgad16 <- roc(predictions_roc$validated, predictions_roc$Voted)

# joining these new prop scores with simulations df I created before
predictions_pgad_to_join <- predictions %>% select(case_id, Voted) %>% 
  rename(case_id=case_id, Voted_pgad=Voted)
predictions_sim <- predictions_sim %>% left_join(predictions_pgad_to_join, by="case_id")


## Combine graphs
pdf(file="~/Dropbox/LikelyVoters/Journal article/Data/Figures/prob_accuracy.pdf", width=6.5, height=4, onefile = F)
ggarrange(fig12a, fig12b, ncol=2, common.legend = T, legend = "bottom")
dev.off()

## Combine graphs
pdf(file="~/Dropbox/LikelyVoters/Journal article/Data/Figures/histograms.pdf", width=7, height=3.5, onefile = F)
ggarrange(hist1, hist2, ncol=2)
dev.off()


###############
# SIMIULATIONS
##############

## simulations of smaller n sample sizes
## compare all 5 approaches for 1000 simulations for 4 different sample sizes
# 1. Already + definitely will vote
# 2. PG 6s
# 3. PG 6s and 5s
# 4. Weight with PG model
# 5. Weight with PGaD model
set.seed(435)
margins_sim <- data.frame()
for (n in c(500, 800, 1000, 1200)) {
  for (i in 1:1000) {
    tmp <- predictions_sim %>% sample_n(n)
    # Already + definitly will vote (intent == 1 or 3)
    vote_share_already_def <-tmp %>% 
      filter(choice %in% c("Donald Trump (Republican)","Hillary Clinton (Democrat)",
                           "Gary Johnson (Libertarian)","Jill Stein (Green)","Other",
                           "I'm Not Sure"),
             intent %in% c(1,3)) %>% 
      group_by(choice) %>% 
      summarise(n = sum(weight)) %>% 
      mutate(vote_share = round(n/sum(n)*100,2)) %>% 
      select(-n)
    margin_already_def <- vote_share_already_def$vote_share[vote_share_already_def$choice=='Hillary Clinton (Democrat)'] - 
      vote_share_already_def$vote_share[vote_share_already_def$choice=='Donald Trump (Republican)']
    # PG 6s
    vote_share_pg_6s <-tmp %>% 
      filter(choice %in% c("Donald Trump (Republican)","Hillary Clinton (Democrat)",
                           "Gary Johnson (Libertarian)","Jill Stein (Green)","Other",
                           "I'm Not Sure"),
             perry_gallup == 6) %>% 
      group_by(choice) %>% 
      summarise(n = sum(weight)) %>% 
      mutate(vote_share = round(n/sum(n)*100,2)) %>% 
      select(-n)
    margin_pg_6s <- vote_share_pg_6s$vote_share[vote_share_pg_6s$choice=='Hillary Clinton (Democrat)'] - 
      vote_share_pg_6s$vote_share[vote_share_pg_6s$choice=='Donald Trump (Republican)']
    # PG 5s
    vote_share_pg_6s_5s <-tmp %>% 
      filter(choice %in% c("Donald Trump (Republican)","Hillary Clinton (Democrat)",
                           "Gary Johnson (Libertarian)","Jill Stein (Green)","Other",
                           "I'm Not Sure"),
             perry_gallup %in% c(6,5)) %>% 
      group_by(choice) %>% 
      summarise(n = sum(weight)) %>% 
      mutate(vote_share = round(n/sum(n)*100,2)) %>% 
      select(-n)
    margin_pg_6s_5s <- vote_share_pg_6s_5s$vote_share[vote_share_pg_6s_5s$choice=='Hillary Clinton (Democrat)'] - 
      vote_share_pg_6s_5s$vote_share[vote_share_pg_6s_5s$choice=='Donald Trump (Republican)']
    # Weight with PG model
    vote_share_pg_weighted <- tmp %>% 
      filter(choice %in% c("Donald Trump (Republican)","Hillary Clinton (Democrat)",
                           "Gary Johnson (Libertarian)","Jill Stein (Green)","Other",
                           "I'm Not Sure")) %>% 
      group_by(choice) %>% 
      summarise(n = sum(weight*Voted)) %>% 
      mutate(vote_share = round(n/sum(n)*100,2)) %>% 
      select(-n)
    margin_pg_weighted <- vote_share_pg_weighted$vote_share[vote_share_pg_weighted$choice=='Hillary Clinton (Democrat)'] - 
      vote_share_pg_weighted$vote_share[vote_share_pg_weighted$choice=='Donald Trump (Republican)']
    # Weight with PGaD model
    vote_share_pgad_weighted <- tmp %>% 
      filter(choice %in% c("Donald Trump (Republican)","Hillary Clinton (Democrat)",
                           "Gary Johnson (Libertarian)","Jill Stein (Green)","Other",
                           "I'm Not Sure")) %>% 
      group_by(choice) %>% 
      summarise(n = sum(weight*Voted_pgad)) %>% 
      mutate(vote_share = round(n/sum(n)*100,2)) %>% 
      select(-n)
    margin_pgad_weighted <- vote_share_pgad_weighted$vote_share[vote_share_pgad_weighted$choice=='Hillary Clinton (Democrat)'] - 
      vote_share_pgad_weighted$vote_share[vote_share_pgad_weighted$choice=='Donald Trump (Republican)']
    # Compute margin among validated voters in the simulated sample
    validated_margin_tmp <- tmp %>%
      filter(year == 2016, !choice %in% c("I Won't Vote In This Election"), !is.na(choice), validated == 'Voted') %>%
      group_by(choice) %>%
      summarise(n = sum(weight)) %>%
      mutate(vote_share = n/sum(n))
    margin_validated <- validated_margin_tmp$vote_share[validated_margin_tmp$choice=='Hillary Clinton (Democrat)'] - 
      validated_margin_tmp$vote_share[validated_margin_tmp$choice=='Donald Trump (Republican)']
    # add to margins_sim df
    margins_sim <- rbind(margins_sim, data.frame(margin_already_def=margin_already_def,
                                                 margin_pg_6s=margin_pg_6s,
                                                 margin_pg_6s_5s=margin_pg_6s_5s,
                                                 margin_pg_weighted=margin_pg_weighted,
                                                 margin_pgad_weighted=margin_pgad_weighted,
                                                 margin_validated=margin_validated,
                                                 sample_size=n))
  }
}

## print metrics
# average absolute error
margins_sim %>% group_by(sample_size) %>% 
  summarise(ad = sum(abs(margin_already_def-margin_validated))/1000,
            pg6 = sum(abs(margin_pg_6s-margin_validated))/1000,
            pg65 = sum(abs(margin_pg_6s_5s-margin_validated))/1000,
            pg_weighted = sum(abs(margin_pg_weighted-margin_validated))/1000,
            pgad_weighted = sum(abs(margin_pgad_weighted-margin_validated))/1000)
# average bias
margins_sim %>% group_by(sample_size) %>% 
  summarise(ad = sum(margin_already_def-margin_validated)/1000,
            pg6 = sum(margin_pg_6s-margin_validated)/1000,
            pg65 = sum(margin_pg_6s_5s-margin_validated)/1000,
            pg_weighted = sum(margin_pg_weighted-margin_validated)/1000,
            pgad_weighted = sum(margin_pgad_weighted-margin_validated)/1000)


# prop of simulations in which margin was w/in 3 points
margins_sim %>% group_by(sample_size) %>% 
  summarise(ad = sum(abs(margin_already_def-margin_validated) < 3)/1000,
            pg6 = sum(abs(margin_pg_6s-margin_validated) < 3)/1000,
            pg65 = sum(abs(margin_pg_6s_5s-margin_validated) < 3)/1000,
            pg_weighted = sum(abs(margin_pg_weighted-margin_validated) < 3)/1000,
            pgad_weighted = sum(abs(margin_pgad_weighted-margin_validated) < 3)/1000)
# prop of simulations in which margin was w/in 5 points
margins_sim %>% group_by(sample_size) %>% 
  summarise(ad = sum(abs(margin_already_def-margin_validated) < 5)/1000,
            pg6 = sum(abs(margin_pg_6s-margin_validated) < 5)/1000,
            pg65 = sum(abs(margin_pg_6s_5s-margin_validated) < 5)/1000,
            pg_weighted = sum(abs(margin_validated-validated_margin16) < 5)/1000,
            pgad_weighted = sum(abs(margin_pgad_weighted-margin_validated) < 5)/1000)
# calculated which method produces best predictions for each simulation
margins_min_pred <- margins_sim %>% 
  mutate(ad = abs(margin_already_def-margin_validated),
         pg6 = abs(margin_pg_6s-margin_validated),
         pg65 = abs(margin_pg_6s_5s-margin_validated),
         pg_weighted = abs(margin_pg_weighted-margin_validated),
         pgad_weighted = abs(margin_pgad_weighted-margin_validated))

cols_of_interest <- c('ad','pg6','pg65','pg_weighted','pgad_weighted')
margins_min_pred$min_method <- apply(margins_min_pred[,cols_of_interest], 1, function(x) cols_of_interest[which.min(x)])

margins_min_pred %>% group_by(sample_size) %>% count(min_method) %>% mutate(prop_best = n/sum(n))

# create plot of distributions of margins from simulations for each method
pdf(file="~/Dropbox/LikelyVoters/Journal article/Data/Figures/distributions_simulated_margins.pdf", width=8, height=5, onefile = F)
ggplot(margins_sim) +
  geom_density(aes(x=margin_already_def, colour='Already or definitely will vote'), fill=NA) +
  geom_density(aes(x=margin_pg_6s, colour='Perry-Gallup 6s'), fill=NA) +
  geom_density(aes(x=margin_pg_6s_5s, colour='Perry-Gallup 6s and 5s'), fill=NA) +
  geom_density(aes(x=margin_pg_weighted, colour='Weighted by PG model'), fill=NA) +
  geom_density(aes(x=margin_pgad_weighted, colour='Weighted by PGaD model'), fill=NA) +
  geom_vline(xintercept = validated_margin16, col = 'black') +
  facet_wrap(~sample_size) +
  labs(x='Clinton margin in simulated sample', y='') +
  scale_color_brewer(palette='Set1') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        legend.title = element_blank())
dev.off()
##



########################################################################
####################### 2016 State Predictions #########################
########################################################################

# create margin-calcuating function
vote_choice_intent_state <- function(location, intention){
  pooled %>% 
    filter(state == location, intent %in% intention) %>% 
    filter(choice %in% c("Donald Trump (Republican)","Hillary Clinton (Democrat)",
                         "Gary Johnson (Libertarian)","Jill Stein (Green)","Other",
                         "I'm Not Sure")) %>% 
    group_by(choice) %>% 
    summarise(n = sum(weight)) %>% 
    mutate(vote_share = round(100*n/sum(n),2)) %>% 
    select(-n)
}

# other set up 
all_states <- unique(pooled$state)
df <- data.frame(model = vector(length = 0),
                 state = vector(length = 0),
                 margin = vector(length = 0))
# loop
for(i in all_states){
  mod1 <- vote_choice_intent_state(i, c(1,3))
  mod2 <- vote_choice_intent_state(i, c(1,2,3))
  mod3 <- vote_choice_intent_state(i, c(1,2,3,5))
  mod4 <- vote_choice_intent_state(i, c(1,2,3,4,5))
  
  mods <- list(mod1, mod2, mod3, mod4)
  
  temp <- data.frame(model = c("Already voted + will definitely vote", "Already voted + will definitely or probably vote", "Already voted + will definitely or probably vote + undecided", "All respondents"),
                     state = i,
                     margin = vector(length = 4))
  
  j <- 1
  while(j <= length(mods)){
    temp$margin[j] = mods[[j]]$vote_share[mods[[j]]$choice == "Hillary Clinton (Democrat)"] - mods[[j]]$vote_share[mods[[j]]$choice == "Donald Trump (Republican)"]
    j = j + 1
  }
  
  temp$model <- factor(temp$model, levels = temp$model)
  
  df <- rbind(df, temp)
}

# calculate actual margins in each state among validated voters
state_margins <- pooled %>% filter(year == 2016, validated == "Voted",
                                   choice %in% c("Donald Trump (Republican)",
                                                 "Hillary Clinton (Democrat)",
                                                 "Gary Johnson (Libertarian)",
                                                 "Jill Stein (Green)","Other",
                                                 "I'm Not Sure")) %>% 
  group_by(state, choice) %>%
  summarise(n = sum(weight)) %>% 
  mutate(vote_share = round(100*n/sum(n),2)) %>% 
  ungroup() %>% 
  group_by(state) %>% 
  mutate(margin = vote_share - lag(vote_share, default=first(vote_share))) %>% 
  ungroup() %>% 
  filter(choice == "Hillary Clinton (Democrat)") %>% 
  select(state, margin)

# combine validated voter margins and model margins
df <- left_join(df, state_margins, by = 'state') %>% 
  rename(model_margin = margin.x, validated_margin = margin.y)

df %>% group_by(model) %>% summarise(mean_error = mean(model_margin - validated_margin),
                                     MSE = mean((model_margin - validated_margin)^2),
                                     MAE = mean(abs(model_margin - validated_margin)))

#### Perry Gallup Cutoff models

# create function to calculate margin
vote_choice_pg_state <- function(location, index){
  pooled %>% 
    filter(state == location, perry_gallup %in% index) %>% 
    filter(choice %in% c("Donald Trump (Republican)","Hillary Clinton (Democrat)",
                         "Gary Johnson (Libertarian)","Jill Stein (Green)","Other",
                         "I'm Not Sure")) %>% 
    group_by(choice) %>% 
    summarise(n = sum(weight)) %>% 
    mutate(vote_share = round(n/sum(n)*100,2)) %>% 
    select(-n)
}

# set up work
all_states <- unique(pooled$state)
df <- data.frame(model = vector(length = 0),
                 state = vector(length = 0),
                 margin = vector(length = 0))

# loop
for(i in all_states){
  mod1 <- vote_choice_pg_state(i, c(6))
  mod2 <- vote_choice_pg_state(i, c(6,5))
  mod3 <- vote_choice_pg_state(i, c(6,5,4))
  mod4 <- vote_choice_pg_state(i, c(6,5,4,3))
  mod5 <- vote_choice_pg_state(i, c(6,5,4,3,2))
  
  mods <- list(mod1, mod2, mod3, mod4, mod5)
  
  temp <- data.frame(model = c("6s","6s and 5s","6s, 5s, and 4s", 
                               "6s, 5s, 4s, and 3s","6s, 5s, 4s, 3s, and 2s"),
                     state = i,
                     margin = vector(length = 5))
  
  j <- 1
  while(j <= length(mods)){
    temp$margin[j] = mods[[j]]$vote_share[mods[[j]]$choice == "Hillary Clinton (Democrat)"] - mods[[j]]$vote_share[mods[[j]]$choice == "Donald Trump (Republican)"]
    j = j + 1
  }
  
  temp$model <- factor(temp$model, levels = temp$model)
  df <- rbind(df, temp)
}

# combine validated voter margins and model margins
df <- left_join(df, state_margins, by = 'state') %>% 
  rename(model_margin = margin.x, validated_margin = margin.y)

# Show average bias, MSE, and MAE across states
df %>% group_by(model) %>% summarise(mean_error = mean(model_margin - validated_margin),
                                     MSE = mean((model_margin - validated_margin)^2),
                                     MAE = mean(abs(model_margin - validated_margin)))


##### Perry Gallup Probabilistic


##### Perry-Gallup only ######
# set up
predictions_all <- data.frame()
all_states <- unique(pooled$state)

# create models and apply them to test data for all states
set.seed(111)
for(i in all_states){
  # create training set 
  train <- pooled %>% filter(state == i, year %in% c(2008,2010,2012,2014))
  test <- pooled %>% filter(state == i, year == 2016)
  
  # run unweighted model
  formula <- as.formula(validated ~ vote_history + intent + interest + registration + 
                          eligible)

  model <- randomForest(formula, data = train)
  # apply the models to test data
  predictions <- cbind(test, predict(model, newdata = test, type = "prob"))
  predictions <- as.data.frame(predictions)
  predictions <- predictions %>% mutate(state = i)
  
  # all this data set to all
  predictions_all <- rbind(predictions_all, predictions)
}

vote_choice_logreg_weighted_state <- function(location){
  predictions_all %>% filter(state == location) %>%
    filter(choice %in% c("Donald Trump (Republican)","Hillary Clinton (Democrat)",
                         "Gary Johnson (Libertarian)","Jill Stein (Green)","Other",
                         "I'm Not Sure")) %>%
    group_by(choice) %>%
    summarise(n = sum(weight*Voted)) %>%
    mutate(vote_share = round(n/sum(n)*100,2)) %>%
    select(-n)
}

margin_by_weighted_turnout_all_pg <- data.frame() 

for (i in all_states){

  margin_by_weighted_turnout <- data.frame()
  temp <- vote_choice_logreg_weighted_state(i)
  margin_by_weighted_turnout[1,1] <- temp$vote_share[temp$choice=='Hillary Clinton (Democrat)'] - temp$vote_share[temp$choice=='Donald Trump (Republican)']
  margin_by_weighted_turnout[1,2] <- i

  margin_by_weighted_turnout_all_pg <- rbind(margin_by_weighted_turnout_all_pg, margin_by_weighted_turnout)
}

margin_by_weighted_turnout_all_pg <- margin_by_weighted_turnout_all_pg %>%
  rename(margin = V1, state = V2)

margin_by_weighted_turnout_all_pg <- margin_by_weighted_turnout_all_pg %>% 
  left_join(validated_margin16_state, by = 'state') %>% 
  rename(margin_predicted = margin.x, margin_validated = margin.y) %>% 
  mutate(dem_bias = margin_predicted - margin_validated)

# print average Dem bias across all states
mean(margin_by_weighted_turnout_all_pg$dem_bias)
# print MAE across all states
mean(abs(margin_by_weighted_turnout_all_pg$dem_bias))

pdf(file="~/Dropbox/LikelyVoters/Journal article/Data/Figures/state_margins_2016_pg.pdf", width=6.5, height=4, onefile = F)
ggplot(margin_by_weighted_turnout_all_pg) + 
  geom_point(aes(x = margin_predicted, y = margin_validated), alpha = 0.75) + 
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  labs(x = "Predicted margin of victory for Clinton", y = "Validated margin of victory for Clinton")
dev.off()


#### Perry-Gallup + demos #####

# set up
predictions_all <- data.frame()
all_states <- unique(pooled$state)

# create models and apply them to test data for all states
set.seed(111)
for(i in all_states){
  # create training set 
  train <- pooled %>% filter(state == i, year %in% c(2008,2010,2012,2014))
  test <- pooled %>% filter(state == i, year == 2016)
  
  # run unweighted model
  formula <- as.formula(validated ~ vote_history + intent + interest + registration + 
                          gender + age + race + education + income_new + 
                          partisan_strength + religiosity + marital_status +
                          residential_mobility)
  model <- randomForest(formula, data = train)
  # apply the models to test data
  predictions <- cbind(test, predict(model, newdata = test, type = "prob"))
  predictions <- as.data.frame(predictions)
  predictions <- predictions %>% mutate(state = i)
  
  # all this data set to all
  predictions_all <- rbind(predictions_all, predictions)
}

vote_choice_logreg_weighted_state <- function(location){
  predictions_all %>% filter(state == location) %>%
    filter(choice %in% c("Donald Trump (Republican)","Hillary Clinton (Democrat)",
                         "Gary Johnson (Libertarian)","Jill Stein (Green)","Other",
                         "I'm Not Sure")) %>%
    group_by(choice) %>%
    summarise(n = sum(weight*Voted)) %>%
    mutate(vote_share = round(n/sum(n)*100,2)) %>%
    select(-n)
}

margin_by_weighted_turnout_all_pg_dems <- data.frame() 

for (i in all_states){
  
  margin_by_weighted_turnout <- data.frame()
  temp <- vote_choice_logreg_weighted_state(i)
  margin_by_weighted_turnout[1,1] <- temp$vote_share[temp$choice=='Hillary Clinton (Democrat)'] - temp$vote_share[temp$choice=='Donald Trump (Republican)']
  margin_by_weighted_turnout[1,2] <- i
  
  margin_by_weighted_turnout_all_pg_dems <- rbind(margin_by_weighted_turnout_all_pg_dems, margin_by_weighted_turnout)
}

margin_by_weighted_turnout_all_pg_dems <- margin_by_weighted_turnout_all_pg_dems %>%
  rename(margin = V1, state = V2)

margin_by_weighted_turnout_all_pg_dems <- margin_by_weighted_turnout_all_pg_dems %>% 
  left_join(validated_margin16_state, by = 'state') %>% 
  rename(margin_predicted = margin.x, margin_validated = margin.y) %>% 
  mutate(dem_bias = margin_predicted - margin_validated)

# print average Dem bias across all states
mean(margin_by_weighted_turnout_all_pg_dems$dem_bias)
# print MAE across all states
mean(abs(margin_by_weighted_turnout_all_pg_dems$dem_bias))

pdf(file="~/Dropbox/LikelyVoters/Journal article/Data/Figures/state_margins_2016_pg_dems.pdf", width=6.5, height=4, onefile = F)
ggplot(margin_by_weighted_turnout_all_pg_dems) + 
  geom_point(aes(x = margin_predicted, y = margin_validated), alpha = 0.75) + 
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  labs(x = "Predicted margin of victory for Clinton", y = "Validated margin of victory for Clinton")
dev.off()



########################################################################
########################## 2014 Predictions ############################
########################################################################

# House vote preference margin among validated voters is -5.57 (D minus R)

# Create function for vote intention

validation_by_intent <- function(intention){
  willvote <- pooled %>% 
    filter(year == 2014, intent %in% intention)
  wontvote <- pooled %>% filter(year == 2014) %>% 
    anti_join(willvote, by = 'case_id')
  
  pred_voters <- willvote %>% 
    count(validated = (validated == 'Voted')) %>% 
    mutate(percent = round((n/sum(n))*100,2)) %>% 
    mutate(validated = replace(validated, validated == "FALSE", "No")) %>% 
    mutate(validated = replace(validated, validated == "TRUE", "Yes")) %>% 
    select(-n)
  
  pred_nonvoters <- wontvote %>% 
    count(validated = (validated == 'Voted')) %>% 
    mutate(percent = round((n/sum(n))*100,2)) %>% 
    mutate(validated = replace(validated, validated == "FALSE", "No")) %>% 
    mutate(validated = replace(validated, validated == "TRUE", "Yes")) %>% 
    select(-n)
  
  left_join(pred_voters, pred_nonvoters, by = "validated", suffix = c("_v","_nv")) %>% 
    rename(voters = percent_v, nonvoters = percent_nv)
}

# Graph true negatives and true positives

# those who say they will definitely vote or have voted already
mod1 <- validation_by_intent(c(1,3))
# those who say they will definitely vote, have voted already, or will probably vote
mod2 <- validation_by_intent(c(1,2,3))
# those who say they will definitely vote, have voted already, will probably vote, or who are undecided
mod3 <- validation_by_intent(c(1,2,3,5))
# all respondents in the sample
mod4 <- validation_by_intent(c(1,2,3,4,5))

mods <- list(mod1, mod2, mod3, mod4)

df <- data.frame(model = c("Already voted + will definitely vote", "Already voted + will definitely or probably vote", "Already voted + will definitely or probably vote + undecided", "All respondents"),
                 true_positive = vector(length = 4),
                 #false_positive = vector(length = 5),
                 #false_negative = vector(length = 5),
                 true_negative = vector(length = 4))
i <- 1
while(i <= length(mods)){
  df$true_positive[i] <- mods[[i]]$voters[mods[[i]]$validated == "Yes"]
  #df$false_positive[i] <- mods[[i]]$voters[mods[[i]]$validated == "No"]
  #df$false_negative[i] <- mods[[i]]$nonvoters[mods[[i]]$validated == "Yes"]
  df$true_negative[i] <- mods[[i]]$nonvoters[mods[[i]]$validated == "No"]
  i = i +1
}

df$model <- factor(df$model, levels = df$model)

pdf(file="~/Dropbox/LikelyVoters/Journal article/Data/Figures/basic_cutoff_2014.pdf", width=7, height=5)
df %>% gather(type, value, true_positive:true_negative) %>%
  mutate(type = replace(type, type == 'true_positive', 'True positive'),
         type = replace(type, type == 'true_negative', 'True negative')) %>% 
  ggplot(aes(factor(model), value, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "", title = "") +
  scale_fill_grey() +
  theme(legend.title=element_blank(), legend.position="bottom") + coord_flip()
dev.off()

vote_choice_intent <- function(intention){
  pooled %>% 
    filter(year == 2014, intent %in% intention) %>% 
    filter(intent_rep %in% c("[Democrat / Candidate 1]","[Republican / Candidate 2]",
                             "[Other / Candidate 3]","Other",
                             "I'm Not Sure")) %>% 
    group_by(intent_rep) %>% 
    summarise(n = sum(weight)) %>% 
    mutate(vote_share = round(100*n/sum(n),2)) %>% 
    select(-n)
}

mod1 <- vote_choice_intent(c(1,3))
mod2 <- vote_choice_intent(c(1,2,3))
mod3 <- vote_choice_intent(c(1,2,3,5))
mod4 <- vote_choice_intent(c(1,2,3,4,5))

mods <- list(mod1, mod2, mod3, mod4)

df <- data.frame(model = c("Already voted + will definitely vote", "Already voted + will definitely or probably vote", "Already voted + will definitely or probably vote + undecided", "All respondents"),
                 margin = vector(length = 4))

i <- 1
while(i <= length(mods)){
  df$margin[i] = mods[[i]]$vote_share[mods[[i]]$intent_rep == "[Democrat / Candidate 1]"] - mods[[i]]$vote_share[mods[[i]]$intent_rep == "[Republican / Candidate 2]"]
  i = i + 1
}

df$model <- factor(df$model, levels = df$model)
df$bias <- df$margin + 5.57

fig1_2014 <- df %>%  
  ggplot() +
  geom_col(aes(model, margin)) +
  coord_flip() +
  geom_hline(yintercept = -5.4, lty = 2, size = 1, colour = "red") +
  labs(x = "", y = "D House Share minus R House Share")
pdf(file="~/Dropbox/LikelyVoters/Journal article/Data/Figures/basic_cutoff_margin_2014.pdf", width=7, height=5)
fig1_2014
dev.off()

# Use Perry-Gallup with Cutoffs

validation_by_pg <- function(index) {
  willvote <- pooled %>% 
    filter(year == 2014, perry_gallup %in% index)
  wontvote <- pooled %>% filter(year == 2014) %>% 
    anti_join(willvote, by = "case_id")
  
  pred_voters <- willvote %>% 
    count(validated = (validated == 'Voted')) %>% 
    mutate(percent = round((n/sum(n))*100,2)) %>% 
    mutate(validated = replace(validated, validated == "FALSE", "No")) %>% 
    mutate(validated = replace(validated, validated == "TRUE", "Yes")) %>% 
    select(-n)
  
  pred_nonvoters <- wontvote %>% 
    count(validated = (validated == 'Voted')) %>% 
    mutate(percent = round((n/sum(n))*100,2)) %>% 
    mutate(validated = replace(validated, validated == "FALSE", "No")) %>% 
    mutate(validated = replace(validated, validated == "TRUE", "Yes")) %>% 
    select(-n)
  
  left_join(pred_voters, pred_nonvoters, by = "validated", suffix = c("_v","_nv")) %>% 
    rename(voters = percent_v, nonvoters = percent_nv)
}
mod1 <- validation_by_pg(6)
mod2 <- validation_by_pg(c(6,5))
mod3 <- validation_by_pg(c(6,5,4))
mod4 <- validation_by_pg(c(6,5,4,3))
mod5 <- validation_by_pg(c(6,5,4,3,2))
mod6 <- validation_by_pg(c(6,5,4,3,2,1))

mods <- list(mod1, mod2, mod3, mod4, mod5, mod6)

df <- data.frame(model = c("6s","6s and 5s","6s, 5s, and 4s","6s, 5s, 4s, and 3s", 
                           "6s, 5s, 4s, 3s, and 2s","All"),
                 true_positive = vector(length = 6),
                 true_negative = vector(length = 6))
i <- 1
while(i <= length(mods)){
  df$true_positive[i] <- mods[[i]]$voters[mods[[i]]$validated == "Yes"]
  df$true_negative[i] <- mods[[i]]$nonvoters[mods[[i]]$validated == "No"]
  i = i +1
}

df$model <- factor(df$model, levels = df$model)

df %>% gather(type, value, true_positive:true_negative) %>%
  mutate(type = replace(type, type == 'true_positive', 'True positive rate'),
         type = replace(type, type == 'true_negative', 'True negative rate')) %>% 
  ggplot(aes(factor(model), value, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "model", y = "", title = "Individual level vote validation by Perry-Gallup index score") + 
  scale_fill_grey() +
  theme(legend.title=element_blank())

vote_choice_pg <- function(index){
  pooled %>% 
    filter(year == 2014, perry_gallup %in% index) %>% 
    filter(intent_rep %in% c("[Democrat / Candidate 1]","[Republican / Candidate 2]",
                             "[Other / Candidate 3]","Other",
                             "I'm Not Sure")) %>%
    group_by(intent_rep) %>% 
    summarise(n = sum(weight)) %>% 
    mutate(vote_share = round(n/sum(n)*100,2)) %>% 
    select(-n)
}

mod1 <- vote_choice_pg(6)
mod2 <- vote_choice_pg(c(6,5))
mod3 <- vote_choice_pg(c(6,5,4))
mod4 <- vote_choice_pg(c(6,5,4,3))
mod5 <- vote_choice_pg(c(6,5,4,3,2))
mod6 <- vote_choice_pg(c(6,5,4,3,2,1))

mods <- list(mod1, mod2, mod3, mod4, mod5, mod6)

df <- data.frame(model = c("6s","6s and 5s","6s, 5s, and 4s","6s, 5s, 4s, and 3s", 
                           "6s, 5s, 4s, 3s, and 2s","All"),
                 margin = vector(length = 6))

i <- 1
while(i <= length(mods)){
  df$margin[i] = mods[[i]]$vote_share[mods[[i]]$intent_rep == "[Democrat / Candidate 1]"] - mods[[i]]$vote_share[mods[[i]]$intent_rep == "[Republican / Candidate 2]"]
  i = i + 1
}

df$model <- factor(df$model, levels = df$model)
df$bias <- df$margin + 5.57

fig5_2014 <- df %>%  
  ggplot() +
  geom_col(aes(model, margin)) +
  coord_flip() +
  labs(x = "", y = "D share minus R share") +
  geom_hline(yintercept = -5.4, lty = 2, size = 1, colour = "red")
fig5_2014

# Just show how many of each group there are
weighted2014 <- svydesign(id=~1,data=subset(pooled, year=="2014"), weights=~weight)
prop.table(svytable(~perry_gallup, weighted2014))


######## Perry Gallup Probabilistic

# create training set 
train <- pooled %>% filter(year %in% c(2008,2010,2012))
test <- pooled %>% filter(year == 2014)

# create eligible variable
# train
train$eligible[train$year %in% c(2008, 2012) & train$age < 22] <- 0
train$eligible[train$year %in% c(2008, 2012) & train$age >= 22] <- 1
train$eligible[train$year %in% c(2010) & train$age < 20] <- 0
train$eligible[train$year %in% c(2010) & train$age >= 20] <- 1
# test
test$eligible[test$age < 22] <- 0
test$eligible[test$age >= 22] <- 1

# run model
formula <- as.formula(validated ~ vote_history + intent + interest + registration + 
                        eligible)
set.seed(111)
model <- randomForest(formula, data = train)
pdf(file="~/Dropbox/LikelyVoters/Journal article/Data/Figures/vip_pg_2014.pdf", width=5, height=4, onefile = F)
varImpPlot(model, main="Perry Gallup Index Only", type=1)
dev.off()

# apply the models to test data
predictions <- cbind(test, predict(model, newdata = test, type = "prob"))
predictions <- as.data.frame(predictions)

# Implied turnout
mean(predictions$Voted*predictions$weight)

# margin weighted by propensity score
margin <- predictions %>%
  filter(intent_rep %in% c("[Democrat / Candidate 1]","[Republican / Candidate 2]",
                           "[Other / Candidate 3]","Other",
                           "I'm Not Sure")) %>% 
  group_by(intent_rep) %>% 
  summarise(n = sum(weight*Voted)) %>% 
  mutate(vote_share = round(n/sum(n)*100,2)) %>% 
  select(-n)

# Predicted Dem bias
print((margin$vote_share[margin$intent_rep=='[Democrat / Candidate 1]'] - margin$vote_share[margin$intent_rep=='[Republican / Candidate 2]'])+5.57)

# Graph of turnout accuracy by vote propensity
fig13a <- predictions %>% 
  group_by(voteprop_bucket = round(Voted*100, -1)) %>% # **
  summarise(turnout = mean(validated=='Voted'), num = n()) %>%
  ggplot(aes(x = voteprop_bucket)) +
  geom_smooth(aes(y = turnout*100), colour = '#0000FF', alpha = 0.5, se = F) +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = 'black') +
  coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
  scale_x_continuous(breaks = seq(0,100,by=10), labels = seq(0,100,by=10)) +
  labs(x='Vote propensity bucket',y='Turnout', title='Perry-Gallup Only') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())

## ROC curve
predictions_roc <- cbind(test, predict(model, newdata = test, type = "prob"))
predictions_roc$validated <- ifelse(predictions_roc$validated == 'Voted', 1, 0)
auc(predictions_roc$validated, predictions_roc$Voted)
roc_pg14 <-roc(predictions_roc$validated, predictions_roc$Voted)

####### Perry Gallup + Demos

# create training set 
train <- pooled %>% filter(year %in% c(2008,2010,2012))
test <- pooled %>% filter(year == 2014)

# run model
formula <- as.formula(validated ~ vote_history + intent + interest + registration + 
                        gender + age + race + education + income_new + 
                        partisan_strength )
set.seed(111)
model <- randomForest(formula, data = train)
pdf(file="~/Dropbox/LikelyVoters/Journal article/Data/Figures/vip_demos_2014.pdf", width=5, height=4, onefile = F)
varImpPlot(model, main = "Perry Gallup + Demographics", type=1)
dev.off()

# apply the models to test data
predictions <- cbind(test, predict(model, newdata = test, type = "prob"))
predictions <- as.data.frame(predictions)

# Implied turnout
mean(predictions$Voted*predictions$weight)

# margin weighted by propensity score
margin <- predictions %>%
  filter(intent_rep %in% c("[Democrat / Candidate 1]","[Republican / Candidate 2]",
                           "[Other / Candidate 3]","Other",
                           "I'm Not Sure")) %>% 
  group_by(intent_rep) %>% 
  summarise(n = sum(weight*Voted)) %>% 
  mutate(vote_share = round(n/sum(n)*100,2)) %>% 
  select(-n)

# Predicted dem bias
print((margin$vote_share[margin$intent_rep=='[Democrat / Candidate 1]'] - margin$vote_share[margin$intent_rep=='[Republican / Candidate 2]'])+5.57)

# Graph of turnout accuracy by vote propensity
fig13b <- predictions %>% 
  group_by(voteprop_bucket = round(Voted*100, -1)) %>% # **
  summarise(turnout = mean(validated=='Voted'), num = n()) %>%
  ggplot(aes(x = voteprop_bucket)) +
  geom_smooth(aes(y = turnout*100), colour = '#0000FF', alpha = 0.5, se = F) +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = 'black') +
  coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
  scale_x_continuous(breaks = seq(0,100,by=10), labels = seq(0,100,by=10)) +
  labs(x='Vote propensity bucket',y='Turnout', title='Perry-Gallup + Demos') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())


## Combine graphs
pdf(file="~/Dropbox/LikelyVoters/Journal article/Data/Figures/prob_accuracy_2014.pdf", width=6.5, height=4, onefile = F)
ggarrange(fig13a, fig13b, ncol=2, common.legend = T, legend = "bottom")
dev.off()

## ROC curve
predictions_roc <- cbind(test, predict(model, newdata = test, type = "prob"))
predictions_roc$validated <- ifelse(predictions_roc$validated == 'Voted', 1, 0)
auc(predictions_roc$validated, predictions_roc$Voted)
roc_pgad14 <- roc(predictions_roc$validated, predictions_roc$Voted)


#### 

# ROC curves
# save all to one file and print numbers here again
pdf(file="~/Dropbox/LikelyVoters/Journal article/Data/Figures/roc_curves.pdf", width=6.5, height=4, onefile = F)
ggroc(list(PG_14=roc_pg14, 
           PGaD_14=roc_pgad14, 
           PG_16=roc_pg16, 
           PGaD_16=roc_pgad16), legacy.axes = T) +
  geom_abline(intercept=0, slope=1, lty= 2) +
  scale_color_manual(values=c('darkgreen', 'darkblue', 'purple', 'red')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        legend.title=element_blank())
dev.off()

# accuracy by state sample size in 2016 
# have to join state sample size to margin_by_weighted_turnout_all_pg and margin_by_weighted_turnout_all_pg_dems
state_sizes <- predictions_all %>% count(state)
margin_by_weighted_turnout_all_pg <- margin_by_weighted_turnout_all_pg %>% 
  left_join(state_sizes, by='state')
margin_by_weighted_turnout_all_pg_dems <- margin_by_weighted_turnout_all_pg_dems %>% 
  left_join(state_sizes, by='state')

error_size_pg <- ggplot(margin_by_weighted_turnout_all_pg) +
  geom_point(aes(x=n, y=abs(dem_bias))) +
  labs(x='State sample size', y='Absolute error', title='Perry-Gallup') +
  ylim(0, 30) +
  theme_bw() 
error_size_pg_dems <- ggplot(margin_by_weighted_turnout_all_pg_dems) +
  geom_point(aes(x=n, y=abs(dem_bias))) +
  labs(x='State sample size', y='Absolute error', title='PGaD') +
  ylim(0, 30) +
  theme_bw()

pdf(file="~/Dropbox/LikelyVoters/Journal article/Data/Figures/error_state_size.pdf", width=10, height=5, onefile = F)
ggarrange(error_size_pg, error_size_pg_dems, ncol=2)
dev.off()


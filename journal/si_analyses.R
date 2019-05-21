# Run load.R before running this script
#### Show results using all possible self-reported intent thresholds ####
library(stargazer)

#### 2016 National
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

print(df)

# Just show how many of each group there are
weighted2016 <- svydesign(id=~1,data=subset(pooled, year=="2016"), weights=~weight)
prop.table(svytable(~intent, weighted2016))


#### 2014 National

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

print(df)

# Just show how many of each group there are
weighted2014 <- svydesign(id=~1,data=subset(pooled, year=="2014"), weights=~weight)
prop.table(svytable(~intent, weighted2014))

#### Show results using all possible Perry-Gallup thresholds ####

#### 2016 National


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

print(df)

# Just show how many of each group there are
weighted2016 <- svydesign(id=~1,data=subset(pooled, year=="2016"), weights=~weight)
prop.table(svytable(~perry_gallup, weighted2016))


#### 2014 National

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

print(df)

# Just show how many of each group there are
weighted2014 <- svydesign(id=~1,data=subset(pooled, year=="2014"), weights=~weight)
prop.table(svytable(~perry_gallup, weighted2014))

#### Duplicate results using logistic regression instead of random forests ####

#### 2016 National

# Drop values that are NA on vote intent (6 cases)
pooled <- subset(pooled, intent!='__NA__')

# Perry gallup only
train <- pooled %>% filter(year %in% c(2008,2010,2012,2014))
test <- pooled %>% filter(year == 2016)

# run unweighted model
formula <- as.formula(validated=='Voted' ~ vote_history + intent + interest + 
                        registration + eligible)
model <- glm(formula, family = binomial(link = "logit"), data = train)

# run weighted model
svy.train <- svydesign(ids = ~ 1, data = train, weights = ~weight)
model_weighted <- svyglm(formula, design = svy.train, family = binomial)

# apply the models to test data
# unweighted
predictions <- cbind(test, predict(model, newdata = test, type = "response"))
predictions <- as.data.frame(predictions)
predictions<- predictions %>% 
  rename(response = 'predict(model, newdata = test, type = \"response\")')

# Implied turnout rate
mean(predictions$response*predictions$weight)


# margin weighted by propensity score
margin <- predictions %>% 
  filter(choice %in% c("Donald Trump (Republican)","Hillary Clinton (Democrat)",
                       "Gary Johnson (Libertarian)","Jill Stein (Green)","Other",
                       "I'm Not Sure")) %>% 
  group_by(choice) %>% 
  summarise(n = sum(weight*response)) %>% 
  mutate(vote_share = round(n/sum(n)*100,2)) %>% 
  select(-n)

# Democratic bias
print((margin$vote_share[margin$choice=='Hillary Clinton (Democrat)'] - margin$vote_share[margin$choice=='Donald Trump (Republican)'])-validated_margin16)

# Perry gallup + Demographics
train <- pooled %>% filter(year %in% c(2008,2010,2012,2014))
test <- pooled %>% filter(year == 2016)

# run unweighted model
formula <- as.formula(validated=='Voted' ~ vote_history + intent + interest + 
                        registration + eligible + gender + age + race + education + income_new + 
                        partisan_strength)
model2 <- glm(formula, family = binomial(link = "logit"), data = train)

# run weighted model
svy.train <- svydesign(ids = ~ 1, data = train, weights = ~weight)
model_weighted <- svyglm(formula, design = svy.train, family = binomial)

# apply the models to test data
# unweighted
predictions <- cbind(test, predict(model2, newdata = test, type = "response"))
predictions <- as.data.frame(predictions)
predictions<- predictions %>% 
  rename(response = 'predict(model2, newdata = test, type = \"response\")')

# Implied turnout rate
mean(predictions$response*predictions$weight)


# margin weighted by propensity score
margin <- predictions %>% 
  filter(choice %in% c("Donald Trump (Republican)","Hillary Clinton (Democrat)",
                       "Gary Johnson (Libertarian)","Jill Stein (Green)","Other",
                       "I'm Not Sure")) %>% 
  group_by(choice) %>% 
  summarise(n = sum(weight*response)) %>% 
  mutate(vote_share = round(n/sum(n)*100,2)) %>% 
  select(-n)

# Democratic bias
print((margin$vote_share[margin$choice=='Hillary Clinton (Democrat)'] - margin$vote_share[margin$choice=='Donald Trump (Republican)'])-validated_margin16)

# Produce regression table

stargazer(model, model2, star.cutoffs=.01, single.row=TRUE)


#### 2014 National

# Perry gallup only
train <- pooled %>% filter(year %in% c(2008,2010,2012))
test <- pooled %>% filter(year == 2014)

# run unweighted model
formula <- as.formula(validated=='Voted' ~ vote_history + intent + interest + 
                        registration + eligible)
model <- glm(formula, family = binomial(link = "logit"), data = train)

# run weighted model
svy.train <- svydesign(ids = ~ 1, data = train, weights = ~weight)
model_weighted <- svyglm(formula, design = svy.train, family = binomial)

# apply the models to test data
# unweighted
predictions <- cbind(test, predict(model, newdata = test, type = "response"))
predictions <- as.data.frame(predictions)
predictions<- predictions %>% 
  rename(response = 'predict(model, newdata = test, type = \"response\")')

# Implied turnout rate
mean(predictions$response*predictions$weight)


# margin weighted by propensity score
margin <- predictions %>% 
  filter(intent_rep %in% c("[Democrat / Candidate 1]","[Republican / Candidate 2]",
                           "[Other / Candidate 3]","Other",
                           "I'm Not Sure")) %>% 
  group_by(intent_rep) %>% 
  summarise(n = sum(weight*response)) %>% 
  mutate(vote_share = round(n/sum(n)*100,2)) %>% 
  select(-n)

# Democratic bias
print((margin$vote_share[margin$intent_rep=='[Democrat / Candidate 1]'] - margin$vote_share[margin$intent_rep=='[Republican / Candidate 2]'])+5.57)


## Perry gallup + Demographics

# run unweighted model
formula <- as.formula(validated=='Voted' ~ vote_history + intent + interest + 
                        registration + eligible + gender + age + race + education + income_new + 
                        partisan_strength)
model2 <- glm(formula, family = binomial(link = "logit"), data = train)

train <- pooled %>% filter(year %in% c(2008,2010,2012))
test <- pooled %>% filter(year == 2014)

# run weighted model
svy.train <- svydesign(ids = ~ 1, data = train, weights = ~weight)
model_weighted <- svyglm(formula, design = svy.train, family = binomial)

# apply the models to test data
# unweighted
predictions <- cbind(test, predict(model2, newdata = test, type = "response"))
predictions <- as.data.frame(predictions)
predictions<- predictions %>% 
  rename(response = 'predict(model2, newdata = test, type = \"response\")')

# Implied turnout rate
mean(predictions$response*predictions$weight)


# margin weighted by propensity score
margin <- predictions %>% 
  filter(intent_rep %in% c("[Democrat / Candidate 1]","[Republican / Candidate 2]",
                           "[Other / Candidate 3]","Other",
                           "I'm Not Sure")) %>% 
  group_by(intent_rep) %>% 
  summarise(n = sum(weight*response)) %>% 
  mutate(vote_share = round(n/sum(n)*100,2)) %>% 
  select(-n)

# Democratic bias
print((margin$vote_share[margin$intent_rep=='[Democrat / Candidate 1]'] - margin$vote_share[margin$intent_rep=='[Republican / Candidate 2]'])+5.57)

# Produce regression table

stargazer(model, model2, star.cutoffs=.01, single.row=TRUE)


library(tidyverse)
library(broom)
library(glmnet)
library(MLmetrics)
library(RColorBrewer)

# regularization w/ LASSO first cut
# data loaded in in load.R

# create training set 
train <- pooled %>% filter(year %in% c(2008,2010,2012,2014))
test <- pooled %>% filter(year == 2016)

# run unregularized model
formula <- as.formula(validated=='Voted' ~ vote_history*intent + interest + registration + 
                        gender + age + race + education + income_new + 
                        partisan_strength + religiosity + marital_status +
                        residential_mobility)
model <- glm(formula, family = binomial(link = "logit"), data = train)

# apply the model to test data
predictions <- cbind(test, predict(model, newdata = test, type = "response"))
predictions <- as.data.frame(predictions)
predictions <- predictions %>% 
  rename(response = 'predict(model, newdata = test, type = \"response\")')

# regularized model
# Define predictor matrices
predictor_matrix_train <- model.matrix(formula, data = train)[, -1]
predictor_matrix_test <- model.matrix(formula, data = test)[, -1]

# try out different penalty values
lambda_inputs <- 10^seq(-2, 10, length = 100)

LASSO <- glmnet(x = predictor_matrix_train, y = train$validated, alpha=1, 
                      lambda = lambda_inputs, family = "binomial")

# Optimal lambdas... for cv
# lambda_star <- LASSO$lambda.min
# lambda_star_1SE <- LASSO$lambda.1se

# Plot CV
# plot(LASSO)
#abline(v = log(lambda_star), col = "red")
#abline(v = log(lambda_star_1SE), col = "blue")

# from here: 
# https://rudeboybert.github.io/STAT495/quickstart.html#21_lasso
get_LASSO_coefficients <- function(LASSO_fit){
  coeff_values <- LASSO_fit %>% 
    broom::tidy() %>% 
    as_tibble() %>% 
    select(-c(step, dev.ratio)) %>% 
    tidyr::complete(lambda, nesting(term), fill = list(estimate = 0)) %>% 
    arrange(desc(lambda)) %>% 
    select(term, estimate, lambda)
  return(coeff_values)
}

LASSO_coefficients <- get_LASSO_coefficients(LASSO)

options(scipen = 999)
LASSO_coefficients %>% filter(lambda <= 1, term != "(Intercept)") %>% 
  mutate(variable = case_when(
    term == 'age' ~ 'age',
    substr(term,1,9) == 'education' ~ 'education',
    substr(term,1,6) == 'intent' ~ 'intent',
    substr(term,1,8) == 'interest' ~ 'interest',
    substr(term,1,7) == 'marital' ~ 'marital_status',
    substr(term,1,8) == 'partisan' ~ 'partisan_strength',
    substr(term,1,4) == 'race'~ 'race',
    substr(term,1,12) == 'registration' ~ 'registration',
    substr(term,1,11) == 'residential' ~ 'residential',
    substr(term,1,4) == 'vote' ~ 'vote_history'
  )) %>%
  ggplot(aes(x=lambda, y=estimate, group=term, col=variable)) +
  geom_line() +
  scale_x_log10() +
  scale_colour_brewer(palette = "Paired") +
  labs(x="lambda (log10-scale)", y="beta-hat coefficient estimate",
       title="LASSO regularized coefficient for each lambda value")

LASSO_coefficients %>% filter(term != "(Intercept)") %>% group_by(term) %>% 
  filter(estimate != 0) %>% summarise(max_lambda = max(lambda)) %>% arrange(desc(max_lambda))

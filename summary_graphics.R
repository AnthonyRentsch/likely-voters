# final summary graphics for results
# take best model from each baseline approach and plot its error
# for logistic regression and random forest sections use range of error in 40-60% turnout range

###### national
summary_national <- data.frame(num = 1:17,
                               model = c("Vote intent",
                                         "Vote intent + vote history",
                                         "Perry-Gallup index",
                                         "",
                                         "Logistic regression",
                                         "Logistic regression",
                                         "Logistic regression",
                                         "Logistic regression",
                                         "Logistic regression",
                                         "Logistic regression",
                                         "",
                                         "Random forests",
                                         "Random forests",
                                         "Random forests",
                                         "Random forests",
                                         "Random forests",
                                         "Random forests"),
                               type = c("Already voted + will definitely vote",
                                        "Voted in 2012 and already voted + will definitely vote",
                                        "6s and 5s on Perry-Gallup index",
                                        "",
                                        "Perry-Gallup",
                                        "Perry-Gallup (weight)",
                                        "Perry-Gallup + demographic",
                                        "Perry-Gallup + demographic (weight)",
                                        "Perry-Gallup + demographic + structural",
                                        "Perry-Gallup + demographic + structural (weight)",
                                        " ",
                                        " Perry-Gallup",
                                        " Perry-Gallup (weight)",
                                        " Perry-Gallup + demographic",
                                        " Perry-Gallup + demographic (weight)",
                                        " Perry-Gallup + demographic + structural",
                                        " Perry-Gallup + demographic + structural (weight)"),
                               error_low = vector(length = 17),
                               error_high = vector(length = 17))

# vote intent
summary_national$error_low[summary_national$model == "Vote intent"] <- 6.52 - validated_margin16
summary_national$error_high[summary_national$model == "Vote intent"] <- 6.52 - validated_margin16

# vote intent + vote history
summary_national$error_low[summary_national$model == "Vote intent + vote history"] <- 7.13 - validated_margin16
summary_national$error_high[summary_national$model == "Vote intent + vote history"] <- 7.13 - validated_margin16

# Perry-Gallup index
summary_national$error_low[summary_national$model == "Perry-Gallup index"] <- 1.71 - validated_margin16
summary_national$error_high[summary_national$model == "Perry-Gallup index"] <- 1.71 - validated_margin16

# logistic regression
summary_national$error_low[summary_national$model == "Logistic regression" &
                         summary_national$type == "Perry-Gallup"] <- 0.57 - validated_margin16
summary_national$error_high[summary_national$model == "Logistic regression" &
                             summary_national$type == "Perry-Gallup"] <- 4.53 - validated_margin16

summary_national$error_low[summary_national$model == "Logistic regression" &
                             summary_national$type == "Perry-Gallup (weight)"] <- 5.8 - validated_margin16
summary_national$error_high[summary_national$model == "Logistic regression" &
                              summary_national$type == "Perry-Gallup (weight)"] <- 5.80 - validated_margin16

summary_national$error_low[summary_national$model == "Logistic regression" &
                             summary_national$type == "Perry-Gallup + demographic"] <- -7.57 - validated_margin16
summary_national$error_high[summary_national$model == "Logistic regression" &
                              summary_national$type == "Perry-Gallup + demographic"] <- -0.78 - validated_margin16

summary_national$error_low[summary_national$model == "Logistic regression" &
                             summary_national$type == "Perry-Gallup + demographic (weight)"] <- 3.17 - validated_margin16
summary_national$error_high[summary_national$model == "Logistic regression" &
                              summary_national$type == "Perry-Gallup + demographic (weight)"] <- 3.17 - validated_margin16

summary_national$error_low[summary_national$model == "Logistic regression" &
                             summary_national$type == "Perry-Gallup + demographic + structural"] <- -7.03 - validated_margin16
summary_national$error_high[summary_national$model == "Logistic regression" &
                              summary_national$type == "Perry-Gallup + demographic + structural"] <- -0.07 - validated_margin16

summary_national$error_low[summary_national$model == "Logistic regression" &
                             summary_national$type == "Perry-Gallup + demographic + structural (weight)"] <- 6.05 - validated_margin16
summary_national$error_high[summary_national$model == "Logistic regression" &
                              summary_national$type == "Perry-Gallup + demographic + structural (weight)"] <- 6.05 - validated_margin16

# random forests
summary_national$error_low[summary_national$model == "Random forests" &
                             summary_national$type == " Perry-Gallup"] <- 5.22 - validated_margin16
summary_national$error_high[summary_national$model == "Random forests" &
                             summary_national$type == " Perry-Gallup"] <- 5.22 - validated_margin16

summary_national$error_low[summary_national$model == "Random forests" &
                             summary_national$type == " Perry-Gallup (weight)"] <- 6.17 - validated_margin16
summary_national$error_high[summary_national$model == "Random forests" &
                              summary_national$type == " Perry-Gallup (weight)"] <- 6.17 - validated_margin16

summary_national$error_low[summary_national$model == "Random forests" &
                             summary_national$type == " Perry-Gallup + demographic"] <- -5.090 - validated_margin16
summary_national$error_high[summary_national$model == "Random forests" &
                              summary_national$type == " Perry-Gallup + demographic"] <- -0.020 - validated_margin16

summary_national$error_low[summary_national$model == "Random forests" &
                             summary_national$type == " Perry-Gallup + demographic (weight)"] <- 2.52 - validated_margin16
summary_national$error_high[summary_national$model == "Random forests" &
                              summary_national$type == " Perry-Gallup + demographic (weight)"] <- 2.52 - validated_margin16

summary_national$error_low[summary_national$model == "Random forests" &
                             summary_national$type == " Perry-Gallup + demographic + structural"] <- -4.480 - validated_margin16
summary_national$error_high[summary_national$model == "Random forests" &
                              summary_national$type == " Perry-Gallup + demographic + structural"] <- -0.020 - validated_margin16

summary_national$error_low[summary_national$model == "Random forests" &
                             summary_national$type == " Perry-Gallup + demographic + structural (weight)"] <- 4.12 - validated_margin16
summary_national$error_high[summary_national$model == "Random forests" &
                              summary_national$type == " Perry-Gallup + demographic + structural (weight)"] <- 4.12 - validated_margin16

# blank spaces for axis groups
summary_national$error_high[summary_national$type %in% c("", " ")] <- NA
summary_national$error_low[summary_national$type %in% c("", " ")] <- NA

# edit df to make plot
summary_national$label <- paste(summary_national$model, ": \n", summary_national$type, sep = "")
summary_national$label <- factor(summary_national$label, levels = rev(summary_national$label))
#summary_national$label <- factor(summary_national$label, 
                                # levels = summary_national$label[order(summary_national$error_low)])
summary_national$type <- factor(summary_national$type, levels = rev(summary_national$type))
summary_national$color <- summary_national$error_low > 0
summary_national$color[summary_national$label == "Logistic regression: \nPerry-Gallup" ] <- NA

pal <- c("#FF0000", "#0000FF")

# visualize
baseline_models <- textGrob("Baseline models", gp=gpar(fontsize=13, fontface="bold"))
log_reg_models <- textGrob("Logistic regression models", gp=gpar(fontsize=13, fontface="bold"))
rf_models <- textGrob("Random forests models", gp=gpar(fontsize=13, fontface="bold"))

fig15 <- ggplot(data = summary_national) +
  geom_point(aes(x = error_low, y = type, colour = color), size = 2.5) +
  geom_point(aes(x = error_high, y = type, colour = color), size = 2.5) +
  geom_segment(aes(x = error_low, y = type, xend = error_high, yend = type, colour = color), size = 1) +
  scale_colour_manual(values = pal, na.value = "purple") +
  geom_vline(aes(xintercept = 0), col = "black", lty = 2) +
  labs(title = "", x = "Error relative to margin among validated voters", y = "") +
  annotation_custom(baseline_models,xmin=-13.5,xmax=-13.5,ymin=17.75,ymax=17.75) +
  annotation_custom(log_reg_models,xmin=-14.9,xmax=-14.9,ymin=13.75,ymax=13.75) +
  annotation_custom(rf_models,xmin=-14.5,xmax=-14.5,ymin=6.75,ymax=6.75) +
  theme(legend.position="none", axis.ticks.y=element_blank())

fig15 <- ggplot_gtable(ggplot_build(fig15))
fig15$layout$clip[fig15$layout$name == "panel"] <- "off"
grid.draw(fig15)



###### states
# code used to compute error for weighted margin
# margin_by_turnout_all %>% group_by(state) %>% summarise(err = mean(margin_weighted) - mean(validated_margin)) %>% ungroup() %>% summarise(avg_error = mean(err, na.rm = T))
summary_states <- data.frame(num = 1:17,
                               model = c("Vote intent",
                                         "Vote intent + vote history",
                                         "Perry-Gallup index",
                                         "",
                                         "Logistic regression",
                                         "Logistic regression",
                                         "Logistic regression",
                                         "Logistic regression",
                                         "Logistic regression",
                                         "Logistic regression",
                                         " ",
                                         "Random forests",
                                         "Random forests",
                                         "Random forests",
                                         "Random forests",
                                         "Random forests",
                                         "Random forests"),
                               type = c("Already voted + will definitely or probably vote\n+ undecided",
                                        "Voted in 2012 and already voted + will definitely vote",
                                        "6s, 5s, and 4s on Perry-Gallup index",
                                        "",
                                        "Perry-Gallup",
                                        "Perry-Gallup (weight)",
                                        "Perry-Gallup + demographic",
                                        "Perry-Gallup + demographic (weight)",
                                        "Perry-Gallup + demographic + structural",
                                        "Perry-Gallup + demographic + structural (weight)",
                                        " ",
                                        " Perry-Gallup",
                                        " Perry-Gallup (weight)",
                                        " Perry-Gallup + demographic",
                                        " Perry-Gallup + demographic (weight)",
                                        " Perry-Gallup + demographic + structural",
                                        " Perry-Gallup + demographic + structural (weight)"),
                               error_low = vector(length = 17),
                               error_high = vector(length = 17))

# vote intent
summary_states$error_low[summary_states$model == "Vote intent"] <- 2.392745
summary_states$error_high[summary_states$model == "Vote intent"] <- 2.392745

# vote intent + vote history
summary_states$error_low[summary_states$model == "Vote intent + vote history"] <- 2.812843
summary_states$error_high[summary_states$model == "Vote intent + vote history"] <- 2.812843

# Perry-Gallup index
summary_states$error_low[summary_states$model == "Perry-Gallup index"] <- 1.449804
summary_states$error_high[summary_states$model == "Perry-Gallup index"] <- 1.449804

# Logistic regression
summary_states$error_low[summary_states$model == "Logistic regression" &
                           summary_states$type == "Perry-Gallup"] <- -0.9348
summary_states$error_high[summary_states$model == "Logistic regression" &
                            summary_states$type == "Perry-Gallup"] <- 1.484

summary_states$error_low[summary_states$model == "Logistic regression" &
                           summary_states$type == "Perry-Gallup (weight)"] <- 1.482237
summary_states$error_high[summary_states$model == "Logistic regression" &
                              summary_states$type == "Perry-Gallup (weight)"] <- 1.482237

summary_states$error_low[summary_states$model == "Logistic regression" &
                           summary_states$type == "Perry-Gallup + demographic"] <-  -7.307 
summary_states$error_high[summary_states$model == "Logistic regression" &
                            summary_states$type == "Perry-Gallup + demographic"] <- -2.666 

summary_states$error_low[summary_states$model == "Logistic regression" &
                           summary_states$type == "Perry-Gallup + demographic (weight)"] <- -0.6538409
summary_states$error_high[summary_states$model == "Logistic regression" &
                            summary_states$type == "Perry-Gallup + demographic (weight)"] <- -0.6538409

summary_states$error_low[summary_states$model == "Logistic regression" &
                           summary_states$type == "Perry-Gallup + demographic + structural"] <- -6.489
summary_states$error_high[summary_states$model == "Logistic regression" &
                            summary_states$type == "Perry-Gallup + demographic + structural"] <- -2.168 

summary_states$error_low[summary_states$model == "Logistic regression" &
                           summary_states$type == "Perry-Gallup + demographic + structural (weight)"] <- 1.650081
summary_states$error_high[summary_states$model == "Logistic regression" &
                            summary_states$type == "Perry-Gallup + demographic + structural (weight)"] <- 1.650081

# random forests
summary_states$error_low[summary_states$model == "Random forests" &
                           summary_states$type == " Perry-Gallup"] <- 0.843
summary_states$error_high[summary_states$model == "Random forests" &
                            summary_states$type == " Perry-Gallup"] <- 1.865

summary_states$error_low[summary_states$model == "Random forests" &
                             summary_states$type == " Perry-Gallup (weight)"] <- 2.143218
summary_states$error_high[summary_states$model == "Random forests" &
                            summary_states$type == " Perry-Gallup (weight)"] <- 2.143218

summary_states$error_low[summary_states$model == "Random forests" &
                           summary_states$type == " Perry-Gallup + demographic"] <- -5.644
summary_states$error_high[summary_states$model == "Random forests" &
                              summary_states$type == " Perry-Gallup + demographic"] <- -1.963  

summary_states$error_low[summary_states$model == "Random forests" &
                           summary_states$type == " Perry-Gallup + demographic (weight)"] <- -0.1981547
summary_states$error_high[summary_states$model == "Random forests" &
                            summary_states$type == " Perry-Gallup + demographic (weight)"] <- -0.1981547

summary_states$error_low[summary_states$model == "Random forests" &
                           summary_states$type == " Perry-Gallup + demographic + structural"] <- -4.792
summary_states$error_high[summary_states$model == "Random forests" &
                              summary_states$type == " Perry-Gallup + demographic + structural"] <- -1.231 

summary_states$error_low[summary_states$model == "Random forests" &
                           summary_states$type == " Perry-Gallup + demographic + structural (weight)"] <- 0.6026296
summary_states$error_high[summary_states$model == "Random forests" &
                            summary_states$type == " Perry-Gallup + demographic + structural (weight)"] <- 0.6026296

# blank spaces for axis groups
summary_states$error_high[summary_states$type %in% c("", " ")] <- NA
summary_states$error_low[summary_states$type %in% c("", " ")] <- NA

# edit df to make plot
summary_states$label <- paste(summary_states$model, ": \n", summary_states$type, sep = "")
summary_states$label <- factor(summary_states$label, levels = rev(summary_states$label))
#summary_states$label <- factor(summary_states$label, 
                            #   levels = summary_states$label[order(summary_states$error_low)])
summary_states$type <- factor(summary_states$type, levels = rev(summary_states$type))
summary_states$color <- summary_states$error_low > 0
summary_states$color[summary_states$label == "Logistic regression: \nPerry-Gallup" ] <- NA

pal <- c("#FF0000", "#0000FF")

# visualize
fig16 <- ggplot(data = summary_states) +
  geom_point(aes(x = error_low, y = type, colour = color), size = 2.5) +
  geom_point(aes(x = error_high, y = type, colour = color), size = 2.5) +
  geom_segment(aes(x = error_low, y = type, xend = error_high, yend = type, colour = color), size = 1) +
  scale_colour_manual(values = pal, na.value = "purple") +
  geom_vline(aes(xintercept = 0), col = "black", lty = 2) +
  labs(title = "", x = "Average error relative to margin among validated voters", y = "") +
  annotation_custom(baseline_models,xmin=-9.5,xmax=-9.5,ymin=17.75,ymax=17.75) +
  annotation_custom(log_reg_models,xmin=-10.5,xmax=-10.5,ymin=13.75,ymax=13.75) +
  annotation_custom(rf_models,xmin=-10.2,xmax=-10.2,ymin=6.75,ymax=6.75) +
  theme(legend.position="none", axis.ticks.y=element_blank())

fig16 <- ggplot_gtable(ggplot_build(fig16))
fig16$layout$clip[fig16$layout$name == "panel"] <- "off"
grid.draw(fig16)


#########

# summary graphics for discussion section - journalist use case mock up

######## state-level random forest model with Perry-Gallup and demographic variables
######## choosing Florida because it has a lot of observations and has been a swing state recently

set.seed(143)
# create training set 
train <- pooled %>% filter(state == "Florida", year %in% c(2008,2010,2012,2014))
test <- pooled %>% filter(state == "Florida", year == 2016)

# run model
formula <- as.formula(validated ~ vote_history + intent + interest + registration + 
                        gender + age + race + education + income_new + 
                        partisan_strength + religiosity + marital_status +
                        residential_mobility)
model <- randomForest(formula, data = train)

# apply the models to test data
predictions <- cbind(test, predict(model, newdata = test, type = "prob"))
predictions <- as.data.frame(predictions)

# no propensity weight for margin
vote_choice_logreg <- function(turnout){
  predictions %>% 
    top_n(round(nrow(predictions)*turnout,0), wt = Voted) %>% 
    filter(choice %in% c("Donald Trump (Republican)","Hillary Clinton (Democrat)",
                         "Gary Johnson (Libertarian)","Jill Stein (Green)","Other",
                         "I'm Not Sure")) %>% 
    group_by(choice) %>% 
    summarise(n = sum(weight)) %>% 
    mutate(vote_share = round(n/sum(n)*100,2)) %>% 
    select(-n)
}

turnout_rates <- seq(from = 0.01, to = 1, by = 0.01)
margin_by_turnout <- data.frame()
i <- 1
while(i <= length(turnout_rates)){
  temp <- vote_choice_logreg(turnout_rates[i])
  margin_by_turnout[i,1] <- temp$vote_share[temp$choice=='Hillary Clinton (Democrat)'] - temp$vote_share[temp$choice=='Donald Trump (Republican)']
  margin_by_turnout[i,2] <- i
  margin_by_turnout[i, 3] <- 100*(sqrt(((temp$vote_share[temp$choice=='Hillary Clinton (Democrat)']/100)*(temp$vote_share[temp$choice=='Donald Trump (Republican)']/100))/
                                         count(top_n(predictions, round(nrow(predictions)*turnout_rates[i],0), wt = Voted))))
  i = i +1
}

# margin weighted by propensity score
vote_choice_logreg_weighted <- function(turnout){
  predictions %>% 
    top_n(round(nrow(predictions)*turnout,0), wt = Voted) %>% 
    filter(choice %in% c("Donald Trump (Republican)","Hillary Clinton (Democrat)",
                         "Gary Johnson (Libertarian)","Jill Stein (Green)","Other",
                         "I'm Not Sure")) %>% 
    group_by(choice) %>% 
    summarise(n = sum(weight*Voted)) %>% 
    mutate(vote_share = round(n/sum(n)*100,2)) %>% 
    select(-n)
}

weighted_margin_by_turnout <- data.frame()
i <- 1
while(i <= length(turnout_rates)){
  temp <- vote_choice_logreg_weighted(turnout_rates[i])
  weighted_margin_by_turnout[i,1] <- temp$vote_share[temp$choice=='Hillary Clinton (Democrat)'] - temp$vote_share[temp$choice=='Donald Trump (Republican)']
  weighted_margin_by_turnout[i,2] <- i
  i = i +1
}

# visualize
margin_by_turnout <- margin_by_turnout %>% rename(margin = V1, i = V2, moe = n)
weighted_margin_by_turnout <- weighted_margin_by_turnout %>% rename(margin = V1, i = V2)
margin_by_turnout$color <- margin_by_turnout$margin > 0
margin_by_turnout$moe_high <- margin_by_turnout$margin + margin_by_turnout$moe
margin_by_turnout$moe_low <- margin_by_turnout$margin - margin_by_turnout$moe
pal <- c("#ff2700", "#008fd5")

fig17 <- ggplot() +
  # bold line at vote prop weighted prediction
  geom_hline(yintercept=weighted_margin_by_turnout$margin[weighted_margin_by_turnout$i==100],
             size=1.2,colour="#535353") +
  annotate("text",x=92,y=-1.4,label = "Vote propensity\nweighted prediction",
           fontface = "bold") +
  # 40 and 60 lines
  geom_vline(xintercept = 40, col = "#535353", size = 1, lty = 2) +
  geom_vline(xintercept = 60, col = "#535353", size = 1, lty = 2) +
  # reasonable turnout text and arrows
  annotate("text",x=48,y=5.5,label="Reasonable\n turnout\n range", 
           fontface="bold", colour = "#535353") +
  geom_segment(aes(x = 44, y = 5.5, xend = 40.25, yend = 5.5), colour = "#535353",
               arrow = arrow(length=unit(0.2,"cm"), type = "closed")) +
  geom_segment(aes(x=53, y=5.5, xend=59.75, yend=5.5), colour = "#535353",
               arrow = arrow(length=unit(0.2,"cm"), type = "closed")) +
  # put background for homemade legend
  geom_rect(aes(xmin = 80, xmax = 100, ymin = -20, ymax = -10), colour = "#D0D0D0", fill = "#F0F0F0") +
  # 2012 turnout
  geom_vline(xintercept = 55.1, col = "#ED713A", size = 0.75, lty = 2) +
  annotate("text", x = 91.25, y = -12.5, label = "2012 turnout", colour = "#ED713A",
           fontface = "bold") +
  geom_segment(aes(x = 82, y = -10.75, xend = 82, yend = -14.25), size = 0.75, 
               lty = 2, colour = "#ED713A") +
  # 2008 turnout
  geom_vline(xintercept = 57.6, col = "#8E3087", size = 0.75, lty = 2) +
  annotate("text", x = 91.25, y = -17.5, label = "2008 turnout", colour = "#8E3087",
           fontface = "bold") +
  geom_segment(aes(x = 82, y = -15.75, xend = 82, yend = -19.25), size = 0.75,
               lty = 2, colour = "#8E3087") +
  # margin 
  #geom_ribbon(data = margin_by_turnout,
              #aes(x = turnout_rates[i]*100, ymin = moe_low, ymax = moe_high), alpha = 0.2) +
  geom_point(data = margin_by_turnout, 
             aes(x = turnout_rates[i]*100, y = margin, colour = color)) +
  # axis text and other theme calls
  labs(title = "Trump leads in low- to medium-turnout FL election",
       #subtitle = "Margin computed from likely voter subsets based on vote propensity scores",
       x = "Turnout (%)",
       y = "") +
  annotate("text", x = 5, y = 4, label = "Clinton wins", 
           colour = "#008fd5", angle = 90, fontface = "bold") +
  geom_segment(aes(x = 5, y = 6.75, xend = 5, yend = 8.25), colour = "#008fd5",
               arrow = arrow(length=unit(0.1,"cm"), type = "closed")) +
  annotate("text", x = 5, y = -4, label = "Trump wins", 
           colour = "#ff2700", angle = 90, fontface = "bold") +
  geom_segment(aes(x = 5, y = -6.5, xend = 5, yend = -8), colour = "#ff2700",
               arrow = arrow(length=unit(0.1,"cm"), type = "closed")) +
  scale_colour_manual(values = pal) +
  theme_bw() +
  # Set the entire chart region to a light gray color
  theme(panel.background=element_rect(fill="#F0F0F0")) +
  theme(plot.background=element_rect(fill="#F0F0F0")) +
  theme(panel.border=element_rect(colour="#F0F0F0")) +
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.75)) +
  scale_x_continuous(minor_breaks=0,breaks=seq(0,100,10),limits=c(0,100)) +
  scale_y_continuous(minor_breaks=0,breaks=seq(-20,10,5),limits=c(-20,10), labels = paste("+", abs(seq(-20,10,5)))) +
  theme(axis.ticks=element_blank()) +
  # Dispose of the legend
  theme(legend.position="none") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=20)) +
  theme(plot.subtitle=element_text(size=11,colour="#535353")) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  # Plot margins and finally line annotations
  theme(plot.margin = unit(c(1, 1, .5, .7), "cm"),
        plot.title = element_text(hjust = 0.5))
fig17


######## national-level Perry-Gallup index

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

df <- data.frame(model = c("6s","6s and 5s","6s, 5s,\nand 4s","6s, 5s,\n4s, and\n3s", 
                           "6s, 5s,\n4s, 3s,\nand 2s","All"),
                 Clinton = vector(length = 6),
                 Trump = vector(length = 6),
                 margin = vector(length = 6))

i <- 1
while(i <= length(mods)){
  df$Clinton[i] = mods[[i]]$vote_share[mods[[i]]$choice == "Hillary Clinton (Democrat)"] 
  df$Trump[i] = mods[[i]]$vote_share[mods[[i]]$choice == "Donald Trump (Republican)"]
  df$margin[i] = df$Clinton[i] - df$Trump[i]
  i = i + 1
}

df$model <- factor(df$model, levels = rev(df$model))

fig18 <- ggplot(data = df) +
  # points and lines
  geom_segment(aes(x = Trump, y = model, xend = Clinton, yend = model), 
               colour = "#535353", alpha = 0.5, size = 1.5) +
  geom_point(aes(x = Clinton, y = model), colour = "#008fd5", size = 4) +
  geom_point(aes(x = Trump, y = model), colour = "#ff2700", size = 4) +
  # turnout rates
  geom_rect(aes(xmin = 60, xmax = 70, ymin = 0, ymax = 7), fill = "#F0F0F0") +
  annotate("text", x = 65, y = 6.5, label = "Projected turnout rate", colour = "#535353", fontface = "bold") +
  annotate("text" , x = 65, y = c(6,5,4,3,2,1), 
           label = c("< 1%", "41.8%", "65.0%", "81.5%", "90.1%", "100%"),
           colour = "#535353", fontface = "bold") +
  geom_text(aes(x = Trump, y = model, label = round(Trump, 1)),
            colour = "#ff2700", fontface = "bold", hjust = 1.5) +
  geom_text(aes(x = Clinton, y = model, label = round(Clinton, 1)),
            colour = "#008fd5", fontface = "bold", hjust = -0.5) +
  annotate("text", x = 66.4, y = 4.71, label = "53.6% (2012)", colour = "#ED713A", fontface = "bold") +
  annotate("text", x = 66.4, y = 4.41, label = "56.9% (2008)", colour = "#8E3087", fontface = "bold") +
  # set axis text
  labs(title = "Clinton holds small lead under most likely turnout scenarios",
       x = "Vote share (%)",
       y = "Vote likelihood scores") +
  # Set the entire chart region to a light gray color
  theme(panel.background=element_rect(fill="#F0F0F0")) +
  theme(plot.background=element_rect(fill="#F0F0F0")) +
  theme(panel.border=element_rect(colour="#F0F0F0")) +
  # Format the grid
  theme(panel.grid.major= element_blank()) +
  scale_x_continuous(minor_breaks=0,breaks=seq(30,60,5),limits=c(30,70)) +
  theme(axis.ticks=element_blank()) +
  # Dispose of the legend
  theme(legend.position="none") +
  theme(plot.title=element_text(face="bold",vjust=2,colour="#3C3C3C",size=20)) +
  theme(plot.subtitle=element_text(size=11,colour="#535353")) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  # Plot margins and finally line annotations
  theme(plot.margin = unit(c(1, 1, .5, .7), "cm"))
fig18

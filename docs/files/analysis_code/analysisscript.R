###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(tidymodels)
library(readr)       # for importing data
library(tidyverse)
library(dotwhisker)
library(broom.mixed)

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)

lm_mod <- 
       linear_reg() %>% 
       set_engine("lm")
lm_fit <- 
       lm_mod %>% 
       fit(BodyTemp ~ RunnyNose, data = mydata)
lm_fit
glance(lm_fit)
tidy(lm_fit)

lm_fit2 <- lm_mod %>%
                     fit(BodyTemp ~ ., data = mydata)
lm_fit2
glance(lm_fit2)
tidy(lm_fit2)

compare_lin_model <- anova(lm_fit$fit, lm_fit2$fit)
compare_lin_model


lg_mod <- 
       logistic_reg() %>% 
       set_engine("glm")
lg_fit <-
       lg_mod %>%
       fit(Fatigue ~ RunnyNose, data = mydata)
lg_fit
glance(lg_fit)
tidy(lg_fit)

lg_fit2 <- lg_mod %>%
        fit(RunnyNose ~ ., data = mydata)
lg_fit2
glance(lg_fit2)
tidy(lg_fit2)

# save fit results table 
saveRDS(compare_lin_model, file = here("results", "analysis", "LinearModel.rds"))
saveRDS(compare_log_model, file = here("results", "analysis", "LogisticModel.rds"))
 


  
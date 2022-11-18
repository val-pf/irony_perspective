###############
#
# Revision 1: Irony & Perspective
#
################
setwd("N:/Projects/irony and perspective/study2_block_order")
require(psych)
require(car)
require(ez)
require(tidyverse)
require(ordinal)

### Script to create tidy dataset of all responses

response4R <- read.table("list4R_responses.csv", header=TRUE, sep=",") 
summary(response4R)
response4R.long <- response4R %>% 
  select(-name) %>%
  pivot_longer(cols = -c(item, rating, literality, addressee, role),
               names_to = "Subject",
               values_to = "response")
response4R.long$Subject <- paste(response4R.long$Subject, "4R", sep = '_')
response4R.long


response3R <- read.table("list3R_responses.csv", header=TRUE, sep=",") 
summary(response3R)
response3R.long <- response3R %>% 
  select(-name) %>%
  pivot_longer(cols = -c(item, rating, literality, addressee, role),
               names_to = "Subject",
               values_to = "response")

response3R.long$Subject <- paste(response3R.long$Subject, "3R", sep = '_')
response3R.long


response2R <- read.table("list2R_responses.csv", header=TRUE, sep=",") 
summary(response2R)
response2R.long <- response2R %>% 
  select(-name) %>%
  pivot_longer(cols = -c(item, rating, literality, addressee, role),
               names_to = "Subject",
               values_to = "response")

response2R.long$Subject <- paste(response2R.long$Subject, "2R", sep = '_')
response2R.long


response1R <- read.table("list1R_responses.csv", header=TRUE, sep=",") 
summary(response1R)
response1R.long <- response1R %>% 
  select(-name) %>%
  pivot_longer(cols = -c(item, rating, literality, addressee, role),
               names_to = "Subject",
               values_to = "response")

response1R.long$Subject <- paste(response1R.long$Subject, "1R", sep = '_')
response1R.long

###########

response_R <- rbind(response1R.long, response2R.long, response3R.long, response4R.long)
response_R

############################################

response4 <- read.table("list4_responses.csv", header=TRUE, sep=",") 
summary(response4)
response4.long <- response4 %>% 
  select(-name) %>%
  pivot_longer(cols = -c(item, rating, literality, addressee, role),
               names_to = "Subject",
               values_to = "response")
response4.long$Subject <- paste(response4.long$Subject, "4", sep = '_')
response4.long


response3 <- read.table("list3_responses.csv", header=TRUE, sep=",") 
summary(response3)
response3.long <- response3 %>% 
  select(-name) %>%
  pivot_longer(cols = -c(item, rating, literality, addressee, role),
               names_to = "Subject",
               values_to = "response")

response3.long$Subject <- paste(response3.long$Subject, "3", sep = '_')
response3.long


response2 <- read.table("list2_responses.csv", header=TRUE, sep=",") 
summary(response2)
response2.long <- response2 %>% 
  select(-name) %>%
  pivot_longer(cols = -c(item, rating, literality, addressee, role),
               names_to = "Subject",
               values_to = "response")

response2.long$Subject <- paste(response2.long$Subject, "2", sep = '_')
response2.long


response1 <- read.table("list1_responses.csv", header=TRUE, sep=",") 
summary(response1)
response1.long <- response1 %>% 
  select(-name) %>%
  pivot_longer(cols = -c(item, rating, literality, addressee, role),
               names_to = "Subject",
               values_to = "response")

response1.long$Subject <- paste(response1.long$Subject, "1", sep = '_')
response1.long

###########

response <- rbind(response1.long, response2.long, response3.long, response4.long)
response

#############################
response_all <- rbind(response, response_R)
response_all
response_all <- na.omit(response_all)

write.csv(response_all, "all_response_tidy.csv")

##################################
valence <- subset(response_all, rating =="Valence")
valence
class(valence$response)

valence$resp_fac <- as.factor(valence$response)
valence$literality <- as.factor(valence$literality)
valence$addressee <- as.factor(valence$addressee)
valence$role <- as.factor(valence$role)
valence$Subject <- as.factor(valence$Subject)
valence$item <- as.factor(valence$item)
fm1 <- clmm2(resp_fac ~ addressee + role + literality, random=Subject, 
             data=valence, Hess=TRUE)
fm2 <- clmm2(resp_fac ~ addressee + role + literality, random=item, 
             data=valence, Hess=TRUE)
fm1
fm2
summary(fm1)
summary(fm2)
exp(coef(fm1))
plot(fm1)

m1 <- clmm(resp_fac ~ addressee + role + literality + 
             (1|Subject) + (1|item), data=valence)
summary(m1)
# RVAideMemoire::Anova.clmm(m1, type="2") results similar to the model
require(emmeans)
marginal = emmeans(m1,~ addressee + role + literality)
pairs(marginal, adjust= "tukey")

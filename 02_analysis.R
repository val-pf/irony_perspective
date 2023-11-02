response_all$resp_fac <- as.factor(response_all$response)
response_all$literality <- as.factor(response_all$literality)
response_all$addressee <- as.factor(response_all$addressee)
response_all$role <- as.factor(response_all$role)
response_all$Subject <- as.factor(response_all$Subject)
response_all$item <- as.factor(response_all$item)

##################################
##### A N A L Y S I S ############
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

levels(valence$literality)
valence$literality <- relevel(valence$literality, ref="literal")

levels(valence$addressee)
levels(valence$role)
valence$role <- relevel(valence$role, ref="Speaker")

# fm1 <- clmm2(resp_fac ~ addressee + role + literality, random=Subject, 
#              data=valence, Hess=TRUE)
# fm2 <- clmm2(resp_fac ~ addressee + role + literality, random=item, 
#              data=valence, Hess=TRUE)
# fm1
# fm2
# summary(fm1)
# summary(fm2)
# exp(coef(fm1))
# plot(fm1)

m1 <- clmm(resp_fac ~ addressee * role * literality + 
             (1|Subject) + (1|item), data=valence)
summary(m1)
# RVAideMemoire::Anova.clmm(m1, type="2") results similar to the model
require(emmeans)
marginal = emmeans(m1,~ addressee * role * literality)

pairs(marginal, adjust= "tukey")

################# Arousal

arousal <- subset(response_all, rating =="Arousal")
arousal
class(arousal$response)

arousal$resp_fac <- as.factor(arousal$response)
arousal$literality <- as.factor(arousal$literality)
arousal$addressee <- as.factor(arousal$addressee)
arousal$role <- as.factor(arousal$role)
arousal$Subject <- as.factor(arousal$Subject)
arousal$item <- as.factor(arousal$item)

levels(arousal$literality)
arousal$literality <- relevel(arousal$literality, ref="literal")

levels(arousal$addressee)
levels(arousal$role)
arousal$role <- relevel(arousal$role, ref="Speaker")

m1 <- clmm(resp_fac ~ addressee * role * literality + 
             (1|Subject) + (1|item), data=arousal)
summary(m1)
# RVAideMemoire::Anova.clmm(m1, type="2") results similar to the model
require(emmeans)
marginal = emmeans(m1,~ addressee * role * literality)

pairs(marginal, adjust= "tukey")

################################################################
#### FAULT analysis ###
## if data need to be read it
response_all <- read.table("all_response_tidy.csv", header=TRUE,
                           stringsAsFactor=TRUE, sep=",") 
summary(response_all)

fault <- subset(response_all, addressee=="self" & role=="Speaker"
                | addressee=="other" & role=="Listener")
summary(fault)


no_fault <- subset(response_all, addressee=="other" & role=="Speaker"
                   | addressee=="self" & role=="Listener")
summary(no_fault)


valence <- subset(fault, rating =="Valence")  #replace fault with no_fault 
valence
class(valence$response)

valence$resp_fac <- as.factor(valence$response)
valence$literality <- as.factor(valence$literality)
valence$addressee <- as.factor(valence$addressee)
valence$role <- as.factor(valence$role)
valence$Subject <- as.factor(valence$Subject)
valence$item <- as.factor(valence$item)

levels(valence$literality)
valence$literality <- relevel(valence$literality, ref="literal")

levels(valence$addressee)
levels(valence$role)
valence$role <- relevel(valence$role, ref="Speaker")


m1 <- clmm(resp_fac ~  role * literality + 
             (1|Subject) + (1|item), data=valence)
summary(m1)

require(emmeans)
marginal = emmeans(m1,~ role + literality)
pairs(marginal, adjust= "tukey")

arousal <- subset(fault, rating =="Arousal") #replace fault with no_fault 
arousal
class(arousal$response)

arousal$resp_fac <- as.factor(arousal$response)
arousal$literality <- as.factor(arousal$literality)
arousal$addressee <- as.factor(arousal$addressee)
arousal$role <- as.factor(arousal$role)
arousal$Subject <- as.factor(arousal$Subject)
arousal$item <- as.factor(arousal$item)

levels(arousal$literality)
arousal$literality <- relevel(arousal$literality, ref="literal")

levels(arousal$addressee)
levels(arousal$role)
arousal$role <- relevel(arousal$role, ref="Speaker")

m1 <- clmm(resp_fac ~  role * literality + 
             (1|Subject) + (1|item), data=arousal)
summary(m1)

marginal = emmeans(m1,~  role * literality)

pairs(marginal, adjust= "tukey")



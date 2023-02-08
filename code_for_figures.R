# Figures for irony paper
require(psych)
require(tidyverse)

########################################################
sem <- function(x){ sd(x)/sqrt(length(x)) }

study2 <- read.table("item_analysis.csv", header=TRUE, stringsAsFactors=TRUE, sep=",") 
describe(study2)
summary(study2)


###########################################################################
## making subsets of the data per Role

##  LISTENER ##

###########################################################################
Listener <- subset(study2, role=="Listener")
summary(Listener)

## VALENCE ## 
resp_stats <- Listener %>%
  group_by(cond) %>%
  summarize(ave_Rat = mean(mean_val),Rat_se = sem(mean_val))
resp_stats


id <- Listener %>% group_by(cond) %>% summarize(vale=mean_val)
id

ggplot(data = id, mapping = aes(x = vale, y = cond, fill=cond)) +
  geom_boxplot(alpha =.5, outlier.shape=NA) +
  geom_jitter(height=0.05)+ 
  scale_fill_manual(values=c("maroon","maroon4", "turquoise","turquoise4"))+
  theme(legend.position = "none")+ 
  theme(text = element_text(size = 19))+
  ggtitle("Listener Valence")+
  xlab("Average Valence Rating") + ylab("")+
  coord_cartesian(xlim=c(0,6))


## AROUSAL ##

resp_stats <- Listener %>%
  group_by(cond) %>%
  summarize(ave_Rat = mean(mean_ar),Rat_se = sem(mean_ar))
resp_stats


id <- Listener %>% group_by(cond) %>% summarize(vale=mean_ar)
id

ggplot(data = id, mapping = aes(x = vale, y = cond, fill=cond)) +
  geom_boxplot(alpha =.5, outlier.shape=NA) +
  geom_jitter(height=0.05)+ 
  scale_fill_manual(values=c("maroon","maroon4", "turquoise","turquoise4"))+
  theme(legend.position = "none")+ 
  theme(text = element_text(size = 19))+
  ggtitle("Listener Arousal")+
  xlab("Average Arousal Rating") + ylab("")+
  coord_cartesian(xlim=c(0,6))


###########################################################################

##  SPEAKER ##

###########################################################################
Speaker <- subset(study2, role=="Speaker")

## VALENCE ##
resp_stats <- Speaker %>%
  group_by(cond) %>%
  summarize(ave_Rat = mean(mean_val),Rat_se = sem(mean_val))
resp_stats


id <- Speaker %>% group_by(cond) %>% summarize(vale=mean_val)
id

ggplot(data = id, mapping = aes(x = vale, y = cond, fill=cond)) +
  geom_boxplot(alpha =.5, outlier.shape=NA) +
  geom_jitter(height=0.05)+ 
  scale_fill_manual(values=c("maroon","maroon4", "turquoise","turquoise4"))+
  theme(legend.position = "none")+ 
  theme(text = element_text(size = 19))+
  ggtitle("Speaker Valence")+
  xlab("Average Valence Rating") + ylab("")+
  coord_cartesian(xlim=c(0,6))

## AROUSAL ##

resp_stats <- Speaker %>%
  group_by(role) %>%
  summarize(ave_Rat = mean(mean_ar),Rat_se = sem(mean_ar))
resp_stats


id <- Speaker %>% group_by(cond) %>% summarize(vale=mean_ar)
id

ggplot(data = id, mapping = aes(x = vale, y = cond, fill=cond)) +
  geom_boxplot(alpha =.5, outlier.shape=NA) +
  geom_jitter(height=0.05)+ 
  scale_fill_manual(values=c("maroon","maroon4", "turquoise","turquoise4"))+
  theme(legend.position = "none")+ 
  theme(text = element_text(size = 19))+
  ggtitle("Speaker Arousal")+
  xlab("Average Arousal Rating") + ylab("")+
  coord_cartesian(xlim=c(0,6))


##########################################################################
##########################################################################
##########################################################################

study2 <- read.table("fault.csv", header=TRUE, stringsAsFactors=TRUE, sep=",") 
describe(study2)

fault <-subset(study2, fault=="yes")
summary(fault)

resp_stats <- fault %>%
  group_by(role) %>%
  summarize(ave_Rat = mean(mean_val),Rat_se = sem(mean_val))
resp_stats


id <- fault %>% group_by(role) %>% summarize(vale=mean_val)
id

ggplot(data = id, mapping = aes(x = vale, y = role, fill=role)) +
  geom_boxplot(alpha =.5, outlier.shape=NA) +
  geom_jitter(height=0.05)+ 
  scale_fill_manual(values=c("maroon","turquoise"))+
  theme(legend.position = "none")+ 
  theme(text = element_text(size = 19))+
  ggtitle("At fault Valence")+
  xlab("Average Valence Rating") + ylab("")+
  coord_cartesian(xlim=c(0,6))

## AROUSAL ##

resp_stats <- fault %>%
  group_by(role) %>%
  summarize(ave_Rat = mean(mean_ar),Rat_se = sem(mean_ar))
resp_stats


id <- fault %>% group_by(role) %>% summarize(vale=mean_ar)
id

ggplot(data = id, mapping = aes(x = vale, y = role, fill=role)) +
  geom_boxplot(alpha =.5, outlier.shape=NA) +
  geom_jitter(height=0.05)+ 
  scale_fill_manual(values=c("maroon", "turquoise"))+
  theme(legend.position = "none")+ 
  theme(text = element_text(size = 19))+
  ggtitle("At fault Arousal")+
  xlab("Average Arousal Rating") + ylab("")+
  coord_cartesian(xlim=c(0,6))

##############################################################
##############################################################
nofault <- subset(study2, fault =="no")

resp_stats <- nofault %>%
  group_by(role) %>%
  summarize(ave_Rat = mean(mean_val),Rat_se = sem(mean_val))
resp_stats


id <- nofault %>% group_by(role) %>% summarize(vale=mean_val)
id

ggplot(data = id, mapping = aes(x = vale, y = role, fill=role)) +
  geom_boxplot(alpha =.5, outlier.shape=NA) +
  geom_jitter(height=0.05)+ 
  scale_fill_manual(values=c("maroon","turquoise"))+
  theme(legend.position = "none")+ 
  theme(text = element_text(size = 19))+
  ggtitle("Not at fault Valence")+
  xlab("Average Valence Rating") + ylab("")+
  coord_cartesian(xlim=c(0,6))

## AROUSAL ##

resp_stats <- nofault %>%
  group_by(role) %>%
  summarize(ave_Rat = mean(mean_ar),Rat_se = sem(mean_ar))
resp_stats


id <- nofault %>% group_by(role) %>% summarize(vale=mean_ar)
id

ggplot(data = id, mapping = aes(x = vale, y = role, fill=role)) +
  geom_boxplot(alpha =.5, outlier.shape=NA) +
  geom_jitter(height=0.05)+ 
  scale_fill_manual(values=c("maroon", "turquoise"))+
  theme(legend.position = "none")+ 
  theme(text = element_text(size = 19))+
  ggtitle("Not at fault Arousal")+
  xlab("Average Arousal Rating") + ylab("")+
  coord_cartesian(xlim=c(0,6))

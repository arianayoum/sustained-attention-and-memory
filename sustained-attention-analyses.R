# Load packages
pacman::p_load(dplyr, lme4, afex, ez, ggplot2, tidyverse, RColorBrewer, wesanderson,
               data.table, retimes, psycho, formattable, ggpubr, effsize, robustHD)

# Set working directories
Dir <- "/Volumes/home/Behavioural Data/"
OA_Dir<-"/Volumes/home/Behavioural Data/OA Data/Day 1/"
YA_Dir<-"/Volumes/home/Behavioural Data/YA Data/"

# Demographics
setwd(Dir)
Demographics <- read.csv('Study1_Demographics.csv', header=TRUE)

# Shipley standard score analysis

ggboxplot(Demographics, x = "Group", y = "Shipley", 
          color = "Group", palette = c("#00AFBB", "#E7B800"),
          ylab = "Shipley Raw Scores", xlab = "Age")

ggboxplot(Demographics, x = "Group", y = "Shipley_standardized", 
          color = "Group", palette = c("#00AFBB", "#E7B800"),
          ylab = "Shipley Standardized Scores", xlab = "Age")

# Welch t-test
t.test(Shipley ~ Group, data=Demographics,
       conf.level=0.95)

t.test(Shipley_standardized ~ Group, data=Demographics,
       conf.level=0.95)

#######################################################
################# Pattern Completion ##################
#######################################################
# Before analyzing the data, I will recode words with same root/meaning 
# as study words as "0" in Exclusion. 

# Directory change 
setwd(paste(OA_Dir,"PSPC/", sep=""))

# Load in OA data
OA.INC <- rbindlist(lapply(list.files(pattern = "*inclusion.csv", recursive = TRUE), 
                           read.csv, header=TRUE), use.names=TRUE)
OA.INC$Age = 'Older'
OA.INC$Condition = 'Inclusion'

# Directory change
setwd(paste(YA_Dir,"PSPC/", sep=""))

# Load in YA data 
YA.INC <- rbindlist(lapply(list.files(pattern = "*inclusion.csv", recursive = TRUE), 
                           read.csv, header=TRUE), use.names=TRUE)
YA.INC$Age = 'Younger'
YA.INC$Condition = 'Inclusion'

# Combine
PC.inc <- data.frame(rbind(OA.INC, YA.INC)) %>% 
  dplyr::select(-task)

# Read in rated exclusions
setwd(Dir)
PC.exc <- read.csv('PC_Exclusions_ratings.csv')

PC.all <- data.frame(rbind(PC.inc, PC.exc))

temp <-PC.all %>%
  group_by(subject) %>%
  summarise(count = n())

PC.all <- PC.all %>%
  mutate_if(is.character, str_trim)

PC.all['task'] <- NULL

# Recode missed as NA
PC.all[PC.all == 999] <- NA

# Make sure that missed items are coded as incorrect only for Inclusion task
PC.all[PC.all$Condition == "Inclusion" & is.na(PC.all$accuracy), "accuracy"] = 0

# Make sure that missed items are coded as NA only for Exclusion task
PC.all[PC.all$Condition == "Exclusion" & is.na(PC.all$response), "accuracy"] = NA

## Excluding participants who generate no words on either inclusion or exclusion
PC.acc <- PC.all %>% 
  group_by(Age, subject, Condition) %>%
  summarise(acc=mean(accuracy, na.rm=TRUE))

# print outliers if they exist 
for (i in PC.acc$acc){
  if (i == 0) {
    print(PC.acc[which(PC.acc == 0), ])
  }
}

## Calculating automatic estimates
require(dplyr)

PC.inc <- PC.all %>% 
  dplyr::select(Age, subject, Condition, accuracy) %>% 
  filter(Condition=='Inclusion') %>% 
  group_by(subject, Age) %>% 
  summarise(inclusion=mean(accuracy))

PC.exc <- PC.all %>% 
  dplyr::select(Age, subject, Condition, accuracy) %>% 
  filter(Condition=='Exclusion') %>% 
  group_by(subject, Age) %>% 
  summarise(exclusion=length(which(accuracy==0))/length(accuracy))

PC <- merge(PC.inc, PC.exc)

PC$controlled <- PC$inclusion-PC$exclusion
PC$automatic <- PC$exclusion/(1-PC$controlled)

# Winsorize outliers within group
PC <- PC %>% 
  group_by(Age) %>% 
  mutate(controlled.w=winsorize(controlled),
         automatic.w=winsorize(automatic))

# Visualization of group differences in automatic vs controlled processes
ggboxplot(PC, x = "Age", y = "automatic.w", 
          color = "Age", palette = c("#00AFBB", "#E7B800"),
          ylab = "Automatic Processes", xlab = "Age")

ggboxplot(PC, x = "Age", y = "controlled.w", 
          color = "Age", palette = c("#00AFBB", "#E7B800"),
          ylab = "Controlled Processes", xlab = "Age")


# PDP ESTIMATES
PC.auto <- PC %>% dplyr::select(subject, Age, automatic, automatic.w)
t.test(automatic.w ~ Age, data = PC.auto, var.equal = TRUE)

PC.controlled <- PC %>% dplyr::select(subject, Age, controlled, controlled.w)
t.test(controlled.w ~ Age, data = PC.controlled, var.equal = TRUE)

# Effect size
library(lsr)
cohensD(automatic.w ~ Age, data = PC.auto) 
cohensD(controlled.w ~ Age, data = PC.controlled) 

################# Accounting for Shipley ##################
PC.auto <- merge(PC.auto, Demographics, by='subject')
PC.auto$shipley.sc <- scale(PC.auto$Shipley_standardized)
summary(lm(automatic.w ~ Group * shipley.sc, data = PC.auto))

PC.controlled <- merge(PC.controlled, Demographics, by='subject')
PC.controlled$shipley.sc <- scale(PC.controlled$Shipley_standardized)
summary(lm(controlled.w ~ Group * shipley.sc, data = PC.controlled))

#######################################################
################# Pattern Separation ##################
#######################################################

# Change directory
setwd(paste(OA_Dir,"PSPC/", sep=""))

# Load OA data
OA.PS <- do.call("rbind", lapply(
  list.files(pattern = "*PSretrieval.csv", recursive = TRUE), 
  read.csv, header=TRUE))
OA.PS$Age = 'Older'

# Change directory
setwd(paste(YA_Dir,"PSPC/", sep=""))

# Load YA data
YA.PS <- do.call("rbind", lapply(
  list.files(pattern = "*PSretrieval.csv", recursive = TRUE), 
  read.csv, header=TRUE))
YA.PS$Age = 'Younger'

# Combine
PS.all <- rbind(YA.PS, OA.PS)

temp <-PS.all %>%
  group_by(subject) %>%
  summarise(count = n())

# make sure relevant columns are character strings
col_char <- c('condition', 'word', 'answer', 'Age')
col_int <- c('accuracy')

PS.all[, col_char] <- sapply(PS.all[, col_char], as.character)
PS.all[, col_int] <- sapply(PS.all[, col_int], as.integer)

# remove unwanted white spaces
PS.all <- PS.all %>%
  mutate_if(is.character, str_trim)

## Excluding trials with RT < 200 ms
PS.all<-PS.all[!(PS.all$RT<=0.2),]

# Subset without missed trials
PS.all <- subset(PS.all, answer!='999')

## Excluding trials with words that participant rated as "1" or "don't know"
# Change directory
setwd(paste(OA_Dir,"PSPC/", sep=""))

# OA Encoding
OA.enc <- do.call("rbind", lapply(
  list.files(pattern = "*PSencoding.csv", recursive = TRUE), 
  read.csv, header=TRUE))
OA.enc$Age = 'Older'

# Change directory
setwd(paste(YA_Dir,"PSPC/", sep=""))

# YA Encoding
YA.enc <- do.call("rbind", lapply(
  list.files(pattern = "*PSencoding.csv", recursive = TRUE), 
  read.csv, header=TRUE))
YA.enc$Age = 'Younger'

PS.encoding <- rbind(OA.enc, YA.enc)

PS.exclusions <- PS.encoding %>% 
  filter(response=='1')

# make sure relevant columns are character strings
col2_char <- c('word', 'Age')
PS.exclusions[, col2_char] <- sapply(PS.exclusions[, col2_char], as.character)

# remove unwanted white spaces
PS.exclusions <- PS.exclusions %>%
  mutate_if(is.character, str_trim)

PS.all <- dplyr::anti_join(PS.all, PS.exclusions, by=c('subject', 'word'))

## Summary of "old" responses
PropOld <- PS.all %>% 
  group_by(Age, subject) %>% 
  summarise(Targets=sum(condition=='Old' & answer=='Old', na.rm = TRUE)/(sum(condition=='Old', na.rm = TRUE)),
            Lures=sum(condition=='Lure' & answer=='Old', na.rm = TRUE)/(sum(condition=='Lure', na.rm = TRUE)),
            Foils=sum(condition=='New' & answer=='Old', na.rm = TRUE)/(sum(condition=='New', na.rm = TRUE))
  )

PropOldLong <- PropOld %>% 
  gather(condition, proportion, Targets:Foils, factor_key = TRUE)

PropOldMeans <- PropOld %>% 
  group_by(Age) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)), -subject) %>% 
  gather(condition, proportion, Targets:Foils, factor_key=TRUE)

PropOldSE <- PropOld %>% 
  group_by(Age) %>% 
  summarise_each(funs(se=sd(.)/sqrt(n())), -subject) %>% 
  gather(condition, se, Targets_se:Foils_se, factor_key=TRUE)

PropOldSD <- PropOld %>% 
  group_by(Age) %>% 
  summarise_each(funs(sd=sd(.)), -subject) %>% 
  gather(condition, sd, Targets_sd:Foils_sd, factor_key=TRUE)

PropOld <- cbind(PropOldMeans, PropOldSE)
PropOld$lower <- PropOld$proportion - PropOld$se
PropOld$upper <- PropOld$proportion + PropOld$se
PropOld <- PropOld[,-c(4:5)]

ageOrder <- c('Younger', 'Older')
pOld <-  ggplot(PropOld, aes(x=as.factor(condition), 
                             y=proportion, 
                             fill=factor(Age, level=ageOrder))) + 
  ylim(0,1) +
  geom_bar(colour='black', stat='identity', width=0.4,position=position_dodge(width=0.5)) + 
  scale_fill_manual(values=c('mistyrose3','mistyrose2')) +
  geom_errorbar(aes(ymin=lower, ymax=upper), 
                position=position_dodge(width=0.5), width=0.2, size=0.5) +
  labs(y='Proportion Old', x=NULL) +
  theme_classic() +
  theme(legend.title=element_blank(),
        legend.text = element_text(margin = margin(r = 10, unit = "pt"), size = 15),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15))

pOld

aov.model <- aov(proportion ~ condition + Age + condition:Age, data = PropOldLong %>% filter(condition!='Foils'))
summary(aov.model)
aov.full.model <- aov(proportion ~ condition + Age + condition:Age, 
                      data = PropOldLong %>% filter(condition!='Targets'))
summary(aov.full.model)

# Lures vs Foils Analysis
LF.anova <- aov(proportion ~ Age*condition, data = PropOldLong %>% filter(condition!='Targets'))
summary(LF.anova)

library(emmeans)
LF.Simple.Effects.By.Type<-emmeans(LF.anova, ~Age|Cond)
pairs(LF.Simple.Effects.By.Type,adjust='holm') %>% summary(infer = TRUE)

# Histogram for false alarms
PS.FA.Lure <- PS.all %>% 
  filter(condition=='Lure' & answer=='Old') 
barplot(prop.table(table(PS.FA.Lure$word)), cex.names=0.1, las=2)

PS.FA.Foil <- PS.all %>% 
  filter(condition=='New' & answer=='Old')
barplot(prop.table(table(PS.FA.Foil$word)),cex.names=0.1,las=2)

## Loglinear correction -- adding 0.5 to both the number of hits and the number of false alarms, and adding 1 to both the number of signal trials and the number of noise trials 
# For target-foil distribution

T_F <- PS.all %>% 
  group_by(subject, Age) %>% 
  summarise(TF.hit=(((sum(condition=='Old' & answer=='Old'))+0.5)/((sum(condition=='Old'))+1)),
            TF.miss=((sum(condition=='Old' & answer=='New'))/((sum(condition=='Old'))+1)),
            TF.FA=(((sum(condition=='New' & answer=='Old'))+0.5)/((sum(condition=='New'))+1)),
            TF.CR=(sum(condition=='New' & answer=='New'))/((sum(condition=='New'))+1)) %>% 
  dplyr::select(subject, Age, TF.hit, TF.miss, TF.FA, TF.CR)

# For target-lure distribution

T_L <- PS.all %>% 
  group_by(subject, Age) %>% 
  summarise(TL.hit=(((sum(condition=='Old' & answer=='Old'))+0.5)/((sum(condition=='Old'))+1)),
            TL.miss=((sum(condition=='Old' & answer=='New'))/((sum(condition=='Old'))+1)),
            TL.FA=(((sum(condition=='Lure' & answer=='Old'))+0.5)/((sum(condition=='Lure'))+1)),
            TL.CR=(sum(condition=='Lure' & answer=='New'))/((sum(condition=='Lure'))+1)) %>% 
  dplyr::select(subject, Age, TL.hit, TL.miss, TL.FA, TL.CR)

# For lure-foil distribution

L_F <- PS.all %>% 
  group_by(subject, Age) %>% 
  summarise(LF.hit=(((sum(condition=='Lure' & answer=='Old'))+0.5)/((sum(condition=='Lure'))+1)),
            LF.miss=((sum(condition=='Lure' & answer=='New'))/((sum(condition=='Lure'))+1)),
            LF.FA=(((sum(condition=='New' & answer=='Old'))+0.5)/((sum(condition=='New'))+1)),
            LF.CR=(sum(condition=='New' & answer=='New'))/((sum(condition=='New'))+1)) %>% 
  dplyr::select(subject, Age, LF.hit, LF.miss, LF.FA, LF.CR)

## Calculating SDT measures
# Calculating d'[T,F]
## Difference in distributions between “old” responses to old targets and “old” responses to new foils

TF.dprime <- T_F %>% 
  group_by(subject) %>% 
  mutate(TF_dprime = as.numeric(unlist(psycho::dprime(TF.hit, TF.FA, TF.miss, TF.CR)['dprime']))) %>% 
  dplyr::select(subject, Age, TF_dprime)

# Calculating d'[T,L]
## Difference in distributions between “old” responses to similar lures and “old” responses to new foils

TL.dprime <- T_L %>% 
  group_by(subject) %>% 
  mutate(TL_dprime = as.numeric(unlist(psycho::dprime(TL.hit, TL.FA, TL.miss, TL.CR)['dprime']))) %>% 
  dplyr::select(subject, Age, TL_dprime)

# Calculating d'[L,F]
## Difference in distributions between “old” responses to old targets and “old” responses to similar lures

LF.dprime <- L_F %>% 
  group_by(subject) %>% 
  mutate(LF_dprime = as.numeric(unlist(psycho::dprime(LF.hit, LF.FA, LF.miss, LF.CR)['dprime']))) %>% 
  dplyr::select(subject, Age, LF_dprime)

# Combine dataframes

PS.sdt <- merge(TF.dprime, TL.dprime) %>% 
  merge(LF.dprime)

# d' to Ad'

library(psych)

dprime_to_Ad <- function(d.prime.value) {
  Ad <- pnorm((d.prime.value)/sqrt(2))
  return(Ad)
}

PS.sdt <- PS.sdt %>%
  mutate(TF.Ad=dprime_to_Ad(TF_dprime),
         TL.Ad=dprime_to_Ad(TL_dprime),
         LF.Ad=dprime_to_Ad(LF_dprime)
  )

## Excluding participants who are unable to discriminate between old targets and new foils 
PS.sdt<-PS.sdt[!(PS.sdt$TF.Ad<=0.5),]

# Ad summary
Ad_summary <- PS.sdt %>% 
  group_by(Age) %>% 
  summarise(mean.TF.Ad=mean(TF.Ad),
            mean.TL.Ad=mean(TL.Ad),
            mean.LF.Ad=mean(LF.Ad),
            sd.TF.Ad=sd(TF.Ad),
            sd.TL.Ad=sd(TL.Ad),
            sd.LF.Ad=sd(LF.Ad))

# Make long
PS.sdt.long <- PS.sdt %>% 
  gather(Type, Ad, TF.Ad:LF.Ad, factor_key=TRUE) %>% 
  dplyr::select(Age, Type, Ad)

# Variables as factors
PS.sdt.long$Age.f <- as.factor(PS.sdt.long$Age)
PS.sdt.long$Type.f <- as.factor(PS.sdt.long$Type)

# Run ANOVA
PS.anova <- aov(Ad ~ Age.f*Type.f, data = PS.sdt.long)
summary(PS.anova)

# Simple Effects
library(emmeans)
PS.Simple.Effects.By.Type<-emmeans(PS.anova, ~Age.f|Type.f)
pairs(PS.Simple.Effects.By.Type,adjust='holm') %>% summary(infer = TRUE)

# Lures
PS.lures <- PS.all  %>%
  filter(condition=='Lure') %>% 
  dplyr::select(Age, subject, word, answer, accuracy, RT) %>%
  mutate_if(is.character, str_trim) %>% 
  mutate(Condition='Lure')

# Foils
PS.foils <- PS.all  %>%
  filter(condition=='New') %>%  
  dplyr::select(Age, subject, word, answer, accuracy, RT) %>%
  mutate_if(is.character, str_trim) %>% 
  mutate(Condition='New')

PS.explore <- rbind(PS.lures, PS.foils) %>%
  mutate_if(is.character, str_trim) %>% 
  mutate(word=tolower(word))

PS.study <- PS.encoding %>% 
  mutate(Study=tolower(word)) %>% 
  dplyr::select(subject, Study) %>%
  mutate_if(is.character, str_trim)

# Add in control measures
setwd(Dir)
word.control <- read.csv('Concreteness_ratings_Brysbaert_et_al_BRM.csv') %>%
  mutate_if(is.character, str_trim)

PS.explore <- left_join(PS.explore, word.control, by=c("word"="Word")) %>% 
  mutate(length=nchar(word))

# If the subject matches between "PS.study" and "PS.explore", for every Lure word
# get the similarity rating from "leacock_chodorow" to every Study word

PS.explore$subject <- as.integer(as.character(PS.explore$subject))

allwords <- left_join(PS.explore, PS.study, by=c("subject")) %>% 
  mutate(word=as.character(word),
         Study=as.character(Study))

## ADD IN MEADOWS ##
setwd('/Volumes/home/Experiment/Meadows/Data/')

meadows_sim <- read.csv('meadows_dissimilarity.csv')
meadows_sim$similarity = 1-meadows_sim$dissimilarity

meadows_sim1 <- meadows_sim %>% 
  rename(Study=conds_1) %>% 
  rename(word=conds_2) %>% 
  mutate(word=as.character(tolower(word)), 
         Study=as.character(tolower(Study))) %>%
  mutate_if(is.character, str_trim) %>% 
  dplyr::select(Study, word, similarity)

meadows_sim2 <- meadows_sim %>% 
  rename(Study=conds_2) %>% 
  rename(word=conds_1) %>% 
  mutate(word=as.character(tolower(word)),
         Study=as.character(tolower(Study))) %>%
  mutate_if(is.character, str_trim) %>% 
  dplyr::select(Study, word, similarity)

meadows_sim_all <- rbind(meadows_sim1, meadows_sim2)

meadows_sim <- merge(allwords, meadows_sim_all, by = c("Study", "word")) %>% 
  dplyr::select(subject, Age, Study, length, Conc.M, Conc.SD, SUBTLEX, Condition, word,
                similarity, answer, accuracy, RT) %>% 
  rename(meadows_sim=similarity)

# SIMILARITY ANALYSES #
sim_summary <- meadows_sim %>% 
  group_by(subject, word) %>% 
  summarise(
    meadows.mean.Sim=mean(meadows_sim),
    meadows.max.Sim=max(meadows_sim),
    accuracy=mean(accuracy),
    RT=mean(RT),
    mean.freq=mean(SUBTLEX, na.rm=TRUE),
    mean.length=mean(length),
    sd.length=sd(length),
    mean.Conc=mean(Conc.M, na.rm=TRUE),
    sd.Conc=sd(Conc.M, na.rm=TRUE),
    mean.SUBTLEX=mean(SUBTLEX, na.rm=TRUE),
    sd.SUBTLEX=sd(SUBTLEX, na.rm=TRUE)
  )

sim_summary_sc <- sim_summary %>% 
  mutate(
    meadows.mean.Sim.sc=scale(meadows.mean.Sim),
    meadows.max.Sim.sc=scale(meadows.max.Sim),
    mean.length.scaled=scale(mean.length),
    mean.Conc.scaled=scale(mean.Conc),
    mean.freq.scaled=scale(mean.freq))

# Check histogram
hist(sim_summary_sc$meadows.mean.Sim.sc)

# SIMILARITY x ACCURACY ANALYSES #

## Set up model
# Effect code Age
sim_summary_sc$Age.ec <- ifelse(sim_summary_sc$subject < 300, 1, -1)

# Scale shipley
shipley <- Demographics %>% dplyr::select(subject, Shipley, Shipley_standardized)
sim_summary_sc <- left_join(sim_summary_sc, shipley, by="subject")

sim_summary_sc$shipley.sc <- scale(sim_summary_sc$Shipley)
sim_summary_sc$shipley.std.sc <- scale(sim_summary_sc$Shipley_standardized)

#### Transform skewed measures
# Concreteness
sim_summary_sc$mean.Conc.scaled <- scale(log(sim_summary_sc$mean.Conc))
hist(sim_summary_sc$mean.Conc.scaled)

# Frequency
library(MASS)
bc <- boxcox(sim_summary_sc$mean.freq ~ 1, lambda = seq(-5, 5, 0.1))
best_lambda <- bc$x[which.max(bc$y)]
sim_summary_sc$freq_boxcox_transformed <- (sim_summary_sc$mean.freq^best_lambda - 1) / best_lambda
hist(sim_summary_sc$freq_boxcox_transformed)

sim_summary_sc$mean.freq.scaled <- scale(sim_summary_sc$freq_boxcox_transformed)

# Meadows
hist(sim_summary_sc$meadows.mean.Sim.sc)
hist(sim_summary_sc$shipley.std.sc)

## Check for outliers
which(abs(sim_summary_sc$meadows.mean.Sim.sc) > 3) # out of all the variables, mean meadows has outliers
sim_summary_sc$meadows.mean.Sim_w <- Winsorize(sim_summary_sc$meadows.mean.Sim)
sim_summary_sc$meadows.mean.Sim_w_sc <- scale(sim_summary_sc$meadows.mean.Sim_w)
which(abs(sim_summary_sc$meadows.mean.Sim_w_sc) > 3) # winsorizing removed the outliers

##### Define Model ##########
# Meadows Mean (scaled) - shipley standardized
meadows.mean.stdShipley.model <- glmer(accuracy ~ meadows.mean.Sim_w_sc * Age.ec + meadows.mean.Sim_w_sc * shipley.std.sc +
                                         mean.Conc.scaled + mean.freq.scaled + mean.length.scaled +
                                         (meadows.mean.Sim_w_sc + mean.Conc.scaled + mean.freq.scaled|subject), 
                                       family=binomial, data=sim_summary_sc)
summary(meadows.mean.stdShipley.model)

plot(ggpredict(meadows.mean.stdShipley.model, terms=c('meadows.mean.Sim_w_sc', 'Age.ec')))

################## SLOPES ANALYSES - MEADOWS #######################

################ Get Individual Slopes #######################
# Create empty dataframe for slope data
Study1_slopes <- data.frame(subject = integer(), slope = numeric())

# Fill slope data
for (i in unique(sim_summary_sc$subject)) {
  mod <- glm(accuracy ~ meadows.mean.Sim_w_sc + mean.freq.scaled, 
             family=binomial, 
             data=(sim_summary_sc %>% filter(subject==i)))
  # Extract coefficients
  cf <- coef(mod)
  Slope <- cf["meadows.mean.Sim_w_sc"]
  temp <- data.frame(subject = i, slope = Slope)
  Study1_slopes <- rbind(Study1_slopes, temp)
}

Study1_slopes$Age <- as.factor(ifelse(Study1_slopes$subject < 300, 'Younger', 'Older'))
Study1_slopes$Slope.w <- winsorize(Study1_slopes$slope)
Study1_slopes$subject <- as.integer(Study1_slopes$subject)


######################### OBJECT MST ##########################

# Set working directory
OA_Dir<-"/Volumes/home/Behavioural Data/OA Data/Day 1/MST/"
YA_Dir<-"/Volumes/home/Behavioural Data/YA Data/MST/"

# Read in files and add subject ID 
setwd(YA_Dir)

files <- list.files(pattern = "*test.csv", recursive = TRUE)
YA.MST <- lapply(files, read.csv, header=TRUE)
for (i in 1:length(YA.MST)){
  YA.MST[[i]] <- cbind(YA.MST[[i]], substr(files[i], 5,7))
}
YA.MST <- do.call("rbind", YA.MST)
YA.MST$Age <- 'Younger'

setwd(OA_Dir)

files <- list.files(pattern = "*test.csv", recursive = TRUE)
OA.MST <- lapply(files, read.csv, header=TRUE)
for (i in 1:length(OA.MST)){
  OA.MST[[i]] <- cbind(OA.MST[[i]], substr(files[i], 5,7))
}
OA.MST <- do.call("rbind", OA.MST)
OA.MST$Age <- 'Older'

MST <- rbind(OA.MST, YA.MST)

## Rename & organize columns
colnames(MST)[colnames(MST)=="substr(files[i], 5, 7)"] <- "subject"
MST <- MST %>% dplyr::select(subject, Age, everything()) %>% 
  mutate(subject=as.integer(subject))
colnames(MST)[colnames(MST)=="RT"] <- "Acc"
colnames(MST)[colnames(MST)=="Corr"] <- "RT"

# Only include not excluded participants & ones that did the MST
# temp <- left_join(Demographics, MST, by="subject")

# Remove subjects who didn't do the MST
temp <- MST %>% 
  group_by(subject) %>% 
  summarise(count = n())  %>% 
  filter(count==120)

# Analysis
# Excluding trials with RT < 200 ms
MST<-MST[!(MST$RT<=0.2),]

# Summary of "old" responses
MST.prop.old <- MST %>% 
  na.omit() %>% 
  group_by(subject, Age, Cond) %>% 
  summarise(prop.old=(sum(Resp==1)/sum(Resp)))

MST.summary <- MST.prop.old %>% 
  group_by(Age, Cond) %>% 
  summarise(mean.prop.old=mean(prop.old))

# Lures vs Foils Analysis
LF.anova <- aov(prop.old ~ Age*Cond, data = MST.prop.old %>% filter(Cond == 'TL' | Cond == 'TF'))
summary(LF.anova)

library(emmeans)
LF.Simple.Effects.By.Type<-emmeans(LF.anova, ~Age|Cond)
pairs(LF.Simple.Effects.By.Type,adjust='holm') %>% summary(infer = TRUE)

## Loglinear correction -- adding 0.5 to both the number of hits and the number of false alarms, and adding 1 to both the number of signal trials and the number of noise trials 

# For target-foil distribution

MST.T_F <- MST %>% 
  group_by(subject, Age) %>% 
  summarise(TF.hit=(((sum(Cond=='TR' & Resp==1))+0.5)/((sum(Cond=='TR'))+1)),
            TF.miss=((sum(Cond=='TR' & Resp==2))/((sum(Cond=='TR'))+1)),
            TF.FA=(((sum(Cond=='TF' & Resp==1))+0.5)/((sum(Cond=='TF'))+1)),
            TF.CR=(sum(Cond=='TF' & Resp==2))/((sum(Cond=='TF'))+1)) %>% 
  dplyr::select(subject, Age, TF.hit, TF.miss, TF.FA, TF.CR)

# For target-lure distribution

MST.T_L <- MST %>% 
  group_by(subject, Age) %>% 
  summarise(TL.hit=(((sum(Cond=='TR' & Resp==1))+0.5)/((sum(Cond=='TR'))+1)),
            TL.miss=((sum(Cond=='TR' & Resp==2))/((sum(Cond=='TR'))+1)),
            TL.FA=(((sum(Cond=='TL' & Resp==1))+0.5)/((sum(Cond=='TL'))+1)),
            TL.CR=(sum(Cond=='TL' & Resp==2))/((sum(Cond=='TL'))+1)) %>% 
  dplyr::select(subject, Age, TL.hit, TL.miss, TL.FA, TL.CR)

# For lure-foil distribution

MST.L_F <- MST %>% 
  group_by(subject, Age) %>% 
  summarise(LF.hit=(((sum(Cond=='TL' & Resp==1))+0.5)/((sum(Cond=='TL'))+1)),
            LF.miss=((sum(Cond=='TL' & Resp==2))/((sum(Cond=='TL'))+1)),
            LF.FA=(((sum(Cond=='TF' & Resp==1))+0.5)/((sum(Cond=='TF'))+1)),
            LF.CR=(sum(Cond=='TF' & Resp==2))/((sum(Cond=='TF'))+1)) %>% 
  dplyr::select(subject, Age, LF.hit, LF.miss, LF.FA, LF.CR)

## Calculating SDT measures

# Calculating d'[T,F]
## Difference in distributions between “old” responses to old targets and “old” responses to new foils

MST.TF.dprime <- MST.T_F %>% 
  na.omit() %>% 
  group_by(subject, Age) %>% 
  mutate(TF_dprime = (psycho::dprime(TF.hit, TF.FA, TF.miss, TF.CR))['dprime']) %>% 
  mutate(TF_dprime = as.numeric(TF_dprime)) %>% 
  dplyr::select(subject, Age, TF_dprime)

# Calculating d'[T,L]
## Difference in distributions between “old” responses to similar lures and “old” responses to new foils

MST.TL.dprime <- MST.T_L %>% 
  na.omit() %>% 
  group_by(subject, Age) %>% 
  mutate(TL_dprime = (psycho::dprime(TL.hit, TL.FA, TL.miss, TL.CR))['dprime']) %>% 
  mutate(TL_dprime = as.numeric(TL_dprime)) %>% 
  dplyr::select(subject, Age, TL_dprime)

# Calculating d'[L,F]
## Difference in distributions between “old” responses to old targets and “old” responses to similar lures

MST.LF.dprime <- MST.L_F %>% 
  na.omit() %>% 
  group_by(subject, Age) %>% 
  mutate(LF_dprime = (psycho::dprime(LF.hit, LF.FA, LF.miss, LF.CR))['dprime']) %>% 
  mutate(LF_dprime = as.numeric(LF_dprime)) %>% 
  dplyr::select(subject, Age, LF_dprime)

# Combine dataframes

MST.sdt <- merge(MST.TF.dprime, MST.TL.dprime) %>% 
  merge(MST.LF.dprime)

# d' to Ad'

library(psych)

dprime_to_Ad <- function(d.prime.value) {
  Ad <- pnorm((d.prime.value)/sqrt(2))
  return(Ad)
}

MST.sdt <- MST.sdt %>%
  mutate(TF.Ad=dprime_to_Ad(TF_dprime),
         TL.Ad=dprime_to_Ad(TL_dprime),
         LF.Ad=dprime_to_Ad(LF_dprime)
  )

## Excluding participants who are unable to discriminate between old targets and new foils 
MST.sdt<-MST.sdt[!(MST.sdt$TF.Ad<=0.5),]

# Ad summary
MST.Ad_summary <- MST.sdt %>% 
  group_by(Age) %>% 
  summarise(mean.TF.Ad=mean(TF.Ad),
            mean.TL.Ad=mean(TL.Ad),
            mean.LF.Ad=mean(LF.Ad),
            sd.TF.Ad=sd(TF.Ad),
            sd.TL.Ad=sd(TL.Ad),
            sd.LF.Ad=sd(LF.Ad))

## MST ANOVA

library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)

# Make long
MST.sdt.long <- MST.sdt %>% 
  gather(Type, Ad, TL.Ad:LF.Ad, factor_key=TRUE) %>% 
  dplyr::select(Age, Type, Ad)

# Variables as factors
MST.sdt.long$Age.f <- as.factor(MST.sdt.long$Age)
MST.sdt.long$Type.f <- as.factor(MST.sdt.long$Type)

# Run ANOVA
MST.anova <- aov(Ad ~ Age.f*Type.f, data = MST.sdt.long)
summary(MST.anova)

# Simple Effects
library(emmeans)
MST.Simple.Effects.By.Type<-emmeans(MST.anova, ~Age.f|Type.f)
pairs(MST.Simple.Effects.By.Type,adjust='holm') %>% summary(infer = TRUE)

########################### SAT ##############################
setwd(paste(OA_Dir,"SAT/", sep=""))

OA.SAT <- do.call("rbind", lapply(
  list.files(pattern = "*_20vs80.csv", recursive = TRUE), 
  read.csv, header=TRUE))
OA.SAT$Age = 'Older'

setwd(paste(YA_Dir,"SAT/", sep=""))

YA.SAT <- do.call("rbind", lapply(
  list.files(pattern = "*_20vs80.csv", recursive = TRUE), 
  read.csv, header=TRUE))
YA.SAT$Age = 'Younger'

SAT.all <- rbind(OA.SAT, YA.SAT)

## Recode missed trials
# recode missed as NA
SAT.all[SAT.all == 9999] <- NA

# explore -- count number of missed trials
SAT.missed <- aggregate(RT ~ subject, data=SAT.all, function(x) {sum(is.na(x))}, na.action = NULL)

# recode missed as incorrect
SAT.all$accuracy <- SAT.all$correct
SAT.all$accuracy[is.na(SAT.all$accuracy)] <- 0

## Excluding participants with hit rates 3 SD below sample mean
SAT.hit.rates <- SAT.all %>%
  group_by(Age, subject) %>%
  summarise(hit.rate=mean(accuracy))

SAT.hr.summary <- SAT.hit.rates %>%
  group_by(Age) %>%
  summarise(mean.hr=mean(hit.rate), sd.hr=sd(hit.rate))

SAT.hit.rates$mean.minus.SD3 <- ifelse(SAT.hit.rates$Age == 'Older',
                                       SAT.hr.summary$mean.hr[1]-(3*(SAT.hr.summary$sd.hr[1])),
                                       SAT.hr.summary$mean.hr[2]-(3*(SAT.hr.summary$sd.hr[2])))

SAT.hit.rates$exclude <- ifelse(SAT.hit.rates$hit.rate <= SAT.hit.rates$mean.minus.SD3,
                                'EXCLUDE', NA)
## Removing linear drift
# first try plotting current data
mean.data <- SAT.all %>% 
  group_by(Age, trial) %>% 
  summarise(mean.RT=mean(RT, na.rm=TRUE))

ggplot(na.omit(mean.data), aes(x=trial, y=mean.RT, colour=Age)) +
  geom_line()


# mean-centers data within participants + corrects for variance of time 
SAT_dt = setDT(SAT.all)
# residuals(lm(RT ~ trial, data = SAT.filtered_dt))
SAT_dt[!is.na(RT), RT_residuals := residuals(lm(RT ~ trial)), by = subject]
SAT_dt[, mean(RT_residuals, na.rm=T), by = subject]

ggplot(na.omit(SAT_dt), aes(x=trial, y=RT, colour=Age)) +
  geom_line()

# summary of raw RT
RT_residuals.summary <- SAT.all %>% 
  group_by(Age) %>% 
  summarise(sd.RT=sd(RT_residuals, na.rm=TRUE))


OA_SAT <- SAT_dt %>% 
  filter(Age=='Older')

YA_SAT <- SAT_dt %>% 
  filter(Age=='Younger')

ggdensity(OA_SAT$RT_residuals, 
          main = "Density plot of OA RT",
          xlab = "RT residuals")

ggdensity(YA_SAT$RT_residuals, 
          main = "Density plot of YA RT",
          xlab = "RT residuals")

## Calculating overall tau
SAT.tau <- SAT_dt %>% 
  drop_na() %>% 
  dplyr::group_by(subject, Age) %>% 
  summarise(tau=mexgauss(RT_residuals)['tau'])

# t-test
t.test(SAT.tau$tau ~ SAT.tau$Age)

# Calculating coefficient of variation
SAT.CoV <- SAT_dt %>% 
  drop_na() %>% 
  dplyr::group_by(subject, Age) %>% 
  summarise(mean.RT = mean(RT),
            sd.RT = sd(RT)) %>% 
  mutate(CoV = sd.RT/mean.RT)

# Validation
temp <- merge(SAT.tau, SAT.CoV)
cor.test(SAT.tau$tau, SAT.CoV$CoV) #r=0.9037447, p< 2.2e-16

############# Summary ##########
# summary of SAT measures for pre-registered sample
Study1_subjects <- Study1_Demographics %>% dplyr::select(subject)
SAT_dt_prereg <- merge(SAT_dt, Study1_subjects)
SAT.CoV_prereg <- merge(SAT.CoV, Study1_subjects)
SAT.tau.prereg <- merge(SAT.tau, Study1_subjects)

SAT.summary <- SAT_dt_prereg %>% 
  group_by(Age) %>% 
  summarise(mean.RT=mean(RT, na.rm=TRUE), 
            median.RT=median(RT, na.rm=TRUE), 
            sd.RT=sd(RT, na.rm=TRUE),
            mean.acc=mean(accuracy),
            sd.acc=sd(accuracy)
  )

# Add CoV
CoV.summary <- SAT.CoV_prereg %>% 
  group_by(Age) %>% 
  summarize(mean.CoV = mean(CoV),
            sd.CoV=sd(CoV))

# Add Tau
tau.summary <- SAT.tau.prereg %>% 
  group_by(Age) %>% 
  summarize(mean.tau = mean(tau),
            sd.tau = sd(tau))

# Add Errors
errors_frequent <- SAT_dt_prereg %>%
  group_by(subject, Age) %>% 
  filter(orientation == 30) %>% 
  summarize(prop.errors_frequent = mean(accuracy == 0)) %>% 
  group_by(Age) %>% 
  summarize(
    mean.prop.errors_frequent = mean(prop.errors_frequent),
    sd.prop.errors_frequent = sd(prop.errors_frequent)
  )

errors_infrequent <- SAT_dt_prereg %>%
  group_by(subject, Age) %>% 
  filter(orientation == 330) %>% 
  summarize(prop.errors_infrequent = mean(accuracy == 0)) %>% 
  group_by(Age) %>% 
  summarize(
    mean.prop.errors_infrequent = mean(prop.errors_infrequent),
    sd.prop.errors_infrequent = sd(prop.errors_infrequent)
  )

SAT.summary.all <- merge(SAT.summary, CoV.summary) %>% 
  merge(tau.summary) %>% 
  merge(errors_frequent) %>% 
  merge(errors_infrequent)

##################################################################
################### PREREGISTERED ANALYSES #######################
##################################################################

prereg_data <- merge(PC.auto, Study1_slopes, by=c('subject', 'Age')) %>% 
  merge(SAT.tau, by=c('subject', 'Age')) %>% 
  mutate(tau.w=winsorize(tau))

# Mediation Analysis 1: PS (SA as mediator)
# Here Age - X, SAT tau - M and BPS - Y. 

model.0 =  lm(Slope.w ~ Age, prereg_data)
summary(model.0)
model.m = lm(tau.w ~ Age, prereg_data)
summary(model.m)
model.y = lm(Slope.w ~ Age + tau.w, prereg_data)
summary(model.y)

# Only run mediation analysis if age differences are significant or trending (p<0.1)

# library(mediation)
# 
# model.mediate = mediate(model.m, model.y, treat = 'Age', mediator = 'tau.w', boot=TRUE, sims = 5000)
# summary(model.mediate)

# Mediation Analysis 2: PC (SA as mediator)
# Here Age - X, SAT tau - M and automatic - Y. 

model.01 =  lm(automatic.w ~ Age, prereg_data)
summary(model.01)
model.m1 = lm(tau.w ~ Age, prereg_data)
summary(model.m1)
model.y1 = lm(automatic.w ~ Age + tau.w, prereg_data)
summary(model.y1)

# Only run mediation analysis if age differences are significant or trending (p<0.1)
model.mediate1 = mediate(model.m1, model.y1, treat = 'Age', mediator = 'tau.w', boot=TRUE, sims = 5000)
summary(model.mediate1)

##################################################################
###################### OLDER ADULTS ONLY #########################
##################################################################

# Bigger should mean better for measures going into this Factor Analysis

# Reverse code VDT Slope
Study1_slopes$Slope.w_rc <- -1*(Study1_slopes$Slope.w)

# Winsorize OA data only (without MST)
OA.PC.inc <- PC.inc %>% filter(Age=='Older')
OA.PC.inc$inclusion_w <- Winsorize(OA.PC.inc$inclusion)

OA.PC.exc <- PC.exc %>% filter(Age=='Older')
OA.PC.exc$exclusion_w <- Winsorize(OA.PC.exc$exclusion)

OA.PC.auto <- PC.auto %>% filter(Age=='Older')
OA.PC.auto$automatic_w <- Winsorize(OA.PC.auto$automatic)

OA.PC.controlled <- PC.controlled %>% filter(Age=='Older')
OA.PC.controlled$controlled_w <- Winsorize(OA.PC.controlled$controlled)

OA.SAT.tau <- SAT.tau %>% filter(Age=='Older')
OA.SAT.tau$tau_w <- Winsorize(OA.SAT.tau$tau)

OA.SAT.cov <- SAT.CoV %>% filter(Age=='Older')
OA.SAT.cov$cov_w <- Winsorize(OA.SAT.cov$CoV)

OA.Slopes <- Study1_slopes %>% filter(Age=='Older')
OA.Slopes$slope_rc_w <- (Winsorize(OA.Slopes$slope))*(-1)

OA_Data <- merge(OA.PC.inc, OA.PC.exc, by=c('subject', 'Age')) %>% 
  merge(OA.PC.auto, by=c('subject', 'Age')) %>% 
  merge(OA.PC.controlled, by=c('subject', 'Age')) %>% 
  merge(OA.SAT.tau, by=c('subject', 'Age')) %>%
  mutate(tau_w=Winsorize(tau)) %>% 
  merge(OA.SAT.cov, by=c('subject', 'Age')) %>%
  mutate(cov_w=Winsorize(CoV)) %>% 
  merge(OA.Slopes, by=c('subject', 'Age')) %>%
  filter(subject > 300) %>% 
  dplyr::select(subject, Age, inclusion_w, exclusion_w, automatic_w, controlled_w,
                CoV, tau, slope_rc_w)

## Setup
OA_Dir<-"/Volumes/home/Behavioural Data/OA Data/Day 1/"
setwd(OA_Dir)
neuropsych <- read.csv("neuropsych.csv")

## Reverse coding so bigger means better

# Trails 
TMT_model <- lm(neuropsych$TrailB_time ~ neuropsych$TrailA_time)
TMT_res <- resid(TMT_model)
neuropsych$TMT_res <- -1*(TMT_res)

# Stroop 
#Stroop_avg <- (neuropsych$StroopWords_score + neuropsych$StroopColor_score)/2
Stroop_model <- lm(neuropsych$StroopColor.Words_score ~ neuropsych$StroopWords_score + neuropsych$StroopColor_score)
Stroop_res <- resid(Stroop_model)
neuropsych$Stroop_res <- Stroop_res

# Haylings
neuropsych$Hayling2_Ascore <- (neuropsych$Hayling2_Ascore)*(-1)
neuropsych$Hayling2_Bscore <- (neuropsych$Hayling2_Bscore)*(-1)
neuropsych$Hayling2_ConvertedScore <- (neuropsych$Hayling2_ConvertedScore)*(-1)
Haylings_model <- lm(neuropsych$Hayling2_TotalTime ~ neuropsych$Hayling1_TotalTime)
Haylings_res <- resid(Haylings_model)
neuropsych$Haylings_res <- -1*(Haylings_res)

# Fluency errors
neuropsych$F_R <- (neuropsych$F_R)*(-1)
neuropsych$F_SL <- (neuropsych$F_SL)*(-1)
neuropsych$A_R <- (neuropsych$A_R)*(-1)
neuropsych$A_SL <- (neuropsych$A_SL)*(-1)
neuropsych$S_R <- (neuropsych$S_R)*(-1)
neuropsych$S_SL <- (neuropsych$S_SL)*(-1)
neuropsych$FAS_Errors <- (neuropsych$FAS_Errors)*(-1)
neuropsych$Animals_R <- (neuropsych$Animals_R)*(-1)
neuropsych$Animals_SL <- (neuropsych$Animals_SL)*(-1)
neuropsych$Animals_Errors <- (neuropsych$Animals_Errors)*(-1)

all.submeasures <- neuropsych %>% dplyr::select(
  subject=Sub_ID, 
  DoorA_subtotal, DoorB_subtotal, Door_total,
  NameA_subtotal, NameB_subtotal, Name_total,
  VPA1_A, VPA1_B, VPA1_C, VPA1_D, VPA1_total,
  VPAII_recall_score, VPAII_recognition_score,
  Digits_forward, Digits_backward, Digits_total,
  TrailA_time, TrailA_errors, TrailB_time, TrailB_errors, TMT_res,
  Hayling1_TotalTime, Hayling2_TotalTime, Hayling2_ConvertedScore, Hayling2_ErrorsScaledScore, Haylings_res,
  StroopColor_score, StroopWords_score, StroopColor.Words_score,Stroop_res,
  F_Total, A_Total, S_TOTAL, FAS_TotalCorr, FAS_Errors,
  Animals_TOTAL, Animals_Errors,
  BNT_Spontaneous, BNT_SponSemantic, BNT_SponSemPhonemic
)

###### Prepare Data ########

#### Add in Shipley
Shipley <- read.csv("/Volumes/home/Behavioural Data/Shipley.csv")
# Find duplicates in the 'subject' column
duplicate_subjects <- Shipley[duplicated(Shipley$subject) | duplicated(Shipley$subject, fromLast = TRUE), ]

measures <- merge(all.submeasures, (Shipley %>% dplyr::select(subject, Shipley)), by='subject')

# Winsorize 
library(DescTools)
measures.w <- measures %>%
  dplyr::select(-subject) %>% 
  mutate(across(everything(), ~ Winsorize(., na.rm = TRUE))) %>% 
  rename_with(~ paste0(., "_w"))

winsorized <- cbind(measures, measures.w)

# Combine neuropsych and experimental data
All_data <- merge(OA_Data, winsorized, by='subject')

# Scale variables 
All_OA_Data.sc <- All_data %>%
  dplyr::select(-subject, -Age) %>% 
  mutate(across(everything(), ~ scale(.))) %>% 
  rename_with(~ paste0(., "_sc"))

# Filter data
Data_f <- cbind(All_data, All_OA_Data.sc) %>% 
  filter(subject < 383)

##################################################################
############## SUPPLEMENTARY FACTOR ANALYSES #####################
##################################################################

#### Define Models #####

# LANGUAGE MODEL
lang <- Data_f %>% 
  dplyr::select(BNT_SponSemantic_w_sc, Shipley_w_sc, Animals_TOTAL_w_sc)

# EF MODEL (Processing and Control)
ef <- Data_f %>% 
  dplyr::select(Haylings_res_w_sc, Digits_total_w_sc,
                FAS_TotalCorr_w_sc, TMT_res_w_sc, Stroop_res_w_sc)

### Factor Analysis ###

# Number of factors to extract
library(ggplot2)
# Define data
data<-lang

fafitfree <- fa(data, nfactors = 1, rotate = "none")
n_factors <- length(fafitfree$e.values)
scree     <- data.frame(
  Factor_n =  as.factor(1:n_factors), 
  Eigenvalue = fafitfree$e.values)
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() +
  xlab("Number of factors") +
  ylab("Initial eigenvalue") +
  labs( title = "Scree Plot", 
        subtitle = "(Based on the unreduced correlation matrix)")

parallel <- fa.parallel(data)

# Language 
fa.none <- fa(r=lang, 
              nfactors = 1, 
              # covar = FALSE, SMC = TRUE,
              fm='pa', # type of factor analysis we want to use (“pa” is principal axis factoring)
              max.iter=100, # (50 is the default, but we have changed it to 100
              rotate='oblimin') # none rotation
print(fa.none)

fa.diagram(fa.none)

lang_fs <- factor.scores(lang, fa.none)
lang_fs <- lang_fs$scores
Data_f <- cbind(Data_f, lang_fs)
names(Data_f)[names(Data_f) == 'PA1'] <- 'language'

# EF 
fa.none <- fa(r=ef, 
              nfactors = 1, 
              # covar = FALSE, SMC = TRUE,
              fm='pa', # type of factor analysis we want to use (“pa” is principal axis factoring)
              max.iter=100, # (50 is the default, but we have changed it to 100
              rotate='oblimin') # none rotation
print(fa.none)

fa.diagram(fa.none)

ef_fs <- factor.scores(ef, fa.none)
ef_fs <- ef_fs$scores
Data_f <- cbind(Data_f, ef_fs)
names(Data_f)[names(Data_f) == 'PA1'] <- 'executive'

# Transform skewed variables

# Transform tau
library(MASS)
bc <- boxcox(Data_f$tau ~ 1, lambda = seq(-5, 5, 0.1))
best_lambda <- bc$x[which.max(bc$y)]
Data_f$tau_boxcox_transformed <- (Data_f$tau^best_lambda - 1) / best_lambda
Data_f$tau_boxcox_transformed_w_sc <- scale(Winsorize(Data_f$tau_boxcox_transformed))
hist(Data_f$tau_boxcox_transformed_w_sc)

####### Interaction Model

# Set up Interaction Model Data - Composites and PC Automatic
interaction.m <- Data_f %>% 
  dplyr::select(subject, tau_boxcox_transformed_w_sc, executive, language, automatic_w_sc, slope_rc_w_sc) %>% 
  gather(testtype, score, automatic_w_sc:slope_rc_w_sc, factor_key=TRUE)

# Effect code Test Type
interaction.m$testtype.ec <- ifelse(interaction.m$testtype=='automatic_w_sc', -1, 1)

int.m <- lmer(score ~ testtype.ec*tau_boxcox_transformed_w_sc + testtype.ec*executive + testtype.ec*language + 
                (1|subject), data=interaction.m)
summary(int.m)

# Simple Effects
simple_slopes(int.m)

# Visualize
library(ggeffects)
ggpredict(int.m, terms=c('tau_boxcox_transformed_w_sc', 'testtype.ec'))
mydf <- ggpredict(int.m, se=TRUE)
mydf <- ggpredict(int.m, terms = c('tau_boxcox_transformed_w_sc', 'testtype.ec'))

# Invert tau on graph
mydf$tau.inv <- 1-mydf$x

ggplot(mydf, aes(x = tau.inv, y = predicted, colour = group)) +
  geom_line(size=4) +
  geom_ribbon( aes(ymin = conf.low, ymax = conf.high, fill = group, color = NULL), 
               alpha = .25) +
  scale_color_manual(values=c('mistyrose4','grey'))+
  scale_fill_manual(values=c('mistyrose2','grey')) +
  labs(
    x= "1-Tau"
  ) +
  theme_classic() +
  theme(legend.text = element_text(margin = margin(r = 10, unit = "pt"), size=20),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20),
        plot.title = element_text(hjust = 0.5, size=20))

##################################################################
################## SUPPLEMENTARY  ANALYSES #######################
##################################################################

############ MST ################

# MST Demographics
MST.Demographics <- data.frame(unique(MST$subject)) %>% 
  rename(subject=unique.MST.subject.) %>% 
  left_join(Demographics, by='subject') %>% 
  na.omit()

round_2 <- function(x) {
  y <- format(round(x, 2), nsmall = 2)
  return(y)
}

MST.Demographics_summary <- MST.Demographics %>% 
  summarise(Females=length(which(Sex=='F')), Males=length(which(Sex=='M')),
            Age.min=(range(Age, na.rm=TRUE))[1], Age.max=(range(Age, na.rm=TRUE))[2], 
            Age.mean=round_2(mean(Age, na.rm=TRUE)), Age.sd=round_2(sd(Age, na.rm=TRUE)),
            Shipley.mean=round_2(mean(Shipley, na.rm=TRUE)), Shipley.sd=round_2(sd(Shipley, na.rm=TRUE)))

# Add MST data
Data_with_MST <- merge(Data_f, MST.sdt %>% dplyr::select(subject, LF.Ad)) %>% 
  merge(MST.Demographics, by='subject') %>% 
  mutate(LF.Ad.sc = scale(winsorize(LF.Ad)))

hist(Data_with_MST$LF.Ad.sc)

# Set up Interaction Model Data
MST.interaction.m <- Data_with_MST %>% 
  dplyr::select(subject, tau_boxcox_transformed, executive, language, automatic_w_sc, LF.Ad.sc) %>% 
  gather(testtype, score, automatic_w_sc:LF.Ad.sc, factor_key=TRUE)

# Effect code Test Type
MST.interaction.m$testtype.ec <- ifelse(MST.interaction.m$testtype=='automatic_w_sc', -1, 1)

MST.int.m <- lmer(score ~ testtype.ec*tau_boxcox_transformed + testtype.ec*executive + testtype.ec*language + 
                    (1|subject), data=MST.interaction.m)
summary(MST.int.m)

# Simple Effects
emm_results <- emmeans(MST.int.m, ~ testtype.ec | tau_boxcox_transformed)
summary(emm_results)
simple_effects_test <- test(emm_results, simple = "each")
summary(simple_effects_test)

library(ggeffects)
ggpredict(MST.int.m, terms=c('tau_boxcox_transformed', 'testtype.ec'))
mydf <- ggpredict(MST.int.m, se=TRUE)
mydf <- ggpredict(MST.int.m, terms = c('tau_boxcox_transformed', 'testtype.ec'))

# Invert tau on graph
mydf$tau.inv <- 1-mydf$x

ggplot(mydf, aes(x = tau.inv, y = predicted, colour = group)) +
  geom_line(size=4) +
  geom_ribbon( aes(ymin = conf.low, ymax = conf.high, fill = group, color = NULL), 
               alpha = .25) +
  scale_color_manual(values=c('mistyrose4','grey'))+
  scale_fill_manual(values=c('mistyrose2','grey')) +
  labs(
    x= "1-Tau"
  ) +
  theme_classic() +
  theme(legend.text = element_text(margin = margin(r = 10, unit = "pt"), size=20),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20),
        plot.title = element_text(hjust = 0.5, size=20))


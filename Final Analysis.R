

##########Packages#############

library(mfx)
library(texreg)
library(modmarg)
library(prediction)
library(margins)
library(ggplot2)
library(readxl)
library(plyr)
library(stargazer)
library(stats)
library(lfe)
library(Matrix)
library(xlsx)
library(foreign)
library(lmtest)
library(sandwich)
library(zoo)
library(grid)


#########Uploading Data##########

#Set Working Directory 
setwd("/Users/matthewobrien/Google Drive/UBC Stuff/594 - Final project/Regressions")


#First we need a function that will merge all the raw data into one dataframe. The funtion
#mergefunc will do just that. 

mergefunc = function(mypath){
    filenames=list.files(path=mypath, full.names=TRUE)
    datalist = lapply(filenames, function(x){read_excel(path=x)})
    Reduce(function(x,y) {merge(x,y, all = T)}, datalist)
}

#Takes all files in the "Raw Data" folder and merges them into one dataframe
Master <- mergefunc("/Users/matthewobrien/Google Drive/UBC Stuff/594 - Final project/Raw Data")


#################Cleaning Data###############

#Only want these columns
MasterClean <- Master[,c(14,26:29,33:37,41:47,56)] 

#Get rid of anything with an NA
MasterClean <- na.omit(MasterClean)

#Create Some new Variables to use later
MasterClean$C1 <- ifelse(MasterClean$EgalEXP_1_player_choice == "Choice 1 (50,50)", 1, 0)
MasterClean$C2 <- ifelse(MasterClean$C1 == 0, 1, 0)
MasterClean$male <- ifelse(MasterClean$survey_1_player_gender == "Male", 1, 0)

#Dowload the new dataset to put onn github
write.csv(MasterClean, file = "/Users/matthewobrien/Google Drive/UBC Stuff/594 - Final project/MasterClean.csv", row.names = FALSE)


##########################################################################################
########For replication start here with MasterClean Dataset###############################
##########################################################################################

#Now I want to make a summry table for each game (with mean, sd) for each treatment

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # For each group return a vector with N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}



##################################################################################
####################Analysis 1 - Looks at Altruism and Envy#######################
##################################################################################


#############################Group Mean/se Graphs##################################

#making as character for graphing purposes
MasterClean$dictator_1_player_initial <- as.character(MasterClean$dictator_1_player_initial) 
MasterClean$dictator_1_player_partner <- as.character(MasterClean$dictator_1_player_partner)
MasterClean$EgalEXP_1_player_initial <- as.character(MasterClean$EgalEXP_1_player_initial) 
MasterClean$EgalEXP_1_player_partner <- as.character(MasterClean$EgalEXP_1_player_partner)

#Also want to rename all endowments that are 0 as the 'control group'
MasterClean$dictator_1_player_initial[MasterClean$dictator_1_player_initial == "0"] <- "Control"
MasterClean$dictator_1_player_partner[MasterClean$dictator_1_player_partner == "0"] <- "Control"
MasterClean$EgalEXP_1_player_initial[MasterClean$EgalEXP_1_player_initial == "0"] <- "Control"
MasterClean$EgalEXP_1_player_partner[MasterClean$EgalEXP_1_player_partner == "0"] <- "Control"


########Average Income Split Kept By Own Initial Treatment Game 1############### 


SumTable1 <- summarySE(MasterClean,
                      measurevar="dictator_1_player_kept", 
                      groupvars=c("dictator_1_player_initial"))
SumTable1


ggplot(SumTable1, aes(x = dictator_1_player_initial, y= dictator_1_player_kept, fill = dictator_1_player_initial)) + 
  geom_bar( stat = "identity", alpha = 0.8) +
  geom_text(aes(label=N), y=50) +
  geom_text(aes(label=round(dictator_1_player_kept, 2)), position = position_nudge(y=SumTable1$se + 0.8), size=3.5) +
  geom_errorbar(aes(ymin=dictator_1_player_kept - se, ymax=dictator_1_player_kept + se),
                width = 0.5, position=position_dodge(.7)) +
  labs(x = "Dictator's Endowment", y = "Average Income Split Kept", fill = "Dictator's Endowment") +
  coord_cartesian(ylim=c(50,70)) +
  ggtitle("Average Income Split Kept by Own Endowment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
  theme(legend.position="bottom")



########Average Income Split Kept By Partner Initial Treatment Game 1############### 

SumTable2 <- summarySE(MasterClean,
                      measurevar="dictator_1_player_kept", 
                      groupvars=c("dictator_1_player_partner"))
SumTable2

ggplot(SumTable2, aes(x = dictator_1_player_partner, y= dictator_1_player_kept, fill = dictator_1_player_partner)) + 
  geom_bar( stat = "identity", alpha = 0.8) +
  geom_text(aes(label=N), y=50) +
  geom_text(aes(label=round(dictator_1_player_kept, 2)), position = position_nudge(y=SumTable2$se + 0.8), size=3.5) +
  geom_errorbar(aes(ymin=dictator_1_player_kept - se, ymax=dictator_1_player_kept + se),
                width = 0.5, position=position_dodge(.7)) +
  labs(x = "Subject's Endowment", y = "Average Income Split", fill = "Subject's Endowment") +
  coord_cartesian(ylim=c(50,70)) +
  ggtitle(" Average Income Split Kept by Partner's Endowment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
  theme(legend.position="bottom")



########Average Income Split Kept By Own and Partner Initial Treatment Game 1############### 

SumTable3 <- summarySE(MasterClean,
                      measurevar="dictator_1_player_kept", 
                      groupvars=c("dictator_1_player_partner","dictator_1_player_initial"))
SumTable3

ggplot(SumTable3, aes(x = dictator_1_player_initial, y= dictator_1_player_kept, fill = dictator_1_player_partner)) + 
  geom_bar(position="dodge", stat = "identity", alpha = 0.8) +
  geom_text(aes(label=N, y=45), position=position_dodge(width=0.9), size=3.5) +
  geom_text(aes(label=round(dictator_1_player_kept, 1), y = SumTable3$dictator_1_player_kept + SumTable3$se + 1), position=position_dodge(width=0.9), size=3) +
  geom_errorbar(aes(ymin=dictator_1_player_kept - se, ymax=dictator_1_player_kept + se),
                width = 0.5, position=position_dodge(0.9)) +
  labs(x = "Dictator's Endownment", y = "Average Income Split", fill = "Subject's Endowment") +
  coord_cartesian(ylim=c(45,75)) +
  ggtitle("Average Income Split Kept by
Own and Partner's Endowment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()  +
  theme(legend.position="bottom")


########Percentage of Choice 1 Decisions by Own Initial Endowment Game 2############### 

SumTable4 <- summarySE(MasterClean,
                      measurevar="C1", 
                      groupvars=c("EgalEXP_1_player_initial"))
SumTable4

ggplot(SumTable4, aes(x = EgalEXP_1_player_initial, y= C1, fill = EgalEXP_1_player_initial)) + 
  geom_bar( stat = "identity", alpha = 0.8) +
  geom_text(aes(label=N), y=0.15, size=3.5) +
  geom_text(aes(label=round(C1, 2)), position = position_nudge(y=SumTable4$se + 0.01), size=3.5) +
  geom_errorbar(aes(ymin=C1 - se, ymax= C1 + se),
                width = 0.5, position=position_dodge(.7)) +
  labs(x = "Dictator's Endowment", y = "Percentage Choosing 50/50 Split", fill = "Dictator's Endowment") +
  coord_cartesian(ylim=c(0.15,0.45)) +
  ggtitle("Portion Choosing 50/50 Split by Own Endowment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
  theme(legend.position="bottom")



########Percentage of Choice 1 Decisions by Partner's Endowment Game 2############### 

SumTable5 <- summarySE(MasterClean,
                      measurevar="C1", 
                      groupvars=c("EgalEXP_1_player_partner"))
SumTable5

ggplot(SumTable5, aes(x = EgalEXP_1_player_partner, y= C1, fill = EgalEXP_1_player_partner)) + 
  geom_bar( stat = "identity", alpha = 0.8) +
  geom_text(aes(label=N), y = 0.15, size=3.5) +
  geom_text(aes(label=round(C1, 2)), position = position_nudge(y=SumTable5$se + 0.015), size=3.5) +
  geom_errorbar(aes(ymin=C1 - se, ymax=C1 + se),
                width = 0.5, position=position_dodge(.7)) +
  labs(x = "Subject's Endowment", y = "Percentage Choosing 50/50 Split", fill = "Subject's Endowment") +
  coord_cartesian(ylim=c(0.15,0.5)) +
  ggtitle("Portion Choosing 50/50 Split by Partner's Endownment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
  theme(legend.position="bottom")


########Percentage of Choice 1 Decisions by Own and Partner's Endowment Game 2############### 

SumTable6 <- summarySE(MasterClean,
                      measurevar="C1", 
                      groupvars=c("EgalEXP_1_player_partner","EgalEXP_1_player_initial"))
SumTable6

ggplot(SumTable6, aes(x = EgalEXP_1_player_initial, y= C1, fill = EgalEXP_1_player_partner)) + 
  geom_bar(position="dodge", stat = "identity", alpha = 0.8) +
  geom_text(aes(label=N, y=0.15), position=position_dodge(width=0.9), size=3.5) +
  geom_text(aes(label=round(C1, 2), y = SumTable6$C1 + SumTable6$se + 0.015), position=position_dodge(width=0.9), size=3) +
  geom_errorbar(aes(ymin=C1 - se, ymax=C1 + se),
                width = 0.5, position=position_dodge(.9)) +
  labs(x = "Dictator's Endownment", y = "Portion Choosing 50/50 Split", fill = "Subject's Endowment") +
  coord_cartesian(ylim=c(0.15,0.6)) +
  ggtitle("Percentage Choosing 50/50 Split by Own and Partner's Endownment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
  theme(legend.position="bottom")


#####################Regressions for Randomization#######################

#######Make Treatment Dummies Game 1######

for (i in 1:10) {
  
MasterClean$T0G1 <- ifelse(MasterClean$dictator_1_player_initial == "0" & 
                             MasterClean$dictator_1_player_partner == "0", 1, 0)

MasterClean$T1G1 <- ifelse(MasterClean$dictator_1_player_initial == "10" & 
                           MasterClean$dictator_1_player_partner == "10", 1, 0)

MasterClean$T2G1 <- ifelse(MasterClean$dictator_1_player_initial == "10" & 
                             MasterClean$dictator_1_player_partner == "100", 1, 0)

MasterClean$T3G1 <- ifelse(MasterClean$dictator_1_player_initial == "10" & 
                             MasterClean$dictator_1_player_partner == "300", 1, 0)

MasterClean$T4G1 <- ifelse(MasterClean$dictator_1_player_initial == "100" & 
                             MasterClean$dictator_1_player_partner == "10", 1, 0)

MasterClean$T5G1 <- ifelse(MasterClean$dictator_1_player_initial == "100" & 
                             MasterClean$dictator_1_player_partner == "100", 1, 0)

MasterClean$T6G1 <- ifelse(MasterClean$dictator_1_player_initial == "100" & 
                             MasterClean$dictator_1_player_partner == "300", 1, 0)

MasterClean$T7G1 <- ifelse(MasterClean$dictator_1_player_initial == "300" & 
                             MasterClean$dictator_1_player_partner == "10", 1, 0)

MasterClean$T8G1 <- ifelse(MasterClean$dictator_1_player_initial == "300" & 
                             MasterClean$dictator_1_player_partner == "100", 1, 0)

MasterClean$T9G1 <- ifelse(MasterClean$dictator_1_player_initial == "300" & 
                             MasterClean$dictator_1_player_partner == "300", 1, 0)
}

###Make Treatment Dummies Game 2##########

for (i in 1:10) {
  
  MasterClean$T0G2 <- ifelse(MasterClean$EgalEXP_1_player_initial == "0" & 
                               MasterClean$EgalEXP_1_player_partner == "0", 1, 0)
  
  MasterClean$T1G2 <- ifelse(MasterClean$EgalEXP_1_player_initial == "10" & 
                               MasterClean$EgalEXP_1_player_partner == "10", 1, 0)
  
  MasterClean$T2G2 <- ifelse(MasterClean$EgalEXP_1_player_initial == "10" & 
                               MasterClean$EgalEXP_1_player_partner == "100", 1, 0)
  
  MasterClean$T3G2 <- ifelse(MasterClean$EgalEXP_1_player_initial == "10" & 
                               MasterClean$EgalEXP_1_player_partner == "300", 1, 0)
  
  MasterClean$T4G2 <- ifelse(MasterClean$EgalEXP_1_player_initial == "100" & 
                               MasterClean$EgalEXP_1_player_partner == "10", 1, 0)
  
  MasterClean$T5G2 <- ifelse(MasterClean$EgalEXP_1_player_initial == "100" & 
                               MasterClean$EgalEXP_1_player_partner == "100", 1, 0)
  
  MasterClean$T6G2 <- ifelse(MasterClean$EgalEXP_1_player_initial == "100" & 
                               MasterClean$EgalEXP_1_player_partner == "300", 1, 0)
  
  MasterClean$T7G2 <- ifelse(MasterClean$EgalEXP_1_player_initial == "300" & 
                               MasterClean$EgalEXP_1_player_partner == "10", 1, 0)
  
  MasterClean$T8G2 <- ifelse(MasterClean$EgalEXP_1_player_initial == "300" & 
                               MasterClean$EgalEXP_1_player_partner == "100", 1, 0)
  
  MasterClean$T9G2 <- ifelse(MasterClean$EgalEXP_1_player_initial == "300" & 
                               MasterClean$EgalEXP_1_player_partner == "300", 1, 0)
}
##########Regress group characteristics on treatments to check randomization#########

##Initialize list of dependent variables

randomize <- c("T0G1", "T1G1", "T2G1", "T3G1", "T4G1",
            "T5G1", "T6G1", "T7G1", "T8G1", 
            "T9G1")

randomize2 <- c("T0G2", "T1G2", "T2G2", "T3G2", "T4G2",
                "T5G2", "T6G2", "T7G2", "T8G2", 
                "T9G2")

#Iterate through all dependent variables and store each regression as a new object

for (i in 1:length(randomize)){
  model <- paste("T",i-1, sep="")
  m <- lm(as.formula(paste(randomize[i],"~ survey_1_player_age + survey_1_player_income + male + factor(survey_1_player_ethnicity) + factor(survey_1_player_ethnicity) + factor(survey_1_player_maritalstatus) + factor(survey_1_player_education) + factor(survey_1_player_employment)")),
          data=MasterClean)
  assign(model,m)
}

for (i in 1:length(randomize2)){
  model <- paste("ET",i-1, sep="")
  m <- lm(as.formula(paste(randomize2[i],"~ survey_1_player_age + survey_1_player_income + male + factor(survey_1_player_ethnicity) + factor(survey_1_player_ethnicity) + factor(survey_1_player_maritalstatus) + factor(survey_1_player_education) + factor(survey_1_player_employment)")),
          data=MasterClean)
  assign(model,m)
}

###Output the Regression Table for Randomization Game 1##

stargazer(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9, type="html", keep = c("survey_1_player_age", "survey_1_player_income", "male"),
          column.labels =c("Control","Treatment 1","Treatment 2","Treatment 3","Treatment 4","Treatment 5","Treatment 6","Treatment 7","Treatment 8","Treatment 9"),
          title = "Randomization In The Sharing Game",
          covariate.labels= c("Age", "Income", "Male"), 
          dep.var.labels = "",
          dep.var.caption = "Dependent Variable: Treatment Status",
          column.sep.width = "-10pt",
          add.lines=list(c("Education Dummies", "NS"), c("Relationship Dummies", "NS"),
          c("Ethnicity Dummies", "NS"), c("Employment Status Dummies", "NS")),
          out="models.htm")

###Output the Regression Table for Randomization Game 2##

stargazer(ET0,ET1,ET2,ET3,ET4,ET5,ET6,ET7,ET8,ET9, type="latex", keep = c("survey_1_player_age", "survey_1_player_income", "male"),
          column.labels =c("Control","Treatment 1","Treatment 2","Treatment 3","Treatment 4","Treatment 5","Treatment 6","Treatment 7","Treatment 8","Treatment 9"),
          title = "Randomization In The Envy Game",
          covariate.labels= c("Age", "Income", "Male"), 
          dep.var.labels = "",
          dep.var.caption = "Dependent Variable: Treatment Status",
          column.sep.width = "-10pt",
          add.lines=list(c("Education Dummies", "NS"), c("Relationship Dummies", "NS"),
                         c("Ethnicity Dummies", "NS"), c("Employment Status Dummies", "NS")),
          out="models2.doc")




################Regressions for Game 1: dep = Amount kept (dictator_player_1_kept),
################ ind = treatments (T1G1, T2G2, ect..) ############################

###Treatment Dummmies for own endowment Game 1###

for (i in 1:length(MasterClean$dictator_1_player_initial)){
  MasterClean$DI0 <- as.numeric(MasterClean$dictator_1_player_initial == "Control")
  MasterClean$DI1 <- as.numeric(MasterClean$dictator_1_player_initial == 10)
  MasterClean$DI2 <- as.numeric(MasterClean$dictator_1_player_initial == 100)
  MasterClean$DI3 <- as.numeric(MasterClean$dictator_1_player_initial == 300)
  }

###Treatment Dummmies for partner's endowment Game 1###

for (i in 1:length(MasterClean$dictator_1_player_partner)){
  MasterClean$DP0 <- as.numeric(MasterClean$dictator_1_player_partner == "Control")
  MasterClean$DP1 <- as.numeric(MasterClean$dictator_1_player_partner == 10)
  MasterClean$DP2 <- as.numeric(MasterClean$dictator_1_player_partner == 100)
  MasterClean$DP3 <- as.numeric(MasterClean$dictator_1_player_partner == 300)
}


###Creat variable for amount allocated to subject

MasterClean$subject <- 100 - MasterClean$dictator_1_player_kept

####Now Run regressions For Game 1######

#Partner's endowment effect conditional on Own Endowment 

RDI1 <- lm(subject ~ DP1 + DP2 + DP3, 
           data =subset(MasterClean, DI1==1 | DI0==1))
cov1 <- vcovHC(RDI1, type = "HC1") #for robust standard errors
robust_se1 <- sqrt(diag(cov1))


RDI2 <- lm(subject ~ DP1 + DP2 + DP3, 
           data =subset(MasterClean, DI2==1 | DI0==1))
cov2 <- vcovHC(RDI2, type = "HC1") #for robust standard errors
robust_se2 <- sqrt(diag(cov2))


RDI3 <- lm(subject ~ DP1 + DP2 + DP3, 
           data =subset(MasterClean, DI3==1 | DI0==1))
cov3 <- vcovHC(RDI3, type = "HC1") #for robust standard errors
robust_se3 <- sqrt(diag(cov3))


##Main Reagressions for Game 1 which interacts Partner's and Own Endowments##

RMG1 <- lm(subject ~ DI1 + DI2 + DI3 + DP1 + DP3
           + survey_1_player_age + male, data =MasterClean )
covRM1 <- vcovHC(RMG1, type = "HC1") #for robust standard errors
robust_seRM1 <- sqrt(diag(covRM1))
robust_pRM1 <- summary(RMG1)$coefficients[,4] 


RG1 <- lm(subject ~ DI1 + DI2 + DI3 + DP1 + DP3 
          + T1G1 +  T3G1 + T7G1 + T9G1 + survey_1_player_age + male, data =MasterClean )
cov4 <- vcovHC(RG1, type = "HC1") #for robust standard errors
robust_se4 <- sqrt(diag(cov4))
robust_p4 <- summary(RG1)$coefficients[,4] 


RGG1 <- lm(subject ~ DI1 + DI2 + DI3 + DP1 + DP3 
            + T5G1 + T6G1 + T8G1 + survey_1_player_age + male, data =MasterClean )
covGG <- vcovHC(RGG1, type = "HC1") #for robust standard errors
robust_seGG <- sqrt(diag(covGG))
robust_pGG <- summary(RGG1)$coefficients[,4] 


RT1 <- lm(subject ~ T1G1 + T2G1 + T3G1 + T4G1 + T5G1
          + T6G1 + T7G1 + T8G1 + T9G1 + survey_1_player_age + male, data =MasterClean )
cov5 <- vcovHC(RT1, type = "HC1") #for robust standard errors
robust_se5 <- sqrt(diag(cov5))
robust_p5 <- summary(RT1)$coefficients[,4] 


###Output the regression results - can either use stargazer or texreg##

texreg(list(RMG1, RG1, RGG1, RT1), caption = "Treatment Effects On Egalitarian Decisions Game 2",
       model.names=c("All", "All", "All", "All", "All"),
       custom.coef.map = list("DI1" = "LDict","DI2" = "MDict","DI3" = "HDict", 
                              "DP1" = "LSubj", "DP3" = "HSubj","T1G1" = "LDict*LSubj", 
                              "T2G1" = "LDict*MSubj", "T3G1" = "LDict*HSubj",
                              "T4G1" = "MDict*LSubj", "T5G1" = "MDict*MSubj",
                              "T6G1" = "MDict*HSubj","T7G1" = "HDict*LSubj", 
                              "T8G1" = "HDict*MSubj","T9G1" = "HDict*HSubj"),
       override.se = list(robust_seRM1, robust_se4, robust_seGG, robust_se5),
       override.pvalues = list(robust_pRM1, robust_p4, robust_pGG, robust_p5),
       digits = 3,
       caption.above = TRUE)


stargazer(RMG1,RG1,RT1, type="html", 
          keep = c("Constant", "DI1", "DI2", "DI3", "DP1", "DP3", "T1G1", "T2G1", "T3G1", "T4G1", "T5G1", "T6G1", "T7G1", "T8G1", "T9G1"),
          column.labels = c("All", "All", "All"),
          dep.var.labels = "",
          dep.var.caption = "Dependent Variable: Income Split Kept In Sharing Game",
          covariate.labels= c("LDict", "MDict", "HDict", "LSubj", "HSubj", 
                              "LDict*LSubj", "LDict*MSubj", "LDict*HSubj",
                              "MDict*LSubj", "MDict*MSubj","MDict*HSubj",
                              "HDict*LSubj", "HDict*MSubj","HDict*HSubj"),
          se = list(robust_seRM1, robust_se4, robust_se5), #If I want robust
          keep.stat = c("f", "rsq", "n"),
          out="Game1.htm")

################Regressions for Game 2: dep = Choice Decision (50/50 = Envious = 1),
################ ind = treatments (T1G1, T2G2, ect..) ############################

###Just so we are clear what the dependent variable is
MasterClean$Envious <- MasterClean$C1

###Treatment Dummmies for own endowment Game 2###

for (i in 1:length(MasterClean$EgalEXP_1_player_initial)){
  MasterClean$EI0 <- as.numeric(MasterClean$EgalEXP_1_player_initial == 0)
  MasterClean$EI1 <- as.numeric(MasterClean$EgalEXP_1_player_initia == 10)
  MasterClean$EI2 <- as.numeric(MasterClean$EgalEXP_1_player_initial == 100)
  MasterClean$EI3 <- as.numeric(MasterClean$EgalEXP_1_player_initial == 300)
}

###Treatment Dummmies for partner's endowment Game 2###

for (i in 1:length(MasterClean$EgalEXP_1_player_partner)){
  MasterClean$EP0 <- as.numeric(MasterClean$EgalEXP_1_player_partner == 0)
  MasterClean$EP1 <- as.numeric(MasterClean$EgalEXP_1_player_partner == 10)
  MasterClean$EP2 <- as.numeric(MasterClean$EgalEXP_1_player_partner == 100)
  MasterClean$EP3 <- as.numeric(MasterClean$EgalEXP_1_player_partner == 300)
}


###########Now Run Regressions for Game 2##############

##Logit with Marginal Effects##

REI1 <- logitmfx(Envious ~  EP1 + EP2 + EP3 , 
                 data =subset(MasterClean, EI1==1 | EI0==1), robust = TRUE)

REI2 <- logitmfx(Envious  ~  EP1 + EP2 + EP3, 
                 data =subset(MasterClean, EI2==1 | EI0==1), robust = TRUE)

REI3 <- logitmfx(Envious ~  EP1 + EP2 + EP3, 
                 data =subset(MasterClean, EI3==1 | EI0==1), robust = TRUE)


##Main Reagressions for Game 2 which interacts Partner's and Own Endowments##

RMG2 <- logitmfx(Envious ~ EI1 + EI2 + EI3 + EP1 + EP3
                +survey_1_player_age + survey_1_player_income, 
                data =MasterClean, robust = TRUE)


RG2 <- logitmfx(Envious ~ EI1 + EI2 + EI3 + EP1 + EP3 
                + T1G2 + T3G2 + T7G2 + T9G2
                +survey_1_player_age + survey_1_player_income, 
                data =MasterClean, robust = TRUE)


RGG2 <- logitmfx(Envious ~ EI1 + EI2 + EI3 + EP1 + EP3 
                + T4G2 + T5G2 + T6G2 + T8G2
                +survey_1_player_age + survey_1_player_income, 
                data =MasterClean, robust = TRUE)


RT2 <- logitmfx(Envious ~ T1G2 + T2G2 + T3G2 + T4G2
                + T5G2 + T6G2 + T7G2 + T8G2 + T9G2
                +survey_1_player_age + survey_1_player_income, 
                data =MasterClean, robust = TRUE)


texreg(list(RMG2, RG2, RGG2, RT2), caption = "Treatment Effects On Egalitarian Decisions Game 2",
       model.names=c("All", "All", "All"),
       custom.coef.map = list("EI1" = "LDict","EI2" = "MDict","EI3" = "HDict", 
                              "EP1" = "LSubj", "EP3" = "HSubj","T1G2" = "LDict*LSubj", 
                              "T2G2" = "LDict*MSubj", "T3G2" = "LDict*HSubj",
                              "T4G2" = "MDict*LSubj", "T5G2" = "MDict*MSubj",
                              "T6G2" = "MDict*HSubj","T7G2" = "HDict*LSubj", 
                              "T8G2" = "HDict*MSubj","T9G2" = "HDict*HSubj"),
       digits = 3,
       caption.above = TRUE)



##############################################################################################
#############Second Analysis - Egalitarian Decision is one that closes the income gap##########
##############################################################################################


MasterClean$Egalitarian1 <- ifelse(MasterClean$dictator_1_player_initial > MasterClean$dictator_1_player_partner & MasterClean$dictator_1_player_kept < 50 |
                                     MasterClean$dictator_1_player_initial < MasterClean$dictator_1_player_partner & MasterClean$dictator_1_player_kept > 50 |
                                     MasterClean$dictator_1_player_initial == MasterClean$dictator_1_player_partner & MasterClean$dictator_1_player_kept == "50", 1, 0)


MasterClean$Egalitarian2 <- ifelse(MasterClean$EgalEXP_1_player_initial > MasterClean$EgalEXP_1_player_partner & MasterClean$EgalEXP_1_player_kept == "75" |
                                     MasterClean$EgalEXP_1_player_initial < MasterClean$EgalEXP_1_player_partner & MasterClean$EgalEXP_1_player_kept == "50" |
                                     MasterClean$EgalEXP_1_player_initial == MasterClean$EgalEXP_1_player_partner & MasterClean$EgalEXP_1_player_kept == "50", 1, 0)


#############################Group Mean/se Graphs for Second Analysis##################################
########Portion Egalitarian By Own Initial Treatment Game 1############### 


SumTable7 <- summarySE(MasterClean,
                       measurevar="Egalitarian1", 
                       groupvars=c("dictator_1_player_initial"))
SumTable7



ggplot(SumTable7, aes(x = dictator_1_player_initial, y=Egalitarian1, fill = dictator_1_player_initial)) + 
  geom_bar( stat = "identity", alpha = 0.8) +
  geom_text(aes(label=N), y=.3) +
  geom_text(aes(label=round(Egalitarian1, 2)), position = position_nudge(y=SumTable7$se + 0.015), size=3.5) +
  geom_errorbar(aes(ymin=Egalitarian1 - se, ymax=Egalitarian1 + se),
                width = 0.5, position=position_dodge(.7)) +
  labs(x = "Dictator's Endowment", y = "Portion Egalitarian", fill = "Dictator's Endowment") +
  coord_cartesian(ylim=c(.3,.6)) +
  ggtitle("Egalitarian Decisions by Own Endowment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
  theme(legend.position="bottom")


########Portion Egalitarian By Partner Initial Treatment Game 1############### 

SumTable8 <- summarySE(MasterClean,
                       measurevar="Egalitarian1", 
                       groupvars=c("dictator_1_player_partner"))
SumTable8



ggplot(SumTable8, aes(x = dictator_1_player_partner, y=Egalitarian1, fill = dictator_1_player_partner)) + 
  geom_bar( stat = "identity", alpha = 0.8) +
  geom_text(aes(label=N), y=.3) +
  geom_text(aes(label=round(Egalitarian1, 2)), position = position_nudge(y=SumTable8$se + 0.015), size=3.5) +
  geom_errorbar(aes(ymin=Egalitarian1 - se, ymax=Egalitarian1 + se),
                width = 0.5, position=position_dodge(.7)) +
  labs(x = "Subject's Endowment", y = "Portion Egalitarian", fill = "Subject's Endowment") +
  coord_cartesian(ylim=c(.3,.6)) +
  ggtitle("Egalitarian Decisions by Partner's Endowment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
  theme(legend.position="bottom")



########Portion Egalitarian By Own and Partner Initial Treatment Game 1############### 

SumTable9 <- summarySE(MasterClean,
                       measurevar="Egalitarian1", 
                       groupvars=c("dictator_1_player_partner","dictator_1_player_initial"))
SumTable9

ggplot(SumTable9, aes(x = dictator_1_player_initial, y= Egalitarian1, fill = dictator_1_player_partner)) + 
  geom_bar(position="dodge", stat = "identity", alpha = 0.8) +
  geom_text(aes(label=N, y=0.1), position=position_dodge(width=0.9), size=3.5) +
  geom_text(aes(label=round(Egalitarian1, 2), y = SumTable9$Egalitarian1 + SumTable9$se + .025), position=position_dodge(width=0.9), size=3) +
  geom_errorbar(aes(ymin=Egalitarian1 - se, ymax=Egalitarian1 + se),
                width = 0.5, position=position_dodge(0.9)) +
  labs(x = "Dictator's Endownment", y = "Portion Egalitarian", fill = "Subject's Endowment") +
  coord_cartesian(ylim=c(0.1,0.8)) +
  ggtitle("Portion Egalitarian by Own and 
        Partner's Endowment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
  theme(legend.position="bottom")



########Portion Egalitarian by Own Initial Endowment Game 2############### 

SumTable10 <- summarySE(MasterClean,
                       measurevar="Egalitarian2", 
                       groupvars=c("EgalEXP_1_player_initial"))
SumTable10


ggplot(SumTable10, aes(x = EgalEXP_1_player_initial, y=Egalitarian2, fill = EgalEXP_1_player_initial)) + 
  geom_bar( stat = "identity", alpha = 0.8) +
  geom_text(aes(label=N), y=.2) +
  geom_text(aes(label=round(Egalitarian2, 2)), position = position_nudge(y=SumTable10$se + 0.02), size=3.5) +
  geom_errorbar(aes(ymin=Egalitarian2 - se, ymax=Egalitarian2 + se),
                width = 0.5, position=position_dodge(.7)) +
  labs(x = "Dictator's Endowment", y = "Portion Egalitarian", fill = "Dictator's Endowment") +
  coord_cartesian(ylim=c(.2,.7)) +
  ggtitle("Egalitarian Decisions by Own Endowment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
  theme(legend.position="bottom")


########Portion Egalitarian by Partner's Endowment Game 2############### 

SumTable11 <- summarySE(MasterClean,
                       measurevar="Egalitarian2", 
                       groupvars=c("EgalEXP_1_player_partner"))
SumTable11



ggplot(SumTable11, aes(x = EgalEXP_1_player_partner, y=Egalitarian2, fill = EgalEXP_1_player_partner)) + 
  geom_bar( stat = "identity", alpha = 0.8) +
  geom_text(aes(label=N), y=.2) +
  geom_text(aes(label=round(Egalitarian2, 2)), position = position_nudge(y=SumTable11$se + 0.02), size=3.5) +
  geom_errorbar(aes(ymin=Egalitarian2 - se, ymax=Egalitarian2 + se),
                width = 0.5, position=position_dodge(.7)) +
  labs(x = "Subject's Endowment", y = "Portion Egalitarian", fill = "Subject's Endowment") +
  coord_cartesian(ylim=c(.2,.7)) +
  ggtitle("Egalitarian Decisions by Partner's Endowment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
  theme(legend.position="bottom")

########Portion Egalitarianby Own and Partner's Endowment Game 2############### 

SumTable12 <- summarySE(MasterClean,
                       measurevar="Egalitarian2", 
                       groupvars=c("EgalEXP_1_player_partner","EgalEXP_1_player_initial"))
SumTable12

ggplot(SumTable12, aes(x = EgalEXP_1_player_initial, y= Egalitarian2, fill = EgalEXP_1_player_partner)) + 
  geom_bar(position="dodge", stat = "identity", alpha = 0.8) +
  geom_text(aes(label=N, y=0.1), position=position_dodge(width=0.9), size=3.5) +
  geom_text(aes(label=round(Egalitarian2, 2), y = SumTable12$Egalitarian2 + SumTable12$se + .02), position=position_dodge(width=0.9), size=3) +
  geom_errorbar(aes(ymin=Egalitarian2 - se, ymax=Egalitarian2 + se),
                width = 0.5, position=position_dodge(0.9)) +
  labs(x = "Dictator's Endownment", y = "Portion Egalitarian", fill = "Subject's Endowment") +
  coord_cartesian(ylim=c(0.1,0.85)) +
  ggtitle("Portion Egalitarian by Own and 
          Partner's Endowment") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
  theme(legend.position="bottom")


########## Egalitarian Regressions for Game 1: dep = prob(egalitarian) = 1,#######
################ ind = treatments (T1G1, T2G2, ect..) ############################


REMG1 <- logitmfx(Egalitarian1 ~ DI1 + DI2 + DI3 + DP1 + DP3
                  + survey_1_player_age + male, 
                 data =MasterClean, robust = TRUE)
REMG1

RE1G1 <- logitmfx(Egalitarian1 ~ DI1 + DI2 + DI3 + DP1 + DP3 
                + T1G1 + T3G1 + T7G1 + T9G1 + survey_1_player_age + male, 
                data =MasterClean, robust = TRUE)
RE1G1

RE2G1 <- logitmfx(Egalitarian1 ~ DI1 + DI2 + DI3 + DP1 + DP3 
                 + T4G1 + T5G1 + T6G1 + T8G1 + survey_1_player_age + male, 
                 data =MasterClean, robust = TRUE)
RE2G1

RET2 <- logitmfx(Egalitarian1 ~ T1G1 + T2G1 + T3G1 + T4G1
                + T5G1 + T6G1 + T7G1 + T8G1 + T9G1 + survey_1_player_age + male, 
                data =MasterClean, robust = TRUE)
RET2


texreg(list(REMG1, RE1G1 ,RE2G1 , RET2), caption = "Treatment Effects On Egalitarian Decisions Game 2",
       model.names=c("All", "All", "All", "All"),
       custom.coef.map = list("DI1" = "LDict","DI2" = "MDict","DI3" = "HDict", 
                              "DP1" = "LSubj", "DP3" = "HSubj","T1G1" = "LDict*LSubj", 
                              "T2G1" = "LDict*MSubj", "T3G1" = "LDict*HSubj",
                              "T4G1" = "MDict*LSubj", "T5G1" = "MDict*MSubj",
                              "T6G1" = "MDict*HSubj","T7G1" = "HDict*LSubj", 
                              "T8G1" = "HDict*MSubj","T9G1" = "HDict*HSubj"),
       digits = 3,
       caption.above = TRUE)

########## Egalitarian Regressions for Game 2: dep = prob(egalitarian) = 1,#######
################ ind = treatments (T1G1, T2G2, ect..) ############################


RLMG1 <- logitmfx(Egalitarian2 ~ EI1 + EI2 + EI3 + EP1 + EP3
                 + survey_1_player_age + survey_1_player_income, 
                  data =MasterClean, robust = TRUE)
RLMG1


RL1G1 <- logitmfx(Egalitarian2 ~ EI1 + EI2 + EI3 + EP1 + EP3 
                  + T1G2 + T3G2 + T7G2 + T9G2
                  + survey_1_player_age + survey_1_player_income, 
                  data =MasterClean, robust = TRUE)
RL1G1


RL2G1 <- logitmfx(Egalitarian2 ~ EI1 + EI2 + EI3 + EP1 + EP3 
                  + T4G2 + T5G2 + T6G2 + T8G2
                  + survey_1_player_age + survey_1_player_income, 
                  data =MasterClean, robust = TRUE)
RL2G1


RLT2 <- logitmfx(Egalitarian2 ~ T1G2 + T2G2 + T3G2 + T4G2
                 + T5G2 + T6G2 + T7G2 + T8G2 + T9G2 
                 + survey_1_player_age + survey_1_player_income, 
                 data =MasterClean, robust = TRUE)
RLT2



#############Output results for egalitarian regressions#################


texreg(list(RLMG1, RL1G1 ,RL2G1 , RLT2), caption = "Treatment Effects On Egalitarian Decisions Game 2",
       model.names=c("All", "All", "All", "All"),
       custom.coef.map = list("EI1" = "LDict","EI2" = "MDict","EI3" = "HDict", 
                              "EP1" = "LSubj", "EP3" = "HSubj","T1G2" = "LDict*LSubj", 
                              "T2G2" = "LDict*MSubj", "T3G2" = "LDict*HSubj",
                              "T4G2" = "MDict*LSubj", "T5G2" = "MDict*MSubj",
                              "T6G2" = "MDict*HSubj","T7G2" = "HDict*LSubj", 
                              "T8G2" = "HDict*MSubj","T9G2" = "HDict*HSubj"),
       digits = 3,
       caption.above = TRUE)


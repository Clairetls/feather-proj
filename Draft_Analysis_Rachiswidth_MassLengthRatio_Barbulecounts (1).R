
###Final Analyses ###

#===Install packages==================

# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("rptR")
# install.packages("lme4")
# install.packages("car")
# install.packages("stargazer")
# install.packages("ggplot2")
# install.packages("ggeffects")
# install.packages("lmerTest")
# install.packages("sjPlot")


#===Set working directory===

# setwd("C:/Users/eliza/OneDrive/Masterproject/Data - Sechelles Warbler Data/Analyses")

#===Include libraries=================

library(tidyverse)
library(readxl)
library(stats)
library(rptR)
library(lme4)
library(car)
library(stargazer)
library(ggplot2)
library(ggeffects)
library(lmtest)
library(lmerTest)
library(sjPlot)

##tab_model(mixed.lmer_MassLengthRatio)

#####===Read data===######

dat1 <- read_excel("2024-01-25_FinalDataSet_Collection.xlsx")
dat1<-dat1[,-c(1,6)]

featherdata<-dat1[,c(1:19,23,27:37)]
#===Convert to right data type=========
# dat2 <- dat1 %>%
#   mutate(
#     BirthDate = as.Date(BirthDate),               # Convert to date
#     BirthYear = as.factor(BirthYear),             # Convert to factor
#     OccasionDate = as.Date(OccasionDate),         # Convert to date
#     BodyMass = as.numeric(BodyMass),              # Convert to numeric
#     MassLengthRatio = as.numeric(MassLengthRatio),# Convert to numeric
#     RightTarsus = as.numeric(RightTarsus),        # Convert to numeric
#     WidthRachis = as.numeric(WidthRachis),        # Convert to numeric
#     BarbuleSUM_R1 = as.numeric(BarbuleSUM_R1),    # Convert to numeric
#     BarbuleSUM_R2 = as.numeric(BarbuleSUM_R2),    # Convert to numeric
#     SexEstimate = as.factor(SexEstimate)          # Convert to factor
#   )


#check lifespan 

lifespan<-read.csv('lifespan.csv')
lifespan<-lifespan[,-c(1)]
featherdata<-left_join(featherdata, lifespan, by='BirdID')
test<-test[c('BirdID',"newlifespan",'Lifespan')]


#=== Set status to dominant, subordinate, helper ===========

test <- featherdata%>%
  mutate(NewRepStatus = case_when(ReproductiveStatus == "BrF" ~ "Dom",
                           ReproductiveStatus == "BrM" ~ "Dom",
                           ReproductiveStatus == "BrU" ~ "Dom",
                           ReproductiveStatus == "H" ~ "H"))
"%!in%"<-Negate('%in%')

test$NewRepStatus[test$ReproductiveStatus %!in% c("BrF",'BrM','BrU',"H")] <- "Sub"


featherdata<-test

#check avg invert 
mean_insect<-read.csv('mean_insect_new.csv')

featherdata$occasionyear<-as.numeric(str_sub(featherdata$OccasionDate, 1,4))
avgbugs<-avgbugs%>%
  group_by(occasionyear)%>%
  mutate(meanbug=mean(avg_invert, na.rm=T))%>%
  select(occasionyear, meanbug)%>%
  unique()

test<-left_join(featherdata, avgbugs, by='occasionyear')
test<-test[c('InsectCounts', 'meanbug')]

#she used one season per year



#### RACHIS WIDTH #######################

#===Create data frame====================

WidthRachis <- dat2$WidthRachis
BirdID <- dat2$BirdID

str(WidthRachis)

hist(dat2$WidthRachis) 


#===lme for the effect of age on rachis width============

dat2$SexEstimate=ifelse(dat2$SexEstimate==0,"F","M")
names(dat2)[18]="Sex"

#Model 1: only body mass
mixed.lmer_WidthRachis1 <- lmer(WidthRachis ~
                                   Age +
                                   I(Age^2) +              #age squared
                                   Age*Sex +               #interaction of age and sex
                                   I(Age^2)*Sex + 
                                   Lifespan +
                                   Sex +
                                   BodyMass +
                                   #BodymassTarsus +  
                                   #RightTarsus +
                                   NewRepStatus +
                                   InsectCounts +
                                   Tailmoult +
                                   (1 | BirdID) +
                                   (1 | BirthYear),        #Cohort
                                 data = dat2)

#Model 2: only tarsus length
mixed.lmer_WidthRachis2 <- lmer(WidthRachis ~
                                  Age +
                                  I(Age^2) +              #age squared
                                  Age*Sex +               #interaction of age and sex
                                  I(Age^2)*Sex + 
                                  Lifespan +
                                  Sex +
                                  #BodyMass +
                                  RightTarsus +
                                  #BodymassTarsus +  
                                  NewRepStatus +
                                  InsectCounts +
                                  Tailmoult +
                                  (1 | BirdID) +
                                  (1 | BirthYear),        #Cohort
                                data = dat2)

#Model 3: both body mass and tarsus length
mixed.lmer_WidthRachis3 <- lmer(WidthRachis ~
                                  Age +
                                  I(Age^2) +              #age squared
                                  Age*Sex +               #interaction of age and sex
                                  I(Age^2)*Sex + 
                                  Lifespan +
                                  Sex +
                                  BodyMass +
                                  RightTarsus +
                                  #BodymassTarsus +  
                                  NewRepStatus +
                                  InsectCounts +
                                  Tailmoult +
                                  (1 | BirdID) +
                                  (1 | BirthYear),        #Cohort
                                data = dat2)

#Model 4: with body mass/tarsus length ratio
mixed.lmer_WidthRachis4 <- lmer(WidthRachis ~
                                  Age +
                                  I(Age^2) +              #age squared
                                  Age*Sex +               #interaction of age and sex
                                  I(Age^2)*Sex + 
                                  Lifespan +
                                  Sex +
                                  #BodyMass +
                                  #RightTarsus +
                                  BodymassTarsus +  
                                  NewRepStatus +
                                  InsectCounts +
                                  Tailmoult +
                                  (1 | BirdID) +
                                  (1 | BirthYear),        #Cohort
                                data = dat2)

lrtest(mixed.lmer_WidthRachis1, mixed.lmer_WidthRachis2, mixed.lmer_WidthRachis3, mixed.lmer_WidthRachis4)

#  #Df LogLik Df   Chisq Pr(>Chisq)    
# 1  15 574.52                          
# 2  15 569.02  0 10.9999    < 2e-16 *** >> choose model 2, which is most significant, thus gives best prediction
# 3  16 570.14  1  2.2322    0.13516    
# 4  15 573.23 -1  6.1894    0.01285 *  


#Check colinearity
vif(mixed.lmer_WidthRachis2) # > fine (age and interaction effects are, but thats logical: they are correlated)

#Plot to check assumptions
plot(mixed.lmer_WidthRachis2) #> fine (checked one outlier, was fine: old & light bird)

#and qqplot
qqnorm(resid(mixed.lmer_WidthRachis2))
qqline(resid(mixed.lmer_WidthRachis2))  # good enough


Anova(mixed.lmer_WidthRachis2)

## OUTPUT:
#
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: WidthRachis
# Chisq Df Pr(>Chisq)   
# Age          0.0804  1   0.776699   
# I(Age^2)     0.3728  1   0.541486   
# Sex          9.1903  1   0.002433 **
# Lifespan     0.8330  1   0.361405   
# RightTarsus  2.0134  1   0.155911   
# NewRepStatus 3.3265  2   0.189523   
# InsectCounts 0.3036  1   0.581655   
# Tailmoult    3.7956  1   0.051388 . 
# Age:Sex      4.7548  1   0.029216 * 
# I(Age^2):Sex 7.0852  1   0.007772 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


mixed.lmer_WidthRachis2

## Model Output:
#
# Linear mixed model fit by REML ['lmerModLmerTest']
# Formula: WidthRachis ~ Age + I(Age^2) + Age * Sex + I(Age^2) * Sex + Lifespan +  
#   Sex + RightTarsus + NewRepStatus + InsectCounts + Tailmoult +      (1 | BirdID) + (1 | BirthYear)
# Data: dat2
# REML criterion at convergence: -1138.047
# Random effects:
#   Groups    Name        Std.Dev.
# BirdID    (Intercept) 0.01153 
# BirthYear (Intercept) 0.00000 
# Residual              0.04771 
# Number of obs: 397, groups:  BirdID, 219; BirthYear, 20
# Fixed Effects:
#   (Intercept)              Age         I(Age^2)             SexM         Lifespan      RightTarsus  
#     4.503e-01        6.635e-03       -7.629e-04        2.972e-02       -3.043e-06        3.878e-03  
# NewRepStatusH  NewRepStatusSub     InsectCounts        Tailmoult         Age:SexM    I(Age^2):SexM  
#    -1.887e-02       -6.531e-03        6.081e-04       -1.099e-02       -8.968e-03        9.440e-04  
# fit warnings:
#   Some predictor variables are on very different scales: consider rescaling
# optimizer (nloptwrap) convergence code: 0 (OK) ; 0 optimizer warnings; 1 lme4 warnings 

    
summary(mixed.lmer_WidthRachis2)

## Model Summary Output: 
## 
#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: WidthRachis ~ Age + I(Age^2) + Age * Sex + I(Age^2) * Sex + Lifespan +  
#   Sex + RightTarsus + NewRepStatus + InsectCounts + Tailmoult +      (1 | BirdID) + (1 | BirthYear)
# Data: dat2
# 
# REML criterion at convergence: -1138
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.6851 -0.5197  0.0845  0.5962  2.4138 
# 
# Random effects:
#   Groups    Name        Variance  Std.Dev.
# BirdID    (Intercept) 0.0001329 0.01153 
# BirthYear (Intercept) 0.0000000 0.00000 
# Residual              0.0022764 0.04771 
# Number of obs: 397, groups:  BirdID, 219; BirthYear, 20
# 
# Fixed effects:
#                   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      4.503e-01  6.818e-02  3.406e+02   6.605 1.53e-10 ***
# Age              6.635e-03  3.798e-03  3.544e+02   1.747  0.08152 .  
# I(Age^2)        -7.629e-04  3.100e-04  3.653e+02  -2.461  0.01432 *  
# SexM             2.972e-02  9.965e-03  3.121e+02   2.982  0.00309 ** 
# Lifespan        -3.043e-06  3.334e-06  3.149e+02  -0.913  0.36210    
# RightTarsus      3.878e-03  2.733e-03  3.448e+02   1.419  0.15681    
# NewRepStatusH   -1.887e-02  1.035e-02  3.293e+02  -1.823  0.06926 .  
# NewRepStatusSub -6.531e-03  7.587e-03  3.643e+02  -0.861  0.38989    
# InsectCounts     6.081e-04  1.104e-03  3.850e+02   0.551  0.58197    
# Tailmoult       -1.099e-02  5.640e-03  3.850e+02  -1.948  0.05211 .  
# Age:SexM        -8.968e-03  4.113e-03  3.297e+02  -2.181  0.02992 *  
# I(Age^2):SexM    9.440e-04  3.547e-04  3.179e+02   2.662  0.00817 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) Age    I(Ag^2) SexM   Lifspn RghtTr NwRpSH NwRpSS InsctC Talmlt Ag:SxM
# Age         -0.212                                                                       
# I(Age^2)     0.205 -0.896                                                                
# SexM         0.344  0.455 -0.359                                                         
# Lifespan    -0.010 -0.188 -0.077   0.005                                                 
# RightTarsus -0.985  0.097 -0.106  -0.436 -0.023                                          
# NewRepSttsH -0.070  0.390 -0.284   0.120 -0.016 -0.013                                   
# NewRpSttsSb -0.149  0.403 -0.296  -0.022  0.035  0.053  0.503                            
# InsectConts -0.072  0.134 -0.097   0.050 -0.164 -0.011  0.020  0.082                     
# Tailmoult    0.034  0.018  0.003   0.049 -0.055 -0.045 -0.024 -0.110 -0.044              
# Age:SexM     0.139 -0.709  0.701  -0.633  0.036 -0.072 -0.049  0.018 -0.046 -0.092       
# I(Ag^2):SxM -0.148  0.682 -0.781   0.460 -0.023  0.092  0.050  0.010  0.064  0.067 -0.922
# fit warnings:
#   Some predictor variables are on very different scales: consider rescaling
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')



###=== Plotting ======================


### Plot overall predictions for the model: 

# Extract the prediction data frame
pred.mm <- ggpredict(mixed.lmer_WidthRachis2, terms = c("Age"))  # this gives overall predictions for the model

# Plot the predictions 
(ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = dat2,                      # adding the raw data (scaled values)
               aes(x = Age, y = WidthRachis, colour = Sex)) + 
    labs(x = "Age", y = "Rachis Width", 
         title = "Title") + 
    theme_minimal()
)

### Plot both datapoints and model predictions for both sexes in 1 plot

(ggpredict(mixed.lmer_WidthRachis2, terms = c("Age", "Sex"), type = "re") %>% 
    plot(show_data = TRUE) +
    labs(x = "Age", y = "RachisWidth", title = "Rachis width over Age") + 
    theme_minimal()
)





#### Rachis Width - for both sexes separately #######################

datM <- read_excel("2024-01-25_Final only males.xlsx")
datF <- read_excel("2024-01-25_Final only females.xlsx")


#===Convert to right data type=========
datM2 <- datM %>%
  mutate(
    BirthDate = as.Date(BirthDate),               # Convert to date
    BirthYear = as.factor(BirthYear),             # Convert to factor
    OccasionDate = as.Date(OccasionDate),         # Convert to date
    BodyMass = as.numeric(BodyMass),              # Convert to numeric
    MassLengthRatio = as.numeric(MassLengthRatio),# Convert to numeric
    RightTarsus = as.numeric(RightTarsus),        # Convert to numeric
    WidthRachis = as.numeric(WidthRachis),        # Convert to numeric
    BarbuleSUM_R1 = as.numeric(BarbuleSUM_R1),    # Convert to numeric
    BarbuleSUM_R2 = as.numeric(BarbuleSUM_R2),    # Convert to numeric
    SexEstimate = as.factor(SexEstimate)          # Convert to factor
  )

datF2 <- datF %>%
  mutate(
    BirthDate = as.Date(BirthDate),               # Convert to date
    BirthYear = as.factor(BirthYear),             # Convert to factor
    OccasionDate = as.Date(OccasionDate),         # Convert to date
    BodyMass = as.numeric(BodyMass),              # Convert to numeric
    MassLengthRatio = as.numeric(MassLengthRatio),# Convert to numeric
    RightTarsus = as.numeric(RightTarsus),        # Convert to numeric
    WidthRachis = as.numeric(WidthRachis),        # Convert to numeric
    BarbuleSUM_R1 = as.numeric(BarbuleSUM_R1),    # Convert to numeric
    BarbuleSUM_R2 = as.numeric(BarbuleSUM_R2),    # Convert to numeric
    SexEstimate = as.factor(SexEstimate)          # Convert to factor
  )

#=== Set status to dominant, subordinate, helper ===========
#For the male data:
ReproductiveStatus <- datM2$ReproductiveStatus

datM2 <- datM2%>%
  mutate(NewRepStatus = case_when(ReproductiveStatus == "BrF" ~ "Dom",
                                  ReproductiveStatus == "BrM" ~ "Dom",
                                  ReproductiveStatus == "BrU" ~ "Dom",
                                  ReproductiveStatus == "H" ~ "H"))

datM2$NewRepStatus[datM2$ReproductiveStatus %in% c("U","SEEN1","SEEN2","AB",'ABX','FLOAT','OFL','B')] <- "Sub"

datM2$NewRepStatus #Check whether converted correctly > fine!
str(datM2$NewRepStatus) #Still 238 values

#For the female data: 
ReproductiveStatus <- datF2$ReproductiveStatus

datF2 <- datF2%>%
  mutate(NewRepStatus = case_when(ReproductiveStatus == "BrF" ~ "Dom",
                                  ReproductiveStatus == "BrM" ~ "Dom",
                                  ReproductiveStatus == "BrU" ~ "Dom",
                                  ReproductiveStatus == "H" ~ "H"))

datF2$NewRepStatus[datF2$ReproductiveStatus %in% c("U","SEEN1","SEEN2","AB",'ABX','FLOAT','OFL','B')] <- "Sub"

datF2$NewRepStatus #Check whether converted correctly > fine!
str(datF2$NewRepStatus) #Still 159 values


#===Create data frame====================

WidthRachisM <- datM2$WidthRachis
BirdIDM <- datM2$BirdID
str(WidthRachisM)
hist(datM2$WidthRachis) 

WidthRachisF <- datF2$WidthRachis
BirdIDF <- datF2$BirdID
str(WidthRachisF)
hist(datF2$WidthRachis) 


#===lme for the effect of age on rachis width============

#===Model MALES==========================================
mixed.lmer_WidthRachisM <- lmer(WidthRachis ~
                                  Age +
                                  I(Age^2) +              #age squared
                                  #Age*Sex +              #interaction of age and sex
                                  #I(Age^2)*Sex + 
                                  Lifespan +
                                  #Sex +
                                  #BodyMass +
                                  RightTarsus +
                                  #BodymassTarsus +  
                                  NewRepStatus +
                                  InsectCounts +
                                  Tailmoult +
                                  (1 | BirdID) +
                                  (1 | BirthYear),        #Cohort
                                data = datM2)

#Plot to check assumptions
plot(mixed.lmer_WidthRachisM) #Fine

qqnorm(resid(mixed.lmer_WidthRachisM))
qqline(resid(mixed.lmer_WidthRachisM))  # good enough

Anova(mixed.lmer_WidthRachisM)
# Response: WidthRachis
# Chisq Df Pr(>Chisq)  
# Age          1.2540  1    0.26279   >> no significant effect of age!
# I(Age^2)     1.3963  1    0.23734  
# Lifespan     0.8430  1    0.35855  
# RightTarsus  3.4528  1    0.06314 .
# NewRepStatus 3.9586  2    0.13817  
# InsectCounts 0.0001  1    0.99196  
# Tailmoult    1.0190  1    0.31276  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


mixed.lmer_WidthRachisM

summary(mixed.lmer_WidthRachisM)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: WidthRachis ~ Age + I(Age^2) + Lifespan + RightTarsus + NewRepStatus +  
#     InsectCounts + Tailmoult + (1 | BirdID) + (1 | BirthYear)
#    Data: datM2
# 
# REML criterion at convergence: -695.7
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.8506 -0.5314  0.0838  0.5104  2.6027 
# 
# Random effects:
#  Groups    Name        Variance Std.Dev.
#  BirdID    (Intercept) 0.000000 0.00000 
#  BirthYear (Intercept) 0.000000 0.00000 
#  Residual              0.002114 0.04598 
# Number of obs: 238, groups:  BirdID, 134; BirthYear, 18
# 
# Fixed effects:
#                   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      4.056e-01  9.951e-02  2.290e+02   4.076 6.33e-05 ***
# Age             -3.585e-03  3.201e-03  2.290e+02  -1.120   0.2640    
# I(Age^2)         2.627e-04  2.223e-04  2.290e+02   1.182   0.2386    
# Lifespan        -3.552e-06  3.869e-06  2.290e+02  -0.918   0.3595    
# RightTarsus      6.979e-03  3.756e-03  2.290e+02   1.858   0.0644 .  
# NewRepStatusH   -2.868e-02  1.442e-02  2.290e+02  -1.989   0.0479 *  
# NewRepStatusSub -9.892e-03  9.429e-03  2.290e+02  -1.049   0.2953    
# InsectCounts    -1.512e-05  1.501e-03  2.290e+02  -0.010   0.9920    
# Tailmoult       -6.786e-03  6.723e-03  2.290e+02  -1.009   0.3138    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) Age    I(A^2) Lifspn RghtTr NwRpSH NwRpSS InsctC
# Age         -0.148                                                 
# I(Age^2)     0.118 -0.864                                          
# Lifespan    -0.034 -0.178 -0.188                                   
# RightTarsus -0.992  0.070 -0.046  0.004                            
# NewRepSttsH -0.130  0.534 -0.419 -0.021  0.068                     
# NewRpSttsSb -0.177  0.625 -0.523  0.132  0.095  0.513              
# InsectConts -0.049  0.127 -0.041 -0.182 -0.021  0.065  0.057       
# Tailmoult    0.061 -0.194  0.182 -0.027 -0.055 -0.071 -0.219 -0.099
# fit warnings:
# Some predictor variables are on very different scales: consider rescaling
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')


#===Model FEMALES======================================
mixed.lmer_WidthRachisF <- lmer(WidthRachis ~
                                  Age +
                                  I(Age^2) +              #age squared
                                  #Age*Sex +              #interaction of age and sex
                                  #I(Age^2)*Sex + 
                                  Lifespan +
                                  #Sex +
                                  #BodyMass +
                                  RightTarsus +
                                  #BodymassTarsus +  
                                  NewRepStatus +
                                  InsectCounts +
                                  Tailmoult +
                                  (1 | BirdID) +
                                  (1 | BirthYear),        #Cohort
                                data = datF2)


#Plot to check assumptions
plot(mixed.lmer_WidthRachisF) #Fine

qqnorm(resid(mixed.lmer_WidthRachisF))
qqline(resid(mixed.lmer_WidthRachisF))  # good enough

Anova(mixed.lmer_WidthRachisF)
# Response: WidthRachis
# Chisq Df Pr(>Chisq)  
# Age          1.6563  1    0.19811  
# I(Age^2)     4.9307  1    0.02638 *
# Lifespan     0.0018  1    0.96571  
# RightTarsus  0.0924  1    0.76112  
# NewRepStatus 0.5628  2    0.75475  
# InsectCounts 0.6765  1    0.41081  
# Tailmoult    3.7735  1    0.05207 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

mixed.lmer_WidthRachisF

summary(mixed.lmer_WidthRachisF)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: WidthRachis ~ Age + I(Age^2) + Lifespan + RightTarsus + NewRepStatus +  
#     InsectCounts + Tailmoult + (1 | BirdID) + (1 | BirthYear)
#    Data: datF2
# 
# REML criterion at convergence: -391.3
# 
# Scaled residuals: 
#      Min       1Q   Median       3Q      Max 
# -2.98627 -0.44522  0.05433  0.65001  2.01842 
# 
# Random effects:
#  Groups    Name        Variance  Std.Dev.
#  BirdID    (Intercept) 4.766e-04 0.021830
#  BirthYear (Intercept) 2.173e-05 0.004662
#  Residual              2.412e-03 0.049111
# Number of obs: 159, groups:  BirdID, 85; BirthYear, 17
# 
# Fixed effects:
#                   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      5.068e-01  1.017e-01  1.432e+02   4.982 1.79e-06 ***
# Age              6.529e-03  5.074e-03  7.219e+01   1.287   0.2022    
# I(Age^2)        -8.228e-04  3.705e-04  6.546e+01  -2.221   0.0299 *  
# Lifespan         2.752e-07  6.402e-06  1.252e+02   0.043   0.9658    
# RightTarsus      1.245e-03  4.096e-03  1.470e+02   0.304   0.7616    
# NewRepStatusH   -1.183e-02  1.578e-02  1.339e+02  -0.750   0.4547    
# NewRepStatusSub -5.331e-03  1.340e-02  1.451e+02  -0.398   0.6914    
# InsectCounts     1.372e-03  1.668e-03  1.450e+02   0.822   0.4122    
# Tailmoult       -2.040e-02  1.050e-02  1.454e+02  -1.943   0.0540 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) Age    I(A^2) Lifspn RghtTr NwRpSH NwRpSS InsctC
# Age         -0.223                                                 
# I(Age^2)     0.222 -0.857                                          
# Lifespan     0.038 -0.337 -0.057                                   
# RightTarsus -0.984  0.108 -0.112 -0.069                            
# NewRepSttsH -0.027  0.478 -0.364 -0.060 -0.061                     
# NewRpSttsSb -0.144  0.592 -0.419 -0.164  0.035  0.501              
# InsectConts -0.090  0.201 -0.151 -0.166 -0.002 -0.004  0.150       
# Tailmoult   -0.006  0.165 -0.077 -0.157 -0.031  0.060  0.092  0.061
# fit warnings:
# Some predictor variables are on very different scales: consider rescaling


###=== Plotting ======================
### Plot overall predictions for the model: 

# Extract the prediction data frame for MALES
pred.mm <- ggpredict(mixed.lmer_WidthRachisM, terms = c("Age"))  # this gives overall predictions for the model

# Plot the predictions 
(ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = dat2,                      # adding the raw data (scaled values)
               aes(x = Age, y = WidthRachis)) + 
    labs(x = "Age", y = "Rachis Width M", 
         title = "Rachis width Males over Age") + 
    theme_minimal()
)

# Extract the prediction data frame for FEMALES
pred.mm <- ggpredict(mixed.lmer_WidthRachisF, terms = c("Age"))  # this gives overall predictions for the model

# Plot the predictions 
(ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = dat2,                      # adding the raw data (scaled values)
               aes(x = Age, y = WidthRachis)) + 
    labs(x = "Age", y = "Rachis Width F", 
         title = "Rachis width Females over Age") + 
    theme_minimal()
)




#================================================

#### Mass To Length Ratio #######################

#================================================
  

#===Create data frame==================

MassLengthRatio <- dat2$MassLengthRatio
BirdID <- dat2$BirdID

str(MassLengthRatio)

hist(MassLengthRatio) #quite normally distributed (only some very high numbers)



#===lme for the effect of age on mass/length ratio============

#Model 1: only body mass
mixed.lmer_MassLengthRatio1 <- lmer(MassLengthRatio ~
                                  Age +
                                  I(Age^2) +              #age squared
                                  Age*Sex +               #interaction of age and sex
                                  I(Age^2)*Sex + 
                                  Lifespan +
                                  Sex +
                                  BodyMass +
                                  #BodymassTarsus +  
                                  #RightTarsus +
                                  NewRepStatus +
                                  InsectCounts +
                                  Tailmoult +
                                  (1 | BirdID) +
                                  (1 | BirthYear),        #Cohort
                                data = dat2)

#Model 2: with body mass/tarsus length ratio
mixed.lmer_MassLengthRatio2 <- lmer(MassLengthRatio ~
                                  Age +
                                  I(Age^2) +              #age squared
                                  Age*Sex +               #interaction of age and sex
                                  I(Age^2)*Sex + 
                                  Lifespan +
                                  Sex +
                                  #BodyMass +
                                  #RightTarsus +
                                  BodymassTarsus +  
                                  NewRepStatus +
                                  InsectCounts +
                                  Tailmoult +
                                  (1 | BirdID) +
                                  (1 | BirthYear),        #Cohort
                                data = dat2)

#Likelihood Ratio Test, to test both models: 
lrtest(mixed.lmer_MassLengthRatio1, mixed.lmer_MassLengthRatio2)

# 1  15 1192.1                         
# 2  15 1191.8  0 0.5955  < 2.2e-16 ***                   > use this model 2
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


vif(mixed.lmer_MassLengthRatio2) #check colinearity > Fine, only high for Age, Age squared and interactions. 


#Plot to check assumptions
plot(mixed.lmer_MassLengthRatio2) #Fine

#and qqplot
qqnorm(resid(mixed.lmer_MassLengthRatio2))
qqline(resid(mixed.lmer_MassLengthRatio2))  # Fine


Anova(mixed.lmer_MassLengthRatio2)

## OUTPUT:
#
# Analysis of Deviance Table (Type II Wald chisquare tests)

# Response: MassLengthRatio
# Chisq Df Pr(>Chisq)    
# Age             5.7600  1   0.016395 *  
# I(Age^2)        0.9838  1   0.321273    
# Sex            31.3784  1  2.123e-08 ***
# Lifespan        7.5229  1   0.006092 ** 
# BodymassTarsus  7.7400  1   0.005401 ** 
# NewRepStatus    4.9888  2   0.082546 .  
# InsectCounts    0.0450  1   0.831917    
# Tailmoult       0.0627  1   0.802248    
# Age:Sex         0.0067  1   0.934553    
# I(Age^2):Sex    0.0909  1   0.763051    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



mixed.lmer_MassLengthRatio2

## Model Output:
#
#Linear mixed model fit by REML ['lmerModLmerTest']
# Formula: MassLengthRatio ~ Age + I(Age^2) + Age * Sex + I(Age^2) * Sex +  
#   Lifespan + Sex + BodymassTarsus + NewRepStatus + InsectCounts +      Tailmoult + (1 | BirdID) + (1 | BirthYear)
# Data: dat2
# REML criterion at convergence: -2383.658
# Random effects:
#   Groups    Name        Std.Dev.
# BirdID    (Intercept) 0.000000
# BirthYear (Intercept) 0.001656
# Residual              0.009718
# Number of obs: 397, groups:  BirdID, 219; BirthYear, 20
# Fixed Effects:
#   (Intercept)              Age         I(Age^2)             SexM         Lifespan   BodymassTarsus  
#     9.527e-02        1.365e-03       -5.401e-05        5.692e-03       -1.849e-06        3.375e-02  
# NewRepStatusH  NewRepStatusSub     InsectCounts        Tailmoult         Age:SexM    I(Age^2):SexM  
#    -2.819e-03       -3.384e-03       -4.797e-05       -2.838e-04       -6.708e-05        2.128e-05  
# fit warnings:
#   Some predictor variables are on very different scales: consider rescaling
# optimizer (nloptwrap) convergence code: 0 (OK) ; 0 optimizer warnings; 1 lme4 warnings 


summary(mixed.lmer_MassLengthRatio2)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: MassLengthRatio ~ Age + I(Age^2) + Age * Sex + I(Age^2) * Sex +  
#   Lifespan + Sex + BodymassTarsus + NewRepStatus + InsectCounts +      Tailmoult + (1 | BirdID) + (1 | BirthYear)
# Data: dat2
# 
# REML criterion at convergence: -2383.7
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.4813 -0.5512 -0.0212  0.4663  4.1453 
# 
# Random effects:
#   Groups    Name        Variance  Std.Dev.
# BirdID    (Intercept) 0.000e+00 0.000000
# BirthYear (Intercept) 2.741e-06 0.001656
# Residual              9.443e-05 0.009718
# Number of obs: 397, groups:  BirdID, 219; BirthYear, 20
# 
# Fixed effects:
#                   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      9.527e-02  7.451e-03  3.837e+02  12.785  < 2e-16 ***
# Age              1.365e-03  7.751e-04  3.562e+02   1.761  0.07918 .  
# I(Age^2)        -5.401e-05  6.314e-05  3.513e+02  -0.855  0.39288    
# SexM             5.692e-03  1.779e-03  3.845e+02   3.200  0.00149 ** 
# Lifespan        -1.849e-06  6.742e-07  3.248e+02  -2.743  0.00643 ** 
# BodymassTarsus   3.375e-02  1.213e-02  3.845e+02   2.782  0.00567 ** 
# NewRepStatusH   -2.819e-03  2.067e-03  3.762e+02  -1.364  0.17338    
# NewRepStatusSub -3.384e-03  1.526e-03  3.850e+02  -2.217  0.02720 *  
# InsectCounts    -4.797e-05  2.260e-04  3.603e+02  -0.212  0.83204    
# Tailmoult       -2.838e-04  1.133e-03  3.834e+02  -0.250  0.80238    
# Age:SexM        -6.708e-05  8.169e-04  3.810e+02  -0.082  0.93460    
# I(Age^2):SexM    2.128e-05  7.059e-05  3.643e+02   0.301  0.76322    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) Age    I(Ag^2) SexM   Lifspn BdymsT NwRpSH NwRpSS InsctC Talmlt Ag:SxM
# Age         -0.086                                                                       
# I(Age^2)     0.046 -0.898                                                                
# SexM        -0.055  0.565 -0.466                                                         
# Lifespan    -0.034 -0.184 -0.074  -0.012                                                 
# BodymssTrss -0.943 -0.139  0.150  -0.123 -0.031                                          
# NewRepSttsH -0.171  0.365 -0.272   0.111  0.013  0.023                                   
# NewRpSttsSb -0.309  0.365 -0.265  -0.026  0.042  0.146  0.510                            
# InsectConts -0.066  0.160 -0.123   0.067 -0.152 -0.101  0.017  0.062                     
# Tailmoult    0.097  0.038 -0.020   0.051 -0.054 -0.124 -0.031 -0.134 -0.024              
# Age:SexM     0.098 -0.705  0.698  -0.744  0.043  0.031 -0.041  0.027 -0.051 -0.103       
# I(Ag^2):SxM -0.050  0.680 -0.780   0.566 -0.028 -0.061  0.047 -0.001  0.073  0.083 -0.922
# fit warnings:
#   Some predictor variables are on very different scales: consider rescaling
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')


###=== Plotting ======================

### Took out insignificant interactions >> to make a linear regression plot
mixed.lmer_MassLengthRatio2 <- lmer(MassLengthRatio ~
                                      Age +
                                      #I(Age^2) +              #age squared
                                      #Age*Sex +               #interaction of age and sex
                                      #I(Age^2)*Sex + 
                                      Lifespan +
                                      Sex +
                                      #BodyMass +
                                      #RightTarsus +
                                      BodymassTarsus +  
                                      NewRepStatus +
                                      InsectCounts +
                                      Tailmoult +
                                      (1 | BirdID) +
                                      (1 | BirthYear),        #Cohort
                                    data = dat2)

### Plot overall predictions for the model: 
# Extract the prediction data frame
pred.mm2 <- ggpredict(mixed.lmer_MassLengthRatio2, terms = c("Age"))  # this gives overall predictions for the model

# Plot the predictions 
(ggplot(pred.mm2) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = dat2,                      # adding the raw data (scaled values)
               aes(x = Age, y = MassLengthRatio, colour = Sex)) + 
    labs(x = "Age", y = "Mass/Length Ratio", 
         title = "Mass/Length Ratio over Age") + 
    theme_minimal()
)




#====================================================

#### ==== Barbule Counts ==== #######################

#====================================================


#####===Read data===######


dat3 <- read_excel("2024-01-25_Final only barbules.xlsx")


#===Convert to right data type=========
dat4 <- dat3 %>%
  mutate(
    BirthDate = as.Date(BirthDate),               # Convert to date
    OccasionDate = as.Date(OccasionDate),         # Convert to date
    BodyMass = as.numeric(BodyMass),              # Convert to numeric
    MassLengthRatio = as.numeric(MassLengthRatio),# Convert to numeric
    RightTarsus = as.numeric(RightTarsus),        # Convert to numeric
    WidthRachis = as.numeric(WidthRachis),        # Convert to numeric
    BarbuleSUM_R1 = as.numeric(BarbuleSUM_R1),    # Convert to numeric
    BarbuleSUM_R2 = as.numeric(BarbuleSUM_R2),    # Convert to numeric
    SexEstimate = as.factor(SexEstimate)          # Convert to factor
  )


dat4$SexEstimate = ifelse(dat4$SexEstimate==0,"F","M")
names(dat4)[18]="Sex"

#=== Set status to dominant, subordinate, helper ===========
ReproductiveStatus <- dat4$ReproductiveStatus

dat4 <- dat4%>%
  mutate(NewRepStatus = case_when(ReproductiveStatus == "BrF" ~ "Dom",
                                  ReproductiveStatus == "BrM" ~ "Dom",
                                  ReproductiveStatus == "BrU" ~ "Dom",
                                  ReproductiveStatus == "H" ~ "H"))

dat4$NewRepStatus[dat4$ReproductiveStatus %in% c("U","SEEN1","SEEN2","AB",'ABX','FLOAT','OFL','B')] <- "Sub"

dat4$NewRepStatus #Check whether converted correctly > fine!
str(dat4$NewRepStatus) #Still 74 values




#===Create data frame==================

TotalBarbuleCount <- dat4$TotalBarbuleCount
BirdID <- dat4$BirdID

str(TotalBarbuleCount)

hist(dat4$TotalBarbuleCount) ##no weird peaks, fine



#===lme for the effect of age on Barbule density============


#Model 1: only body mass
mixed.lmer_TotalBarbuleCount1 <- lmer(TotalBarbuleCount ~
                                      Age +
                                      I(Age^2) +              #age squared
                                      Age*Sex +               #interaction of age and sex
                                      I(Age^2)*Sex + 
                                      Lifespan +
                                      Sex +
                                      BodyMass +
                                      #BodymassTarsus +  
                                      #RightTarsus +
                                      NewRepStatus +
                                      InsectCounts +
                                      Tailmoult +
                                      (1 | BirdID) +
                                      (1 | BirthYear),        #Cohort
                                    data = dat4)

#Model 2: only tarsus length
mixed.lmer_TotalBarbuleCount2 <- lmer(TotalBarbuleCount ~
                                  Age +
                                  I(Age^2) +              #age squared
                                  Age*Sex +               #interaction of age and sex
                                  I(Age^2)*Sex + 
                                  Lifespan +
                                  Sex +
                                  #BodyMass +
                                  RightTarsus +
                                  #BodymassTarsus +  
                                  NewRepStatus +
                                  InsectCounts +
                                  Tailmoult +
                                  (1 | BirdID) +
                                  (1 | BirthYear),        #Cohort
                                data = dat4)

#Model 3: both body mass and tarsus length
mixed.lmer_TotalBarbuleCount3 <- lmer(TotalBarbuleCount ~
                                  Age +
                                  I(Age^2) +              #age squared
                                  Age*Sex +               #interaction of age and sex
                                  I(Age^2)*Sex + 
                                  Lifespan +
                                  Sex +
                                  BodyMass +
                                  RightTarsus +
                                  #BodymassTarsus +  
                                  NewRepStatus +
                                  InsectCounts +
                                  Tailmoult +
                                  (1 | BirdID) +
                                  (1 | BirthYear),        #Cohort
                                data = dat4)

#Model 4: with body mass/tarsus length ratio
mixed.lmer_TotalBarbuleCount4 <- lmer(TotalBarbuleCount ~
                                  Age +
                                  I(Age^2) +              #age squared
                                  Age*Sex +               #interaction of age and sex
                                  I(Age^2)*Sex + 
                                  Lifespan +
                                  Sex +
                                  #BodyMass +
                                  #RightTarsus +
                                  BodymassTarsus +  
                                  NewRepStatus +
                                  InsectCounts +
                                  Tailmoult +
                                  (1 | BirdID) +
                                  (1 | BirthYear),        #Cohort
                                data = dat4)

lrtest(mixed.lmer_TotalBarbuleCount1, mixed.lmer_TotalBarbuleCount2, mixed.lmer_TotalBarbuleCount3, mixed.lmer_TotalBarbuleCount4)

#      Df  LogLik Df  Chisq Pr(>Chisq)    
#   1  15 -317.89                         
#   2  15 -320.51  0 5.2538  < 2.2e-16 *** >> use model 2
#   3  16 -315.59  1 9.8495   0.001699 ** 
#   4  15 -315.53 -1 0.1118   0.738063    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#check colinearity
vif(mixed.lmer_TotalBarbuleCount2) #> high for age, age squared and interactions again. 

#Plot to check assumptions
plot(mixed.lmer_TotalBarbuleCount2) #Fine!

#and qqplot
qqnorm(resid(mixed.lmer_TotalBarbuleCount2))
qqline(resid(mixed.lmer_TotalBarbuleCount2))  #Fine!


Anova(mixed.lmer_TotalBarbuleCount2)

## OUTPUT:
#
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: TotalBarbuleCount
#               Chisq Df Pr(>Chisq)  
# Age          0.6556  1    0.41811  
# I(Age^2)     0.1800  1    0.67140  
# Sex          2.7655  1    0.09632 .
# Lifespan     0.0906  1    0.76339  
# RightTarsus  0.2449  1    0.62065  
# NewRepStatus 3.5803  2    0.16694  
# InsectCounts 0.8577  1    0.35438  
# Tailmoult    0.1503  1    0.69823  
# Age:Sex      0.0040  1    0.94968  
# I(Age^2):Sex 0.0047  1    0.94517  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


mixed.lmer_TotalBarbuleCount2

## Model Output:
#Linear mixed model fit by REML ['lmerModLmerTest']
# Formula: TotalBarbuleCount ~ Age + I(Age^2) + Age * Sex + I(Age^2) * Sex +  
#   Lifespan + Sex + RightTarsus + NewRepStatus + InsectCounts +      Tailmoult + (1 | BirdID) + (1 | BirthYear)
# Data: dat4
# REML criterion at convergence: 641.0245
# Random effects:
#   Groups    Name        Std.Dev.
# BirdID    (Intercept) 11.26   
# BirthYear (Intercept) 10.99   
# Residual              20.93   
# Number of obs: 74, groups:  BirdID, 35; BirthYear, 13
# Fixed Effects:
#   (Intercept)              Age         I(Age^2)             SexM         Lifespan      RightTarsus  
#    676.506795         2.869285        -0.068745        17.495046        -0.001308         2.003702  
# NewRepStatusH  NewRepStatusSub     InsectCounts        Tailmoult         Age:SexM    I(Age^2):SexM  
#     11.282658        -9.360487        -1.884281         2.558174        -0.326766        -0.025872  
# fit warnings:
#   Some predictor variables are on very different scales: consider rescaling


summary(mixed.lmer_TotalBarbuleCount2)

# Model Summary Output: 
# 
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: TotalBarbuleCount ~ Age + I(Age^2) + Age * Sex + I(Age^2) * Sex +  
#   Lifespan + Sex + RightTarsus + NewRepStatus + InsectCounts +      Tailmoult + (1 | BirdID) + (1 | BirthYear)
# Data: dat4
# 
# REML criterion at convergence: 641
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -1.86991 -0.64622  0.04215  0.53377  1.89840 
# 
# Random effects:
#   Groups    Name        Variance Std.Dev.
# BirdID    (Intercept) 126.8    11.26   
# BirthYear (Intercept) 120.7    10.99   
# Residual              438.1    20.93   
# Number of obs: 74, groups:  BirdID, 35; BirthYear, 13
# 
# Fixed effects:
#                   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)     676.506795 100.117425  54.032253   6.757 1.02e-08 ***
# Age               2.869285   5.676153  59.370298   0.505    0.615    
# I(Age^2)         -0.068745   0.380450  52.223243  -0.181    0.857    
# SexM             17.495046  13.730179  43.291975   1.274    0.209    
# Lifespan         -0.001308   0.004347  36.246510  -0.301    0.765    
# RightTarsus       2.003702   4.048507  54.126740   0.495    0.623    
# NewRepStatusH    11.282658  14.876750  58.703646   0.758    0.451    
# NewRepStatusSub  -9.360487  11.572680  61.432310  -0.809    0.422    
# InsectCounts     -1.884281   2.034561  58.233373  -0.926    0.358    
# Tailmoult         2.558174   6.598257  57.419673   0.388    0.700    
# Age:SexM         -0.326766   5.177812  58.356535  -0.063    0.950    
# I(Age^2):SexM    -0.025872   0.376183  52.881002  -0.069    0.945    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) Age    I(Ag^2) SexM   Lifspn RghtTr NwRpSH NwRpSS InsctC Talmlt Ag:SxM
# Age         -0.168                                                                       
# I(Age^2)     0.181 -0.924                                                                
# SexM         0.335  0.554 -0.405                                                         
# Lifespan    -0.036 -0.080 -0.135  -0.235                                                 
# RightTarsus -0.981  0.029 -0.054  -0.430 -0.016                                          
# NewRepSttsH -0.022  0.596 -0.501   0.292  0.028 -0.098                                   
# NewRpSttsSb -0.044  0.574 -0.498   0.213  0.165 -0.089  0.657                            
# InsectConts -0.063  0.083 -0.057   0.033 -0.080 -0.027  0.096  0.114                     
# Tailmoult    0.089  0.159 -0.173   0.056 -0.117 -0.087 -0.039 -0.048 -0.201              
# Age:SexM     0.059 -0.829  0.780  -0.658  0.170  0.034 -0.282 -0.231 -0.044 -0.251       
# I(Ag^2):SxM -0.082  0.763 -0.828   0.489 -0.092  0.003  0.237  0.196  0.033  0.279 -0.934
# fit warnings:
#   Some predictor variables are on very different scales: consider rescaling



###=== Plotting ======================


### Took out insignificant interactions >> to make a linear regression plot
mixed.lmer_TotalBarbuleCount2 <- lmer(TotalBarbuleCount ~
                                      Age +
                                      #I(Age^2) +              #age squared
                                      #Age*Sex +               #interaction of age and sex
                                      #I(Age^2)*Sex + 
                                      Lifespan +
                                      Sex +
                                      #BodyMass +
                                      RightTarsus +
                                      #BodymassTarsus +  
                                      NewRepStatus +
                                      InsectCounts +
                                      Tailmoult +
                                      (1 | BirdID) +
                                      (1 | BirthYear),        #Cohort
                                    data = dat4)

### Plot overall predictions for the model: 

# Extract the prediction data frame
pred.mm3 <- ggpredict(mixed.lmer_TotalBarbuleCount2, terms = c("Age"))  # this gives overall predictions for the model

# Plot the predictions 
(ggplot(pred.mm3) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = dat4,                      # adding the raw data (scaled values)
               aes(x = Age, y = TotalBarbuleCount, colour = Sex)) + 
    labs(x = "Age", y = "Total BarbuleCount", 
         title = "Barbule Density over Age") + 
    theme_minimal()
)



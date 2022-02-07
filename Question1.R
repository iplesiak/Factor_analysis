## setting working directory 
rm(list = ls()) #clear workspace
setwd("~/Desktop/Marketing Masters/Statistical learning/Tutorials")

library (readr)
slim <- read_csv("~/Desktop/Marketing Masters/Statistical learning/group assignment/SLIMSustainableFashionData.csv")
summary(slim)

####################Checking for missing values in the data
colSums(is.na(slim))

#------------------------------------Question 1---------------------------------------#
#standardize the data set
slim.sc <- slim
# only column 11:34 because the other variables are demographics
slim.sc [, 11:34] <- data.frame(scale(slim[, 11:34]))
summary (slim.sc)

#subset lifestyle and perception
lifestyle <- data.frame (slim.sc [,11:23])
perception <- data.frame (slim.sc [, 24:34])

###subset lifestyle###
# KMO and Bartlett's test on lifestyle
kmo <- function(x)
  
{
  x <- subset(x, complete.cases(x))
  r <- cor(x)
  r2 <- r^2
  i <- solve(r)
  d <- diag(i)
  p2 <- (-i/sqrt(outer(d, d)))^2
  diag(r2) <- diag(p2) <- 0
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}

kmo(lifestyle)

bartlett.sphere<-function(data){chi.square=-( (dim(data)[1]-1) -
                                                (2*dim(data)[2]-5)/6 )*
  log(det(cor(data,use='pairwise.complete.obs')));cat('chi.square value ',
                                                      chi.square , ' on ', (dim(data)[2]^2-dim(data)[2])/2, ' degrees of freedom.'
                                                      , ' p-value: ', 1-pchisq(chi.square,(dim(data)[2]^2-dim(data)[2])/2))}
bartlett.sphere(lifestyle)

#decide on how many factors
library(nFactors)
nScree(lifestyle)
plotuScree(lifestyle, x = lifestyle, model = "components",
           ylab = "Eigenvalues", xlab = "Components", main = "Scree Plot for 'lifestyle'")
# says 4 is best, we will check from 4-1 to 4+1
eigen(cor(lifestyle))
factanal(lifestyle, factors=3)
# 3 is ok, but 4 is better (chi-square? And p-value?)
factanal(lifestyle, factors=4)
# 4 is good
factanal(lifestyle, factors=5)
# 5 is too much

#rotation needed?
library(GPArotation)
# before rotation, hard to interpret
(lifestyle.fac.ro <- factanal(slim.sc[, 11:23], factors=4, rotation="none", scores = "Bartlett"))
# oblimin rotation (we think they're related, knowledge vs ignorance)
(lifestyle.fac.ro <- factanal(slim.sc[, 11:23], factors=4, rotation="oblimin", scores = "Bartlett"))
# orthogonal rotation (assignment asked for orthogonal so we're using this)
(lifestyle.fac.ro <- factanal(slim.sc[, 11:23], factors=4, rotation = "varimax", scores = "Bartlett"))

#decided on 4 factors with orthogonal rotation
(lifestyle.fac.ro <- factanal(slim.sc[, 11:23], factors=4, rotation = "varimax", scores = "Bartlett"))
View(slim.sc)

#visualization
library(ggplot2)
library(RColorBrewer)
heatmap.2(lifestyle.fac.ro$loadings, 
          col=brewer.pal(9, "Blues"), trace="none", key=FALSE, dend="none",
          Colv=FALSE, cexCol = 1.2,
          main="\n\n\n\n\nFactor loadings for lifestyle")


###perception subset###
# KMO and Bartlett's test on perception
kmo <- function(x)
  
{
  x <- subset(x, complete.cases(x))
  r <- cor(x)
  r2 <- r^2
  i <- solve(r)
  d <- diag(i)
  p2 <- (-i/sqrt(outer(d, d)))^2
  diag(r2) <- diag(p2) <- 0
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}

kmo(perception)

bartlett.sphere<-function(data){chi.square=-( (dim(data)[1]-1) -
                                                (2*dim(data)[2]-5)/6 )*
  log(det(cor(data,use='pairwise.complete.obs')));cat('chi.square value ',
                                                      chi.square , ' on ', (dim(data)[2]^2-dim(data)[2])/2, ' degrees of freedom.'
                                                      , ' p-value: ', 1-pchisq(chi.square,(dim(data)[2]^2-dim(data)[2])/2))}
bartlett.sphere(perception)


#decide on how many factors
library(nFactors)
nScree(perception)
plotuScree(perception, x = perception, model = "components",
           ylab = "Eigenvalues", xlab = "Components", main = "Scree Plot for 'perception'")
# says 3 is the best, we will check 3-1 to 3+1
eigen(cor(perception))
factanal(perception, factors=2)
# 2 is sufficient, high chi sq, but low p-value, so we don't want 2 factors
factanal(perception, factors=3)
# 3 is sufficient, chi sq = 27.25
factanal(perception, factors=4)
# 4 is too much, chi sq = 6.45, lower than when 3 factors so we use 3.

#rotation needed?
library(GPArotation)
# before rotation, doesn't make sense with the reverse questions being put with
# non-reverse questions -> hard to interpret
(perc.fac.ro <- factanal(slim.sc[, 24:34], factors=3, rotation="none"))
# orthogonal rotation to interpret better
(perc.fac.ro <- factanal(slim.sc[, 24:34], factors=3, rotation = "varimax"))

#decided on 3 factors with orthogonal rotation
(perc.fac.ro <- factanal(slim.sc[, 24:34], factors=3, rotation = "varimax", scores = "Bartlett"))

#visualization
heatmap.2(perc.fac.ro $loadings, 
          col=brewer.pal(9, "YlOrRd"), trace="none", key=FALSE, dend="none",
          Colv=FALSE, cexCol = 1.2,
          main="\n\n\n\n\nFactor loadings for perception")

### scores on age ###
#age on lifestyle
lifestyle.vec <- c(factanal(slim.sc[,11:23],factors=4, rotation="varimax",scores="Bartlett"))
lifestyle.score <- data.frame(lifestyle.vec$scores)
lifestyle.score$AgeCategory <- slim.sc$AgeCategory
head(lifestyle.score)
lifestyle.score$AgeCategory[lifestyle.score$AgeCategory 
                            %in% c("1", "2", "3", "4", "5", "6")] <- c("<20", "21-30", "31-40", "41-50", "51-60", "61+")
lifestyle.score_mean <- aggregate(.~AgeCategory, data=lifestyle.score, mean)
rownames(lifestyle.score_mean) <- lifestyle.score_mean [, 1]
lifestyle.score_mean <- lifestyle.score_mean [, -1]
names(lifestyle.score_mean) <- c ("Shopaholic","Taste", "Status", "Functionality")
lifestyle.score_mean


#visualization
library("RColorBrewer")
library(gplots)
library(brew)
heatmap.2(as.matrix(lifestyle.score_mean), 
          col=brewer.pal(9, "Reds"), 
          Rowv = cbind("<20", "21-30", "31-40","41-50","51-60","60+"), 
          trace="none", key=FALSE, dend="none",margins=c(13,10),
          cexCol=1.5, main="\n\n\n\n\nLifestyle factor scores by age groups")

### scores on education level ###
#education on lifestyle
lifestyle.vece <- c(factanal(slim.sc[,11:23],factors=4, rotation="varimax",scores="Bartlett"))
lifestyle.scoree <- data.frame(lifestyle.vec$scores)
lifestyle.scoree$Education <- slim.sc$Education
head(lifestyle.scoree)
lifestyle.scoree$Education[lifestyle.scoree$Education
                           %in% c("1", "2", "3", "4")] <- c("Highschool", "MBO", "HBO", "UNI")
lifestyle.scoree_mean <- aggregate(.~Education, data=lifestyle.scoree, mean)
rownames(lifestyle.scoree_mean) <- lifestyle.scoree_mean [, 1]
lifestyle.scoree_mean <- lifestyle.scoree_mean [, -1]
names(lifestyle.scoree_mean) <- c ("Shopaholic","Taste", "Status", "Functionality")
lifestyle.scoree_mean

heatmap.2(as.matrix(lifestyle.scoree_mean), 
          col=brewer.pal(9, "Purples"), 
          Rowv = cbind("Highschool", "MBO", "HBO", "UNI"), 
          trace="none", key=FALSE, dend="none",margins=c(13,10),
          cexCol=1.5, main="\n\n\n\n\nLifestyle factor scores by education levels")

#age on perception
perc.vec<- c(factanal(slim.sc[,24:34],factors=3, rotation="varimax", scores="Bartlett"))
perc.score <- data.frame(perc.vec$scores)
perc.score$AgeCategory <- slim.sc$AgeCategory
head(perc.score)
perc.score$AgeCategory[perc.score$AgeCategory 
                       %in% c("1", "2", "3", "4", "5", "6")] <- c("<20", "21-30", "31-40", "41-50", "51-60", "61+")
perc.score_mean <- aggregate(.~AgeCategory, data=perc.score, mean)
rownames(perc.score_mean) <- perc.score_mean [, 1]
perc.score_mean <- perc.score_mean [, -1]
names(perc.score_mean) <- c ("Awareness", "Ignorance", "Willingness to buy")
perc.score_mean

#visualization
library("RColorBrewer")
library(gplots)
library(brew)
heatmap.2(as.matrix(perc.score_mean), 
          col=brewer.pal(9, "Blues"), Rowv = 6, trace="none", key=FALSE, dend="none",margins=c(13,10),
          cexCol=1.5, main="\n\n\n\n\nPerception factor scores by age groups")



#education on perception
perc.vece<- c(factanal(slim.sc[,24:34],factors=3, rotation="varimax", scores="Bartlett"))
perc.scoree <- data.frame(perc.vece$scores)
perc.scoree$Education <- slim.sc$Education
head(perc.scoree)
perc.scoree$Education[perc.scoree$Education 
                      %in% c("1", "2", "3", "4")] <- c("Highschool", "MBO", "HBO", "UNI")
perc.scoree_mean <- aggregate(.~Education, data=perc.scoree, mean)
rownames(perc.scoree_mean) <- perc.scoree_mean [, 1]
perc.scoree_mean <- perc.scoree_mean [, -1]
names(perc.scoree_mean) <- c ("Awareness", "Ignorance", "Willingness to buy")
perc.scoree_mean

#visualization
library("RColorBrewer")
library(gplots)
library(brew)
heatmap.2(as.matrix(perc.scoree_mean), 
          col=brewer.pal(9, "RdPu"), Rowv = 6, trace="none", key=FALSE, dend="none",margins=c(13,10),
          cexCol=1.5, main="\n\n\n\n\nPerception factor scores by education levels")

#------------------------------------Question 2---------------------------------------#
#making variables
intention <- data.frame (slim.sc [, 35])
willingness <- data.frame (slim.sc [, 37])
education <- data.frame (slim.sc [, 4])
purchasefreq <- data.frame (slim.sc [, 7])


#Education level dummies, m-1, benchmark is lowest level education i.e. high school
Lmbo <- ifelse(education == '2', 1, 0)
Lhbo <- ifelse(education == '3', 1, 0)
Luni <- ifelse(education == '4', 1, 0)

#vectorizing the variables
intention1 <- unlist(intention)
willingness1 <- unlist(willingness)
purchasefreq1 <- unlist(purchasefreq)

###ANOVA###
#check full models vs. reduced models
#full model
mod1 <-lm(intention1~Lmbo + Lhbo + Luni, data=slim.sc)
summary(mod1)
#reduced model
moda <-lm(intention1~1, data=slim.sc)
summary(moda)

#full model
mod2 <-lm(willingness1 ~Lmbo + Lhbo + Luni, data=slim.sc)
summary(mod2)
#reduced model
modb <-lm(willingness1~1, data=slim.sc)
summary(modb)

#Compare full with reduced (1 & 2 = full, a & b = reduced)
anova(mod1, moda)
anova(mod2, modb)
#Both model comparisons have higher p-values than 5%, reduced model is better
#no differences across education levels

#Then check homogeneity or else can't run ANCOVA
###Homogeneity test###
modhom1 <- lm (intention1~purchasefreq1 + Lmbo +Lhbo +Luni , data=slim.sc)
modhom2 <-lm(intention1~purchasefreq1 + Lmbo + Lhbo + Luni + Lmbo*purchasefreq1+ Lhbo*purchasefreq1+ Luni*purchasefreq1, data=slim.sc)
anova(modhom1, modhom2)
#High p-value so cannot reject, homogeneity is respected


modhom3 <- lm (willingness1~purchasefreq1 + Lmbo +Lhbo +Luni , data=slim.sc)
modhom4 <-lm(willingness1~purchasefreq1 + Lmbo + Lhbo + Luni +Lmbo*purchasefreq1+ Lhbo*purchasefreq1+ Luni*purchasefreq1, data=slim.sc)
anova(modhom3, modhom4)
#High p-value so cannot reject, homogeneity is respected

###ANCOVA###
#check full models vs. reduced models
#full model
mod4 <-lm(intention1~purchasefreq1 + Lmbo + Lhbo + Luni, data=slim.sc)
summary(mod4)
#reduced model
mod3 <- lm (intention1~purchasefreq1, data=slim.sc)
summary(mod3)

#full model
mod6 <-lm(willingness1~purchasefreq1 + Lmbo + Lhbo + Luni, data=slim.sc)
summary(mod6)
#reduced
mod5 <- lm (willingness1~purchasefreq1, data=slim.sc)
summary(mod5)

#Compare full with reduced (4 & 6 = full, 3 & 5 = reduced)
anova(mod4, mod3)
#lower p-values than 5%, full model is better 
#differences across education levels for intention
anova(mod6, mod5)
#higher p-value than 5%, reduced model is better
#no differences across education level for willingness



#------------------------------------Question 3---------------------------------------#
lifestyle.fac <- factanal(slim.sc[, 11:23], factors = 4, rotation = "varimax",
                          scores = "Bartlett")
perc.fac <- factanal(slim.sc[, 24:34], factors = 3, rotation = "varimax",
                     scores = "Bartlett")
#making variables
Life <- data.frame (lifestyle.fac$scores)
Perc <- data.frame (perc.fac$scores)
shop1 <- data.frame (Life[,1])
taste1<- data.frame (Life[,2])
status1 <- data.frame(Life[,3])
func1 <- data.frame(Life[,4])
awa1 <- data.frame(Perc[,1])
igno1 <- data.frame(Perc[,2])
w.tobuy1 <- data.frame (Perc[,3])

shop <- unlist(shop1)
taste <- unlist(taste1)
status <- unlist(status1)
func <- unlist(func1)
awa <- unlist(awa1)
igno <- unlist(igno1)
w.tobuy <- unlist (w.tobuy1)
intention1 <- unlist (intention)
willingness1 <- unlist (willingness)
Gender <- data.frame(slim [, 1])
Female <- ifelse(Gender == 1, 1, 0)

#standardize the variables
factors <- as.data.frame(cbind(shop,taste, status,func,awa,igno,w.tobuy,intention1, willingness1, Female))
factors.sc <- data.frame(scale(factors))
summary (factors.sc)
### Regression ###
#building regression model for intention
m1intention <- lm(intention1~ shop + taste + status + func + awa + igno + w.tobuy + Female, data=factors.sc) 
summary(m1intention)
# status and gender female are the most important variables when looking at purchase intention
#status and female have a strong correlation -> therefore multicollinearity test

#Testing for multicollinearity
library(car)
#create vector of VIF values and a vector of tolerance values
vif_values <- vif(m1intention)
tolerance <- 1/vif_values
# the closer to 1, the lower the level of multicollinearity
vif_values
#The closer to 0, the higher the level of multicollinearity
tolerance


#horizontal bar chart to look at each VIF value 
barplot(vif_values, main = "VIF Values on Intention", horiz = TRUE, xlim = c(0,12), col = "red")
#add vertical line at 4 and 10
abline(v = 4, lwd = 3, lty = 2)
#We don't add abline 10 because we all VIFs are below 4 and make it
#harder to see the values
#no multicollinearity 
#status has the strongest impact on intention to purchase


#building regression model for willingness
m1will <- lm(willingness1~shop + taste + status + func + awa + igno + w.tobuy + Female, data=factors.sc) 
summary(m1will)
#status is the only significant variable

#Testing for multicollinearity
#create vector of VIF values and a vector of tolerance values
vif_values <- vif(m1will)
tolerance <- 1/vif_values
# the closer to 1, the lower the level of multicollinearity
vif_values
#The closer to 0, the higher the level of multicollinearity
tolerance

#horizontal bar chart to look at each VIF value
barplot(vif_values, main = "VIF Values on Willingness", horiz = TRUE, xlim = c(0,5), col = "red")
#add vertical line at 4 
abline(v = 4, lwd = 3, lty = 2)
#no multicollinearity 
#status has the strongest impact on willingness to recommend

#------------------------------------Question 4---------------------------------------#
#mean centering of money spent needed because we need to rescale the variable
center_scale <- function(x){scale(x,scale = FALSE)}
moneyspent1 <- data.frame (slim.sc[,8])
moneyspent.csc1 <- center_scale(moneyspent1)
moneyspent.csc <- unlist(moneyspent.csc1)

##intention (status)  and money spent##
# Has money spent a moderating effect on the most important factor of intention -> status?
#full model
m2mod.status <-lm(intention1~ 
                    shop + taste + status + func + awa + igno + w.tobuy + Female + 
                    moneyspent.csc + status*moneyspent.csc, data=factors)
summary(m2mod.status)

#Testing for multicollinearity
#create vector of VIF values and a vector of tolerance values
vif_values <- vif(m2mod.status)
tolerance <- 1/vif_values
# the closer to 1, the lower the level of multicollinearity
vif_values
#The closer to 0, the higher the level of multicollinearity
tolerance

#horizontal bar chart to look at each VIF value
barplot(vif_values, main = "VIF Values moderation on status", horiz = TRUE, xlim = c(0,5), col = "orchid3")

#add vertical line at 4 (and not 10 because it's much lower and not necessary)
abline(v = 4, lwd = 3, lty = 2)
#no multicollinearity


##willingness(status) and education level##
# Has education level a moderating effect on the most important factor of willingness -> status?
Highschool <- ifelse(education == '1', 1, 0)
MBO <- ifelse(education == '2', 1, 0)
HBO <- ifelse(education == '3', 1, 0)

#full model
m2mod.statusedu <-lm(willingness1~ shop + taste + status + func + awa + igno + w.tobuy + Highschool + MBO + HBO + Highschool*status + MBO*status + HBO*status, data=factors)
summary(m2mod.statusedu)
# 0.06 p-value, so if we use 90% confidence intervals then it is significant
# Depends on our levels, for simplicity we will reject H0 and use p-value (10%)

#Testing for multicollinearity
#create vector of VIF values and a vector of tolerance values
vif_values <- vif(m2mod.statusedu)
tolerance <- 1/vif_values
# the closer to 1, the lower the level of multicollinearity
vif_values
#The closer to 0, the higher the level of multicollinearity
tolerance

#horizontal bar chart to look at each VIF value
barplot(vif_values, main = "VIF Values moderation on status2", horiz = TRUE, xlim = c(0,5), col = "slate blue")

#add vertical line at 4 
abline(v = 4, lwd = 3, lty = 2)

# Final coefficients
Status <-summary(m2mod.statusedu)$coefficients[4,1]
Status_MBO <-summary(m2mod.statusedu)$coefficients[13,1]
edu <-cbind(status,Status_MBO)
barplot(edu, main = "Status customers with MBO degree", ylim = c(-1,1), col = "darkseagreen3")
abline(h = 0.0, lwd = 3, lty = 2)

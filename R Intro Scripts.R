
# Choosing your directory

getwd() #gets current R directory
setwd("C:/Users/My Folder/My other folder/... bla bla") #Set your directory
setwd("C:/Users/eucli/Google Drive/Docência/Introduction to R/") #Set your directory


#Basic ops
1+1
4/2
4^2
4*2
4-5
4.5-2.1
sqrt(4) #square root
1+2+2*(2+2)


# Assining objects
x <-1+4
x+1
y <-10
y
class(y) # how to know the class of an object
y+x
y<x
y==x
y!=x

heights <- c(169,178,169,175) #Creating variable
heights <- c(169,178,169,175, "Barb") # The heights object is coerced to factor type
mean(heights)
heights <- heights[-4] #deleting fourth element
heights <- as.numeric(heights)
mean(heights)

#Basics of functions

# Fancy BMI function

BMI <- function(height, weight){
BMI <- weight /  height^2
  cat("Yor BMI is", BMI, if(BMI>=30){"Obesity level 1"}else{"Under typical threshold"})
    } 

BMI(height = 1.81, weight = 60)


#### Reading datasets ####

install.packages("foreign") # Package used for reading SPSS
library(foreign) # Loading the package for use

height <- read.spss("Heights (R Workshop).sav", to.data.frame = T)


# Subsetting
heightF <- height[height$gender=="feminino",] # Creates a dataset with only females

# Creating var with english labels
height$genderEnglish <-  factor(height$gender, labels = c("Female","Male")) #Changing the labels


# adding new case to heights dataset
Barb <- data.frame("Barbara", 159,178, 165,"feminino", 0 , "Female" )
names(Barb) <- names(heights)

heights <- rbind(heights, Barb) #rbind and cbind are quite useful for merging datasets
View(heights)

# Fitting linear regression
library(psych)
describeBy(x =  height$height,group = height$genderEnglish)

t.test(height$height ~ height$genderEnglish, var.equal=TRUE)

fitHeight <-  lm(height$height ~ height$genderEnglish)
summary(fitHeight)

fitHeight2 <- lm(height$height ~ height$genderEnglish + height$mom_height)
summary(fitHeight2)

anova(fitHeight, fitHeight2)

fitHeight <- lm(heights$height ~ heights$dad_height) #Linear Regression

summary(fitHeight) # Results of Linear Regression

fitHeight2 <- lm(heights$height ~ heights$mom_height + heights$dad_height) # Linear regression wiht 2 predictors
summary(fitHeight2)

anova(fitHeight, fitHeight2) #comparing regression models

# Correlation of height, dad's height and mom's height
cor(heights[,c("height", "dad_height","mom_height")])
cor(heights[,2:4]) # same thing

# Make random weight observations normally distributed with mean 75, SD 10
rnorm()
set.seed(1) # For reproduction of same results, the rnorm will randomly generate the same results
heights$weight <- rnorm(n = 101,mean = 75,sd = 10 )

#Histogram
hist(heights$height, main="Plot Title", xlab="Label of X (Height)", ylab = "Label of Y (Frequency)") 

# Plot of moms'   height and participants' height
plot(x = heights$mom_height, y = heights$height, col=heights$genderEnglish)
abline(lm(heights$height ~heights$mom_height), col="red") #add regression line
abline(h = mean(heights$height)) #add horizontal line with the mean of height

# Boxplot
plot(heights$height ~heights$genderEnglish)

# Count barplot
barplot(table(heights$genderEnglish)) 

# Proportion barplot
barplot(prop.table(table(heights$genderEnglish))) 

# Percentage barplot
barplot(prop.table(table(heights$genderEnglish))*100) 







#### Child dev ####
library(foreign)
CDev <- read.spss("Child Dev 2 (R Workshop).sav", to.data.frame = TRUE,use.value.labels = T)
CDev <- CDev[complete.cases(CDev),]
levels(CDev$NormsBands)

AgeQuad <- CDev$childAge^2
FitCog <- lm(CDev$COG_Dev ~  CDev$childAge + AgeQuad)
plot(CDev$COG_Dev ~  CDev$childAge)

lines(CDev$childAge, y=predict.lm(FitCog), col = "blue")


#### Sencond Day ####

#Child Development data

# Reading SPSS
library(foreign)
CDev <- read.spss("Child Dev 2 (R Workshop).sav",to.data.frame = TRUE)

complete.cases(CDev)
names(CDev)
complete.cases(CDev[,c("COG_Dev","Exp_Language_Dev",
                       "Socioemotional_Dev","childAge")])

CDevClean <- CDev[complete.cases(CDev),]  # dropping cases in all variables with missing

summary(CDevClean)

plot(x = CDevClean$childAge, 
     y = CDevClean$COG_Dev, col=CDevClean$diagnosis37) #plot of age and cog dev

table(CDevClean$diagnosis37)

# Fancy Graphics
#using GGPLOT2

install.packages("ggplot2", dependencies = FALSE)

library(ggplot2)

# Using GGPLOT

ggplot(data = CDevClean, aes(y =COG_Dev, x = childAge,group =  childSex21,col=childSex21))+
  geom_smooth(method = lm)+geom_point()

# Fitting separate regression as function of sex

CdevFem <- CDevClean[CDevClean$childSex21=="Female",]
CdevMale <- CDevClean[CDevClean$childSex21=="Male",]

fitCogDevFem <- lm(CdevFem$COG_Dev ~ CdevFem$childAge)
summary(fitCogDevFem)

fitCogDevMale <- lm(CdevMale$COG_Dev ~ CdevMale$childAge)
summary(fitCogDevMale)


ggplot(data = heights,
       aes(x = mom_height, y=height,  group=genderEnglish, shape=genderEnglish,col=genderEnglish))+
  geom_point()+geom_smooth(method = lm)

fitHeightMOD <- lm(heights$height ~ heights$genderEnglish*heights$mom_height) # Regression with main effects and interaction
summary(fitHeightMOD)

ggplot(data = CDevClean, 
       aes(y =COG_Dev, x = childAge,
           group =  Age_bandsOrdered,col=Age_bandsOrdered))+
  geom_smooth(method = lm)+geom_point()

fitCogDev2 <- lm(CDevClean$COG_Dev ~ CDevClean$childAge*CDevClean$Age_bandsOrdered)
summary(fitCogDev2)

# DRD$ Data set

drd4 <- read.spss("Douglas_research_day_dataset.sav", to.data.frame = TRUE)
names(drd4)

fitEE <- lm(CEBQ_emo_over_eat_score.48m ~ PrediXCan_DRD4_PFC_Quat*B_index_postnatal, data=drd4)
summary(fitEE)

quantile(drd4$PrediXCan_DRD4_PFC)

library(car)
drd4$PrediXCan_DRD4_PFC_Quat <- recode(drd4$PrediXCan_DRD4_PFC,
                                       "-0.68:-0.3= 'First'; -0.301:-0.06800309=NA;-0.069:0.03672681=NA;0.037:0.17745019='Fourth' ") # selecting only 1st and 4th quartile

ggplot(drd4,aes(x=B_index_postnatal,y = CEBQ_emo_over_eat_score.48m, group= PrediXCan_DRD4_PFC_Quat,col=PrediXCan_DRD4_PFC_Quat))+
  geom_line(method=lm)

pred <- predict.lm(fitEE)  



table(drd4$PrediXCan_DRD4_PFC_Quat) # frequencies










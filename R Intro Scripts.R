

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

# Instaling packages
install.packages("foreign")
install.packages("car")
install.packages("psych")

# Loading it
library(foreign) # Reads SPSS files
library(car)
library(psych)

#### Reading datasets ####

height <- read.spss("https://github.com/euclidesmendonca/McGillIntroR/raw/master/Heights%20(R%20Workshop).sav", 
                    to.data.frame = T)

CDev <- read.spss("https://github.com/euclidesmendonca/McGillIntroR/raw/master/Child%20Dev%20(R%20Workshop).sav", 
                    to.data.frame = T)

# Subsetting
heightF <- height[height$gender=="feminino",] # Creates a dataset with only females

# Creating var with english labels

height$genderEnglish <-  factor(height$gender, labels = c("Female","Male")) #Changing the labels

describeBy(x =  height$height,group = height$gender)

t.test(height$height ~ height$genderEnglish, var.equal=TRUE)

fitHeight <-  lm(height$height ~ height$gender)
summary(fitHeight)

# Fitting linear regression

fitHeight2 <- lm(height$height ~ height$genderEnglish + height$mom_height)
summary(fitHeight2)

anova(fitHeight, fitHeight2)

fitHeight <- lm(height$height ~ height$dad_height) #Linear Regression

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

AgeQuad <- CDev$childAge^2
FitCog <- lm(CDev$COG_Dev ~  CDev$childAge + AgeQuad)
plot(CDev$COG_Dev ~  CDev$childAge)

lines(CDev$childAge, y=predict.lm(FitCog), col = "blue")


#### Sencond Day ####

#Child Development data

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



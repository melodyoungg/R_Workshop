setwd("~/Dropbox/Applications, Forms, Awards, etc/Workshops/R_Bootcamp")

## using #(hashtags) allows you to comment code 

# downloading and loading necessary packages to run this code 
list.of.packages <- c("lme4", "lmerTest", "ggplot2", "dplyr", "ggpubr", "multcomp", "car") #place packages you need in quotes here
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, library, character.only = TRUE))

my_data = read.csv('GRIPSTRENGTH_Rbootcamp.csv') #reading the data file, remember to convert to a .csv file 
#my_data = read.csv(file.choose()) #alternatively, you can also open a file explorer to select your file 

#sometimes your datafiles are not read into R correctly, meaning continuous variables may be read 
#as categorical and vice versa, you can fix this by specifying in code to convert categorical 
#values into categorical values using the function as.factor()
#use $ to call a specific column of data 

my_data$Group = as.factor(my_data$Group) 
my_data$Gender = as.factor(my_data$Gender)

#### Exercise: please convert the remaining categorical values using as.factor() function 
my_data$SIDEDNESS = as.factor(my_data$SIDEDNESS)
my_data$Handedness = as.factor(my_data$Handedness)

#numerical (continuous variables) can be converted into continuous using as.numeric function
#### Exercise: please convert the continuous values into continous below following convention from above 
my_data$Weight..kg. = as.numeric(my_data$Weight..kg.)
my_data$AVG_GRIP = as.numeric(my_data$AVG_GRIP)
my_data$Total_AVG_GRIP = as.numeric(my_data$Total_AVG_GRIP)

## DATA VISUALIZATION ## 

# first we will create a boxplot of TOTAL_AVG_GRIP split by Gender

ggplot(my_data, aes(x=Gender, y=Total_AVG_GRIP)) + 
  geom_boxplot(alpha=0.2)

#adding aesthetics to ggplot is easy, follow documentation online: https://ggplot2.tidyverse.org/

## Ex: adding titles and labels 

ggplot(my_data, aes(x=Gender, y=Total_AVG_GRIP)) + 
  geom_boxplot(alpha=0.2) + labs(title = 'Body weight by Sex') +
  xlab('Sex') + ylab('Total Grip Force (%BW)')

## Ex: add color /fill 

ggplot(my_data, aes(x=Gender, y=Total_AVG_GRIP, color = Gender, fill = Gender)) + 
  geom_boxplot(alpha=0.2) + labs(title = 'Grip Force by Sex') +
  xlab('Sex') + ylab('Total Grip Force (%BW)')

## Ex: adding jitter (a lot of journals ask for this now)

ggplot(my_data, aes(x=Gender, y=Total_AVG_GRIP, color = Gender, fill = Gender)) +
  geom_point(alpha = 0.3, position = position_jitterdodge()) +
  geom_boxplot(alpha=0.6, outlier.shape = NA) + labs(title = 'Grip Force by Sex') +
  xlab('Sex') + ylab('Total Grip Force (%BW)')

#### Exercise: Create a boxplot of AVG_GRIP by gender and fill/color by sidedness 

ggplot(my_data, aes(x=Gender, y=AVG_GRIP, color = SIDEDNESS, fill = SIDEDNESS)) + 
  geom_boxplot(alpha=0.6, outlier.shape = NA) +  labs(title = 'Grip Force by Sex') +
  xlab('Sex') + ylab('Total Grip Force (%BW)')

#### Exercise: Create a boxplot of Total_AVG_GRIP by Group and fill/color by sidedness ADDING jitter

ggplot(my_data, aes(x=Group, y=AVG_GRIP, color = SIDEDNESS, fill = SIDEDNESS)) + 
  geom_point(alpha = 0.3, position = position_jitterdodge()) +
  geom_boxplot(alpha=0.6, outlier.shape = NA) +  labs(title = 'Grip Force by Sex') +
  xlab('Sex') + ylab('Total Grip Force (%BW)')

#you can use ggsave(filename.ext, Width=, Height =) to save the file 
#ex:

ggsave('testing.pdf', width = 5, height = 7)

## you can use the following: '+ coord_cartesian(ylim = c(-25,100))' exchanging ylim/xlim to set the coordinates
## or bounds of the graph 

# now we will create a correlation between weight and Total_AVG_GRIP 
ggplot(my_data, aes(x=Age, y=AVG_GRIP)) +
  geom_point(color = 'blue')

# now we will add a regression line 
ggplot(my_data, aes(x=Age, y=AVG_GRIP)) +
  geom_smooth(method=lm, se= FALSE) +
  geom_point(color = 'blue') 

#the method=lm, can be altered to do different types of curves (parabolic, logistic)...
#see documentation online for more info 

#### Exercise: Create regression using same code from above but color/fill by Gender 
#and use geom_smooth(method=lm,formula = y ~ poly(x, 2), se= FALSE) to add a parabolic line 

ggplot(my_data, aes(x=Age, y=AVG_GRIP, fill=Gender, color = Gender)) +
  geom_smooth(method=lm,formula = y ~ poly(x, 2), se= FALSE) +
  geom_point(position = position_jitterdodge())


## CREATING DATAFRAMES ## 
#many times you will need to subset your data to look at the nuanced nature of certain
#relationships or for graphical purposes, for example I might be curious to about data
#purely within the adults 

adults <- filter(my_data, Group == 'ADULT')

#### Exercise: create a dataframe of just males and then just females 

male <- filter(my_data, Gender == 'M')
female <- filter(my_data, Gender == 'F')

#once data frames are made you can graph just as we did above just changing out 
#the dataset the graph is pulling from 

#for example: 

ggplot(adults, aes(x=Age, y=AVG_GRIP, fill=Gender, color = Gender)) +
  geom_smooth(method=lm,formula = y ~ poly(x, 2), se= FALSE) +
  geom_point(position = position_jitterdodge())

## STATISTICAL TESTING ## 

#normality 
# if p value is > 0.5 your data is normally distributed 
shapiro.test(my_data$AVG_GRIP)
#### Exercise: run normality test on Total_AVG_GRIP
shapiro.test(my_data$Total_AVG_GRIP)

#### Exercise: create ranked version of Total_AVG_GRIP 
my_data$RTotal_AVG_GRIP <- rank(my_data$Total_AVG_GRIP)

#running t tests 
#(IF YOUR DATA SETS ARE DIFFERENT SIZES) 
#you must make sure your datasets pass test of homoscedasticity 
#essentially we want to know if the variances between the data are 
#homogeneous using the Fisher's F-test

var.test(male$Total_AVG_GRIP,female$Total_AVG_GRIP) #run this line of code to test 

# if p > 0.05 both samples are homogeneous, and you run a classic Student's 
#two sample t-test and set var.equal = TRUE 

# if p < 0.05 samples are NOT homogeneous, and you run a Welsch t test instead 
# and set var.equal = FALSE

t.test(male$Total_AVG_GRIP,female$Total_AVG_GRIP,var.equal = FALSE)

#### Exercise: run a t-test to assess differences in Total_AVG_GRIP between
####            adults and high school students

#first create dataframes
hs <- filter(my_data, Group == 'HIGHSCHOOL')
var.test(hs$Total_AVG_GRIP, adults$Total_AVG_GRIP)

t.test(adults$Total_AVG_GRIP,hs$Total_AVG_GRIP,var.equal = TRUE)

#running anovas 
# you run anovas when you are comparing more than one group
# assumes data are 
# A. distributed normally and
# B. variances of groups are equal 

#A. Normally distributed data: test for this using a Shapiro-Wilk 
#if p > 0.05 data is normallly distributed 
#if p < 0.05 data is NOT normally distributed 
#if your sample size is n>30 normality is NOT required (central limit theorem)
shapiro.test(my_data$Total_AVG_GRIP)

#B. Variances equal 
#this it heteroscedasticity, and can be tested using Levene's test
leveneTest(my_data$Total_AVG_GRIP~my_data$Group)

#if p > 0.05 variances in data are equal
#if p < 0.05 variances in data are NOT equal
#in this case, you can run a Welch ANOVA (data still must be normally distributed)
Welch <- oneway.test(my_data$Total_AVG_GRIP~my_data$Group, var.equal = FALSE)
#there are probably ways to run post hocs on Welch ANOVA, please research yourself

#Another way to achieve normality/equal variances is to either log transform
#or rank transform data 

my_data$RTotal_AVG_GRIP <- rank(my_data$Total_AVG_GRIP)
#linear mixed effect models 
#these are really conservative controlling for everything stats 

lmm <- lmer(RTotal_AVG_GRIP~Group+Handedness+Age + (1|Individual), data = my_data,REML=FALSE)
summary(lmm)
anova(lmm) #this will tell you a difference exists 

summary(glht(lmm, linfct = mcp(Group = "Tukey")), test = adjusted("holm")) ## this is for post hoc tukeys, change the Group to your
#variable fo interests 
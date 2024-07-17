######################
# Downloading the data
######################

# Data source
url<- "https://ftp.cs.wisc.edu/math-prog/cpo-dataset/machine-learn/cancer/WDBC/WDBC.dat"

# Checking if headers are present
readLines(url)[1:5] # Base R function reading the first five lines
# Headers (variable name is not present) and the values are comma separated

# read.csv is part of base R and used to read in comma separated files
dat<- read.csv(url, header = F)

dim(dat) # data dimension (first values is the number of ros and second the number of columns)

remove(url)

#######################
# Assign variable names
#######################

# Information on the variables is avialble in the link below (WDBC.doc)
# https://ftp.cs.wisc.edu/math-prog/cpo-dataset/machine-learn/cancer/WDBC/

# Standard Error (SE) is a measure of variability among the features and does
# make clinical sense to be used as predictors and will be removed from
# the data

# column 13 to 22 contains the SE of the 10 features

dat<- dat[, -c(13:22)] # removing the variables. Retaining all rows- removing column



header_names<- c("ID", "Diagnosis", "radius_mean", "texture_mean", "perimeter_mean",
                 "area_mean", "smoothness_mean", "compactness_mean",
                 "concavity_mean", "concave_points_mean", "symmetry_mean",
                 "fractal_dimension_mean", "radius_worst", "texture_worst", "perimeter_worst",
                 "area_worst", "smoothness_worst", "compactness_worst",
                 "concavity_worst", "concave_points_worst", "symmetry_worst",
                 "fractal_dimension_worst")  

names(dat)<- header_names

remove(header_names)

#############################
# Loading required libraries
#############################
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

# The above lines of codes downloads and loads the libraries (if not installed already)
# The first four packages are useful for data wangling and plots
# The last three has function for Machine learning algorithms

##########################
# Exploring variable class
##########################

glimpse(dat) # a function of dplyr package
# All the predictor variables are numeric (dbl)
# The variable to be predicted is character (chr). It needs to be converted
# into factor for model development

table(dat$Diagnosis)
dat$Diagnosis<- factor(dat$Diagnosis, levels = c("M", "B"), # order of the levels do not matter
                       labels = c("Malignant", "Benign"))

##############################################################################
# Exploring overall distribution of prediction variable and splitting the data
##############################################################################

# Distribution
table(dat$Diagnosis)
prop.table(table(dat$Diagnosis))

# 212 (37.3%) patients have malignancy and 357 (62.7%) patients have benign lesions

# Splitting
set.seed(3) # This is done for reproducibility of training and testing case selection
test_index<- createDataPartition(y = dat$Diagnosis, times = 1,
                                 p = 0.3, list = F)
# The above function is from caret package. Split was done with 30% (0.3) for
# testing set. This function maintains similar distribution of the variable
# to be predicted (Diagnosis)

train_set<- dat[-test_index, ] # remove (minus sign) the cases that are in testing
test_set<- dat[test_index, ] # retain only the cases in testing set

remove(dat, test_index)

#############################################
# Exploratory analysis using the training set
#############################################
nrow(train_set) # number of observation (cases) in the training set

# Distribution (number & %) of malignant and benign in trainin set
table(train_set$Diagnosis)
prop.table(table(train_set$Diagnosis))

####################
# Table 1 codes
####################

########
# Radius
########

## Mean
# Summarize function below will return a tibble and round off to 3 digits 
# since we want two decimals- changing it to five
library(tibble)
options(pillar.sigfig = 5)

train_set %>% # group_by and summarise functions are part of dplyr package
  group_by(Diagnosis) %>%  # Avg = mean and Std is standard deviation
  summarise(Avg = mean(radius_mean), Std = sd(radius_mean))

t.test(radius_mean ~ Diagnosis, data = train_set, alternative = "two.sided",
       paired = F, var.equal = F, conf.level = 0.95) 
# The first argument is the formula (generically y ~ a factor x)
# The second argument is the name of the data
# The third is to see for difference in both sides (higher or lower)
# Paired is for pre and post measurement. Here there are two groups- malignant and benign
# It is best to set equal variance assumption to False. Else, a test to
# determine equal variance should be done
# Confidence level is always typically set at 0.95 in medical research
# NOTE: arguments 3 to 6 needed NOT be defined. By default R will choose it
# Type ? t.test for more information
t.test(radius_mean ~ Diagnosis, data = train_set)
## Worst
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(radius_worst), Std = sd(radius_worst))

t.test(radius_worst ~ Diagnosis, data = train_set)

#########
# Texture
#########
## Mean
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(texture_mean), Std = sd(texture_mean))

t.test(texture_mean ~ Diagnosis, data = train_set)

## Worst
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(texture_worst), Std = sd(texture_worst))

t.test(texture_worst ~ Diagnosis, data = train_set)

#############
# Perimeter
#############
## Mean
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(perimeter_mean), Std = sd(perimeter_mean))

t.test(perimeter_mean ~ Diagnosis, data = train_set)

## Worst
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(perimeter_worst), Std = sd(perimeter_worst))

t.test(perimeter_worst ~ Diagnosis, data = train_set)

########
# Area
########
## Mean
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(area_mean), Std = sd(area_mean))

t.test(area_mean ~ Diagnosis, data = train_set)

## Worst
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(area_worst), Std = sd(area_worst))

t.test(area_worst ~ Diagnosis, data = train_set)

#############
# Smoothness
#############
## Mean

train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(smoothness_mean), Std = sd(smoothness_mean))

t.test(smoothness_mean ~ Diagnosis, data = train_set)

## Worst
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(smoothness_worst), Std = sd(smoothness_worst))

t.test(smoothness_worst ~ Diagnosis, data = train_set)

#################
# Compactness
#################
## Mean
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(compactness_mean), Std = sd(compactness_mean))

t.test(compactness_mean ~ Diagnosis, data = train_set)

## Worst
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(compactness_worst), Std = sd(compactness_worst))

t.test(compactness_worst ~ Diagnosis, data = train_set)

##############
# Concavity
##############
## Mean
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(concavity_mean), Std = sd(concavity_mean))

t.test(concavity_mean ~ Diagnosis, data = train_set)

## Worst
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(concavity_worst), Std = sd(concavity_worst))

t.test(concavity_worst ~ Diagnosis, data = train_set)

################
# Concave points
################
## Mean
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(concave_points_mean), Std = sd(concave_points_mean))

t.test(concave_points_mean ~ Diagnosis, data = train_set)

## Worst
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(concave_points_worst), Std = sd(concave_points_worst))

t.test(concave_points_worst ~ Diagnosis, data = train_set)

##########
# Symmetry
##########
## Mean
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(symmetry_mean), Std = sd(symmetry_mean))

t.test(symmetry_mean ~ Diagnosis, data = train_set)

## Worst
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(symmetry_worst), Std = sd(symmetry_worst))

t.test(symmetry_worst ~ Diagnosis, data = train_set)

####################
# Fractal dimension
####################
## Mean
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(fractal_dimension_mean), Std = sd(fractal_dimension_mean))

t.test(fractal_dimension_mean ~ Diagnosis, data = train_set)

## Worst
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(fractal_dimension_worst), Std = sd(fractal_dimension_worst))

t.test(fractal_dimension_worst ~ Diagnosis, data = train_set)

#################
# Figure 1 codes
#################

r_mean<- train_set %>% # this plot is made using ggplot2
  # for boxplot x should be a factor variable and y numeric
  ggplot(aes(x = Diagnosis, y = radius_mean)) +
  geom_boxplot() + theme_classic() +
  ggtitle("Fig 1A: Radius mean") + # title of the plot
  ylab("") +                       # since the title indicate the feature
  # we do not want to have it in axis again
  theme(plot.title = element_text(size = 7.5)) # control the text size
# the plot is saved as an object to merge
r_worst<- train_set %>%
  ggplot(aes(x = Diagnosis, y = radius_worst)) +
  geom_boxplot() + theme_classic() +
  ggtitle("Fig 1B: Radius worst") + 
  ylab("") +
  theme(plot.title = element_text(size = 7.5))


tex_mean<- train_set %>%
  ggplot(aes(x = Diagnosis, y = texture_mean)) +
  geom_boxplot() + theme_classic() +
  ggtitle("Fig 1C: Texture mean") + 
  ylab("") +
  theme(plot.title = element_text(size = 7.5))

tex_worst<- train_set %>%
  ggplot(aes(x = Diagnosis, y = texture_worst)) +
  geom_boxplot() + theme_classic() +
  ggtitle("Fig 1D: Texture worst") + 
  ylab("") +
  theme(plot.title = element_text(size = 7.5))

per_mean<- train_set %>%
  ggplot(aes(x = Diagnosis, y = perimeter_mean)) +
  geom_boxplot() + theme_classic() +
  ggtitle("Fig 1E: Perimeter mean") + 
  ylab("") +
  theme(plot.title = element_text(size = 7.5))

per_worst<- train_set %>%
  ggplot(aes(x = Diagnosis, y = perimeter_worst)) +
  geom_boxplot() + theme_classic() +
  ggtitle("Fig 1F: Perimeter worst") + 
  ylab("") +
  theme(plot.title = element_text(size = 7.5))

are_mean<- train_set %>%
  ggplot(aes(x = Diagnosis, y = area_mean)) +
  geom_boxplot() + theme_classic() +
  ggtitle("Fig 1G: Area mean") + 
  ylab("") +
  theme(plot.title = element_text(size = 7.5))

are_worst<- train_set %>%
  ggplot(aes(x = Diagnosis, y = area_worst)) +
  geom_boxplot() + theme_classic() +
  ggtitle("Fig 1H: Area worst") + 
  ylab("") +
  theme(plot.title = element_text(size = 7.5))


smo_mean<- train_set %>%
  ggplot(aes(x = Diagnosis, y = smoothness_mean)) +
  geom_boxplot() + theme_classic() +
  ggtitle("Fig 1I: Smoothness mean") + 
  ylab("") +
  theme(plot.title = element_text(size = 7.5))

smo_worst<- train_set %>%
  ggplot(aes(x = Diagnosis, y = smoothness_worst)) +
  geom_boxplot() + theme_classic() +
  ggtitle("Fig 1J: Smoothness worst") + 
  ylab("") +
  theme(plot.title = element_text(size = 7.5))

Fig1<- grid.arrange(r_mean, r_worst, # grid.arrange is a function of gridExtra
                    # Merges the plot saved as objects
                    tex_mean, tex_worst,
                    per_mean, per_worst,
                    are_mean, are_worst,
                    smo_mean, smo_worst, ncol = 2) # Number of columns
# number of rows will be guessed by
# the number of objects- here it will be 5

ggsave(filename = "Fig1.jpeg",  # File name we want (will be saved in our working directory)
       plot = Fig1,
       width = 960,
       height = 1200,
       dpi = 150,              # resolution
       units = "px",           # unit of the width and height
       limitsize = F)

remove(r_mean, r_worst, tex_mean, tex_worst, per_mean, per_worst,
       are_mean, are_worst, smo_mean, smo_worst, Fig1)

################
# Figure 2 codes
################

com_mean<- train_set %>%
  ggplot(aes(x = Diagnosis, y = compactness_mean)) +
  geom_boxplot() + theme_classic() +
  ggtitle("Fig 2A: Compactness mean") + 
  ylab("") +
  theme(plot.title = element_text(size = 7.5))

com_worst<- train_set %>%
  ggplot(aes(x = Diagnosis, y = compactness_worst)) +
  geom_boxplot() + theme_classic() +
  ggtitle("Fig 2B: Compactness worst") + 
  ylab("") +
  theme(plot.title = element_text(size = 7.5))

con_mean<- train_set %>%
  ggplot(aes(x = Diagnosis, y = concavity_mean)) +
  geom_boxplot() + theme_classic() +
  ggtitle("Fig 2C: Concavity mean") + 
  ylab("") +
  theme(plot.title = element_text(size = 7.5))

con_worst<- train_set %>%
  ggplot(aes(x = Diagnosis, y = concavity_worst)) +
  geom_boxplot() + theme_classic() +
  ggtitle("Fig 2D: Concavity worst") + 
  ylab("") +
  theme(plot.title = element_text(size = 7.5))

con1_mean<- train_set %>%
  ggplot(aes(x = Diagnosis, y = concave_points_mean)) +
  geom_boxplot() + theme_classic() +
  ggtitle("Fig 2E: Concave points mean") + 
  ylab("") +
  theme(plot.title = element_text(size = 7.5))


con1_worst<- train_set %>%
  ggplot(aes(x = Diagnosis, y = concave_points_worst)) +
  geom_boxplot() + theme_classic() +
  ggtitle("Fig 2F: Concave points worst") + 
  ylab("") +
  theme(plot.title = element_text(size = 7.5))

sym_mean<- train_set %>%
  ggplot(aes(x = Diagnosis, y = symmetry_mean)) +
  geom_boxplot() + theme_classic() +
  ggtitle("Fig 2G: Symmetry mean") + 
  ylab("") +
  theme(plot.title = element_text(size = 7.5))

sym_worst<- train_set %>%
  ggplot(aes(x = Diagnosis, y = symmetry_worst)) +
  geom_boxplot() + theme_classic() +
  ggtitle("Fig 2H: Symmetry worst") + 
  ylab("") +
  theme(plot.title = element_text(size = 7.5))

fra_mean<- train_set %>%
  ggplot(aes(x = Diagnosis, y = fractal_dimension_mean)) +
  geom_boxplot() + theme_classic() +
  ggtitle("Fig 2I: Fractal dimension mean") + 
  ylab("") +
  theme(plot.title = element_text(size = 7.5))

fra_worst<- train_set %>%
  ggplot(aes(x = Diagnosis, y = fractal_dimension_worst)) +
  geom_boxplot() + theme_classic() +
  ggtitle("Fig 2J: Fractal dimension worst") + 
  ylab("") +
  theme(plot.title = element_text(size = 7.5))

Fig2<- grid.arrange(com_mean, com_worst,
                    con_mean, con_worst,
                    con1_mean, con1_worst,
                    sym_mean, sym_worst,
                    fra_mean, fra_worst, ncol = 2)

ggsave(filename = "Fig2.jpeg",  
       plot = Fig2,
       width = 960,
       height = 1200,
       dpi = 150,             
       units = "px",          
       limitsize = F)

remove(com_mean, com_worst, con_mean, con_worst, con1_mean,
       con1_worst, sym_mean, sym_worst, fra_mean, fra_worst, Fig2)

################################################
# Checking for correlation within the predictors
################################################

pred_train<- train_set %>% # removing the ID, diagnosis and fractal dimension mean. Fractal dimension mean is removed because of its high P value. ID is removed as it adds nothing to the model
  select(-c(ID, Diagnosis, fractal_dimension_mean))

pred_train<- pred_train %>%  # Renaming predictors name to fit in plot
  rename(RM = "radius_mean", RW = "radius_worst",
         TM = "texture_mean", TW = "texture_worst",
         PM = "perimeter_mean", PW = "perimeter_worst",
         AM = "area_mean", AW = "area_worst",
         SM_M = "smoothness_mean", SM_W = "smoothness_worst",
         COM_M = "compactness_mean", COM_W = "compactness_worst",
         CON_M = "concavity_mean", CON_W = "concavity_worst",
         CPM = "concave_points_mean", CPW = "concave_points_worst",
         SYM = "symmetry_mean", SYW = "symmetry_worst",
         FDW = "fractal_dimension_worst")

###############
# Figure 3 code
###############
Fig3<- ggcorr(pred_train, low = "#FF0000", mid = "#FFFFFF", high = "#00FF00", 
              label = TRUE, label_round = 2, legend.position = "none", size = 1,
              label_size = 1) +
  labs(caption = "R = radius, T = texture, P = perimeter, A = area, SM = smoothness,
       COM = compactness, CON = concavity, CP = concave points,
       SY = symmetry, FD = fractal dimension. M reprsents mean and W the worst") +
  theme(plot.caption = element_text(size = 3))

ggsave("Fig3.jpeg",
       Fig3,
       width = 700,
       height = 700,
       units = "px")
###  find correlation method here


findCorrelation(cor(pred_train),cutoff = 0.75, names=TRUE)

remove(Fig3, pred_train)

#########################################
# two training data and two testing data
#########################################

# data with 19 predictors
train_set_19<- train_set %>% select(-c (ID, fractal_dimension_mean))
# removed ID because it is not part of predictors

test_set_19<- test_set %>% select(-c(ID, Diagnosis, fractal_dimension_mean))
# removed diagnosis as it is the variable being predicted

# data with 6 predictors
train_set_6<- train_set %>%
  select(Diagnosis, radius_mean, texture_mean, smoothness_mean,
         symmetry_mean, symmetry_worst, fractal_dimension_worst)
test_set_6<- test_set %>%
  select(radius_mean, texture_mean, smoothness_mean,
         symmetry_mean, symmetry_worst, fractal_dimension_worst)
# NOTE: diagnosis is retained in training set

# scaling
scale2 <- function(x) (x - mean(x)) / sd(x) # the scale function in base returns object of
# class = matrix; this custom function  
# preserves the vector class


train_set_19<- train_set_19 %>%
  mutate_at(c(2:20), .funs = scale2) # first variable is diagnosis
test_set_19<- test_set_19 %>%
  mutate_all(.funs = scale2)

train_set_6<- train_set_6 %>%
  mutate_at(c(2:7), .funs = scale2) # first variable is diagnosis
test_set_6<- test_set_6 %>%
  mutate_all(.funs = scale2)

#####################
# Logistic regression
#####################

# 19 predictors
lg_fit19<- glm(Diagnosis ~ ., family = "binomial", data = train_set_19)
# message due to correlation
levels(train_set_19$Diagnosis)
# glm by default gives the odds of the second level

summary(lg_fit19)

# Odds ratio for each predictors
exp(lg_fit19$coefficients)


lg_fit19_pred<- predict(lg_fit19, newdata = test_set_19, type = "response")
lg_fit19_pred<- ifelse(lg_fit19_pred <= 0.5, "Malignant", "Benign")
levels(lg_fit19_pred)

lg_fit19_pred<- factor(lg_fit19_pred, levels = c("Malignant", "Benign"))

table(Prediction = lg_fit19_pred, 'Reference standard' = test_set$Diagnosis)

# function to compute 95% CI
if(!require(DescTools)) install.packages("DescTools", repos = "http://cran.us.r-project.org")

# accuracy
BinomCI(x = 165, n = 172, method = "modified wilson")
# sensitivity
BinomCI(62, 64) # default is modifieid wilson
# specificity
BinomCI(103, 108)

# six predictors
lg_fit6<- glm(Diagnosis ~ ., family = "binomial", data = train_set_6)
# message due to correlation
levels(train_set_6$Diagnosis)
# glm by default gives the odds of the second level

summary(lg_fit6)

# Odds ratio for each predictors
exp(lg_fit6$coefficients)


lg_fit6_pred<- predict(lg_fit6, newdata = test_set_6, type = "response")
lg_fit6_pred<- ifelse(lg_fit6_pred <= 0.5, "Malignant", "Benign")


lg_fit6_pred<- factor(lg_fit6_pred, levels = c("Malignant", "Benign"))

table(lg_fit6_pred, test_set$Diagnosis)

# accuracy
BinomCI(166, 172)
# sensitivity
BinomCI(62, 64)
# specificity
BinomCI(104, 108)

remove(lg_fit19, lg_fit19_pred, lg_fit6, lg_fit6_pred, scale2)

#######
# QDA
#######

## 19 predictors
qda_fit19<- train(Diagnosis ~ ., method = "qda", data = train_set_19)
qda_fit19_pred<- predict(qda_fit19, newdata = test_set_19, type = "raw")
table(qda_fit19_pred, test_set$Diagnosis)
# accuracy
BinomCI(160, 172)

# sensitivity

BinomCI(62, 64)
# specificity

BinomCI(98, 108)

## 6 predictors
qda_fit6<- train(Diagnosis ~ ., method = "qda", data = train_set_6)
qda_fit6_pred<- predict(qda_fit6, newdata = test_set_6, type = "raw")
table(qda_fit6_pred, test_set$Diagnosis)
# accuracy
BinomCI(162, 172)
# sensitivity
BinomCI(60, 64)
# specificity
BinomCI(102, 108)

remove(qda_fit19, qda_fit19_pred, qda_fit6, qda_fit6_pred)

#######
# LDA
#######
## 19 predictors
lda_fit19<- train(Diagnosis ~ ., method = "lda", data = train_set_19)
lda_fit19_pred<- predict(lda_fit19, newdata = test_set_19, type = "raw")
table(lda_fit19_pred, test_set$Diagnosis)

# accuracy
BinomCI(163, 172)
# sensitivity
BinomCI(56, 64)
# specificity
BinomCI(107, 108)

## 6 predictors
lda_fit6<- train(Diagnosis ~ ., method = "lda", data = train_set_6)
lda_fit6_pred<- predict(lda_fit6, newdata = test_set_6, type = "raw")
table(lda_fit6_pred, test_set$Diagnosis)

# accuracy
BinomCI(165, 172)
# sensitivity
BinomCI(58, 64)
# specificity
BinomCI(107, 108)

remove(lda_fit19, lda_fit19_pred, lda_fit6, lda_fit6_pred)

########
kNN
########
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10, p = 0.75) # p = holdout proportion

## cross validation and best k
set.seed(3)
knn_fit19 <- train(Diagnosis ~ ., method = "knn",
                   data = train_set_19,
                   tuneGrid = data.frame(k = seq(3, 45, 2)),
                   trControl = fitControl)

p1<- ggplot(knn_fit19, highlight = TRUE) + theme_classic() +
  ggtitle("CV with 19 predictors") + ylab("Accuracy") +
  theme(title = element_text(size = 8))

knn_fit19$bestTune

set.seed(3)
knn_fit6 <- train(Diagnosis ~ ., method = "knn",
                  data = train_set_6,
                  tuneGrid = data.frame(k = seq(3, 45, 2)),
                  trControl = fitControl)

p2<- ggplot(knn_fit6, highlight = TRUE) + theme_classic() +
  ggtitle("CV with six predictors") + ylab("Accuracy") +
  theme(title = element_text(size = 8))

knn_fit6$bestTune

Fig4<- grid.arrange(p1, p2, ncol = 1)

ggsave("Fig4.jpeg",
       Fig4,
       width = 1200,
       height = 1200,
       units = "px")

remove(Fig4, p1, p2)

## 19 predictors
knn_fit19_pred<- predict(knn_fit19, newdata = test_set_19,
                         type = "raw")

table(knn_fit19_pred, test_set$Diagnosis)

# accuracy
BinomCI(165, 172)
# sensitivity
BinomCI(60, 64)
# specificity
BinomCI(105, 108)

## six predictors
knn_fit6_pred<- predict(knn_fit6, newdata = test_set_6,
                        type = "raw")

table(knn_fit6_pred, test_set$Diagnosis)

# accuracy
BinomCI(159, 172)
# sensitivity
BinomCI(57, 64)
# specificity
BinomCI(102, 108)

remove(knn_fit19, knn_fit19_pred, knn_fit6, knn_fit6_pred, fitControl)

# For the remaining two algorithm we do not need scaled values
remove(train_set_19, test_set_19, train_set_6, test_set_6,
       scale2)

######################
# Classification tree
######################

# this will have the un scaled data
train_set_19<- train_set %>%
  select(-c(ID, fractal_dimension_mean))
test_set_19<- test_set %>%
  select(-c(ID, Diagnosis, fractal_dimension_mean))

train_set_6<- train_set %>% 
  select(Diagnosis, radius_mean, texture_mean, smoothness_mean,
         symmetry_mean, symmetry_worst, fractal_dimension_worst)
test_set_6<- test_set %>%
  select(radius_mean, texture_mean, smoothness_mean,
         symmetry_mean, symmetry_worst, fractal_dimension_worst)


# Finding the best complexity parameter
tree_fit19<- train(Diagnosis ~ .,
                   method = "rpart",
                   tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                   data = train_set_19,
                   control = rpart.control(minsplit = 5))

p1<- plot(tree_fit19, ylab = "Accuracy", main = "19 predictors") 


tree_fit6<- train(Diagnosis ~ .,
                  method = "rpart",
                  tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                  data = train_set_6,
                  control = rpart.control(minsplit = 5))

p2<- plot(tree_fit6, ylab = "Accuracy", main = "6 predictors") 

tree_fit19$bestTune
tree_fit6$bestTune

#### Optional#############################
plot(tree_fit19$finalModel, margin = 0.1)
text(tree_fit19$finalModel, cex = 0.75)

plot(tree_fit6$finalModel, margin = 0.1)
text(tree_fit6$finalModel, cex = 0.75)
###################################
Fig5<- grid.arrange(p1, p2, ncol = 1)

ggsave("Fig5.jpeg",
       Fig5,
       height = 1200,
       width = 1000,
       units = "px")
remove(p1, p2, Fig5)

## 19 predictors
tree_fit19_pred<- predict(tree_fit19, newdata = test_set_19)

table(tree_fit19_pred, test_set$Diagnosis)

# accuracy
BinomCI(161, 172)
# sensitivity
BinomCI(59, 64)
# specificity
BinomCI(102, 108)

## six predictors
tree_fit6_pred<- predict(tree_fit6, newdata = test_set_6)

table(tree_fit6_pred, test_set$Diagnosis)

# accuracy
BinomCI(154, 172)
# sensitivity
BinomCI(53, 64)
# specificity
BinomCI(101, 108)

###############
# Random forest
###############

## 19 predictors
rf_fit19<- train(Diagnosis ~ ., method = "rf",
                 data = train_set_19)

rf_fit19_pred<- predict(rf_fit19, newdata = test_set_19)

table(rf_fit19_pred, test_set$Diagnosis)
# accuracy
BinomCI(169, 172)
# sensitivity
BinomCI(63, 64)
# specificity
BinomCI(106, 108)


## six predictors
rf_fit6<- train(Diagnosis ~ ., method = "rf",
                data = train_set_6)

rf_fit6_pred<- predict(rf_fit6, newdata = test_set_6)

table(rf_fit6_pred, test_set$Diagnosis)

# accuracy
BinomCI(161, 172)
# sensitivity
BinomCI(59, 64)
# specificity
BinomCI(102, 108)

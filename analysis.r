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
if(!require(FactoMineR)) install.packages("FactoMineR", repos = "http://cran.us.r-project.org")
if(!require(infotheo)) install.packages("infotheo", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")  # Correct package for ggcorr
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")

library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(grid)


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
wilcox.test(radius_mean ~ Diagnosis, data = train_set)

# The first argument is the formula (generically y ~ a factor x)
# The second argument is the name of the data
# The third is to see for difference in both sides (higher or lower)

# Type ? wilcox.test for more information
wilcox.test(radius_mean ~ Diagnosis, data = train_set)
## Worst
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(radius_worst), Std = sd(radius_worst))

wilcox.test(radius_worst ~ Diagnosis, data = train_set)

#########
# Texture
#########
## Mean
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(texture_mean), Std = sd(texture_mean))

wilcox.test(texture_mean ~ Diagnosis, data = train_set)

## Worst
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(texture_worst), Std = sd(texture_worst))

wilcox.test(texture_worst ~ Diagnosis, data = train_set)

#############
# Perimeter
#############
## Mean
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(perimeter_mean), Std = sd(perimeter_mean))

wilcox.test(perimeter_mean ~ Diagnosis, data = train_set)

## Worst
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(perimeter_worst), Std = sd(perimeter_worst))

wilcox.test(perimeter_worst ~ Diagnosis, data = train_set)

########
# Area
########
## Mean
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(area_mean), Std = sd(area_mean))

wilcox.test(area_mean ~ Diagnosis, data = train_set)

## Worst
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(area_worst), Std = sd(area_worst))

wilcox.test(area_worst ~ Diagnosis, data = train_set)

#############
# Smoothness
#############
## Mean

train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(smoothness_mean), Std = sd(smoothness_mean))

wilcox.test(smoothness_mean ~ Diagnosis, data = train_set)

## Worst
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(smoothness_worst), Std = sd(smoothness_worst))

wilcox.test(smoothness_worst ~ Diagnosis, data = train_set)

#################
# Compactness
#################
## Mean
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(compactness_mean), Std = sd(compactness_mean))

wilcox.test(compactness_mean ~ Diagnosis, data = train_set)

## Worst
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(compactness_worst), Std = sd(compactness_worst))

wilcox.test(compactness_worst ~ Diagnosis, data = train_set)

##############
# Concavity
##############
## Mean
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(concavity_mean), Std = sd(concavity_mean))

wilcox.test(concavity_mean ~ Diagnosis, data = train_set)

## Worst
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(concavity_worst), Std = sd(concavity_worst))

wilcox.test(concavity_worst ~ Diagnosis, data = train_set)

################
# Concave points
################
## Mean
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(concave_points_mean), Std = sd(concave_points_mean))

wilcox.test(concave_points_mean ~ Diagnosis, data = train_set)

## Worst
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(concave_points_worst), Std = sd(concave_points_worst))

wilcox.test(concave_points_worst ~ Diagnosis, data = train_set)

##########
# Symmetry
##########
## Mean
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(symmetry_mean), Std = sd(symmetry_mean))

wilcox.test(symmetry_mean ~ Diagnosis, data = train_set)

## Worst
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(symmetry_worst), Std = sd(symmetry_worst))

wilcox.test(symmetry_worst ~ Diagnosis, data = train_set)

####################
# Fractal dimension
####################
## Mean
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(fractal_dimension_mean), Std = sd(fractal_dimension_mean))

wilcox.test(fractal_dimension_mean ~ Diagnosis, data = train_set)

## Worst
train_set %>%
  group_by(Diagnosis) %>%  
  summarise(Avg = mean(fractal_dimension_worst), Std = sd(fractal_dimension_worst))

wilcox.test(fractal_dimension_worst ~ Diagnosis, data = train_set)

#################
# Figure 1 codes
#################

# Figure 1 codes
# Color palette for plots
cb_palette <- c("#E69F00", "#56B4E9")

r_mean <- train_set %>% 
  ggplot(aes(x = Diagnosis, y = radius_mean, fill = Diagnosis)) +
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values = cb_palette) +
  ggtitle("Fig 1A: Radius mean (mm)") + 
  ylab("Radius (mm)") +                       
  theme(plot.title = element_text(size = 7.5)) +
  stat_compare_means(method = "wilcox.test", label = "p.signif", 
                     label.y = max(train_set$radius_mean, na.rm = TRUE) * 1.2, size = 3 ) 

r_worst <- train_set %>%
  ggplot(aes(x = Diagnosis, y = radius_worst, fill = Diagnosis)) +
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values = cb_palette) +
  ggtitle("Fig 1B: Radius worst (mm)") + 
  ylab("Radius (mm)") +
  theme(plot.title = element_text(size = 7.5), legend.position = "none") +
  stat_compare_means(method = "wilcox.test", label = "p.signif", 
                     label.y = max(train_set$radius_worst, na.rm = TRUE) * 1.2, size = 3 ) 

tex_mean <- train_set %>%
  ggplot(aes(x = Diagnosis, y = texture_mean, fill = Diagnosis)) +
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values = cb_palette) +
  ggtitle("Fig 1C: Texture mean") + 
  ylab("Texture") +
  theme(plot.title = element_text(size = 7.5), legend.position = "none") +
  stat_compare_means(method = "wilcox.test", label = "p.signif", 
                     label.y = max(train_set$texture_mean, na.rm = TRUE) * 1.2, size = 3 ) 

tex_worst <- train_set %>%
  ggplot(aes(x = Diagnosis, y = texture_worst, fill = Diagnosis)) +
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values = cb_palette) +
  ggtitle("Fig 1D: Texture worst") + 
  ylab("Texture") +
  theme(plot.title = element_text(size = 7.5), legend.position = "none") +
  stat_compare_means(method = "wilcox.test", label = "p.signif", 
                     label.y = max(train_set$texture_worst, na.rm = TRUE) * 1.2, size = 3 ) 
per_mean <- train_set %>%
  ggplot(aes(x = Diagnosis, y = perimeter_mean, fill = Diagnosis)) +
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values = cb_palette) +
  ggtitle("Fig 1E: Perimeter mean (mm)") + 
  ylab("Perimeter (mm)") +
  theme(plot.title = element_text(size = 7.5), legend.position = "none") +
  stat_compare_means(method = "wilcox.test", label = "p.signif", 
                     label.y = max(train_set$perimeter_mean, na.rm = TRUE) * 1.2, size = 3 ) 

per_worst <- train_set %>%
  ggplot(aes(x = Diagnosis, y = perimeter_worst, fill = Diagnosis)) +
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values = cb_palette) +
  ggtitle("Fig 1F: Perimeter worst (mm)") + 
  ylab("Perimeter (mm)") +
  theme(plot.title = element_text(size = 7.5), legend.position = "none") +
  stat_compare_means(method = "wilcox.test", label = "p.signif", 
                     label.y = max(train_set$perimeter_worst, na.rm = TRUE) * 1.2, size = 3 ) 

are_mean <- train_set %>%
  ggplot(aes(x = Diagnosis, y = area_mean, fill = Diagnosis)) +
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values = cb_palette) +
  ggtitle("Fig 1G: Area mean (mm²)") + 
  ylab("Area (mm²)") +
  theme(plot.title = element_text(size = 7.5), legend.position = "none") +
  stat_compare_means(method = "wilcox.test", label = "p.signif", 
                     label.y = max(train_set$area_mean, na.rm = TRUE) * 1.2, size = 3 ) 

are_worst <- train_set %>%
  ggplot(aes(x = Diagnosis, y = area_worst, fill = Diagnosis)) +
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values = cb_palette) +
  ggtitle("Fig 1H: Area worst (mm²)") + 
  ylab("Area (mm²)") +
  theme(plot.title = element_text(size = 7.5), legend.position = "none") +
  stat_compare_means(method = "wilcox.test", label = "p.signif", 
                     label.y = max(train_set$area_worst, na.rm = TRUE) * 1.2, size = 3 ) 
smo_mean <- train_set %>%
  ggplot(aes(x = Diagnosis, y = smoothness_mean, fill = Diagnosis)) +
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values = cb_palette) +
  ggtitle("Fig 1I: Smoothness mean") + 
  ylab("Smoothness") +
  theme(plot.title = element_text(size = 7.5), legend.position = "none") +
  stat_compare_means(method = "wilcox.test", label = "p.signif", 
                     label.y = max(train_set$smoothness_mean, na.rm = TRUE) * 1.2, size = 3 ) 



smo_worst <- train_set %>%
  ggplot(aes(x = Diagnosis, y = smoothness_worst, fill = Diagnosis)) +
  geom_boxplot() +
  scale_fill_manual(values = cb_palette) +
  ggtitle("Fig 1J: Smoothness worst") + 
  ylab("Smoothness") +
  theme_classic() +
  theme(plot.title = element_text(size = 7.5), legend.position = "none") +
  stat_compare_means(method = "wilcox.test", label = "p.signif",
                     label.y = max(train_set$smoothness_worst, na.rm = TRUE) * 1.2, size = 3) 
sig_legend <- "Significance levels:\nns = not significant   * p ≤ 0.05   ** p ≤ 0.01   *** p ≤ 0.001   **** p ≤ 0.0001"



# Arrange and save as before
Fig1 <- grid.arrange(r_mean, r_worst, 
                     tex_mean, tex_worst,
                     per_mean, per_worst,
                     are_mean, are_worst,
                     smo_mean, smo_worst, ncol = 2,
                     bottom = textGrob(sig_legend, gp = gpar(fontsize = 12), hjust = 0.5))

ggsave(filename = "Fig1.jpeg",  
       plot = Fig1,
       width = 1920,    # increase as needed for bigger panels
       height = 2400,   # increase as needed for bigger panels
       dpi = 150,               
       units = "px",          
       limitsize = F)

remove(r_mean, r_worst, tex_mean, tex_worst, per_mean, per_worst,
       are_mean, are_worst, smo_mean, smo_worst, Fig1)

# Figure 2 codes
# Compactness mean
com_mean <- train_set %>%
  ggplot(aes(x = Diagnosis, y = compactness_mean, fill = Diagnosis)) +
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values = cb_palette) +
  ggtitle("Fig 2A: Compactness mean") + 
  ylab("Compactness") +
  theme(plot.title = element_text(size = 7.5)) +
  stat_compare_means(method = "wilcox.test", label = "p.signif",
                     label.y = max(train_set$compactness_mean, na.rm = TRUE) * 1.2, size = 3) 

# Compactness worst
com_worst <- train_set %>%
  ggplot(aes(x = Diagnosis, y = compactness_worst, fill = Diagnosis)) +
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values = cb_palette) +
  ggtitle("Fig 2B: Compactness worst") + 
  ylab("Compactness") +
  theme(plot.title = element_text(size = 7.5), legend.position = "none") +
  stat_compare_means(method = "wilcox.test", label = "p.signif",
                     label.y = max(train_set$compactness_worst, na.rm = TRUE) * 1.2, size = 3)

# Concavity mean
con_mean <- train_set %>%
  ggplot(aes(x = Diagnosis, y = concavity_mean, fill = Diagnosis)) +
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values = cb_palette) +
  ggtitle("Fig 2C: Concavity mean") + 
  ylab("Concavity") +
  theme(plot.title = element_text(size = 7.5), legend.position = "none") +
  stat_compare_means(method = "wilcox.test", label = "p.signif",
                     label.y = max(train_set$concavity_mean, na.rm = TRUE) * 1.2, size = 3) 

# Concavity worst
con_worst <- train_set %>%
  ggplot(aes(x = Diagnosis, y = concavity_worst, fill = Diagnosis)) +
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values = cb_palette) +
  ggtitle("Fig 2D: Concavity worst") + 
  ylab("Concavity") +
  theme(plot.title = element_text(size = 7.5), legend.position = "none") +
  stat_compare_means(method = "wilcox.test", label = "p.signif",
                     label.y = max(train_set$concavity_worst, na.rm = TRUE) * 1.2, size = 3)

# Concave points mean
con1_mean <- train_set %>%
  ggplot(aes(x = Diagnosis, y = concave_points_mean, fill = Diagnosis)) +
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values = cb_palette) +
  ggtitle("Fig 2E: Concave points mean") + 
  ylab("Concave points") +
  theme(plot.title = element_text(size = 7.5), legend.position = "none") +
  stat_compare_means(method = "wilcox.test", label = "p.signif",
                     label.y = max(train_set$concave_points_mean, na.rm = TRUE) * 1.2, size = 3)

# Concave points worst
con1_worst <- train_set %>%
  ggplot(aes(x = Diagnosis, y = concave_points_worst, fill = Diagnosis)) +
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values = cb_palette) +
  ggtitle("Fig 2F: Concave points worst") + 
  ylab("Concave points") +
  theme(plot.title = element_text(size = 7.5), legend.position = "none") +
  stat_compare_means(method = "wilcox.test", label = "p.signif",
                     label.y = max(train_set$concave_points_worst, na.rm = TRUE) * 1.2, size = 3)

# Symmetry mean
sym_mean <- train_set %>%
  ggplot(aes(x = Diagnosis, y = symmetry_mean, fill = Diagnosis)) +
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values = cb_palette) +
  ggtitle("Fig 2G: Symmetry mean") + 
  ylab("Symmetry") +
  theme(plot.title = element_text(size = 7.5), legend.position = "none") +
  stat_compare_means(method = "wilcox.test", label = "p.signif",
                     label.y = max(train_set$symmetry_mean, na.rm = TRUE) * 1.2, size = 3) 
# Symmetry worst
sym_worst <- train_set %>%
  ggplot(aes(x = Diagnosis, y = symmetry_worst, fill = Diagnosis)) +
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values = cb_palette) +
  ggtitle("Fig 2H: Symmetry worst") + 
  ylab("Symmetry") +
  theme(plot.title = element_text(size = 7.5), legend.position = "none") +
  stat_compare_means(method = "wilcox.test", label = "p.signif",
                     label.y = max(train_set$symmetry_worst, na.rm = TRUE) * 1.2, size = 3) 
# Fractal dimension mean
fra_mean <- train_set %>%
  ggplot(aes(x = Diagnosis, y = fractal_dimension_mean, fill = Diagnosis)) +
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values = cb_palette) +
  ggtitle("Fig 2I: Fractal dimension mean") + 
  ylab("Fractal dimension") +
  theme(plot.title = element_text(size = 7.5), legend.position = "none") +
  stat_compare_means(method = "wilcox.test", label = "p.signif",
                     label.y = max(train_set$fractal_dimension_mean, na.rm = TRUE) * 1.2, size = 3) 

# Fractal dimension worst
fra_worst <- train_set %>%
  ggplot(aes(x = Diagnosis, y = fractal_dimension_worst, fill = Diagnosis)) +
  geom_boxplot() + theme_classic() +
  scale_fill_manual(values = cb_palette) +
  ggtitle("Fig 2J: Fractal dimension worst") + 
  ylab("Fractal dimension") +
  theme(plot.title = element_text(size = 7.5), legend.position = "none") +
  stat_compare_means(method = "wilcox.test", label = "p.signif",
                     label.y = max(train_set$fractal_dimension_worst, na.rm = TRUE) * 1.2, size = 3) 

# Arrange all panels in a grid
Fig2 <- grid.arrange(com_mean, com_worst,
                     con_mean, con_worst,
                     con1_mean, con1_worst,
                     sym_mean, sym_worst,
                     fra_mean, fra_worst, ncol = 2,
                     bottom = textGrob(sig_legend, gp = gpar(fontsize = 12), hjust = 0.5))

# Save the figure (increase width/height for bigger panels if needed)
ggsave(filename = "Fig2.jpeg",  
       plot = Fig2,
       width = 1920,    # increase as needed for bigger panels
       height = 2400,   # increase as needed for bigger panels
       dpi = 150,             
       units = "px",          
       limitsize = F)



remove(com_mean, com_worst, con_mean, con_worst, con1_mean,
       con1_worst, sym_mean, sym_worst, fra_mean, fra_worst, Fig2)

###############
# Figure 3 code
###############
# Define pred_train
pred_train <- train_set %>% 
  select(-c(ID, Diagnosis, fractal_dimension_mean))

pred_train <- pred_train %>%  
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

# Figure 3 code
Fig3 <- ggcorr(pred_train, low = "#E69F00", mid = "#FFFFFF", high = "#56B4E9", 
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
Fig3 <- ggcorr(pred_train, low = "#E69F00", mid = "#FFFFFF", high = "#56B4E9", 
               label = TRUE, label_round = 2, legend.position = "none", size = 1,
               label_size = 1)  +
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
BinomCI(104, 106)

## Reducing Features using PCA and applying Logistic Regression

## Dimentionality Reduction with PCA##

# PCA for dimensionality reduction
pca_result <- PCA(train_set[, -c(1, 2)], graph = FALSE)
summary(pca_result)

num_components <- min(10, ncol(pca_result$ind$coord))
train_set_pca <- data.frame(Diagnosis = train_set$Diagnosis, pca_result$ind$coord[, 1:num_components])



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
# kNN
########
# kNN
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

p1 <- ggplot(knn_fit19, highlight = TRUE) + theme_classic() +
  ggtitle("CV with 19 predictors") + 
  ylab("Accuracy") +
  ylim(0.920, 0.970) + 
  theme(title = element_text(size = 8))

knn_fit19$bestTune

set.seed(3)
knn_fit6 <- train(Diagnosis ~ ., method = "knn",
                  data = train_set_6,
                  tuneGrid = data.frame(k = seq(3, 45, 2)),
                  trControl = fitControl)

p2 <- ggplot(knn_fit6, highlight = TRUE) + theme_classic() +
  ggtitle("CV with six predictors") + 
  ylab("Accuracy") +
  ylim(0.920, 0.970) + 
  theme(title = element_text(size = 8))

knn_fit6$bestTune

Fig4 <- grid.arrange(p1, p2, ncol = 1)

ggsave("Fig4.jpeg",
       Fig4,
       width = 1200,
       height = 1200,
       units = "px")

remove(Fig4, p1, p2)

## 19 predictors
knn_fit19_pred <- predict(knn_fit19, newdata = test_set_19,
                          type = "raw")

table(knn_fit19_pred, test_set$Diagnosis)

# accuracy
BinomCI(165, 172)
# sensitivity
BinomCI(60, 64)
# specificity
BinomCI(105, 108)

## six predictors
knn_fit6_pred <- predict(knn_fit6, newdata = test_set_6,
                         type = "raw")

table(knn_fit6_pred, test_set$Diagnosis)

# accuracy
BinomCI(159, 172)
# sensitivity
BinomCI(57, 64)
# specificity
BinomCI(102, 108)

remove(knn_fit19, knn_fit19_pred, knn_fit6, knn_fit6_pred, fitControl)

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
tree_fit19 <- train(Diagnosis ~ .,
                    method = "rpart",
                    tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                    data = train_set_19,
                    control = rpart.control(minsplit = 5))

p1 <- ggplot(tree_fit19) + 
  geom_point(aes(x = cp, y = Accuracy)) + 
  theme_classic() +
  ggtitle("19 predictors") +
  ylab("Accuracy") +
  ylim(0.7, 1.0) +
  theme(plot.title = element_text(size = 8))

tree_fit6 <- train(Diagnosis ~ .,
                   method = "rpart",
                   tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                   data = train_set_6,
                   control = rpart.control(minsplit = 5))

p2 <- ggplot(tree_fit6) + 
  geom_point(aes(x = cp, y = Accuracy)) + 
  theme_classic() +
  ggtitle("6 predictors") +
  ylab("Accuracy") +
  ylim(0.7, 1.0) +
  theme(plot.title = element_text(size = 8))

Fig5 <- grid.arrange(p1, p2, ncol = 1)

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

# Transform the test set using the PCA result and using the 2 most accurate algorithms
test_set_pca_values <- predict(pca_result, newdata = test_set[, -c(1, 2)])$coord[, 1:num_components]
test_set_pca <- data.frame(Diagnosis = test_set$Diagnosis, test_set_pca_values)



######Logistic Regression########
lg_fitpca<- glm(Diagnosis ~ ., family = "binomial", data = train_set_pca)
# message due to correlation
levels(train_set_pca$Diagnosis)
# glm by default gives the odds of the second level

summary(lg_fitpca)

# Odds ratio for each predictors
exp(lg_fitpca$coefficients)


lg_fitpca_pred<- predict(lg_fitpca, newdata = test_set_pca, type = "response")
lg_fitpca_pred<- ifelse(lg_fitpca_pred <= 0.5, "Malignant", "Benign")


lg_fitpca_pred<- factor(lg_fitpca_pred, levels = c("Malignant", "Benign"))

table(lg_fitpca_pred, test_set_pca$Diagnosis)

# accuracy
BinomCI(168, 172)
# sensitivity
BinomCI(61, 62)
# specificity
BinomCI(107, 110)

#### Random Forest####
rf_fitpca<- train(Diagnosis ~ ., method = "rf",
                  data = train_set_pca)

rf_fitpca_pred<- predict(rf_fitpca, newdata = test_set_pca)

table(rf_fitpca_pred, test_set_pca$Diagnosis)

# accuracy
BinomCI(162, 172)
# sensitivity
BinomCI(60, 66)
# specificity
BinomCI(102, 106)

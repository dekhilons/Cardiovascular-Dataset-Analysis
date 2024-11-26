########### Installing Packages ############

install.packages("tidyverse")
install.packages("caret")
install.packages("randomForest")
install.packages("tidymodels")
install.packages("dplyr")
install.packages("GGally")
install.packages("superml")
install.packages("Boruta")
install.packages("baguette")
install.packages("discrim")
install.packages("bonsai")
install.packages("ggplot2")
install.packages("stringr")
install.packages("Hmisc")

########### Loading Libraries ############

library(dplyr)
library(tidyverse)
library(caret)
library(randomForest)
library(tidymodels)
library(GGally)
library(superml)
library(Boruta)
library(baguette)
library(discrim)
library(bonsai)
library(ggplot2)
library(stringr)
library(Hmisc)

####################################################################################################

########### Importing DataSet###########

data <- read.csv(file.choose(), header = TRUE, sep = ";", dec = ".", quote = "", fill = TRUE)
data_ne<- read.csv(file.choose(), header = TRUE, sep = ";", dec = ".", quote = "", fill = TRUE)

####################################################################################################

#I]DATA PREPARATION & EXPLORATION

####################################################################################################

#---------------------Data Preparation---------------------

# 1) DATA EXPLORATION :

############ Let's check some information about our data #############

dim(data)             #####dimensions#####
head(data)            #####first lines of our data#####
tail(data)            #####last lines of our data#####
names(data)           #####names of columns#####
data<- distinct(data) ###### check duplicated rows #####
summary(data)         ###### Summary Of Numerical Features #####
str(data)             ###### The internal structure ###### 
         
#2) HANDLING MISSING VALUES : 

missing_values <- colSums(is.na(data)|data == "")
missing_values

percentage_of_zeros_Cholesterol <- sum(data$Cholesterol == "" | is.na(data$Cholesterol)) / length(data$Cholesterol) * 100
percentage_of_zeros_Cholesterol

percentage_of_zeros_TypeDouleurThoracique<- sum(data$TypeDouleurThoracique== "" | is.na(data$TypeDouleurThoracique)) / length(data$TypeDouleurThoracique) * 100
percentage_of_zeros_TypeDouleurThoracique

percentage_of_zeros_ECGRepos<- sum(data$ECGRepos== ""| is.na(data$ECGRepos)) / length(data$ECGRepos) * 100
percentage_of_zeros_ECGRepos

data$Cholesterol[is.na(data$Cholesterol) | data$Cholesterol == ""] <- NA
data$Age[is.na(data$Age) | data$Age== ""] <- NA
data$Cholesterol[is.na(data$Sexe) | data$Sexe== ""] <- NA
data$Cholesterol[is.na(data$TypeDouleurThoracique) | data$TypeDouleurThoracique== ""] <- NA
data$Cholesterol[is.na(data$GlycemieJeune) | data$GlycemieJeune== ""] <- NA
data$Cholesterol[is.na(data$TensionArterielleRepos) | data$TensionArterielleRepos== ""] <- NA
data$Cholesterol[is.na(data$ECGRepos) | data$ECGRepos== ""] <- NA
data$Cholesterol[is.na(data$FrequenceCardiaqueMax) | data$FrequenceCardiaqueMax== ""] <- NA
data$Cholesterol[is.na(data$AngineExercice) | data$AngineExercice== ""] <- NA
data$Cholesterol[is.na(data$MaladieCardiaque) | data$MaladieCardiaque== ""] <- NA
data$Cholesterol[is.na(data$PenteSTExercice) | data$PenteSTExercice== ""] <- NA
data$Cholesterol[is.na(data$DepressionAncienne) | data$DepressionAncienne== ""] <- NA

data <- na.omit(data)
data_ne <- na.omit(data_ne)


#3) HANDELING OUTLIERS:

############# function to impute the outliers ############# 

replace_outliers <- function(data, variable) {
  Q1 <- quantile(data[[variable]], 0.25)
  Q3 <- quantile(data[[variable]], 0.75)
  IQR_value <- Q3 - Q1
  lower_limit <- Q1 - 1.5 * IQR_value
  upper_limit <- Q3 + 1.5 * IQR_value
  
  data[[variable]][data[[variable]] < lower_limit] <- lower_limit
  data[[variable]][data[[variable]] > upper_limit] <- upper_limit
  
  return(data)
}

############# Let's check our features if they have any outliers ############# 

############## AGE ##############

boxplot(data$Age)                     

############## TensionArterielleRepos #############

boxplot(data$TensionArterielleRepos)   

data_ne$GlycemieJeune <- as.numeric(data_ne$GlycemieJeune)
data$TensionArterielleRepos

data_ne <- replace_outliers(data_ne, "TensionArterielleRepos")
data <- replace_outliers(data, "TensionArterielleRepos")

############GlycemieJeune#################

data_ne$GlycemieJeune <- as.numeric(data_ne$GlycemieJeune)
data$GlycemieJeune <- as.numeric(data$GlycemieJeune)

boxplot(data$GlycemieJeune)

#we won't drop the outlier here as the values will be all 0s

##########FrequenceCardiaqueMax############

data_ne$FrequenceCardiaqueMax <- as.numeric(data_ne$FrequenceCardiaqueMax)
data$FrequenceCardiaqueMax <- as.numeric(data$FrequenceCardiaqueMax)

data$FrequenceCardiaqueMax
boxplot(data$FrequenceCardiaqueMax)

data_ne <- replace_outliers(data_ne, "FrequenceCardiaqueMax")
data <- replace_outliers(data, "FrequenceCardiaqueMax")
data$FrequenceCardiaqueMax

##########DepressionAncienne############

data_ne$DepressionAncienne <- as.numeric(data_ne$DepressionAncienne)
data$DepressionAncienne <- as.numeric(data$DepressionAncienne)
boxplot(data$FrequenceCardiaqueMax)

data_ne <- replace_outliers(data_ne, "DepressionAncienne")
data <- replace_outliers(data, "DepressionAncienne")
data$DepressionAncienne


# 4) Exploring Features/Target :


#4.1) Target quantitative distribution :
 
#MALADIECARDIAQUE Distribution

mypal <- c('#FC05FB', '#FEAEFE', '#FCD2FC', '#F3FEFA', '#B4FFE4', '#3FFEBA')

p <- ggplot(data, aes(x = factor(MaladieCardiaque), fill = factor(MaladieCardiaque))) +
  geom_bar() +
  scale_fill_manual(values = mypal) +
  theme_minimal(base_size = 12) +
  theme(panel.background = element_rect(fill = "#F6F5F4", color = NA),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(title = "MaladieCardiaque Variable Distribution")
print(p) 
     
counts <- table(data$MaladieCardiaque)

count <- prop.table(counts) * 100

percentage_labels <- paste0(formatC(count, format = "f", digits = 1), " %")

annotations <- data.frame(MaladieCardiaque = as.factor(names(counts)), percentage = count, label = percentage_labels)

p + geom_text(data = annotations, aes(label = label, y = count), position = position_stack(vjust = 0.5))

#4.2) Target qualitative distribution :

unique_categories <- unique(data$Cholesterol)
unique_categories

draw_histogram <- function(dataframe, feature) {
  hist(dataframe[[feature]], 
       main = paste(feature, "Distribution"), 
       col = "#3FFEBA", 
       border = "black", 
       xlim = range(dataframe[[feature]]),
       xlab = feature,
       ylab = "Frequency",
       breaks = 20)
}

draw_histogram(data, "Cholesterol")


#5)Data Transformation

#using one hot encoding 
one_hot_encode <- function(data, categorical_columns) {
 
  data_subset <- data[categorical_columns]
  encoded_matrix <- model.matrix(~ . - 1, data = data_subset)

  data_encoded <- as.data.frame(encoded_matrix)

  data_combined <- cbind(data, data_encoded)

  data_combined <- data_combined[, !(names(data_combined) %in% categorical_columns)]
  
  return(data_combined)
}

categorical_features <- c("AngineExercice", "PenteSTExercice", "Sexe","TypeDouleurThoracique","ECGRepos" )
data_enco <- one_hot_encode(data, categorical_features)


view(data_enco)

#6) scaling 

# Function for scaling between 0 and 1 

scale_0_1 <- function(x) {
  if (any(is.na(x))) {
    return(x)
  } else {
    return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  }
}

# Identify numeric columns for scaling
numeric_columns <- names(data_enco)[sapply(data_enco, is.numeric)]

# Apply scaling only to numeric columns
data_scaled <- data_enco %>% 
  mutate(across(all_of(numeric_columns), scale_0_1))

# View the resulting dataframe
View(data_scaled)

#----------------------------------------------------------------------------------------------

#---------------------UNIVARIANTE ANALYSIS---------------------

###########################################################################

##########  Numerical variables ################

#1-Visulization through histograms:

draw_histograms <- function(dataframe, features, rows, cols) {
  par(mfrow = c(rows, cols), mar = c(4, 4, 2, 1))
  
  for (i in seq_along(features)) {
    feature <- features[i]
    hist(dataframe[[feature]], main = paste(feature, "Distribution"), 
         col = "#FEAEFE", xlab = feature, ylab = "Count", 
         border = "white", las = 1)
  }
  
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 2, 0))
  
}
selected_features <- c('Age', 'TensionArterielleRepos')
draw_histograms(data[selected_features], selected_features, 1, 2)

#####Statistical and graphical testing 

#1) Test the normality 

#Using visualization

#-----Histogram --------

par(mfrow=c(2,3))
hist(data_ne$Age, prob = TRUE, main = "Histogram of Age", xlab = "Age", ylab = "Density of distribution", col = "#FEAEFE") 
lines(density(data_ne$Age),col="black", lwd = 2)
hist(data_ne$TensionArterielleRepos, prob = TRUE, main = "Histogram of Tension Arterielle au Repos", xlab = "TensionArterielleRepos", ylab = "Density of distribution", col = "#FEAEFE") # Histogramme en densité
lines(density(data_ne$TensionArterielleRepos),col="black", lwd = 2)
hist(data_ne$Cholesterol, prob = TRUE, main = "Histogram of Cholesterol", xlab = "Cholesterol", ylab = "Densité de la distribution", col = "#FEAEFE") 
lines(density(data_ne$Cholesterol),col="black", lwd = 2)
hist(data_ne$FrequenceCardiaqueMax, prob = TRUE, main = "Histogram of Frequence Cardiaque Max", xlab = "FrequenceCardiaqueMax", ylab = "Density of distribution", col = "#FEAEFE") # Histogramme en densité
lines(density(data_ne$FrequenceCardiaqueMax),col="black", lwd = 2)
hist(data_ne$DepressionAncienne, prob = TRUE, main = "Histogram of Depression Ancienne", xlab = "DepressionAncienne", ylab = "Density of distribution", col = "#FEAEFE") # Histogramme en densité
lines(density(data_ne$DepressionAncienne),col="black", lwd = 2)


#-----Normal Probability Graph --------

#The Q-Q plot compares what you'd expect if data was normally distributed (theoretical quantiles) to what we actually have (empirical quantiles)
#Quantiles help us understand the distribution of a dataset by splitting it into parts, each containing a specified proportion of the data. 

par(mfrow = c(2, 3))

# Function to draw Q-Q plot with custom labels to reconfirm the normality test

draw_qq_plot <- function(variable, title) {
  qqnorm(variable, main = paste("Q-Q Plot for", title))
  qqline(variable, col = "#FC05FB")
}

draw_qq_plot(data_ne$Age, "Age")
draw_qq_plot(data_ne$TensionArterielleRepos, "Tension Artérielle au Repos")
draw_qq_plot(data_ne$Cholesterol, "Cholestérol")
draw_qq_plot(data_ne$FrequenceCardiaqueMax, "Fréquence Cardiaque Max")
draw_qq_plot(data_ne$DepressionAncienne, "Dépression Ancienne")


#2) shapiro test

#=================================================================
##Hypothesis:

#H0 : variable follows normal distribution
#H1 : variable doesn't follow a normal distribution
#=================================================================

shapiro.test(data_ne$Age)

#=================================================================
#p_value<0.05 : We reject H0 and accept H1 
#The feature Age doesn't follow a normal distribution
#=================================================================

shapiro.test(data_ne$TensionArterielleRepos)

#=================================================================
#p_value<0.05 : We reject H0 and accept H1 
#The feature TensionArterielleRepos doesn't follow a normal distribution
#=================================================================

shapiro.test(data_ne$Cholesterol)

#=================================================================
#p_value<0.05 : We reject H0 and accept H1 
#The feature Cholesterol doesn't follow a normal distribution
#=================================================================

shapiro.test(data_ne$FrequenceCardiaqueMax)

#=================================================================

#p_value<0.05 : We reject H0 and accept H1 
#The feature FrequenceCardiaqueMax doesn't follow a normal distribution
#=================================================================

shapiro.test(data_ne$DepressionAncienne)

#=================================================================
#p_value<0.05 : We reject H0 and accept H1 
#The feature DepressionAncienne doesn't follow a normal distribution
#=================================================================




#---------------------BIVARIANTE ANALYSIS--------------------------

#Visulization of bivariant relationship :

################sexe##################

data <- data[data$Sexe != "", ]
data_ne <- data_ne[data_ne$Sexe != "", ]

# visualization of the feature 'Sexe' 

count_plot <- function(data, cat_feature, target_var) {
  my_palette <- c("#3FFEBA", "#FEAEFE")
    p <- ggplot(data, aes(x = !!as.name(cat_feature), fill = factor(!!as.name(target_var)))) +
    geom_bar(position = "dodge") +
    labs(x = cat_feature, y = "Count") +
    scale_fill_manual(values = my_palette, name = target_var) +
    theme_minimal() +
    theme(legend.position = "right")
    print(p)
}

count_plot(data, "Sexe", "MaladieCardiaque")

###############TypeDouleurThoracique##################

data_ne <- data_ne[data_ne$TypeDouleurThoracique != "", ]
data <- data[data$TypeDouleurThoracique != "", ]

count_plot <- function(data, cat_feature, target_var) {
  my_palette <- c("#3FFEBA", "#FEAEFE")
  
  p <- ggplot(data, aes(x = !!as.name(cat_feature), fill = factor(!!as.name(target_var)))) +
    geom_bar(position = "dodge") +
    labs(x = cat_feature, y = "Count") +
    scale_fill_manual(values = my_palette, name = target_var) +
    theme_minimal() +
    theme(legend.position = "right")
  
  print(p)
}

count_plot(data, "TypeDouleurThoracique", "MaladieCardiaque")


###############GlycemieJeune##################

data <- data[data$GlycemieJeune != "", ]
data_ne <- data_ne[data_ne$GlycemieJeune != "", ]

count_plot <- function(data, cat_feature, target_var) {
  my_palette <- c("#3FFEBA", "#FEAEFE")
  
  p <- ggplot(data, aes(x = !!as.name(cat_feature), fill = factor(!!as.name(target_var)))) +
    geom_bar(position = "dodge") +
    labs(x = cat_feature, y = "Count") +
    scale_fill_manual(values = my_palette, name = target_var) +
    theme_minimal() +
    theme(legend.position = "right")
  
  print(p)
}

count_plot(data, "GlycemieJeune", "MaladieCardiaque")


###############ECGRepos##################

data_ne <- data_ne[data_ne$ECGRepos != "", ]
data <- data[data$ECGRepos != "", ]

count_plot <- function(data, cat_feature, target_var) {
  my_palette <- c("#3FFEBA", "#FEAEFE")
  
  p <- ggplot(data, aes(x = !!as.name(cat_feature), fill = factor(!!as.name(target_var)))) +
    geom_bar(position = "dodge") +
    labs(x = cat_feature, y = "Count") +
    scale_fill_manual(values = my_palette, name = target_var) +
    theme_minimal() +
    theme(legend.position = "right")
  
  print(p)
}

count_plot(data, "ECGRepos", "MaladieCardiaque")                   

##############FrequenceCardiaqueMax##################

count_plot <- function(data, cat_feature, target_var) {
  my_palette <- c("#3FFEBA", "#FEAEFE")
  
  p <- ggplot(data, aes(x = !!as.name(cat_feature), fill = factor(!!as.name(target_var)))) +
    geom_bar(position = "dodge") +
    labs(x = cat_feature, y = "Count") +
    scale_fill_manual(values = my_palette, name = target_var) +
    theme_minimal() +
    theme(legend.position = "right")
  
  print(p)
}

count_plot(data, "FrequenceCardiaqueMax", "MaladieCardiaque")
###############AngineExercice##################

data_ne <- data_ne[data_ne$AngineExercice != "", ]
data <- data[data$AngineExercice != "", ]

count_plot <- function(data, cat_feature, target_var) {
  my_palette <- c("#3FFEBA", "#FEAEFE")
  
  p <- ggplot(data, aes(x = !!as.name(cat_feature), fill = factor(!!as.name(target_var)))) +
    geom_bar(position = "dodge") +
    labs(x = cat_feature, y = "Count") +
    scale_fill_manual(values = my_palette, name = target_var) +
    theme_minimal() +
    theme(legend.position = "right")
  
  print(p)
}

count_plot(data, "AngineExercice", "MaladieCardiaque")                   


###############PenteSTExercice##################

data_ne <- data_ne[data_ne$PenteSTExercice != "", ]
data <- data[data$PenteSTExercice != "", ]

count_plot <- function(data, cat_feature, target_var) {
  my_palette <- c("#3FFEBA", "#FEAEFE")
  
  p <- ggplot(data, aes(x = !!as.name(cat_feature), fill = factor(!!as.name(target_var)))) +
    geom_bar(position = "dodge") +
    labs(x = cat_feature, y = "Count") +
    scale_fill_manual(values = my_palette, name = target_var) +
    theme_minimal() +
    theme(legend.position = "right")
  
  print(p)
}

count_plot(data, "PenteSTExercice", "MaladieCardiaque") 


#######Continuous variables vs Categorical variables #############




tapply (data$TensionArterielleRepos,data$MaladieCardiaque,shapiro.test)
#test of normality not valid
#scale
data$TensionArterielleRepos<-scale(data$TensionArterielleRepos)
#check for normality
tapply (data$TensionArterielleRepos,data$MaladieCardiaque,shapiro.test)
#accept H1 => non-param
#use wilcox for 2 modlities
wilcox.test(data$TensionArterielleRepos~data$MaladieCardiaque,data=data)
#we accept H1 there is a link between 2 variables 

tapply (data$Age,data$MaladieCardiaque,shapiro.test)
#test of normality not valid cause of 1 
data$Age<-scale(data$Age)
tapply (data$Age,data$MaladieCardiaque,shapiro.test)
wilcox.test(data$Age~data$MaladieCardiaque,data=data)
#we accept H1 there is a link between 2 variables 

tapply (data$Cholesterol,data$MaladieCardiaque,shapiro.test)
#test of normality not valid 
data$Cholesterol<-scale(data$Cholesterol)
wilcox.test(data$Cholesterol~data$MaladieCardiaque,data=data)
#we accept H1 there is a link between 2 variables 

tapply (data$TensionArterielleRepos,data$MaladieCardiaque,shapiro.test)
#test of normality not valid 
data$TensionArterielleRepos<-scale(data$TensionArterielleRepos)

wilcox.test(data$TensionArterielleRepos~data$MaladieCardiaque,data=data)
#we accept H1 there is a link between 2 variables 

tapply (data$FrequenceCardiaqueMax,data$MaladieCardiaque,shapiro.test)
#test of normality not valid 
data$FrequenceCardiaqueMax<-scale(data$FrequenceCardiaqueMax)
wilcox.test(data$FrequenceCardiaqueMax~data$MaladieCardiaque,data=data)
#we accept H1 there is a link between 2 variables 


tapply (data$DepressionAncienne,data$MaladieCardiaque,shapiro.test)
#test of normality not valid 
data$DepressionAncienne<-scale(data$DepressionAncienne)
wilcox.test(data$DepressionAncienne~data$MaladieCardiaque,data=data)
#we accept H1 there is a link between 2 variables 




#-----------------visualization of the correlation 

install.packages("corrplot")

library(corrplot)

numeric_columns <- sapply(data_ne, is.numeric)

cor_matrix <- cor(data_ne[, numeric_columns], method = "spearman")

corrplot(cor_matrix, type = "lower", method = "number", tl.col = "black")


#######categorical variables vs Categorical variables #############

#checking if there is a significant association between the "MaladieCardiaque" and the qualitative features 

#-------------------TypeDouleurThoracique-------------------

contingency_table <- table(data_ne$MaladieCardiaque, data_ne$TypeDouleurThoracique)
chi_square_test_result <- chisq.test(contingency_table)
print(chi_square_test_result)
#p-value < 2.2e-16  

#-------------------ECGRepos-------------------

contingency_table <- table(data_ne$MaladieCardiaque, data_ne$ECGRepos)
chi_square_test_result <- chisq.test(contingency_table)
print(chi_square_test_result)
#p-value = 0.004404  

#-------------------AngineExercice-------------------

contingency_table <- table(data_ne$MaladieCardiaque, data_ne$AngineExercice)
chi_square_test_result <- chisq.test(contingency_table)
print(chi_square_test_result)
#p-value < 2.2e-16

#-------------------PenteSTExercice-------------------

contingency_table <- table(data_ne$MaladieCardiaque, data_ne$PenteSTExercice)
chi_square_test_result <- chisq.test(contingency_table)
print(chi_square_test_result)
#p-value < 2.2e-16


#---------------------LINEAR REGRESSION--------------------------

# 1) Simple Linear Regression :

#####Hypothesis:

#H0: All the parameters are null
#H1: One non null parameter at least exists

#########Age ###############

set.seed(42)  # for reproducibility
splitIndex <- createDataPartition(data_ne$Cholesterol, p = 0.8, list = FALSE)
train_data <- data_ne[splitIndex, ]
test_data <- data_ne[-splitIndex, ]

model <- lm(Cholesterol ~ Age, data = train_data)

predictions <- predict(model, newdata = test_data)

ggplot(test_data, aes(x = Age, y = Cholesterol)) +
  geom_point(color = 'black', size = 3, alpha = 0.7) +
  geom_line(aes(y = predictions), color = 'blue', size = 1.5) +
  labs(title = 'Linear Regression: Age vs. Cholesterol', 
       x = 'Age', y = 'Cholesterol') +
  theme_minimal()

model=lm(Cholesterol~ Age,data=data_ne)
summary(model)


######### TensionArterielleRepos ###############

set.seed(42)  # for reproducibility
splitIndex <- createDataPartition(data_ne$Cholesterol, p = 0.8, list = FALSE)
train_data <- data_ne[splitIndex, ]
test_data <- data_ne[-splitIndex, ]

model <- lm(Cholesterol ~ TensionArterielleRepos, data = train_data)

predictions <- predict(model, newdata = test_data)

ggplot(test_data, aes(x = TensionArterielleRepos, y = Cholesterol)) +
  geom_point(color = 'black', size = 3, alpha = 0.7) +
  geom_line(aes(y = predictions), color = 'blue', size = 1.5) +
  labs(title = 'Linear Regression: TensionArterielleRepos vs. Cholesterol', 
       x = 'TensionArterielleRepos', y = 'Cholesterol') +
  theme_minimal()

model=lm(Cholesterol~ TensionArterielleRepos,data=data_ne)
summary(model)

######### GlycemieJeune ###############

set.seed(42)  # for reproducibility
splitIndex <- createDataPartition(data_ne$Cholesterol, p = 0.8, list = FALSE)
train_data <- data_ne[splitIndex, ]
test_data <- data_ne[-splitIndex, ]

model <- lm(Cholesterol ~ GlycemieJeune, data = train_data)

predictions <- predict(model, newdata = test_data)

ggplot(test_data, aes(x = GlycemieJeune, y = Cholesterol)) +
  geom_point(color = 'black', size = 3, alpha = 0.7) +
  geom_line(aes(y = predictions), color = 'blue', size = 1.5) +
  labs(title = 'Linear Regression: GlycemieJeune vs. Cholesterol', 
       x = 'GlycemieJeune', y = 'Cholesterol') +
  theme_minimal()

model=lm(Cholesterol~ GlycemieJeune,data=data_ne)
summary(model)


######### FrequenceCardiaqueMax ###############

set.seed(42)  # for reproducibility
splitIndex <- createDataPartition(data_ne$Cholesterol, p = 0.8, list = FALSE)
train_data <- data_ne[splitIndex, ]
test_data <- data_ne[-splitIndex, ]

model <- lm(Cholesterol ~ FrequenceCardiaqueMax, data = train_data)

predictions <- predict(model, newdata = test_data)

ggplot(test_data, aes(x = FrequenceCardiaqueMax, y = Cholesterol)) +
  geom_point(color = 'black', size = 3, alpha = 0.7) +
  geom_line(aes(y = predictions), color = 'blue', size = 1.5) +
  labs(title = 'Linear Regression: FrequenceCardiaqueMax vs. Cholesterol', 
       x = 'FrequenceCardiaqueMax', y = 'Cholesterol') +
  theme_minimal()

model=lm(Cholesterol~ FrequenceCardiaqueMax,data=data_ne)
summary(model)

######### DepressionAncienne ###############

set.seed(42)  # for reproducibility
splitIndex <- createDataPartition(data_ne$Cholesterol, p = 0.8, list = FALSE)
train_data <- data_ne[splitIndex, ]
test_data <- data_ne[-splitIndex, ]

model <- lm(Cholesterol ~ DepressionAncienne, data = train_data)

predictions <- predict(model, newdata = test_data)

ggplot(test_data, aes(x = DepressionAncienne, y = Cholesterol)) +
  geom_point(color = 'black', size = 3, alpha = 0.7) +
  geom_line(aes(y = predictions), color = 'blue', size = 1.5) +
  labs(title = 'Linear Regression: DepressionAncienne vs. Cholesterol', 
       x = 'DepressionAncienne', y = 'Cholesterol') +
  theme_minimal()

model=lm(Cholesterol~ DepressionAncienne,data=data_ne)
summary(model)



# 2) multiple Linear Regression :

#####Hypothesis:

#H0: All the parameters are null
#H1: One non null parameter at least exists


############

model1<-lm(Cholesterol~Age+TensionArterielleRepos+DepressionAncienne+FrequenceCardiaqueMax,data)
summary(model1)

model2<-lm(Cholesterol~TensionArterielleRepos+DepressionAncienne+FrequenceCardiaqueMax,data)
summary(model2)

shapiro.test(resid(model1))
shapiro.test(resid(model2))

#accept H1 that means no realtion between coef: resi doesnt follow normal dis / p_value < alpha accept H1 / R low

AIC(model1)
AIC(model2)

#Interpretation: Overall Significance: p-value: < 2.2e-16

#We reject the null hypothesis (H0): there is at least one non-null parameter.
#Regression Quality: R-squared = 0.01766 => the model quality is not good.
#1.864% of the variability in Cholesterol is explained by the other variables.

#------------------------Model Performance Improvement-------------------------

#Less significant variables are those with the most important p-values.
#The two least significant variables are: "TypeDouleurThoraciqueTA" and "Age".


#----------------------------By eliminating TypeDouleurThoraciqueTA & Age-----------------


#No notable change in R-squared (0.1821): the model quality is not affected.
#The eliminated variable is not important.



####################################################################################################

#III]Multivariate Analysis

####################################################################################################
# 1) ANOVA Test :

####Inaccurate results therefore we decided to do the KrusKal Test

#ANOVA

tapply(data$FrequenceCardiaqueMax ,data$TypeDouleurThoracique,shapiro.test)
#check for intra
#accept H1 no normality 
kruskal.test(data$FrequenceCardiaqueMax ~ data$TypeDouleurThoracique)

#2)Logistic Regression 
lbl = LabelEncoder$new()
data$Sexe<- lbl$fit_transform(data$Sexe)
data$AngineExercice<- lbl$fit_transform(data$AngineExercice)
data$PenteSTExercice <- lbl$fit_transform(data$PenteSTExercice )
data$ECGRepos<- lbl$fit_transform(data$ECGRepos)
data$TypeDouleurThoracique<- lbl$fit_transform(data$TypeDouleurThoracique)
data$GlycemieJeune<- lbl$fit_transform(data$GlycemieJeune)

str(data)

#Data split
set.seed(1223)
data_split <- initial_split(data, prop = 0.75, strata = data$MaladieCardiaque)
data_train <- data_split %>% training()
data_test <- data_split %>% testing()

logmod1 <- glm(MaladieCardiaque~., data = data_train, family = 'binomial')
summary(logmod1)
AIC(logmod1)
prediction <- predict(logmod1, newdata = data_test, type = 'response')
prediction[1:10]

logmod2 <- glm(MaladieCardiaque~.-TensionArterielleRepos, data = data_train, family = 'binomial')
summary(logmod2)
AIC(logmod2)
prediction <- predict(logmod2, newdata = data_test, type = 'response')


logmod3 <- glm(MaladieCardiaque~.-TensionArterielleRepos-ECGRepos, data = data_train, family = 'binomial')
summary(logmod3)
AIC(logmod3)
prediction <- predict(logmod3, newdata = data_test, type = 'response')

logmod4 <- glm(MaladieCardiaque~.-TensionArterielleRepos-ECGRepos-TypeDouleurThoracique, data = data_train, family = 'binomial')
summary(logmod4)
AIC(logmod4)
prediction <- predict(logmod4, newdata = data_test, type = 'response')

logmod5 <- glm(MaladieCardiaque~.-TensionArterielleRepos-ECGRepos-TypeDouleurThoracique-Age, data = data_train, family = 'binomial')
summary(logmod5)
AIC(logmod5)
prediction <- predict(logmod5, newdata = data_test, type = 'response')

shapiro.test(resid(logmod5))
#we accept h1

#lda
lda1 <- lda(MaladieCardiaque~., data = data_train)
summary(lda1)
AIC(lda1)

# Assuming 'target' is the column indicating the presence or absence of heart disease

# Install and load the MASS library for LDA
install.packages("MASS")
library(MASS)

# Assuming your data frame is called 'heart_data'
# Assuming 'target' is the column indicating the presence or absence of heart disease
# Replace 'your_explanatory_variables' with the actual column names of your explanatory variables

# Subset the data into features and target variable
features_train <- data_train[, c('Age', 'Sexe','TypeDouleurThoracique','TensionArterielleRepos','Cholesterol','GlycemieJeune','ECGRepos','FrequenceCardiaqueMax','AngineExercice','DepressionAncienne','PenteSTExercice','MaladieCardiaque')]
target_train <- data_train$MaladieCardiaque
features_test <- data_test[, c('Age', 'Sexe','TypeDouleurThoracique','TensionArterielleRepos','Cholesterol','GlycemieJeune','ECGRepos','FrequenceCardiaqueMax','AngineExercice','DepressionAncienne','PenteSTExercice','MaladieCardiaque')]
target_test <- data_test$MaladieCardiaque

str(target )

# Fit LDA model
lda_model <- lda(data_train$MaladieCardiaque~ ., data = features_train)

# Print summary
summary(lda_model)

# Predict using the LDA model
predictions <- predict(lda_model, newdata = features_test)

# Confusion matrix
conf_matrix <- table(predictions$class, data_test$MaladieCardiaque)
print(conf_matrix)

# check for accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", round(accuracy, 2)))







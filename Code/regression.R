
rm(list=ls())
requiredPackages <- c("knitr",	"ggplot2",	"plyr",	"dplyr",	"corrplot",	"caret",	"gridExtra",	"scales",	"Rmisc",	"ggrepel",	
                      "randomForest",	"psych",	"xgboost",	"ggthemes",	"mice",	"data.table"
                      # "data.table",	"tidyverse",	"magrittr",	"tibble",	"writexl",	"haven",	"RPostgreSQL",	"arsenal",	"lubridate", "ggplot2", "shiny",
)
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}


# TODO DEFINE PATH
Dir_main = "//auiag.corp/corpdata/nasfiler3-gPIDUnderwritingHO/ACD_Pricing&Analysis/Team/Ally/Learning&Dev/Kaggle"
PROJECT = "regression_houseprice"

# CREATE NEW FOLDER 
# dir.create(file.path(Dir_main, PROJECT, "Data"))
# dir.create(file.path(Dir_main, PROJECT, "Code"))
# dir.create(file.path(Dir_main, PROJECT, "Submission"))


############################################ 
# IMPORT DATA 
############################################ 
setwd(file.path(Dir_main, PROJECT, "Data"))
train <- fread(file = 'train.csv')
test <- fread(file = 'test.csv')

full  <- bind_rows(train, test) # bind training & test data
summary(full)
str(full)

ggplot(data=full[!is.na(full$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

############################################ 
# NUMERIC DATA 
############################################

numericVars <- which(sapply(full, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')

full <- as.data.frame(full)

all_numVar <- full[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high correlations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")


# check for outliers 
ggplot(data=full[!is.na(full$SalePrice),], aes(x=factor(OverallQual), y=SalePrice))+
  geom_boxplot(col='blue') + labs(x='Overall Quality') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

############################################ 
# IMPORT DATA 
############################################

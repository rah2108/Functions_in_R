###===================================================================================================###
###                                   ASSIGNMENT - 2                                                  ###
###                                  OUTLIER DETECTION                                                 ###
###                                                                                                   ###                
###===================================================================================================###



#========================================================================================================
#1.    PROBLEM 1: 
#========================================================================================================
##   Outliers function
##   Create a function that can detect the presence of outliers in the variables of a dataset
##   The function will accept the data as an argument and return you a matrix indicating
##   which variable has outliers in it. The output is expected as follows:
##
##   OUTPUT:
##                Outlier
##   Variable1    -
##   Variable2    Outlier Detected
##   Variable3    Outlier Detected
##   Variable4    -
##   Variable5    Outlier Detected
##   
##   etc.

#========================================================================================================
#1.A   USER-DEFINED FUNCTIONS
#========================================================================================================
fn_Find_Outliers <- function(data)
  
{
  # empty Vectors
  Variable_Name <- c()
  Variable <- c()
  Variable_Type <- c()
  Least_Val <- c()
  Max_Val <- c()
  Outliers_Present <- c()
  lower <- c()
  upper <- c()
  
  # Reading all Columns in dataset
  for(i in 1:ncol(data))
  {
    Columns <- c(i)
    Variable_Name[i] <- names(data[i])
    
    for(var in Columns)
    {
      
      # if column values are not numeric OR count of unique values us very less
      if(!is.numeric(data[, var]) | (length(unique(data[,var]))/nrow(data)*100 < 5))
      {
        Variable_Type[i] <- ('Categorical')
        
      }
      else if(is.numeric(data[, var]))
      {
        Variable_Type[i] <- ('Numerical')
        {          
          # find the upper and lower limits of the data observations
          #lowest and highest observations that are still inside the region defined by the following limits:
          Lower_Out <- c()
          Upper_Out <- c()
          
          # Upper Limit: Q3 + 1.5 ? IQR 
          upperLimit = min(max(data[,var]), (quantile(data[,var],0.75) + 1.5*IQR(data[,var])))
          
          # Lower Limit: Q1 - 1.5 ? IQR
          lowerLimit = max(min(data[,var]), (quantile(data[,var],0.25) - 1.5*IQR(data[,var])))
          
          lower[i] <- lowerLimit
          upper[i] <- upperLimit
          
          for(j in data[,var])
          {
            if(j < lowerLimit)
            {
              # if obsn is beyond the lower limit, add obsn to lower outliers
              Lower_Out <- c(Lower_Out,j)
            }
          }
          
          for(k in data[,var])
          {
            if(k > upperLimit)
            {
              # if obsn is beyond the upper limit, add obsn to upper outlier
              Upper_Out <- c(Upper_Out,k)
            }
          }          
          
          # append all vectors values
          Variable <- Variable[!is.na(Variable)]
          Lower_Out <- Lower_Out[!is.na(Lower_Out)]
          Upper_Out <- Upper_Out[!is.na(Upper_Out)]
          Least_Val <- Least_Val[!is.na(Least_Val)]
          Max_Val <- Max_Val[!is.na(Max_Val)]
          Variable[i] <- names(data[var])
          Least_Val[i] <- length(Lower_Out)
          Max_Val[i] <- length(Upper_Out)
          
        } 
      }
    }
  }
  
  df_VarType <- data.frame(Variable_Name, Variable_Type)
  df_Outlier <- data.frame(Variable, lower, Least_Val, upper, Max_Val)
  names(df_Outlier)
  colnames(df_Outlier) <- c("Variable", "Lower_Bound", "Count_Lower_Outliers", "Upper_Bound","Count_Upper_Outliers")
  
  df_Outlier <- df_Outlier[-which(is.na(df_Outlier$Variable)),]
  
  rownames(df_Outlier) <- 1:nrow(df_Outlier)
  
  for(x in 1:nrow(df_Outlier))
  {
    if((df_Outlier[x,3] == 0) & (df_Outlier[x,5])== 0)
    {
      Outliers_Present <- c(Outliers_Present, "--None--")
    }
    
    else
      
      Outliers_Present <- c(Outliers_Present, "YES")
  }
  
  df_Outlier$Outliers_Present <- Outliers_Present
  
  df_Outlier=data.frame(df_Outlier)
  
  #library(formattable)
  formattable(df_VarType, 
              Variable_Type = formatter("span", 
                                        style = x ~ style(color = ifelse(x=="Numerical", "green", "red"))))
  
  View(df_VarType)
  
  return(df_Outlier)
  
}


#========================================================================================================
#2.   PROBLEM 2: 
#========================================================================================================
##   Add an extra feature to the function in 3. The function should return an object with
##   two extra columns that gives the count of the outliers when log transformation of the
##   variable is taken.

#========================================================================================================
#2.A    USER-DEFINED FUNCTIONS
#========================================================================================================
fn_Log_Outliers <- function(data)
  
{
  # empty Vectors
  Variable_Name <- c()
  Variable <- c()
  Variable_Type <- c()
  Least_Val <- c()
  Max_Val <- c()
  Outliers_post_LOG_Transfm <- c()
  lower <- c()
  upper <- c()
  
  # Reading all Columns in dataset
  for(i in 1:ncol(data))
  {
    Columns <- c(i)
    Variable_Name[i] <- names(data[i])
    
    for(var in Columns)
    {
      
      # if column values are not numeric OR count of unique values us very less
      if(!is.numeric(data[, var]) | (length(unique(data[,var]))/nrow(data)*100 < 5))
      {
        Variable_Type[i] <- ('Categorical')
        
      }
      else if(is.numeric(data[, var]))
      {
        Variable_Type[i] <- ('Numerical')
        {
          for(x in 1:nrow(data))
          {
            if(data[x,var] == 0)
              data[x,var] <- log10(data[x,var]+1)
            else
              data[x,var] <- log10(data[x,var])
          }
          # find the upper and lower limits of the data observations
          #lowest and highest observations that are still inside the region defined by the following limits:
          Log_Lower_Out <- c()
          Log_Upper_Out <- c()
          
          # Upper Limit: Q3 + 1.5 ? IQR 
          upperLimit = min(max(data[,var]), (quantile(data[,var],0.75) + 1.5*IQR(data[,var])))
          
          # Lower Limit: Q1 - 1.5 ? IQR
          lowerLimit = max(min(data[,var]), (quantile(data[,var],0.25) - 1.5*IQR(data[,var])))
          
          lower[i] <- lowerLimit
          upper[i] <- upperLimit
          
          for(j in data[,var])
          {
            if(j < lowerLimit)
            {
              # if obsn is beyond the lower limit, add obsn to lower outliers
              Log_Lower_Out <- c(Log_Lower_Out,j)
            }
          }
          
          for(k in data[,var])
          {
            if(k > upperLimit)
            {
              # if obsn is beyond the upper limit, add obsn to upper outlier
              Log_Upper_Out <- c(Log_Upper_Out,k)
            }
          }          
          
          # append all vectors values
          Variable <- Variable[!is.na(Variable)]
          Log_Lower_Out <- Log_Lower_Out[!is.na(Log_Lower_Out)]
          Log_Upper_Out <- Log_Upper_Out[!is.na(Log_Upper_Out)]
          Least_Val <- Least_Val[!is.na(Least_Val)]
          Max_Val <- Max_Val[!is.na(Max_Val)]
          Variable[i] <- names(data[var])
          Least_Val[i] <- length(Log_Lower_Out)
          Max_Val[i] <- length(Log_Upper_Out)
          
        }
      }
    }
    
  }
  
  df_VarType <- data.frame(Variable_Name, Variable_Type)
  df_Log_Outlier <- data.frame(Variable, lower, Least_Val, upper, Max_Val)
  
  colnames(df_Log_Outlier) <- c("Variable", "Lower_Bound_LOG", "Count_Lower_Outliers_LOG", "Upper_Bound_LOG","Count_Upper_Outliers_LOG")
  
  df_Log_Outlier <- df_Log_Outlier[-which(is.na(df_Log_Outlier$Variable)),]
  
  rownames(df_Log_Outlier) <- 1:nrow(df_Log_Outlier)
  
  for(x in 1:nrow(df_Log_Outlier))
  {
    if((df_Log_Outlier[x,3] == 0) & (df_Log_Outlier[x,5])== 0)
    {
      Outliers_post_LOG_Transfm <- c(Outliers_post_LOG_Transfm, "--None--")
    }
    
    else
      
      Outliers_post_LOG_Transfm <- c(Outliers_post_LOG_Transfm, "YES")
  }
  
  df_Log_Outlier$Outliers_post_LOG_Transfm <- Outliers_post_LOG_Transfm
  df_Log_Outlier=data.frame(df_Log_Outlier)
  
  library(formattable)
  formattable(df_VarType, list(
    Variable_Type = formatter("span", style = x ~ style(color = ifelse(x=="Numerical", "green", "red")))))
  
  View(df_VarType)
  
  return(df_Log_Outlier)
  
}

#========================================================================================================
#2.C MAIN PROGRAM
#========================================================================================================

# 2.C.i Define the libraries and datasets
#====================================================================
install.packages("formattable")
library(formattable)

getwd()                                 
#cars = read.csv("cars.csv")
#Attr = read.csv("Attrition.csv")
data(Boston)

# 2.C.ii Check for Numeric Outliers in any dataset of your choice
#====================================================================

#------- CARS --------#

#Outlier_cars = fn_Find_Outliers(cars)

#formattable(Outlier_cars, list(
#  area(col = c(Count_Lower_Outliers, Count_Upper_Outliers)) ~ normalize_bar("lightblue"), 
#  Outliers_Present = formatter("span", style = x ~ style(color = ifelse(x=="YES", "green", "red")))))###

#View(Outlier_cars)

#----- ATTRITION -----#

Outlier_Attr = fn_Find_Outliers(Attr)

formattable(Outlier_Attr, list(
  area(col = c(Count_Lower_Outliers, Count_Upper_Outliers)) ~ normalize_bar("lightblue"), 
  Outliers_Present = formatter("span", style = x ~ style(color = ifelse(x=="YES", "green", "red")))))

View(Outlier_Attr)

# 2.C.iii Check for Log Converted Outliers in any dataset of your choice
#========================================================================

#------- CARS --------#
#log_Outlier_cars = fn_Log_Outliers(cars)

#formattable(log_Outlier_cars, list(
#  area(col = c(Count_Lower_Outliers_LOG, Count_Upper_Outliers_LOG)) ~ normalize_bar("lightblue"), 
#  Outliers_post_LOG_Transfm = formatter("span", style = x ~ style(color = ifelse(x=="YES", "green", "red")))))#

#View(log_Outlier_cars)


#----- ATTRITION -----#

log_Outlier_Attr = fn_Log_Outliers(Attr)

formattable(log_Outlier_Attr, list(
  area(col = c(Count_Lower_Outliers_LOG, Count_Upper_Outliers_LOG)) ~ normalize_bar("lightblue"), 
  Outliers_post_LOG_Transfm = formatter("span", style = x ~ style(color = ifelse(x=="YES", "green", "red")))))

View(log_Outlier_Attr)

# 2.C.iv Merge and Compare the results
#====================================================================

#------- CARS --------#
#Merge_Cars <- merge(x = Outlier_cars, y = log_Outlier_cars, by = "Variable",all=TRUE)

#formattable(Merge_Cars, list(
#  area(col = c(Count_Lower_Outliers, Count_Lower_Outliers_LOG, Count_Upper_Outliers, Count_Upper_Outliers_LOG)) 
#  ~ normalize_bar("lightblue"), 
#  Outliers_Present = formatter("span", style = x ~ style(color = ifelse(x=="YES", "green", "red"))), 
#  Outliers_post_LOG_Transfm = formatter("span", style = x ~ style(color = ifelse(x=="YES", "green", "red")))))

#View(Merge_Cars)

#----- ATTRITION -----#
Merge_Attr <- merge(x = Outlier_Attr, y = log_Outlier_Attr, by = "Variable",all=TRUE)

formattable(Merge_Attr, list(
  area(col = c(Count_Lower_Outliers, Count_Lower_Outliers_LOG, Count_Upper_Outliers, Count_Upper_Outliers_LOG)) 
  ~ normalize_bar("lightblue"), 
  Outliers_Present = formatter("span", style = x ~ style(color = ifelse(x=="YES", "green", "red"))),
  Outliers_post_LOG_Transfm = formatter("span", style = x ~ style(color = ifelse(x=="YES", "green", "red")))))

View(Merge_Attr)


#========================================================================================================
#3.    PROBLEM 3: 
#========================================================================================================
# Try to create the percentage of missing values function using the for loop.

#========================================================================================================
#3.A    USER-DEFINED FUNCTIONS
#========================================================================================================
fn_Missing_Values1 <- function(data)
{
  # empty vectors
  Variable_Name <- c()
  Count_Miss_Values <- c()
  Percent_Miss_Values <- c()
  
  for(i in 1:ncol(data))
  {
    Count_Miss_Values <- c(Count_Miss_Values, sum(is.na(data[,i])))
    Percent_Miss_Values <- round(c(Percent_Miss_Values, sum(is.na(data[,i]))/length(data[,i])*100), digits = 3)
    Variable_Name <- c(Variable_Name, names(data[i]))
  }
  
  df_Missing <- data.frame(Variable_Name, Count_Miss_Values, Percent_Miss_Values)
  
  return(df_Missing)
}

#========================================================================================================
#3.B  MAIN PROGRAM
#========================================================================================================

#3.B.i  Define the libraries and datasets
#====================================================================
install.packages("formattable")
library(formattable)

getwd()                                 


#3.B.ii  Check for Numeric Outliers in any dataset of your choice
#====================================================================

#------- testing for dataset Attrition.csv --------# 

#data = read.csv("Attrition.csv")
data(Boston)
fn_Missing_Values1(data)
formattable(data, list(
  Count_Miss_Values = formatter("span", style = x ~ style(color = ifelse(x>0, "blue", NA))),
  Percent_Miss_Values = color_bar("lightpink")))

View(data)
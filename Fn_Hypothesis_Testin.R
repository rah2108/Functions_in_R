###===================================================================================================###
###                                Hypothesis Testing :                                               ###
###                                For Binary Variables                                               ###                
###===================================================================================================###

#======================================================================================================== 
# 1. PROBLEM 1: 
#======================================================================================================== 
# 
# SUGGESTION 1:
# Often we are not required the graphs for all the numeric variables. Try to improve the code
# by adding an additional parameter "variable" that can take a vector of variable index and 
# return the graphs for only those variables.
#
# Example: Graphs(Boston, var = c(1, 3, 4))
# Below function Will generate the graphics for only the numerical variables given as arguments 
# along with dataframe, at time of function call.
#======================================================================================================== 

#------- fn_Hyp_Test --------#

# Requires 2  arguments:
# X: Dataframe excluding the column with target Variable  (Mandatory)
# Y: Column defined as Target Variable (Mandatory)



fn_Hyp_Test <- function(df_x,col_Y)
{
  if (is.data.frame(df_x)&&length(unique(col_Y))==2)
  {
    vect(P-Val) = c()
    test_Cal_chi = c()
    test_Cal_t = c()
    for (i in 1:ncol(df_x))
    {
      if (class(i)=="factor"|((length(unique(df_x[,i]))/nrow(df_x)*100)<10&&(length(unique(df_x[,i])))>1))
      {
        test <- chisq.test(df_x[,i],col_Y)
        vect(P-Val)[i] = test$p.value
        test_Cal_chi[i] = test$statistic
      }
      else if(is.numeric(df_x[,i]) && (length(unique(df_x[,i])))>1)
      {
        a <- df_x[,i][col_Y == unique(col_Y)[1]]
        b <- df_x[,i][col_Y == unique(col_Y)[2]]
        test = t.test(a,b)
        vect(P-Val)[i] = test$p.value
        test_Cal_t[i] = test$statistic
      }
    }
    
    Table  =  cbind(vect(P-Val),test_Cal_chi,test_Cal_t)
    rownames(Table)  =  names(df_x)
    colnames(Table)=c("P-value","t-Statistic","Chi-Sqr-Statistic")
    return(Table)
    
    if (class(col_Y)=="Numeric")
    {
      print("ANOVA Test")
      print(aov(col_Y~as.matrix(df_x)))
    }
    
  }
  else
  {
    print("Target variable is not binary")
  }
}

#======================================================================================================== 
# MAIN PROGRAM
#======================================================================================================== 

# Define the libraries and datasets
#==================================================================== 


getwd()                                 
data = read.csv("Attrition.csv")

View(data)
dataFrm_x = data[, -2]
column_Y = data[, 2]
fn_Hyp_Test(dataFrm_x, column_Y)
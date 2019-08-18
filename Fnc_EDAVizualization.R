###===================================================================================================###
###                          EDA: Plotting Graphs for both                                            ###
###                       Numerical and Categorical Variables                                         ###
###                     and Storing in a given target Directory                                       ###
###===================================================================================================###
#
# Takes 4  arguments:
# Mandatory:
# Arg 1: dataframe
# Optional:
# Arg 2: Name of dataframe -> useful in ceating Target Directory   (Default = "dataset")
# Arg 3: Directory where Target Folder is to be created            (Default = "D:\\")
# Arg 4: Vector of Selected Column Numbers                         (Default = All Columns in dataset)

GraphToDir <- function(data, folder = "dataset", location="D:\\", variable = c(1:ncol(data)))
{
  Variable_Name <- c()
  Variable_Type <- c()
  Graph_Type <- c()
  
  date = format(Sys.time(), "%Y%m%d_%H%M%S")
  fileDir = paste(location, "EDA", sep = '')
  filePath = paste(fileDir, folder, date, sep = '_')
  print(paste("Creating Directory: ",filePath))
  ifelse(!dir.exists(file.path(filePath)), dir.create(file.path(filePath)), print("Dir already exists !"))
  
  if (length(variable)==0)
  {
    variable = c(1:ncol(data))
  }
  for(var in variable)
  {
    Variable_Name[var] <- names(data)[var]
    
    if(!is.numeric(data[,var]) | (length(unique(data[,var]))/nrow(data)*100 < .5))
    {
      if (length(unique(data[,var]))/nrow(data)*100 > 5)
      {
        Variable_Type[var] <- (' NA ')
        Graph_Type[var] <- ('-- NA for Graphs --')
      }
      else
      {
        Variable_Type[var] <- ('Categorical')
        Graph_Type[var] <- (' Bar Plot & Pie Chart ')
        
        frequencies <- table(data[var])
        
        count_slices <- c(frequencies)
        labels_slices <- c(frequencies)
        
        fname = paste(names(data)[var], date, sep="_")
        png(paste(fname, ".png", sep="")) 
        
        par(mfrow=c(2,1))
        pie(count_slices, labels = labels_slices, main = paste("Pie chart for", names(data[var])), col = rainbow(length(count_slices)))
        barplot(frequencies, main = paste("Barplot of", names(data[var])), col = rainbow(length(count_slices)))
        dev.off()
      }
      
    }
    else if(is.numeric(data[,var]))
    {
      Variable_Type[var] <- ('Numerical')
      Graph_Type[var] <- (' Histogram & Box Plot ')
      
      fname = paste(names(data)[var], date, sep="_")
      png(paste(fname, ".png", sep="")) 
      
      par(mfrow=c(2,1))
      boxplot(data[,var], main = paste("Boxplot for", names(data)[var]), ylab = names(data)[var], col = "pink", border = "blue", horizontal = T)
      hist(data[,var], main = paste("Histogram for", names(data)[var]), xlab = names(data)[var], ylab = "Count", col = "coral", border=F)
      
      dev.off()
    }
  }
  
  graphs = list.files(pattern = paste("*", basename(date),"*.png", sep=''))
  print(paste("Moving below files to :", filePath))
  for (f in graphs)
  {
    print(paste(" moving File : ", f))
  }
  file.move(graphs, filePath)
  
  df_VarType <- data.frame(Variable_Name, Variable_Type, Graph_Type)
  View(df_VarType)
}


#========================================================================================================
# 6.  MAIN PROGRAM
#========================================================================================================

# 6.A  Define the libraries and datasets
#====================================================================
install.packages("filesstrings")
library(filesstrings)

getwd()                                 
data1 = read.csv("cars.csv")
data2 = read.csv("Attrition.csv")

# 6.B.  Call the Functions on any dataset of your choice
#====================================================================
GraphToDir(data)
GraphToDir(data2,"Attrition",c(2,4,5,6,9,10))




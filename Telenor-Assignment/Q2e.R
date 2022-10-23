################################################################################
# e. Find any interesting trends/behaviors that you encounter when you analyze the dataset.
################################################################################

rm(list=ls()) # clear all gloabl and local environment variables
setwd("/home/mnaeem/Downloads/tel")  # change it according to your local folder

################################################################################
#**************  Muhammad.Naeem@univ-lyon2.fr  *******************
#*********************  Question 2 (c) ***************************
################################################################################

AnalyseEntityWise <- function(df, rwName1, rwName2) { 

# initialize factos elements to shape up the final structure (matrix)
# they all holds the results 
subs <- c("Unknown", "fire", "shot down", "weather", "storm", "engine failure", "pilot error") 
crashes <- as.numeric(as.data.frame(matrix(0, ncol =length(subs), nrow = 1)))
deaths <- crashes

# crux of the logic, a few loops with if and else
for (i in 1:length(df$Summary)) {
  mainText <- as.character(df$Summary[i])
  
  if (nchar(mainText) == 0) {
    crashes[1] <- crashes[1] + 1   
    
    # deals only Unknown cases   
    if  (nchar(as.character(df$Fatalities[i])) > 0) {
        if (is.na(df$Fatalities[i]) == FALSE  ) {
          deaths[1] <- deaths[1] + as.numeric(df$Fatalities[i])     
        }
    }
  }
 
  # deals all cases except 'Unknown'   
  else { 
  for (j in 2:length(subs)) {
      if (grepl(subs[j], tolower(mainText) ) == TRUE) {
        crashes[j] <- crashes[j] + 1
        
        if  (nchar(as.character(df$Fatalities[i])) > 0) {
          if (is.na(df$Fatalities[i]) == FALSE  ) {
            deaths[j] <- deaths[j] + as.numeric(df$Fatalities[i])     
          }
        }
      }
  }
 }  
}

# develope final matrix to hold number of crashes and deaths 
# given 7 clustering parameters (labels)

m <- matrix(0, ncol = length(subs) , nrow = 0)
d <- data.frame(m)
d = rbind(d,crashes)
colnames(d) <- subs
d = rbind(d,deaths)
rownames(d) <- c(rwName1, rwName2)

# print ( paste("Total Crashes: ", nrow(df), sep="" ))  
# print(head(d))
return(d)

}


################################################################################
source("funcQ2b.R")
# load the data
df <- read.csv("3-Airplane_Crashes_Since_1908.txt", header = TRUE, sep = ",") # df hold all dataset
# crashes by Type of aircraft
obj <- funcQ2b(df$Type, "TypeCloud.png", "Type", 20)
cat("\014") # clear the console

allObj <- AnalyseEntityWise(df, "Total-Crashes", "Total-Deaths") # analysis with all

for (i in 1:length(obj$x)) {
  
  txt <-  as.character(obj$x[i]) 
  
  newDf <- (subset(df, df$Type == txt))
  
  # cells showing no Type (empty cells) should be writted as Unspecified in table
  if (nchar(txt) == 0 ) {
    txt <- "UnSpecified"
  }
  
  out <- AnalyseEntityWise(newDf, paste(txt,"Crashes", sep = ".") , paste(txt,"Deaths", sep = "-"))
  allObj <- rbind(allObj, out)
  
}

 # print(allObj)

crashObj <- allObj
evn <- c(seq(nrow(allObj), 2, -2))
crashObj <- crashObj[-evn,] # remove all of the instancces of deaths
crashObj <- crashObj[-1,]  # remove total count

# cat("\014") # clear the console
print(crashObj)
print("Please read the file Q1e.pdf for novel finding /trends / behaviors")
#========================  End  =============================


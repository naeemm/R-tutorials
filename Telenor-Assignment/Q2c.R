################################################################################
# c. ‘Summary’ field has the details about the crashes. 
#     Find the reasons of the crash and categorize them in different clusters 
#     i.e Fire, shot down, weather (for the ‘Blanks’ in the data category can be UNKNOWN) 
#     you are open to make clusters of your choice but they should not exceed 7.
################################################################################

# load the data
rm(list=ls()) # clear all gloabl and local environment variables
setwd("/home/mnaeem/Downloads/tel")  # change it according to your local folder
cat("\014") # clear the console
df <- read.csv("3-Airplane_Crashes_Since_1908.txt", header = TRUE, sep = ",") # df hold all dataset

################################################################################
#**************  Muhammad.Naeem@univ-lyon2.fr  *******************
#*********************  Question 2 (c) ***************************
################################################################################

# initialize factos elements to shape up the final structure (matrix)
# they all holds the results 
d <- data.frame(matrix(0, ncol = 8, nrow = 0))
subs <- c("id","Unknown", "fire", "shot down", "weather", "storm", "engine failure", "pilot error") 

# crux of the logic goes here, a few loops with if and else
for (i in 1:length(df$Summary)) {
  mainText <- as.character(df$Summary[i])
  
  # deals only Unknown cases   
  if (nchar(mainText) == 0) {
    
    newrow = c(0:0)
    d = rbind(d,newrow)  # insert new row with all zero elements
    d[i,1] <- i # row id
    d[i,2] <- 1 # Unknown
    for (j in 3:length(subs)) {
      d[i,j] <- 0  
    }
  }
  
  # deals all cases except 'Unknown'   
  else {
    newrow = c(-1:(length(subs)-2))
    d = rbind(d,newrow)
    d[i,1] <- i
  
    for (j in 3:length(subs)) {
      if (grepl(subs[j], mainText, ignore.case = TRUE) == TRUE) {
        d[i,j] <- 1
      } else {
        d[i,j] <- 0
      }
    }
  }  
}

# change arbitrary columns into real names from our dataset
colnames(d) <- subs

#==============  Draw venn diagram .. examin overlapping reasons     ===============
# install.packages("venneuler")
library(venneuler)

# reformat the data for venneuler
# without it, wrong overlapping appears
library(reshape2)

# we melt dataframe matrix into 3 col dataframe e.g. 'id, CrashRemarks, Occured' in long
# form
dSets <- melt(d, id = "id")
# Keep id and CrashRemarks columns, use 'Occured' column to
# reduce this to just the set of id with occurence.
dSets <- (subset(dSets, value == 1))[1:2]

# format that venneuler takes: first column is elements
# (id) in each set, second column is set names (CrashRemarks)
plot(venneuler(dSets), main = "Airline Crash Reasons")

## Note
# Overlapping in Unknown and engine failure is misplaced in graph. 
# package 'venneuler' is still suffering from bugs 
# but it is best as other competitors can't display more than 4 in effective way
nrow(subset(d, d$Unknown == 1 & d$`engine failure` == 1))
nrow(subset(d, d$Unknown == 1))
nrow(subset(d, d$`engine failure` == 1))

head(d, n=10L) # snippet of the true/false matrix

#========================  End  =============================


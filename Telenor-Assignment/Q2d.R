################################################################################
# d. Find the number of crashed aircrafts 
#    and 
#    number of deaths against each category from above step (Q.2c)
################################################################################

# load the data
rm(list=ls()) # clear all gloabl and local environment variables
setwd("/home/mnaeem/Downloads/tel")  # change it according to your local folder
# dev.off(dev.list()["RStudioGD"]) # clears all graph, requires momentary pause in code execution for refreshing otherwise plotting error
cat("\014") # clear the console
df <- read.csv("3-Airplane_Crashes_Since_1908.txt", header = TRUE, sep = ",") # df hold all dataset

################################################################################
#**************  Muhammad.Naeem@univ-lyon2.fr  *******************
#*********************  Question 2 (c) ***************************
################################################################################

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
rownames(d) <- c("Crashes", "Deaths")

# pairwise analysis by plotting
for (i in 1:(length(subs)-1)) {
  for (j in (i+1):length(subs)) { 
    
    # drwas the unit plot between two variables   # 
    plot(d[,i] ~ d[,j], 
        xlim = c(0, pmax(max(d[,i]), max(d[,j]))), 
        xlab = colnames(d)[j] , ylab = colnames(d)[i], 
        main = 'Crash Reasons', data = d[1:2,], 
        col = rgb(runif(5),runif(5),runif(5)) )  
    # label it 
    with(d[1:2,], 
         text(d[,i] ~ d[,j], 
         labels = row.names(d[1:2,]), pos = 4))
    
  }
  
  
}

# All analysis in one graph
plot(d)

# plot(d$Unknown~d$fire, xlim = c(0, pmax(max(d$Unknown), max(d$fire))), xlab = colnames(d)[2] , ylab = colnames(d)[1], main = 'Crashes Reasons', data = d[1:2,])
# with(d[1:2,], text(d$Unknown~ d$fire , labels = row.names(d[1:2,]), pos = 4))

# detail out the tabulated information
head(d)

#========================  End  =============================


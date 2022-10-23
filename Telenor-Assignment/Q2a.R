################################################################################
# a. Yearly how many planes crashed? 
#    how many people were on board? 
#    how many survived? 
#    how many died?
################################################################################

# load the data
rm(list=ls()) # clear all gloabl and local environment variables
setwd("/home/mnaeem/Downloads/tel")  # change it according to your local folder
cat("\014") # clear the console
df <- read.csv("3-Airplane_Crashes_Since_1908.txt", header = TRUE, sep = ",") # df hold all dataset

################################################################################
#**************  Muhammad.Naeem@univ-lyon2.fr  *******************
#*********************  Question 2 (a) ***************************
################################################################################
mdy <- strsplit(as.character(df$Date),'/') # extract month Day and Year in a list structure
mdy <- do.call(rbind.data.frame, mdy)  # need to bind to shape into a table
# a <- as.data.frame(table(mdy[3]))
Year <- table(mdy[3]) # 1=month, 2=Day, 3=Year,  We are interested only in Year
# summary(Year)
# different types of plots, anyone can explain the phenomen well
plot(Year, type = "p", col = "red", main = "Airline Crash", xlab = "Year", ylab = "Count")
plot(Year, type = "l", col = "blue", main = "Airline Crash", xlab = "Year", ylab = "Count")
plot(Year, type = "b", col = "green", main = "Airline Crash", xlab = "Year", ylab = "Count")
plot(Year, type = "c", col = rgb(0.3,0.3,0.4), main = "Airline Crash", xlab = "Year", ylab = "Count")
plot(Year, type = "o", col = rgb(0.1,0.3,0.6), main = "Airline Crash", xlab = "Year", ylab = "Count")
plot(Year, type = "h", col = rgb(0.6,0.3,0.1), main = "Airline Crash", xlab = "Year", ylab = "Count")
plot(Year, type = "s", col = rgb(0.5,0,0.5), main = "Airline Crash", xlab = "Year", ylab = "Count")
plot(Year, type = "S", col = rgb(0,0.5,0.5), main = "Airline Crash", xlab = "Year", ylab = "Count")
################################################################################

# build the frame and then aggregate on year and personsOnBoard
OnboardFrame <- data.frame(YearOfFlight=factor(mdy[[3]]), Onboard=matrix(df$Aboard))
OnboardRes <- aggregate(OnboardFrame$Onboard, by=list(YearOfFlight=OnboardFrame$YearOfFlight), FUN=sum)

# build the frame and then aggregate on year and Casualities (Fatalities is the column name in given data)
FatalitiesFrame <- data.frame(YearOfFlight=factor(mdy[[3]]), Fatalities=matrix(df$Fatalities))
FatalitiesRes <- aggregate(FatalitiesFrame$Fatalities, by=list(YearOfFlight=FatalitiesFrame$YearOfFlight), FUN=sum)

# implied variable
surv <- df$Aboard - df$Fatalities
survFrame <- data.frame(YearOfFlight=factor(mdy[[3]]), surv=surv)
survRes <- aggregate(survFrame$surv, by=list(YearOfFlight=OnboardFrame$YearOfFlight), FUN=sum)

# Draw three lines in same plot
plot(OnboardRes, xlab = "Year", ylab = "Total")

lines(OnboardRes, xlab = "Year", ylab = "Total", col="red")  # fix the width of line by lwd=2
lines(FatalitiesRes, xlab = "Year", ylab = "Total", col="green")
lines(survRes, xlab = "Year", ylab = "Total", col="blue")

title(main = "Aircrach Passengers & Crew Members", 
      col.main=rgb(0.5,0.2,0.3), font.main=4)

title(xlab="Year", col.lab=rgb(0.5,0.3,0.2))
title(ylab="Total", col.lab=rgb(0.5,0.3,0.2))

legend("topleft", c("Onboard","Died", "Survived"), 
       col=c("red","green","blue"), 
       ncol = 1, 
       lwd=1, 
       bty="n" , 
       text.font=3)

#========================  End  =============================


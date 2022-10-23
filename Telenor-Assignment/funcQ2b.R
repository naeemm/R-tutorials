funcQ2b <- function(Operator, ImgName, Feature, lmt1) {

  library(plyr)
  d <- count(Operator)
  lmt2 <- (max(d$freq) +1 )
  d <- d[(d$freq > lmt1) & (d$freq < lmt2),]
  
  retObj <- d # return object for analysis in Q.2.e
  
  mainTitle <- paste ("Aircrach (>" , lmt1 , " and <", lmt2 ,") by " , Feature, sep = "", collapse = NULL)
  plot(d$freq ~ factor(d$x), d, las=2,  xlab="", ylab="counts", main=mainTitle, type="p", color="red")
  pie(d$freq, factor(d$x), main=mainTitle, col = rainbow(12)) # terrain.colors(12) is also impressive
  
  ################################################################################
  
  library("RColorBrewer")
  library("wordcloud")
  
  d<-data.frame(theNames=Operator)
  
  tb<-table(d$theNames)
  set.seed(1234)
  png(ImgName, width=700,height=600)
  wordcloud(names(tb),as.numeric(tb), 
            min.freq=10,max.words=1000, 
            random.order=T, rot.per=.15, 
            colors=brewer.pal(8,"Dark2"), 
            vfont=c("sans serif","plain"))
  dev.off()
  
  # display.brewer.all() # choose best color set according to data density
  msg <- paste("See the image file ", ImgName, " in current folder " )
  print (msg)
  msg <- "One Plot chart and one Pie chart loaded in chart area"
  print (msg)
  
  # return object for analysis in Q.2b and Q.2e
  return (retObj)  
}

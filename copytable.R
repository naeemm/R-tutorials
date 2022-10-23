cat("Use this for copying tabular data from R into Word or Excel\n")
cat("by Michael Hahsler (http://michael.hahsler.net)\n\n")
cat("Usage example: copytable(summary(iris))\n\n")
cat("It will open a HTML version of the table in your Web browser and ")
cat("you can copy&paste into programs like Word and Excel.\n\n")

cat("License: This work is licensed under the Creative Commons Attribution 4.0 International License (http://creativecommons.org/licenses/by/4.0/)\n")

if(!"xtable" %in% rownames(installed.packages())) install.packages("xtable")
library(xtable)

copytable <- function(x, ...) {
  f <- tempfile(fileext=".html")
  print(xtable(x, ...), "html", file = f)
  browseURL(f)
}


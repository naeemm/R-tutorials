library(venneuler)
MyVenn <- venneuler(c(A=74344,B=33197,C=26464,D=148531,"A&B"=11797, "A&C"=9004,"B&C"=6056,"A&B&C"=2172,"A&D"=0,"A&D"=0,"B&D"=0,"C&D"=0))
MyVenn$labels <- c("A\n22","B\n7","C\n5","D\n58")
plot(MyVenn)
text(0.59,0.52,"1")
text(0.535,0.51,"3")
text(0.60,0.57,"2")
text(0.64,0.48,"4") 

library(venn)
library(VennDiagram)

venn.diagram(list(B = 1:1800, A = 1571:2020),fill = c("red", "green"),
             alpha = c(0.5, 0.5), cex = 2,cat.fontface = 4,lty =2, fontfamily =3, 
             filename = "trial2.emf")

## modified slightly from the example given in the documentation
## Example using a list of item names belonging to the
## specified group.
##
require(gplots) 
## construct some fake gene names..
oneName <- function() paste(sample(LETTERS,5,replace=TRUE),collapse="")
geneNames <- replicate(1000, oneName())

## 
GroupA <- sample(geneNames, 400, replace=FALSE)
GroupB <- sample(geneNames, 750, replace=FALSE)
GroupC <- sample(geneNames, 250, replace=FALSE)
GroupD <- sample(geneNames, 300, replace=FALSE)

venn(list(GrpA=GroupA,GrpB=GroupB,GrpC=GroupC,GrpD=GroupD))

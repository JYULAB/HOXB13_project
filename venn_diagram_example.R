setwd("D:\\sarki\\han")
library(Vennerable)

venn<- read.table("GSM935508vsGSM935408vsds015.txt", header = TRUE, sep = "\t")
c =   venn[1,4]
b =   venn[2,4]
cb =  venn[3,4]
a =   venn[4,4]
ca =  venn[5,4]
ab =  venn[6,4]
abc = venn[7,4]

Vdemo3 <- Venn(SetNames = c("a", "b", "c"), Weight = c(0,a,b,ab,c,ca,cb,abc))
plot(Vdemo3, doWeights = TRUE)
savePlot(filename = "xxxxxxx overlapping", type = "pdf")

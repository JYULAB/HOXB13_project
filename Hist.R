#This is a template
DataFilePath <- paste("$$DataDir", "/DataR/",sep = "")
setwd(DataFilePath )
pdfOutPath <- paste("$$DataDir", "/PDF",sep = "")

#step2, file
OpenFile="$$FileName"

#step3, title,label
myTitle="View signal density "
LegTitle=""
#yLabel =" ChIP-seq Count"
#yLabel ="motif per bp per peak"
yLabel ="tag counts"

xLabel ="Distance to Peak Center (bp)"
#xLabel ="Distance to TSS (bp)"

hist<- read.table(OpenFile, header=T, sep="\t")
x <- hist$Distance
myRow <- 1:nrow(hist)  
#step4, define yMax
#yMax <- 2.5
#get max value of all columns except Distance
yMax <- max(hist[1:nrow(hist),names(hist)[2:ncol(hist)]]) 
#names(hist)
count=1 
y_range <- range(0, yMax)
pdfOutFile=paste(OpenFile,".pdf",sep="")
pdf(file.path(pdfOutPath, pdfOutFile), height=5, width=5) 
listcolor = c("red","green","orange","blue","purple","black","gray","yellow","cyan","pink","gold3")
for( n in names(hist) ) {
	     if ( n=="Distance") next
	    if ( count==1 ) plot(x, hist[myRow ,n],  ylim=y_range, xlab =xLabel  , ylab =yLabel , main =paste(myTitle, "\n",OpenFile),cex.main = 0.7,col="white" )
	    count= count +1     
	     lines(smooth.spline(x, hist[myRow ,n], spar=0.4,),col=listcolor[count-1], lwd=2)
}
leg.txt <- c( names(hist)[2:ncol(hist)]);
legend("topleft", legend = leg.txt, pch=".", cex=0.6,lty=1, lwd=2, col=listcolor,title=LegTitle)
#legend("topleft", legend = leg.txt, pch=".", cex=0.6,lty=1, lwd=2, col=2:count,title=LegTitle)
#axis(2, at=seq(0, 21, by=3) , labels= seq(0, 21, by=3))
#dev.off()

library(scales)

setwd("C:\\work\\xiaodong\\2021_05_15\\result_08_09_21")    

returns <- runif(100)
yticks_val <- pretty_breaks(n=10)(returns)

a1 <- scan("high.txt")
a2 <- matrix(a1,ncol=101,byrow=T)
a3 <- apply(a2[2:50,],2,mean, na.rm=TRUE)
b1 <- scan("low.txt")
b2 <- matrix(b1,ncol=101,byrow=T)
b3 <- apply(b2[2:50,],2,mean, na.rm=TRUE)


plot(cbind(a2[1,]/1000,a3),type="h",xlab="HOXB13 location (kb) at Chromosome 17",ylim=c(0,1),ylab="Percentage methylation",col=rgb(red = 1, green = 0, blue = 0, alpha = 1),pch = 0, lwd = 3, cex = 8, yaxt="n")
points(cbind(b2[1,]/1000,b3),type="h",col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2), pch = 0, lwd = 3, cex = 8)
#plot(cbind(b2[1,]/1000,b3),type="p",xlab="HOXB13 location (kb) at Chromosome 17",ylim=c(0,1),ylab="Percentage methylation",col=rgb(red = 0, green = 0, blue = 1, alpha = 1),pch = 16, lwd = 3, cex = 1, yaxt="n")
#points(cbind(a2[1,]/1000,a3),type="p",col = rgb(red = 1, green = 0, blue = 0, alpha = 1), pch = 16, lwd = 3, cex = 1)
legend('topright', c("HOXB13 low", "HOXB13 high"), lty=1, col=c('blue', 'red'), bty='n', cex=1, lwd = 3)
axis(2, at=yticks_val, lab=percent(yticks_val, trim=FALSE, scale = 100, accuracy=1))
abline(v=48728750/1000, col="orange", lty=2)  ## TSS
abline(v=48724763/1000, col="black", lty=2)   ## TES
text(locator(), labels = c("TSS", "TES"))

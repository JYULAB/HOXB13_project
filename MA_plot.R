------MA plot by log2 value add intensity of HOXB13 (m854) as color
data <- read.table("./input/HDAC3_peaks_data_for_ma_plot_tag_counts_m682_vs_m683_add_m854.txt", header=TRUE)
#data = data[data$control>=1 & data$treatment>=1,]
data = data[order(data$HOXB13),]   # sort by intensity of HOXB13
a <- data$control
b <- data$treatment
c <- data$HOXB13

x = (a+b)/2
y = a-b

#install.packages("table1")
library("table1")  # for eqcut function
library(xfun) # for eqcut function

k = eqcut(c,20)

library(RColorBrewer)
#rbPal <- colorRampPalette(brewer.pal(20, "YlOrRd"))
rbPal <- colorRampPalette(c("navy", "green", "yellow", "firebrick3"))
my_color <- rbPal(20)[as.numeric(k)]
plot(x, y, cex=0.5, xlab = "Average HDAC3 enrichment (log2)", xlim=c(0,20), ylab = "Log2 Fold HDAC3 enrichment (pGIPZ / shHoxB13)", col = my_color, pch = 8, frame = FALSE)
#legend("topright",legend=c(seq(2,20,by=2)),col =rbPal(20),pch=8)
abline(h=1, col="black", lty = 2)
abline(h=-1, col="black", lty = 2)


up = length(y[y>=1])
down = length(y[y<=-1])
unchanged = length(y[y<1 & y>-1])

text(16, -4, up, cex = .8, col="black")
text(16, -7, unchanged, cex = .8, col="black")
text(16, -10, down, cex = .8, col="black")
savePlot(filename = "./result/HDAC3_MA_plot_m682_vs_m683_addm854", type = "pdf")

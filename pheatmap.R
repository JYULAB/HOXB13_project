library(pheatmap)

setwd("D:\\kevin\\2020_07_12")
heatdata1=read.table("USP7_BUB3_FOXA1_co_regulated_gene_pheatmap.txt", sep="\t", header=TRUE)
heatdata2=heatdata1[,2:13]
rownames(heatdata2) = heatdata1[,1]

drows = dist(heatdata2, method = "minkowski")
dcols = dist(t(heatdata2), method = "minkowski")

pdf(file = "USP7_BUB3_FOXA1_co_regulated_gene_pheatmap.pdf", onefile=FALSE)
map = pheatmap(heatdata2, cellwidth = 10, cellheight = 1, scale="row", breaks = seq(-3, 3, 0.01), 
		border_color="gray", cluster_cols = FALSE, cluster_row=TRUE, 
		color = colorRampPalette(c("navy", "white", "firebrick3"))(600), fontsize_row=8, fontsize_col=8, fontsize=8) 

##Extracting gene names of a cluster in heatmap
heatdata2[map$tree_row$order,]
write.table(heatdata2[map$tree_row$order,], "USP7_BUB3_FOXA1_co_regulated_gene_pheatmap_labels.txt",sep="\t", row.names=TRUE)

dev.off()

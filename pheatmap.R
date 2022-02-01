library(pheatmap)
heatdata1=read.table("./input/HOXB13_regulated_gene_rnaseq_heatmap_in_mr163-mr170_by_foldchange.txt", sep="\t", header=TRUE)
heatdata2=heatdata1[,2:3]
rownames(heatdata2) = heatdata1[,1]

drows = dist(heatdata2, method = "minkowski")
dcols = dist(t(heatdata2), method = "minkowski")

pdf(file = "./result/HOXB13_regulated_gene_rnaseq_heatmap_in_mr163-mr170_by_foldchange_pheatmap.pdf", onefile=FALSE)
pheatmap(heatdata2, cellwidth = 20, cellheight = 1, scale="none", breaks = seq(-3, 3, 0.001), 
		border_color="gray", cluster_cols = FALSE, cluster_row=FALSE, 
		color = colorRampPalette(c("navy", "white", "firebrick3"))(6000), fontsize_row=5, fontsize_col=4, fontsize=6) 
dev.off()








library(pheatmap)
heatdata1=read.table("./input/HDAC3_regulated_gene_rnaseq_heatmap_in_mr163-mr170_by_foldchange.txt", sep="\t", header=TRUE)
heatdata2=heatdata1[,2:3]
rownames(heatdata2) = heatdata1[,1]

drows = dist(heatdata2, method = "minkowski")
dcols = dist(t(heatdata2), method = "minkowski")

pdf(file = "./result/HDAC3_regulated_gene_rnaseq_heatmap_in_mr163-mr170_by_foldchange_pheatmap.pdf", onefile=FALSE)
pheatmap(heatdata2, cellwidth = 20, cellheight = 0.6, scale="none", breaks = seq(-3, 3, 0.001), 
		border_color="gray", cluster_cols = FALSE, cluster_row=FALSE, 
		color = colorRampPalette(c("navy", "white", "firebrick3"))(6000), fontsize_row=5, fontsize_col=4, fontsize=6) 
dev.off()

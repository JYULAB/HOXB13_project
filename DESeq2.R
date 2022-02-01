countdata <- read.table("./input/mr88_mr89_mr90_mr91.txt", header=TRUE, row.names=1)
# Convert to matrix
countdata <- as.matrix(countdata)
head(countdata)

(condition <- factor(c(rep("plko", 2), rep("plko_HoxB13", 2))))

# Analysis with DESeq2 ----------------------------------------------------

library(DESeq2)

(coldata <- data.frame(row.names=colnames(countdata), condition))
dds <- DESeqDataSetFromMatrix(countData=countdata, colData=coldata, design=~condition)
dds

# Run the DESeq pipeline
dds <- DESeq(dds)

# Get differential expression results
res <- results(dds)
table(res$padj<0.05)
## Order by adjusted p-value
res <- res[order(res$padj), ]
## Merge with normalized count data
resdata <- merge(as.data.frame(res), as.data.frame(counts(dds, normalized=TRUE)), by="row.names", sort=FALSE)
names(resdata)[1] <- "Gene"
head(resdata)
## Write results
write.csv(resdata, file="./result/mr88_mr89_mr90_mr91_diffexpr-results.csv")

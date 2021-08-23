setwd("D:\\xiaodong\\2021_05_15\\corr")               
inputFile="input.txt"                                   
gene1="HOXB13_expression"                                            
gene2="methylation_rate"                                               

rt=read.table(inputFile,sep="\t",header=FALSE,check.names=F,row.names=1)
i=rt[gene1,]
j=rt[gene2,]
x=as.numeric(i)
y=log2(as.numeric(j)+1)
corT=cor.test(x,y)
z=lm(y~x)
cor=corT$estimate
cor=round(cor,3)
pvalue=corT$p.value
pval=signif(pvalue,4)
pval=format(pval, scientific = TRUE)

pdf(file="methylation_RNAexpression_cpg29.pdf",width =7,height = 7)                  
plot(x,y, 
    type="p",
    pch=16,
    main=paste("Cor=",cor," (p-value=",pval,")",sep=""),
    xlab=paste(gene1),
    ylab=paste(gene2) )
lines(x,fitted(z),col=2)
dev.off()


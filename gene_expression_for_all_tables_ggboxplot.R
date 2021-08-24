## each time, just change the name of gene which needs to be checked
## note: need to use R 64 bits, not R 32 bits program

setwd("D:\\galina\\2019_03_18\\expression")
gene = 'ACKR3'
gene_exp_table = ''

library(scales)
library(RODBC)
#install.packages('ReporteRs') need 64bit java
library(ReporteRs)
library(ggpubr)
#library(formattable)
library(plyr)
library(htmltools)
library(webshot)    
library(rlist)
library(DT)
library(htmlwidgets)
#library(webshot)
##need install wkhtmltox in c: drive, use this command wkhtmltox/wkhtmltopdf/bin/wkhtmltoimage.exe (or wkhtmltopdf in the future) , don't use webshot since summary table gets bigger, screenshot will not work

## download phantomjs and put this    C:\phantomjs\bin  in the system path variable
## for first summary slide, html table in order to use red color to highlight value

##export_formattable <- function(f, file, width = "100%", height = NULL, 
##                               background = "white", delay = 0.2)
##    {
##      w <- as.htmlwidget(f, width = width, height = height)
##      path <- html_print(w, background = background, viewer = NULL)
##      url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
##      webshot(url,
##              file = file,
##              selector = ".formattable_widget",
##              delay = delay)
##    }

    

doc =pptx( ) # create pptx


con <- odbcConnect("XXXX", uid="XXXX", pwd="XXXXX", believeNRows=FALSE)
table_list <- sqlQuery(con, "SELECT distinct table_name FROM LAB_DATA_TABLE ORDER BY TABLE_NAME")

summary_table <- data.frame(matrix(ncol = 7, nrow = 0), stringsAsFactors = FALSE)
colnames(summary_table) <- c("dataset", "NvsT","TvsM", "NvsM", "color_nt", "color_tm", "color_nm")

for (j in 1:length(table_list$TABLE_NAME))
{
	gene = toupper(gene)
	gene_exp_table = table_list$TABLE_NAME[j]
	statement1 = paste0("SELECT SAMPLE_ID, TYPE FROM LAB_DATA_TABLE WHERE TABLE_NAME = '", gene_exp_table, "' AND TYPE IS NOT NULL")
	idtype <- sqlQuery(con, statement1)

	if (nrow(idtype)) 
	{
		## add comma
		pid = paste(shQuote(t(idtype[,1]), type="sh"), collapse=", ")
		##remove single quote
		pid = gsub("'", '', pid)
		statement2 = paste0("SELECT ", pid," FROM ", gene_exp_table, " WHERE UPPER(GENE_NAME) = ", "'", gene, "'")

		expression <- sqlQuery(con, statement2)

		if (nrow(expression))  ## no gene found, should return 0 row of that data frame
		{
			expression = t(expression)
			idexpression = cbind(expression, idtype)
			idexpression = na.omit(idexpression)
			colnames(idexpression)[1] = "Expression"

			comptype = as.list(levels(idtype$TYPE))
			datasetname = substr(gene_exp_table, 5, nchar(toString(gene_exp_table)))
	
			medn = median(idexpression[idexpression$TYPE=='N',]$Expression)
			medt = median(idexpression[idexpression$TYPE=='T',]$Expression)
			medm = median(idexpression[idexpression$TYPE=='M',]$Expression)
			
			nt=ifelse(medn <= medt, 1, -1)
			tm=ifelse(medt <= medm, 1, -1)
			nm=ifelse(medn <= medm, 1, -1)

			if (paste0(comptype[1],comptype[2],comptype[3])=="MNT")
			{
				pvalues = compare_means(Expression ~ TYPE, data = idexpression, method = "t.test")
				for (i in 1:3)
				{
					if (pvalues$p[i] <= 2.2e-16)
					{
						pvalues$p.format[i] = 2.2e-16
					}
				}
				my_comparisons <- list(c("N", "T"), c("T", "M"), c("N", "M"))
				orderlist = c("N", "T", "M")
				colorlist = c("#00AFBB", "#E7B800", "#FC4E07")
				labellist = c(paste0("N=", sum(idexpression$TYPE=="N")), paste0("T=", sum(idexpression$TYPE=="T")), paste0("M=", sum(idexpression$TYPE=="M")))
				
				NvsT=pvalues$p.format[3]
				TvsM=pvalues$p.format[2]
				NvsM=pvalues$p.format[1]	
				
				color_nt=ifelse(as.numeric(NvsT)<=0.05, nt, 0)
				color_tm=ifelse(as.numeric(TvsM)<=0.05, tm, 0)
				color_nm=ifelse(as.numeric(NvsM)<=0.05, nm, 0)
				
				mntrow <- data.frame(dataset=datasetname, NvsT, TvsM, NvsM, color_nt, color_tm, color_nm, stringsAsFactors = FALSE)
				summary_table <- rbind.fill(summary_table, mntrow)
			}else if (((paste0(comptype[1],comptype[2])=="MN")&&(comptype[3]=="NULL")))
			{
				pvalues = compare_means(Expression ~ TYPE, data = idexpression, method = "t.test")
				for (i in 1:1)
				{
					if (pvalues$p[i] <= 2.2e-16)
					{
						pvalues$p.format[i] = 2.2e-16
					}
				}
				my_comparisons <- list(c("N", "M"))		
				orderlist = c("N", "M")
				colorlist = c("#00AFBB", "#FC4E07")
				labellist = c(paste0("N=", sum(idexpression$TYPE=="N")), paste0("M=", sum(idexpression$TYPE=="M")))

				NvsT=''
				TvsM=''
				NvsM=pvalues$p.format[1]

				color_nt=0
				color_tm=0
				color_nm=ifelse(as.numeric(NvsM)<=0.05, nm, 0)

				mnrow <- data.frame(dataset=datasetname, NvsT, TvsM, NvsM, color_nt, color_tm, color_nm, stringsAsFactors = FALSE)
				summary_table <- rbind.fill(summary_table, mnrow)
			}else if (((paste0(comptype[1],comptype[2])=="MT")&&(comptype[3]=="NULL")))
			{ 
				pvalues = compare_means(Expression ~ TYPE, data = idexpression, method = "t.test")
				for (i in 1:1)
				{
					if (pvalues$p[i] <= 2.2e-16)
					{
						pvalues$p.format[i] = 2.2e-16
					}
				}
				my_comparisons <- list(c("T", "M"))		
				orderlist = c("T", "M")
				colorlist = c("#E7B800", "#FC4E07")
				labellist = c(paste0("T=", sum(idexpression$TYPE=="T")), paste0("M=", sum(idexpression$TYPE=="M")))
				
				NvsT=''
				TvsM=pvalues$p.format[1]
				NvsM=''

				color_nt=0
				color_tm=ifelse(as.numeric(TvsM)<=0.05, tm, 0)
				color_nm=0

				mtrow <- data.frame(dataset=datasetname, NvsT, TvsM, NvsM, color_nt, color_tm, color_nm, stringsAsFactors = FALSE)
				summary_table <- rbind.fill(summary_table, mtrow)
			}else if (((paste0(comptype[1],comptype[2])=="NT")&&(comptype[3]=="NULL")))
			{
				pvalues = compare_means(Expression ~ TYPE, data = idexpression, method = "t.test")
				for (i in 1:1)
				{
					if (pvalues$p[i] <= 2.2e-16)
					{
						pvalues$p.format[i] = 2.2e-16
					}
				}
				my_comparisons <- list(c("N", "T"))		
				orderlist = c("N", "T")
				colorlist = c("#00AFBB", "#E7B800")
				labellist = c(paste0("N=", sum(idexpression$TYPE=="N")), paste0("T=", sum(idexpression$TYPE=="T")))
				
				NvsT=pvalues$p.format[1]
				TvsM=''
				NvsM=''

				color_nt=ifelse(as.numeric(NvsT)<=0.05, nt, 0)
				color_tm=0
				color_nm=0
				
				ntrow <- data.frame(dataset=datasetname, NvsT, TvsM, NvsM, color_nt, color_tm, color_nm, stringsAsFactors = FALSE)
				summary_table <- rbind.fill(summary_table, ntrow)
			}
			else #jump to next in the loop
			{
				next
			}

			
			##ggboxplot
			myplot <- ggboxplot(idexpression, x="TYPE", y="Expression", color = "TYPE", order = orderlist, palette = colorlist, xlab=FALSE, legend = "none", title = paste0(gene, " in ", datasetname), 
			       add = "jitter", shape="TYPE", outlier.shape = NA) + stat_compare_means(comparisons = my_comparisons, method = "t.test") + 
			       theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
			       scale_x_discrete(labels = labellist)

			ggsave(filename = paste0(gene, " boxplot in ", datasetname, ".pdf"), plot=myplot)
			ggsave(filename = paste0(gene, " boxplot in ", datasetname, ".jpg"), plot=myplot)
		}
	}
}
close(con)


##sumslide = formattable(summary_table, list(
##   NvsT = formatter("span", style = x ~ ifelse(round(as.numeric(x), 4) < 0.05, style(color = "red", font.weight = "bold"), NA)),
##   TvsM = formatter("span", style = x ~ ifelse(round(as.numeric(x), 4) < 0.05, style(color = "red", font.weight = "bold"), NA)),
##   NvsM = formatter("span", style = x ~ ifelse(round(as.numeric(x), 4) < 0.05, style(color = "red", font.weight = "bold"), NA))
##))
##sumslidefile = paste0(gene, " expression summary.png")
##export_formattable(sumslide, sumslidefile)


widget_expression = datatable(summary_table, colnames = c('dataset', 'p value (N vs T)', 'p value (T vs M)', 'p value (N vs M)', "color_nt", "color_tm", "color_nm"), options=list(dom = 't', ordering=F, pageLength = 30, columnDefs = list(list(visible=FALSE, targets=c(5:7))))) %>%
formatStyle(columns = 2:4, textAlign = 'center', valueColumns = 5:7, color = styleEqual(c(-1, 0, 1), c('red', 'black', 'green')))
saveWidget(widget_expression, "temp_expression.html", selfcontained = FALSE)
#sumslidefile = "expression summary.jpg"
#webshot("temp_expression.html", file = sumslidefile, cliprect = "viewport")
shell("c:/wkhtmltox/wkhtmltopdf/bin/wkhtmltoimage.exe --javascript-delay 1 temp_expression.html expression_summary.jpg")

doc = addSlide(doc, "Two Content")
doc = addImage(doc, "expression_summary.jpg", width = 8.5, height = 6.8, offx = 2, offy = 0.2)

patn = glob2rx(paste0(gene, "*.jpg"))
file_gene=list.files(path = getwd(), pattern =patn, full.names = TRUE, recursive = TRUE)

for (file in file_gene)
{
	doc = addSlide(doc, "Two Content")
	doc = addImage(doc, file, width = 7, height = 6.8 , offx = 3, offy = 0.2)
	doc = addFooter(doc, basename(file))
	writeDoc(doc, paste0(gene, " expression in PCa dataset.pptx"))
}



dir.create("raw_data")
dir.create("processed_data")
dir.create("scripts")
dir.create("results")
dir.create("plots")
if (!requireNamespace("BiocManager", quietly = TRUE)) 
  install.packages("BiocManager")
BiocManager::install(c("GEOquery","affy","arrayQualityMetrics"))
install.packages("dplyr")
library(GEOquery)             
library(affy)                 
library(arrayQualityMetrics)  
library(dplyr) 

gse_data <- getGEO("GSE32924", GSEMatrix = TRUE)
expression_data <- exprs(gse_data$GSE32924_series_matrix.txt.gz)
feature_data <- fData(gse_data$GSE32924_series_matrix.txt.gz)
phenotype_data <- pData(gse_data$GSE32924_series_matrix.txt.gz)
sum(is.na(phenotype_data$source_name_ch1))

untar("C:\\Users\\megha\\Desktop\\GSE32924\\GSE32924_RAW.tar", exdir = "raw_data\\CEL_files")
Raw_data <- ReadAffy(celfile.path = "raw_data/CEL_Files")
Raw_data

#QC Before normalization
arrayQualityMetrics(expressionset = Raw_data,outdir = "results\\QC_Raw_data",force = TRUE,do.logtransform = TRUE)
#test_data <- Raw_data[, 1:10]  # first 10 samples
#arrayQualityMetrics(expressionset = test_data, outdir = "Results/QC_Test", force = TRUE, do.logtransform = TRUE)
normalized_data <- rma(Raw_data)
# QC after data normalization 
arrayQualityMetrics(expressionset = normalized_data,
                    outdir = "results\\QC_Normalized_Data",
                    force = TRUE)
processed_data <- as.data.frame(exprs(normalized_data))
dim(processed_data)

#Filter low-variance transcripts

row_median <- rowMedians(as.matrix(processed_data))
row_median
hist(row_median, breaks = 100, freq = FALSE, main = "Median Intensity Distribution")
threshold <- 5
abline(v= threshold, col = "red", lwd=2)
indx <- row_median> threshold
filtered_data <- processed_data[indx, ]

colnames(filtered_data) <- rownames(phenotype_data)
processed_data <- filtered_data

class(phenotype_data$source_name_ch1) 

# Define experimental groups (normal vs cancer)
groups <- factor(phenotype_data$source_name_ch1,
                 levels = c("ANL", "AL","Normal"),
                 label = c("dermatitis", "dermatitis","normal"))

class(groups)
levels(groups)
save.image(file= "GSE32924.RData")

classify_gene<- function(log2Fc,padj){
  if(log2Fc > 1 & padj < 0.05){return("Upregulated")}
  else if(log2Fc < -1 & padj < 0.05){return("Downregulated")}
  else {return("Not_Significant")}
}
#putting values to check whether the function works
classify_gene(log2Fc=2,0.09)
classify_gene(-2,0.04)
#function works

# Define input and output folders
input_dir <- "raw_Data" 
output_dir <- "results"
# List which files to process
files_to_process <- c("DEGs_Data_1.csv","DEGs_Data_2.csv")
# Prepare empty list to store results in R 
result_list <- list()
 
# Then:
#   - Apply it in a for-loop to process both datasets (DEGs_data_1.csv, DEGs_data_2.csv)

for (d in files_to_process) {
  cat("\nProcessing:",d,"\n")
  input_file_path <- file.path(input_dir,d)
  #Import dataset
  DEG <- read.csv(input_file_path, header = TRUE)
  cat("File imported. Checking for missing values...\n")
  #check if logFC column exists
  if(!("logFC" %in% names(DEG))){stop("logFC column not found in:",d)}
  #Replace missing padj values with 1
  if ("padj" %in% names(DEG)){
    missing_count <- sum(is.na(DEG$padj))
    cat("Missing padj values are:",missing_count,"\n")
    DEG$padj[is.na(DEG$padj)] <- 1
  }
  #Add a new column 'status'
  #since function is written with if/else if we simply call function it will only check first element since if in R is non-vectorized. so mapply()
  #needs to be used here so that the function will check each row. 
  #Alternative write function with ifelse()
  #classify_gene <- function(log2FC, padj) {
  #ifelse(padj < 0.05 & log2FC > 1, "Upregulated",
         #ifelse(padj < 0.05 & log2FC < -1, "Downregulated", "Not_Significant"))}

  DEG$status <- mapply(classify_gene,DEG$logFC,DEG$padj)
  
  result_list[[d]]<- DEG
  #Save processed files into Results folder
  output_file_path <- file.path(output_dir,paste0("Classification_results",d))
  write.csv(DEG,output_file_path,row.names = FALSE)
  cat("Results saved to:",output_file_path,"\n")
  
  #Print summary counts of significant, upregulated, and downregulated genes
  cat("Summary counts for", d, ":\n")
  print(table(DEG$status))
  

    
  
}
save.image(file= "Meghana_R_Nair_Class_2_Assignment.RData")

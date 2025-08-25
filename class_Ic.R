#### Try It Yourself ####
# Practice Exercises 

# ----------------------------------------------------------------------------------------------------------------

# 1. Check Cholesterol level (using if) 
# Write an If statement to check cholesterol level is greater than 240, 
# if true, it will prints “High Cholesterol”

cholesterol <- 230
if(cholesterol > 240){
  print("High Cholesterol")}

# 2. Blood Pressure Status (using if...else)
# Write an if…else statement to check if blood pressure is normal.
# If it’s less than 120, print: “Blood Pressure is normal”
# If false then print: “Blood Pressure is high”

Systolic_bp <- 130
if (Systolic_bp < 120){print("Blood Pressure is normal")}else{print("Blood Pressure is high")}

#3. Automating Data Type Conversion with for loop

# Use patient_info.csv data and metadata.csv
# Perform the following steps separately on each dataset (patient_info.csv data and metadata.csv)
# Create a copy of the dataset to work on.
# Identify all columns that should be converted to factor type.
# Store their names in a variable (factor_cols).

# Example: factor_cols <- c("gender", "smoking_status")

# Use a for loop to convert all the columns in factor_cols to factor type.
# Pass factor_cols to the loop as a vector.

# Hint:
# for (col in factor_cols) {
#   data[[col]] <- as.factor(data[[col]])  # Replace 'data' with the name of your dataset
# }

# ----------------------------------------------------------------------------------------------------------------

# 4. Converting Factors to Numeric Codes

# Choose one or more factor columns (e.g., smoking_status).
# Convert "Yes" to 1 and "No" to 0 using a for loop.

# Hint:
# binary_cols <- c("smoking_status")   # store column names in a vector
# use ifelse() condition inside the loop to replace Yes with 1 and No with 0
# for (col in binary_cols) {
#   data[[col]] <- # insert your ifelse() code here
# }

# ----------------------------------------------------------------------------------------------------------------

#  Verification:
#    Compare the original and modified datasets to confirm changes.
#    str(original_data)
#    str(data)


metadata<-read.csv(file.choose())
clean_metadata<-metadata
patient_data<-read.csv(file.choose())
clean_patientdata<-patient_data
str(clean_metadata)
str(clean_patientdata)
factor_colsmeta<-c("height","gender")
factor_colspatient<-c("gender","diagnosis","smoker")

for(i in factor_colsmeta){clean_metadata[[i]]<-as.factor(clean_metadata[[i]])}
str(clean_metadata)
for(p in factor_colspatient){clean_patientdata[[p]]<-as.factor(clean_patientdata[[p]])}
str(clean_patientdata)

binary_cols1<-c("smoker")
for (col in binary_cols1){clean_patientdata[[col]]<-ifelse(clean_patientdata[[col]]=="Yes",1,0)}
str(clean_patientdata)


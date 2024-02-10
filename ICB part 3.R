# Correcting the column names based on your dataframe structure
dataframe$Percentage_of_Patients_in_High_Load_Practices <- dataframe$Patients_in_High_Load_Practices / dataframe$"Total number of patients"
dataframe$Percentage_of_Patients_in_High_Carr_Hill_Load_Practices <- dataframe$Patients_in_High_Carr_Hill_Load_Practices / dataframe$"Total number of patients CR"

# Ensure we're correctly referencing the column names with spaces by using backticks or quotes as needed
# It seems there might have been a typo or incorrect reference in the previous attempt

# Print the updated dataframe to check the first few rows
print(head(dataframe))
# Convert the dataframe to an HTML table string
html_table_string <- htmlTable::htmlTable(dataframe)

# Specify the file path where you want to save the HTML table
file_path <- "dataframe_table.html"

# Save the HTML table to the file
writeLines(html_table_string, file_path)

# Print a message to indicate the file has been saved
cat("The dataframe has been exported as an HTML table to", file_path)

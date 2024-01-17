# Load necessary libraries
library(readxl)
library(sf)
library(dplyr)
library(ggplot2)

# Unzip the shapefile
unzip("Integrated_Care_Boards_(April_2023)_EN_BGC.zip")

# Assuming the unzipped files are in the current working directory
# Read the shapefile
icb_shape <- st_read("ICB_APR_2023_EN_BGC.shp")

# Read your data
data <- read_excel("icb_GP_stats_2022.xlsx")

# Ensure the column names used for merging are correctly matched
# Here, replace 'ICB_Name' with the actual name of the column in your shapefile that corresponds to 'Icb22Nm' in your data
# Merge the shapefile data with your Excel data
merged_data <- merge(icb_shape, data, by.x = "ICB23NM", by.y = "Icb22Nm", all.x = TRUE)

# Plot the data
# Load the ggplot2 library
library(ggplot2)
library(grid)  # For using grob (graphics objects)


# Create the plot
ggplot(merged_data) +
  geom_sf(aes(fill = `proportion pts with >5k pt:GP ratio`)) +
  scale_fill_gradient(low = "gray", high = "red") +
  theme_classic() +
  theme(axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5),  # Make title bold, larger, and centered
        plot.margin = margin(5.5, 5.5, 5.5, 5.5, "mm")) +  # Increase bottom margin for footnote
  labs(title = "Proportion of Patients with over \n5,000 pt:GP ratio by ICB",  # Title over two lines
       fill = "Proportion >5K pt:GP",
       caption = "Reproduced with permission from Steve Black @sib313")  # Add footnote


# Install and load the rsdmx package
install.packages("rsdmx")
library(rsdmx)

# Define the URL 
library(rsdmx)

# Define the URLs
url_consultations <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/HEALTH_PROC/CONSTOTT.NBPHABNB.AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+NMEC+ARG+BRA+BGR+CHN+HRV+IND+IDN+PER+ROU+RUS+ZAF/all?startTime=2010&endTime=2022"
url_discharges <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/HEALTH_PROC/HUTIINDI+HUTIINST+HUTICUDI+HUTIJOCS+HUTICUST+HUTIOCST.NOMBRENB+NBPHABNB+TXOCCUTX+JOUPERNB+RTOALLNB.AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+NMEC+ARG+BRA+BGR+CHN+HRV+IND+IDN+PER+ROU+RUS+ZAF/all?startTime=2010&endTime=2022"

# Read the SDMX data for both URLs
OECD_consultations <- as.data.frame(readSDMX(url_consultations))
OECD_discharges <- as.data.frame(readSDMX(url_discharges))

# View the first few rows of both data frames for a quick check
head(OECD_consultations)
head(OECD_discharges)
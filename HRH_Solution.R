## Data Transformation Script for HRH Needs and Optimization Solution
# Extract data from Excel based Data Collection Template
# Transform the data appropriately and perform calculations
# Output transformed data into prescribed csv format
## Date modified: 29 October 2020

# Instructions on running the R script
#   1. Ensure that the full path to the HRH Solution main folder is "C:/HRH Solution" as provided in the set-up folders
#      If not replace the "C:/HRH Solution" in the two lines below with your actual path to the HRH Solution folder
#   3. When specifying the path use forward slash (/) and not backward slash (\)
# After specifying the path press the source button on the menu above

# Set path variables 
setwd('C:/HRH Solution')

# Load the DCT 
# dct <- "Data/Tanzania_COP 21_FY 20_2020-10-02.xlsx"
dct <- "Data/Nigeria_COP 21_FY 20-20_2020-10-02.xlsx"

source('C:/HRH Solution/Scripts/index.R')

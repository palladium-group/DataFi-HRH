## Data Transformation Script for HRH Needs and Optimization Solution

##  PURPOSE
#   -Extract data from Excel based Data Collection Template
#   -Transform the data and perform calculations
#   -Output transformed data into prescribed .xlsx format for tableau
## Date modified: 01 December 2020

## INSTRUCTIONS
#   1. Click on the Source button in the upper right-hand corner of this windowpane.
#   2. A dialogue box (or an icon for the box) will open asking you to Select the Data Collection Template you want to run. 
#      Navigate to the Data folder, select your file, and click the Open button. 
#   3. The R Script will run automatically. You will see the lines of code appear as they run in the Console windowpane. 
#      It will take approximately 2 minutes for the script to run.   
#   4. When the script run is completed, the message 'End of the R script run' will appear in Console windowpane.
#   5. The script has now written the 11 .xlsx output files to the Dataout folder.
#   6. You may now Quit the RStudio session and navigate to the Tableau workbook in the Output folder.
## END OF INSTRUCTIONS

dct <- file.choose()
setwd(regmatches(dct, regexpr(".*HRH Solution", dct)))
source('Scripts/R-Script.R')

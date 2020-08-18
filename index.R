PackagesList <- c("readxl", "openxlsx","sjmisc", "tidyverse", "glue")
Packages <- PackagesList[!(PackagesList %in% installed.packages()[,"Package"])]
if(length(Packages)) install.packages(Packages)

library(readxl)
library(sjmisc)
library(glue)
# library(openxlsx)

# Merge data collection templates 
# path <- "HRH Data Collection"
# master_file_name <- "HRH Data Collection Template_Master.xlsm"
# 
# data_collection_template_list <- list.files(path= path, full.names=TRUE)
# 
# All <- lapply(data_collection_template_list,function(filename){
#   print(paste("Merging",filename,sep = " "))
#   read.xlsx(filename)
# })
# 
# df <- do.call(rbind.data.frame, All)
# write.xlsx(df,master_file_name)

#Import data from master file
import_location <- "HRH Data Collection/HRH Data Collection Template_Country Name.xlsm"

#Output tables path
output_location <- "OutputTables/"
# Import PSNU List
PSNUList <- read_excel(import_location, 
                       sheet = "1. PSNU List")
# Import Program targets for COP year
ProgramTargets <- read_excel(import_location, 
                             sheet = "2. Program targets for COP year",
                             skip = 1)
# Import PEPFAR Target Attribution
TargetAttribution <- read_excel(import_location, 
                                sheet = "3. PEPFAR Target Attribution",
                                skip = 1)
# Import HIV Services Time Allocation
HIVServicesTime <- read_excel(import_location, 
                              sheet = "4. HIV Services Time Allocation",
                              skip = 1)
# Import Client Pathways
ClientPathways <- read_excel(import_location, 
                             sheet = "5. Client Pathways", range = "C1:D76")
ClientPathways <- rotate_df(ClientPathways, cn = TRUE)
# Import Current PEPFAR HRH
CurrentHRH <- read_excel(import_location, 
                         sheet = "6. Current PEPFAR HRH",
                         skip = 1)
# Import Current HRH Salaries
CurrentSalaries <- read_excel(import_location, 
                              sheet = "7. Current HRH Salaries",
                              skip = 1)
# Import Program Budgets
ProgramBudgets <- read_excel(import_location, 
                             sheet = "8. Program Budgets", range = "A2:E3")
# Available working time
AvailableWorkingTime <- read_excel(import_location, 
                                   sheet = "9. Available Working Time", 
                                  range = "B4:I12")
# Default non-clinical work hours per week
WeeklyNonClinicalWorkingHours <- read_excel(import_location, 
                                            sheet = "9. Available Working Time", 
                                            range = "B17:C25")

# write csv
# Number of staff by cadre by PSNU
write.csv(CurrentHRH, glue('{output_location}NumberOfStaff.csv'), row.names = TRUE)
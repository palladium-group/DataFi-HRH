library(readxl)
library(openxlsx)

# Merge data collection templates 
path <- "HRH Data Collection"
master_file_name <- "HRH Data Collection Template_Master.xlsm"

data_collection_template_list <- list.files(path= path, full.names=TRUE)

All <- lapply(data_collection_template_list,function(filename){
  print(paste("Merging",filename,sep = " "))
  read.xlsx(filename)
})

df <- do.call(rbind.data.frame, All)
write.xlsx(df,master_file_name)

#Import data from master file
import_location <- "HRH Data Collection Template_Master.xlsm"
# Import File ID and Ref
FileIdRef <- read_excel(import_location, 
                        sheet = "1. File ID and Ref")
# Import Program targets for COP year
ProgramTargets <- read_excel(import_location, 
                             sheet = "2. Program targets for COP year")
# Import PEPFAR Target Attribution
TargetAttribution <- read_excel(import_location, 
                             sheet = "3. PEPFAR Target Atribution")
# Import HIV Services Time Allocation
HIVServicesTime <- read_excel(import_location, 
                                sheet = "4. HIV Services Time Allocation")
# Import Client Pathways
ClientPathways <- read_excel(import_location, 
                             sheet = "5. Client Pathways", range = "A2:B21", 
                             col_names = FALSE)
# Import Current PEPFAR HRH
CurrentHRH <- read_excel(import_location, 
                        sheet = "6. Current PEPFAR HRH")
# Import Current HRH Salaries
CurrentSalaries <- read_excel(import_location, 
                             sheet = "7. Current HRH Salaries")
# Import Program Budgets
CurrentSalaries <- read_excel(import_location, 
                              sheet = "8. Program Budgets")
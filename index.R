# install packages if not available
# =================================
PackagesList <-
  c("readxl",
    "openxlsx",
    "sjmisc",
    "tidyverse",
    "glue",
    "stringr",
    "data.table")
Packages <-
  PackagesList[!(PackagesList %in% installed.packages()[, "Package"])]
if (length(Packages))
  install.packages(Packages)

library(readxl)
library(sjmisc)
library(glue)
library(tidyverse)
library(stringr)
library(data.table)

#Import data from master file
# ============================
import_location <-
  "HRH Data Collection/HRH Data Collection Template_Country Name.xlsm"

#Output tables path
output_location <- "Output Tables/"
# Import PSNU List
PSNUList <- read_excel(import_location,
                       sheet = "1. PSNU List")
# Import Program targets for COP year
ProgramTargets <- read_excel(import_location,
                             sheet = "2. Program targets for COP year",
                             skip = 1)
# Import PEPFAR Target Attribution
TargetAttribution <- read_excel(import_location,
                                sheet = "3. PEPFAR Target Attribution")

TargetAttribution <- rotate_df(TargetAttribution, cn = TRUE)
colnames(TargetAttribution) <- c("ProgramArea", "TargetAttribution")

# Import HIV Services Time Allocation
HIVServicesTime <- read_excel(import_location,
                              sheet = "4. HIV Services Time Allocation")
HIVServicesTime <- rotate_df(HIVServicesTime, cn = TRUE)
colnames(HIVServicesTime) <- c("Cadre", "HIVServicesTime")

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
names(AvailableWorkingTime) <-
  str_replace_all(names(AvailableWorkingTime), c(" " = "" , "," = "" ,  "/" = "Per"))

# Default non-clinical work hours per week
WeeklyNonClinicalWorkingHours <- read_excel(import_location,
                                            sheet = "9. Available Working Time",
                                            range = "B17:C25")
colnames(WeeklyNonClinicalWorkingHours) <-
  c("Cadre", "WeeklyNonClinicalWorkingHours")

# Service Standard
ClientTime <- read_excel("WISNInputs.xlsx",
                         sheet = "Client Time")

# Format data set columns to usable names
# =======================================
names(AvailableWorkingTime) <-
  str_replace_all(names(AvailableWorkingTime), c(" " = "" , "/" = "Per"))
names(CurrentHRH) <-
  str_replace_all(names(CurrentHRH), c(" " = "" ,  "-" = "_"))
names(CurrentSalaries) <-
  str_replace_all(names(CurrentSalaries), c(" " = ""))
names(HIVServicesTime) <-
  str_replace_all(names(HIVServicesTime), c(" " = "" ,  "-" = "_"))
names(ProgramBudgets) <-
  str_replace_all(names(ProgramBudgets), c(" " = "" , "/" = "Or"))
names(ProgramTargets) <-
  str_replace_all(names(ProgramTargets), c(" " = ""))
names(PSNUList) <- str_replace_all(names(PSNUList), c(" " = ""))
names(TargetAttribution) <-
  str_replace_all(names(TargetAttribution), c(" " = ""))
names(WeeklyNonClinicalWorkingHours) <-
  str_replace_all(names(WeeklyNonClinicalWorkingHours), c(" " = ""))

# Output Calculations
# ====================

# select the program Targets
ProgramTargets <- ProgramTargets %>%
  rename(PSNU = PSNUlist) %>%
  select(
    RefID,
    PSNU,
    PrEP_NEW,
    PrEP_CURR,
    HTS_SELF,
    HTS_TST,
    TX_NEW,
    TX_CURR,
    PMTCT_ART,
    TX_PVLS
  )
# Format program targets
ProgramTargets_PrEP_NEW <-  ProgramTargets %>%
  mutate(ProgramArea = "PrEP_NEW") %>%
  select(RefID, PSNU, ProgramArea, PrEP_NEW) %>%
  rename(ProgramTargets = PrEP_NEW)
ProgramTargets_PrEP_CURR <-  ProgramTargets %>%
  mutate(ProgramArea = "PrEP_CURR") %>%
  select(RefID, PSNU, ProgramArea, PrEP_CURR) %>%
  rename(ProgramTargets = PrEP_CURR)
ProgramTargets_HTS_SELF <-  ProgramTargets %>%
  mutate(ProgramArea = "HTS_SELF") %>%
  select(RefID, PSNU, ProgramArea, HTS_SELF) %>%
  rename(ProgramTargets = HTS_SELF)
ProgramTargets_HTS_TST <-  ProgramTargets %>%
  mutate(ProgramArea = "HTS_TST") %>%
  select(RefID, PSNU, ProgramArea, HTS_TST) %>%
  rename(ProgramTargets = HTS_TST)
ProgramTargets_TX_NEW <-  ProgramTargets %>%
  mutate(ProgramArea = "TX_NEW") %>%
  select(RefID, PSNU, ProgramArea, TX_NEW) %>%
  rename(ProgramTargets = TX_NEW)
ProgramTargets_TX_CURR <-  ProgramTargets %>%
  mutate(ProgramArea = "TX_CURR") %>%
  select(RefID, PSNU, ProgramArea, TX_CURR) %>%
  rename(ProgramTargets = TX_CURR)
ProgramTargets_PMTCT_ART <-  ProgramTargets %>%
  mutate(ProgramArea = "PMTCT_ART") %>%
  select(RefID, PSNU, ProgramArea, PMTCT_ART) %>%
  rename(ProgramTargets = PMTCT_ART)
ProgramTargets_TX_PVLS <-  ProgramTargets %>%
  mutate(ProgramArea = "TX_PVLS") %>%
  select(RefID, PSNU, ProgramArea, TX_PVLS) %>%
  rename(ProgramTargets = TX_PVLS)
ProgramTargetsList = list(
  ProgramTargets_PrEP_NEW,
  ProgramTargets_PrEP_CURR,
  ProgramTargets_HTS_SELF,
  ProgramTargets_HTS_TST,
  ProgramTargets_TX_NEW,
  ProgramTargets_TX_CURR,
  ProgramTargets_PMTCT_ART,
  ProgramTargets_TX_PVLS
)
COPProgramTargets <- rbindlist(ProgramTargetsList) %>%
  full_join(AvailableWorkingTime, by = character()) %>%
  select(RefID, PSNU, ...1, ProgramArea, ProgramTargets) %>%
  rename(Cadre = ...1)

# Available work time
AvailableWorkingTime <- AvailableWorkingTime %>%
  mutate(
    AWT_Days = (WorkingDaysPerWeek * 52) -
      (AnnualLeave + PublicHolidays + SickLeave + TrainingDaysperYear)
  ) %>%
  mutate(AWT_Hours = AWT_Days * WorkinghrsPerday) %>%
  rename(Cadre = ...1) %>%
  select(Cadre,
         WorkingDaysPerWeek,
         WorkinghrsPerday,
         AWT_Days,
         AWT_Hours)

#Dashboard one data
Data_With_HCW <-
  full_join(PSNUList, AvailableWorkingTime, by = character()) %>%
  full_join(TargetAttribution, by = character()) %>%
  inner_join(HIVServicesTime, by = "Cadre") %>%
  rename(RefID = RecordID) %>%
  inner_join(COPProgramTargets,
             by = c("RefID", "PSNU", "Cadre", "ProgramArea")) %>%
  inner_join(ClientTime,
             by = c("Cadre", "ProgramArea")) %>%
  inner_join(WeeklyNonClinicalWorkingHours,
             by = "Cadre") %>%
  #calculate service standard
  mutate(ServiceStandard = 60 / ClientTime) %>%
  mutate(StandardWorkload = ServiceStandard * AWT_Hours) %>%
  mutate(CategoryAllowedStandard = (
    WeeklyNonClinicalWorkingHours / (WorkingDaysPerWeek * WorkinghrsPerday)
  ) / WorkinghrsPerday) %>%
  select(
    RefID,
    PSNU,
    Cadre,
    ProgramArea,
    ProgramTargets,
    TargetAttribution,
    HIVServicesTime,
    ClientTime,
    WorkingDaysPerWeek,
    WorkinghrsPerday,
    WeeklyNonClinicalWorkingHours,
    AWT_Days,
    AWT_Hours,
    ServiceStandard,
    StandardWorkload,
    CategoryAllowedStandard
  )


# write output files
# ====================
# Number of staff by cadre by PSNU
write.csv(CurrentHRH,
          glue('{output_location}NumberOfStaff.csv'),
          row.names = TRUE)

# Current HRH salaries
write.csv(CurrentSalaries,
          glue('{output_location}CurrentHRHSalaries.csv'),
          row.names = TRUE)

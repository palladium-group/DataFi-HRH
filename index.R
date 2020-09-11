# install packages if not available
# =================================
PackagesList <-
  c(
    "readxl",
    "openxlsx",
    "sjmisc",
    "tidyverse",
    "glue",
    "stringr",
    "data.table",
    "stringi"
  )
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
library(stringi)

#Output tables path
output_location <- "Output Tables/"

# Import Location
import_location <-
  "HRH Data Collection/Data Collection Template.xlsm"

#Import data from DCT
# ============================

# Import PSNU List
PSNUList <-
  read_excel(import_location,
             sheet = "1. PSNU List",
             skip = 2)
# Import Program targets
ProgramTargets <-
  read_excel(import_location,
             sheet = "2. Program targets ",
             na = "0",
             skip = 3)
# Import Current PEPFAR HRH
# ============================
CurrentHRH <-
  read_excel(import_location,
             sheet = "3. Current PEPFAR HRH ",
             skip = 3)
# Import Current HRH Salaries
CurrentSalaries <-
  read_excel(import_location,
             sheet = "4. HRH Salaries",
             range = "A2:I4")
colnames(CurrentSalaries) <-
  c("0", "1", "2", "3", "4", "5", "6", "7", "8")
CurrentSalaries <- rotate_df(CurrentSalaries, cn = TRUE)
colnames(CurrentSalaries) <-
  c("Cadre", "CurrentSalaries")
CurrentSalaries[is.na(CurrentSalaries)] <- 0
# Import customization parameters
CustomizationParameters <-
  read_excel(import_location,
             sheet = "5. Customization parameters",
             range = "B3:C46")
CustomizationParameters <-
  rotate_df(CustomizationParameters, cn = TRUE)
# Service Standard
ClientTime <- read_excel(import_location,
                         sheet = "Client Time")
# Program Areas
ProgramArea <- read_excel("ProgramAreas.xlsx",
                          sheet = "Program Area")
# Available working time
AvailableWorkingTime <- read_excel(import_location,
                                   sheet = "9. Available Working Time",
                                   range = "B4:I12")

# Default non-clinical work hours per week
WeeklyNonClinicalWorkingHours <- read_excel(import_location,
                                            sheet = "9. Available Working Time",
                                            range = "B17:C25")

# Format data set columns to usable names
# =======================================
ProgramTargets <- ProgramTargets %>% filter(!is.na(`PSNU list`))
colnames(WeeklyNonClinicalWorkingHours) <-
  c("Cadre", "WeeklyNonClinicalWorkingHours")
names(AvailableWorkingTime) <-
  str_replace_all(names(AvailableWorkingTime), c(" " = "" , "," = "" ,  "/" = "Per"))
names(PSNUList) <- str_replace_all(names(PSNUList), c(" " = ""))
names(ProgramTargets) <-
  str_replace_all(names(ProgramTargets), c(" " = ""))
names(ProgramTargets) <-
  stri_replace_all_fixed(names(ProgramTargets), "(Total)", "")
CurrentHRH <- CurrentHRH %>%
  rename(RefID = ...1, PSNU = ...2)
PSNUList <- PSNUList %>% filter(!is.na(PSNU))
CurrentHRH <- CurrentHRH %>% filter(!is.na(RefID))
CurrentHRH <- CurrentHRH %>% filter(!is.na(PSNU))
# Calculations
# ====================

# Available work time
# -------------------
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

# Program Targets
# ----------------
# Select
ProgramTargets <- ProgramTargets %>%
  rename(TX_CURR = `TX_CURR `) %>%
  rename(PSNU = PSNUlist) %>%
  mutate(
    HTS_TST = `HTS_TST(Mobile)` + `HTS_TST(IndexMod)` + `HTS_TST(FacilityIndex)` + `HTS_TST(PMTCT_ANC1)` + `HTS_TST(PMTCT_PostANC1)` + `HTS_TST(STI)` + `HTS_TST(OtherPITC)` + `HTS_TST(Inpatient)`
  ) %>%
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
# Format
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
  select(RefID, PSNU, Cadre, ProgramArea, ProgramTargets)

# Current HRH
# -----------
# Format
CurrentHRH_ClinicalMedical <-  CurrentHRH %>%
  mutate(Cadre = "Clinical-Medical") %>%
  select(RefID, PSNU, Cadre, `Total...3`) %>%
  rename(CurrentHRH = `Total...3`)
CurrentHRH_ClinicalNursing <-  CurrentHRH %>%
  mutate(Cadre = "Clinical-Nursing") %>%
  select(RefID, PSNU, Cadre, `Total...10`) %>%
  rename(CurrentHRH = `Total...10`)
CurrentHRH_LayCHW <-  CurrentHRH %>%
  mutate(Cadre = "Lay-CHW") %>%
  select(RefID, PSNU, Cadre, `Total...26`) %>%
  rename(CurrentHRH = `Total...26`)
CurrentHRH_LayCounselor <-  CurrentHRH %>%
  mutate(Cadre = "Lay-Counselor") %>%
  select(RefID, PSNU, Cadre, `Total...19`) %>%
  rename(CurrentHRH = `Total...19`)
CurrentHRH_CaseManager <-  CurrentHRH %>%
  mutate(Cadre = "Case Manager") %>%
  select(RefID, PSNU, Cadre, `Total...33`) %>%
  rename(CurrentHRH = `Total...33`)
CurrentHRH_Pharmacy <-  CurrentHRH %>%
  mutate(Cadre = "Pharmacy") %>%
  select(RefID, PSNU, Cadre, `Total...38`) %>%
  rename(CurrentHRH = `Total...38`)
CurrentHRH_Laboratory <-  CurrentHRH %>%
  mutate(Cadre = "Laboratory") %>%
  select(RefID, PSNU, Cadre, `Total...43`) %>%
  rename(CurrentHRH = `Total...43`)
CurrentHRH_DataClerk <-  CurrentHRH %>%
  mutate(Cadre = "Data Clerk") %>%
  select(RefID, PSNU, Cadre, `Total...48`) %>%
  rename(CurrentHRH = `Total...48`)
CurrentHRHList = list(
  CurrentHRH_ClinicalMedical,
  CurrentHRH_ClinicalNursing,
  CurrentHRH_LayCHW,
  CurrentHRH_LayCounselor,
  CurrentHRH_CaseManager,
  CurrentHRH_Pharmacy,
  CurrentHRH_Laboratory,
  CurrentHRH_DataClerk
)
CurrentHRH_Formated <- rbindlist(CurrentHRHList)
CurrentHRH_Formated[is.na(CurrentHRH_Formated)] <- 0

# Dashboard one data
Data_With_HCW <-
  full_join(PSNUList, AvailableWorkingTime, by = character()) %>%
  full_join(ProgramArea, by = character()) %>%
  rename(RefID = RecordID) %>%
  inner_join(COPProgramTargets,
             by = c("RefID", "PSNU", "Cadre", "ProgramArea")) %>%
  inner_join(CurrentHRH_Formated,
             by = c("RefID", "PSNU", "Cadre")) %>%
  inner_join(ClientTime,
             by = c("Cadre", "ProgramArea")) %>%
  inner_join(WeeklyNonClinicalWorkingHours,
             by = "Cadre") %>%
  inner_join(CurrentSalaries,
             by = "Cadre") %>%
  #calculate Number of visits -- Not accurate -- needs verification
  mutate(
    NumberOfVisits = case_when(
      ProgramArea == "PrEP_NEW" ~ 1,
      ProgramArea == "PrEP_CURR" ~ 4,
      ProgramArea == "HTS_SELF" ~ 1,
      ProgramArea == "HTS_TST" ~ 1,
      ProgramArea == "TX_NEW" ~ 1,
      ProgramArea == "TX_CURR" ~ 12,
      ProgramArea == "PMTCT_ART" ~ 4,
      ProgramArea == "TX_PVLS" ~ 2
    )
  ) %>%
  #calculate service standard
  mutate(ServiceStandard = 60 / ClientTime) %>%
  #calculate Annual workload -- Not accurate -- needs verification
  mutate(AnnualWorkload = ProgramTargets * NumberOfVisits * ServiceStandard) %>%
  mutate(StandardWorkload = ServiceStandard * AWT_Hours) %>%
  #Category Allowed Standard
  mutate(CategoryAllowedStandard = (
    WeeklyNonClinicalWorkingHours / (WorkingDaysPerWeek * WorkinghrsPerday)
  ) / WorkinghrsPerday) %>%
  #Category Allowed Factor
  mutate(CategoryAllowedFactor = 1 / (1 - (CategoryAllowedStandard / AWT_Hours * 100))) %>%
  mutate(HRHRequirement = (AnnualWorkload / StandardWorkload) * CategoryAllowedFactor) %>%
  mutate(Country = OperatingUnit) %>%
  select(
    RefID,
    Country,
    PSNU,
    Cadre,
    ProgramArea,
    CurrentSalaries,
    ProgramTargets,
    ClientTime,
    WorkingDaysPerWeek,
    WorkinghrsPerday,
    WeeklyNonClinicalWorkingHours,
    AWT_Days,
    AWT_Hours,
    NumberOfVisits,
    AnnualWorkload,
    ServiceStandard,
    StandardWorkload,
    CategoryAllowedStandard,
    CategoryAllowedFactor,
    CurrentHRH,
    HRHRequirement
  )

# Replace N/A with zeros
Data_With_HCW[is.na(Data_With_HCW)] <- 0

HRHData <- Data_With_HCW %>%
  mutate(`Gap - Staffing` = (HRHRequirement - CurrentHRH)) %>%
  mutate(`Need - Costing` = (HRHRequirement * as.numeric(CurrentSalaries))) %>%
  mutate(`Gap - Costing` = ((HRHRequirement - CurrentHRH) * as.numeric(CurrentSalaries))) %>%
  mutate(`Existing - Costing` = CurrentHRH - as.numeric(CurrentSalaries)) %>%
  mutate(`%Shortage` = HRHRequirement - CurrentHRH) %>%
  mutate(`%Allocated` = HRHRequirement - CurrentHRH) %>%
  mutate(`Existing FTEs` = CurrentHRH) %>%
  mutate(`HCW Need` = HRHRequirement) %>%
  select(
    Country,
    PSNU,
    ProgramArea,
    Cadre,
    `Existing FTEs`,
    `HCW Need`,
    `Gap - Staffing`,
    `Need - Costing`,
    `Gap - Costing`,
    `%Shortage`,
    `Existing - Costing`,
    `%Allocated`
  )

# write output files
# ====================

# Dashboard one data
write.csv(HRHData,
          glue('{output_location}HRHData.csv'),
          row.names = TRUE)

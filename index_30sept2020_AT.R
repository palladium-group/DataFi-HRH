# Date modified: 30 Sep 2020

# AF: Client Time column in excel is not picking up some of the customization parameters. 
# AF: This Client Time code should be moved over to R, as detailed in the initial Specs. 
# AF: Will need to verify that the Client Time formulas match the pathways on the PowerPoint. 
# AF: When this code is moved into R, comments should be added to the R code detailing the formula that is being used, 
# and where that formula can be found (such as the slide number in the PowerPoint)

# install packages if not available
# ===================================================================================================
PackagesList <- c("readxl","openxlsx","sjmisc","plyr","tidyverse","glue","stringr","data.table","stringi")
Packages <- PackagesList[!(PackagesList %in% installed.packages()[, "Package"])]
if (length(Packages)) install.packages(Packages)

library(readxl)
library(sjmisc)
library(glue)
library(plyr)
library(tidyverse)
library(stringr)
library(data.table)
library(stringi)

# set path variables
# ==================================================================================================
# working directory
setwd('C:/Users/Amos/Documents/palladium/HRH')

# Output tables path
output_location <- "Output Tables"

#Import data from DCT
# ==================================================================================================
dct <- "HRH Data Collection/2020 09 24 Data Collection Template Revised.xlsx"

# 5. Customization parameters
customPar <- read_excel(dct, sheet = "5. Customization parameters", skip = 4, col_names = F)
names(customPar) <- c('param','question','response')
customPar$qn <- as.numeric(customPar$response)                                                    # % response
customPar$resp <- ifelse(customPar$response %in% c('Yes','No'), customPar$response=='Yes', NA)    # yes/no response

# 2. Program targets
programTargets <- read_excel(dct, sheet = "2. Program targets ", na = "0", skip = 3)

# replace problematic spaces and brackets in variable names with underscores
names(programTargets) <- gsub("\\s\\(", '_', gsub("\\)", '', names(programTargets)))
names(programTargets) <- gsub("\\s", '_', names(programTargets))

# filter out entirely empty rows that come with the excel sheet
programTargets <- programTargets %>% 
  filter(!is.na(PSNU_list))

# Calculations
# ==================================================================================================
# Total no. of minutes needed by a CHW annually for a particular pathway and PSNU
# --------------------------------------
# use a subset of pathways to avoid errors: to use the full list of pathways after adding code for all pathways
# plus some like PrEP_NEW (KP) in DCT are not are not needed here
xxprogramTargets <- programTargets %>%
  gather(pathway, cop_target, PrEP_NEW_Total:TX_PVLS_Total) %>% 
  filter(pathway %in% c('PrEP_NEW_Total','PrEP_CURR_Total','HTS_SELF_Total'))

tot_mins <- ddply(xxprogramTargets, .(PSNU_list), 
            function(x){
              ########### PREP
              # PREP_NEW
              # COP targets for PrEP_NEW clients = COP targets for PrEP_CURR clients starting PrEP in upcoming COP
              prep_new_target <- x$cop_target[x$pathway=='PrEP_NEW_Total']
              
              # no of clients served
              # 5. What percentage of people who start PrEP in the upcoming COP year will likely start PrEP in a 
              #    health facility (vs. community)? 
              clients_served_fac <- prep_new_target*customPar$qn[5]        # facility
              clients_served_com <- prep_new_target*(1-customPar$qn[5])    # community
              
              # total no. of visits annually
              tot_visits_fac <- clients_served_fac*1       # Facility
              tot_visits_com <- clients_served_com*1       # community
              
              # Total no. of minutes
              tot_mins <- list( 
                # 1. Are there case managers in place for follow-up of PrEP_NEW clients? 
                # 14. How many clients would you expect to need to screen to get to the total PrEP_NEW target?
                'Case Manager', tot_visits_fac*(15*customPar$qn[14]*100/prep_new_target + ifelse(customPar$resp[1], 10*1, 0)),
                tot_visits_com*(15*customPar$qn[14]*100/prep_new_target + ifelse(customPar$resp[1], 10*1, 0)),
                'Clinical-Medical', tot_visits_fac*20*1, NA,
                'Clinical-Nursing', NA, tot_visits_com*25*1,
                'Data Clerk', tot_visits_fac*(10*1 + 10*1), tot_visits_com*(10*1 + 10*1), 
                'Laboratory', tot_visits_fac*20*1, tot_visits_com*20*1,
                'Lay-CHW', NA, NA,
                'Lay-Counselor', tot_visits_fac*20*customPar$qn[14]*100/prep_new_target, 
                tot_visits_com*20*customPar$qn[14]*100/prep_new_target,
                'Pharmacy', tot_visits_fac*15*1, NA)
              
              tot_mins_PrEP_NEW <- data.frame(pathway='PrEP_NEW', matrix(unlist(tot_mins), ncol = 3, byrow = T))
              names(tot_mins_PrEP_NEW) <- c('pathway','cadre','tot_mins_fac','tot_mins_com')
              
              # PrEP_CURR
              # COP targets for PrEP_CURR rolling over on PrEP from previous COP year
              prep_roll_targ <- x$cop_target[x$pathway=='PrEP_CURR_Total'] - prep_new_target
              
              # adjusted no. of visits per client
              #  ?????? where is the input for: Adjustment for clients that initiate PrEP throughout the year (e.g. scale-up)
              # !!!! this should be added
              # 15. How many follow up visits, on average, will people who start PrEP in the upcoming COP year likely have?
              # 17. What is the targeted continuation rate for clients starting PrEP in the upcoming COP year? 
              # 16. How many follow up visits, on average, will people who started PrEP in the current COP year likely have?
              # 18. What is the targeted continuation rate for clients rolling over on PrEP from the current COP year? 
              # !!!! the 0.5 below should be replaced with "Fixed values to include in R-script. This was decided upon with USAID." 
              adj_visits_start <- customPar$qn[15]*customPar$qn[17]*0.5    #client starting PrEP
              adj_visits_roll <- customPar$qn[16]*customPar$qn[18]     #client rolling over
              
              # no of clients served
              # 6. What percentage of people who start PrEP in the upcoming COP year will likely continue 
              #    PrEP from a health facility (vs. community)?
              # 7. What percentage of people who started PrEP in the current COP year will likely continue 
              #    PrEP from a health facility (vs. community)?
              clients_served_fac_start <- prep_new_target*customPar$qn[6]        # starting PrEP served at facility
              clients_served_com_start <- prep_new_target*(1-customPar$qn[6])    # starting PrEP served at community
              clients_served_fac_roll <- prep_roll_targ*customPar$qn[7]          # rolling over PrEP served at facility
              clients_served_com_roll <- prep_roll_targ*(1-customPar$qn[7])      # rolling over PrEP served at community
              
              # total no. of visits annually
              tot_visits_fac <- clients_served_fac_start*adj_visits_start + clients_served_fac_roll*adj_visits_roll # Facility
              tot_visits_com <- clients_served_com_start*adj_visits_start + clients_served_com_roll*adj_visits_roll # community
              
              # Total no. of minutes
              tot_mins <- list( 
                # 2. Are there case managers in place for follow-up of PrEP_CURR clients? 
                # 13. What percentage of people who continue PrEP will have their PrEP delivered to their home? 
                'Case Manager', tot_visits_fac*(ifelse(customPar$resp[2], 10*1, 0)*1 + 90*customPar$qn[13]), 
                tot_visits_com*(ifelse(customPar$resp[2], 10*1, 0)*1 + 90*customPar$qn[13]),
                'Clinical-Medical', tot_visits_fac*10*1, NA,
                'Clinical-Nursing', NA, tot_visits_com*25*1,
                'Data Clerk', tot_visits_fac*10*1, tot_visits_com*10*1, 
                'Laboratory', tot_visits_fac*20*1, tot_visits_com*20*1,
                'Lay-CHW', NA, NA,
                'Lay-Counselor', tot_visits_fac*30*1, tot_visits_com*30*1,
                'Pharmacy', tot_visits_fac*15*1, tot_visits_com*15*1)
              
              tot_mins_PrEP_CURR <- data.frame(pathway='PrEP_CURR', matrix(unlist(tot_mins), ncol = 3, byrow = T))
              names(tot_mins_PrEP_CURR) <- c('pathway','cadre','tot_mins_fac','tot_mins_com')
              
              ########### HTS
              # HTS_SELF (Total)
              # COP targets for HTS_SELF clients
              hts_self_target <- x$cop_target[x$pathway=='HTS_SELF_Total']
              
              # no of clients served
              # 19. What percentage of HTS_SELF are unassisted (vs. assisted)?
              clients_served_unass <- hts_self_target*customPar$qn[19]        # unassisted
              # 8. What percentage of assisted HTS_SELF are seen at a facility (vs. community)? 
              clients_served_fac <- hts_self_target*(1-customPar$qn[19])*customPar$qn[8]      # assisted facility
              clients_served_com <- hts_self_target*(1-customPar$qn[19])*(1-customPar$qn[8])    # assisted community
              
              # total no. of visits annually
              tot_visits_unass <- clients_served_unass*1       # unassisted
              tot_visits_fac <- clients_served_fac*1       # Facility
              tot_visits_com <- clients_served_com*1       # community
              
              # Total no. of minutes
              tot_mins <- list( 
                'Case Manager', NA, NA, NA,
                'Clinical-Medical', NA, NA, NA,
                'Clinical-Nursing', NA, NA, NA,
                'Data Clerk', tot_visits_unass*10*1, tot_visits_fac*(10*1 + 5*1), tot_visits_com*(10*1 + 5*1), 
                'Laboratory', NA, NA, NA,
                # 20. What percentage of assisted HTS_SELF  are seen at a facility by a Lay-Counselor (vs. Lay-CHW)?
                # 21. What percentage of unassisted HTS_SELF  are seen by a Lay-Counselor (vs. Lay-CHW):
                'Lay-CHW', tot_visits_unass*2*customPar$qn[21], 
                tot_visits_fac*(10*customPar$qn[20] + 10*0.05), 
                tot_visits_com*(20*1 + 15*0.05),
                'Lay-Counselor', tot_visits_unass*2*(1 - customPar$qn[21]), 
                tot_visits_fac*(10*(1 - customPar$qn[20]) + 10*0.05), 
                NA,
                'Pharmacy', NA, NA, NA)
              
              tot_mins_HTS_SELF <- data.frame(pathway='HTS_SELF', matrix(unlist(tot_mins), ncol = 4, byrow = T))
              names(tot_mins_HTS_SELF) <- c('pathway','cadre','tot_mins_unass','tot_mins_fac','tot_mins_com')
              
              
              tot_mins <- tot_mins_PrEP_CURR %>% full_join(tot_mins_PrEP_NEW) %>% 
                full_join(tot_mins_HTS_SELF)
            })

#Import data from DCT
# ==================================================================================================
# PSNU List
PSNUList <- read_excel(dct, sheet = "1. PSNU List", skip = 2)
# Program targets
ProgramTargets <- read_excel(dct, sheet = "2. Program targets ", na = "0", skip = 3)
# Current PEPFAR HRH
# AF: In the Data Collection Template on Current PEPFAR HRH tab, which columns are each "total" column summing?
CurrentHRH <- read_excel(dct, sheet = "3. Current PEPFAR HRH ", skip = 3)
# Current HRH Salaries
# AF: this returns a warning message and says that it generates an NA. I plugged a value into the Data Clerk cell on the HRH Salaries tab of the Data Collection Template and it worked properly, so this warning message is not of concern.
CurrentSalaries <- read_excel(dct, sheet = "4. HRH Salaries", range = "A5:B13")
colnames(CurrentSalaries) <- c("Cadre", "CurrentSalaries")
CurrentSalaries[is.na(CurrentSalaries)] <- 0
# Service Standard
ClientTime <- read_excel(dct, sheet = "Client Time", range = "A1:K65")
# Program Areas
ProgramArea <- read_excel("HRH Data Collection/ProgramAreas.xlsx", sheet = "Program Area")
# Available working time
AvailableWorkingTime <- read_excel(dct, sheet = "9. Available Working Time", range = "B4:I12")
# Default non-clinical work hours per week
WeeklyNonClinicalWorkingHours <- read_excel(dct, sheet = "9. Available Working Time", range = "B17:C25")

# Format data set columns to usable names
# ==================================================================================================
ProgramTargets <- ProgramTargets %>% filter(!is.na(`PSNU list`))
colnames(WeeklyNonClinicalWorkingHours) <- c("Cadre", "WeeklyNonClinicalWorkingHours")
names(AvailableWorkingTime) <-
  str_replace_all(names(AvailableWorkingTime), c(" " = "" , "," = "" ,  "/" = "Per"))
names(PSNUList) <- str_replace_all(names(PSNUList), c(" " = ""))
names(ProgramTargets) <- str_replace_all(names(ProgramTargets), c(" " = ""))
names(ProgramTargets) <- stri_replace_all_fixed(names(ProgramTargets), "(Total)", "")
CurrentHRH <- CurrentHRH %>% rename(RefID = ...1, PSNU = ...2)
PSNUList <- PSNUList %>% filter(!is.na(PSNU))
CurrentHRH <- CurrentHRH %>% filter(!is.na(RefID))
CurrentHRH <- CurrentHRH %>% filter(!is.na(PSNU))

# Calculations
# ==================================================================================================

# Available work time
# -------------------
AvailableWorkingTime <- AvailableWorkingTime %>%
  mutate(AWT_Days = (WorkingDaysPerWeek * 52) -
      (AnnualLeave + PublicHolidays + SickLeave + TrainingDaysperYear)) %>%
  mutate(AWT_Hours = AWT_Days * WorkinghrsPerday) %>%
  rename(Cadre = ...1) %>%
  select(Cadre, WorkingDaysPerWeek, WorkinghrsPerday, AWT_Days, AWT_Hours)

# Program Targets
# ----------------
# Select
ProgramTargets <- ProgramTargets %>%
  rename(TX_CURR = `TX_CURR `) %>%
  rename(PSNU = PSNUlist) %>%
  mutate(
    HTS_TST = `HTS_TST(Mobile)` + `HTS_TST(IndexMod)` + `HTS_TST(FacilityIndex)` + `HTS_TST(PMTCT_ANC1)` + `HTS_TST(PMTCT_PostANC1)` + `HTS_TST(STI)` + `HTS_TST(OtherPITC)` + `HTS_TST(Inpatient)`
  ) %>%
  select(RefID,PSNU,PrEP_NEW,PrEP_CURR,HTS_SELF,HTS_TST,TX_NEW,TX_CURR,PMTCT_ART,TX_PVLS)
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
  ProgramTargets_TX_PVLS)

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
  CurrentHRH_DataClerk)
CurrentHRH_Formated <- rbindlist(CurrentHRHList)
CurrentHRH_Formated[is.na(CurrentHRH_Formated)] <- 0

# Dashboard one data
Data_With_HCW <- full_join(PSNUList, AvailableWorkingTime, by = character()) %>%
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
  #calculate service standard
  mutate(ServiceStandard = ifelse(ClientTime > 0, 60 / ClientTime, 0)) %>%
  #calculate Annual workload
  mutate(AnnualWorkload = ProgramTargets * ServiceStandard) %>%
  mutate(StandardWorkload = ServiceStandard * AWT_Hours) %>%
  #Category Allowed Standard
  mutate(CategoryAllowedStandard = (
    WeeklyNonClinicalWorkingHours / (WorkingDaysPerWeek * WorkinghrsPerday)
  ) / WorkinghrsPerday) %>%
  #Category Allowed Factor
  mutate(CategoryAllowedFactor = 1 / (1 - (CategoryAllowedStandard / AWT_Hours * 100))) %>%
  mutate(HRHRequirement = (AnnualWorkload / StandardWorkload) * CategoryAllowedFactor) %>%
  mutate(Country = OperatingUnit) %>%
  # Assumption that the current HRH are equally distributed amongst the different program areas
  mutate(CurrentHRH = CurrentHRH / 8) %>%
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
# Prioritization ranking
PRI_List <- HRHData %>%
  # defaults to zero as this is missing from the DCT
  mutate(`Newly Assigned Staff` = 0) %>%
  select(Country,
         PSNU,
         Cadre,
         `Existing FTEs`,
         `HCW Need`,
         `Newly Assigned Staff`) %>%
  group_by(Country,
           PSNU,
           Cadre) %>%
  summarise_each(funs(sum)) %>%
  # PRI 0 when there is a surplus and 99999 where there are no existing staff
  mutate(PRI = case_when(
    `Existing FTEs` > `HCW Need` ~ 0,
    `Existing FTEs` == 0  ~ 99999,
    TRUE ~ (`HCW Need` - (`Existing FTEs` + `Newly Assigned Staff`)) /
      ((`Existing FTEs` + `Newly Assigned Staff`) * 100)
  )) %>%
  mutate(PRI = round(PRI, digits = 4))

PRI_Ranking <- PRI_List %>%
  select(PRI,
         Cadre,
         PSNU,
         Country) %>%
  arrange(desc(PRI))


# write output files
# ==================================================================================================

# Dashboard one data
write.csv(HRHData,
          glue('{output_location}HRHData.csv'),
          row.names = TRUE)
# PRI List
write.csv(PRI_List,
          glue('{output_location}PRI_List.csv'),
          row.names = TRUE)
# PRI Ranking
write.csv(PRI_Ranking,
          glue('{output_location}PRI_Ranking.csv'),
          row.names = TRUE)
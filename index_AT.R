## Data Transformation Script for HRH Needs and Optimization Solution
   # Extract data from Excel based Data Collection Template
   # Transform the data appropriately and perform calculations
   # Output transformed data into prescribed csv format
## Date modified: 21 October 2020

# install packages if not available
# ===================================================================================================
PackagesList <- c("readxl","plyr","tidyverse")
Packages <- PackagesList[!(PackagesList %in% installed.packages()[, "Package"])]
if (length(Packages)) install.packages(Packages)

library(readxl)
library(plyr)
library(dplyr)
library(tidyr)

# set path variables
# ==================================================================================================
setwd('C:/Users/owner/Documents/palladium/HRH/DataFi-HRH/HRH-DataFiles')

#Import data from DCT
# ==================================================================================================
dct <- "Data Collection Templates/2020 10 19 Data.FI HRH Solution Data Collection Template.USAID_Nigeria pilot.xlsx"
# dct <- "Data Collection Templates/2020 10 01 Data.FI HRH Solution Data Collection Template_TZ.lp.droppedQn22Qn24.xlsx"
# dct <- "HRH Data Collection/2020 10 13 Data.FI HRH Solution Data Collection Template.USAID_TZ.xlsx"

# Customization parameters
customPar <- read_excel(dct, sheet = "5. Customization Parameters", skip = 4, col_names = F)
names(customPar) <- c('param','question','response')
customPar$qn <- as.numeric(customPar$response)                                                    # % response
customPar$resp <- ifelse(customPar$response %in% c('Yes','No'), customPar$response=='Yes', NA)    # yes/no response

# Program targets
program_targets <- read_excel(dct, sheet = "2. Program Targets ", na = "0", skip = 3)

# replace problematic spaces and brackets in variable names with underscores
names(program_targets) <- gsub("\\s\\(", '_', gsub("\\)", '', names(program_targets)))
names(program_targets) <- gsub("\\s+", '_', names(program_targets))
names(program_targets) <- gsub("__", '_', names(program_targets))

# Calculations
# ==================================================================================================
# Total no. of minutes needed by a HW annually
# --------------------------------------
program_targets <- as.data.frame(program_targets) %>%
  filter(!is.na(PSNU)) %>% # filter out entirely empty rows that come with the excel sheet
  select(-DATIM_UID,-Ref_ID)

names(program_targets)[grepl('PMTCT_ART_New', names(program_targets))] <- 'PMTCT_ART_New'
names(program_targets)[grepl('PMTCT_ART_Already', names(program_targets))] <- 'PMTCT_ART_Already'

for (i in (1:length(names(program_targets)))[!(names(program_targets) %in% c("Ref_ID","PSNU"))]) {
  program_targets[,i] <- ifelse(is.na(program_targets[,i]), 0, program_targets[,i])
}

program_targets_time <- program_targets %>% 
  gather(target, cop_target,-PSNU)

tot_mins <- ddply(program_targets_time, .(PSNU), 
            function(x){
              TOT_MINS <- list()
              
              ########### PREP
              # [1] PREP_NEW General population
              prep_new_target <- x$cop_target[x$target=='PrEP_NEW_Total']     # COP target
              
              # no of clients served
              # 5. What percentage of people who start PrEP in the upcoming COP year will likely start PrEP in a 
              #    health facility (vs. community)? 
              clients_served_fac <- customPar$qn[5]        # facility
              clients_served_com <- 1-customPar$qn[5]    # community
              
              # total no. of visits annually
              tot_visits_fac <- clients_served_fac*1       # Facility
              tot_visits_com <- clients_served_com*1       # community
              
              # Total no. of minutes
              tot_mins <- list( 
                # 1. Are there case managers in place for follow-up of PrEP_NEW clients? 
                # 14. In the planning COP year, how many clients would you expect to need to screen to reach the PrEP_NEW target?
                'Case Manager', tot_visits_fac*(ifelse(prep_new_target==0, 0, 15*customPar$qn[14]*100/prep_new_target) + 
                                                ifelse(customPar$resp[1], 10*1, 0)) +
                tot_visits_com*(ifelse(prep_new_target==0, 0, 15*customPar$qn[14]*100/prep_new_target) + 
                                ifelse(customPar$resp[1], 10*1, 0)),
                'Clinical-Medical', tot_visits_fac*20*1 + 0,
                'Clinical-Nursing', 0 + tot_visits_com*25*1,
                'Data Clerk', tot_visits_fac*(10*1 + 10*1) + tot_visits_com*(10*1 + 10*1), 
                'Laboratory', tot_visits_fac*20*1 + tot_visits_com*20*1,
                'Lay-CHW', 0 + 0,
                'Lay-Counselor', ifelse(prep_new_target==0, 0, tot_visits_fac*20*customPar$qn[14]*100/prep_new_target) + 
                                 ifelse(prep_new_target==0, 0, tot_visits_com*20*customPar$qn[14]*100/prep_new_target),
                'Pharmacy', tot_visits_fac*15*1 + 0)
              
              TOT_MINS[[1]] <- data.frame(pathway='PrEP_NEW - GP', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F) 
              
              # [2] PREP_NEW key populations
              TOT_MINS[[2]] <- data.frame(pathway='PrEP_NEW - KP', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F) 
              
              
              # [3] PrEP_CURR General population 
              # no of clients served
              # 6. What percentage of people who start PrEP in the upcoming COP year will likely continue 
              #    PrEP from a health facility (vs. community)?
              # 7. What percentage of people who started PrEP in the current COP year will likely continue 
              #    PrEP from a health facility (vs. community)?
              clients_served_fac_start <- customPar$qn[6]        # starting PrEP served at facility
              clients_served_com_start <- 1-customPar$qn[6]    # starting PrEP served at community
              clients_served_fac_roll <- customPar$qn[7]          # rolling over PrEP served at facility
              clients_served_com_roll <- 1-customPar$qn[7]      # rolling over PrEP served at community
              
              # adjusted no. of visits per client
              # 15. How many continuation visits, on average, will clients who start PrEP in the planning COP 
              #     year have in the planning COP year?
              # 17. What percentage of clients who start PrEP in the planning COP year would you aim to be on PrEP at 
              #     the end of the planning COP year?
              # 16. How many continuation visits, on average, will clients who started PrEP in the year preceding the 
              #     planning COP year have in the planning COP year?
              # 18. What percentage of clients who started PrEP in the year preceding the planning COP year and continue 
              #     PrEP into the planning COP year would you aim to be on PrEP at the end of the planning COP year? 
              adj_visits_start <- customPar$qn[15]*customPar$qn[17]*0.5    #client starting PrEP
              adj_visits_roll <- customPar$qn[16]*customPar$qn[18]     #client rolling over
              
              # total no. of visits annually
              tot_visits_fac <- clients_served_fac_start*adj_visits_start + clients_served_fac_roll*adj_visits_roll # Facility
              tot_visits_com <- clients_served_com_start*adj_visits_start + clients_served_com_roll*adj_visits_roll # community
              
              # Total no. of minutes
              tot_mins <- list( 
                # 2. Are there case managers in place for follow-up of PrEP_CURR clients? 
                # 13. What percentage of people who continue PrEP will have their PrEP delivered to their home? 
                'Case Manager', tot_visits_fac*(ifelse(customPar$resp[2], 10*1, 0)*1 + 90*customPar$qn[13]) + 
                tot_visits_com*(ifelse(customPar$resp[2], 10*1, 0)*1 + 90*customPar$qn[13]),
                'Clinical-Medical', tot_visits_fac*10*1 + 0,
                'Clinical-Nursing', 0 + tot_visits_com*25*1,
                'Data Clerk', tot_visits_fac*10*1 + tot_visits_com*10*1, 
                'Laboratory', tot_visits_fac*20*1 + tot_visits_com*20*1,
                'Lay-CHW', 0 + 0,
                'Lay-Counselor', tot_visits_fac*30*1 + tot_visits_com*30*1,
                'Pharmacy', tot_visits_fac*15*1 + 0)
              
              TOT_MINS[[3]] <- data.frame(pathway='PrEP_CURR - GP', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F)
              
              # [4] PrEP_CURR key population
              TOT_MINS[[4]] <- data.frame(pathway='PrEP_CURR - KP', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F)
              
              ########### HTS
              # [5] HTS_SELF (Total)
              # no of clients served
              # 19. What percentage of HTS_SELF are unassisted (vs. assisted)?
              clients_served_unass <- customPar$qn[19]        # unassisted
              # 8. What percentage of assisted HTS_SELF are seen at a facility (vs. community)? 
              clients_served_fac <- (1-customPar$qn[19])*customPar$qn[8]      # assisted facility
              clients_served_com <- (1-customPar$qn[19])*(1-customPar$qn[8])    # assisted community
              
              # total no. of visits annually
              tot_visits_unass <- clients_served_unass*1       # unassisted
              tot_visits_fac <- clients_served_fac*1       # Facility
              tot_visits_com <- clients_served_com*1       # community
              
              # Total no. of minutes
              tot_mins <- list( 
                'Case Manager', 0 + 0 + 0,
                'Clinical-Medical', 0 + 0 + 0,
                'Clinical-Nursing', 0 + 0 + 0,
                'Data Clerk', tot_visits_unass*10*1 + tot_visits_fac*(10*1 + 5*1) + tot_visits_com*(10*1 + 5*1), 
                'Laboratory', 0 + 0 + 0,
                # 20. What percentage of assisted HTS_SELF  are seen at a facility by a Lay-Counselor (vs. Lay-CHW)?
                # 21. What percentage of unassisted HTS_SELF  are seen by a Lay-Counselor (vs. Lay-CHW):
                'Lay-CHW', tot_visits_unass*2*(1 - customPar$qn[21]) +       # unassisted
                           tot_visits_fac*(10*(1 - customPar$qn[20]) + 10*0.05) +    # assisted facility
                           tot_visits_com*(20*1 + 15*0.05),    # assisted community
                'Lay-Counselor', tot_visits_unass*2*customPar$qn[21] +  # unassisted
                           tot_visits_fac*(10*customPar$qn[20] + 10*0.05) + 0,      # assisted facility
                'Pharmacy', 0 + 0 + 0)
              
              TOT_MINS[[5]] <- data.frame(pathway='HTS_SELF', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F)
              
              # [6] HTS_TST_Mobile
              # Total no. of minutes
              tot_mins <- list( 
                'Case Manager', 0,
                'Clinical-Medical', 0,
                'Clinical-Nursing', 0,
                'Data Clerk', 10*0.8, 
                'Laboratory', 30*1, 
                'Lay-CHW', 0, 
                'Lay-Counselor', 55*1, 
                'Pharmacy', 0)
              
              TOT_MINS[[6]] <- data.frame(pathway='HTS_TST_Mobile', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F)
              
              # Total no. of minutes
              htsTime <- function(pathway) {
                
                cm_deliver_test_time <- ifelse(pathway %in% c('HTS_TST_PMTCT_ANC1','HTS_TST_PMTCT_Post_ANC1','HTS_TST_Facility_Index'),
                                               10, 0)
                
                tot_mins <- list( 
                  # 3.  Can Case Managers provide testing? 
                  'Case Manager', cm_deliver_test_time + ifelse(customPar$resp[3], 30*1, 0),
                  'Clinical-Medical', 0,
                  'Clinical-Nursing', 30*0.9,
                  'Data Clerk', 10*1, 
                  'Laboratory', 0 + 30*0.1,
                  'Lay-CHW', 0, 
                  'Lay-Counselor', 0,
                  'Pharmacy', 0)
                
                tot_mins <- data.frame(pathway=pathway, matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F)
              }
              
              TOT_MINS[[7]] <- htsTime('HTS_TST_PMTCT_ANC1')          # [5] HTS_TST_PMTCT_ANC1
              TOT_MINS[[8]] <- htsTime('HTS_TST_PMTCT_Post_ANC1')     # [6] HTS_TST_PMTCT_Post_ANC1
              TOT_MINS[[9]] <- htsTime('HTS_TST_Facility_Index')      # [7] HTS_TST_Facility_Index
              
              TOT_MINS[[10]] <- htsTime('HTS_TST_STI')                 # [8] HTS_TST_STI 
              TOT_MINS[[11]] <- htsTime('HTS_TST_Other_PITC')          # [9] HTS_TST_Other_PITC
              TOT_MINS[[12]] <- htsTime('HTS_TST_Inpatient')          # [10] HTS_TST_Inpatient
              
              # [13] HTS_TST_IndexMod
              # Total no. of minutes
              tot_mins <- list( 
                # 4.  Can Lay-Counselors provide testing? 
                'Case Manager', 0,
                'Clinical-Medical', ifelse(customPar$resp[4], 0, 60*0.9),
                'Clinical-Nursing', ifelse(customPar$resp[4], 0, 60*0.05),
                'Data Clerk', 10*1, 
                'Laboratory', ifelse(customPar$resp[4], 0, 60*0.05),
                'Lay-CHW', ifelse(customPar$resp[4], 0, 60*1), 
                'Lay-Counselor', ifelse(customPar$resp[4], 125*1, 0),
                'Pharmacy', 0)
              
              TOT_MINS[[13]] <- data.frame(pathway='HTS_TST_IndexMod', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F)
              
              ########### TX 
              # [14] TX_NEW General Patients 
              # no of clients served
              # 9. What percentage of TX_NEW: general population are seen at a facility (vs. community)? 
              clients_served_fac <- customPar$qn[9]        # facility
              clients_served_com <- 1-customPar$qn[9]    # community
              
              # total no. of visits annually
              tot_visits_fac <- clients_served_fac*1       # Facility
              tot_visits_com <- clients_served_com*1       # community
              
              # Total no. of minutes
              tot_mins <- list( 
                #resp[22] 22. Among TX_NEW seen at a facility, is initiation primarily nurse-led?   
                'Case Manager', tot_visits_fac*(50*1) + tot_visits_com*(50*1),
                'Clinical-Medical', tot_visits_fac*ifelse(customPar$resp[22], 0, 30*1) + tot_visits_com*40*1,
                'Clinical-Nursing', tot_visits_fac*ifelse(customPar$resp[22], 60*1, 10*1 + 10*1) + 0,
                'Data Clerk', tot_visits_fac*(10*1 + 15*1) + tot_visits_com*(10*1 + 15*1), 
                'Laboratory', tot_visits_fac*30*1 + tot_visits_com*30*1,
                'Lay-CHW', 0 + 0,
                'Lay-Counselor', 0 + 0,
                'Pharmacy', tot_visits_fac*ifelse(customPar$resp[22], 0, 15*1) + tot_visits_com*15*1)
              
              TOT_MINS[[14]] <- data.frame(pathway='TX_NEW', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F) 
              
              # [15] TX_NEW_KP 
              # no of clients served
              # 10.  What percentage of TX_NEW: KP are seen at a facility (vs. community? 
              clients_served_fac <- customPar$qn[10]        # facility
              clients_served_com <- 1-customPar$qn[10]    # community
              
              # total no. of visits annually
              tot_visits_fac <- clients_served_fac*1       # Facility
              tot_visits_com <- clients_served_com*1       # community
              
              # Total no. of minutes
              tot_mins <- list( 
                #resp[22] 22. Among TX_NEW seen at a facility, is initiation primarily nurse-led?   
                'Case Manager', tot_visits_fac*(50*1) + tot_visits_com*(50*1),
                'Clinical-Medical', tot_visits_fac*ifelse(customPar$resp[22], 0, 30*1) + tot_visits_com*40*1,
                'Clinical-Nursing', tot_visits_fac*ifelse(customPar$resp[22], 60*1, 10*1 + 10*1) + 0,
                'Data Clerk', tot_visits_fac*(10*1 + 15*1) + tot_visits_com*(10*1 + 15*1), 
                'Laboratory', tot_visits_fac*30*1 + tot_visits_com*30*1,
                'Lay-CHW', 0 + 0,
                'Lay-Counselor', 120*1.72,
                'Pharmacy', tot_visits_fac*ifelse(customPar$resp[22], 0, 15*1) + tot_visits_com*15*1)
              
              TOT_MINS[[15]] <- data.frame(pathway='TX_NEW_KP', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F) 
              
              # [16] PMTCT_ART_New_on_life-long_ART 
              # Total no. of minutes
              tot_mins <- list( 
                'Case Manager', 40*1 + 30*1, 
                'Clinical-Medical', 0,
                'Clinical-Nursing', 30*1,
                'Data Clerk', 25*1, 
                'Laboratory', 30*1,
                'Lay-CHW', 0,
                'Lay-Counselor', 0,
                'Pharmacy', 0)
              
              TOT_MINS[[16]] <- data.frame(pathway='PMTCT_ART_New', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F)
              
              # [17] TX_CURR General Patients: Drug Dispensing Frequency & Clinical Consultations
              # no of clients served
              # 11.  What percentage of TX_CURR: general population are seen at a facility (vs. community)? 
              clients_served_fac <- customPar$qn[11]        # facility
              clients_served_com <- 1-customPar$qn[11]    # community
              
              # total no. of visits annually
              tot_visits_fac <- clients_served_fac*1       # Facility
              tot_visits_com <- clients_served_com*1       # community
              
              # Total no. of minutes
              tot_mins <- list( 
              # 23.  What percentage of TX_CURR are receiving drugs on the following schedules?
                  # qn[24]. <3 months
                  # qn[25]. 3-5 months
                  # qn[26]. 6 months or more
                
                # qn[31] 25. Among TX_CURR, is the tracing of clients to their home due to missed appointments done primarily 
                #     by a Lay-CHW or a Case Manager? 
                # qn[32] 26. What percentage of TX_CURR are traced to their home due to missed appointments?
                'Case Manager', tot_visits_fac*(                                                  
                          (15*1 + ifelse(customPar$response[31]=='Lay-CHW', 0,   
                                     120*customPar$qn[32]))*(               # Drug Dispensing Frequency
                                         12*customPar$qn[24] +                   # <3 months
                                         4*customPar$qn[25] +                    # 3-5 months     
                                         2*customPar$qn[26]) +                   # 6 months or more    
                          (15*customPar$qn[24]*(                          # Clinical Consultations
                                         12*customPar$qn[28] +                   # <3 months
                                         0 +                                     # 3-5 months
                                         0))) +                                  # 6 months or more
                      tot_visits_com*(                                                  
                          (15*1 + ifelse(customPar$response[31]=='Lay-CHW', 0,            
                                    120*customPar$qn[32]))*(              # Drug Dispensing Frequency
                                        12*customPar$qn[24] +                    # <3 months
                                        4*customPar$qn[25] +                     # 3-5 months
                                        2*customPar$qn[26]) +                    # 6 months or more
                      0),                            
                'Clinical-Medical', tot_visits_fac*20*1*(0 +              # Clinical Consultations
                                        12*customPar$qn[28] +                    # <3 months
                                        4*customPar$qn[29] +                     # 3-5 months
                                        2*customPar$qn[30]) +                    # 6 months or more
                       tot_visits_com*20*1*(0 +                           # Clinical Consultations
                                         12*customPar$qn[28] +                   # <3 months
                                         4*customPar$qn[29] +                    # 3-5 months
                                         2*customPar$qn[30]),                    # 6 months or more
                'Clinical-Nursing', tot_visits_fac*(                      
                                      15*1*(                              # Drug Dispensing Frequency
                                         12*customPar$qn[24] +                   # <3 months
                                         4*customPar$qn[25] +                    # 3-5 months
                                         2*customPar$qn[26]) +                   # 6 months or more
                                      5*1*(                               # Clinical Consultations
                                         12*customPar$qn[28] +                   # <3 months
                                         4*customPar$qn[29] +                    # 3-5 months
                                         2*customPar$qn[30])) +                  # 6 months or more
                                        (0 + 0),
                'Data Clerk', tot_visits_fac*(                                                  
                                      10*1*(                              # Drug Dispensing Frequency
                                         12*customPar$qn[24] +                   # <3 months
                                         4*customPar$qn[25] +                    # 3-5 months
                                         2*customPar$qn[26]) +                   # 6 months or more
                                      10*1*(                              # Clinical Consultations
                                         12*customPar$qn[28] +                   # <3 months
                                         4*customPar$qn[29] +                    # 3-5 months
                                         2*customPar$qn[30])) +                  # 6 months or more
                              tot_visits_com*(                                                                                              
                                      10*1*(                              # Drug Dispensing Frequency
                                         12*customPar$qn[24] +                   # <3 months
                                         4*customPar$qn[25] +                    # 3-5 months
                                         2*customPar$qn[26]) +                   # 6 months or more
                                      10*1*(                              # Clinical Consultations
                                         12*customPar$qn[28] +                   # <3 months
                                         4*customPar$qn[29] +                    # 3-5 months
                                         2*customPar$qn[30])),                   # 6 months or more
                'Laboratory', tot_visits_fac*(0 + 
                                      15*1*(                              # Clinical Consultations
                                         12*customPar$qn[28] +                   # <3 months
                                         4*customPar$qn[29] +                    # 3-5 months
                                         2*customPar$qn[30]) ) +                 # 6 months or more
                              tot_visits_com*(0 + 
                                      15*1*(                              # Clinical Consultations
                                         12*customPar$qn[28] +                   # <3 months
                                         4*customPar$qn[29] +                    # 3-5 months
                                         2*customPar$qn[30])),                   # 6 months or more
                'Lay-CHW', 
                              tot_visits_fac*(
                                ifelse(customPar$response[31]=='Lay-CHW', 120*customPar$qn[32], 0)*(# Drug Dispensing Frequency 
                                         12*customPar$qn[24] +                   # <3 months 
                                         4*customPar$qn[25] +                    # 3-5 months 
                                         2*customPar$qn[26]) +                   # 6 months or more 
                                 0) + 
                              tot_visits_com*(
                                ifelse(customPar$response[31]=='Lay-CHW', 120*customPar$qn[32], 0)*(# Drug Dispensing Frequency 
                                        12*customPar$qn[24] +                    # <3 months 
                                        4*customPar$qn[25] +                     # 3-5 months 
                                        2*customPar$qn[26]) +                    # 6 months or more 
                                    0),
                'Lay-Counselor', tot_visits_fac*(                                                                                   
                                   10*1*(                                   # Drug Dispensing Frequency
                                        12*customPar$qn[24] +                   # <3 months
                                        4*customPar$qn[25] +                    # 3-5 months
                                        2*customPar$qn[26]) +                   # 6 months or more 
                                    0) + 
                                  tot_visits_com*(                                                                                     
                                    10*1*(                                  # Drug Dispensing Frequency 
                                        12*customPar$qn[24] +                   # <3 months
                                        4*customPar$qn[25] +                    # 3-5 months
                                        2*customPar$qn[26]) +                   # 6 months or more 
                                      0),
                'Pharmacy', tot_visits_fac*(                                                                                      
                                    5*1*(                                   # Drug Dispensing Frequency  
                                        12*customPar$qn[24] +                   # <3 months
                                        4*customPar$qn[25] +                    # 3-5 months
                                        2*customPar$qn[26]) +                   # 6 months or more 
                                      0) + 
                            tot_visits_com*(                                                                                            
                                    5*1*(                                  # Drug Dispensing Frequency
                                        12*customPar$qn[24] +                  # <3 months
                                        4*customPar$qn[25] +                   # 3-5 months
                                        2*customPar$qn[26]) +                  # 6 months or more 
                                        0))
              
              TOT_MINS[[17]] <- data.frame(pathway='TX_CURR', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F) 
              
              # [18] PMTCT_ART_Already_on_life-long_ART_at_the_beginning_of_current_pregnancy 
              # Total no. of minutes
              tot_mins <- list( 
                # qn[37] 28. Among PMTCT_ART Already on Life-long ART at the beginning of the current pregnancy, is the tracing 
                #     of clients to their home due to missed appointments done primarily by a Lay-CHW or a Case Manager? 
                # qn[38] 29. What percentage of PMTCT_ART Already on Life-long ART at the beginning of the current pregnancy 
                #     seen at a facility are traced to their home due to missed appointments? 
                # qn[39] 30. What percentage of PMTCT_ ART Already on Life-long ART at the beginning of the current pregnancy are 
                #     unstable and have monthly visits to a facility (vs. those who are stable and have quarterly visits)? 
                'Case Manager', (
                                     30*1 + 
                                     ifelse(customPar$response[37]=='Lay-CHW', 0, 
                                                        120*customPar$qn[38]))*12*customPar$qn[39] +  # Unstable - Monthly
                                     (30*1 + 
                                      ifelse(customPar$response[37]=='Lay-CHW', 0, 
                                                        120*customPar$qn[38]))*4*(1 - customPar$qn[39]), # Stable - Quarterly
                'Clinical-Medical', 0,
                'Clinical-Nursing',           25*1*12*customPar$qn[39] +  # Unstable - Monthly
                                              25*1*4*(1 - customPar$qn[39]), # Stable - Quarterly
                'Data Clerk',                 10*1*12*customPar$qn[39] +  # Unstable - Monthly
                                              10*1*4*(1 - customPar$qn[39]), # Stable - Quarterly 
                'Laboratory', 0,
                'Lay-CHW',            ifelse(customPar$response[37]=='Lay-CHW', 120*customPar$qn[38], 
                                                               0)*12*customPar$qn[39] + # Unstable - Monthly
                                      ifelse(customPar$response[37]=='Lay-CHW', 120*customPar$qn[38], 
                                                               0)*4*(1 - customPar$qn[39]), # Stable - Quarterly
                'Lay-Counselor', 0,
                'Pharmacy', 15*1)
              
              TOT_MINS[[18]] <- data.frame(pathway='PMTCT_ART_Already', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F)
              
              # [19] TX_CURR Key Population: Drug Dispensing Frequency & Clinical Consultations
              # no of clients served
              # qn[12] 12.  What percentage of TX_CURR: KP are seen at a facility (vs. community)?
              clients_served_fac <- customPar$qn[12]        # facility
              clients_served_com <- 1-customPar$qn[12]    # community
              
              # total no. of visits annually
              tot_visits_fac <- clients_served_fac*1       # Facility
              tot_visits_com <- clients_served_com*1       # community
              
              # Total no. of minutes
              tot_mins <- list( 
                # response[31] 25. Among TX_CURR, is the tracing of clients to their home due to missed appointments done primarily by a Lay-CHW or a Case Manager? 
                # 29. What percentage of TX_CURR are traced to their home due to missed appointments?
                # 23.  What percentage of TX_CURR are receiving drugs on the following schedules?
                    # qn[24] <3 months
                    # qn[25] 3-5 months
                    # qn[26] 6 months or more
                # 24.  What percentage of TX_CURR  have clinical consults at the following frequencies?
                    # qn[28] <3 months, 
                    # qn[29] 3-5 months, 
                    # qn[30] 6 months or more
                'Case Manager', tot_visits_fac*(                                                  
                              (15*1 + ifelse(customPar$response[31]=='Lay-CHW', 0,   # Drug Dispensing Frequency
                                 120*customPar$qn[32]))*(12*customPar$qn[24] +            # <3 months
                                                          4*customPar$qn[25] +            # 3-5 months     
                                                          2*customPar$qn[26]) +           # 6 months or more    
                                        (15*customPar$qn[24]*(                      # Clinical Consultations
                                                       12*customPar$qn[28] +              # <3 months
                                                       0 +                                # 3-5 months
                                                       0))) +                             # 6 months or more
                                tot_visits_com*(                                                  
                                        (15*1 + ifelse(customPar$response[31]=='Lay-CHW', 0,            
                                                  120*customPar$qn[32]))*(          # Drug Dispensing Frequency
                                                      12*customPar$qn[24] +               # <3 months
                                                       4*customPar$qn[25] +               # 3-5 months
                                                       2*customPar$qn[26]) +              # 6 months or more
                                        0),                            
                'Clinical-Medical', tot_visits_fac*20*1*(0 +                        # Clinical Consultations
                                                      12*customPar$qn[28] +               # <3 months
                                                      4*customPar$qn[29] +                # 3-5 months
                                                      2*customPar$qn[30]) +               # 6 months or more
                                    tot_visits_com*20*1*(0 +                        # Clinical Consultations
                                                      12*customPar$qn[28] +               # <3 months
                                                      4*customPar$qn[29] +                # 3-5 months
                                                      2*customPar$qn[30]),                # 6 months or more
                'Clinical-Nursing', tot_visits_fac*(                                # Drug Dispensing Frequency
                                                    15*1*(
                                                      12*customPar$qn[24] +               # <3 months
                                                      4*customPar$qn[25] +                # 3-5 months
                                                      2*customPar$qn[26]) +               # 6 months or more
                                                                                                
                                                    5*1*(                           # Clinical Consultations
                                                      12*customPar$qn[28] +               # <3 months
                                                      4*customPar$qn[29] +                # 3-5 months
                                                      2*customPar$qn[30])) +              # 6 months or more
                                                   (0 + 0),
                'Data Clerk', tot_visits_fac*(                                                  
                                                    10*1*(                          # Drug Dispensing Frequency
                                                      12*customPar$qn[24] +               # <3 months
                                                      4*customPar$qn[25] +                # 3-5 months
                                                      2*customPar$qn[26]) +               # 6 months or more
                                                    10*1*(                          # Clinical Consultations
                                                      12*customPar$qn[28] +               # <3 months
                                                      4*customPar$qn[29] +                # 3-5 months
                                                      2*customPar$qn[30])) +              # 6 months or more
                              tot_visits_com*(                                                                                              
                                                    10*1*(                          # Drug Dispensing Frequency
                                                      12*customPar$qn[24] +               # <3 months
                                                      4*customPar$qn[25] +                # 3-5 months
                                                      2*customPar$qn[26]) +               # 6 months or more
                                                                                                
                                                    10*1*(                          # Clinical Consultations
                                                      12*customPar$qn[28] +               # <3 months
                                                      4*customPar$qn[29] +                # 3-5 months
                                                      2*customPar$qn[30])),               # 6 months or more
                'Laboratory', tot_visits_fac*(0 + 
                                                                                                
                                                    15*1*(                          # Clinical Consultations
                                                      12*customPar$qn[28] +               # <3 months
                                                      4*customPar$qn[29] +                # 3-5 months
                                                      2*customPar$qn[30]) ) +             # 6 months or more
                              tot_visits_com*(0 + 
                                                    15*1*(                          # Clinical Consultations
                                                      12*customPar$qn[28] +               # <3 months
                                                      4*customPar$qn[29] +                # 3-5 months
                                                      2*customPar$qn[30])),               # 6 months or more
                'Lay-CHW', 
                  tot_visits_fac*(
                    ifelse(customPar$response[31]=='Lay-CHW', 120*customPar$qn[32], 0)*(# Drug Dispensing Frequency 
                                                      12*customPar$qn[24] +               # <3 months 
                                                      4*customPar$qn[25] +                # 3-5 months 
                                                      2*customPar$qn[26]) +               # 6 months or more 
                      0) + 
                  tot_visits_com*(
                    ifelse(customPar$response[31]=='Lay-CHW', 120*customPar$qn[32], 0)*(# Drug Dispensing Frequency 
                                                      12*customPar$qn[24] +               # <3 months 
                                                      4*customPar$qn[25] +                # 3-5 months 
                                                      2*customPar$qn[26]) +               # 6 months or more 
                      0),
                'Lay-Counselor', tot_visits_fac*(                                       # Drug Dispensing Frequency                                            
                                                    10*1*(
                                                      12*customPar$qn[24] +               # <3 months
                                                      4*customPar$qn[25] +                # 3-5 months
                                                      2*customPar$qn[26]) +               # 6 months or more 
                                                0) + 
                              tot_visits_com*(                                                                                     
                                                    10*1*(                              # Drug Dispensing Frequency 
                                                      12*customPar$qn[24] +               # <3 months
                                                      4*customPar$qn[25] +                # 3-5 months
                                                      2*customPar$qn[26]) +               # 6 months or more 
                                            0),
                'Pharmacy', tot_visits_fac*(                                                                                      
                                                    5*1*(                              # Drug Dispensing Frequency  
                                                      12*customPar$qn[24] +               # <3 months
                                                      4*customPar$qn[25] +                # 3-5 months
                                                      2*customPar$qn[26]) +               # 6 months or more 
                                            0) + 
                            tot_visits_com*(                                                                                            
                                                    5*1*(                             # Drug Dispensing Frequency
                                                      12*customPar$qn[24] +               # <3 months
                                                      4*customPar$qn[25] +                # 3-5 months
                                                      2*customPar$qn[26]) +               # 6 months or more 
                                            0)
                )
              
              TOT_MINS[[19]] <- data.frame(pathway='TX_CURR_KP', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F) 
              
              # [20] TX_PVLS_Total 
              # 27. What percentage of TX_PVLS are targeted for the following schedules?
                      # qn[34] >2x/year, 
                      # qn[35] 2x/year
                      # qn[36] 1x/year
              # Total no. of minutes
              tot_mins <- list( 
                'Case Manager', (10*1 + 10*1)*(4*customPar$qn[34] +   # >2x/year
                                                      2*customPar$qn[35] +   # 2x/year
                                                      1*customPar$qn[36]),   # 1x/year
                'Clinical-Medical', (5*1 + 10*1)*(4*customPar$qn[34] +   # >2x/year
                                                         2*customPar$qn[35] +   # 2x/year
                                                         1*customPar$qn[36]),   # 1x/year
                'Clinical-Nursing', 0,
                'Data Clerk', 20*1*(4*customPar$qn[34] +   # >2x/year
                                           2*customPar$qn[35] +   # 2x/year
                                           1*customPar$qn[36]),   # 1x/year 
                'Laboratory', 15*1*(4*customPar$qn[34] +   # >2x/year
                                           2*customPar$qn[35] +   # 2x/year
                                           1*customPar$qn[36]),   # 1x/year
                'Lay-CHW', 0,
                'Lay-Counselor', 0,
                'Pharmacy', 0)
              
              TOT_MINS[[20]] <- data.frame(pathway='TX_PVLS', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F) 
              
              TOT_MINS <- do.call(rbind, TOT_MINS)
              names(TOT_MINS) <- c('pathway','cadre','tot_mins')
              return(TOT_MINS)
            })

client_time <- tot_mins %>% 
  mutate(target=gsub(".-\\s[A-Z]+$", '', pathway)) %>%  
  group_by(PSNU,target,cadre) %>% 
  summarise(tot_mins=sum(as.numeric(tot_mins))) %>% 
    ungroup()

# Available working time
# --------------------------------------
awt <- data.frame(cadre=c('Clinical-Nursing','Clinical-Medical','Data Clerk','Case Manager','Lay-Counselor',
                          'Lay-CHW','Laboratory','Pharmacy'), working_days_wk=5, working_hrs_day=8, 
                  public_holidays=15, special_leave=4, stringsAsFactors = F)

awt$training_days <- apply(awt['cadre'], 1, 
                           function(x) switch(x,
                                              'Clinical-Nursing' = 11,
                                              'Clinical-Medical' = 12,
                                              'Data Clerk' = 13,
                                              'Case Manager' = 14,
                                              'Lay-Counselor' = 14,
                                              'Lay-CHW' = 14,
                                              'Laboratory' = 11,
                                              'Pharmacy' = 11))

awt$weekly_non_clinical_hrs <- apply(awt['cadre'], 1, 
                           function(x) switch(x,
                                              'Clinical-Nursing' = 13,
                                              'Clinical-Medical' = 11,
                                              'Data Clerk' = 20,
                                              'Case Manager' = 12,
                                              'Lay-Counselor' = 12,
                                              'Lay-CHW' = 12,
                                              'Laboratory' = 11,
                                              'Pharmacy' = 11))

awt$adjusted_ftes_mins <- apply(awt['cadre'], 1, 
                           function(x) switch(x,
                                              'Clinical-Nursing' = 66744,
                                              'Clinical-Medical' = 69948,
                                              'Data Clerk' = 48000,
                                              'Case Manager' = 67872,
                                              'Lay-Counselor' = 67872,
                                              'Lay-CHW' = 67872,
                                              'Laboratory' = 71688,
                                              'Pharmacy' = 71688))
awt <- awt %>% mutate(
    annual_leave=ifelse(cadre %in% c('Clinical-Medical','Data Clerk'), 24, 21),
    sick_leave=ifelse(cadre %in% c('Clinical-Nursing','Laboratory','Pharmacy'), 3, 4),
    awt_days=working_days_wk*52 - (annual_leave+public_holidays+sick_leave+special_leave+training_days),
    awt_hrs=awt_days*8) 

# HRH need
# --------------------------------------
psnu_list <- read_excel(dct, sheet = "1. PSNU List", skip = 2)
psnu_list <- psnu_list %>% 
  rename(`Target Scenario`=`FY Target Scenario`) %>% 
  select(-`Record ID`,-`DATIM UID`) %>% 
  filter(!is.na(PSNU))

current_salaries <- read_excel(dct, sheet = "4. Avg. Annual HRH Remuneration", range = "A5:B13")
names(current_salaries) <- c('cadre','current_salaries')

current_hrh <- read_excel(dct, sheet = "3.Current PEPFAR-Supported HRH ", skip = 3)
current_hrh <- current_hrh[-1]

names(current_hrh) <- c('DATIM_UID','PSNU',
                        'Clinical_Medical_Total','Clinical_Medical_PrEP','Clinical_Medical_HTS','Clinical_Medical_TX',
                        'Clinical_Nursing_Total','Clinical_Nursing_PrEP','Clinical_Nursing_HTS','Clinical_Nursing_TX',
                        'Lay_Counselor_Total','Lay_Counselor_PrEP','Lay_Counselor_HTS','Lay_Counselor_TX',
                        'Lay_CHW_Total','Lay_CHW_PrEP','Lay_CHW_HTS','Lay_CHW_TX',
                        'Case_Manager_Total','Case_Manager_PrEP','Case_Manager_HTS','Case_Manager_TX',
                        'Pharmacy_Total','Pharmacy_PrEP','Pharmacy_HTS','Pharmacy_TX',
                        'Laboratory_Total','Laboratory_PrEP','Laboratory_HTS','Laboratory_TX',
                        'Data_Clerk_Total','Data_Clerk_PrEP','Data_Clerk_HTS','Data_Clerk_TX',
                        'Total_FTE')

current_hrh <- current_hrh %>% 
  filter(!is.na(PSNU),
         PSNU!="0",
         PSNU!='Total FTE') %>% 
  select(-matches('Total'), -DATIM_UID) %>%
  gather(cadre,current_hrh,Clinical_Medical_PrEP:Data_Clerk_TX) %>%
  mutate(cadre=gsub('_', '-', gsub('_Total', '', cadre)),
         program_area=gsub("\\w+-", '', cadre),
         cadre=gsub(".\\w+$", '', cadre),
         current_hrh=as.numeric(current_hrh),
         cadre=gsub('Case-Manager', 'Case Manager', cadre),
         cadre=gsub('Data-Clerk', 'Data Clerk', cadre)) %>% 
  full_join(current_salaries)

PrEP_Target <- c('COP','[-]15%','[+]15%','COP','[-]15%','[+]15%','COP','[-]15%','[+]15%','COP','[-]15%','[+]15%','COP',
                 '[-]15%','[+]15%','COP','[-]15%','[+]15%','COP','[-]15%','[+]15%','COP','[-]15%','[+]15%','COP','[-]15%','[+]15%')
HTS_Target <- c('COP','COP','COP','[-]15%','[-]15%','[-]15%','[+]15%','[+]15%','[+]15%','COP','COP','COP','[-]15%','[-]15%',
                '[-]15%','[+]15%','[+]15%','[+]15%','COP','COP','COP','[-]15%','[-]15%','[-]15%','[+]15%','[+]15%','[+]15%')
TX_Target <- c('COP','COP','COP','COP','COP','COP','COP','COP','COP','[-]15%','[-]15%','[-]15%','[-]15%','[-]15%','[-]15%',
               '[-]15%','[-]15%','[-]15%','[+]15%','[+]15%','[+]15%','[+]15%','[+]15%','[+]15%','[+]15%','[+]15%','[+]15%')

scenarios <- data.frame(PrEP_Target=PrEP_Target, HTS_Target=HTS_Target,	TX_Target=TX_Target, stringsAsFactors = F)
scenarios <- scenarios %>% 
  mutate(target_level=paste(PrEP_Target,HTS_Target,TX_Target, sep='&')) %>%
  gather(program_area,target_multiplier,PrEP_Target:TX_Target) %>% 
  mutate(target_multiplier=ifelse(target_multiplier=='COP', 1, ifelse(target_multiplier=='[-]15%', 0.85, 1.15)),
         program_area=gsub('_Target', '', program_area))

hrh <- program_targets %>% 
  mutate(TX_NEW=TX_NEW_Total - TX_NEW_KP - PMTCT_ART_New,
         TX_CURR=TX_CURR_Total - TX_CURR_KP - PMTCT_ART_Already) %>% 
  select(-TX_CURR_Total,-TX_NEW_Total) %>% 
  gather(target, cop_target,-PSNU) %>% 
  mutate(target=gsub('_Total', '', target),
         temp_target=ifelse(grepl('PMTCT_ART', target), 'TX', target),
         program_area=gsub("_\\w+$", '', temp_target)) %>% 
  full_join(client_time) %>% 
  full_join(awt) %>%
  full_join(psnu_list) %>% 
  full_join(current_hrh) 

calculateNeed <- function(hrh_data){
  hrh_need <- hrh_data %>%
    mutate(annual_workload=cop_target*tot_mins,
           need=annual_workload/adjusted_ftes_mins)  
}

tableRoutput <- function(hrh_data){
  hrh_data <- calculateNeed(hrh_data)
  
  hrh_data <- hrh_data %>%
    select(`Operating Unit`,`COP Planning Year`,`Target Scenario`,current_hrh,PSNU,program_area,cadre,current_hrh,need) %>% 
    group_by(program_area,cadre) %>% 
    mutate(Current=sum(current_hrh),
           Need=sum(need),
           Gap=(need - Current)) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(-PSNU,-current_hrh, -need) %>% 
    gather(measure, value, Current,Need,Gap)
  
  total_hrh_data <- hrh_data %>%    
    group_by(cadre,measure) %>% 
    mutate(value=sum(value),
           program_area='Total') %>%
    ungroup()
  
  prog_area_gap <- hrh_data %>%
    filter(measure=='Gap') %>% 
    group_by(program_area) %>% 
    mutate(value=sum(value)) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(-cadre)
  
  prog_area_need_gap <- hrh_data %>%
    filter(measure=='Gap' | measure=='Need') %>% 
    group_by(program_area,measure) %>% 
    mutate(value=sum(value)) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(-cadre) %>% 
    spread(measure,value) %>% 
    mutate(value=ifelse(Need>0, Gap*100/Need, 0),
           measure="Gap %") %>% 
    select(-Gap,-Need)
  
  tot_prog_area_need_gap <- hrh_data %>%
    filter(measure=='Gap' | measure=='Need') %>% 
    group_by(measure) %>%
    mutate(value=sum(value),
           program_area='Total') %>% 
    slice(1) %>% 
    select(-cadre) %>% 
    spread(measure,value) %>% 
    mutate(value=ifelse(Need>0, Gap*100/Need, 0),
           measure="Gap %") %>% 
    select(-Gap,-Need)
  
  tot_prog_area_gap <- prog_area_gap %>% 
    mutate(value=sum(value),
           program_area='Total') %>% 
    slice(1)
  
  hrh_data <- full_join(total_hrh_data,hrh_data) %>% 
    filter(measure!='Gap') %>% 
    full_join(prog_area_gap) %>% 
    full_join(tot_prog_area_gap) %>% 
    full_join(prog_area_need_gap) %>% 
    full_join(tot_prog_area_need_gap)
}

# table_r <- tableRoutput(hrh)

table_r <- ddply(scenarios, .(target_level), function(x){
  this_scenario <- full_join(hrh,x) %>% 
    mutate(cop_target=cop_target * target_multiplier)
  
  this_scenario <- tableRoutput(this_scenario)
  
  this_scenario$PrEP_Target = strsplit(x$target_level, '&')[[1]][1]
  this_scenario$HTS_Target = strsplit(x$target_level, '&')[[1]][2]
  this_scenario$TX_Target = strsplit(x$target_level, '&')[[1]][3]
  
  this_scenario$measure[this_scenario$measure=='Need'] <- 'Total Estimated Need'
  this_scenario$measure[this_scenario$measure=='Current'] <- 'Current Staffing'
  
  this_scenario <- this_scenario %>% 
    mutate(`Program Area`=program_area,
           CurrentAndNeed=measure,
           Cadre=cadre,
           Value=value) %>% 
    arrange(program_area,measure,cadre) %>% 
    select(`Operating Unit`,`COP Planning Year`,`Target Scenario`, PrEP_Target,	HTS_Target,	TX_Target, `Program Area`,CurrentAndNeed,Cadre,Value)
})

table_r <- table_r %>% select(-target_level)

table_g <- table_r %>% 
  filter(grepl('Gap', CurrentAndNeed)) %>% 
  spread(CurrentAndNeed,Value) %>% 
  select(-Cadre)

table_r <- table_r %>%
  filter(!grepl('Gap', CurrentAndNeed))


# Prioritization ranking
# --------------------------------------






# write output files
# ==================================================================================================
output_prefix <- paste('Output Files/',table_r$`Operating Unit`[1], '_', table_r$`COP Planning Year`[1], '_', 
                       table_r$`Target Scenario`[1], sep='')

# archived files from the active output folder 
out_files <- c('c','l','r','g')
file_exists <- sapply(out_files, function(x) file.exists(paste('Output Tables/table_', x, '.csv', sep='')))
if (TRUE %in% file_exists) {
  arch_dir <- paste('Archive/', gsub(':', '-', Sys.time()), sep='')
  dir.create(arch_dir)
  
  for (out_file in out_files[file_exists]) {
    arch_file <- paste('Output Tables/table_', out_file, '.csv', sep='')
    file.copy(arch_file, paste(arch_dir, '/table_', out_file, '.csv', sep=''))
  }
}
Sys.sleep(6)

# Dashboard one data
# --------------------------------------
# L-Current staff & costs
table_l <- current_hrh %>% 
  group_by(PSNU,cadre) %>% 
  mutate(current_hrh=sum(current_hrh),
         program_area='Total') %>% 
  slice(1) %>% 
  ungroup() %>% 
  full_join(current_hrh) %>% 
  full_join(psnu_list) %>% 
  mutate(`Cost (USD)`=current_hrh*current_salaries) %>% 
  rename(Cadre=cadre,
         `Program Area`=program_area,
         `Current staff (FTEs)`=current_hrh) %>% 
  select(`Operating Unit`,`COP Planning Year`,`Target Scenario`,`PSNU`,`Program Area`,`Cadre`,`Current staff (FTEs)`,`Cost (USD)`)

write.csv(table_l, paste(output_prefix, '_Dashboard1_CurrentStaff&Costs_', gsub(':', '-', Sys.time()), '.csv', sep=''), row.names = F)

# C-Current staff and targets
current_hrh_out <- current_hrh %>% 
  select(PSNU,cadre,current_hrh,program_area) %>% 
  group_by(PSNU,program_area) %>% 
  select(-cadre) %>% 
  mutate(current_hrh=sum(current_hrh)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(program_area=paste(program_area, ': TOTAL (current staff)', sep='')) %>%
  spread(program_area,current_hrh) 

table_c <- program_targets %>% 
  mutate(HTS_TST_Total=rowSums(program_targets[names(program_targets)[grepl('HTS_TST', names(program_targets))]], na.rm =T)) 

names(table_c) <- gsub('_Total', ' (target)', names(table_c))

table_c <- table_c %>% 
  full_join(psnu_list) %>% 
  full_join(current_hrh_out) %>% 
  select(`Operating Unit`,`COP Planning Year`,`Target Scenario`,PSNU,`PrEP: TOTAL (current staff)`,`HTS: TOTAL (current staff)`,`TX: TOTAL (current staff)`,
         `PrEP_NEW (target)`,`PrEP_CURR (target)`,`HTS_SELF (target)`,`HTS_TST (target)`,`TX_NEW (target)`,
         `TX_CURR (target)`,`TX_PVLS (target)`)

write.csv(table_c, paste(output_prefix, '_Dashboard2_CurrentStaff&Targets_', gsub(':', '-', Sys.time()), '.csv', sep=''), row.names = F)

 # Current staff, need & gap
write.csv(table_r, paste(output_prefix, '_Dashboard3_Need_', gsub(':', '-', Sys.time()), '.csv', sep=''), row.names = F)

# G-gap
write.csv(table_g, paste(output_prefix, '_Dashboard3_Gap_', gsub(':', '-', Sys.time()), '.csv', sep=''), row.names = F)

# output for PRI calculations
scenarios_mini <- data.frame(PrEP_Target=unique(PrEP_Target), HTS_Target=unique(HTS_Target),	TX_Target=unique(TX_Target), stringsAsFactors = F)
scenarios_mini <- scenarios_mini %>% 
  gather(program_area,target_level,PrEP_Target:TX_Target) %>% 
  mutate(target_multiplier=ifelse(target_level=='COP', 1, ifelse(target_level=='[-]15%', 0.85, 1.15)),
         program_area=gsub('_Target', '', program_area))

hrh_out_pri <- ddply(scenarios_mini, .(target_level), function(x){
  this_scenario <- full_join(hrh,x) %>% 
    mutate(cop_target=cop_target * target_multiplier)
  
  this_scenario <- calculateNeed(this_scenario) 
})

hrh_out_pri <- hrh_out_pri %>% 
  group_by(PSNU,program_area,current_hrh,cadre,`Target Scenario`,current_salaries,target_level) %>% 
  summarise(
  # current_hrh=sum(current_hrh),
            need=sum(need)) 

saveRDS(hrh_out_pri, file='Output Files/hrh_out_pri.RData')
# xx <- readRDS('Output Files/hrh_out_pri.RData') 


# PRI List


# PRI Ranking

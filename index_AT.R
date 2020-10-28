## Data Transformation Script for HRH Needs and Optimization Solution
   # Extract data from Excel based Data Collection Template
   # Transform the data appropriately and perform calculations
   # Output transformed data into prescribed csv format
## Date modified: 28 October 2020

# Load packages ----------------------------------------------------------------------
library(readxl)
library(plyr)
library(dplyr)
library(tidyr)

# Set Global Variables ---------------------------------------------------------------------- 
BUDGET_SCENARIOS <- c(.8, .9, .95, 1.05, 1.1, 1.2) # factor by which to multiply current budget

# Import Customization parameters ----------------------------------------------------------------------
customPar <- read_excel(dct, sheet = "5. Customization Parameters", skip = 4, col_names = F)
names(customPar) <- c('param','question','response')
customPar$qn <- as.numeric(customPar$response)                                                    # % response
customPar$resp <- ifelse(customPar$response %in% c('Yes','No'), customPar$response=='Yes', NA)    # yes/no response

# Import Program targets ----------------------------------------------------------------------
program_targets <- read_excel(dct, sheet = "2. Program Targets ", skip = 3)

# replace problematic spaces and brackets in variable names with underscores
names(program_targets) <- gsub("\\s\\(", '_', gsub("\\)", '', names(program_targets)))
names(program_targets) <- gsub("\\s+", '_', names(program_targets))
names(program_targets) <- gsub("__", '_', names(program_targets))

program_targets <- as.data.frame(program_targets) %>%
  filter(!is.na(PSNU),
         PSNU!="0") %>% # filter out entirely empty rows that come with the excel sheet
  select(-DATIM_UID,-Ref_ID)

names(program_targets)[grepl('PMTCT_ART_New', names(program_targets))] <- 'PMTCT_ART_New'
names(program_targets)[grepl('PMTCT_ART_Already', names(program_targets))] <- 'PMTCT_ART_Already'

# Calculate client time ----------------------------------------------------------------------
program_targets_time <- program_targets %>% 
  gather(target, cop_target,-PSNU)

# 14. In the planning COP year, how many clients would you expect to need to screen to reach the PrEP_NEW target?
tot_prep_new_target <- sum(program_targets$PrEP_NEW_Total)
expect_screening_pctg <- ifelse(tot_prep_new_target==0, 0, 
                                ifelse(customPar$qn[14]<tot_prep_new_target, 1, customPar$qn[14]/tot_prep_new_target))

clientTime <- function(){
  TOT_MINS <- list()
  ########### PREP
  # [1] PREP_NEW Total population
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
    'Case Manager', tot_visits_fac*(expect_screening_pctg*15 + ifelse(customPar$resp[1], 10*1, 0)) +
                    tot_visits_com*(expect_screening_pctg*15 + ifelse(customPar$resp[1], 10*1, 0)),
    'Clinical-Medical', tot_visits_fac*20*1 + 0,
    'Clinical-Nursing', 0 + tot_visits_com*25*1,
    'Data Clerk', tot_visits_fac*(10*1 + 10*1) + tot_visits_com*(10*1 + 10*1), 
    'Laboratory', tot_visits_fac*20*1 + tot_visits_com*20*1,
    'Lay-CHW', 0 + 0,
    'Lay-Counselor', tot_visits_fac*expect_screening_pctg*20 + tot_visits_com*expect_screening_pctg*20,
    'Pharmacy', tot_visits_fac*15*1 + 0)
  
  TOT_MINS[[1]] <- data.frame(pathway='PrEP_NEW', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F) 
  
  # [2] PrEP_CURR Total population 
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
  
  TOT_MINS[[2]] <- data.frame(pathway='PrEP_CURR', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F)
  
  ########### HTS
  # [3] HTS_SELF (Total)
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
  
  TOT_MINS[[3]] <- data.frame(pathway='HTS_SELF', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F)
  
  # [4] HTS_TST_Mobile
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
  
  TOT_MINS[[4]] <- data.frame(pathway='HTS_TST_Mobile', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F)
  
  # Total no. of minutes
  htsTime <- function(pathway) {
    cm_deliver_test_time <- ifelse(pathway %in% c('HTS_TST_PMTCT_ANC1','HTS_TST_PMTCT_Post_ANC1','HTS_TST_Facility_Index'), 10, 0)
    
    tot_mins <- list( 
      # 3.  Can Case Managers provide testing? 
      'Case Manager', cm_deliver_test_time + ifelse(customPar$resp[3], 30*1, 0),
      'Clinical-Medical', 0,
      'Clinical-Nursing', ifelse(customPar$resp[3], 0, 30*0.9),
      # 'Clinical-Nursing', 30*0.9,
      'Data Clerk', 10*1, 
      # 'Laboratory', 0 + 30*0.1,
      'Laboratory', ifelse(customPar$resp[3], 0, 30*0.1),
      'Lay-CHW', 0, 
      'Lay-Counselor', 0,
      'Pharmacy', 0)
    
    tot_mins <- data.frame(pathway=pathway, matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F)
  }
  
  TOT_MINS[[5]] <- htsTime('HTS_TST_PMTCT_ANC1')          # [5] HTS_TST_PMTCT_ANC1
  TOT_MINS[[6]] <- htsTime('HTS_TST_PMTCT_Post_ANC1')     # [6] HTS_TST_PMTCT_Post_ANC1
  TOT_MINS[[7]] <- htsTime('HTS_TST_Facility_Index')      # [7] HTS_TST_Facility_Index
  
  TOT_MINS[[8]] <- htsTime('HTS_TST_STI')                 # [8] HTS_TST_STI 
  TOT_MINS[[9]] <- htsTime('HTS_TST_Other_PITC')          # [9] HTS_TST_Other_PITC
  TOT_MINS[[10]] <- htsTime('HTS_TST_Inpatient')          # [10] HTS_TST_Inpatient
  
  # [11] HTS_TST_IndexMod
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
  
  TOT_MINS[[11]] <- data.frame(pathway='HTS_TST_IndexMod', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F)
  
  ########### TX 
  # [12] TX_NEW General Patients 
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
  
  TOT_MINS[[12]] <- data.frame(pathway='TX_NEW', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F) 
  
  # [13] TX_NEW_KP 
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
  
  TOT_MINS[[13]] <- data.frame(pathway='TX_NEW_KP', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F) 
  
  # [14] PMTCT_ART_New_on_life-long_ART 
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
  
  TOT_MINS[[14]] <- data.frame(pathway='PMTCT_ART_New', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F)
  
  # [15] TX_CURR General Patients: Drug Dispensing Frequency & Clinical Consultations
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
    # qn[32] 29. What percentage of TX_CURR are traced to their home due to missed appointments?
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
  
  TOT_MINS[[15]] <- data.frame(pathway='TX_CURR', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F) 
  
  # [16] PMTCT_ART_Already_on_life-long_ART_at_the_beginning_of_current_pregnancy 
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
  
  TOT_MINS[[16]] <- data.frame(pathway='PMTCT_ART_Already', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F)
  
  # [17] TX_CURR Key Population: Drug Dispensing Frequency & Clinical Consultations
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
    # qn[32] 29. What percentage of TX_CURR are traced to their home due to missed appointments?
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
  
  TOT_MINS[[17]] <- data.frame(pathway='TX_CURR_KP', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F) 
  
  # [18] TX_PVLS_Total 
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
  
  TOT_MINS[[18]] <- data.frame(pathway='TX_PVLS', matrix(unlist(tot_mins), ncol=2,byrow=T), stringsAsFactors = F) 
  
  TOT_MINS <- do.call(rbind, TOT_MINS)
  names(TOT_MINS) <- c('pathway','cadre','tot_mins')
  return(TOT_MINS)
}

client_time <- clientTime()
client_time <- client_time %>% 
  mutate(target=gsub(".-\\s[A-Z]+$", '', pathway)) %>%  
  group_by(target,cadre) %>% 
  summarise(tot_mins=sum(as.numeric(tot_mins))) %>% 
    ungroup()

# Set up Available working time parameters ----------------------------------------------------------------------
awt <- data.frame(cadre=c('Clinical-Nursing','Clinical-Medical','Data Clerk','Case Manager','Lay-Counselor',
                          'Lay-CHW','Laboratory','Pharmacy'), stringsAsFactors = F)

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

# Import Data Collection Home ----------------------------------------------------------------------
dct_home <- read_excel(dct, sheet = "Data Collection Home", na = "0", skip = 3)
dct_home <- as.data.frame(dct_home)
completion_date <- as.Date(as.numeric(dct_home[8,2]), origin = "1899-12-30")
ou <- dct_home[2,2]
cop_planning_year <- dct_home[3,2]
fy_target_scenario <- dct_home[4,2]

dct_home <- data.frame(ou=ou) %>%
  mutate(`Operating Unit`=ou,
         `COP Planning Year`=cop_planning_year,
         `Target Scenario`=fy_target_scenario,
         `Completion Date`=completion_date,
         `Run Date`=regmatches(Sys.time(), regexpr("[0-9-]+", Sys.time()))) %>% 
  select(-ou)

# Import PSNU list ----------------------------------------------------------------------
psnu_list <- read_excel(dct, sheet = "1. PSNU List", skip = 2)
psnu_list <- psnu_list %>% 
  # rename(`Target Scenario`=`FY Target Scenario`) %>% 
  # select(-`Record ID`) %>% 
  select(`DATIM UID`,PSNU) %>% 
  filter(!is.na(PSNU))

# Import current salaries ----------------------------------------------------------------------
current_salaries <- read_excel(dct, sheet = "4. Avg. Annual HRH Remuneration", range = "A5:B13")
names(current_salaries) <- c('cadre','current_salaries')

# Import current HRH ----------------------------------------------------------------------
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

# Calculate need and gap ----------------------------------------------------------------------
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
    # select(`Operating Unit`,`COP Planning Year`,`Target Scenario`,current_hrh,PSNU,program_area,target,cadre,current_hrh,need) %>% 
    select(current_hrh,PSNU,program_area,target,cadre,current_hrh,need) %>% 
    group_by(program_area,PSNU,cadre) %>% 
    mutate(Need=sum(need)) %>%   #sum up the need for indicators within the same (program_area,PSNU,cadre)
    slice(1) %>% 
    ungroup() %>% 
    group_by(program_area,cadre) %>% 
    mutate(Current=sum(current_hrh),   #sum up the current_hrh for PSNUs within the same (program_area,cadre)
           Need=sum(Need),             #sum up the Need for PSNUs within the same (program_area,cadre)
           Gap=(Need - Current)) %>%  
    slice(1) %>% 
    ungroup() %>% 
    select(-PSNU,-current_hrh, -need,-target) %>% 
    gather(measure, value, Current,Need,Gap)
  
  # aggregate Gap, Need and Current hrh for all program areas by cadre
  total_hrh_data <- hrh_data %>%    
    group_by(cadre,measure) %>% 
    mutate(value=sum(value),    #sum up the Need,current_hrh,Gap for program_areas within the same cadre
           program_area='Total') %>%
    slice(1) %>% 
    ungroup()
  
  # aggregate Gap, Need and Current hrh for all cadre by program area
  prog_area_need_gap <- hrh_data %>%
    group_by(program_area,measure) %>% 
    mutate(value=sum(value),
           cadre='Total') %>%        
    slice(1) %>% 
    ungroup() %>% 
    spread(measure,value) %>% 
    mutate(`Gap %`=ifelse(Need>0, Gap*100/Need, 0)) %>% 
    gather(measure,value,Current:`Gap %`)
  
  # aggregate Gap, Need and Current hrh for all cadres and program areas
  tot_values <- prog_area_need_gap %>% 
    group_by(measure) %>% 
    mutate(value=sum(value),
           program_area='Total') %>%        
    slice(1) %>% 
    ungroup()
  
  tot_values$value[tot_values$measure=='Gap %'] <- tot_values$value[tot_values$measure=='Gap']*100/tot_values$value[tot_values$measure=='Need']
  
  hrh_data <- full_join(total_hrh_data,hrh_data) %>% 
    filter(measure!='Gap') %>% 
    full_join(prog_area_need_gap) %>%
    full_join(tot_values)
}

table_r <- ddply(scenarios, .(target_level), function(x){
  this_scenario <- full_join(hrh,x) %>% 
    mutate(cop_target=cop_target * target_multiplier)
  
  this_scenario <- tableRoutput(this_scenario)
  
  this_scenario$PrEP_Target = strsplit(x$target_level, '&')[[1]][1]
  this_scenario$HTS_Target = strsplit(x$target_level, '&')[[1]][2]
  this_scenario$TX_Target = strsplit(x$target_level, '&')[[1]][3]
  
  this_scenario$measure[this_scenario$measure=='Need'] <- 'Total Estimated Need'
  this_scenario$measure[this_scenario$measure=='Current'] <- 'Current Staffing'
  # 
  # this_scenario <- this_scenario %>% 
  #   mutate(`Program Area`=program_area,
  #          CurrentAndNeed=measure,
  #          Cadre=cadre,
  #          Value=value) %>% 
  #   arrange(program_area,measure,cadre) %>% 
  #   select(`Operating Unit`,`COP Planning Year`,`Target Scenario`, PrEP_Target,	HTS_Target,	TX_Target, `Program Area`,CurrentAndNeed,Cadre,Value,
  #          `Completion Date`,`Run Date`)  
  this_scenario <- this_scenario %>% 
    mutate(`Program Area`=program_area,
           CurrentAndNeed=measure,
           Cadre=cadre,
           Value=value) %>% 
    arrange(program_area,measure,cadre) %>% 
    select(PrEP_Target,	HTS_Target,	TX_Target, `Program Area`,CurrentAndNeed,Cadre,Value)
})

table_r <- table_r %>% select(-target_level)

table_g <- table_r %>%
  filter(grepl('Gap', CurrentAndNeed)) %>%
  spread(CurrentAndNeed,Value) %>%
  select(-Cadre)

table_r <- table_r %>%
filter(!grepl('Gap', CurrentAndNeed)) %>% 
  full_join(table_g)

table_r <- cbind(dct_home,table_r)

# Set the output folder ----------------------------------------------------------------------
# create folder with name based on: OU, COP planning year and Target scenario
output_dir <- paste(table_r$`Operating Unit`[1], '_', table_r$`COP Planning Year`[1], '_', table_r$`Target Scenario`[1], sep='')
out_folder <- paste('Dataout/', output_dir, sep='')

# archived files from the active output folder 
if (file.exists(out_folder)) {
  arch_dir <- paste('Dataout/Archive/', output_dir, '_', gsub(':', '-', Sys.time()), sep='')
  dir.create(arch_dir)
  
  out_files <- list.files(out_folder)
  for (out_file in out_files) {
    arch_file <- paste(out_folder, '/', out_file, sep='')
    file.copy(arch_file, paste(arch_dir, '/', out_file, '.csv', sep=''))
  }
}else{
  dir.create(out_folder)
}
Sys.sleep(6)

# Output Dashboard1_CurrentStaff&Costs ----------------------------------------------------------------------
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
  select(PSNU,`DATIM UID`,`Program Area`,`Cadre`,`Current staff (FTEs)`,`Cost (USD)`)  
  # select(`Operating Unit`,`COP Planning Year`,`Target Scenario`,`PSNU`,`Program Area`,`Cadre`,`Current staff (FTEs)`,`Cost (USD)`)
table_l <- cbind(dct_home,table_l)

write.csv(table_l, paste(out_folder, '/Dashboard1_CurrentStaff&Costs.csv', sep=''), row.names = F)

# Output Dashboard2_CurrentStaff&Targets ----------------------------------------------------------------------
current_hrh_out <- current_hrh %>% 
  select(PSNU,cadre,current_hrh,program_area) %>% 
  group_by(PSNU,program_area) %>% 
  select(-cadre) %>% 
  mutate(current_hrh=sum(current_hrh)) %>% 
  slice(1) %>% 
  ungroup() #%>% 
  # mutate(program_area=paste(program_area, ': TOTAL (current staff)', sep='')) %>%
  # spread(program_area,current_hrh) 

table_c <- program_targets %>% 
  mutate(HTS_TST_Total=rowSums(program_targets[names(program_targets)[grepl('HTS_TST', names(program_targets))]], na.rm =T),
         PMTCT_ART_Total=PMTCT_ART_New + PMTCT_ART_Already) %>% 
  gather(`Target Type`,Target,-PSNU) %>% 
  filter(grepl('_Total', `Target Type`)) %>% 
  mutate(`Target Type`=gsub('_Total', '', `Target Type`)) %>%
# names(table_c) <- gsub('_Total', ' (target)', names(table_c))
# table_c <- table_c %>% 
  full_join(psnu_list) %>% 
  mutate(program_area=ifelse(grepl('PMTCT', `Target Type`), 'TX', gsub("_\\w+$", '', `Target Type`))) %>% 
  full_join(current_hrh_out) %>% 
  mutate(`Current Staff`=current_hrh,
         `Program Area`=program_area) %>% 
  select(PSNU,`DATIM UID`,`DATIM UID`,`Program Area`,`Target Type`,Target,`Current Staff`)
  # select(`Operating Unit`,`COP Planning Year`,`Target Scenario`,PSNU,`DATIM UID`,`Program Area`,`Target Type`,Target,`Current Staff`)
  # select(`Operating Unit`,`COP Planning Year`,`Target Scenario`,PSNU,`PrEP: TOTAL (current staff)`,`HTS: TOTAL (current staff)`,`TX: TOTAL (current staff)`,
  #        `PrEP_NEW (target)`,`PrEP_CURR (target)`,`HTS_SELF (target)`,`HTS_TST (target)`,`TX_NEW (target)`,
  #        `TX_CURR (target)`,`TX_PVLS (target)`)

table_c <- cbind(dct_home,table_c)

write.csv(table_c, paste(out_folder, '/Dashboard2_CurrentStaff&Targets.csv', sep=''), row.names = F)

# Output Dashboard3_StaffingNeed ----------------------------------------------------------------------
# write.csv(table_r, paste(out_folder, '_Dashboard3_StaffingNeed_', gsub(':', '-', Sys.time()), '.csv', sep=''), row.names = F)
write.csv(table_r, paste(out_folder, '/Dashboard3_StaffingNeed.csv', sep=''), row.names = F)

# output for PRI calculations
scenarios_mini <- data.frame(PrEP_Target=unique(PrEP_Target), HTS_Target=unique(HTS_Target),	TX_Target=unique(TX_Target), stringsAsFactors = F)
scenarios_mini <- scenarios_mini %>% 
  gather(program_area,target_level,PrEP_Target:TX_Target) %>% 
  mutate(target_multiplier=ifelse(target_level=='COP', 1, ifelse(target_level=='[-]15%', 0.85, 1.15)),
         program_area=gsub('_Target', '', program_area))

hrh_inputs <- ddply(scenarios_mini, .(target_level), function(x){
  this_scenario <- full_join(hrh,x) %>% 
    mutate(cop_target=cop_target * target_multiplier)
  
  this_scenario <- calculateNeed(this_scenario) 
})

hrh_inputs <- hrh_inputs %>% 
  mutate(`Target Scenario`=fy_target_scenario) %>% 
  group_by(PSNU,program_area,current_hrh,cadre,`Target Scenario`,current_salaries,target_level) %>% 
  summarise(
  # current_hrh=sum(current_hrh),
            need=sum(need)) %>% 
  ungroup()

# saveRDS(hrh_inputs, file='Output Files/hrh_out_pri.RData')
# hrh_inputs <- readRDS('Output Files/hrh_out_pri.RData') 


# PRI calculations -------------------------------------------------------------------
# Load Derived Inputs
# Number of rows = # PSNU (varies) * # Cadre (8) * # Program Area (3) * # Target Level (3)
# Columns = PSNU | Program_Area | Cadre | Target_Level | Need
# hrh_inputs <- readRDS('./hrh_out_pri.RData')
names(hrh_inputs) <- tolower(names(hrh_inputs))

# Get unique values of dimensions of interest
CADRES <- unique(hrh_inputs$cadre)
PROGRAM_AREAS <- unique(hrh_inputs$program_area)
PSNU <- unique(hrh_inputs$psnu)
TARGET_LEVELS <- unique(hrh_inputs$target_level)

## Salaries by Cadre
# Rows = # cadre
# Columns = Cadre | Salary

salaries <- unique(hrh_inputs[, c("cadre", "current_salaries")])

## Calculate Budget
# Budget is by Program Area - it is the sum product of staff by cadre by salary

# Get total budget by program area
budget <- hrh_inputs %>%
  # filter for any target level so that we dont triple count the budget
  filter(target_level == TARGET_LEVELS[1]) %>%
  mutate(cost = current_hrh * current_salaries) %>%
  group_by(program_area) %>%
  summarize(pa_budget = sum(cost), .groups = "drop")

# Calculate Optimal PRI Scores --------------------------------------------------------

# Set up list to hold outputs
optimal_scores_out <- data.frame()

## For each PSNU | Program Area | Cadre | Target Level
for (i in 1:length(PSNU)){
  for (j in 1:length(PROGRAM_AREAS)){
    for (k in 1:length(CADRES)){
      for (l in 1:length(TARGET_LEVELS)){
        
        # Get subset from HRH_Inputs
        dat <- hrh_inputs %>% filter(psnu == PSNU[i] & program_area == PROGRAM_AREAS[j] &
                                       cadre == CADRES[k] & target_level == TARGET_LEVELS[l])
        if(floor(dat$need) == 0) {next}
        
        # Get need
        need <- dat$need
        # Get supply
        supply <- 0
        # Get gap
        gap <- need - supply
        # Get rounded down gap (this will be maximum number of new hires, and number of new hires for which we need PRI scores)
        gap_rounded <- floor(gap)
        
        # Set up dataframe to hold results (number of rows = gap_rounded)
        pri_scores <- expand.grid(psnu = PSNU[i], program_area = PROGRAM_AREAS[j],
                                  cadre = CADRES[k], target_level = TARGET_LEVELS[l], hire = 1:gap_rounded, pri_score = 0)
        
        # For each new employee (number of gap_rounded), calculate PRI Score
        for (m in 1:gap_rounded){
          
          # Get number of new hires thus far added to supply, subtract one as first time through, no new hires added
          new <- m - 1
          # Add new hires to supply to get updated supply
          updated_supply <- supply + new
          # If supply is zero, then for first new hire, updated_supply_denominator will be zero. Set to 1 to avoid dividing by zero.
          updated_supply_denominator <- ifelse(updated_supply == 0, 1, updated_supply)
          # calculate PRI score - if need is 0, set pri score to zero
          if (floor(need) == 0){
            pri_scores$pri_score[m] <- 0
          } else {
            pri_scores$pri_score[m] <- 100 * (need - updated_supply) / (updated_supply_denominator)
          }
          
        }
        
        # Add scores to list
        optimal_scores_out <- rbind(optimal_scores_out, pri_scores)
        
      }
    }
  }
}

# Allocate Hires Optimally -------------------------------------------------------------

calcAllocation <- function(budget_data, pri_scores, salaries_data){
  
  # Set up dataframe to hold final lists for each program area / target level
  optimal_out <- data.frame()
  
  # Loop through program areas and target levels
  for (i in PROGRAM_AREAS){
    for(j in TARGET_LEVELS){
      
      # Get budget for program area
      budget_temp <- budget_data[budget_data$program_area == i, "pa_budget"]
      
      # Filter pri score list for particular Program Area and Target Level
      dat <- pri_scores %>% filter(program_area == i & target_level == j)
      
      # Merge salary data
      dat <- merge(dat, salaries_data, by = "cadre")
      
      # Sort data in descending order by Pri_Score
      dat <- dat %>% arrange(desc(pri_score))
      
      # Calculate cumulative sum of salary
      dat <- dat %>%
        mutate(cumulative_salary = cumsum(current_salaries))
      
      # Cut off list where Cumulative_Salary > Budget
      dat <- dat %>%
        filter(cumulative_salary < as.numeric(budget_temp))
      
      optimal_out <- rbind(optimal_out, dat)
      
    }
  }
  
  return(optimal_out)
}

optimal_allocation <- calcAllocation(budget_data = budget, pri_scores = optimal_scores_out, salaries_data = salaries)

# Summarize Optimal Allocation by PSNU, Program Area, Target Level, Cadre
optimal_summary <- optimal_allocation %>%
  group_by(psnu, program_area, target_level, cadre) %>%
  summarize(optimal = n(), .groups = "drop")

# Calculate Attribution of Current Staff----------------------------------------------------------------------

# Merge optimal allocation and current allocation and Need
optimal_current_need <- merge(hrh_inputs, optimal_summary, by = c("psnu", "program_area", "target_level", "cadre"), all.x = T)

# Some combinations of PSNU | Program Area | Target Level | Cadre have no FTE in optimal allocation, so come through as NA
# Set these to zero
optimal_current_need <- optimal_current_need %>%
  mutate(optimal = ifelse(is.na(optimal), 0, optimal))

calcAttribution <- function(data, current = TRUE){
  
  # One percent attribution score for each program area and target level
  attribution_out <- data.frame()
  
  for (i in PROGRAM_AREAS){
    for(j in TARGET_LEVELS){
      
      # Filter to program area
      dat <- data %>% filter(program_area == i & target_level == j)
      # Get sum of optimal staff and sum of need for later calculations
      sum_optimal_pa <- sum(dat$optimal)
      sum_need_pa <- sum(dat$need)
      
      # Set up list to hold deviations for later average deviation calculation
      deviations <- c()
      
      # loop through PSNUs
      for (k in PSNU){
        
        # Loop through Cadres
        for (l in CADRES){
          
          # Get data for psnu / cadre
          dat_cadre <- dat %>% filter(psnu == k & cadre == l) 
          # Get optimal hires (Column C in Excel)
          sum_optimal <- sum(dat_cadre$optimal)
          # Get current or redistributed staff (Column D in Excel)
          if(current == TRUE){
            sum_staff <- sum(dat_cadre$current_hrh)
          } else {
            sum_staff <- sum(dat_cadre$staff_redistributed)
          }
          # Calculate deviation (column G in Excel)
          deviation <- min(abs((sum_staff - sum_optimal) / sum_optimal), 1)
          # Calculate weighted average by cadre (column H in Excel)
          weighted_deviation <- deviation * (sum_optimal / sum_optimal_pa)
          # If optimal allocation is zero, this will NaN - set to zero
          if(is.nan(weighted_deviation)){weighted_deviation <- 0}
          # Append deviation
          deviations <- c(deviations, weighted_deviation)
        }
      }
      
      # calculate average deviation (Column I in Excel)
      average_deviation <- sum(deviations)
      
      # Calculate optimal attribution (sum of optimal positions / sum of needed positions) by program area (Column J in Excel)
      # if need is zero, this will be nan - in that case, set to zero
      optimal_attribution <- sum_optimal_pa / sum_need_pa
      if(is.nan(optimal_attribution)){optimal_attribution <- 0}
      
      # calculate current attribution (Column K in Excel)
      attribution <- optimal_attribution * (1-average_deviation)
      
      # Collect fields for output
      att_df <- data.frame(program_area = i, target_level = j, sum_optimal_pa, sum_need_pa,
                           average_deviation, optimal_attribution, attribution)
      
      if(current == TRUE) {
        att_df <- att_df %>% rename('current_attribution' = attribution) 
      } else {
        att_df <- att_df %>% rename('red_attribution' = attribution)
      }
      
      attribution_out <- rbind(attribution_out, att_df)
    }
  }
  
  return(attribution_out)
  
}

attribution_out <- calcAttribution(data = optimal_current_need, current = TRUE)

calcTotalAttribution <- function(attribution_data, current = TRUE){
  
  # Split output by program area
  attributions_split <- split(attribution_data, attribution_out$program_area)
  # Rename variables to tag by program area and drop program area variable
  attributions_renamed <- lapply(attributions_split, function(x){
    names(x) <- paste0(x$program_area[1], "_", names(x))
    x[, -1]
  })
  # Join together so we have all combinations of program area / target level (if 3 of each, then 3^2 = 27 rows)
  attribution_wide <- attributions_renamed[[1]] %>%
    merge(., attributions_renamed[[2]]) %>%
    merge(., attributions_renamed[[3]])
  
  # Calculate total attributions
  attribution_totals <- attribution_wide %>%
    mutate(total_average_deviation = (HTS_average_deviation * HTS_sum_optimal_pa +
                                        TX_average_deviation * TX_sum_optimal_pa +
                                        PrEP_average_deviation * PrEP_sum_optimal_pa) / 
             (HTS_sum_optimal_pa + TX_sum_optimal_pa + PrEP_sum_optimal_pa),
           total_optimal_attribution = (HTS_sum_optimal_pa + TX_sum_optimal_pa + PrEP_sum_optimal_pa) /
             (HTS_sum_need_pa + TX_sum_need_pa + PrEP_sum_need_pa),
           total_attribution = total_optimal_attribution * (1 - total_average_deviation))
  
  if(current == TRUE){
    attribution_totals <- attribution_totals %>% rename('total_current_attribution' = total_attribution)
  } else {
    attribution_totals <- attribution_totals %>% rename('total_red_attribution' = total_attribution)
  }
  
  return(attribution_totals)
  
}

attribution_current_totals <- calcTotalAttribution(attribution_data = attribution_out)

# Select variables needed later on
attribution_current_clean <- attribution_current_totals %>%
  select(PrEP_target_level, HTS_target_level, TX_target_level, PrEP_current_attribution, HTS_current_attribution, TX_current_attribution,
         total_current_attribution, PrEP_optimal_attribution, HTS_optimal_attribution, TX_optimal_attribution, total_optimal_attribution)

# Redistribute Existing Staff -------------------------------------------------------------------------

# if no need for staff in a given program area / cadre, but there is existing staff, keep staff where they are

# Set up data frame to hold output
redistributed_out <- data.frame() 
unredistributed_out <- data.frame() # to hold staff not redistributed because supply > need for the cadre

# loop through program areas, target levels, and cadres
for (i in PROGRAM_AREAS){
  for (j in TARGET_LEVELS){
    for (k in CADRES){
      
      # get sum of current Cadre staff in Program Area
      current_staff <- hrh_inputs %>%
        ungroup() %>%
        # Filtering by target level also - otherwise we'd get triple the number of current staff
        filter(program_area == i & cadre == k & target_level == j) %>%
        group_by(program_area, cadre) %>%
        summarize(total_staff = sum(current_hrh), .groups = "drop")
      
      # Get PRI list for Program Area and Cadre and sort by PRI score, dropping any PRI scores of zero so these are never selected
      pri_sorted <- optimal_scores_out %>% 
        filter(pri_score > 0) %>%
        filter(program_area == i & target_level == j & cadre == k) %>%
        arrange(desc(pri_score))
      
      # Cut off PRI list at number of current staff
      # top_n() was being wonky so avoiding that function
      pri_selected <- pri_sorted %>%
        mutate(rownum = row_number()) %>%
        filter(rownum <= round(current_staff$total_staff)) %>%
        select(psnu, program_area, cadre, target_level)
      
      # if there are more current staff than are needed for the program area / cadre, then keep surplus staff that are 
      # not redistributed in their current PSNU
      # practically, if there are 10 staff but only 4 are needed, those 4 will populated via redistribution in the 
      # preceeding code. Six remaining current staff will be selected at random in their current PSNUs with the 
      # code below.
      if(nrow(pri_selected) < round(current_staff$total_staff)){
        
        unredistributed <- hrh_inputs %>%
          ungroup() %>%
          filter(program_area == i & cadre == k & target_level == j) %>%
          mutate(rownum = row_number()) %>%
          filter(rownum > nrow(pri_selected)) %>%
          select(psnu, program_area, cadre, target_level, current_hrh)
        
        unredistributed_out <- rbind(unredistributed_out, unredistributed)
        
      }
      
      redistributed_out <- rbind(redistributed_out, pri_selected)
      
    }
  }
}

# summarize unredistributed output
unredistributed_summary <- unredistributed_out %>%
  group_by(psnu, program_area, cadre, target_level) %>%
  summarize(staff_redistributed = sum(current_hrh), .groups = "drop") %>%
  filter(staff_redistributed > 0)

# summarize redistributed output
redistributed_only_summary <- redistributed_out %>%
  group_by(psnu, program_area, cadre, target_level) %>%
  summarize(staff_redistributed = n(), .groups = "drop")

# combine
redistributed_summary <- merge(redistributed_only_summary, unredistributed_summary,
                               by = c("psnu", "program_area", "cadre", "target_level"),
                               all = TRUE) %>%
  mutate(staff_redistributed.x = ifelse(is.na(staff_redistributed.x), 0, staff_redistributed.x),
         staff_redistributed.y = ifelse(is.na(staff_redistributed.y), 0, staff_redistributed.y)) %>%
  mutate(staff_redistributed = staff_redistributed.x + staff_redistributed.y) %>%
  select(psnu, program_area, cadre, target_level, staff_redistributed)

# Calculate Percent Attribution of Redistributed Staff ----------------------------------------------

# Merge optimal allocation and current allocation and Need
optimal_red_need <- merge(hrh_inputs, redistributed_summary, by = c("psnu", "program_area", "target_level", "cadre"), all.x = T) %>%
  merge(., optimal_summary, by = c("psnu", "program_area", "target_level", "cadre"), all.x = T)

# Set NAs to zero - some combinations have no staff in optimal or redistributed scenarios
optimal_red_need[is.na(optimal_red_need)] <- 0

# Calculate attributions
attribution_red_out <- calcAttribution(data = optimal_red_need, current = FALSE)

# Calculate total attributions
attribution_red_totals <- calcTotalAttribution(attribution_data = attribution_red_out, current = FALSE)

# Select variables
attribution_red_clean <- attribution_red_totals %>%
  select(PrEP_target_level, HTS_target_level, TX_target_level, PrEP_red_attribution, HTS_red_attribution, TX_red_attribution, total_red_attribution)

# Generate output for dashboard 4 Attribution ----------------------------------------

attribution_all <- merge(attribution_current_clean, attribution_red_clean, 
                         by = c("PrEP_target_level", "HTS_target_level", "TX_target_level"))
attribution_all <- cbind(dct_home,attribution_all)

write.csv(attribution_all, paste(out_folder, "./Dashboard4_Attribution.csv", sep=''), row.names = F)

# Generate output for dashboard 5 Redistribution Profiles ------------------------------------

# merge with current staff
red_default <- redistributed_summary %>% filter(target_level == "COP") %>% select(-target_level)
current_default <- hrh_inputs %>% filter(target_level == "COP")

# set up a shell to make sure we don't lose any staff that didn't show up in both tables
shell <- expand.grid(psnu = PSNU, program_area = PROGRAM_AREAS, cadre = CADRES)
current_vs_redistributed <- shell %>%
  merge(., red_default, by = c("psnu", "program_area", "cadre"), all.x = T) %>%
  merge(., current_default, by = c("psnu", "program_area", "cadre"), all.x = T)

# For diagnostics
# a <- current_vs_redistributed %>% filter(target_level == 'Low_15') %>% group_by(program_area, cadre) %>%
#   summarize(curr = sum(current_hrh), red = sum(staff_redistributed, na.rm = T))

## Format wide for dashboard output
# First, set NA staff_redistributed to zero
current_vs_redistributed[is.na(current_vs_redistributed)] <- 0

# drop unused columns
dashboard5 <- current_vs_redistributed %>%
  select(psnu, program_area, target_level, cadre, staff_redistributed, current_hrh)
dashboard5 <- cbind(dct_home,dashboard5)

# #save out in wide format
write.csv(dashboard5, paste(out_folder, "./Dashboard5_RedistributionProfiles.csv", sep=''), row.names = F)

# Generate output for dashboard 6 Optimal Allocation ---------------------------------------------------------

# Take cvr_wide, and append optimal allocations by cadre
# get optimal into a table 77 * 3 (psnu by program area) 231 rows, with 8 columns for each cadre
# only want optimal allocation for COP targets
optimal_cop <- optimal_summary %>% filter(target_level == 'COP')

# Merge and keep in long format
dashboard6 <- merge(current_vs_redistributed, optimal_cop,
                    by = c("psnu", "program_area", "target_level", "cadre"),
                    all.x = T)

dashboard6[is.na(dashboard6)] <- 0
dashboard6 <- cbind(dct_home,dashboard6)

write.csv(dashboard6, paste(out_folder, "./Dashboard6_OptimalAllocation.csv", sep=''), row.names = F)


# Adjusted Funding Scenarios ----------------------------------------------------------------------

# Let's try to join redistributed to optimal, then sort through budget and sort by redistributed and PRI score
# and select until budget is used
# Let's first limit optimal and redistributed to the COP target scenarios
optimal_cop <- optimal_scores_out %>% filter(target_level == "COP") %>% select(-target_level)
red_cop <- redistributed_summary %>% filter(target_level == "COP") %>% select(-target_level)

# Let's join the redistributed hires and PRI list
optimal_red <- merge(optimal_cop, red_cop,
                     by = c("psnu", "program_area", "cadre"),
                     all.x = T)

# If no redistributed staff, then appears as NA - set to zero
optimal_red[is.na(optimal_red)] <- 0

# Create indicator if staff is being redistributed
optimal_red_ind <- optimal_red %>%
  filter(hire > 0) %>%
  mutate(red_ind = ifelse(hire <= staff_redistributed, 1, 0))

# Now, let's loop through each program area and budget scenario
# Set up list to hold outputs
budget_hires_out <- data.frame()

for (i in PROGRAM_AREAS){
  for (j in BUDGET_SCENARIOS){
    
    # Get budget for program area
    budget_temp <- budget[budget$program_area == i, "pa_budget"]
    
    # If program area has no budget (ie no current staff), then skip
    if(budget_temp == 0) {next}
    
    # Multiply budget by budget scenario
    budget_temp <- budget_temp * j
    
    # Filter optimal_less_red for particular Program Area
    dat <- optimal_red_ind %>% filter(program_area == i)
    
    # Merge salary data onto Dat
    dat <- merge(dat, salaries, by = "cadre")
    
    # Sort first by red_ind, then in descending order by Pri_Score
    dat <- dat %>% arrange(desc(red_ind), desc(pri_score))
    
    # Calculate cumulative sum of salary
    dat <- dat %>%
      mutate(cumulative_salary = cumsum(current_salaries))
    
    # Cut off list where Cumulative_Salary > Budget
    dat <- dat %>%
      filter(cumulative_salary < budget_temp$pa_budget)
    
    # Add label for budget scenario
    dat$budget_scenario <- if(j > 1){
      paste0("+", (j*100)-100, "% funding")
    } else {
      paste0((j*100)-100, "% funding")
    }
    
    # Add budget amount
    dat$budget_amount <- budget_temp$pa_budget
    
    budget_hires_out <- rbind(budget_hires_out, dat)
  }
}

# Generate output for dashboard 7 Adjusted Funding -----------------------------------

# summarize output by budget scenario
budget_hires_summary <- budget_hires_out %>%
  group_by(psnu, program_area, budget_scenario, cadre) %>%
  summarize(staff = n(), .groups = "drop")

# get this in a wider format
bhs_wide <- pivot_wider(budget_hires_summary,
                        id_cols = c("psnu", "program_area", "cadre"),
                        names_from = "budget_scenario",
                        values_from = "staff")

bhs_wide[is.na(bhs_wide)] <- 0

# merge with redistributed hires and calculate new staff numbers
# full outer join, as hires could appear in one table but not the other
bhs_red <- merge(red_cop, bhs_wide,
                 by = c("psnu", "program_area", "cadre"),
                 all = T)

# let's add current staff
hrh_current_summary <- hrh_inputs %>%
  filter(target_level == "COP") %>%
  ungroup() %>%
  select(psnu, program_area, cadre, current_hrh)

# join the tables
bhs_red_current <- merge(bhs_red, hrh_current_summary,
                         by = c("psnu", "program_area", "cadre"), 
                         all = TRUE)

# set NAs to zero
bhs_red_current[is.na(bhs_red_current)] <- 0

# rename variables for export
bhs_red_current <- bhs_red_current %>%
  rename(`current funding level` = staff_redistributed,
         current_staffing_fixed = current_hrh)

# add another column for redistributed_fixed equal to current funding level to display both as row and columns
bhs_red_current$redistributed_fixed <- bhs_red_current$`current funding level`

# first, get this into a long format
brc_long <- pivot_longer(bhs_red_current,
                         cols = c(grep("%", names(bhs_red_current)), grep("current funding level", names(bhs_red_current))), # all columns with adjusted budget scenarios 
                         names_to = "budget_scenario",
                         values_to = "redistributed_budget_scenario")

# get budget amounts and merge
adjusted_budgets <- budget_hires_out %>%
  group_by(program_area, budget_scenario) %>%
  summarize(budget_amount = mean(budget_amount), .groups = "drop")

brc_amounts <- merge(brc_long, adjusted_budgets, by = c("program_area", "budget_scenario"), all.x = TRUE)

# In case a program area has no budget, like PrEP in Tanzania, set NAs to zero
brc_amounts[is.na(brc_amounts)] <- 0
brc_amounts <- cbind(dct_home,brc_amounts)

# save out in long format
write.csv(brc_amounts, paste(out_folder, '/Dashboard7_AdjustedFunding.csv', sep=''), row.names = F)


# Generate output for Surplus Staff ----------------------------------------------------------------------

# Calculate surplus staff and associated budget
surplus <- hrh_inputs %>%
  filter(target_level == "COP") %>%
  ungroup() %>%
  mutate(surplus = current_hrh - need) %>%
  filter(surplus > 0) %>%
  mutate(surplus_cost = surplus * current_salaries) %>%
  summarize(total_surplus = sum(surplus),
            total_surplus_cost = sum(surplus_cost),
            .groups = "drop")
surplus <- cbind(dct_home,surplus)

# save out 
write.csv(surplus, paste(out_folder, "./SurplusStaff.csv", sep=''), row.names = F)

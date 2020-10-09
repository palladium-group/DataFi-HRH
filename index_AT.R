## ETL (Transforming) Script for HRH Needs and Optimization Solution
   # Extract data from Excel based data Capture Tool
   # Transform the data appropriately 
   # Output transformed data into prescribed csv format
## Date modified: 08 October 2020

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
setwd('C:/Users/Amos/Documents/palladium/HRH')

#Import data from DCT
# ==================================================================================================
dct <- "HRH Data Collection/2020 10 01 Data.FI HRH Solution Data Collection Template_TZ.lp.AF.xlsx"

# Customization parameters
customPar <- read_excel(dct, sheet = "5. Customization Parameters", skip = 4, col_names = F)
names(customPar) <- c('param','question','response')
customPar$qn <- as.numeric(customPar$response)                                                    # % response
customPar$resp <- ifelse(customPar$response %in% c('Yes','No'), customPar$response=='Yes', NA)    # yes/no response

# Program targets
programTargets <- read_excel(dct, sheet = "2. Program Targets ", na = "0", skip = 3)

# replace problematic spaces and brackets in variable names with underscores
names(programTargets) <- gsub("\\s\\(", '_', gsub("\\)", '', names(programTargets)))
names(programTargets) <- gsub("\\s+", '_', names(programTargets))
names(programTargets) <- gsub("__", '_', names(programTargets))

# Calculations
# ==================================================================================================
# Total no. of minutes needed by a CHW annually for a particular pathway and PSNU
# --------------------------------------
programTargets <- programTargets %>%
  filter(!is.na(PSNU)) %>%  # filter out entirely empty rows that come with the excel sheet
  gather(pathway, cop_target, PrEP_NEW_Total:TX_PVLS_Total)

tot_mins <- ddply(programTargets, .(PSNU), 
            function(x){
              TOT_MINS <- list()
              
              ########### PREP
              # [1] PREP_NEW
              prep_new_target <- x$cop_target[x$pathway=='PrEP_NEW_Total']     # COP target
              
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
                'Case Manager', tot_visits_fac*(15*customPar$qn[14]*100/prep_new_target + ifelse(customPar$resp[1], 10*1, 0)) +
                tot_visits_com*(15*customPar$qn[14]*100/prep_new_target + ifelse(customPar$resp[1], 10*1, 0)),
                'Clinical-Medical', tot_visits_fac*20*1 + 0,
                'Clinical-Nursing', 0 + tot_visits_com*25*1,
                'Data Clerk', tot_visits_fac*(10*1 + 10*1) + tot_visits_com*(10*1 + 10*1), 
                'Laboratory', tot_visits_fac*20*1 + tot_visits_com*20*1,
                'Lay-CHW', 0 + 0,
                'Lay-Counselor', tot_visits_fac*20*customPar$qn[14]*100/prep_new_target + 
                tot_visits_com*20*customPar$qn[14]*100/prep_new_target,
                'Pharmacy', tot_visits_fac*15*1 + 0)
              
              TOT_MINS[[1]] <- data.frame(indicator='PrEP_NEW', matrix(unlist(tot_mins), ncol = 2, byrow = T)) 
              
              # [2] PrEP_CURR
              # COP targets for PrEP_CURR rolling over on PrEP from previous COP year
              target <- x$cop_target[x$pathway=='PrEP_CURR_Total'] - prep_new_target
              
              # no of clients served
              # 6. What percentage of people who start PrEP in the upcoming COP year will likely continue 
              #    PrEP from a health facility (vs. community)?
              # 7. What percentage of people who started PrEP in the current COP year will likely continue 
              #    PrEP from a health facility (vs. community)?
              clients_served_fac_start <- prep_new_target*customPar$qn[6]        # starting PrEP served at facility
              clients_served_com_start <- prep_new_target*(1-customPar$qn[6])    # starting PrEP served at community
              clients_served_fac_roll <- target*customPar$qn[7]          # rolling over PrEP served at facility
              clients_served_com_roll <- target*(1-customPar$qn[7])      # rolling over PrEP served at community
              
              # adjusted no. of visits per client
              # 15. How many follow up visits, on average, will people who start PrEP in the upcoming COP year likely have?
              # 17. What is the targeted continuation rate for clients starting PrEP in the upcoming COP year? 
              # 16. How many follow up visits, on average, will people who started PrEP in the current COP year likely have?
              # 18. What is the targeted continuation rate for clients rolling over on PrEP from the current COP year? 
              # the 0.5 is fixed adjustment for clients that initiate PrEP throughout the year 
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
                'Pharmacy', tot_visits_fac*15*1 + tot_visits_com*15*1)
              
              TOT_MINS[[2]] <- data.frame(indicator='PrEP_CURR', matrix(unlist(tot_mins), ncol = 2, byrow = T))
              
              ########### HTS
              # [3] HTS_SELF (Total)
              target <- x$cop_target[x$pathway=='HTS_SELF_Total']        # COP target
              
              # no of clients served
              # 19. What percentage of HTS_SELF are unassisted (vs. assisted)?
              clients_served_unass <- target*customPar$qn[19]        # unassisted
              # 8. What percentage of assisted HTS_SELF are seen at a facility (vs. community)? 
              clients_served_fac <- target*(1-customPar$qn[19])*customPar$qn[8]      # assisted facility
              clients_served_com <- target*(1-customPar$qn[19])*(1-customPar$qn[8])    # assisted community
              
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
                'Lay-CHW', tot_visits_unass*2*customPar$qn[21] + tot_visits_fac*(10*customPar$qn[20] + 10*0.05) + 
                tot_visits_com*(20*1 + 15*0.05),
                'Lay-Counselor', tot_visits_unass*2*(1 - customPar$qn[21]) + 
                  tot_visits_fac*(10*(1 - customPar$qn[20]) + 10*0.05) + 0,
                'Pharmacy', 0 + 0 + 0)
              
              TOT_MINS[[3]] <- data.frame(indicator='HTS_SELF', matrix(unlist(tot_mins), ncol = 2, byrow = T))
              
              # [4] HTS_TST_Mobile
              target <- x$cop_target[x$pathway=='HTS_TST_Mobile']      # COP target
              
              # Total no. of minutes
              tot_mins <- list( 
                'Case Manager', 0,
                'Clinical-Medical', 0,
                'Clinical-Nursing', 0,
                'Data Clerk', target*10*0.8, 
                'Laboratory', target*30*1, 
                'Lay-CHW', 0, 
                'Lay-Counselor', target*55*1, 
                'Pharmacy', 0)
              
              TOT_MINS[[4]] <- data.frame(indicator='HTS_TST', matrix(unlist(tot_mins), ncol = 2, byrow = T))
              
              # [5] HTS_TST_PMTCT_ANC1
              target <- x$cop_target[x$pathway=='HTS_TST_PMTCT_ANC1']    # COP target
              
              # Total no. of minutes
              tot_mins <- list( 
                # 3.  Can Case Managers provide testing? 
                'Case Manager', target*(10 + ifelse(customPar$resp[3], 30*1, 0)),
                'Clinical-Medical', 0,
                'Clinical-Nursing', target*(30*0.9),
                'Data Clerk', target*10*1, 
                'Laboratory', target*(0 + 30*0.1),
                'Lay-CHW', 0, 
                'Lay-Counselor', 0,
                'Pharmacy', 0)
              
              TOT_MINS[[5]] <- data.frame(indicator='HTS_TST', matrix(unlist(tot_mins), ncol = 2, byrow = T))
              
              # [6] HTS_TST_PMTCT_Post_ANC1
              TOT_MINS[[6]] <- data.frame(indicator='HTS_TST', matrix(unlist(tot_mins), ncol = 2, byrow = T))
              # [7] HTS_TST_Facility_Index
              TOT_MINS[[7]] <- data.frame(indicator='HTS_TST', matrix(unlist(tot_mins), ncol = 2, byrow = T))
              
              # [8] HTS_TST_IndexMod
              target <- x$cop_target[x$pathway=='HTS_TST_IndexMod'] # COP targets
              
              # Total no. of minutes
              tot_mins <- list( 
                # 4.  Can Lay-Counselors provide testing? 
                'Case Manager', 0,
                'Clinical-Medical', target*(ifelse(customPar$resp[4], 0, 60*0.9)),
                'Clinical-Nursing', target*(ifelse(customPar$resp[4], 0, 60*0.05)),
                'Data Clerk', target*(10*1), 
                'Laboratory', target*(ifelse(customPar$resp[4], 0, 60*0.05)),
                'Lay-CHW', target*(ifelse(customPar$resp[4], 0, 60*1)), 
                'Lay-Counselor', target*(ifelse(customPar$resp[4], 125*1, 0)),
                'Pharmacy', 0)
              
              TOT_MINS[[8]] <- data.frame(indicator='HTS_TST', matrix(unlist(tot_mins), ncol = 2, byrow = T))
              
              # [9] HTS_TST_STI
              target <- x$cop_target[x$pathway=='HTS_TST_STI'] # COP targets
              
              # Total no. of minutes
              tot_mins <- list( 
                # 3. Can Case Managers provide testing?
                'Case Manager', target*(ifelse(customPar$resp[3], 0, 30*1)),
                'Clinical-Medical', 0,
                'Clinical-Nursing', target*(ifelse(customPar$resp[3], 0, 30*0.9)),
                'Data Clerk', target*(10*1), 
                'Laboratory', target*(ifelse(customPar$resp[3], 0, 30*0.1)),
                'Lay-CHW', 0, 
                'Lay-Counselor', 0,
                'Pharmacy', 0)
              
              TOT_MINS[[9]] <- data.frame(indicator='HTS_TST', matrix(unlist(tot_mins), ncol = 2, byrow = T))
              TOT_MINS[[10]] <- data.frame(indicator='HTS_TST', matrix(unlist(tot_mins), ncol = 2, byrow = T))
              TOT_MINS[[11]] <- data.frame(indicator='HTS_TST', matrix(unlist(tot_mins), ncol = 2, byrow = T))
              
              ########### TX 
              # [12] TX_NEW General Patients 
              tx_new_total_target <- x$cop_target[x$pathway=='TX_NEW_Total']     # COP target
              tx_new_kp_target <- x$cop_target[x$pathway=='TX_NEW_KP']     # COP target
              tx_new_pmtct_new_target <- x$cop_target[x$pathway=='PMTCT_ART_New_on_life-long_ART']     # COP target
              tx_new_target <- tx_new_total_target - tx_new_kp_target - tx_new_pmtct_new_target
              
              # no of clients served
              # 9. What percentage of TX_NEW: general population are seen at a facility (vs. community)? 
              clients_served_fac <- tx_new_target*customPar$qn[9]        # facility
              clients_served_com <- tx_new_target*(1-customPar$qn[9])    # community
              
              # total no. of visits annually
              tot_visits_fac <- clients_served_fac*1       # Facility
              tot_visits_com <- clients_served_com*1       # community
              
              # Total no. of minutes
              tot_mins <- list( 
                # 25. Among TX_NEW seen at a facility, is initiation primarily nurse-led?   
                'Case Manager', tot_visits_fac*(50*1) + tot_visits_com*(50*1),
                'Clinical-Medical', tot_visits_fac*ifelse(customPar$resp[25], 0, 30*1) + tot_visits_com*40*1,
                'Clinical-Nursing', tot_visits_fac*ifelse(customPar$resp[25], 60*1, 10*1) + 0,
                'Data Clerk', tot_visits_fac*(10*1 + 15*1) + tot_visits_com*(10*1 + 15*1), 
                'Laboratory', tot_visits_fac*30*1 + tot_visits_com*30*1,
                'Lay-CHW', 0 + 0,
                'Lay-Counselor', 0 + 0,
                'Pharmacy', tot_visits_fac*ifelse(customPar$resp[25], 0, 15*1) + tot_visits_com*15*1)
              
              TOT_MINS[[12]] <- data.frame(indicator='TX_NEW', matrix(unlist(tot_mins), ncol = 2, byrow = T)) 
              
              # [13] TX_NEW_KP 
              # no of clients served
              # 10.  What percentage of TX_NEW: KP are seen at a facility (vs. community? 
              clients_served_fac <- tx_new_kp_target*customPar$qn[10]        # facility
              clients_served_com <- tx_new_kp_target*(1-customPar$qn[10])    # community
              
              # total no. of visits annually
              tot_visits_fac <- clients_served_fac*1       # Facility
              tot_visits_com <- clients_served_com*1       # community
              
              # Total no. of minutes
              tot_mins <- list( 
                # 25. Among TX_NEW seen at a facility, is initiation primarily nurse-led?   
                'Case Manager', tot_visits_fac*(50*1) + tot_visits_com*(50*1),
                'Clinical-Medical', tot_visits_fac*ifelse(customPar$resp[25], 0, 30*1) + tot_visits_com*40*1,
                'Clinical-Nursing', tot_visits_fac*ifelse(customPar$resp[25], 60*1, 10*1) + 0,
                'Data Clerk', tot_visits_fac*(10*1 + 15*1) + tot_visits_com*(10*1 + 15*1), 
                'Laboratory', tot_visits_fac*30*1 + tot_visits_com*30*1,
                'Lay-CHW', 0 + 0,
                'Lay-Counselor', 0 + 0,
                'Pharmacy', tot_visits_fac*ifelse(customPar$resp[25], 0, 15*1) + tot_visits_com*15*1)
              
              TOT_MINS[[13]] <- data.frame(indicator='TX_NEW', matrix(unlist(tot_mins), ncol = 2, byrow = T)) 
              
              # [14] PMTCT_ART_New_on_life-long_ART 
              # Total no. of minutes
              tot_mins <- list( 
                'Case Manager', tx_new_pmtct_new_target*40*1, 
                'Clinical-Medical', 0,
                'Clinical-Nursing', tx_new_pmtct_new_target*30*1,
                'Data Clerk', tx_new_pmtct_new_target*25*1, 
                'Laboratory', tx_new_pmtct_new_target*30*1,
                'Lay-CHW', 0,
                'Lay-Counselor', 0,
                'Pharmacy', 0)
              
              TOT_MINS[[14]] <- data.frame(indicator='TX_NEW', matrix(unlist(tot_mins), ncol = 2, byrow = T))
              
              # [15] TX_CURR General Patients: Drug Dispensing Frequency & Clinical Consultations
              tx_curr_total_target <- x$cop_target[x$pathway=='TX_CURR_Total']     # COP target
              tx_curr_kp_target <- x$cop_target[x$pathway=='TX_CURR_KP']     # COP target
              tx_curr_pmtct_already_target <- x$cop_target[x$pathway=='PMTCT_ART_Already_on_life-long_ART_at_the_beginning_of_current_pregnancy']     # COP target
              target <- tx_curr_total_target - tx_curr_kp_target - tx_curr_pmtct_already_target
              
              # no of clients served
              # 11.  What percentage of TX_CURR: general population are seen at a facility (vs. community)? 
              clients_served_fac <- target*customPar$qn[11]        # facility
              clients_served_com <- target*(1-customPar$qn[11])    # community
              
              # total no. of visits annually
              tot_visits_fac <- clients_served_fac*1       # Facility
              tot_visits_com <- clients_served_com*1       # community
              
              # Total no. of minutes
              tot_mins <- list( 
                # 28. Among TX_CURR, is the tracing of clients to their home due to missed appointments done primarily 
                #     by a Lay-CHW or a Case Manager? 
                # 29. What percentage of TX_CURR are traced to their home due to missed appointments?
                'Case Manager', tot_visits_fac*(                                                  
                          (15*1 + ifelse(customPar$response[34]=='Lay-CHW', 0,   
                                     120*customPar$qn[35]))*(               # Drug Dispensing Frequency
                                         12*customPar$qn[27] +                   # <3 months
                                         4*customPar$qn[28] +                    # 3-5 months     
                                         2*customPar$qn[29]) +                   # 6 months or more    
                          (15*customPar$qn[27]*(                          # Clinical Consultations
                                         12*customPar$qn[31] +                   # <3 months
                                         0 +                                     # 3-5 months
                                         0))) +                                  # 6 months or more
                      tot_visits_com*(                                                  
                          (15*1 + ifelse(customPar$response[34]=='Lay-CHW', 0,            
                                    120*customPar$qn[35]))*(              # Drug Dispensing Frequency
                                        12*customPar$qn[27] +                    # <3 months
                                        4*customPar$qn[28] +                     # 3-5 months
                                        2*customPar$qn[29]) +                    # 6 months or more
                      0),                            
                'Clinical-Medical', tot_visits_fac*20*1*(0 +              # Clinical Consultations
                                        12*customPar$qn[31] +                    # <3 months
                                        4*customPar$qn[32] +                     # 3-5 months
                                        2*customPar$qn[33]) +                    # 6 months or more
                       tot_visits_com*20*1*(0 +                           # Clinical Consultations
                                         12*customPar$qn[31] +                   # <3 months
                                         4*customPar$qn[32] +                    # 3-5 months
                                         2*customPar$qn[33]),                    # 6 months or more
                'Clinical-Nursing', tot_visits_fac*(                      
                                      15*1*(                              # Drug Dispensing Frequency
                                         12*customPar$qn[27] +                   # <3 months
                                         4*customPar$qn[28] +                    # 3-5 months
                                         2*customPar$qn[29]) +                   # 6 months or more
                                      5*1*(                               # Clinical Consultations
                                         12*customPar$qn[31] +                   # <3 months
                                         4*customPar$qn[32] +                    # 3-5 months
                                         2*customPar$qn[33])) +                  # 6 months or more
                                        (0 + 0),
                'Data Clerk', tot_visits_fac*(                                                  
                                      10*1*(                              # Drug Dispensing Frequency
                                         12*customPar$qn[27] +                   # <3 months
                                         4*customPar$qn[28] +                    # 3-5 months
                                         2*customPar$qn[29]) +                   # 6 months or more
                                      10*1*(                              # Clinical Consultations
                                         12*customPar$qn[31] +                   # <3 months
                                         4*customPar$qn[32] +                    # 3-5 months
                                         2*customPar$qn[33])) +                  # 6 months or more
                              tot_visits_com*(                                                                                              
                                      10*1*(                              # Drug Dispensing Frequency
                                         12*customPar$qn[27] +                   # <3 months
                                         4*customPar$qn[28] +                    # 3-5 months
                                         2*customPar$qn[29]) +                   # 6 months or more
                                      10*1*(                              # Clinical Consultations
                                         12*customPar$qn[31] +                   # <3 months
                                         4*customPar$qn[32] +                    # 3-5 months
                                         2*customPar$qn[33])),                   # 6 months or more
                'Laboratory', tot_visits_fac*(0 + 
                                      15*1*(                              # Clinical Consultations
                                         12*customPar$qn[31] +                   # <3 months
                                         4*customPar$qn[32] +                    # 3-5 months
                                         2*customPar$qn[33]) ) +                 # 6 months or more
                              tot_visits_com*(0 + 
                                      15*1*(                              # Clinical Consultations
                                         12*customPar$qn[31] +                   # <3 months
                                         4*customPar$qn[32] +                    # 3-5 months
                                         2*customPar$qn[33])),                   # 6 months or more
                'Lay-CHW', 
                              tot_visits_fac*(
                                ifelse(customPar$response[34]=='Lay-CHW', 120*customPar$qn[35], 0)*(# Drug Dispensing Frequency 
                                         12*customPar$qn[27] +                   # <3 months 
                                         4*customPar$qn[28] +                    # 3-5 months 
                                         2*customPar$qn[29]) +                   # 6 months or more 
                                 0) + 
                              tot_visits_com*(
                                ifelse(customPar$response[34]=='Lay-CHW', 120*customPar$qn[35], 0)*(# Drug Dispensing Frequency 
                                        12*customPar$qn[27] +                    # <3 months 
                                        4*customPar$qn[28] +                     # 3-5 months 
                                        2*customPar$qn[29]) +                    # 6 months or more 
                                    0),
                'Lay-Counselor', tot_visits_fac*(                                                                                   
                                   10*1*(                                   # Drug Dispensing Frequency
                                        12*customPar$qn[27] +                   # <3 months
                                        4*customPar$qn[28] +                    # 3-5 months
                                        2*customPar$qn[29]) +                   # 6 months or more 
                                    0) + 
                                  tot_visits_com*(                                                                                     
                                    10*1*(                                  # Drug Dispensing Frequency 
                                        12*customPar$qn[27] +                   # <3 months
                                        4*customPar$qn[28] +                    # 3-5 months
                                        2*customPar$qn[29]) +                   # 6 months or more 
                                      0),
                'Pharmacy', tot_visits_fac*(                                                                                      
                                    5*1*(                                   # Drug Dispensing Frequency  
                                        12*customPar$qn[27] +                   # <3 months
                                        4*customPar$qn[28] +                    # 3-5 months
                                        2*customPar$qn[29]) +                   # 6 months or more 
                                      0) + 
                            tot_visits_com*(                                                                                            
                                    5*1*(                                  # Drug Dispensing Frequency
                                        12*customPar$qn[27] +                  # <3 months
                                        4*customPar$qn[28] +                   # 3-5 months
                                        2*customPar$qn[29]) +                  # 6 months or more 
                                        0))
              
              TOT_MINS[[15]] <- data.frame(indicator='TX_CURR', matrix(unlist(tot_mins), ncol = 2, byrow = T)) 
              
              # [16] PMTCT_ART_Already_on_life-long_ART_at_the_beginning_of_current_pregnancy 
              # Total no. of minutes
              tot_mins <- list( 
                # 31. Among PMTCT_ART Already on Life-long ART at the beginning of the current pregnancy, is the tracing 
                #     of clients to their home due to missed appointments done primarily by a Lay-CHW or a Case Manager? 
                # 32. What percentage of PMTCT_ART Already on Life-long ART at the beginning of the current pregnancy 
                #     seen at a facility are traced to their home due to missed appointments? 
                # 33. What percentage of PMTCT_ ART Already on Life-long ART at the beginning of the current pregnancy are 
                #     unstable and have monthly visits to a facility (vs. those who are stable and have quarterly visits)? 
                'Case Manager', tx_curr_pmtct_already_target*((
                                     30*1 + 
                                     ifelse(customPar$response[40]=='Lay-CHW', 0, 
                                                        120*customPar$qn[41]))*12*customPar$qn[42] +  # Unstable - Monthly
                                     (30*1 + 
                                      ifelse(customPar$response[40]=='Lay-CHW', 0, 
                                                        120*customPar$qn[41]))*4*(1 - customPar$qn[42])), # Stable - Quarterly
                'Clinical-Medical', 0,
                'Clinical-Nursing', tx_curr_pmtct_already_target*(
                                              25*1*12*customPar$qn[42] +  # Unstable - Monthly
                                              25*1*4*(1 - customPar$qn[42])), # Stable - Quarterly
                'Data Clerk', tx_curr_pmtct_already_target*(
                                              10*1*12*customPar$qn[42] +  # Unstable - Monthly
                                              10*1*4*(1 - customPar$qn[42])), # Stable - Quarterly 
                'Laboratory', 0,
                'Lay-CHW', tx_curr_pmtct_already_target*(
                                              ifelse(customPar$response[40]=='Lay-CHW', 120*customPar$qn[41], 
                                                               0)*12*customPar$qn[42] + # Unstable - Monthly
                                              ifelse(customPar$response[40]=='Lay-CHW', 120*customPar$qn[41], 
                                                               0)*4*(1 - customPar$qn[42])), # Stable - Quarterly
                'Lay-Counselor', 0,
                'Pharmacy', 0)
              
              TOT_MINS[[16]] <- data.frame(indicator='TX_NEW', matrix(unlist(tot_mins), ncol = 2, byrow = T))
              
              # [17] TX_CURR Key Population: Drug Dispensing Frequency & Clinical Consultations
              # no of clients served
              # 12.  What percentage of TX_CURR: KP are seen at a facility (vs. community)?
              clients_served_fac <- tx_curr_kp_target*customPar$qn[12]        # facility
              clients_served_com <- tx_curr_kp_target*(1-customPar$qn[12])    # community
              
              # total no. of visits annually
              tot_visits_fac <- clients_served_fac*1       # Facility
              tot_visits_com <- clients_served_com*1       # community
              
              # Total no. of minutes
              tot_mins <- list( 
                # 28. Among TX_CURR, is the tracing of clients to their home due to missed appointments done primarily by a Lay-CHW or a Case Manager? 
                # 29. What percentage of TX_CURR are traced to their home due to missed appointments?
  # 26.  What percentage of TX_CURR are receiving drugs on the following schedules?
  # <3 months, 3-5 months, 6 months or more
  # 27.  What percentage of TX_CURR  have clinical consults at the following frequencies?
  #   <3 months, 3-5 months, 6 months or more
  # 31 32 33
                'Case Manager', tot_visits_fac*(                                                  
                              (15*1 + ifelse(customPar$response[34]=='Lay-CHW', 0,   # Drug Dispensing Frequency
                                 120*customPar$qn[35]))*(12*customPar$qn[27] +            # <3 months
                                                          4*customPar$qn[28] +            # 3-5 months     
                                                          2*customPar$qn[29]) +           # 6 months or more    
                                        (15*customPar$qn[27]*(                      # Clinical Consultations
                                                       12*customPar$qn[31] +              # <3 months
                                                       0 +                                # 3-5 months
                                                       0))) +                             # 6 months or more
                                tot_visits_com*(                                                  
                                        (15*1 + ifelse(customPar$response[34]=='Lay-CHW', 0,            
                                                  120*customPar$qn[35]))*(          # Drug Dispensing Frequency
                                                      12*customPar$qn[27] +               # <3 months
                                                       4*customPar$qn[28] +               # 3-5 months
                                                       2*customPar$qn[29]) +              # 6 months or more
                                        0),                            
                'Clinical-Medical', tot_visits_fac*20*1*(0 +                        # Clinical Consultations
                                                      12*customPar$qn[31] +               # <3 months
                                                      4*customPar$qn[32] +                # 3-5 months
                                                      2*customPar$qn[33]) +               # 6 months or more
                                    tot_visits_com*20*1*(0 +                        # Clinical Consultations
                                                      12*customPar$qn[31] +               # <3 months
                                                      4*customPar$qn[32] +                # 3-5 months
                                                      2*customPar$qn[33]),                # 6 months or more
                'Clinical-Nursing', tot_visits_fac*(                                # Drug Dispensing Frequency
                                                    15*1*(
                                                      12*customPar$qn[27] +               # <3 months
                                                      4*customPar$qn[28] +                # 3-5 months
                                                      2*customPar$qn[29]) +               # 6 months or more
                                                                                                
                                                    5*1*(                           # Clinical Consultations
                                                      12*customPar$qn[31] +               # <3 months
                                                      4*customPar$qn[32] +                # 3-5 months
                                                      2*customPar$qn[33])) +              # 6 months or more
                                                   (0 + 0),
                'Data Clerk', tot_visits_fac*(                                                  
                                                    10*1*(                          # Drug Dispensing Frequency
                                                      12*customPar$qn[27] +               # <3 months
                                                      4*customPar$qn[28] +                # 3-5 months
                                                      2*customPar$qn[29]) +               # 6 months or more
                                                    10*1*(                          # Clinical Consultations
                                                      12*customPar$qn[31] +               # <3 months
                                                      4*customPar$qn[32] +                # 3-5 months
                                                      2*customPar$qn[33])) +              # 6 months or more
                              tot_visits_com*(                                                                                              
                                                    10*1*(                          # Drug Dispensing Frequency
                                                      12*customPar$qn[27] +               # <3 months
                                                      4*customPar$qn[28] +                # 3-5 months
                                                      2*customPar$qn[29]) +               # 6 months or more
                                                                                                
                                                    10*1*(                          # Clinical Consultations
                                                      12*customPar$qn[31] +               # <3 months
                                                      4*customPar$qn[32] +                # 3-5 months
                                                      2*customPar$qn[33])),               # 6 months or more
                'Laboratory', tot_visits_fac*(0 + 
                                                                                                
                                                    15*1*(                          # Clinical Consultations
                                                      12*customPar$qn[31] +               # <3 months
                                                      4*customPar$qn[32] +                # 3-5 months
                                                      2*customPar$qn[33]) ) +             # 6 months or more
                              tot_visits_com*(0 + 
                                                    15*1*(                          # Clinical Consultations
                                                      12*customPar$qn[31] +               # <3 months
                                                      4*customPar$qn[32] +                # 3-5 months
                                                      2*customPar$qn[33])),               # 6 months or more
                'Lay-CHW', 
                  tot_visits_fac*(
                    ifelse(customPar$response[34]=='Lay-CHW', 120*customPar$qn[35], 0)*(# Drug Dispensing Frequency 
                                                      12*customPar$qn[27] +               # <3 months 
                                                      4*customPar$qn[28] +                # 3-5 months 
                                                      2*customPar$qn[29]) +               # 6 months or more 
                      0) + 
                  tot_visits_com*(
                    ifelse(customPar$response[34]=='Lay-CHW', 120*customPar$qn[35], 0)*(# Drug Dispensing Frequency 
                                                      12*customPar$qn[27] +               # <3 months 
                                                      4*customPar$qn[28] +                # 3-5 months 
                                                      2*customPar$qn[29]) +               # 6 months or more 
                      0),
                'Lay-Counselor', tot_visits_fac*(                                       # Drug Dispensing Frequency                                            
                                                    10*1*(
                                                      12*customPar$qn[27] +               # <3 months
                                                      4*customPar$qn[28] +                # 3-5 months
                                                      2*customPar$qn[29]) +               # 6 months or more 
                                                0) + 
                              tot_visits_com*(                                                                                     
                                                    10*1*(                              # Drug Dispensing Frequency 
                                                      12*customPar$qn[27] +               # <3 months
                                                      4*customPar$qn[28] +                # 3-5 months
                                                      2*customPar$qn[29]) +               # 6 months or more 
                                            0),
                'Pharmacy', tot_visits_fac*(                                                                                      
                                                    5*1*(                              # Drug Dispensing Frequency  
                                                      12*customPar$qn[27] +               # <3 months
                                                      4*customPar$qn[28] +                # 3-5 months
                                                      2*customPar$qn[29]) +               # 6 months or more 
                                            0) + 
                            tot_visits_com*(                                                                                            
                                                    5*1*(                             # Drug Dispensing Frequency
                                                      12*customPar$qn[27] +               # <3 months
                                                      4*customPar$qn[28] +                # 3-5 months
                                                      2*customPar$qn[29]) +               # 6 months or more 
                                            0)
                )
              
              TOT_MINS[[17]] <- data.frame(indicator='TX_CURR', matrix(unlist(tot_mins), ncol = 2, byrow = T)) 
              
              # [18] TX_PVLS_Total 
              target <- x$cop_target[x$pathway=='TX_PVLS_Total']     # COP target
              # 30. What percentage of TX_PVLS are targeted for the following schedules?
                      # >2x/year, 2x/year, 1x/year
              # Total no. of minutes
              tot_mins <- list( 
                'Case Manager', target*(10*1 + 10*1)*(4*customPar$qn[37] +   # >2x/year
                                                      2*customPar$qn[38] +   # 2x/year
                                                      1*customPar$qn[39]),   # 1x/year
                'Clinical-Medical', target*(5*1 + 10*1)*(4*customPar$qn[37] +   # >2x/year
                                                         2*customPar$qn[38] +   # 2x/year
                                                         1*customPar$qn[39]),   # 1x/year
                'Clinical-Nursing', 0,
                'Data Clerk', target*20*1*(4*customPar$qn[37] +   # >2x/year
                                           2*customPar$qn[38] +   # 2x/year
                                           1*customPar$qn[39]),   # 1x/year 
                'Laboratory', target*15*1*(4*customPar$qn[37] +   # >2x/year
                                           2*customPar$qn[38] +   # 2x/year
                                           1*customPar$qn[39]),   # 1x/year
                'Lay-CHW', 0,
                'Lay-Counselor', 0,
                'Pharmacy', 0)
              
              TOT_MINS[[18]] <- data.frame(indicator='TX_PVLS', matrix(unlist(tot_mins), ncol = 2, byrow = T)) 
              
              TOT_MINS <- do.call(rbind, TOT_MINS)
              names(TOT_MINS) <- c('indicator','cadre','tot_mins')
              return(TOT_MINS)
            })

tot_mins <- tot_mins %>% 
  group_by(PSNU,indicator,cadre) %>% 
  summarise(tot_mins=sum(as.numeric(tot_mins)))

# Available working time
# --------------------------------------
awt <- data.frame(cadre=c('Clinical-Nursing','Clinical-Medical','Data Clerk','Case Manager','Lay-Counselor',
                          'Lay-CHW','Laboratory','Pharmacy'), working_days_wk=5, working_hrs_day=8, 
                  public_holidays=15, special_leave=4)

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
awt <- awt %>% mutate(
    annual_leave=ifelse(cadre %in% c('Clinical-Medical','Data Clerk'), 24, 21),
    sick_leave=ifelse(cadre %in% c('Clinical-Nursing','Laboratory','Pharmacy'), 3, 4),
    awt_days=working_days_wk*52 - (annual_leave+public_holidays+sick_leave+special_leave+training_days),
    awt_hrs=awt_days*8) 

# HRH need
# --------------------------------------
psnu_list <- read_excel(dct, sheet = "1. PSNU List", skip = 2)
names(psnu_list) <- gsub('\\s', '', names(psnu_list))

current_salaries <- read_excel(dct, sheet = "4. Avg. Annual HRH Remuneration", range = "A5:B13")
names(current_salaries) <- c('cadre','current_salaries')

current_hrh <- read_excel(dct, sheet = "3.Current PEPFAR-Supported HRH ", skip = 3)
current_hrh <- current_hrh[-1]
# Current HRH Salaries
names(current_hrh) <- c('DATIM_UID','PSNU',
                        'Clinical_Medical_Total','Clinical_Medical_PrEP','Clinical_Medical_HTS','Clinical_Medical_TX',
                        'Clinical_Nursing_Total','Clinical_Nursing_PrEP','Clinical_Nursing_HTS','Clinical_Nursing_TX',
                        'LayCounselor_Total','Lay_Counselor_PrEP','Lay_Counselor_HTS','Lay_Counselor_TX',
                        'Lay_CHW_Total','Lay_CHW_PrEP','Lay_CHW_HTS','Lay_CHW_TX',
                        'Case_Manager_Total','Case_Manager_PrEP','Case_Manager_HTS','Case_Manager_TX',
                        'Pharmacy_Total','Pharmacy_PrEP','Pharmacy_HTS','Pharmacy_TX',
                        'Laboratory_Total','Laboratory_PrEP','Laboratory_HTS','Laboratory_TX',
                        'Data_Clerk_Total','Data_Clerk_PrEP','DataClerk_HTS','Data_Clerk_TX',
                        'Total_FTE')

current_hrh <- current_hrh %>% 
  filter(!is.na(PSNU)) %>% 
  select(DATIM_UID,PSNU,matches('_Total')) %>% 
  gather(cadre,current_hrh,Clinical_Medical_Total:Data_Clerk_Total) %>% 
  mutate(cadre=gsub('_', '-', gsub('_Total', '', cadre)))

hrh <- programTargets %>% 
  filter(!grepl('PMTCT_ART', pathway)) %>% 
  mutate(indicator=gsub("(_\\w+)_.*", "\\1", pathway),
         indicator=ifelse(grepl( 'HTS_TST', indicator), 'HTS_TST', indicator),
         program_area=gsub("(*_)*.[A-Z]+$", '', indicator)) %>% 
  group_by(PSNU,program_area) %>% 
  mutate(cop_target=sum(cop_target)) %>% 
  ungroup() %>% 
  full_join(tot_mins) %>% 
  full_join(awt) %>%
  full_join(psnu_list) %>% 
  full_join(current_hrh) %>% 
  full_join(current_salaries) %>% 
  mutate(ServiceStandard = ifelse(tot_mins>0, 60/tot_mins, 0),
       AnnualWorkload = cop_target * ServiceStandard,
       StandardWorkload = ServiceStandard * awt_hrs,
       CategoryAllowedStandard = (weekly_non_clinical_hrs/(working_days_wk * working_hrs_day))/working_hrs_day,
       CategoryAllowedFactor = 1 / (1 - (CategoryAllowedStandard / awt_hrs * 100)), #Category Allowed Factor
       HRHRequirement = (AnnualWorkload / StandardWorkload) * CategoryAllowedFactor,
       country = OperatingUnit,
       current_hrh = current_hrh / 8, # Assumption: current HRH are equally distributed amongst different program areas
       `Gap - Staffing` = (HRHRequirement - current_hrh),
       `Need - Costing` = (HRHRequirement * as.numeric(current_salaries)),
       `Gap - Costing` = ((HRHRequirement - current_hrh) * as.numeric(current_salaries)),
       `Existing - Costing` = current_hrh - as.numeric(current_salaries),
       `%Shortage` = HRHRequirement - current_hrh,
       `%Allocated` = HRHRequirement - current_hrh,
       `Existing FTEs` = current_hrh,
       `HCW Need` = HRHRequirement) 

# Prioritization ranking
# --------------------------------------








# write output files
# ==================================================================================================

# Dashboard one data
write.csv(hrh, 'Output Tables/HRHData.csv', row.names = F)

# PRI List


# PRI Ranking

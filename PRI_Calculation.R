# Set Global Variables -------------------------------------------------------------------
BUDGET_SCENARIOS <- c(.8, .9, .95, 1.05, 1.1, 1.2) # factor by which to multiply current budget

# Load Libraries ------------------------------------------------------------------------
library(dplyr)
library(tidyr)

# Load Derived Inputs -------------------------------------------------------------------

# Number of rows = # PSNU (varies) * # Cadre (8) * # Program Area (3) * # Target Level (3)
# Columns = PSNU | Program_Area | Cadre | Target_Level | Need
hrh_inputs <- readRDS('./hrh_out_pri.RData')
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

# Set up dataframe to hold final lists for each program area / target level
optimal_out <- data.frame()

# Loop through program areas and target levels
for (i in PROGRAM_AREAS){
  for(j in TARGET_LEVELS){
    
    # Get budget for program area
    budget_temp <- budget[budget$program_area == i, "pa_budget"]
    
    # Filter pri score list for particular Program Area and Target Level
    dat <- optimal_scores_out %>% filter(program_area == i & target_level == j)
    
    # Merge salary data
    dat <- merge(dat, salaries, by = "cadre")
    
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

# Summarize Optimal Allocation by PSNU, Program Area, Target Level, Cadre
optimal_summary <- optimal_out %>%
  group_by(psnu, program_area, target_level, cadre) %>%
  summarize(optimal = n(), .groups = "drop")

# Calculate Attribution of Current Staff----------------------------------------------------------------------

# Merge optimal allocation and current allocation and Need
optimal_current_need <- merge(hrh_inputs, optimal_summary, by = c("psnu", "program_area", "target_level", "cadre"), all.x = T)

# Some combinations of PSNU | Program Area | Target Level | Cadre have no FTE in optimal allocation, so come through as NA
# Set these to zero
optimal_current_need <- optimal_current_need %>%
  mutate(optimal = ifelse(is.na(optimal), 0, optimal))

# One percent attribution score for each program area and target level
attribution_out <- data.frame()

for (i in PROGRAM_AREAS){
  for(j in TARGET_LEVELS){
  
    # Filter to program area
    dat <- optimal_current_need %>% filter(program_area == i & target_level == j)
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
        # Get current staff (Column D in Excel)
        sum_current <- sum(dat_cadre$current_hrh)
        # Calculate deviation (column G in Excel)
        deviation <- min(abs((sum_current - sum_optimal) / sum_optimal), 1)
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
    current_attribution <- optimal_attribution * (1-average_deviation)
    
    # Collect fields for output
    att_df <- data.frame(program_area = i, target_level = j, sum_optimal_pa, sum_need_pa,
                         average_deviation, optimal_attribution, current_attribution)
    attribution_out <- rbind(attribution_out, att_df)
  }
}

# Split output by program area
attributions_split <- split(attribution_out, attribution_out$program_area)
# Rename variables to tag by program area and drop program area variable
attributions_renamed <- lapply(attributions_split, function(x){
  names(x) <- paste0(x$program_area[1], "_", names(x))
  x[, -1]
  })
# Join together so we have all combinations of program area / target level (if 3 of each, then 3^2 = 27 rows)
attribution_current <- attributions_renamed[[1]] %>%
  merge(., attributions_renamed[[2]]) %>%
  merge(., attributions_renamed[[3]])

# Calculate total attributions
attribution_current_totals <- attribution_current %>%
  mutate(total_average_deviation = (HTS_average_deviation * HTS_sum_optimal_pa +
           TX_average_deviation * TX_sum_optimal_pa +
           PrEP_average_deviation * PrEP_sum_optimal_pa) / 
           (HTS_sum_optimal_pa + TX_sum_optimal_pa + PrEP_sum_optimal_pa),
         total_optimal_attribution = (HTS_sum_optimal_pa + TX_sum_optimal_pa + PrEP_sum_optimal_pa) /
           (HTS_sum_need_pa + TX_sum_need_pa + PrEP_sum_need_pa),
         total_current_attribution = total_optimal_attribution * (1 - total_average_deviation))

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

# One percent attribution calculation for each program area
attribution_red_out <- data.frame()

for (i in PROGRAM_AREAS){
  for(j in TARGET_LEVELS){
    
    # Filter to program area
    dat <- optimal_red_need %>% filter(program_area == i & target_level == j)
    sum_optimal_pa <- sum(dat$optimal)
    sum_need_pa <- sum(dat$need)
    
    deviations <- c()
    
    # loop through PSNUs
    for (k in PSNU){
      # Loop through Cadres
      for (l in CADRES){
        
        dat_cadre <- dat %>% filter(psnu == k & cadre == l) 
        
        # Get optimal hires (Column C in Excel)
        sum_optimal <- sum(dat_cadre$optimal)
        # Get current staff (Column D in Excel)
        sum_red <- sum(dat_cadre$staff_redistributed)
        # Calculate deviation (column G in Excel)
        deviation <- min(abs((sum_red - sum_optimal) / sum_optimal), 1)
        # If sum_optimal is 0, then deviation will be NaN - in this case, set to zero
        if(is.nan(deviation)) {deviation <- 0}
        # Calculate weighted average by cadre (column H in Excel)
        weighted_deviation <- deviation * (sum_optimal / sum_optimal_pa)
        if(is.nan(weighted_deviation)){weighted_deviation <- 0}
        
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
    red_attribution <- optimal_attribution * (1-average_deviation)
    
    att_df <- data.frame(program_area = i, target_level = j, sum_optimal_pa, sum_need_pa,
                         average_deviation, optimal_attribution, red_attribution)
    attribution_red_out <- rbind(attribution_red_out, att_df)
  }
}

# Split output by program area
attributions_red_split <- split(attribution_red_out, attribution_red_out$program_area)
# Rename variables to tag by program area and drop program area variable
attributions_red_renamed <- lapply(attributions_red_split, function(x){
  names(x) <- paste0(x$program_area[1], "_", names(x))
  x[, -1]
})
# Join together so we have all combinations of program area / target level (if 3 of each, then 3^2 = 27 rows)
attribution_red <- attributions_red_renamed[[1]] %>%
  merge(., attributions_red_renamed[[2]]) %>%
  merge(., attributions_red_renamed[[3]])


# Calculate total attributions
attribution_red_totals <- attribution_red %>%
  mutate(total_average_deviation = (HTS_average_deviation * HTS_sum_optimal_pa +
           TX_average_deviation * TX_sum_optimal_pa +
           PrEP_average_deviation * PrEP_sum_optimal_pa) / 
           (HTS_sum_optimal_pa + TX_sum_optimal_pa + PrEP_sum_optimal_pa),
         total_optimal_attribution = (HTS_sum_optimal_pa + TX_sum_optimal_pa + PrEP_sum_optimal_pa) /
           (HTS_sum_need_pa + TX_sum_need_pa + PrEP_sum_need_pa),
         total_red_attribution = total_optimal_attribution * (1 - total_average_deviation))

attribution_red_clean <- attribution_red_totals %>%
  select(PrEP_target_level, HTS_target_level, TX_target_level, PrEP_red_attribution, HTS_red_attribution, TX_red_attribution, total_red_attribution)

# Generate output for dashboard 4 Attribution ----------------------------------------

attribution_all <- merge(attribution_current_clean, attribution_red_clean, 
                         by = c("PrEP_target_level", "HTS_target_level", "TX_target_level"))

write.csv(attribution_all, "./Dashboard4_Attribution.csv")

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

# #save out in wide format
write.csv(dashboard5, "./Dashboard5_RedistributionProfiles.csv")

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

write.csv(dashboard6, "./Dashboard6_OptimalAllocation.csv")


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

# save out in long format
write.csv(brc_long, "./Dashboard7_AdjustedFunding.csv")


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

# save out 
write.csv(surplus, "./SurplusStaff.csv")
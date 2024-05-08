#----------------------------------------------------------------------------------------------------------#
#                                                                                                          #
#   A D U L T   A S T H M A    S C   O U T C O M E S   A N A L Y S I S   s c R I P T                       #
#                                                                                                          #
#   Author: Alex Adamson                                                                                   #
#----------------------------------------------------------------------------------------------------------#




# Set up the libraries

library(lme4)
library(dplyr)
# library(readstata13)
# library(xlsx)
source("C:/Users/aadamson/Documents/R/My R functions/tidyoutput.R")
source("C:/Users/aadamson/Documents/R/My R functions/lintestOR.R")


library(janitor)
library(officer)
library(flextable)
library(tidyverse)
library(tidyr)
library(survival)
library(survminer)
library(ggplot2)
library(survsup)
# library(epitools)
library(psych)
library(comorbidity)
library(lme4)

nlc <- function(x) {cat(paste("\n", x, "\n", sep = ""))}
CP <- function(x) {write.table(x, "clipboard", sep = "\t", row.names = FALSE)}
CPwithrn <- function(x) {write.table(x, "clipboard", sep = "\t", row.names = TRUE)}



meanSumRound <- function(x, variable, roundno) {
  variable <- as.character(variable)
  
  # Using Psych...
  
  # varcol <- filter(psychic, vars == variable) %>% 
  #   dplyr::select(vars, N, mean, sd)

  # without Psych...
  
  
  varcol <- x %>% ungroup() %>% select(!!!variable) %>% drop_na(!!!variable) %>%
    summarise(vars = variable, N = n(), mean = mean(.data[[variable]]), sd = sd(.data[[variable]])) %>% as.data.frame()

    varcol[ ,3:4] <- format(round(varcol[ ,3:4], roundno), nsmall = roundno)
  colnames(varcol) <- paste(variable, colnames(varcol), sep = "_")
  return(varcol[ , -1])
  
}


mediSumRound <- function(x, variable, roundno = 0) {
  variable <- as.character(variable)
  

  # Using Psych...
  
  # varcol <- filter(psychic, vars == variable) %>% 
  #   dplyr::select(vars, N, median, lo.quart, hi.quart)

  

  # without Psych...
  
  varcol <- x %>% ungroup() %>% select(!!!variable) %>% drop_na(!!!variable) %>%
    summarise(vars = variable, N = n(), median = median(.data[[variable]]), lo.quart = summary(.data[[variable]])[2],
              hi.quart = summary(.data[[variable]])[5]) %>% as.data.frame()
  
  

    # function updated so that it just gives numbers back rounded according to roundno,
  # without making any exceptions for midway points etc
  varcol[ ,3:5] <- sprintf(paste0("%.", roundno, "f"), 
                           round(varcol[ ,3:5], roundno), nsmall = roundno) # otherwise use 'roundno'
  
  
  
  colnames(varcol) <- paste(variable, colnames(varcol), sep = "_")
  return(varcol[ , -1])
}



FreqSum <- function(x, varname) {
  
  varname <- as.character(varname)
  gen <- x %>% dplyr::select(!!varname) %>% drop_na()
  var_N <- data.frame(nrow(gen))
  colnames(var_N) <- paste0(varname, "_N")
  
  #   if(nrow(gen) == 0) {return(var_N)}
  
  #  else {
  
  gen0 <- as.data.frame(table(gen[[1]]))
  gen1 <- as.data.frame(round(prop.table(table(gen[[1]]))*100, 1), nsmall = 1) %>% 
    dplyr::rename(perc = Freq)
  gen2 <- inner_join(gen0, gen1, by = "Var1")
  gen2$perc <- sprintf("%.1f", gen2$perc)
  # gen.E2$England <- paste(gen.E2$Freq, " (", gen.E2$perc, ")", sep = "")
  # gen.E2 <- select(gen.E2, Var1, England)
  for (i in 1:nrow(gen2)) {
    gen3 <- gen2
    gen3$Var1 <- as.character(gen3$Var1)
    gen3 <- gen3[i, ]
    colnames(gen3) <- c("Var1", paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_n"),
                        paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_perc")) 
    var_N <- cbind(var_N, gen3[ ,2:3])
  }
  return(var_N)
  
  # }
}



medTable <- function(x, varname, roundno = 0) {   
  # x is the dataset, varname is the variable name, val is the value of interest (e.g. males) 
  
  # NOTE!!! Medians rounded to 0dp by default
  
  varname <- as.character(varname)
  
  eng <- x %>% filter(country == "England") %>% dplyr::select(varname)
  EN <- length(eng[!is.na(eng)])
  engIQR <- round(quantile(eng[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), roundno)
  eng <- paste(engIQR[2], " (", engIQR[1], " to ", engIQR[3], ")", sep = "")
  
  
  wal <- x %>% filter(country == "Wales") %>% dplyr::select(varname)
  WN <- length(wal[!is.na(wal)])
  walIQR <- round(quantile(wal[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), roundno)
  wal <- paste(walIQR[2], " (", walIQR[1], " to ", walIQR[3], ")", sep = "")
  
  
  scot <- x %>% filter(country == "Scotland") %>% dplyr::select(varname)
  SN <- length(scot[!is.na(scot)])
  scotIQR <- round(quantile(scot[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), roundno)
  scot <- paste(scotIQR[2], " (", scotIQR[1], " to ", scotIQR[3], ")", sep = "")
  
  
  all <- x %>% dplyr::select(varname)
  AN <- length(all[!is.na(all)])
  allIQR <- round(quantile(all[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE), roundno)
  all <- paste(allIQR[2], " (", allIQR[1], " to ", allIQR[3], ")", sep = "")
  
  ret <- matrix(c(varname, eng, scot, wal, all), nrow = 1, ncol = 5)
  
  colnames(ret) <- c("Variable", 
                     paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
                     paste("Scotland (N=", format(SN, big.mark=",", trim=TRUE), ")", sep = ""),
                     paste("Wales (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""),
                     paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  
  # colnames(ret) <- c("Variable",
  #                    paste("All (N=", AN, ")", sep = ""),
  #                    paste("England (N=", EN, ")", sep = ""),
  #                    paste("Scotland (N=", SN, ")", sep = ""),
  #                    paste("Wales (N=", WN, ")", sep = ""))
  
  ret <- as.data.frame(ret)
  
  return(ret)
}



# And another one that will work for calculatng frequencies:

# Changing this so it's inline with what Sophie wants

myFreqTable <- function(x, varname) {
  
  
  varname <- as.character(varname)
  #  print(varname)
  gen.E <- x %>% filter(country == "England") %>% dplyr::select(!!varname) %>% drop_na()
  EN <- nrow(gen.E)
  gen.E0 <- as.data.frame(table(gen.E[[1]]))
  gen.E1 <- as.data.frame(round(prop.table(table(gen.E[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.E2 <- inner_join(gen.E0, gen.E1, by = "Var1")
  gen.E2$England <- paste(format(gen.E2$Freq, big.mark=",", trim=TRUE), " (", # N
                          trimws(format(round(gen.E2$perc, 1), nsmall = 1)), "%)", sep = "") # %
  gen.E2 <- select(gen.E2, Var1, England)
  #  print(gen.E2)
  
  
  gen.W <- x %>% filter(country == "Wales") %>% dplyr::select(!!varname) %>% drop_na()
  WN <- nrow(gen.W)
  gen.W0 <- as.data.frame(table(gen.W[[1]]))
  gen.W1 <- as.data.frame(round(prop.table(table(gen.W[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.W2 <- inner_join(gen.W0, gen.W1, by = "Var1")
  gen.W2$Wales <- paste(format(gen.W2$Freq, big.mark=",", trim=TRUE), " (",
                        trimws(format(round(gen.W2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.W2 <- select(gen.W2, Var1, Wales)
  # print(gen.W2)
  
  gen.S <- x %>% filter(country == "Scotland") %>% dplyr::select(!!varname) %>% drop_na()
  SN <- nrow(gen.S)
  gen.S0 <- as.data.frame(table(gen.S[[1]]))
  gen.S1 <- as.data.frame(round(prop.table(table(gen.S[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.S2 <- inner_join(gen.S0, gen.S1, by = "Var1")
  gen.S2$Scotland <- paste(format(gen.S2$Freq, big.mark=",", trim=TRUE)," (",
                           trimws(format(round(gen.S2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.S2 <- select(gen.S2, Var1, Scotland)
  # print(gen.S2)
  
  gen.A <- x %>% dplyr::select(!!varname) %>% drop_na()
  AN <- nrow(gen.A)
  gen.A0 <- as.data.frame(table(gen.A[[1]]))
  gen.A1 <- as.data.frame(round(prop.table(table(gen.A[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.A2 <- inner_join(gen.A0, gen.A1, by = "Var1")
  gen.A2$All <- paste(format(gen.A2$Freq, big.mark=",", trim=TRUE), " (",
                      trimws(format(round(gen.A2$perc, 1), nsmall = 1)),  "%)", sep = "")
  gen.A2 <- select(gen.A2, Var1, All)
  # print(gen.A2)
  
  gen.table <- inner_join(gen.E2, gen.S2, by = "Var1") %>%
    inner_join(gen.W2, by = "Var1") %>% inner_join(gen.A2, by = "Var1")
  
  # Changed order to suit what they want. Need to change column names as well.  
  # gen.table <- inner_join(gen.E2, gen.S2, by = "Var1") %>% inner_join(gen.W2, by = "Var1") %>%
  #   inner_join(gen.A2, by = "Var1")
  
  
  colnames(gen.table) <- c(varname, 
                           paste("England (N=", format(EN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("Scotland (N=", format(SN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("Wales (N=", format(WN, big.mark=",", trim=TRUE), ")", sep = ""),
                           paste("All (N=", format(AN, big.mark=",", trim=TRUE), ")", sep = ""))
  
  
  
  # row.names(gen.table) <- gen.table$Var1
  
  return(gen.table)
}


library(chron)

# Read in the clean data from the analysis script:

dat1 <- readRDS(
"C:/Users/aadamson/Documents/Adult asthma/SC_outcomes_2018-19/Data/tidyData/linked_audit_HES_PEDW_ONS_data_AA_2018-19.RDS")


# dat1 is unfortunately missing lsoa so need to join it back in. 

bringbacklsoa <- read.csv("C:/Users/aadamson/Documents/Adult Asthma/Secondary Care Clinical 2018-2019/rawData/NACAP-AA-1811-1903-v101.csv")

bringbacklsoa %>% select(PatientID) %>% unique() %>% nrow()

bringbacklsoa <- bringbacklsoa %>% arrange(PatientID, X1.1a.Arrival.Date) %>% group_by(PatientID) %>% slice(1) %>% 
  select(PatientID, lsoa11) %>% ungroup() %>% rename(patient_ID = PatientID, LSOA = lsoa11)

colnames(dat1)

nrow(bringbacklsoa)

bringbacklsoa <- unique(bringbacklsoa)


dat1 <- left_join(dat1, bringbacklsoa, by = "patient_ID")


dat2 <- readRDS(
"C:/Users/aadamson/Documents/Adult asthma/SC_outcomes_2019-20/Data/tidyData/linked_audit_HES_PEDW_ONS_data_AA_2019-20.RDS")

dat2$LSOA <- as.character(dat2$LSOA)

dat1 <- dat1 %>% mutate(arrival_time = as.character(arrival_time),
                        PEF_init_time = as.character(PEF_init_time),
                        RSR_time = as.character(RSR_time),
                        steroids_admin_time = as.character(steroids_admin_time),
                        b2a_admin_time = as.character(b2a_admin_time),
                        discharge_time = as.character(discharge_time),
                        arrival_to_PEF_whole_days = as.numeric(arrival_to_PEF_whole_days)) # %>%
#  mutate(.vars = starts_with("arrival"), .funs = ~as.numeric)

dat <- bind_rows(dat1, dat2)                        

head(dat1$arrival_time)
str(dat1)
head(dat2$arrival_time)

sort(colnames(dat))
# here I'll have to bind_rows.

colnames(dat)

# we create a new variable for readmitted just for ease.

dat$readmitted30 <- "No"
dat$readmitted90 <- "No"

dat$readmitted30[dat$read30flag == "Readmitted"] <- "Yes"
dat$readmitted90[dat$read90flag == "Readmitted"] <- "Yes"

FreqSum(dat, "readmitted30")



# add in the new IMD data

new_IMD <- read.csv("C:/Users/aadamson/Documents/PS_AA/General UK data/IMD/2019_England_and_Wales_Income_Employment_IMD_clean.csv",
                    stringsAsFactors = FALSE)

colnames(new_IMD)

new_IMD <- new_IMD %>% rename(LSOA = LSOA_code_2011) 

new_IMD <- new_IMD %>% select(LSOA, new_IMD_quintile)

dat <- left_join(dat, new_IMD, by = "LSOA")

summary(dat$new_IMD_quintile)

dat %>% select(new_IMD_quintile, country) %>% table(useNA = "ifany")

dat$new_IMD_quintile[is.na(dat$new_IMD_quintile)] <- "Missing / Unavailable"

dat$new_IMD_quintile <- factor(dat$new_IMD_quintile)

summary(dat$new_IMD_quintile)

# we create a new variable for readmitted just for ease.

dat$readmitted30 <- "No"
dat$readmitted90 <- "No"

dat$readmitted30[dat$read30flag == "Readmitted"] <- "Yes"
dat$readmitted90[dat$read90flag == "Readmitted"] <- "Yes"

dat$readmitted30[dat$life_status != "Alive"] <- NA
dat$readmitted90[dat$life_status != "Alive"] <- NA

FreqSum(dat, "readmitted30")

# There should not be people who are aged <16...
dat <- dat %>% filter(age > 15)

# need to create some new columns...

dat %>% filter(LSOA == "") %>% nrow()

# summary(dat)
# 
# glm(died30 ~ news2 + age + gender, data = dat, family = binomial(link = "logit")) %>% summary()
# glm(read30 ~ news2 + age + gender, data = dat, family = binomial(link = "logit")) %>% summary()
# 
# glm(died90 ~ news2 + age + gender, data = dat, family = binomial(link = "logit")) %>% summary()
# glm(read90 ~ news2 + age + gender, data = dat, family = binomial(link = "logit")) %>% summary()
# 
# dat %>% select(contains("news")) %>% summary()

# Remove the hospadmissions column, because this is calculated by the audit data rather than the HES data
dat$hospadmissions <- NULL

# Don't forget to ensure that you are using the correct dataset for analyses - for some things you need to use all
# admissions, for others you need to only use the index admissions.

# Need to tally up total index admissions, total 30-day readmissions, and total 90-day readmissions.

dat.save <- dat


# # # # # # start of national level # # # # # # # # 

dat <- dat.save

dat_general <- data.frame(hosp_code = NA, hosp_name = NA, trust_code = NA, trust_name = NA,
                          region = NA, country = "All") 



dat$national <- "All"


dat <- dat %>% group_by(national) %>% add_tally(wt = indexadmission) %>% rename(hospindexadmissions = n) %>%
  add_tally(wt = read30) %>% rename(read30hospcount_reason_N = n) %>%
  add_tally(wt = read90) %>% rename(read90hospcount_reason_N = n) %>% ungroup()



# As a brief explanation: I'm groupng by hospital, keeping hospital 30 day admission number in the dataset
# because I'll need it later, filtering just those readmitted within 30 days, counting up all the reasons for 
# readmission (this is a 'summarise' function; from here all the single admissions go away), then we order the 
# dataset (within hospital) by the number of readmissions, then we just keep the top 5 (using slice), then use
# row_number so that we can write the top 5 most common diagnoses, and then we use the new 'pivot_wider' command
# that's been newly added to the tidy verse to transform the data from long to wide, and add percentages in for
# the number of diagnoses of a certain code divided by total number of 30 day readmissions.

dat30read <- dat %>%
  group_by(national, read30hospcount_reason_N) %>% filter(read30 == 1) %>% count(DIAG_01_conv) %>%
  rename(readmiss30diagcount = n) %>% arrange(national, desc(readmiss30diagcount)) %>% slice(1:5) %>%
  rename(readmiss30_diag_code = DIAG_01_conv) %>% mutate(readmissrank = paste0("highest_diag_", row_number())) %>%
  pivot_wider(id_cols = c(national, read30hospcount_reason_N), names_from = readmissrank,
              values_from = c(readmiss30_diag_code, readmiss30diagcount)) %>% ungroup() %>%
  mutate(readmissdiag30perc_highest_diag_1 = (readmiss30diagcount_highest_diag_1/read30hospcount_reason_N)*100,
         readmissdiag30perc_highest_diag_2 = (readmiss30diagcount_highest_diag_2/read30hospcount_reason_N)*100,
         readmissdiag30perc_highest_diag_3 = (readmiss30diagcount_highest_diag_3/read30hospcount_reason_N)*100,
         readmissdiag30perc_highest_diag_4 = (readmiss30diagcount_highest_diag_4/read30hospcount_reason_N)*100,
         readmissdiag30perc_highest_diag_5 = (readmiss30diagcount_highest_diag_5/read30hospcount_reason_N)*100) %>% ungroup()

# Then do the same for 90-day readmissions


dat90read <- dat %>%
  group_by(national, read90hospcount_reason_N) %>% filter(read90 == 1) %>% count(DIAG_01_conv) %>% 
  rename(readmiss90diagcount = n) %>% arrange(national, desc(readmiss90diagcount)) %>% slice(1:5) %>%
  rename(readmiss90_diag_code = DIAG_01_conv) %>%
  mutate(readmissrank = paste0("highest_diag_", row_number())) %>%
  pivot_wider(id_cols = c(national, read90hospcount_reason_N), names_from = readmissrank,
              values_from = c(readmiss90_diag_code, readmiss90diagcount)) %>% ungroup() %>%
  mutate(readmissdiag90perc_highest_diag_1 = (readmiss90diagcount_highest_diag_1/read90hospcount_reason_N)*100,
         readmissdiag90perc_highest_diag_2 = (readmiss90diagcount_highest_diag_2/read90hospcount_reason_N)*100,
         readmissdiag90perc_highest_diag_3 = (readmiss90diagcount_highest_diag_3/read90hospcount_reason_N)*100,
         readmissdiag90perc_highest_diag_4 = (readmiss90diagcount_highest_diag_4/read90hospcount_reason_N)*100,
         readmissdiag90perc_highest_diag_5 = (readmiss90diagcount_highest_diag_5/read90hospcount_reason_N)*100) %>% ungroup()


# Now, we don't need anyone else who wasn't an index admission, so we drop them


dat <- dat %>% filter(indexadmission == 1) 

# And now we add the died30/90 tallies.

dat <- dat %>% group_by(national) %>%
  add_tally(wt = died30) %>% rename(died30hospcount_reason_N = n) %>%
  add_tally(wt = died90) %>% rename(died90hospcount_reason_N = n) %>% ungroup()

# we also create the readmission tables
# quite difficult working with the '3+' variable so we recode it

dat$read30totalcat <- as.character(dat$read30totalcat)
dat$read30totalcat[dat$read30totalcat == "3+"] <- "3plus"
dat$read30totalcat <- factor(dat$read30totalcat, levels = c("0", "1", "2", "3plus"))

dat$read90totalcat <- as.character(dat$read90totalcat)
dat$read90totalcat[dat$read90totalcat == "3+"] <- "3plus"
dat$read90totalcat <- factor(dat$read90totalcat, levels = c("0", "1", "2", "3plus"))

# #
# Must add .drop = FALSE so that empty groups are still counted
# #
summary(dat$life_status)

dat30read2 <- dat %>% filter(life_status == "Alive") %>% group_by(national) %>% add_count() %>% 
  rename(readmission_denom_N = n) %>%
  group_by(national, readmission_denom_N) %>% count(read30totalcat, .drop = FALSE) %>%
  rename(read30groups = read30totalcat) %>%
  pivot_wider(id_cols = c(national, readmission_denom_N), names_from = read30groups,
              values_from = n, names_prefix = "read30no_cat_") %>% 
  rename_at(vars(starts_with("read30no_cat_")), funs(paste0(., "_n"))) %>% ungroup() %>%
  mutate(read30no_cat_0_perc = (read30no_cat_0_n/readmission_denom_N)*100,
         read30no_cat_1_perc = (read30no_cat_1_n/readmission_denom_N)*100,
         read30no_cat_2_perc = (read30no_cat_2_n/readmission_denom_N)*100,
         read30no_cat_3plus_perc = (read30no_cat_3plus_n/readmission_denom_N)*100)


dat90read2 <- dat %>% filter(life_status == "Alive") %>% group_by(national) %>% add_count() %>% 
  rename(readmission_denom_N = n) %>%
  group_by(national, readmission_denom_N) %>% count(read90totalcat, .drop = FALSE) %>%
  rename(read90groups = read90totalcat) %>%
  pivot_wider(id_cols = c(national, readmission_denom_N), names_from = read90groups,
              values_from = n, names_prefix = "read90no_cat_") %>% 
  rename_at(vars(starts_with("read90no_cat_")), funs(paste0(., "_n"))) %>% ungroup() %>%
  mutate(read90no_cat_0_perc = (read90no_cat_0_n/readmission_denom_N)*100,
         read90no_cat_1_perc = (read90no_cat_1_n/readmission_denom_N)*100,
         read90no_cat_2_perc = (read90no_cat_2_n/readmission_denom_N)*100,
         read90no_cat_3plus_perc = (read90no_cat_3plus_n/readmission_denom_N)*100)



# Now we go back to the mortality.

hospsumm <- dat %>% group_by(national) %>%
  summarise(mortality_denom_N = n(),
            died30_n = sum(died30),
            died30_perc = (died30_n/mortality_denom_N)*100,
            died90_n = sum(died90),
            died90_perc = (died90_n/mortality_denom_N)*100)


# Cause of mortality tables:

dat30died <- dat %>%
  group_by(national, died30hospcount_reason_N) %>% filter(died30 == 1) %>% count(CAUSE_OF_DEATH_conv) %>%
  rename(COD30diagcount = n) %>% arrange(national, desc(COD30diagcount)) %>% slice(1:5) %>%
  rename(COD30_diag_code = CAUSE_OF_DEATH_conv) %>% mutate(CODrank = paste0("highest_diag_", row_number())) %>%
  pivot_wider(id_cols = c(national, died30hospcount_reason_N), names_from = CODrank,
              values_from = c(COD30_diag_code, COD30diagcount)) %>% ungroup() %>%
  mutate(CODdiag30perc_highest_diag_1 = (COD30diagcount_highest_diag_1/died30hospcount_reason_N)*100,
         CODdiag30perc_highest_diag_2 = (COD30diagcount_highest_diag_2/died30hospcount_reason_N)*100,
         CODdiag30perc_highest_diag_3 = (COD30diagcount_highest_diag_3/died30hospcount_reason_N)*100,
         CODdiag30perc_highest_diag_4 = (COD30diagcount_highest_diag_4/died30hospcount_reason_N)*100,
         CODdiag30perc_highest_diag_5 = (COD30diagcount_highest_diag_5/died30hospcount_reason_N)*100) %>% ungroup()



dat90died <- dat %>%
  group_by(national, died90hospcount_reason_N) %>% filter(died90 == 1) %>% count(CAUSE_OF_DEATH_conv) %>%
  rename(COD90diagcount = n) %>% arrange(national, desc(COD90diagcount)) %>% slice(1:5) %>%
  rename(COD90_diag_code = CAUSE_OF_DEATH_conv) %>% mutate(CODrank = paste0("highest_diag_", row_number())) %>%
  pivot_wider(id_cols = c(national, died90hospcount_reason_N), names_from = CODrank,
              values_from = c(COD90_diag_code, COD90diagcount)) %>% ungroup() %>%
  mutate(CODdiag90perc_highest_diag_1 = (COD90diagcount_highest_diag_1/died90hospcount_reason_N)*100,
         CODdiag90perc_highest_diag_2 = (COD90diagcount_highest_diag_2/died90hospcount_reason_N)*100,
         CODdiag90perc_highest_diag_3 = (COD90diagcount_highest_diag_3/died90hospcount_reason_N)*100,
         CODdiag90perc_highest_diag_4 = (COD90diagcount_highest_diag_4/died90hospcount_reason_N)*100,
         CODdiag90perc_highest_diag_5 = (COD90diagcount_highest_diag_5/died90hospcount_reason_N)*100) %>% ungroup()




# And then we stitch them all together in order by national

all_dat <- left_join(hospsumm, dat30died, by = "national") %>% left_join(., dat90died, by = "national") %>%
  left_join(., dat30read2, by = "national") %>% left_join(., dat30read, by = "national") %>%
  left_join(., select(dat90read2, -readmission_denom_N), by = "national") %>%
  left_join(., dat90read, by = "national")



all_dat$died30hospcount_reason_N[is.na(all_dat$died30hospcount_reason_N)] <- 0
all_dat$died90hospcount_reason_N[is.na(all_dat$died90hospcount_reason_N)] <- 0
all_dat$read30hospcount_reason_N[is.na(all_dat$read30hospcount_reason_N)] <- 0
all_dat$read90hospcount_reason_N[is.na(all_dat$read90hospcount_reason_N)] <- 0

all_dat$national <- NULL
all_dat <- bind_cols(dat_general, all_dat)




# but we add the relevant variables that are associated 


all_dat <- bind_cols(all_dat,
                     FreqSum(dat, "readmitted30"),
                     FreqSum(dat, "readmitted90"),
          mediSumRound(dat, "age", 0),
          FreqSum(dat, "gender"), 
          FreqSum(dat, "IMD_quintile_all"),
          FreqSum(dat, "CCIweightedcat"),
)




all_dat_national <- all_dat
glimpse(all_dat_national)

# # # # # # End of national level # # # # # # # # 










# # # # # # start of country level # # # # # # # # 

dat <- dat.save

dat_general <- dat %>% group_by(country) %>% select(country) %>% unique() %>% 
  mutate(hosp_code = NA, hosp_name = NA, trust_code = NA, trust_name = NA, region = NA)
  

dat <- dat %>% group_by(country) %>% add_tally(wt = indexadmission) %>% rename(hospindexadmissions = n) %>%
  add_tally(wt = read30) %>% rename(read30hospcount_reason_N = n) %>%
  add_tally(wt = read90) %>% rename(read90hospcount_reason_N = n) %>% ungroup()



# As a brief explanation: I'm groupng by hospital, keeping hospital 30 day admission number in the dataset
# because I'll need it later, filtering just those readmitted within 30 days, counting up all the reasons for 
# readmission (this is a 'summarise' function; from here all the single admissions go away), then we order the 
# dataset (within hospital) by the number of readmissions, then we just keep the top 5 (using slice), then use
# row_number so that we can write the top 5 most common diagnoses, and then we use the new 'pivot_wider' command
# that's been newly added to the tidy verse to transform the data from long to wide, and add percentages in for
# the number of diagnoses of a certain code divided by total number of 30 day readmissions.

dat30read <- dat %>%
  group_by(country, read30hospcount_reason_N) %>% filter(read30 == 1) %>% count(DIAG_01_conv) %>%
  rename(readmiss30diagcount = n) %>% arrange(country, desc(readmiss30diagcount)) %>% slice(1:5) %>%
  rename(readmiss30_diag_code = DIAG_01_conv) %>% mutate(readmissrank = paste0("highest_diag_", row_number())) %>%
  pivot_wider(id_cols = c(country, read30hospcount_reason_N), names_from = readmissrank,
              values_from = c(readmiss30_diag_code, readmiss30diagcount)) %>% ungroup() %>%
  mutate(readmissdiag30perc_highest_diag_1 = (readmiss30diagcount_highest_diag_1/read30hospcount_reason_N)*100,
         readmissdiag30perc_highest_diag_2 = (readmiss30diagcount_highest_diag_2/read30hospcount_reason_N)*100,
         readmissdiag30perc_highest_diag_3 = (readmiss30diagcount_highest_diag_3/read30hospcount_reason_N)*100,
         readmissdiag30perc_highest_diag_4 = (readmiss30diagcount_highest_diag_4/read30hospcount_reason_N)*100,
         readmissdiag30perc_highest_diag_5 = (readmiss30diagcount_highest_diag_5/read30hospcount_reason_N)*100) %>% ungroup()

# Then do the same for 90-day readmissions


dat90read <- dat %>%
  group_by(country, read90hospcount_reason_N) %>% filter(read90 == 1) %>% count(DIAG_01_conv) %>% 
  rename(readmiss90diagcount = n) %>% arrange(country, desc(readmiss90diagcount)) %>% slice(1:5) %>%
  rename(readmiss90_diag_code = DIAG_01_conv) %>%
  mutate(readmissrank = paste0("highest_diag_", row_number())) %>%
  pivot_wider(id_cols = c(country, read90hospcount_reason_N), names_from = readmissrank,
              values_from = c(readmiss90_diag_code, readmiss90diagcount)) %>% ungroup() %>%
  mutate(readmissdiag90perc_highest_diag_1 = (readmiss90diagcount_highest_diag_1/read90hospcount_reason_N)*100,
         readmissdiag90perc_highest_diag_2 = (readmiss90diagcount_highest_diag_2/read90hospcount_reason_N)*100,
         readmissdiag90perc_highest_diag_3 = (readmiss90diagcount_highest_diag_3/read90hospcount_reason_N)*100,
         readmissdiag90perc_highest_diag_4 = (readmiss90diagcount_highest_diag_4/read90hospcount_reason_N)*100,
         readmissdiag90perc_highest_diag_5 = (readmiss90diagcount_highest_diag_5/read90hospcount_reason_N)*100) %>% ungroup()


# Now, we don't need anyone else who wasn't an index admission, so we drop them


dat <- dat %>% filter(indexadmission == 1) 

# And now we add the died30/90 tallies.

dat <- dat %>% group_by(country) %>%
  add_tally(wt = died30) %>% rename(died30hospcount_reason_N = n) %>%
  add_tally(wt = died90) %>% rename(died90hospcount_reason_N = n) %>% ungroup()

# we also create the readmission tables
# quite difficult working with the '3+' variable so we recode it

dat$read30totalcat <- as.character(dat$read30totalcat)
dat$read30totalcat[dat$read30totalcat == "3+"] <- "3plus"
dat$read30totalcat <- factor(dat$read30totalcat, levels = c("0", "1", "2", "3plus"))

dat$read90totalcat <- as.character(dat$read90totalcat)
dat$read90totalcat[dat$read90totalcat == "3+"] <- "3plus"
dat$read90totalcat <- factor(dat$read90totalcat, levels = c("0", "1", "2", "3plus"))

# #
# Must add .drop = FALSE so that empty groups are still counted
# #
summary(dat$life_status)

dat30read2 <- dat %>% filter(life_status == "Alive") %>% group_by(country) %>% add_count() %>% 
  rename(readmission_denom_N = n) %>%
  group_by(country, readmission_denom_N) %>% count(read30totalcat, .drop = FALSE) %>%
  rename(read30groups = read30totalcat) %>%
  pivot_wider(id_cols = c(country, readmission_denom_N), names_from = read30groups,
              values_from = n, names_prefix = "read30no_cat_") %>% 
  rename_at(vars(starts_with("read30no_cat_")), funs(paste0(., "_n"))) %>% ungroup() %>%
  mutate(read30no_cat_0_perc = (read30no_cat_0_n/readmission_denom_N)*100,
         read30no_cat_1_perc = (read30no_cat_1_n/readmission_denom_N)*100,
         read30no_cat_2_perc = (read30no_cat_2_n/readmission_denom_N)*100,
         read30no_cat_3plus_perc = (read30no_cat_3plus_n/readmission_denom_N)*100)


dat90read2 <- dat %>% filter(life_status == "Alive") %>% group_by(country) %>% add_count() %>% 
  rename(readmission_denom_N = n) %>%
  group_by(country, readmission_denom_N) %>% count(read90totalcat, .drop = FALSE) %>%
  rename(read90groups = read90totalcat) %>%
  pivot_wider(id_cols = c(country, readmission_denom_N), names_from = read90groups,
              values_from = n, names_prefix = "read90no_cat_") %>% 
  rename_at(vars(starts_with("read90no_cat_")), funs(paste0(., "_n"))) %>% ungroup() %>%
  mutate(read90no_cat_0_perc = (read90no_cat_0_n/readmission_denom_N)*100,
         read90no_cat_1_perc = (read90no_cat_1_n/readmission_denom_N)*100,
         read90no_cat_2_perc = (read90no_cat_2_n/readmission_denom_N)*100,
         read90no_cat_3plus_perc = (read90no_cat_3plus_n/readmission_denom_N)*100)



# Now we go back to the mortality.

hospsumm <- dat %>% group_by(country) %>%
  summarise(mortality_denom_N = n(),
            died30_n = sum(died30),
            died30_perc = (died30_n/mortality_denom_N)*100,
            died90_n = sum(died90),
            died90_perc = (died90_n/mortality_denom_N)*100)


# Cause of mortality tables:

dat30died <- dat %>%
  group_by(country, died30hospcount_reason_N) %>% filter(died30 == 1) %>% count(CAUSE_OF_DEATH_conv) %>%
  rename(COD30diagcount = n) %>% arrange(country, desc(COD30diagcount)) %>% slice(1:5) %>%
  rename(COD30_diag_code = CAUSE_OF_DEATH_conv) %>% mutate(CODrank = paste0("highest_diag_", row_number())) %>%
  pivot_wider(id_cols = c(country, died30hospcount_reason_N), names_from = CODrank,
              values_from = c(COD30_diag_code, COD30diagcount)) %>% ungroup() %>%
  mutate(CODdiag30perc_highest_diag_1 = (COD30diagcount_highest_diag_1/died30hospcount_reason_N)*100,
         CODdiag30perc_highest_diag_2 = (COD30diagcount_highest_diag_2/died30hospcount_reason_N)*100,
         CODdiag30perc_highest_diag_3 = (COD30diagcount_highest_diag_3/died30hospcount_reason_N)*100,
         CODdiag30perc_highest_diag_4 = (COD30diagcount_highest_diag_4/died30hospcount_reason_N)*100,
         CODdiag30perc_highest_diag_5 = (COD30diagcount_highest_diag_5/died30hospcount_reason_N)*100) %>% ungroup()



dat90died <- dat %>%
  group_by(country, died90hospcount_reason_N) %>% filter(died90 == 1) %>% count(CAUSE_OF_DEATH_conv) %>%
  rename(COD90diagcount = n) %>% arrange(country, desc(COD90diagcount)) %>% slice(1:5) %>%
  rename(COD90_diag_code = CAUSE_OF_DEATH_conv) %>% mutate(CODrank = paste0("highest_diag_", row_number())) %>%
  pivot_wider(id_cols = c(country, died90hospcount_reason_N), names_from = CODrank,
              values_from = c(COD90_diag_code, COD90diagcount)) %>% ungroup() %>%
  mutate(CODdiag90perc_highest_diag_1 = (COD90diagcount_highest_diag_1/died90hospcount_reason_N)*100,
         CODdiag90perc_highest_diag_2 = (COD90diagcount_highest_diag_2/died90hospcount_reason_N)*100,
         CODdiag90perc_highest_diag_3 = (COD90diagcount_highest_diag_3/died90hospcount_reason_N)*100,
         CODdiag90perc_highest_diag_4 = (COD90diagcount_highest_diag_4/died90hospcount_reason_N)*100,
         CODdiag90perc_highest_diag_5 = (COD90diagcount_highest_diag_5/died90hospcount_reason_N)*100) %>% ungroup()




# And then we stitch them all together in order by country

all_dat <- left_join(hospsumm, dat30died, by = "country") %>% left_join(., dat90died, by = "country") %>%
  left_join(., dat30read2, by = "country") %>% left_join(., dat30read, by = "country") %>%
  left_join(., select(dat90read2, -readmission_denom_N), by = "country") %>%
  left_join(., dat90read, by = "country")



all_dat$died30hospcount_reason_N[is.na(all_dat$died30hospcount_reason_N)] <- 0
all_dat$died90hospcount_reason_N[is.na(all_dat$died90hospcount_reason_N)] <- 0
all_dat$read30hospcount_reason_N[is.na(all_dat$read30hospcount_reason_N)] <- 0
all_dat$read90hospcount_reason_N[is.na(all_dat$read90hospcount_reason_N)] <- 0

all_dat <- left_join(all_dat, dat_general, by = "country") 





# but we add the relevant variables that are associated 


# I guess because of the way we've done this before, I should just use a loop...

empty <- bind_cols(FreqSum(dat, "readmitted30"),
                   FreqSum(dat, "readmitted90"),
                   mediSumRound(dat, "age", 0),
                   FreqSum(dat, "gender"), 
                   FreqSum(dat, "IMD_quintile_all"),
                   FreqSum(dat, "CCIweightedcat"))
empty <- empty[-1, ]


for (i in unique(dat$country)) {
  
  dat_filt <- dat %>% filter(country == i)
  additional <- bind_cols(FreqSum(dat_filt, "readmitted30"),
                          FreqSum(dat_filt, "readmitted90"),
                          mediSumRound(dat_filt, "age", 0),
                          FreqSum(dat_filt, "gender"), 
                          FreqSum(dat_filt, "IMD_quintile_all"),
                          FreqSum(dat_filt, "CCIweightedcat"))
  empty <- bind_rows(empty, additional)
  
}

full <- empty
full



all_dat <- bind_cols(all_dat, full)



all_dat_country <- all_dat
glimpse(all_dat_country)



# # # # # # End of country level # # # # # # # # 





# # # # # # start of hospital level # # # # # # # # 

dat <- dat.save

colnames(dat)


dat_general <- dat %>% group_by(hosp_code) %>% slice(1) %>% select(hosp_code, hosp_name, trust_code, trust_name)

dat <- dat %>% group_by(hosp_code) %>% add_tally(wt = indexadmission) %>% rename(hospindexadmissions = n) %>%
                               add_tally(wt = read30) %>% rename(read30hospcount_reason_N = n) %>%
                               add_tally(wt = read90) %>% rename(read90hospcount_reason_N = n) %>% ungroup()



# As a brief explanation: I'm groupng by hospital, keeping hospital 30 day admission number in the dataset
# because I'll need it later, filtering just those readmitted within 30 days, counting up all the reasons for 
# readmission (this is a 'summarise' function; from here all the single admissions go away), then we order the 
# dataset (within hospital) by the number of readmissions, then we just keep the top 5 (using slice), then use
# row_number so that we can write the top 5 most common diagnoses, and then we use the new 'pivot_wider' command
# that's been newly added to the tidy verse to transform the data from long to wide, and add percentages in for
# the number of diagnoses of a certain code divided by total number of 30 day readmissions.

dat30read <- dat %>%
  group_by(hosp_code, read30hospcount_reason_N) %>% filter(read30 == 1) %>% count(DIAG_01_conv) %>%
  rename(readmiss30diagcount = n) %>% arrange(hosp_code, desc(readmiss30diagcount)) %>% slice(1:5) %>%
  rename(readmiss30_diag_code = DIAG_01_conv) %>% mutate(readmissrank = paste0("highest_diag_", row_number())) %>%
  pivot_wider(id_cols = c(hosp_code, read30hospcount_reason_N), names_from = readmissrank,
              values_from = c(readmiss30_diag_code, readmiss30diagcount)) %>% ungroup() %>%
  mutate(readmissdiag30perc_highest_diag_1 = (readmiss30diagcount_highest_diag_1/read30hospcount_reason_N)*100,
         readmissdiag30perc_highest_diag_2 = (readmiss30diagcount_highest_diag_2/read30hospcount_reason_N)*100,
         readmissdiag30perc_highest_diag_3 = (readmiss30diagcount_highest_diag_3/read30hospcount_reason_N)*100,
         readmissdiag30perc_highest_diag_4 = (readmiss30diagcount_highest_diag_4/read30hospcount_reason_N)*100,
         readmissdiag30perc_highest_diag_5 = (readmiss30diagcount_highest_diag_5/read30hospcount_reason_N)*100) %>% ungroup()

# Then do the same for 90-day readmissions


dat90read <- dat %>%
  group_by(hosp_code, read90hospcount_reason_N) %>% filter(read90 == 1) %>% count(DIAG_01_conv) %>% 
  rename(readmiss90diagcount = n) %>% arrange(hosp_code, desc(readmiss90diagcount)) %>% slice(1:5) %>%
  rename(readmiss90_diag_code = DIAG_01_conv) %>%
  mutate(readmissrank = paste0("highest_diag_", row_number())) %>%
  pivot_wider(id_cols = c(hosp_code, read90hospcount_reason_N), names_from = readmissrank,
              values_from = c(readmiss90_diag_code, readmiss90diagcount)) %>% ungroup() %>%
  mutate(readmissdiag90perc_highest_diag_1 = (readmiss90diagcount_highest_diag_1/read90hospcount_reason_N)*100,
         readmissdiag90perc_highest_diag_2 = (readmiss90diagcount_highest_diag_2/read90hospcount_reason_N)*100,
         readmissdiag90perc_highest_diag_3 = (readmiss90diagcount_highest_diag_3/read90hospcount_reason_N)*100,
         readmissdiag90perc_highest_diag_4 = (readmiss90diagcount_highest_diag_4/read90hospcount_reason_N)*100,
         readmissdiag90perc_highest_diag_5 = (readmiss90diagcount_highest_diag_5/read90hospcount_reason_N)*100) %>% ungroup()


# Now, we don't need anyone else who wasn't an index admission, so we drop them


dat <- dat %>% filter(indexadmission == 1) 

# And now we add the died30/90 tallies.

dat <- dat %>% group_by(hosp_code) %>%
  add_tally(wt = died30) %>% rename(died30hospcount_reason_N = n) %>%
  add_tally(wt = died90) %>% rename(died90hospcount_reason_N = n) %>% ungroup()

# we also create the readmission tables
# quite difficult working with the '3+' variable so we recode it

dat$read30totalcat <- as.character(dat$read30totalcat)
dat$read30totalcat[dat$read30totalcat == "3+"] <- "3plus"
dat$read30totalcat <- factor(dat$read30totalcat, levels = c("0", "1", "2", "3plus"))

dat$read90totalcat <- as.character(dat$read90totalcat)
dat$read90totalcat[dat$read90totalcat == "3+"] <- "3plus"
dat$read90totalcat <- factor(dat$read90totalcat, levels = c("0", "1", "2", "3plus"))

# #
# Must add .drop = FALSE so that empty groups are still counted
# #
summary(dat$life_status)

dat30read2 <- dat %>% filter(life_status == "Alive") %>% group_by(hosp_code) %>% add_count() %>% 
  rename(readmission_denom_N = n) %>%
  group_by(hosp_code, readmission_denom_N) %>% count(read30totalcat, .drop = FALSE) %>%
  rename(read30groups = read30totalcat) %>%
  pivot_wider(id_cols = c(hosp_code, readmission_denom_N), names_from = read30groups,
              values_from = n, names_prefix = "read30no_cat_") %>% 
  rename_at(vars(starts_with("read30no_cat_")), funs(paste0(., "_n"))) %>% ungroup() %>%
  mutate(read30no_cat_0_perc = (read30no_cat_0_n/readmission_denom_N)*100,
         read30no_cat_1_perc = (read30no_cat_1_n/readmission_denom_N)*100,
         read30no_cat_2_perc = (read30no_cat_2_n/readmission_denom_N)*100,
         read30no_cat_3plus_perc = (read30no_cat_3plus_n/readmission_denom_N)*100)


dat90read2 <- dat %>% filter(life_status == "Alive") %>% group_by(hosp_code) %>% add_count() %>% 
  rename(readmission_denom_N = n) %>%
  group_by(hosp_code, readmission_denom_N) %>% count(read90totalcat, .drop = FALSE) %>%
  rename(read90groups = read90totalcat) %>%
  pivot_wider(id_cols = c(hosp_code, readmission_denom_N), names_from = read90groups,
              values_from = n, names_prefix = "read90no_cat_") %>% 
  rename_at(vars(starts_with("read90no_cat_")), funs(paste0(., "_n"))) %>% ungroup() %>%
  mutate(read90no_cat_0_perc = (read90no_cat_0_n/readmission_denom_N)*100,
         read90no_cat_1_perc = (read90no_cat_1_n/readmission_denom_N)*100,
         read90no_cat_2_perc = (read90no_cat_2_n/readmission_denom_N)*100,
         read90no_cat_3plus_perc = (read90no_cat_3plus_n/readmission_denom_N)*100)



# Now we go back to the mortality.

hospsumm <- dat %>% group_by(hosp_code) %>%
  summarise(mortality_denom_N = n(),
            died30_n = sum(died30),
            died30_perc = (died30_n/mortality_denom_N)*100,
            died90_n = sum(died90),
            died90_perc = (died90_n/mortality_denom_N)*100)


# Cause of mortality tables:
            
dat30died <- dat %>%
  group_by(hosp_code, died30hospcount_reason_N) %>% filter(died30 == 1) %>% count(CAUSE_OF_DEATH_conv) %>%
  rename(COD30diagcount = n) %>% arrange(hosp_code, desc(COD30diagcount)) %>% slice(1:3) %>%
  rename(COD30_diag_code = CAUSE_OF_DEATH_conv) %>% mutate(CODrank = paste0("highest_diag_", row_number())) %>% 
  # ungroup() %>% select(COD30_diag_code)  %>% table()
  pivot_wider(id_cols = c(hosp_code, died30hospcount_reason_N), names_from = CODrank,
              values_from = c(COD30_diag_code, COD30diagcount)) %>% ungroup() %>% # View() # %>%
  mutate(CODdiag30perc_highest_diag_1 = (COD30diagcount_highest_diag_1/died30hospcount_reason_N)*100,
         CODdiag30perc_highest_diag_2 = (COD30diagcount_highest_diag_2/died30hospcount_reason_N)*100,
         CODdiag30perc_highest_diag_3 = (COD30diagcount_highest_diag_3/died30hospcount_reason_N)*100,
         CODdiag30perc_highest_diag_4 = ifelse(!is.null(.$COD30diagcount_highest_diag_4), (COD30diagcount_highest_diag_4/died30hospcount_reason_N)*100, NA),
         CODdiag30perc_highest_diag_5 = ifelse(!is.null(.$COD30diagcount_highest_diag_5), (COD30diagcount_highest_diag_5/died30hospcount_reason_N)*100, NA)) %>% ungroup()

glimpse(dat30died)


dat90died <- dat %>%
  group_by(hosp_code, died90hospcount_reason_N) %>% filter(died90 == 1) %>% count(CAUSE_OF_DEATH_conv) %>%
  rename(COD90diagcount = n) %>% arrange(hosp_code, desc(COD90diagcount)) %>% slice(1:5) %>%
  rename(COD90_diag_code = CAUSE_OF_DEATH_conv) %>% mutate(CODrank = paste0("highest_diag_", row_number())) %>%
  pivot_wider(id_cols = c(hosp_code, died90hospcount_reason_N), names_from = CODrank,
              values_from = c(COD90_diag_code, COD90diagcount)) %>% ungroup() %>%
  mutate(CODdiag90perc_highest_diag_1 = (COD90diagcount_highest_diag_1/died90hospcount_reason_N)*100,
         CODdiag90perc_highest_diag_2 = (COD90diagcount_highest_diag_2/died90hospcount_reason_N)*100,
         CODdiag90perc_highest_diag_3 = (COD90diagcount_highest_diag_3/died90hospcount_reason_N)*100,
         CODdiag90perc_highest_diag_4 = ifelse(!is.null(COD90diagcount_highest_diag_4), (COD90diagcount_highest_diag_4/died90hospcount_reason_N)*100, NA),
         CODdiag90perc_highest_diag_5 = ifelse(!is.null(COD90diagcount_highest_diag_5), (COD90diagcount_highest_diag_5/died90hospcount_reason_N)*100, NA)) %>% ungroup()

glimpse(dat90died)

# And then we stitch them all together in order by hospital

all_dat <- left_join(hospsumm, dat30died, by = "hosp_code") %>% left_join(., dat90died, by = "hosp_code") %>%
           left_join(., dat30read2, by = "hosp_code") %>% left_join(., dat30read, by = "hosp_code") %>%
           left_join(., select(dat90read2, -readmission_denom_N), by = "hosp_code") %>%
           left_join(., dat90read, by = "hosp_code")



all_dat$died30hospcount_reason_N[is.na(all_dat$died30hospcount_reason_N)] <- 0
all_dat$died90hospcount_reason_N[is.na(all_dat$died90hospcount_reason_N)] <- 0
all_dat$read30hospcount_reason_N[is.na(all_dat$read30hospcount_reason_N)] <- 0
all_dat$read90hospcount_reason_N[is.na(all_dat$read90hospcount_reason_N)] <- 0

all_dat <- left_join(all_dat, dat_general, by = "hosp_code")




# but we add the relevant variables that are associated 


# I guess because of the way we've done this before, I should just use a loop...

empty <- bind_cols(FreqSum(dat, "readmitted30"),
                   FreqSum(dat, "readmitted90"),
                   mediSumRound(dat, "age", 0),
                   FreqSum(dat, "gender"), 
                   FreqSum(dat, "IMD_quintile_all"),
                   FreqSum(dat, "CCIweightedcat"))
empty <- empty[-1, ]


for (i in unique(dat$hosp_code)) {
  
  dat_filt <- dat %>% filter(hosp_code == i)
  additional <- bind_cols(FreqSum(dat_filt, "readmitted30"),
                          FreqSum(dat_filt, "readmitted90"),
                          mediSumRound(dat_filt, "age", 0),
                          FreqSum(dat_filt, "gender"), 
                          FreqSum(dat_filt, "IMD_quintile_all"),
                          FreqSum(dat_filt, "CCIweightedcat"))
  empty <- bind_rows(empty, additional)
  
}

full <- empty
full



all_dat <- bind_cols(all_dat, full)



all_dat_hosp <- all_dat
glimpse(all_dat_hosp)



# # # # # # End of hospital level # # # # # # # # 

# Now bind them all together.


all_dat <- bind_rows(all_dat_national, all_dat_country, all_dat_hosp)

glimpse(all_dat)
# all_dat$national <- coalesce(all_dat$national, all_dat$country, all_dat$hospital)

# all_dat <- all_dat %>% select(-country, -hospital) %>% rename(hospital = national)

# New bit of code - will it work?
# Worked when I tested it...

 
all_dat <- all_dat %>% mutate_at(vars(contains('perc'), -contains('gender'), -contains('IMD_quintile_all'), -contains('CCIweightedcat'),
                                      -contains('readmitted')), 
                                 ~ sprintf("%.1f", round(., 1), nsmall = 1))


glimpse(all_dat)

# Now we save it

nrow(dat)

# Just keep the columns we actually want.

all_dat <- all_dat %>% select(-ends_with("_4"), -ends_with("5"), -starts_with("read90no"), 
                              -starts_with("read30no")) 


glimpse(all_dat)


# write.csv(all_dat, "C:/Users/aadamson/Documents/Adult asthma/SC_outcomes_2018-19/Data/dataStore/AA_SC_outcomes_2018-20_data.csv",
#        row.names = FALSE)



# Done!


# For died, we have less people, so we need to do some things differently.



table(dat$gender, dat$age)


dat <- dat %>% filter(gender %in% c("Male", "Female"))

dat$gender <- factor(dat$gender, levels = c("Male", "Female"))

dat$agecen <- dat$age - 50
dat$agecen2 <- dat$agecen^2


dat$age2 <- dat$age^2
dat$CCIweighted2 <- dat$CCIweighted^2

dat$CCIweightedcen <- dat$CCIweighted - 1


table(dat$life_status)

datalive <- dat %>% filter(life_status != "Died as inpatient")



summary(dat$agecat)
# 
# tidyoutput <- function(x) {
#   tidee <- exp(cbind(summary(x)$coefficients[ ,1], confint(x, parm="beta_", method = "Wald")))
#   colnames(tidee) <- c("est", "lo", "hi")
#   tidee <- round(tidee, 2)
#   return(tidee)
# }


dat %>% filter(is.na(agecat)) %>% select(age)
summary(dat$agecat)

dat$agecat <- relevel(dat$agecat, ref = "25-34")
datalive$agecat <- relevel(datalive$agecat, ref = "25-34")





nlc("For died within 30 days, adjusted analysis:")

# m9 <- glmer(died30flag ~ 1 + gender + IMD.quintile + agecat + CCIweightedcat + longstayHES + 
#               NIVgiven + (1 | hospital), family=binomial(link = "logit"), data = dat,
#             glmerControl(optimizer = "Nelder_Mead"))




# For died from asthma, we do things  bit differently. 
# We use just the English data, and include age and CCI as quadratics. 

datfordiedmodel <- dat %>% filter(gender %in% c("Male", "Female"))

summary(datfordiedmodel$new_IMD_quintile)


dat$CCIweightedfac <- factor(dat$CCIweighted)
lintestOR(dat, "died30flag", "CCIweightedfac")


# died30asthma_mod <- glmer(died30flag ~ 1 + gender + IMD_quintile_Eng + agecen + agecen2 + CCIweighted + CCIweighted2 +  
#                             (1 | hosp_code), family=binomial(link = "logit"), data = datfordiedmodel,
#                           glmerControl(optimizer = "Nelder_Mead"))
# v large eigenvalue, didn't converge.


mean(dat$CCIweighted)



# died30asthma_mod <- glmer(died30flag ~ 1 + gender + IMD_quintile_Eng + agecen + agecen2 + CCIweighted +  
#               (1 | hosp_code), family=binomial(link = "logit"), data = datfordiedmodel,
#             glmerControl(optimizer = "Nelder_Mead"))
# difficulty converging


died30asthma_mod <- glmer(died30flag ~ 1 + gender + new_IMD_quintile + agecen + agecen2 + CCIweighted + asthma_sev +
              (1 | hosp_code), family=binomial(link = "logit"), data = datfordiedmodel,
            glmerControl(optimizer = "Nelder_Mead"))


died30asthma_mod_fixed <- glm(died30flag ~ 1 + gender + new_IMD_quintile + agecen + agecen2 + CCIweighted + asthma_sev,
                                family=binomial(link = "logit"), data = datfordiedmodel)
                          
summary(died30asthma_mod_fixed)

saveRDS(died30asthma_mod, "C:/Users/aadamson/Documents/Adult asthma/SC_outcomes_2018-19/Data/tidyData/died30asthma_mod.RDS")

# checked the accuracy of the results, below, all seems legit, now hashed out.

# ss <- getME(died30asthma_mod, c("theta", "fixef"))
# died30asthma_mod_update1 <- update(died30asthma_mod, start = ss, control = glmerControl(optCtrl = list(maxfun = 2e5)))
# ss1 <- getME(died30asthma_mod_update1, c("theta", "fixef"))
# died30asthma_mod_update2 <- update(died30asthma_mod_update1, start = ss1, control = glmerControl(optCtrl = list(maxfun = 2e5)))
# ss2 <- getME(died30asthma_mod_update2, c("theta", "fixef"))
# died30asthma_mod_update3 <- update(died30asthma_mod_update2, start = ss2, control = glmerControl(optCtrl = list(maxfun = 2e5)))
# 
# died30asthma_mod_fixed <- glm(died30flag ~ 1 + gender + IMD_quintile_Eng + agecen + agecen2 + CCIweightedcen,
#                              family=binomial(link = "logit"), data = datfordiedmodel)
# 
# tidyoutput
# 
# died30asthma_mod %>% tidyoutput(meth = "Wald")
# died30asthma_mod_fixed %>% tidyoutput(meth = "Wald", MEM = FALSE)
# 
# died90asthma_mod_fixed <- glm(died90flag ~ 1 + gender + IMD_quintile_Eng + agecen + agecen2 + CCIweightedcen,
#                               family=binomial(link = "logit"), data = datfordiedmodel)
# 
# died90asthma_mod %>% tidyoutput(meth = "Wald")
# died90asthma_mod_fixed %>% tidyoutput(meth = "Wald", MEM = FALSE)
# 
# 
# summary(died30asthma_mod)

# try and update it

# ss <- getME(test, c("theta", "fixef"))
# test_update <- update(test, start = ss, control = glmerControl(optCtrl = list(maxfun = 2e4)))
# 
# summary(test_update)
# summary(test)
# summary(died30asthma_mod)
# 
# test2 <- glm(died30flag ~ 1 + gender + new_IMD_quintile + agecat + CCIweightedcat, family=binomial(link = "logit"), data = filter(dat, gender == "Male" | gender == "Female"),
#               glmerControl(optimizer = "Nelder_Mead"))



died90asthma_mod <- glmer(died90flag ~ 1 + gender + new_IMD_quintile + agecen + agecen2 + CCIweighted + asthma_sev +
                            (1 | hosp_code), family=binomial(link = "logit"), data = datfordiedmodel,
                        glmerControl(optimizer = "Nelder_Mead"))

Sys.time()

summary(died90asthma_mod)

died90asthma_mod_fixed <- glm(died90flag ~ 1 + gender + new_IMD_quintile + agecen + agecen2 + CCIweighted + asthma_sev,
                              family=binomial(link = "logit"), data = datfordiedmodel)


saveRDS(died90asthma_mod, "C:/Users/aadamson/Documents/Adult asthma/SC_outcomes_2018-19/Data/tidyData/died90asthma_mod.RDS")

summary(as.numeric(dat$LOS))
read30asthma_mod <- glmer(read30flag ~ 1 + gender + new_IMD_quintile + agecat + CCIweightedcat + asthma_sev + LOS +
                          (1 | hosp_code), family=binomial(link = "logit"), data = datalive,
                        glmerControl(optimizer = "Nelder_Mead"))

Sys.time()

read30asthma_mod_fixed <- glm(read30flag ~ 1 + gender + new_IMD_quintile + agecat + CCIweightedcat + asthma_sev + LOS,
                              family=binomial(link = "logit"), data = datalive)

# left_join((read30asthma_mod %>% tidyoutput(meth = "Wald")), (read30asthma_mod_fixed %>% tidyoutput(MEM = FALSE, meth = "Wald")), by = "Variable")

saveRDS(read30asthma_mod, "C:/Users/aadamson/Documents/Adult asthma/SC_outcomes_2018-19/Data/tidyData/read30asthma_mod.RDS")


read90asthma_mod <- glmer(read90flag ~ 1 + gender + new_IMD_quintile + agecat + CCIweightedcat + asthma_sev + LOS +
                          (1 | hosp_code), family=binomial(link = "logit"), data = datalive,
                        glmerControl(optimizer = "Nelder_Mead"))

Sys.time()


saveRDS(read90asthma_mod, "C:/Users/aadamson/Documents/Adult asthma/SC_outcomes_2018-19/Data/tidyData/read90asthma_mod.RDS")

Sys.time()


read90asthma_mod_fixed <- glm(read90flag ~ 1 + gender + new_IMD_quintile + agecat + CCIweightedcat + asthma_sev,
                              family=binomial(link = "logit"), data = datalive)



library(dplyr)
library(tidyr)
library(tibble)


furtherCleaning <- function(x) {
  x$est <- sprintf("%.2f", x$est)
  x$lo <- sprintf("%.2f", x$lo)
  x$hi <- sprintf("%.2f", x$hi)
  
  x$Estimate <- paste0(x$est, " (", x$lo, " to ", x$hi, ")")
  
  x <- x %>% dplyr::select(Variable, Estimate)
  
  return(x)
}





di30asthma_fixed <- tidyoutput(died30asthma_mod_fixed, meth = "Wald", MEM = FALSE) %>% furtherCleaning() %>% rename(`Estimates: Died in 30 days` = Estimate)
di90asthma_fixed <- tidyoutput(died90asthma_mod_fixed, meth = "Wald", MEM = FALSE) %>% furtherCleaning() %>% rename(`Estimates: Died in 90 days` = Estimate)
re30asthma_fixed <- tidyoutput(read30asthma_mod_fixed, meth = "Wald", MEM = FALSE) %>% furtherCleaning() %>% rename(`Estimates: Readmitted in 30 days` = Estimate)
re90asthma_fixed <- tidyoutput(read90asthma_mod_fixed, meth = "Wald", MEM = FALSE) %>% furtherCleaning() %>% rename(`Estimates: Readmitted in 90 days` = Estimate)

rownames_to_column

di30asthma <- tidyoutput(died30asthma_mod, meth = "Wald") %>% furtherCleaning() %>% rename(`Estimates: Died in 30 days` = Estimate)
di90asthma <- tidyoutput(died90asthma_mod, meth = "Wald") %>% furtherCleaning() %>% rename(`Estimates: Died in 90 days` = Estimate)
re30asthma <- tidyoutput(read30asthma_mod, meth = "Wald") %>% furtherCleaning() %>% rename(`Estimates: Readmitted in 30 days` = Estimate)
re90asthma <- tidyoutput(read90asthma_mod, meth = "Wald") %>% furtherCleaning() %>% rename(`Estimates: Readmitted in 90 days` = Estimate)


di30asthma
di90asthma
re30asthma
re90asthma

all <- di30 %>% full_join(., di90, by = "Variable") %>% full_join(., re30, by = "Variable") %>% full_join(., re90, by = "Variable") 

all <- all %>% filter(!(Variable %in% c("(Intercept)", "genderTransgender", "genderOther", "genderNot recorded/Preferred not to say",
                                        "new_IMD_quintileMissing")))
                                        
all

# write.csv(all, "C:/Users/aadamson/Documents/Adult Asthma/SC_outcomes_2018-19/Data/dataStore/asthma_regression_results_v2.csv")

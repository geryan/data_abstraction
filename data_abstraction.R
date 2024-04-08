abundance <- read.csv("D:/THE VECTOR ATLAS/DATA ABSTRACTION/VA DATA ABSTRACTION/FINAL - CHECKED AND UPDATED DATA (MAP)_OCC AND BIO COMBINED/final_species.csv")

### borrowing code from Gerry to define third  condition below ###

##IF n_tot>0, THEN 'occurrence' = y
##IF n_tot==0, THEN 'occurrence'=n
##IF ANY sampling_n = FILL AND corresponding n_n = NA, THEN occurrence =y
##IF ANY sampling_n = FILL and n_n = 0 and there are no more sampling_n, then occurrence = n


### filling in abundance may be misleading so only adding one
### occurrence/absence column ###

## add in the occurrence/absence column and fill from zeros
abundance.calc <- abundance %>%
  mutate(occur=case_when (
    n_tot>0~'y',
    n_tot==0~'n'))

####### FOLLOWING CODE FROM GERRY  #####

abundance.calc <- abundance.calc %>%
  mutate((if_else(n_1 == "NA", NA, .)
  )) # this is converting all of the non-blank cells with "NA" written in them to actual R NAs.
##[this throws an error and I've not been able to correct it]

abundance.calc <- abundance.calc %>%
  mutate(across(
    starts_with("n_"),
    as.numeric
    )
  ) %>%
  rowwise %>% # NB this rowwise is necessary for the below `any` to work by row, but may be slow on a very large dataset
  mutate(
    any_sm_na_count = any(
      !is.na(sampling.method_1) & is.na(n_1),
      !is.na(sampling.method_2) & is.na(n_2),
      !is.na(sampling.method_3) & is.na(n_3),
      !is.na(sampling.method_4) & is.na(n_4)
    )) # this checks if there are any non-empty sampling methods with a NA count
## [this doesn't account for zeros in the n_# columns]

abundance.calc <- abundance.calc %>%
  relocate(any_sm_na_count, .after = n_tot)

abund.clip <- select(abundance.calc, sampling.method_1, n_1,sampling.method_2, n_2,sampling.method_3, n_3,sampling.method_4, n_4,n_tot, occur, any_sm_na_count)

write.csv(abund.clip, "D:/THE VECTOR ATLAS/DATA ABSTRACTION/VA DATA ABSTRACTION/FINAL - CHECKED AND UPDATED DATA (MAP)_OCC AND BIO COMBINED/abundance_calc_clip.csv")



###### GERRY 20240123
library(readr)
#abundance <- read_csv("final_species.csv")


library(dplyr)
library(readxl)
abundance <- read_xlsx(
  "abundance_calc_clip_marked.xlsx",
  na = "NA" # this is needed to convert things entered as "NA" to NA in R (I did it differently in before but this is better)
)

occurence <- abundance %>%
  rowwise %>%
  mutate(
    any_sm_na_count = any(
      !is.na(sampling.method_1) & is.na(n_1),
      !is.na(sampling.method_2) & is.na(n_2),
      !is.na(sampling.method_3) & is.na(n_3),
      !is.na(sampling.method_4) & is.na(n_4)
    ),
    n_tot = case_when(
      any_sm_na_count ~ NA, # assign NA n_tot if there is a non-empty sampling method that is NA
      TRUE ~ sum(c_across(starts_with("n_")), na.rm = TRUE) # otherwise sum up the values ignoring NAs
    ),
    occur = case_when(
      !any_sm_na_count & n_tot == 0 ~ "n",
      !any_sm_na_count & n_tot > 0 ~ "y",
      TRUE ~ NA # I'm not sure here if you want all non-true absences to be NA as here, or "y", if "y" then remove this line and un-comment the following
      #TRUE ~ "y"
    )
  )


occurence[c(1:10, 59, 78, 94),]


##
library(dplyr)
library(readr)
abun <- read_csv("final_species.csv", na = c("NA", "")) %>%
  select(
    `...1`,
    sampling.method_1,
    n_1,
    sampling.method_2,
    n_2,
    sampling.method_3,
    n_3,
    sampling.method_4,n_4
  )

occ <- abun %>%
  rowwise %>%
  mutate(
    any_sm_na_count = any(
      !is.na(sampling.method_1) & is.na(n_1),
      !is.na(sampling.method_2) & is.na(n_2),
      !is.na(sampling.method_3) & is.na(n_3),
      !is.na(sampling.method_4) & is.na(n_4)
    ),
    n_tot = case_when(
      any_sm_na_count ~ NA, # assign NA n_tot if there is a non-empty sampling method that is NA
      TRUE ~ sum(c_across(starts_with("n_")), na.rm = TRUE) # otherwise sum up the values ignoring NAs
    ),
    occur = case_when(
      !any_sm_na_count & n_tot == 0 ~ "n",
      !any_sm_na_count & n_tot > 0 ~ "y",
      TRUE ~ NA # I'm not sure here if you want all non-true absences to be NA as here, or "y", if "y" then remove this line and un-comment the following
      #TRUE ~ "y"
    )
  )


occ[c(1:10, 59, 78, 94),]


library(readxl)
library(dplyr)

dat <- read_xlsx(
  path = "bit of data for eg.xlsx",
  sheet = "Sheet1"
)


dat %>%
  select(-starts_with("month"), -starts_with("year"), -species) %>% # this is just to tidy the data and get rid of the columns we're not looking at here, but will want to delete this if applying to the real spreadsheet
  mutate(
    across(
      starts_with("n_"),
      ~if_else(. == "NA", NA, .)
    ), # this is converting all of the non-blank cells with "NA" written in them to actual R NAs.
    across(
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
    ) # this checks if there are any non-empty sampling methods with a NA count
  ) %>%
  rename(entered_n_tot = n_tot) %>% # renaming becase want to keep for checking but will get double-counted by the sum(c_across(...)) below if left with a name beginning "n_"
  mutate(
    n_tot = case_when(
      any_sm_na_count ~ NA, # assign NA n_tot if there is a non-empty sampling method that is NA
      TRUE ~ sum(c_across(starts_with("n_")), na.rm = TRUE) # otherwise sum up the values ignoring NAs
    )
  ) %>%
  relocate(entered_n_tot, .after = n_tot) %>% print(n = 32) # tidying to see the calculated and original values together.
 
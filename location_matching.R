library(dplyr)
library(readr)
library(tidyr)


raw_data <- read_csv(
  file = "2024-03-14 - VA old but updated data for cleaning/final_species.csv",
  guess_max = 30000
)


old_db <- read_csv(
  file = "2024-03-14 - VA old but updated data for cleaning/OLD MAP database dump/20210726_vector_extraction (1).csv"
)

merge_data <- read_csv(
  file = "2024-03-14 - VA old but updated data for cleaning/merge_data.csv"
)


old_id <- old_db |>
  select(
    vector_site_id,
    vector_site_coordinates_latitude,
    vector_site_coordinates_longitude
  ) |>
  distinct()



old_sites <- old_db |>
  select(
    id = vector_site_id,
    site = vector_site_full_name,
    lat = vector_site_coordinates_latitude,
    lon = vector_site_coordinates_longitude
  ) |>
  distinct()

raw_data |>
  select(`...1`, site) |>
  left_join(
    y = ,
    by = join_by(site == vector_site_full_name)
  )




# get unique articles missing site and lat longs
raw_data |>
  filter(is.na(site)) |>
  pull(article.title) |>
  unique()

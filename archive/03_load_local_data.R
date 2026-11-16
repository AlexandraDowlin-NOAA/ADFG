# UPDATE below depending on srvy and yr ----------------------------------------

# summary tables -----------------------------------------------------------
# data tables from Sean code needed for report
# Load tables made in 'data_analysis.R'
haul_by_area <- 
  readr::read_csv(here::here(
    "output", region,
    paste0(region, "_", cruise, "_adfg_haul.csv")
  ))

catch_by_area <- 
  readr::read_csv(here::here(
    "output", region,
    paste0(region, "_", cruise, "_adfg_catch.csv")
  ))

# catch_total <- readr::read_csv(here::here(
#   "output", region,
#   paste0(region, "_", cruise, "_adfg_catch.csv")
# ))

# age_count_3nm <- readr::read_csv(here::here(
#   "output", region,
#   paste0(region, "_", cruise, "_adfg_specimens.csv")
# ))

age_by_area <- readr::read_csv(here::here(
  "output", region,
  paste0(region, "_", cruise, "_adfg_specimens.csv")
))

vouchers_by_area <- readr::read_csv(here::here(
  "output", region,
  paste0(region, "_", cruise, "_adfg_voucher.csv")
))
# vouchers_total <- readr::read_csv(here::here(
#   "output", region,
#   paste0(region, "_", cruise, "_adfg_voucher_total.csv")
# ))


# Table of hauls within 3 nm ----------------------------------------------
state_hauls <- read.csv(here::here("output", region, paste0(region, "_", cruise, "_state_hauls.csv")))

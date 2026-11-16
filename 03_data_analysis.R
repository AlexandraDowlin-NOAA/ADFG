# Calculate summary statistics for ADFG report

# Load data ----

haul_by_area <- 
  readr::read_csv(here::here(
    "output", region,
    paste0(region, "_", cruise, "_adfg_haul.csv")
  ))

catch_by_area <- 
  readr::read_csv(here::here(
    "output", region,
    paste0(region, "_", cruise, "_adfg_catch.csv")
  ))|>
  dplyr::mutate(taxon = dplyr::case_when(
    SPECIES_CODE <= 31550 ~ "fish",
    SPECIES_CODE >= 40001 ~ "invert"
  ))

age_by_area <- readr::read_csv(here::here(
  "output", region,
  paste0(region, "_", cruise, "_adfg_specimens.csv")
))|>
  dplyr::mutate(taxon = dplyr::case_when(
    SPECIES_CODE <= 31550 ~ "fish",
    SPECIES_CODE >= 40001 ~ "invert"
  ))

vouchers_by_area <- readr::read_csv(here::here(
  "output", region,
  paste0(region, "_", cruise, "_adfg_voucher.csv")
))|>
  dplyr::mutate(taxon = dplyr::case_when(
    SPECIES_CODE <= 31550 ~ "fish",
    SPECIES_CODE >= 40001 ~ "invert"
  ))

state_hauls <- read.csv(here::here("output", region, paste0(region, "_", cruise, "_state_hauls.csv")))

# survey number -----------------------------------------------------------
# generated from Oracle tables using SQL script from Ned
survnumber <- readRDS(file = here::here("output", region, paste0(region, "_", cruise, "_", "survnumber.rds"))) 

#  Short values -----------------------------------------------------------
# successful biomass tows 526sql

s_stations <- dplyr::filter(haul_by_area, ABUNDANCE_HAUL == "Y") |>
  nrow()

a_hauls <- dplyr::filter(haul_by_area, HAUL_TYPE == 3) |>
  nrow()

u_hauls <- dplyr::filter(haul_by_area, HAUL_TYPE == 3 & ABUNDANCE_HAUL == "N") |>
   nrow()

# stations within 3nm 
stations_3nm <- haul_by_area |>
  dplyr::filter(
    ABUNDANCE_HAUL == "Y",
    STATE_WATERS == "STATE") |>
  nrow()

# TAXA COUNTS ------------------------------------------------------------------
# total count of fish taxa encounter within 3nm

catch_by_taxon <- 
  catch_by_area  |>
  dplyr::group_by(
    taxon, STATE_WATERS
  ) |>
  dplyr::summarise(
    TOTAL_WEIGHT = sum(TOTAL_WEIGHT_KG),
    TOTAL_COUNT = sum(TOTAL_COUNT),
    N_TAXA = n()
  )

fish_3nm <- catch_by_taxon$TOTAL_COUNT[catch_by_taxon$STATE_WATERS == "STATE" & catch_by_taxon$taxon == "fish"]
  
# total weight of fish taxa encountered within 3nm
fish_3nm_wt <- catch_by_taxon$TOTAL_WEIGHT[catch_by_taxon$STATE_WATERS == "STATE" & catch_by_taxon$taxon == "fish"]

# total count of fish taxa encountered in all survey
fish_all <- length(unique(catch_by_area$SPECIES_CODE[catch_by_area$taxon == "fish"]))

# total weight of fish taxa encountered in all survey
fish_all_wt <- sum(catch_by_taxon$TOTAL_WEIGHT[catch_by_taxon$taxon == "fish"])

# total count of invert taxa within 3nm
inverts_3nm <- catch_by_taxon$TOTAL_COUNT[catch_by_taxon$STATE_WATERS == "STATE" & catch_by_taxon$taxon == "invert"]

# total weight of invert taxa within 3nm
inverts_3nm_wt <- catch_by_taxon$TOTAL_WEIGHT[catch_by_taxon$STATE_WATERS == "STATE" & catch_by_taxon$taxon == "invert"]

# total count of invert taxa in all survey
inverts_all <- length(unique(catch_by_area$SPECIES_CODE[catch_by_area$taxon == "invert"]))

# total weight of invert taxa in all survey
inverts_all_wt <- sum(catch_by_taxon$TOTAL_WEIGHT[catch_by_taxon$taxon == "invert"])

# VOUCHERS ----------------------------------------------------------------
# Voucher summary 3nm ----------------------------------------------------------
# MCS: need to confirm! does this look like a reasonable # of vouchers in state waters?
voucher_count_3nm <- sum(vouchers_by_area$N_RECORDS[vouchers_by_area$STATE_WATERS == "STATE"])


# combine and stack vouchers and age samples
voucher_age_3nm <- dplyr::bind_rows(voucher_count_3nm, age_count_3nm) |>
  dplyr::select(COMMON_NAME, SPECIES_NAME, N_RECORDS_STATE, SAMPLE_TYPE)

# voucher summary all survey  --------------------------------------------------
# counts of vouchers for all survey
voucher_count <- sum(vouchers_by_area$N_RECORDS)

# find out how to put together the new specimen table with the above voucher
voucher_age_all <- dplyr::bind_rows(voucher_count, age_count) |>
  dplyr::select(COMMON_NAME, SPECIES_NAME, N_RECORDS_TOTAL, SAMPLE_TYPE)

voucher_fish_taxa_count <- sum(vouchers_by_area$N_RECORDS[vouchers_by_area$taxon == "fish"])

voucher_invert_taxa_count <- sum(vouchers_by_area$N_RECORDS[vouchers_by_area$taxon == "invert"])

voucher_fish_taxa_count_3nm <- sum(vouchers_by_area$N_RECORDS[vouchers_by_area$taxon == "fish" & vouchers_by_area$STATE_WATERS == "STATE"])

voucher_invert_taxa_count_3nm <- sum(vouchers_by_area$N_RECORDS[vouchers_by_area$taxon == "invert" & vouchers_by_area$STATE_WATERS == "STATE"])


# count of vouchers collected within 3nm
voucher_3nm_count <- sum(vouchers_by_area$N_RECORDS[vouchers_by_area$STATE_WATERS == "STATE"])

# count of vouchers collected in all survey #there's a zero
voucher_all_count <- sum(vouchers_by_area$N_RECORDS)

# Taxa of fish and inverts
voucher_fish_taxa <- vouchers_total |>
  dplyr::left_join(species0, by = c("SPECIES_CODE" = "species_code")) |>
  dplyr::mutate(taxon = dplyr::case_when(
    SPECIES_CODE <= 31550 ~ "fish",
    SPECIES_CODE >= 40001 ~ "invert"
  )) |>
  dplyr::filter(taxon == "fish")

voucher_invert_taxa <- vouchers_total |>
  dplyr::left_join(species0, by = c("SPECIES_CODE" = "species_code")) |>
  dplyr::mutate(taxon = dplyr::case_when(
    SPECIES_CODE <= 31550 ~ "fish",
    SPECIES_CODE >= 40001 ~ "invert"
  )) |>
  dplyr::filter(taxon == "invert")


# AGE SAMPLES -------------------------------------------------------------
# total count of taxa otolith were collected from in all survey
oto_taxa <- voucher_age_all |>
  dplyr::filter(SAMPLE_TYPE == "Otoliths") |>
  nrow()

# total count of all survey otoliths collected
oto_all <- voucher_age_all |>
  dplyr::filter(SAMPLE_TYPE == "Otoliths") |>
  dplyr::summarize(total = sum(N_RECORDS_TOTAL)) |>
  as.numeric()

# total count of otoliths collected from within 3nm
oto_3nm <- voucher_age_3nm |>
  dplyr::filter(SAMPLE_TYPE == "Otoliths") |>
  dplyr::mutate(N_RECORDS_STATE = as.numeric(N_RECORDS_STATE)) |>
  dplyr::summarize(total = sum(N_RECORDS_STATE)) |>
  as.numeric()

# total count of taxa otoliths were collected from within 3nm
oto_3nm_taxa <- voucher_age_3nm |>
  dplyr::filter(SAMPLE_TYPE == "Otoliths") |>
  nrow()


# Total catch
total_wt_all_catch <- catch_total |>
  dplyr::summarize(total = sum(TOTAL_WEIGHT_KG)) |>
  as.numeric()
total_wt_all_80 <- total_wt_all_catch * 0.8


# Total 3nm catch
total_wt_3nm_catch <- catch_state |>
  dplyr::summarize(total = sum(TOTAL_WEIGHT_KG_STATE)) |>
  as.numeric()
total_wt_3nm_80 <- total_wt_3nm_catch * 0.8

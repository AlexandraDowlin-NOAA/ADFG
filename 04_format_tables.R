# Formatting functions
parse_species <- function(df, include_egg_only = TRUE) {
  df <- df |>
    tidyr::separate(
      SPECIES_NAME, c("species", "species_suffix"),
      sep = "(?= sp\\.)", extra = "merge", remove = FALSE
    ) |>
    dplyr::mutate(species_suffix = dplyr::if_else(
      stringr::str_count(species, "\\w+") == 1 & is.na(species_suffix),
      species,
      species_suffix
    )) |>
    tidyr::separate(
      species, c("species_2", "egg_case"),
      sep = "(?=egg case)", remove = TRUE, extra = "merge"
    ) |>
    tidyr::separate(
      species_2, c("species_2", "egg_only"),
      sep = "(?= egg$)", remove = TRUE, extra = "merge"
    )
  
  if (include_egg_only) {
    df |>
      dplyr::mutate(
        new_suffix = dplyr::coalesce(species_suffix, egg_case, egg_only),
        species_2 = dplyr::if_else(species_2 == new_suffix, "", species_2, missing = species_2)
      ) |>
      dplyr::select(-species_suffix, -egg_case, -egg_only)
  } else {
    df |>
      dplyr::mutate(new_suffix = dplyr::coalesce(species_suffix, egg_case)) |>
      dplyr::select(-species_suffix, -egg_case)
  }
}

# Format catch count and weight columns
format_catch_metrics <- function(df) {
  df |>
    dplyr::mutate(
      TOTAL_COUNT = dplyr::if_else(
        round(TOTAL_COUNT) == 0,
        "--",
        format(round(TOTAL_COUNT), big.mark = ",", trim = TRUE)
      ),
      TOTAL_WEIGHT_KG_CHR = dplyr::if_else(
        TOTAL_WEIGHT_KG < 0.01,
        "< 0.01",
        formatC(TOTAL_WEIGHT_KG, format = "f", digits = 2, big.mark = ",")
      )
    )
}

#  Make tables ----
catch_sum_3nm <- catch_by_area |>
  dplyr::filter(STATE_WATERS == "STATE") |>
  parse_species(include_egg_only = TRUE) |>
  format_catch_metrics() |>
  dplyr::arrange(SPECIES_CODE)

catch_sum_all <- catch_by_area |>
  dplyr::summarise(
    TOTAL_WEIGHT_KG = sum(TOTAL_WEIGHT_KG, na.rm = TRUE),
    TOTAL_COUNT = sum(TOTAL_COUNT, na.rm = TRUE),
    .by = c(COMMON_NAME, SPECIES_NAME, SPECIES_CODE)
  ) |>
  parse_species(include_egg_only = TRUE) |>
  format_catch_metrics() |>
  dplyr::arrange(SPECIES_CODE)

vouchers_ages_combined <- dplyr::bind_rows(vouchers_by_area, age_by_area)

voucher_3nm <- vouchers_ages_combined |>
  dplyr::filter(STATE_WATERS == "STATE") |>
  parse_species(include_egg_only = FALSE) |>
  dplyr::arrange(SPECIES_CODE)

voucher_all <- vouchers_ages_combined |>
  dplyr::summarise(
    N_RECORDS = sum(N_RECORDS, na.rm = TRUE),
    .by = c(COMMON_NAME, SPECIES_NAME, SPECIES_CODE, SAMPLE_TYPE)
  ) |>
  parse_species(include_egg_only = FALSE) |>
  dplyr::arrange(SPECIES_CODE)

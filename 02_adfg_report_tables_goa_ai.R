# Author: Sean Rohan

channel <- navmaps::get_connected(schema = "AFSC")

# Generate towpath linestrings ----
haul_points <- 
  RODBC::sqlQuery(
    channel = channel,
    query = paste0(
      "SELECT * FROM RACEBASE.HAUL
  WHERE 
    vessel in (", paste(vessel, collapse = ","), ")
  AND cruise = ", cruise
    )
  )

start_coords <-
  haul_points |> 
  dplyr::mutate(x = START_LONGITUDE, y = START_LATITUDE)

end_coords <-
  haul_points |> 
  dplyr::mutate(x = END_LONGITUDE, y = END_LATITUDE)

towpaths <-
  dplyr::bind_rows(
    start_coords,
    end_coords
  ) |>
  sf::st_as_sf(coords = c("x", "y"), crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338") |>
  dplyr::group_by(
    VESSEL, CRUISE, HAUL, PERFORMANCE, HAUL_TYPE, GEAR, ACCESSORIES, DURATION, DISTANCE_FISHED
  ) |>
  dplyr::summarise(do_union = TRUE) |>
  sf::st_cast(to = "LINESTRING")

sf::st_write(
  towpaths,
  dsn = here::here("data", paste0(region, "_towpaths_", cruise, ".gpkg")),
  overwrite = TRUE,
  append = FALSE
)

# Check that towpaths have been generated for the selected vessels/cruise
nrow(towpaths)

# Retrieve Alaska DNR Land polygon from akgfmaps ----
layers <-
  akgfmaps::get_base_layers(
    select.region = region,
    set.crs = "EPSG:3338",
    high.resolution.coast = TRUE
  )

# Add 3 nm buffer to land ----
land_buffer <-
  layers$akland |>
  dplyr::filter(STATE_PROVINCE == "Alaska") |>
  sf::st_buffer(
    dist = 3 * 1852, # 3 nautical miles
    single_side = TRUE
  )

# Classify state vs. external waters based on intersection between state waters and tow paths
state_hauls <-
  towpaths |>
  sf::st_intersection(land_buffer) |>
  sf::st_drop_geometry() |>
  dplyr::select(VESSEL, CRUISE, HAUL) |>
  dplyr::mutate(STATE_WATERS = "STATE_KG") |>
  unique()

# save this table; we need it later
write.csv(state_hauls,
          file = here::here("output", region, paste0(region, "_", cruise, "_state_hauls.csv")),
          row.names = FALSE
)


# Get haul data ----
haul <-
  RODBC::sqlQuery(
    channel = channel,
    query =
      paste0(
        "
  SELECT
    VESSEL,
    CRUISE,
    HAUL,
    STATIONID,
    STRATUM,
    START_TIME,
    ABUNDANCE_HAUL,
    HAUL_TYPE
  FROM RACEBASE.HAUL
  WHERE
    VESSEL IN (", paste(vessel, collapse = ", "), ")",
        "AND CRUISE = ", cruise
      )
  ) |>
  dplyr::left_join(state_hauls,
                   by = c("VESSEL", "CRUISE", "HAUL")
  ) |>
  dplyr::mutate(STATE_WATERS = ifelse(is.na(STATE_WATERS), "FEDERAL", "STATE"))

# Retrieve catch and specimen data ----
catch <-
  RODBC::sqlQuery(
    channel = channel,
    query =
      paste0(
        "
  SELECT
    VESSEL,
    CRUISE,
    HAUL,
    SPECIES_CODE,
    WEIGHT,
    NUMBER_FISH,
    VOUCHER
  FROM RACEBASE.CATCH
  WHERE
    VESSEL IN (", paste(vessel, collapse = ", "), ")",
        "AND CRUISE = ", cruise
      )
  ) |>
  dplyr::left_join(state_hauls,
                   by = c("VESSEL", "CRUISE", "HAUL")
  ) |>
  dplyr::mutate(STATE_WATERS = ifelse(is.na(STATE_WATERS), "FEDERAL", "STATE"))

specimen <-
  RODBC::sqlQuery(
    channel = channel,
    query =
      paste0(
        "
  SELECT
    VESSEL,
    CRUISE,
    HAUL,
    SPECIES_CODE,
    SPECIMENID,
    SPECIMEN_SAMPLE_TYPE
  FROM RACEBASE.SPECIMEN
  WHERE
    VESSEL IN (", paste(vessel, collapse = ", "), ")",
        "AND CRUISE = ", cruise
      )
  ) |>
  dplyr::left_join(state_hauls,
                   by = c("VESSEL", "CRUISE", "HAUL")
  ) |>
  dplyr::mutate(STATE_WATERS = ifelse(is.na(STATE_WATERS), "FEDERAL", "STATE"))

# Get taxonomic info
survey_taxa <-
  RODBC::sqlQuery(
    channel = channel,
    query = "
      SELECT DISTINCT 
        SPECIES_NAME, 
        COMMON_NAME, 
        SPECIES_CODE
      FROM
        GAP_PRODUCTS.AKFIN_TAXONOMIC_CLASSIFICATION"
  )

# Get specimen types ----

specimen_sample_type <-
  RODBC::sqlQuery(
    channel = channel,
    query = "
      SELECT
        SPECIMEN_SAMPLE_TYPE_ID AS SPECIMEN_SAMPLE_TYPE,
        NAME AS SAMPLE_TYPE
      FROM
        RACE_DATA.SPECIMEN_SAMPLE_TYPES"
  )

# Calculate total catch and weight

catch_by_area <-
  catch |>
  dplyr::left_join(
    survey_taxa
  ) |>
  dplyr::group_by(SPECIES_CODE, SPECIES_NAME, COMMON_NAME, STATE_WATERS) |>
  dplyr::summarise(
    TOTAL_WEIGHT_KG = sum(WEIGHT, na.rm = TRUE),
    TOTAL_COUNT = sum(NUMBER_FISH, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(-TOTAL_WEIGHT_KG) |>
  dplyr::select(COMMON_NAME, SPECIES_NAME, SPECIES_CODE, TOTAL_WEIGHT_KG, TOTAL_COUNT, STATE_WATERS)


# Tally vouchers ----
vouchers_by_area <-
  catch |>
  dplyr::filter(!is.na(VOUCHER)) |>
  dplyr::left_join(survey_taxa, by = "SPECIES_CODE") |>
  dplyr::group_by(COMMON_NAME, SPECIES_NAME, SPECIES_CODE, STATE_WATERS) |>
  dplyr::summarise(N_RECORDS = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(SPECIES_CODE) |>
  dplyr::mutate(SAMPLE_TYPE = "Voucher")


# Tally specimens ----
specimen_by_area <-
  specimen |>
  dplyr::left_join(survey_taxa, by = "SPECIES_CODE") |>
  dplyr::left_join(specimen_sample_type, by = "SPECIMEN_SAMPLE_TYPE") |>
  dplyr::group_by(COMMON_NAME, SPECIES_NAME, STATE_WATERS, SPECIES_CODE, SAMPLE_TYPE) |>
  dplyr::summarise(N_RECORDS = n(), .groups = "drop")

# Combine specimen and voucher tables

specimen_voucher <-
  dplyr::bind_rows(
    vouchers_by_area,
    specimen_by_area
  )

# Write to .csv
if (!dir.exists(here::here("output", region, "tables"))) {
  dir.create(here::here("output", region, "tables"))
}

write.csv(
  x = haul,
  file = here::here("output", region, paste0(region, "_", cruise, "_adfg_haul.csv")),
  row.names = FALSE
)

write.csv(
  x = vouchers_by_area,
  file = here::here("output", region, paste0(region, "_", cruise, "_adfg_voucher.csv")),
  row.names = FALSE
)

write.csv(
  x = catch_by_area,
  file = here::here("output", region, paste0(region, "_", cruise, "_adfg_catch.csv")),
  row.names = FALSE
)

write.csv(
  x = specimen_by_area,
  file = here::here("output", region, paste0(region, "_", cruise, "_adfg_specimens.csv")),
  row.names = FALSE
)


# Get nth cruise number ---------------------------------------------------
survnumber <-
  RODBC::sqlQuery(
    channel = channel,
    query = "
      select distinct floor(cruise/100) survey_year
      from racebase.haul
      where region = 'GOA'
      and abundance_haul = 'Y'
      order by floor(cruise/100);"
  ) |>
  nrow() |>
  scales::ordinal()

saveRDS(survnumber, file = paste0("output/", region, "/", region, "_", cruise, "_survnumber.rds"))


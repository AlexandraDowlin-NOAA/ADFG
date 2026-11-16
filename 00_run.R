# Use 00_run.R to create entire ADFG document

source("./01_functions.R") # Update packages and load

# Modify these settings ----------------------------------------------------------------------------
refresh_tables <- TRUE #set to true if downloading new data

region <- "ai"
cruise <- 202601
vessel <- c(148, 176)
vessel1 <- "FV Ocean Explorer"
vessel2 <- "FV Alaska Provider"
maxyr <- floor(cruise/100)

dates_conducted <- "25 May and 3 August" # check dates for vessels
data_finalized <- "August 25, 2025"

t_stations <- "480" # Total planned stations; confirmed this is the allocation

# Updated based on region
if(region == "goa") {
  minyr <- 1990
  region_name <- "Gulf of Alaska"
  regionw_abbr <- "Gulf of Alaska (GOA)"
}

if(region == "ai") {
  minyr <- 1991
  region_name <- "Aleutian Islands"
  regionw_abbr <- "Aleutian Islands (AI)"
}

time_series <- paste0(maxyr-minyr+1, " year time series")


# Setup directories
dir.create("data", showWarnings = FALSE)
dir.create(here::here("output", region), recursive = TRUE, showWarnings = FALSE)

# Run scripts

# Everything below requires a connection, time, etc.
if(refresh_tables){
  source('./02_adfg_report_tables_goa_ai.R') # Retrieve haul, catch, voucher, specimen data
}

source("./03_load_local_data.R")
source('./04_data_analysis.R') # load and calculate values used in report

# Create the day's output folder
dir_out <- paste0("./output/", Sys.Date())
dir.create(path = dir_out)
rmarkdown::render(
  paste0("./ADFG_REPORT.Rmd"),
  output_dir = dir_out,
  output_file = paste0(maxyr,"_ADFG_REPORT.docx")
)

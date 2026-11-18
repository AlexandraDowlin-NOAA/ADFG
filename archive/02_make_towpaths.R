# create tables needed for ADFG report

# Make layers using navmaps
library(navmaps)
if(!dir.exists("data")){dir.create("data")}

#I think you can just use whatever SQL query you had for RACEBASE.VOUCHER that you had before, and left join that to state_hauls (lines 42-48 in adfg_report_goa_ai.R) to get whether it's in state waters. So you'd have to source adfg_report_goa_ai.R first, then pull the VOUCHER table, then join them
#(by haul, vessel, cruise)



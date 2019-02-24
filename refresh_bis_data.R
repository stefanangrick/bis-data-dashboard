### Housekeeping ###############################################################
Sys.setenv(TZ = "Asia/Tokyo")
options(tz = "Asia/Tokyo")
options("scipen" = 100, "digits" = 4)

# Libraries
library("BIS")          # To retrieve BIS data

### Refresh BIS data ###########################################################
ds  <- get_datasets()

#lbs <- get_bis(ds$url[grep("Locational banking statistics", ds$name)])
download.file(ds$url[grep("Locational banking statistics", ds$name)],
              mode = "wb", destfile = "full_bis_lbs_diss_csv.zip")
unzip("full_bis_lbs_diss_csv.zip")
lbs <- read.csv("WEBSTATS_LBS_D_PUB_DATAFLOW_csv_col.csv", header = TRUE,
                stringsAsFactors = FALSE)

#cbs <- get_bis(ds$url[grep("Consolidated banking statistics", ds$name)])
download.file(ds$url[grep("Consolidated banking statistics", ds$name)],
              mode = "wb", destfile = "full_bis_cbs_csv.zip")
unzip("full_bis_cbs_csv.zip")
cbs <- read.csv("WEBSTATS_CBS_DATAFLOW_csv_col.csv", header = TRUE,
                stringsAsFactors = FALSE)

#sec <- get_bis(ds$url[grep("Debt securities statistics", ds$name)])
download.file(ds$url[grep("Debt securities statistics", ds$name)],
              mode = "wb", destfile = "full_bis_debt_sec2_csv.zip")
unzip("full_bis_debt_sec2_csv.zip")
sec <- read.csv("WEBSTATS_DEBTSEC_DATAFLOW_csv_col.csv", header = TRUE,
                stringsAsFactors = FALSE)

xtd <- get_bis(ds$url[grep("Exchange-traded derivatives statistics",
                           ds$name)])

#otc <- get_bis(ds$url[grep("OTC derivatives outstanding", ds$name)])
download.file(ds$url[grep("OTC derivatives outstanding", ds$name)],
              mode = "wb", destfile = "full_bis_otc_csv.zip")
unzip("full_bis_otc_csv.zip")
otc <- read.csv("WEBSTATS_OTC_DATAFLOW_csv_col.csv", header = TRUE,
                stringsAsFactors = FALSE)

#der <- get_bis(ds$url[grep("Triennial Survey statistics on", ds$name)])
download.file(ds$url[grep("Triennial Survey statistics on", ds$name)],
              mode = "wb", destfile = "full_BIS_DER_csv.zip")
unzip("full_BIS_DER_csv.zip")
der <- read.csv("full_WEBSTATS_DER_DATAFLOW_csv.csv", header = TRUE,
                stringsAsFactors = FALSE, skip = 10)

gli <- get_bis(ds$url[grep("Global liquidity indicators", ds$name)])
cre <- get_bis(ds$url[grep("Credit to the non-financial sector", ds$name)])
ctg <- get_bis(ds$url[grep("Credit-to-GDP gaps", ds$name)])
dsr <- get_bis(ds$url[grep("Debt service ratios", ds$name)])
lpp <- get_bis(ds$url[grep("Property prices: long series", ds$name)])
spp <- get_bis(ds$url[grep("Property prices: selected series", ds$name)])
cpi <- get_bis(ds$url[grep("Consumer prices", ds$name)])
xru <- get_bis(ds$url[grep("US dollar exchange rates.*daily.*vertical",
                           ds$name)])
eer <- get_bis(ds$url[grep("Effective exchange rate.*daily.*vertical",
                           ds$name)])
cbp <- get_bis(ds$url[grep("Policy rates.*monthly", ds$name)])
lastrefresh <- Sys.time()

save(list = c("ds", "lbs", "cbs", "sec", "xtd", "otc", "der", "gli", "cre",
              "ctg", "dsr", "lpp", "spp", "cpi", "xru", "eer", "cbp",
              "lastrefresh"),
     file = "bis_stats.RData")

file.remove("WEBSTATS_LBS_D_PUB_DATAFLOW_csv_col.csv")
file.remove("full_bis_lbs_diss_csv.zip")

file.remove("WEBSTATS_CBS_DATAFLOW_csv_col.csv")
file.remove("full_bis_cbs_csv.zip")

file.remove("WEBSTATS_DEBTSEC_DATAFLOW_csv_col.csv")
file.remove("full_bis_debt_sec2_csv.zip")

file.remove("WEBSTATS_OTC_DATAFLOW_csv_col.csv")
file.remove("full_bis_otc_csv.zip")

file.remove("full_WEBSTATS_DER_DATAFLOW_csv.csv")
file.remove("full_BIS_DER_csv.zip")

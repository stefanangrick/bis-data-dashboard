### Housekeeping ###############################################################
Sys.setenv(TZ = "Asia/Tokyo")
options(tz = "Asia/Tokyo")
options("scipen" = 100, "digits" = 4)

# Libraries
library("BIS")          # To retrieve BIS data
library("countrycode")  # For matching country names
library("zoo")          # For managing dates
library("reshape2")     # To reshape datasets
library("xts")          # Advanced time series data management
library("plotly")       # Interactive plots

### Helper functions ############################################################
PlotlyLine <- function(xts.obj, labs = NULL, hghlght = "", xlab = "",
                       ylab = "", startdate = NULL, yrng = NULL) {
  if (is.null(labs)) {
    labs <- colnames(xts.obj)
  }
  colnames(xts.obj) <- make.names(colnames(xts.obj))
  
  # If no startdate is set, start in 2000
  if (is.null(startdate)) {
    startdate <- as.Date("2000-01-01")
  }
  
  # Set x date range
  xstart <- as.numeric(startdate) * 24 * 60 * 60 * 1000
  xend   <- as.numeric(max(index(xts.obj))) * 24 * 60 * 60 * 1000
  
  # Determine optimal y range
  yrng <- c(NA, NA)
  sigd <- 0
  if (diff(range(xts.obj[paste0(startdate, "/")], na.rm = TRUE)) < 2) {
    sigd <- 1
  }
  yrng[1] <- round(min(xts.obj[paste0(startdate, "/")], na.rm = TRUE) -
                     diff(range(xts.obj[paste0(startdate, "/")],
                                na.rm = TRUE)) / 25, sigd)
  yrng[2] <- round(max(xts.obj[paste0(startdate, "/")], na.rm = TRUE) +
                     diff(range(xts.obj[paste0(startdate, "/")],
                                na.rm = TRUE)) / 25, sigd)
  
  # Set up plotly object
  pp <- plot_ly(data.frame(coredata(xts.obj)), x = index(xts.obj))
  
  for (i in 1:ncol(xts.obj)) {
    lw <- ifelse((colnames(xts.obj)[i] == hghlght), 3, 2)
    pp <- add_trace(pp, y = data.frame(coredata(xts.obj))[, i],
                    name = labs[i], type = "scatter", mode = "lines",
                    line = list(width = lw))
  }
  
  pp <- layout(pp,
               plot_bgcolor = "rgba(235,235,235,1)",
               paper_bgcolor = "rgba(255,255,255,1)",
               margin = list(l = 5, r = 5, b = 5, t = 5, pad = 1),
               xaxis = list(title = xlab,
                            titlefont = list(size = 12),
                            gridcolor = toRGB("white"),
                            rangeselector = list(
                              buttons = list(
                                list(count = 6, label = "6m", step = "month",
                                     stepmode = "backward"),
                                list(count = 5, label = "5y", step = "year",
                                     stepmode = "backward"),
                                list(count = 10, label = "10y", step = "year",
                                     stepmode = "backward"),
                                list(count = 20, label = "20y", step = "year",
                                     stepmode = "backward"),
                                list(step = "all"))),
                            range = c(xstart, xend)),
               yaxis = list(title = ylab,
                            titlefont = list(size = 12),
                            fixedrange = FALSE,
                            range = yrng,
                            gridcolor = toRGB("white")))
  pp <- config(pp, displaylogo = FALSE, collaborate = FALSE)
  return(pp)
}

PlotlyBar <- function(xts.obj, labs = NULL, hghlght = "", xlab = "",
                      ylab = "", startdate = NULL, bmode = "relative") {
  if (is.null(labs)) {
    labs <- colnames(xts.obj)
  }
  colnames(xts.obj) <- make.names(colnames(xts.obj))
  
  # If no startdate is set, start in 2000
  if (is.null(startdate)) {
    startdate <- as.Date("2000-01-01")
  }
  
  # Set x date range
  td <- as.numeric(diff(index(xts.obj)[(nrow(xts.obj) - 1):nrow(xts.obj)])) / 2
  xstart <- (as.numeric(startdate) - td / 2) * 24 * 60 * 60 * 1000
  xend   <- (as.numeric(max(index(xts.obj))) + td) * 24 * 60 * 60 * 1000
  
  # Set up plotly object
  pp <- plot_ly(data.frame(coredata(xts.obj)), x = index(xts.obj))
  
  for (i in 1:ncol(xts.obj)) {
    pp <- add_trace(pp, y = data.frame(coredata(xts.obj))[, i],
                    name = labs[i], type = "bar")
  }
  
  pp <- layout(pp,
               barmode = bmode,
               margin = list(l = 5, r = 5, b = 5, t = 5, pad = 1),
               plot_bgcolor = "rgba(235,235,235,1)",
               paper_bgcolor = "rgba(255,255,255,1)",
               xaxis = list(title = xlab,
                            titlefont = list(size = 12),
                            gridcolor = toRGB("white"),
                            rangeselector = list(
                              buttons = list(
                                list(count = 6, label = "6m", step = "month",
                                     stepmode = "backward"),
                                list(count = 5, label = "5y", step = "year",
                                     stepmode = "backward"),
                                list(count = 10, label = "10y", step = "year",
                                     stepmode = "backward"),
                                list(count = 20, label = "20y", step = "year",
                                     stepmode = "backward"),
                                list(step = "all"))),
                            range = c(xstart, xend)),
               yaxis = list(title = ylab,
                            titlefont = list(size = 12),
                            gridcolor = toRGB("white")))
  pp <- config(pp, displaylogo = FALSE, collaborate = FALSE)
  return(pp)
}

### Functions to get subsets of BIS data in xts format #########################
### Locational banking statistics, by currency #####
GetLbsDataCurr <- function(lbs, ctr, lpos = "C") {
  if (ctr == "XM") {
    ctr <- "5C"
  }
  lbs <- subset(lbs, (L_MEASURE == "S"))
  lbs <- subset(lbs, (L_POSITION == lpos))
  lbs <- subset(lbs, (L_INSTR == "A"))
  lbs <- subset(lbs, (L_CURR_TYPE == "A"))
  lbs <- subset(lbs, (L_PARENT_CTY == "5J"))
  lbs <- subset(lbs, (L_REP_CTY == "5A"))
  lbs <- subset(lbs, (L_CP_COUNTRY == ctr))
  lbs <- subset(lbs, (L_CP_SECTOR == "A"))
  if (!(ctr %in% lbs$L_CP_COUNTRY)) {
    return(NULL)
  }
  lbs <- lbs[, c("L_DENOM", grep("^X.*", colnames(lbs), value = TRUE))]
  lbs <- melt(lbs, id = "L_DENOM")
  lbs <- subset(lbs, !(is.na(value)))
  lbs$date <- as.Date(as.yearqtr(gsub("\\.", " ",
                                      gsub("^X", "", lbs$variable))))
  
  # Set magnitude to 0
  lbs$value <- lbs$value * 10^6
  
  # Invert liabilities
  if (lpos == "L") {
    lbs$value <- -lbs$value
  }
  
  lbs <- subset(lbs, (!is.na(value)))
  lbs <- dcast(lbs, "date ~ L_DENOM")
  lbs$OTH <- lbs$TO1 - lbs$USD - lbs$EUR - lbs$JPY - lbs$GBP - lbs$CHF - lbs$UN9
  lbs$TO1 <- NULL
  lbs <- lbs[, c("date", "USD", "EUR", "JPY", "GBP", "CHF", "OTH", "UN9")]
  lbs <- xts(lbs[, -1, drop = FALSE], order.by = lbs[, 1])
  return(lbs)
}

### Locational banking statistics, by instrument #####
GetLbsDataInstr <- function(lbs, ctr, lpos = "C") {
  if (ctr == "XM") {
    ctr <- "5C"
  }
  lbs <- subset(lbs, (L_MEASURE == "S"))
  lbs <- subset(lbs, (L_POSITION == lpos))
  lbs <- subset(lbs, (L_DENOM) == "TO1")
  lbs <- subset(lbs, (L_CURR_TYPE == "A"))
  lbs <- subset(lbs, (L_PARENT_CTY == "5J"))
  lbs <- subset(lbs, (L_REP_CTY == "5A"))
  lbs <- subset(lbs, (L_CP_COUNTRY == ctr))
  lbs <- subset(lbs, (L_CP_SECTOR == "A"))
  if (!(ctr %in% lbs$L_CP_COUNTRY)) {
    return(NULL)
  }
  lbs <- lbs[, c("L_INSTR", grep("^X.*", colnames(lbs), value = TRUE))]
  lbs <- melt(lbs, id = "L_INSTR")
  lbs <- subset(lbs, !(is.na(value)))
  lbs$date <- as.Date(as.yearqtr(gsub("\\.", " ",
                                      gsub("^X", "", lbs$variable))))
  
  # Set magnitude to 0
  lbs$value <- lbs$value * 10^6
  
  # Invert liabilities
  if (lpos == "L") {
    lbs$value <- -lbs$value
  }
  
  lbs <- subset(lbs, (!is.na(value)))
  lbs <- dcast(lbs, "date ~ L_INSTR")
  lbs$A <- NULL
  lbs <- xts(lbs[, -1, drop = FALSE], order.by = lbs[, 1])
  return(lbs)
}

### Locational banking statistics, by country #####
GetLbsDataCtr <- function(lbs, ctr, lpos = "C") {
  if (ctr == "XM") {
    ctr <- "5C"
  }
  lbs <- subset(lbs, (L_MEASURE == "S"))
  lbs <- subset(lbs, (L_POSITION == lpos))
  lbs <- subset(lbs, (L_INSTR == "A"))
  lbs <- subset(lbs, (L_DENOM) == "TO1")
  lbs <- subset(lbs, (L_CURR_TYPE == "A"))
  lbs <- subset(lbs, (L_PARENT_CTY == "5J"))
  lbs <- subset(lbs, (L_REP_CTY %in% c("5A", na.omit(unique(codelist$iso2c)))))
  lbs <- subset(lbs, (L_CP_COUNTRY == ctr))
  lbs <- subset(lbs, (L_CP_SECTOR == "A"))
  if (!(ctr %in% lbs$L_CP_COUNTRY)) {
    return(NULL)
  }
  lbs <- lbs[, c("L_REP_CTY", grep("^X.*", colnames(lbs), value = TRUE))]
  lbs <- melt(lbs, id = "L_REP_CTY")
  lbs <- subset(lbs, !(is.na(value)))
  lbs$date <- as.Date(as.yearqtr(gsub("\\.", " ", gsub("^X", "",
                                                       lbs$variable))))
  
  # Set magnitude to 0
  lbs$value <- lbs$value * 10^6
  
  # Invert liabilities
  decr <- TRUE
  if (lpos == "L") {
    lbs$value <- -lbs$value
    decr <- FALSE
  }
  
  lbs <- subset(lbs, (!is.na(value)))
  lbs <- dcast(lbs, "date ~ L_REP_CTY")
  if (ncol(lbs) <= 2) {
    return(NULL)
  }
  tnl <- names(sort(colSums(lbs[(nrow(lbs)-36):nrow(lbs), -1], na.rm = TRUE),
                    decreasing = decr)[2:10])
  lbs <- lbs[, c("date", "5A", tnl)]
  lbs$OT <- lbs[, "5A"] -
    rowSums(lbs[, -which(colnames(lbs) %in% c("date", "5A"))], na.rm = TRUE)
  lbs[, "5A"] <- NULL
  lbs <- xts(lbs[, -1, drop = FALSE], order.by = lbs[, 1])
  return(lbs)
}

### Consolidated banking statistics, immediate counterparty basis, by country #####
GetCbsDataIcCtr <- function(cbs, ctr) {
  if (ctr == "XM") {
    ctr <- "5C"
  }
  if (!(ctr %in% cbs$L_CP_COUNTRY)) {
    return(NULL)
  }
  cbs <- subset(cbs, (L_MEASURE == "S"))
  cbs <- subset(cbs, (L_REP_CTY %in% c("5A", na.omit(unique(codelist$iso2c)))))
  cbs <- subset(cbs, (CBS_BANK_TYPE == "4R"))
  cbs <- subset(cbs, (CBS_BASIS == "F"))
  cbs <- subset(cbs, (L_POSITION == "C"))
  cbs <- subset(cbs, (L_INSTR == "A"))
  cbs <- subset(cbs, (REM_MATURITY == "A"))
  cbs <- subset(cbs, (CURR_TYPE_BOOK) == "TO1") 
  cbs <- subset(cbs, (L_CP_SECTOR == "A"))
  cbs <- subset(cbs, (L_CP_COUNTRY == ctr))
  cbs <- cbs[, c("L_REP_CTY", grep("^X.*", colnames(cbs), value = TRUE))]
  cbs <- melt(cbs, id = "L_REP_CTY")
  cbs <- subset(cbs, !(is.na(value)))
  cbs$date <- as.Date(as.yearqtr(gsub("\\.", " ",
                                      gsub("^X", "", cbs$variable))))
  
  # Set magnitude to 0
  cbs$value <- cbs$value * 10^6
  
  cbs <- subset(cbs, (!is.na(value)))
  cbs <- dcast(cbs, "date ~ L_REP_CTY")
  tnl <- names(sort(colSums(cbs[(nrow(cbs) - 36):nrow(cbs), -1], na.rm = TRUE),
                    decreasing = TRUE)[2:10])
  cbs <- cbs[, c("date", "5A", tnl)]
  cbs$OT <- cbs[, "5A"] -
    rowSums(cbs[, -which(colnames(cbs) %in% c("date", "5A"))], na.rm = TRUE)
  cbs[, "5A"] <- NULL
  cbs <- xts(cbs[, -1, drop = FALSE], order.by = cbs[, 1])
  return(cbs)
}

### Foreign bank business within market #####
GetCbsDataIcCtrForeignbankshare <- function(cbs, ctr, refctr) {
  if (ctr == "XM") {
    ctr <- "5C"
  }
  if (!(ctr %in% cbs$L_CP_COUNTRY)) {
    return(NULL)
  }
  cbs <- subset(cbs, (L_MEASURE == "S"))
  cbs <- subset(cbs, (L_REP_CTY == "5A"))
  cbs <- subset(cbs, (CBS_BANK_TYPE == "4R"))
  cbs <- subset(cbs, (CBS_BASIS == "F"))
  cbs <- subset(cbs, (L_POSITION %in% c("C", "B")))
  cbs <- subset(cbs, (L_INSTR == "A"))
  cbs <- subset(cbs, (REM_MATURITY == "A"))
  cbs <- subset(cbs, (CURR_TYPE_BOOK) %in% c("TO1", "LC1"))
  cbs <- subset(cbs, (L_CP_SECTOR == "A"))
  cbs <- subset(cbs, (L_CP_COUNTRY %in% c(ctr, refctr)))
  cbs <- cbs[, c("L_POSITION", "L_CP_COUNTRY",
                 grep("^X.*", colnames(cbs), value = TRUE))]
  cbs <- melt(cbs, id = c("L_POSITION", "L_CP_COUNTRY"))
  cbs <- subset(cbs, !(is.na(value)))
  cbs$date <- as.Date(as.yearqtr(gsub("\\.", " ",
                                      gsub("^X", "", cbs$variable))))
  
  # Set magnitude to 0
  cbs$value <- cbs$value * 10^6
  
  cbs <- subset(cbs, (!is.na(value)))
  cbs <- dcast(cbs, "date + L_CP_COUNTRY ~ L_POSITION")
  cbs$R <- cbs$B / cbs$C * 100
  cbs <- dcast(cbs, "date ~ L_CP_COUNTRY", value.var = "R")
  cbs <- xts(cbs[, -1, drop = FALSE], order.by = cbs[, 1])
  return(cbs)
}

### Consolidated banking statistics, ultimate risk basis, by country #####
GetCbsDataUrCtr <- function(cbs, ctr) {
  if (ctr == "XM") {
    ctr <- "5C"
  }
  if (!(ctr %in% cbs$L_CP_COUNTRY)) {
    return(NULL)
  }
  cbs <- subset(cbs, (L_MEASURE == "S"))
  cbs <- subset(cbs, (L_REP_CTY %in% c("5A", na.omit(unique(codelist$iso2c)))))
  cbs <- subset(cbs, (CBS_BANK_TYPE == "4R"))
  cbs <- subset(cbs, (CBS_BASIS == "U"))
  cbs <- subset(cbs, (L_POSITION == "C"))
  cbs <- subset(cbs, (L_INSTR == "A"))
  cbs <- subset(cbs, (REM_MATURITY == "A"))
  cbs <- subset(cbs, (CURR_TYPE_BOOK) == "TO1") 
  cbs <- subset(cbs, (L_CP_SECTOR == "A"))
  cbs <- subset(cbs, (L_CP_COUNTRY == ctr))
  cbs <- cbs[, c("L_REP_CTY", grep("^X.*", colnames(cbs), value = TRUE))]
  cbs <- melt(cbs, id = "L_REP_CTY")
  cbs <- subset(cbs, !(is.na(value)))
  cbs$date <- as.Date(as.yearqtr(gsub("\\.", " ",
                                      gsub("^X", "", cbs$variable))))
  
  # Set magnitude to 0
  cbs$value <- cbs$value * 10^6
  
  cbs <- subset(cbs, (!is.na(value)))
  cbs <- dcast(cbs, "date ~ L_REP_CTY")
  tnl <- names(sort(colSums(cbs[(nrow(cbs) - 36):nrow(cbs), -1], na.rm = TRUE),
                    decreasing = TRUE)[2:10])
  cbs <- cbs[, intersect(c("date", "5A", tnl), colnames(cbs))]
  cbs$OT <- cbs[, "5A"] -
    rowSums(cbs[, -which(colnames(cbs) %in% c("date", "5A"))], na.rm = TRUE)
  cbs[, "5A"] <- NULL
  cbs <- xts(cbs[, -1, drop = FALSE], order.by = cbs[, 1])
  return(cbs)
}

### Consolidated banking statistics, ultimate risk basis, by sector #####
GetCbsDataUrSect <- function(cbs, ctr) {
  if (ctr == "XM") {
    ctr <- "5C"
  }
  if (!(ctr %in% cbs$L_CP_COUNTRY)) {
    return(NULL)
  }
  cbs <- subset(cbs, (L_MEASURE == "S"))
  cbs <- subset(cbs, (L_REP_CTY == "5A"))
  cbs <- subset(cbs, (CBS_BANK_TYPE == "4R"))
  cbs <- subset(cbs, (CBS_BASIS == "U"))
  # Reclassify other potential exposures as sectors
  cbs$L_CP_SECTOR[(cbs$L_INSTR == "V")]    <- "V"
  cbs$L_INSTR[(cbs$L_INSTR == "V")]        <- "A"
  cbs$L_CP_SECTOR[(cbs$L_POSITION == "W")] <- "W"
  cbs$L_POSITION[(cbs$L_POSITION == "W")]  <- "C"
  cbs$L_CP_SECTOR[(cbs$L_POSITION == "X")] <- "X"
  cbs$L_POSITION[(cbs$L_POSITION == "X")]  <- "C"
  cbs <- subset(cbs, (L_POSITION == "C"))
  cbs <- subset(cbs, (L_INSTR == "A"))
  cbs <- subset(cbs, (REM_MATURITY == "A"))
  cbs <- subset(cbs, (CURR_TYPE_BOOK) == "TO1") 
  cbs <- subset(cbs, (L_CP_SECTOR %in% c("B", "R", "O", "V", "W", "X")))
  cbs <- subset(cbs, (L_CP_COUNTRY == ctr))
  cbs <- cbs[, c("L_CP_SECTOR", grep("^X.*", colnames(cbs), value = TRUE))]
  cbs <- melt(cbs, id = "L_CP_SECTOR")
  cbs <- subset(cbs, !(is.na(value)))
  cbs$date <- as.Date(as.yearqtr(gsub("\\.", " ",
                                      gsub("^X", "", cbs$variable))))
  
  # Set magnitude to 0
  cbs$value <- cbs$value * 10^6
  
  cbs <- subset(cbs, (!is.na(value)))
  cbs <- dcast(cbs, "date ~ L_CP_SECTOR")
  cbs <- xts(cbs[, -1, drop = FALSE], order.by = cbs[, 1])
  return(cbs)
}

### Debt securities statistics, resident issuers #####
GetSecDataRes <- function(sec, ctr) {
  # Total debt securities
  dds <- sec
  dds <- subset(dds, (ISSUER_RES == ctr))
  dds <- subset(dds, (ISSUER_NAT == "3P"))
  dds <- subset(dds, (ISSUER_BUS_IMM == "1"))
  dds <- subset(dds, (ISSUER_BUS_ULT == "1"))
  dds <- subset(dds, (MARKET %in% c("1", "C")))
  dds <- subset(dds, (ISSUE_TYPE == "A"))
  dds <- subset(dds, (ISSUE_CUR_GROUP == "A"))
  dds <- subset(dds, (ISSUE_CUR == "TO1"))
  if (!(ctr %in% dds$ISSUER_RES)) {
    return(NULL)
  }
  dds <- subset(dds, (ISSUE_OR_MAT == "A"))
  dds <- subset(dds, (ISSUE_RE_MAT == "A"))
  dds <- subset(dds, (ISSUE_RATE == "A"))
  dds <- subset(dds, (ISSUE_RISK == "A"))
  dds <- subset(dds, (ISSUE_COL == "A"))
  dds <- subset(dds, (MEASURE == "I"))
  dds <- dds[, c("MARKET", grep("^X.*", colnames(dds), value = TRUE))]
  dds <- melt(dds, id = "MARKET")
  dds <- subset(dds, !(is.na(value)))
  dds$date <- as.Date(as.yearqtr(gsub("\\.", " ",
                                      gsub("^X", "", dds$variable))))
  
  # Set magnitude to 0
  dds$value <- dds$value * 10^6
  
  dds <- dcast(dds, "date ~ MARKET")
  if (ncol(dds) <= 2) {
    return(NULL)
  }
  dds$DDS <- dds[, "1"] - dds[, "C"]
  dds <- dds[, c("date", "DDS")]
  
  # International debt securities
  ids <- sec
  ids <- subset(ids, (ISSUER_RES == ctr))
  ids <- subset(ids, (ISSUER_NAT == "3P"))
  ids <- subset(ids, (ISSUER_BUS_IMM == "1"))
  ids <- subset(ids, (ISSUER_BUS_ULT == "1"))
  ids <- subset(ids, (MARKET == "C"))
  ids <- subset(ids, (ISSUE_TYPE == "A"))
  ids <- subset(ids, (ISSUE_CUR_GROUP %in% c("D", "F")))
  # Reclassify currency group as issue currency
  ids$ISSUE_CUR[(ids$ISSUE_CUR_GROUP == "D")] <- "D"
  ids <- subset(ids, (ISSUE_CUR %in% c("TO1", "USD", "EU1", "D")))
  ids <- subset(ids, (ISSUE_OR_MAT == "A"))
  ids <- subset(ids, (ISSUE_RE_MAT == "A"))
  ids <- subset(ids, (ISSUE_RATE == "A"))
  ids <- subset(ids, (ISSUE_RISK == "A"))
  ids <- subset(ids, (ISSUE_COL == "A"))
  ids <- subset(ids, (MEASURE == "I"))
  if (!(ctr %in% ids$ISSUER_RES)) {
    return(NULL)
  }
  ids <- ids[, c("ISSUE_CUR", grep("^X.*", colnames(ids), value = TRUE))]
  ids <- melt(ids, id = "ISSUE_CUR")
  ids <- subset(ids, !(is.na(value)))
  ids$date <- as.Date(as.yearqtr(gsub("\\.", " ",
                                      gsub("^X", "", ids$variable))))
  
  # Set magnitude to 0
  ids$value <- ids$value * 10^6
  
  ids <- dcast(ids, "date ~ ISSUE_CUR")
  ids[, "F"] <- ids$TO1 - ids$USD - ids$EU1
  ids$TO1 <- NULL
  ids <- ids[, intersect(c("date", "D", "USD", "EU1", "F"),
                         colnames(ids))]
  
  # Merge both into one set
  sec <- merge(x = dds, y = ids, by = "date", all = TRUE)
  sec <- sec[, intersect(c("date", "DDS", "D", "USD", "EU1", "F"),
                         colnames(sec))]
  sec <- xts(sec[, -1, drop = FALSE], order.by = sec[, 1])
  return(sec)
}

### Debt securities statistics, national issuers #####
GetSecDataNatl <- function(sec, ctr) {
  ids <- sec
  ids <- subset(ids, (ISSUER_RES == "3P"))
  ids <- subset(ids, (ISSUER_NAT == ctr))
  ids <- subset(ids, (ISSUER_BUS_IMM == "1"))
  ids <- subset(ids, (ISSUER_BUS_ULT == "1"))
  ids <- subset(ids, (MARKET == "C"))
  ids <- subset(ids, (ISSUE_TYPE == "A"))
  ids <- subset(ids, (ISSUE_CUR_GROUP == "A"))
  ids <- subset(ids, (ISSUE_CUR %in% c("TO1", "USD", "EU1")))
  ids <- subset(ids, (ISSUE_OR_MAT == "A"))
  ids <- subset(ids, (ISSUE_RE_MAT == "A"))
  ids <- subset(ids, (ISSUE_RATE == "A"))
  ids <- subset(ids, (ISSUE_RISK == "A"))
  ids <- subset(ids, (ISSUE_COL == "A"))
  ids <- subset(ids, (MEASURE == "I"))
  if (!(ctr %in% ids$ISSUER_NAT)) {
    return(NULL)
  }
  ids <- ids[, c("ISSUE_CUR", grep("^X.*", colnames(ids), value = TRUE))]
  ids <- melt(ids, id = "ISSUE_CUR")
  ids <- subset(ids, !(is.na(value)))
  ids$date <- as.Date(as.yearqtr(gsub("\\.", " ",
                                      gsub("^X", "", ids$variable))))
  
  # Set magnitude to 0
  ids$value <- ids$value * 10^6
  
  ids <- dcast(ids, "date ~ ISSUE_CUR")
  ids$OTH <- ids$TO1 - ids$USD
  if ("EU1" %in% colnames(ids)) {
    ids$OTH <- ids$OTH - ids$EU1
  }
  ids$TO1 <- NULL
  ids <- ids[, intersect(c("date", "USD", "EU1", "OTH"), colnames(ids))]
  ids <- xts(ids[, -1, drop = FALSE], order.by = ids[, 1])
  return(ids)
}

### International debt securities by currency #####
GetSecDataCurr <- function(sec) {
  sec <- subset(sec, (ISSUER_RES == "3P"))
  sec <- subset(sec, (ISSUER_NAT == "3P"))
  sec <- subset(sec, (ISSUER_BUS_IMM == "1"))
  sec <- subset(sec, (ISSUER_BUS_ULT == "1"))
  sec <- subset(sec, (MARKET == "C"))
  sec <- subset(sec, (ISSUE_TYPE == "A"))
  sec <- subset(sec, (ISSUE_CUR_GROUP == "A"))
  sec <- subset(sec, (ISSUE_CUR %in% c("TO1", "USD", "EUR", "GBP", "JPY")))
  sec <- subset(sec, (ISSUE_OR_MAT == "A"))
  sec <- subset(sec, (ISSUE_RE_MAT == "A"))
  sec <- subset(sec, (ISSUE_RATE == "A"))
  sec <- subset(sec, (ISSUE_RISK == "A"))
  sec <- subset(sec, (ISSUE_COL == "A"))
  sec <- subset(sec, (MEASURE == "I"))
  sec <- sec[, c("ISSUE_CUR", grep("^X.*", colnames(sec), value = TRUE))]
  sec <- melt(sec, id = "ISSUE_CUR")
  sec <- subset(sec, !(is.na(value)))
  sec$date <- as.Date(as.yearqtr(gsub("\\.", " ",
                                      gsub("^X", "", sec$variable))))
  
  # Set magnitude to 0
  sec$value <- sec$value * 10^6
  
  sec <- dcast(sec, "date ~ ISSUE_CUR")
  sec$OTH <- sec$TO1 - sec$USD - sec$EUR - sec$GBP - sec$JPY
  sec$TO1 <- NULL
  sec <- sec[, c("date", "USD", "EUR", "GBP", "JPY", "OTH")]
  sec <- xts(sec[, -1, drop = FALSE], order.by = sec[, 1])
  return(sec)
}

### Exchange-traded derivatives, by currency #####
GetXtdData <- function(xtd, riskcat = "C", mak = 3) {
  xtd <- subset(xtd, (freq == "M"))
  xtd <- subset(xtd, (od_risk_cat == riskcat))
  xtd <- subset(xtd, (od_instr == "A"))
  xtd <- subset(xtd, (xd_exchange == "8A"))
  xtd$date <- as.Date(paste0(xtd$date, "-01"))
  
  # Set magnitude to 0
  xtd$obs_value <- xtd$obs_value * 10^6
  
  xtd <- dcast(xtd, "date ~ issue_cur", value.var = "obs_value")
  
  tnl <- names(sort(colSums(xtd[(nrow(xtd) - 36):nrow(xtd), -1], na.rm = TRUE),
                    decreasing = TRUE)[2:10])
  xtd <- xtd[, c("date", "TO1", tnl)]
  
  # Forex derivatives always have two currencies, need to double total
  if (riskcat == "B") {
    xtd$TO1 <- xtd$TO1 * 2
  }
  
  xtd$OTH <- xtd$TO1 -
    rowSums(xtd[, -which(colnames(xtd) %in% c("date", "TO1"))], na.rm = TRUE)
  xtd$OTH[(xtd$OTH < 0)] <- 0
  xtd$TO1 <- NULL
  xtd <- xts(xtd[, -1, drop = FALSE], order.by = xtd[, 1])
  xtd <- rollmean(xtd, k = mak, align = "right")
  return(xtd)
}

### OTC derivatives outstanding, by risk category #####
GetOtcDataRisk <- function(otc) {
  otc <- subset(otc, (DER_TYPE == "A"))
  otc <- subset(otc, (DER_INSTR == "A"))
  otc <- subset(otc, (DER_RISK %in% c("J", "T", "E", "B", "D", "U")))
  otc <- subset(otc, (DER_SECTOR_CPY == "A"))
  otc <- subset(otc, (DER_CPC == "5J"))
  otc <- subset(otc, (DER_CURR_LEG1 == "TO1"))
  otc <- subset(otc, (DER_CURR_LEG2 == "TO1"))
  otc <- subset(otc, (DER_ISSUE_MAT == "A"))
  otc <- otc[, c("DER_RISK", grep("^X....", colnames(otc), value = TRUE))]
  otc <- melt(otc, id = "DER_RISK")
  otc <- subset(otc, !(is.na(value)))
  otc$date <- gsub("^X", "", otc$variable)
  otc$date <- gsub(".S1", "-01-01", otc$date)
  otc$date <- gsub(".S2", "-07-01", otc$date)
  otc$date <- as.Date(otc$date)
  
  # Set magnitude to 0
  otc$value <- otc$value * 10^6
  
  otc$variable <- NULL
  otc <- dcast(otc, "date ~ DER_RISK")
  otc <- otc[, c("date", "U", "D", "E", "T", "J", "B")]
  otc <- xts(otc[, -1, drop = FALSE], order.by = otc[, 1])
  return(otc)
}

### Gross market value of OTC interest rate derivatives, by currency denomination leg 1 #####
GetOtcDataCurr <- function(otc) {
  otc <- subset(otc, (DER_TYPE == "A"))
  otc <- subset(otc, (DER_INSTR == "A"))
  otc <- subset(otc, (DER_RISK == "D"))
  otc <- subset(otc, (DER_SECTOR_CPY == "A"))
  otc <- subset(otc, (DER_CPC == "5J"))
  otc <- subset(otc, (DER_CURR_LEG2 == "TO1"))
  otc <- subset(otc, (DER_ISSUE_MAT == "A"))
  otc <- otc[, c("DER_CURR_LEG1", grep("^X....", colnames(otc), value = TRUE))]
  otc <- melt(otc, id = "DER_CURR_LEG1")
  otc <- subset(otc, !(is.na(value)))
  otc$date <- gsub("^X", "", otc$variable)
  otc$date <- gsub(".S1", "-01-01", otc$date)
  otc$date <- gsub(".S2", "-07-01", otc$date)
  otc$date <- as.Date(otc$date)
  
  # Set magnitude to 0
  otc$value <- otc$value * 10^6
  
  otc$variable <- NULL
  otc <- dcast(otc, "date ~ DER_CURR_LEG1")
  otc$OTH <- otc$TO1 -
    rowSums(otc[, -which(colnames(otc) %in% c("date", "TO1"))], na.rm = TRUE)
  otc$TO1 <- NULL
  otc <- otc[, c("date", names(sort(colSums(otc[(nrow(otc) - 2):nrow(otc), -1],
                                            na.rm = TRUE), decreasing = TRUE)))]
  otc <- xts(otc[, -1, drop = FALSE], order.by = otc[, 1])
  return(otc)
}

### Outstanding notional amount of CDS, by sector of counterparty #####
GetOtcDataCDSSect <- function(otc) {
  otc <- subset(otc, (DER_TYPE == "A"))
  otc <- subset(otc, (DER_INSTR == "U"))
  otc <- subset(otc, (DER_RISK == "T"))
  otc <- subset(otc, (DER_SECTOR_CPY %in% c("K", "B", "A")))
  otc <- subset(otc, (DER_CPC == "5J"))
  otc <- subset(otc, (DER_SECTOR_UDL == "A"))
  otc <- subset(otc, (DER_CURR_LEG1 == "TO1"))
  otc <- subset(otc, (DER_CURR_LEG2 == "TO1"))
  otc <- subset(otc, (DER_ISSUE_MAT == "A"))
  otc <- subset(otc, (DER_RATING == "A"))
  otc <- otc[, c("DER_SECTOR_CPY", grep("^X....", colnames(otc), value = TRUE))]
  otc <- melt(otc, id = "DER_SECTOR_CPY")
  otc <- subset(otc, !(is.na(value)))
  otc$date <- gsub("^X", "", otc$variable)
  otc$date <- gsub(".S1", "-01-01", otc$date)
  otc$date <- gsub(".S2", "-07-01", otc$date)
  otc$date <- as.Date(otc$date)
  
  # Set magnitude to 0
  otc$value <- otc$value * 10^6
  
  otc$variable <- NULL
  otc <- subset(otc, (!is.na(value)))
  otc <- dcast(otc, "date ~ DER_SECTOR_CPY")
  otc <- xts(otc[, -1, drop = FALSE], order.by = otc[, 1])
  return(otc)
}

### Triennial Survey statistics on forex turnover #####
GetDerData <- function(der) {
  der <- subset(der, grepl("A:Total", der$Derivatives.instrument))
  der <- subset(der, grepl("B:Foreign", der$Derivatives.risk.category))
  der <- subset(der, grepl("5J:All", der$Derivatives.reporting.country))
  der <- subset(der, grepl("A:Total", der$Derivatives.counterparty.sector))
  der <- subset(der, grepl("5J:All", der$Derivatives.counterparty.country))
  der <- subset(der, grepl("TO1:", der$Derivatives.currency.denomination.leg.2))
  der <- subset(der, grepl("3:Total", der$Derivatives.execution.method))
  der <- subset(der, grepl("C:Net", der$Derivatives.basis))
  der <- der[, c("Derivatives.currency.denomination.leg.1",
                 grep("^X....", colnames(der), value = TRUE))]
  der <- melt(der, id = "Derivatives.currency.denomination.leg.1")
  der <- subset(der, !(is.na(value)))
  der$date <- as.Date(paste0(gsub("^X", "", der$variable), "-01-01"))
  
  # Set magnitude to 0
  der$value <- der$value * 10^6
  
  der <- subset(der, (!is.na(value)))
  der$Derivatives.currency.denomination.leg.1 <-
    substr(der$Derivatives.currency.denomination.leg.1, 1, 3)
  der <- subset(der, (!is.na(value)))
  der <- dcast(der, "date ~ Derivatives.currency.denomination.leg.1")
  tnl <- c(names(sort(colSums(der[(nrow(der)-2):nrow(der), -1], na.rm = TRUE),
                      decreasing = TRUE)[2:10]), "DEM", "FRF")
  der <- der[, c("date", "TO1", tnl)]
  der$OTH <- der$TO1 * 2 -
    rowSums(der[, -which(colnames(der) %in% c("date", "TO1"))], na.rm = TRUE)
  der$TO1 <- NULL
  
  der <- xts(der[, -1, drop = FALSE], order.by = der[, 1])
  return(der)
}

### Global liquidity indicators #####
GetGliData <- function(gli, curr = "USD") {
  gli$date <- as.Date(as.yearqtr(gsub("-", "", gli$date)))
  gli <- subset(gli, (curr_denom == curr))
  gli <- subset(gli, (borrowers_cty == "3P"))
  gli <- subset(gli, (borrowers_sector == "N"))
  gli <- subset(gli, (lenders_sector == "A"))
  gli <- subset(gli, (l_pos_type == "I"))
  gli <- subset(gli, (l_instr == "B"))
  gli <- subset(gli, (unit_measure == curr))
  
  # Set magnitude to 0
  gli$obs_value <- gli$obs_value * 10^6
  
  gli <- dcast(gli, "date ~ curr_denom", value.var = "obs_value")
  gli <- xts(gli[, -1, drop = FALSE], order.by = gli[, 1])
  return(gli)
}

### Total credit to the private non-financial sector #####
GetCreData <- function(cre, ctr, refctr) {
  if (!(ctr %in% cre$borrowers_cty)) {
    return(NULL)
  }
  cre$date <- as.Date(as.yearqtr(gsub("-", "", cre$date)))
  cre <- subset(cre, (borrowers_cty %in% unique(c(ctr, refctr))))
  cre <- subset(cre, (tc_lenders == "A"))
  cre <- subset(cre, (tc_borrowers == "P"))
  cre <- subset(cre, (tc_adjust == "A"))
  cre <- subset(cre, (unit_type == 770))
  cre <- dcast(cre, "date ~ borrowers_cty", value.var = "obs_value")
  cre <- xts(cre[, -1, drop = FALSE], order.by = cre[, 1])
  return(cre)
}

### Total credit to the private non-financial sector: Households, nonfinancial corportions #####
GetCreDataSect <- function(cre, ctr, data) {
  if (!(ctr %in% cre$borrowers_cty)) {
    return(NULL)
  }
  cre$date <- as.Date(as.yearqtr(gsub("-", "", cre$date)))
  cre <- subset(cre, (borrowers_cty == ctr))
  cre <- subset(cre, (tc_lenders == "A"))
  cre <- subset(cre, (tc_borrowers %in% c("H", "N")))
  cre <- subset(cre, (tc_adjust == "A"))
  cre <- subset(cre, (unit_type == 770))
  cre <- dcast(cre, "date ~ tc_borrowers", value.var = "obs_value")
  cre <- xts(cre[, -1, drop = FALSE], order.by = cre[, 1])
  return(cre)
}

### Bank share of total credit to the private non-financial sector #####
GetCreDataBankshare <- function(cre, ctr, refctr) {
  if (!(ctr %in% cre$borrowers_cty)) {
    return(NULL)
  }
  cre$date <- as.Date(as.yearqtr(gsub("-", "", cre$date)))
  cre <- subset(cre, (borrowers_cty %in% unique(c(ctr, refctr))))
  cre <- subset(cre, (tc_lenders %in% c("A", "B")))
  cre <- subset(cre, (tc_borrowers == "P"))
  cre <- subset(cre, (tc_adjust == "A"))
  cre <- subset(cre, (unit_type == 770))
  cre <- dcast(cre, "date + borrowers_cty ~ tc_lenders",
               value.var = "obs_value")
  cre$R <- cre$B / cre$A * 100
  cre <- dcast(cre, "date ~ borrowers_cty", value.var = "R")
  cre <- xts(cre[, -1, drop = FALSE], order.by = cre[, 1])
  return(cre)
}

### Credit-to-GDP gap #####
GetCtgData <- function(ctg, ctr, refctr) {
  if (!(ctr %in% ctg$borrowers_cty)) {
    return(NULL)
  }
  ctg$date <- as.Date(as.yearqtr(gsub("-", "", ctg$date)))
  ctg <- subset(ctg, (borrowers_cty %in% unique(c(ctr, refctr))))
  ctg <- subset(ctg, (cg_dtype == "C"))
  ctg <- dcast(ctg, "date ~ borrowers_cty", value.var = "obs_value")
  ctg <- xts(ctg[, -1, drop = FALSE], order.by = ctg[, 1])
  return(ctg)
}

### Credit-to-GDP actual & trend #####
GetCtgDataActualtrend <- function(ctg, ctr) {
  if (!(ctr %in% ctg$borrowers_cty)) {
    return(NULL)
  }
  ctg$date <- as.Date(as.yearqtr(gsub("-", "", ctg$date)))
  ctg <- subset(ctg, (borrowers_cty == ctr))
  ctg <- subset(ctg, (cg_dtype %in% c("A", "B")))
  ctg <- dcast(ctg, "date ~ cg_dtype", value.var = "obs_value")
  ctg <- xts(ctg[, -1, drop = FALSE], order.by = ctg[, 1])
  return(ctg)
}

### Debt service ratio, international comparison #####
GetDsrData <- function(dsr, ctr, refctr) {
  if (!(ctr %in% dsr$borrowers_cty)) {
    return(NULL)
  }
  dsr$date <- as.Date(as.yearqtr(gsub("-", "", dsr$date)))
  dsr <- subset(dsr, (borrowers_cty %in% unique(c(ctr, refctr))))
  dsr <- subset(dsr, (dsr_borrowers == "P"))
  dsr <- dcast(dsr, "date ~ borrowers_cty", value.var = "obs_value")
  dsr <- xts(dsr[, -1, drop = FALSE], order.by = dsr[, 1])
  return(dsr)
}

### Domestic debt service ratios, households and corporations #####
GetDsrDataSect <- function(dsr, ctr, refctr) {
  if (!(ctr %in% dsr$borrowers_cty)) {
    return(NULL)
  }
  dsr$date <- as.Date(as.yearqtr(gsub("-", "", dsr$date)))
  dsr <- subset(dsr, (borrowers_cty == ctr))
  dsr <- subset(dsr, (dsr_borrowers %in% c("H", "N")))
  if (!(ctr %in% dsr$borrowers_cty)) {
    return(NULL)
  }
  dsr <- dcast(dsr, "date ~ dsr_borrowers", value.var = "obs_value")
  dsr <- xts(dsr[, -1, drop = FALSE], order.by = dsr[, 1])
  return(dsr)
}

### Property prices, long and selected series combined
GetSppData <- function(spp, lpp, ctr, refctr) {
  lpp$date <- as.Date(as.yearqtr(gsub("-", "", lpp$date)))
  lpp <- dcast(lpp, "date ~ ref_area", value.var = "obs_value")
  lpp <- xts(lpp[, -1, drop = FALSE], order.by = lpp[, 1])
  coredata(lpp) <- sapply(lpp, FUN = function(x) coredata(x)*100 /
                            mean(x["2010", ]))
  
  spp$date <- as.Date(as.yearqtr(gsub("-", "", spp$date)))
  spp <- subset(spp, (!(ref_area %in% colnames(lpp))))
  spp <- subset(spp, (value == "N"))
  spp <- subset(spp, (unit_measure == 628))
  spp <- dcast(spp, "date ~ ref_area", value.var = "obs_value")
  spp <- xts(spp[, -1, drop = FALSE], order.by = spp[, 1])
  
  pp <- merge.xts(lpp, spp, all = TRUE)
  if (!(ctr %in% colnames(pp))) {
    return(NULL)
  }
  pp <- pp[, intersect(unique(c(ctr, refctr)), colnames(pp))]
  pp <- pp[, sort(colnames(pp))]
  return(pp)
}

### Consumer prices #####
GetCpiData <- function(cpi, ctr, refctr) {
  if (!(ctr %in% cpi$ref_area)) {
    return(NULL)
  }
  cpi <- subset(cpi, (freq == "M"))
  cpi$date <- as.Date(as.yearmon(cpi$date))
  cpi <- subset(cpi, (ref_area %in% unique(c(ctr, refctr))))
  cpi <- subset(cpi, (unit_measure == 771))
  cpi <- dcast(cpi, "date ~ ref_area", value.var = "obs_value")
  cpi <- xts(cpi[, -1, drop = FALSE], order.by = cpi[, 1])
  return(cpi)
}

### USD exchange rates #####
GetXruData <- function(xru, ctr, refctr) {
  if (!(ctr %in% substr(eer$reference_area, 1, 2))) {
    return(NULL)
  }
  xru$date <- as.Date(xru$date)
  xru$reference_area <- substr(xru$reference_area, 1, 2)
  xru <- subset(xru, (reference_area %in% unique(c(ctr, refctr))))
  xru <- dcast(xru, "date ~ reference_area", value.var = "obs_value")
  xru$JP <- xru$JP / 100
  xru <- xts(xru[, -1, drop = FALSE], order.by = xru[, 1])
  xru <- na.approx(xru, na.rm = FALSE)
  xru <- apply.monthly(xru, mean)
  return(xru)
}

### Nominal effective exchange rate, narrow #####
GetEerData <- function(eer, ctr, refctr) {
  if (!(ctr %in% substr(eer$reference_area, 1, 2))) {
    return(NULL)
  }
  eer$date <- as.Date(eer$date)
  eer$reference_area <- substr(eer$reference_area, 1, 2)
  eer <- subset(eer, (reference_area %in% unique(c(ctr, refctr))))
  eer <- subset(eer, (basket == "N:Narrow (27 economies)"))
  eer <- dcast(eer, "date ~ reference_area", value.var = "obs_value")
  eer <- xts(eer[, -1, drop = FALSE], order.by = eer[, 1])
  eer <- na.approx(eer, na.rm = FALSE)
  eer <- apply.monthly(eer, mean)
  return(eer)
}

### Central bank policy rates #####
GetCbpData <- function(cbp, ctr, refctr) {
  if (!(ctr %in% cbp$ref_area)) {
    return(NULL)
  }
  cbp$date <- as.Date(as.yearmon(cbp$date))
  cbp <- subset(cbp, (ref_area %in% unique(c(ctr, refctr))))
  cbp <- dcast(cbp, "date ~ ref_area", value.var = "obs_value")
  cbp <- xts(cbp[, -1, drop = FALSE], order.by = cbp[, 1])
  return(cbp)
}

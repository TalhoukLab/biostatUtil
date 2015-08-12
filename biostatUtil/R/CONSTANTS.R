## CONSTANTS ##
# Dates
MM.DD.YYYY <- "%m/%d/%Y"
DD.MM.YYYY <- "%d/%m/%Y"
DD_MMM_YY  <- "%d-%b-%y"
YYYY_MM_DD <- "%Y-%m-%d"
YYYYMMDD   <- "%Y%m%d"
DDMMYYYY   <- "%d%m%Y"
MMDDYYYY   <- "%m%d%Y"
DATE.ORIGIN <- as.Date("1970-01-01")
NUM.DAYS.IN.YEAR <- 365.241 #365.25
NUM.DAYS.IN.MONTH <- 30.5

# Styles
COL.TH.STYLE <- "border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;"
ROW.TH.STYLE <- "text-align: center; padding-right:10px; padding-right:10px;"
TABLE.CAPTION.STYLE <- "display: table-caption; text-align: left;"

ROW.TD.STYLE.FOR.MULTI.COX <- "border-bottom: 1px solid grey; text-align: center; padding-right:10px; padding-right:10px;"
ROW.TD.STYLE.FOR.MULTI.COX.ALIGN.TOP <- "border-bottom: 1px solid grey; text-align: center; vertical-align: text-top; padding-right:10px; padding-right:10px;"

# Values
VALUE.CODING.INIT.TREATMENT.NO <- "No treatment"
VALUE.CODING.INIT.TREATMENT.CHEMO.ONLY <- "chemo.only"
VALUE.CODING.INIT.TREATMENT.RT.ONLY <- "rt.only"
VALUE.CODING.INIT.TREATMENT.VAG.BRACHY.ONLY <- "vag.brachy.only"
VALUE.CODING.INIT.TREATMENT.BOTH <- "both"

# Missing codes
MISSING.EXPLICIT <- "N/A" # missing value code for values that are explicitily indicated as missing from data source e.g. "X" in grade
MISSING.UNK <- "Unk" # missing because values was not found (e.g. in data files) but the value must exist somewhere.
MISSING...NOT.FOUND.IN.DATA.FILE <- "" # data point not mentioned in data file.
MISSING.BIOMARKER.EXPLICIT <- MISSING.UNK # missing value code for values that are explicitily indicated as missing from data source e.g. "X" in grade
MISSING.BIOMARKER...NOT.FOUND.IN.DATA.FILE <- "" # data point not mentioned in data file.
ALL.MISSING.CODES <- unique(c(MISSING.EXPLICIT, MISSING...NOT.FOUND.IN.DATA.FILE, MISSING.UNK, MISSING.BIOMARKER.EXPLICIT, MISSING.BIOMARKER...NOT.FOUND.IN.DATA.FILE))

# Labels
FIRTH.THRESHOLD <- 0.8 # percent of censor cases to use Firth correction in Cox model
FIRTH.CAPTION <- "<sup>F</sup>" # text to indicate values are Firth corrected

BCSS.TITLE <- "Breast cancer specific survival"
BCSS.XLAB  <- "Total follow-up (years)"
BCSS.YLAB  <- "Cumulative breast cancer specific survival (BCSS)"

DSS.TITLE <- "Disease specific survival (DSS)"
DSS.XLAB  <- BCSS.XLAB
DSS.YLAB  <- DSS.TITLE

OS.TITLE <- "Overall survival"
OS.XLAB <- DSS.XLAB
OS.YLAB <- OS.TITLE

RFS.TITLE <- "Any relapse-free survival"
RFS.XLAB <- paste(RFS.TITLE,"time")
RFS.YLAB <- RFS.TITLE

DRFS.TITLE <- "Distant relapse-free survival"
DRFS.XLAB <- paste(DRFS.TITLE,"time")
DRFS.YLAB <- DRFS.TITLE

LRFS.TITLE <- "Rocal relapse-free survival"
LRFS.XLAB <- paste(LRFS.TITLE,"time")
LRFS.YLAB <- LRFS.TITLE

RRFS.TITLE <- "regional relapse-free survival"
RRFS.XLAB <- paste(RRFS.TITLE,"time")
RRFS.YLAB <- RRFS.TITLE

LRRFS.TITLE <- "Locoregional relapse-free survival"
LRRFS.XLAB <- paste(LRRFS.TITLE,"time")
LRRFS.YLAB <- LRRFS.TITLE
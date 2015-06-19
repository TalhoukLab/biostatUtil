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

# Missing codes
MISSING.EXPLICIT <- "N/A" # missing value code for values that are explicitily indicated as missing from data source e.g. "X" in grade
MISSING.UNK <- "Unk" # missing because values was not found (e.g. in data files) but the value must exist somewhere.
MISSING...NOT.FOUND.IN.DATA.FILE <- "" # data point not mentioned in data file.
MISSING.BIOMARKER.EXPLICIT <- MISSING.UNK # missing value code for values that are explicitily indicated as missing from data source e.g. "X" in grade
MISSING.BIOMARKER...NOT.FOUND.IN.DATA.FILE <- "" # data point not mentioned in data file.
ALL.MISSING.CODES <- unique(c(MISSING.EXPLICIT, MISSING...NOT.FOUND.IN.DATA.FILE, MISSING.UNK, MISSING.BIOMARKER.EXPLICIT, MISSING.BIOMARKER...NOT.FOUND.IN.DATA.FILE))

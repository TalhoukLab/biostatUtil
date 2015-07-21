# POLE paper - Remark #12 (remark diagram)
# 
# Author: samuelc
###############################################################################
if (!exists("RUN.IN.MARKDOWN")) {
  # if variable does not exist, that means we are not running in the markdown file
  RUN.IN.MARKDOWN <- FALSE # indicate whether we are running the files in the marked down file
  # do set up if not fun in markdown
  # this is for code development & testing ... so assume this will never get called from 
  # Aline's computer
  source('SetUp_db.R')
}

# find surgery years ranges for different tma's
if (!RUN.IN.MARKDOWN) {
  for (tma.name in names(table(emdb.w.duplicate$TMA))) {
    cat(tma.name, ":",
        minDateArray(emdb.w.duplicate$Date.of.Surgery.mm.dd.yyyy
                     [emdb.w.duplicate$TMA == tma.name], 
                     existing.missing.codes = ALL.MISSING.CODES, 
                     return.missing.code = NA),
        "-",
        maxDateArray(emdb.w.duplicate$Date.of.Surgery.mm.dd.yyyy
                     [emdb.w.duplicate$TMA == tma.name],
                     existing.missing.codes = ALL.MISSING.CODES,
                     return.missing.code = NA),
        "\n"
    )
  }
}

# TMA breakdown
temp.d <- cbind(emdb.w.duplicate, "TMA_wSurgYrs" = "",
                stringsAsFactors = FALSE)

# remove all POLE missing (data from 2014-11-08) ...
# do not use POLE.mut.consolidated.numeric because we want to see
# which cases on the physical block (i.e. belonging to which TMA)
# has POLE data.
temp.d <- temp.d[!temp.d$POLE.mut.2014.11.08.numeric %in% ALL.MISSING.CODES, ]
# per meeting with Aline 2015-01-13, do not show the 2 duplicate case ... just 
# show the unique 406 cases
temp.d <- temp.d[temp.d$comment.about.duplicate != "first occurrence", ]
for (tma.name in names(table(emdb.w.duplicate$TMA))) {
  temp.d$TMA_wSurgYrs[temp.d$TMA == tma.name] <- paste0(
    tma.name, "<br><i style='font-size:10px'>surgery years: ",
    substr(minDateArray(emdb.w.duplicate$Date.of.Surgery.mm.dd.yyyy
                        [emdb.w.duplicate$TMA == tma.name],
                        existing.missing.codes = ALL.MISSING.CODES,
                        return.missing.code = NA), 7, 10),
    "-",
    substr(maxDateArray(emdb.w.duplicate$Date.of.Surgery.mm.dd.yyyy
                        [emdb.w.duplicate$TMA == tma.name],
                        existing.missing.codes = ALL.MISSING.CODES,
                        return.missing.code = NA), 7, 10),
    "; ",
    ifelse(tma.name %in% c("09-004 (Endometrial carcinoma)",
                           "10-005 (High grade endometrioid)",
                           "10-006 (High grade serous)"), "FFPE", "frozen"),
    " sample sequenced",
    "</i>")
}

item12.tma.breakdown <- 
  doCohortCharacteristics(temp.d,
                          "POLE.mut.germline.as.missing",
                          "POLE",
                          c("TMA_wSurgYrs"), 
                          c(FALSE), 
                          c("tissue microarray"),
                          caption = "Distribution of POLE wild type / mutated
                          case among TMA's (includes cases represented in
                          multiple TMA's)",
                          show.missing = FALSE,
                          banded.rows = FALSE)

## execute the following line to show the number of duplicate cases on each TMA
if (!RUN.IN.MARKDOWN) {
  print(emdb.w.duplicate[emdb.w.duplicate$Study.Identifier %in% 
                           emdb.w.duplicate$Study.Identifier
                         [emdb.w.duplicate$comment.about.duplicate ==
                           "first occurrence"],
                         c("TMA","comment.about.duplicate")])
}
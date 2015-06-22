# POLE paper - Remark #13 (cohort characteristics)
# 
# Author: samuelc
###############################################################################
if (!exists("RUN.IN.MARKDOWN")) {
	# if variable does not exist, that means we are not running in the markdown file
	RUN.IN.MARKDOWN <- FALSE # indicate whether we are running the files in the marked down file
	# do set up if not fun in markdown
	# this is for code development & testing ... so assume this will never get called from 
	# Aline's computer
	source('remark_setup.R')
}

### Distribution of POLE with other variables ... raw categories when possible
# per jessica email 2014-11-26: 
# Age
# BMI
# Stage (Will group togethers for all stage I, stage II etc-not substages)
# Grade
# Histol
# LVSI
# any positive nodes
# NO treatment
# RT only
# Chemo only
# Both
# Unknown treatment
# MMR IHC loss
# MMR IHC intact  (still deciding if we will share this in this paper)

missing.codes.highlights <- list(
		"age.at.surgery" = NA,
		"bmi" = NA,
		"stage" = NA,
		"Tumour.Grade" = NA,
		"Histological.Subtype" = NA,
		"grade_x_endo" = NA,
		"LVSI" = NA,
		"any.positive.nodes.for.cohort.char" = "No lymph node dissection performed",
		"init.treatment" = NA)

# per email 2015-05-01, want to show cohort characteristics with missing category calculated as port of the percentages
# email from Jessica 2015-05-04
# ... therefore, format emdb so that the missing values are no longer part of ALL.MISSING.CODES ... BUT NA will always remain missing!!!
emdb.for.cohort.char <- emdb

emdb.for.cohort.char$stage <- sapply(emdb.for.cohort.char$stage, as.character)
factor.levels <- names(table(emdb.for.cohort.char$stage))
factor.levels <- c(factor.levels[factor.levels != MISSING.EXPLICIT],
                   MISSING.EXPLICIT)
emdb.for.cohort.char$stage <- factor(emdb.for.cohort.char$stage,
                                     levels = factor.levels)

emdb.for.cohort.char$LVSI <- sapply(emdb.for.cohort.char$LVSI, as.character)
factor.levels <- names(table(emdb.for.cohort.char$LVSI))
factor.levels <- c(factor.levels[factor.levels != MISSING.EXPLICIT],
                   MISSING.EXPLICIT)
emdb.for.cohort.char$LVSI <- factor(emdb.for.cohort.char$LVSI,
                                    levels = factor.levels)

emdb.for.cohort.char$any.positive.nodes.for.cohort.char <-
  sapply(emdb.for.cohort.char$any.positive.nodes.for.cohort.char, as.character)
factor.levels <- c(names(table(emdb.for.cohort.char$any.positive.nodes.for.cohort.char)),
                   MISSING.EXPLICIT)
emdb.for.cohort.char$any.positive.nodes.for.cohort.char[is.na(
  emdb.for.cohort.char$any.positive.nodes.for.cohort.char)] <- MISSING.EXPLICIT
emdb.for.cohort.char$any.positive.nodes.for.cohort.char <-
  factor(emdb.for.cohort.char$any.positive.nodes.for.cohort.char,
         levels = factor.levels)

emdb.for.cohort.char$init.treatment <-
  sapply(emdb.for.cohort.char$init.treatment, as.character)
factor.levels <- c(names(table(emdb.for.cohort.char$init.treatment)),
                   MISSING.EXPLICIT)
emdb.for.cohort.char$init.treatment[emdb.for.cohort.char$init.treatment 
                                    == ""] <- MISSING.EXPLICIT
emdb.for.cohort.char$init.treatment <-
  factor(emdb.for.cohort.char$init.treatment, levels = factor.levels)

item13.cohort.characteristics <-
  doCohortCharacteristics(emdb.for.cohort.char, "POLE.mut.germline.as.missing",
                          "POLE", 
                          c("age.at.surgery", "bmi", "stage", "Tumour.Grade",
                            "Histological.Subtype", "grade_x_endo", "LVSI",
                            "any.positive.nodes.for.cohort.char",
                            "init.treatment"),
                          c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
                            FALSE, FALSE), 
                          c("Age at surgery", "BMI", "Stage", "Grade",
                            "Histological subtype",
                            "Grade x Histological subtype",
                            "Lymphovascular invasion", "Any positive nodes",
                            "Initial adjuvant treatment"),
                          show.missing = FALSE,
                          missing.codes.highlight = NULL,  # missing.codes.highlights,
                          missing.codes = NA,  # want to show missing in percentages
                          caption = "Cohort characteristics - whole cohort",
                          banded.rows = TRUE)

# a version of cohort charactistics with any adjuvant treatment
item13.cohort.characteristics.any.tx <-
  doCohortCharacteristics(emdb.for.cohort.char[
    !emdb$init.treatment %in% ALL.MISSING.CODES &
      emdb$init.treatment != "no.treatment", ],
    "POLE.mut.germline.as.missing", "POLE", 
    c("age.at.surgery", "bmi", "stage", "Tumour.Grade", "Histological.Subtype",
      "grade_x_endo", "LVSI", "any.positive.nodes.for.cohort.char",
      "init.treatment"), 
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), 
    c("Age at surgery", "BMI", "Stage", "Grade", "Histological subtype",
      "Grade x Histological subtype", "Lymphovascular invasion",
      "Any positive nodes", "Initial adjuvant treatment"),
    show.missing = FALSE,
    missing.codes.highlight = NULL,  # missing.codes.highlights,
    missing.codes = NA,  # want to show missing in percentages
    caption = "Cohort characteristics - any adjuvant treatment",
    banded.rows = TRUE)

# a version of cohort charactistics with no adjuvant treatment
item13.cohort.characteristics.no.tx <-
  doCohortCharacteristics(emdb.for.cohort.char[
    !emdb$init.treatment %in% ALL.MISSING.CODES &
      emdb$init.treatment == "no.treatment", ],
    "POLE.mut.germline.as.missing", "POLE", 
	c("age.at.surgery", "bmi", "stage", "Tumour.Grade", "Histological.Subtype",
	  "grade_x_endo", "LVSI", "any.positive.nodes.for.cohort.char",
	  "init.treatment"), 
	c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), 
	c("Age at surgery", "BMI", "Stage", "Grade", "Histological subtype",
	  "Grade x Histological subtype", "Lymphovascular invasion",
	  "Any positive nodes","Initial adjuvant treatment"),
	show.missing = FALSE,
	missing.codes.highlight = NULL,  # missing.codes.highlights,
	missing.codes = NA,  # want to show missing in percentages
	caption = "Cohort characteristics - no adjuvant treatment",
	banded.rows = TRUE)

item13.cohort.missing <-
  doCohortCharacteristics(emdb, "POLE.mut.germline.as.missing", "POLE",
	c("age.at.surgery_bXvS", "bmi_bXvS", "stage_bXvS", "Tumour.Grade_bXvS",
	  "Histological.Subtype_bXvS", "LVSI_bXvS", "any.positive.nodes_bXvS",
	  "any.init.treatment_bXvS"),
	c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), 
	c("Age at surgery", "BMI", "Stage", "Grade", "Histological subtype",
	  "Lymphovascular invasion", "Any positive nodes",
	  "Initial adjuvant treatment"),
	stat.tests = c("fisher", "fisher", "fisher", "fisher",
	               "fisher", "fisher", "fisher", "fisher"),
	stat.test.column.header = "association test",
	do.droplevels = FALSE,
	caption = "Missing value analysis",
	banded.rows = TRUE,
	show.missing = FALSE)
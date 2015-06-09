# POLE paper - Remark #8
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

item8.frequencies.table.pole <- single.row.percent.as.html(
	table(emdb$POLE.mut.consolidated.numeric),
	digits=NUM.DIGITS.TO.SHOW,
	pretty.text=TRUE,
	show.count=TRUE,
	column.names=c(MISSING.BIOMARKER.EXPLICIT,"wild type","mutated","germline mutation"))

# clinical params ...
# per jessica email 2014-11-26: ANY LYMPH NODE DISSECTION  to tables ie LND yes/no
# per jessica email 2014-11-26: 
# Age
# BMI
# Stage (Will group togethers for all stage I, stage II etc-not substages)
# Grade
# Histol
# LVSI
# NO treatment
# RT only
# Chemo only
# Both
# Unknown treatment
# Median Followup  (for whole cohort, POLE mt POLE wt)
# Median PFS  "
# Median OS  "  "
# Median DSS
# MMRIHC loss
# MMR IHC intact  (still deciding if we will share this in this paper)

# Age
item8.frequencies.table.age <- summary.as.html(emdb$age.at.surgery)
# do histogram in markup file

# BMI
item8.frequencies.table.bmi <- summary.as.html(emdb$bmi)

# Stage (Will group togethers for all stage I, stage II etc-not substages)
item8.frequencies.table.stage <- single.row.percent.as.html(
		table(emdb$stage),
		digits=NUM.DIGITS.TO.SHOW,
		pretty.text=TRUE,
		show.count=TRUE)

# Grade
item8.frequencies.table.grade <- single.row.percent.as.html(
		table(emdb$Tumour.Grade),
		digits=NUM.DIGITS.TO.SHOW,
		pretty.text=TRUE,
		show.count=TRUE)

# Histol
item8.frequencies.table.hist.detail <- single.row.percent.as.html(
		table(emdb$Histological.Subtype),
		digits=NUM.DIGITS.TO.SHOW,
		pretty.text=TRUE,
		show.count=TRUE,
		banded.rows=TRUE,
		transpose=TRUE)
item8.frequencies.table.hist.simple <- single.row.percent.as.html(
		table(emdb$hist.simple),
		digits=NUM.DIGITS.TO.SHOW,
		pretty.text=TRUE,
		show.count=TRUE)

# LVSI
item8.frequencies.table.lvi <- single.row.percent.as.html(
		table(emdb$LVSI),
		digits=NUM.DIGITS.TO.SHOW,
		pretty.text=TRUE,
		show.count=TRUE)

# ANY LYMPH NODE DISSECTION 
item8.frequencies.table.any.nodes <- single.row.percent.as.html(
		table(emdb$any.positive.nodes),
		digits=NUM.DIGITS.TO.SHOW,
		pretty.text=TRUE,
		show.count=TRUE)

# NO treatment
# RT only
# Chemo only
# Both
# Unknown treatment
item8.frequencies.table.init.treatment <- single.row.percent.as.html(
	table(emdb$init.treatment),
	digits=NUM.DIGITS.TO.SHOW,
	pretty.text=TRUE,
	show.count=TRUE,
	column.names=c("[blank]",VALUE.CODING.INIT.TREATMENT.NO,
		VALUE.CODING.INIT.TREATMENT.VAG.BRACHY.ONLY,
		VALUE.CODING.INIT.TREATMENT.RT.ONLY,
		VALUE.CODING.INIT.TREATMENT.CHEMO.ONLY,
		VALUE.CODING.INIT.TREATMENT.BOTH))


# MMRIHC loss
# MMR IHC intact  (still deciding if we will share this in this paper)
# LVSI

# POLE paper - Remark #11 (how marker values were handled)
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

# reverse order of excluding duplicates so that 11-010 cases picked instead of
# 10-005 and 10-006
temp.d <- emdb.w.duplicate[emdb.w.duplicate$comment.about.duplicate == "" |
                             emdb.w.duplicate$comment.about.duplicate != 
                             "first occurrence", ]

# want to list the frequences of amino acid changes
pole.mut.amino.acid.changes <- sapply(temp.d$POLE.mut, function(x){
  x <- trimws(x)
  if (x %in% c("", "Wild Type")) {
    return(x)
  } else if (x == "S297A, V411L") {
    return("S297A, V411L")
  } else if (x == "H422Y(40%)- is germline") {
    return("germline")
  } else {
    return(trimws(strsplit(strsplit(x, "\\(")[[1]][1], " ")[[1]][1]))		
  }
})
   
item11.pole.mutation.amino.acid.changes <- rowPercentAsHTML(
	table(pole.mut.amino.acid.changes[!temp.d$POLE.mut.germline.as.missing %in% 
	                                    c(ALL.MISSING.CODES, "wild type")]),
	digits = 2,
	show.count = TRUE,
	caption = "Frequencies of POLE mutation amino acid changes",
	transpose = TRUE,
	banded.rows = TRUE
)
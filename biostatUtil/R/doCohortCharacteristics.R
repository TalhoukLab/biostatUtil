#' Generate cohort characteristics
#' @param input.d: The dataframe containing the data
#' @param marker.name: The variable that you want to split into different columns
#' @param var.names: The variables that you want the statistics for
#' @param is.var.continuous: Vector of length equal to the length of var.names with 1 indicating a continuous variable and 0 otherwise (this should be inferred in the function)
#' @param var.descriptions: Vector of strings to describe the variables as they are to appear in the table
#' @param marker.value.labels.tolower: Indicator as to whether to put marker value labels to lower case
#' @param show.missing: an indicator to whether to show missing values
#' @param show.missing.continuous: if set to FALSE and show.missing==FALSE, will not show the number of missing cases for continuous variables,  otherwise, it shows the number of missing for continuous variables even if show.missing==FALSE.
#' @param do.droplevels: drop categories of unobserved levels set to TRUE
#' @param show.percent:  defaults to "both" which shows both rows and columns other possible values: "column", "row".
#' @param stat.tests
#' @param stat.test.column.header: The name to show on the header defaults to "association/correlation test"
#' @param round.digits.p.value: the number of digits to round the P values
#' @param num.boot.for.ci: the number of bootstrap samples for any bootstrap method that may be used
#' @param missing.codes.highlight: default to NULL this indicates whether we wanted the missing values broken down down or lumped together.
#' @param missing.codes a vector to indicate how missing values are coded, default is c("N/A","","Unk")
#' @param decimal Set to 0
#' @param caption is the caption to use for the Table

#' @export
doCohortCharacteristics <- function(input.d, marker.name, marker.description, var.names, is.var.continuous, var.descriptions, marker.value.labels.tolower = TRUE, show.missing = TRUE,show.missing.continuous = TRUE, 
  do.droplevels = TRUE, show.percent = "both", stat.tests = NULL,
  stat.test.column.header = "association/correlation test", round.digits.p.value = 4, num.boot.for.ci = 1000, missing.codes.highlight=NULL, missing.codes=c("N/A","","Unk"), decimal=0, caption=NA, html.table.border=0,
  banded.rows=FALSE, css.class.name.odd="odd", css.class.name.even="even") {
  
col.th.style <- "border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;"
row.th.style <- "text-align: left; padding-right:10px; padding-right:10px;"
  
if (is.null(missing.codes.highlight)) {
     missing.codes.highlight <- list()
    for (var.name in var.names) {
      missing.codes.highlight[var.name] <- NA
    }
  }
  
# input.d.no.missing DEFINED HERE!!!

#Filter Missing Value for the marker but other missing values may still exist
input.d.no.missing <- input.d[!is.na(input.d[,marker.name]) & !input.d[,marker.name] %in% missing.codes, ] 
  
# Droplevels
if (is.factor(input.d.no.missing[,marker.name]) & do.droplevels) {
    input.d.no.missing[, marker.name] <- droplevels(input.d.no.missing[, marker.name])
}
# Formatting Labels
marker.categories <- names(table(input.d.no.missing[, marker.name]))
marker.categories <- marker.categories[!marker.categories %in% missing.codes]
total.label <- ifelse(sum(sapply(var.descriptions, function(x){
    return(ifelse(isFirstLetterUpperCase(x), 1, 0))})) == length(var.descriptions), "Total", "total")

if (marker.value.labels.tolower) {
    result.table.col.names <- c(total.label, paste(marker.description, tolower(marker.categories)))
  } else {
    result.table.col.names <- c(total.label, paste(marker.description, marker.categories))	
  }
  
# If all items in var.descriptions starts with capital letter, first row header should be capital as well i.e. "Total"
result.table.row.names <- total.label
result.table <- c(paste(nrow(input.d)," (100%)", sep = ""),
    sapply(marker.categories, function(x){
      return(paste(sum(input.d.no.missing[, marker.name] == x), " (", round(sum(input.d.no.missing[, marker.name] == x) / nrow(input.d.no.missing) * 100, decimal), "%)", sep = ""))
      })
  )

do.stats <- !is.null(stat.tests)
stat.tests.results <- c()
  
num.var <- length(var.names)

# Loop over all variables
for (i in 1:num.var) {
#############################
# NOTE: three data matrices here ...
# input.d.no.missing - no missing marker only (may contain missing var)
# input.d.no.missing.var.only - missing var only (may contain missing marker)
# input.d.no.missing.var - no missing marker and no missing var
#############################
    
# Header row for each var
result.table <- rbind(result.table, c("", rep("", length(marker.categories))))
result.table.row.names <- c(result.table.row.names, paste("**", var.descriptions[i], "**", sep=""))
var.name <- var.names[i]
var.description <- var.descriptions[i]
    
num.missing.row.header.name <- c()
if (sum(!is.na(missing.codes.highlight[[var.name]])) > 0) {
      num.missing.row.header.name <- c(num.missing.row.header.name, missing.codes.highlight[[var.name]]) # need double [[]] for missing.codes.highlight because its a list
    }
num.missing.row.header.name <- c(num.missing.row.header.name, "Missing")
    
# input.d.no.missing.var.only DEFINED HERE!!!
input.d.no.missing.var.only <- input.d[!is.na(input.d[, var.name]) & !input.d[, var.name] %in% missing.codes & !input.d[, var.name] %in% missing.codes.highlight[[var.name]], ]
if (is.factor(input.d.no.missing.var.only[, var.name]) & do.droplevels) {
      input.d.no.missing.var.only[,var.name] <- droplevels(input.d.no.missing.var.only[, var.name])
}

# input.d.no.missing.var DEFINED HERE!!!
input.d.no.missing.var <- input.d.no.missing.var.only[!is.na(input.d.no.missing.var.only[, marker.name]) & !input.d.no.missing.var.only[, marker.name] %in% missing.codes, ]
if (is.factor(input.d.no.missing.var[, marker.name]) & do.droplevels) {
  input.d.no.missing.var[, marker.name] <- droplevels(input.d.no.missing.var[, marker.name])
}


if (is.var.continuous[i]) { # If continuous variable
input.d.no.missing.var.only[, var.name] <- as.numeric(input.d.no.missing.var.only[, var.name])
input.d.no.missing.var[, var.name] <- as.numeric(input.d.no.missing.var[, var.name])

# 4 rows: mean (+/- std dev) / median / IQR / number of missing
var.row.names <- c("mean","median","interquartile range","range")
if (show.missing | show.missing.continuous) {
        var.row.names <- c(var.row.names, num.missing.row.header.name)
      }
result.table.row.names <- c(result.table.row.names,var.row.names)
      
# Statistical testing for continuous variables (ignore non-applicable tests!) ...
stat.test.result <- NA
if (do.stats) {
switch (stat.tests[i],
 spearman={spearman.result <- cor.test(input.d.no.missing.var[, var.name], 
                          as.numeric(input.d.no.missing.var[, marker.name]),
                          method="spearman")
          stat.test.result <- paste( "Spearman correlation<br>",
                    "rho = ", round(spearman.result$estimate,2),
                    "<br>P = ", sprintf(paste("%.", round.digits.p.value, "f",
                    sep=""), round(spearman.result$p.value, digits = round.digits.p.value)), sep = "")
          },
 kruskal={kruskal.result <- kruskal.test(input.d.no.missing.var[, var.name] ~ 
                                           as.numeric(input.d.no.missing.var[, marker.name]))
          stat.test.result <- paste("Kruskal-Wallis rank sum test<br>", "<br>P = ", 
                    sprintf(paste("%.", round.digits.p.value, "f", sep = ""), 
                            round(kruskal.result$p.value, digits = round.digits.p.value)), sep = "")
          },
 wilcox={wilcox.result <- wilcox.test(input.d.no.missing.var[, var.name] ~ 
                                        as.numeric(input.d.no.missing.var[, marker.name]))
          stat.test.result <- paste("Wilcoxon rank sum test<br>", "P = ",
                    sprintf(paste("%.",round.digits.p.value, "f",sep = ""),
                            round(wilcox.result$p.value,digits = round.digits.p.value)),sep = ""
                  )
          }
  )
 
  stat.tests.results <- c(stat.tests.results, stat.test.result)
}
 
# Assemble Continuous Results     
result.table <- rbind(result.table, # Add the mean
                  c(paste(round(mean(input.d.no.missing.var.only[, var.name]), decimal),
                          round(sem( input.d.no.missing.var.only[, var.name]), decimal),
                              sep=" &#177; "), sapply(marker.categories,function(x){
                              temp.d <- input.d.no.missing.var[input.d.no.missing.var[,marker.name] == x,
                                                               var.name]
                          if (length(temp.d) == 0){
                                  return(MISSING.EXPLICIT)
                                } else {
                                  return(paste(
                                    round(mean(temp.d), decimal),
                                    round(sem( temp.d), decimal),											
                                    sep=" &#177; "
                                  ))		
                                }
                              })
                            ),
                    c( # Add the median
                      round(median(input.d.no.missing.var.only[,var.name]), decimal), 
                        sapply(marker.categories, function(x){
                        temp.d <- input.d.no.missing.var[input.d.no.missing.var[, marker.name] == x, 
                                                         var.name]
                                if (length(temp.d) == 0){
                                  return(MISSING.EXPLICIT)
                                } else {
                                  return(round(median(temp.d), decimal))
                                }
                              })
                            ),
                    c( # Add inter quartile range
                    paste(round(quantile(input.d.no.missing.var.only[, var.name], c(0.25,0.75)),
                                decimal), collapse = " to "), 
                      sapply(marker.categories,function(x){
                        temp.d <- input.d.no.missing.var[input.d.no.missing.var[, marker.name] == x,
                                                         var.name]
                                if (length(temp.d) == 0){
                                  return(MISSING.EXPLICIT)
                                } else {
                                  return(paste(round(quantile(temp.d,c(0.25,0.75)), decimal),
                                               collapse = " to "))		
                                }
                              })
                    ),
                    c( # Add range
                      paste(round(range(input.d.no.missing.var.only[, var.name]),
                                  decimal), collapse = " to "), 
                      sapply(marker.categories,function(x){
                        temp.d <- input.d.no.missing.var[input.d.no.missing.var[, marker.name] == x,
                                                         var.name]
                        if (length(temp.d) == 0){
                          return(MISSING.EXPLICIT)
                        } else {
                          return(paste(round(range(temp.d), decimal),
                                       collapse = " to "))		
                        }
                      }))
      )
    } else { # If categorical variable
var.categories <- names(table(input.d.no.missing.var.only[, var.name]))
var.row.names <- var.categories
if (show.missing) {
        var.row.names <- c(var.row.names, num.missing.row.header.name)
      }
result.table.row.names <- c(result.table.row.names,var.row.names)

# do statistical test for continuous variables (ignore non-applicable tests!) ...
stat.test.result <- NA
if (do.stats) {switch (stat.tests[i],
      kendall = {kendall.result <- cor.test(as.numeric(input.d.no.missing.var[, var.name]),
                                  as.numeric(input.d.no.missing.var[, marker.name]), method = "kendall")
                stat.test.result <- paste("Kendall correlation<br>",
                                  "tau = ", round(kendall.result$estimate, 2),
                                  "<br>P = ", sprintf(paste("%.", round.digits.p.value, "f", sep = ""),
                                 round(kendall.result$p.value, digits = round.digits.p.value)), sep= "" )
                },
      chisq = {chisq.result <- chisq.test(table(input.d.no.missing.var[, var.name],
                    input.d.no.missing.var[, marker.name]))
              stat.test.result <- paste("Chi-square test<br>",
                                  "P = ", sprintf(paste("%.", round.digits.p.value, "f", sep = ""),
                            round(chisq.result$p.value, digits = round.digits.p.value)), sep = "")
                },
      fisher = {fisher.result <- fisher.test(table(input.d.no.missing.var[, var.name],
                    input.d.no.missing.var[, marker.name]), workspace = 2e6)
                  stat.test.result <- paste("Fisher's exact test<br>", "P = ", 
                            sprintf(paste("%.", round.digits.p.value, "f", sep = ""),
                            round(fisher.result$p.value,digits=round.digits.p.value)), sep = "")
                },
      confusionMarkerAsRef={ 
        # confusion matrix, marker as the reference
        # require both marker and var to be factor ...
        # if not, just print err msg
if (!is.factor(input.d.no.missing.var[,var.name]) | !is.factor(input.d.no.missing.var[, marker.name])) {
    stat.test.result <- "error: both marker and variable needs to be factor"
    } else {
    stat.test.result <- confusionResultToHtmlTable(as.numeric(input.d.no.missing.var[, var.name]),
                                                   as.numeric(input.d.no.missing.var[,marker.name]),
                                                   marker.description,round.digits.p.value,
                                                   num.boot.for.ci=num.boot.for.ci)
            }
                            },
      confusionVarAsRef={ 
        # confusion matrix, variable as the reference
        # require both marker and var to be factor ...
        # if not, just print err msg
if (!is.factor(input.d.no.missing.var[, var.name]) | !is.factor(input.d.no.missing.var[, marker.name])) {
    stat.test.result <- "error: both marker and variable needs to be factor"
    } else {
    stat.test.result <- confusionResultToHtmlTable(as.numeric(input.d.no.missing.var[, marker.name]),
                                                   as.numeric(input.d.no.missing.var[, var.name]),
                                                   var.description, round.digits.p.value,
                                                   num.boot.for.ci=num.boot.for.ci)
            }
                          }
)

 stat.tests.results <- c(stat.tests.results, stat.test.result)
}
      
for (var.category in var.categories) {
        total.value <- paste(sum(input.d.no.missing.var.only[, var.name] == var.category), " (",
          round(sum(input.d.no.missing.var.only[, var.name] == var.category) / 
                  nrow(input.d.no.missing.var.only) * 100, decimal), "%)",sep = "")
        result.table <- rbind(result.table,
    switch(show.percent,
    row={c(total.value, sapply(marker.categories, function(x){
    return(paste(sum(input.d.no.missing.var[, var.name] == var.category & 
                       input.d.no.missing.var[,marker.name] == x), " (",
                 ifelse(sum(input.d.no.missing.var[, var.name] == var.category) > 0,
                        round(sum(input.d.no.missing.var[, var.name] == var.category & 
                          input.d.no.missing.var[,marker.name] == x) / 
                            sum(input.d.no.missing.var[, var.name] == x) * 100, decimal),
                            0), 
                 "%)", sep = ""))
      })
    )},
    column={c(total.value, sapply(marker.categories, function(x){
      return(paste(sum(input.d.no.missing.var[, var.name] == var.category & 
                         input.d.no.missing.var[,marker.name] == x), " (",
                   ifelse(sum(input.d.no.missing.var[, var.name] == var.category) > 0,
                          round(sum(input.d.no.missing.var[, var.name] == var.category & 
                                      input.d.no.missing.var[,marker.name] == x) / 
                                  sum(input.d.no.missing.var[, var.name] == var.category) * 100, decimal),
                          0), 
                   "%)", sep = ""))
    })
    )},
    both={c(total.value, sapply(marker.categories,function(x){
                return(paste(sum(input.d.no.missing.var[, var.name] == var.category & 
                                   input.d.no.missing.var[,marker.name] == x),
                                           "<i><br>",
                                           ifelse(
                                             sum(input.d.no.missing.var[,var.name]==var.category) > 0,
                                             round(sum(input.d.no.missing.var[,var.name]==var.category & input.d.no.missing.var[,marker.name]==x)/sum(input.d.no.missing.var[,var.name]==var.category)*100,decimal),
                                             0
                                           ),
                                           "%<br>",
                                           ifelse(
                                             sum(input.d.no.missing.var[,marker.name]==x) > 0,
                                             round(sum(input.d.no.missing.var[,var.name]==var.category & input.d.no.missing.var[,marker.name]==x)/sum(input.d.no.missing.var[,marker.name]==x)*100,decimal),
                                             0
                                           ),
                                           "%</i>",
                                           sep=""
                                         ))
                                       })
                                     )}
                              )
        )
      }
    }
    if (show.missing | is.var.continuous[i]&show.missing.continuous) {
      if (sum(!is.na(missing.codes.highlight[[var.name]]))>0) {
        # there's some missing values we want to highlight ...
        for (missing.code in missing.codes.highlight[[var.name]]) {
          result.table <- rbind(result.table,
                                c( # number of missing
                                  sum(!is.na(input.d[,var.name]) & input.d[,var.name]==missing.code),
                                  sapply(marker.categories,function(x){
                                    temp.d <- input.d.no.missing[input.d.no.missing[,marker.name]==x,var.name]
                                    return(sum(!is.na(temp.d) & temp.d==missing.code))		
                                  })
                                )
          )
        }
      } 
      result.table <- rbind(result.table,
                            c( # number of missing
                              sum(is.na(input.d[,var.name]) | input.d[,var.name]%in%missing.codes),
                              sapply(marker.categories,function(x){
                                temp.d <- input.d.no.missing[input.d.no.missing[,marker.name]==x,var.name]
                                return(sum(is.na(temp.d) | temp.d%in%missing.codes))		
                              })
                            )
      )
    }
  }
  colnames(result.table) <- result.table.col.names
  rownames(result.table) <- result.table.row.names
  
  # generate html table ...	
  result.table.html <- paste("<table border=",html.table.border,">",ifelse(is.na(caption),"",paste("<caption style='",TABLE.CAPTION.STYLE,"'>",addTableNumber(caption),"</caption>",sep="")),sep="")
  result.table.html <- paste(
    result.table.html,
    "<tr><th style='",col.th.style,"' colspan=2></th><th style='",col.th.style,"'>",
    paste(
      result.table.col.names,collapse=paste("</th><th style='",col.th.style,"'>",
                                            sep="")
    ),
    "</th>",
    ifelse(do.stats,paste("<th style='",col.th.style,"'>",stat.test.column.header,"</th>",sep=""),""),
    "</tr>",
    sep=""
  )
  
  row.band.toggle <- TRUE
  var.count <- -1 # want to skip the header row which contains the total count
  for (i in 1:nrow(result.table)) {
    
    num.missing.row.header.name <- c()
    if (var.count > 0) {
      var.name <- var.names[var.count]
      if (sum(!is.na(missing.codes.highlight[[var.name]]))>0) {
        num.missing.row.header.name <- c(num.missing.row.header.name, missing.codes.highlight[[var.name]]) # need double [[]] for missing.codes.highlight because its a list
      }
    }
    num.missing.row.header.name <- c(num.missing.row.header.name,"missing")
    
    
    # check to see if this is the start of a new category or 1st row ...
    var.header.row <- FALSE
    if ( i==1 | sum(result.table[i,]=="",na.rm=TRUE)>1) {
      # this must be the start of a first category since row contains >1 empty cells (there will be 1 empty cell for any var with no stats test)
      var.header.row <- TRUE
      var.count <- var.count+1
      if (i > 1) { # varname not available for i==1 (the row for the total)
        var.name <- var.names[var.count]
      }
      row.band.toggle <- !row.band.toggle
      tr.class <- ifelse(banded.rows,paste(" class='",ifelse(row.band.toggle,css.class.name.even,css.class.name.odd),"'",sep=""),"")
      result.table.html <- paste(
        result.table.html,
        "<tr",tr.class,"><th style='",row.th.style,"' colspan=",2,">",result.table.row.names[i],"</th>",
        sep=""
      )
    } else {
      if (num.var==1) {
        row.band.toggle <- !row.band.toggle # always toggle when there's only one variable
      }
      tr.class <- ifelse(banded.rows,paste(" class='",ifelse(row.band.toggle,css.class.name.even,css.class.name.odd),"'",sep=""),"")
      result.table.html <- paste(
        result.table.html,
        "<tr",tr.class,"><th>&nbsp;&nbsp;&nbsp;&nbsp;</th><th style='",row.th.style,"'>",
        result.table.row.names[i],
        ifelse(!is.var.continuous[var.count] & show.percent=="both",
               ifelse(!result.table.row.names[i]%in%num.missing.row.header.name | !show.missing,"<i><br>&nbsp;&nbsp;&nbsp;&nbsp;row%<br>&nbsp;&nbsp;&nbsp;&nbsp;col%</i>",""),
               ""),
        "</th>",
        sep=""
      )
    }	 
    
    result.table.html <- paste(
      result.table.html,
      "<td>",paste(result.table[i,],collapse="</td><td>"),"</td>",
      ifelse(
        (var.header.row & do.stats & var.count>0) | i==1, 
        ifelse(i==1,
               "<td></td>",
               paste(
                 "<td rowspan=",
                 ifelse(
                   is.var.continuous[var.count],
                   ifelse(show.missing,1+sum(!is.na(missing.codes.highlight[[var.name]])),0)+4,
                   ifelse(show.missing,1+sum(!is.na(missing.codes.highlight[[var.name]])),0)+1+as.numeric(ifelse(
                     is.factor(input.d.no.missing[,var.name]),
                     length(names(table(droplevels(input.d.no.missing.var.only[!is.na(input.d.no.missing.var.only[,var.name]) & !input.d.no.missing.var.only[,var.name]%in%c(missing.codes,missing.codes.highlight[[var.name]]),var.name])))),
                     length(names(table(           input.d.no.missing.var.only[!is.na(input.d.no.missing.var.only[,var.name]) & !input.d.no.missing.var.only[,var.name]%in%c(missing.codes,missing.codes.highlight[[var.name]]),var.name] )))
                   ))
                 ),
                 ">",stat.tests.results[var.count],"</td>",
                 sep=""
               )
        ),
        ""
      ),
      "</tr>",sep="")
  }
  
  result.table.html <- paste(result.table.html,"</table>",sep="")
  return(list("result.table"=result.table, "stat.tests.results"=stat.tests.results, "result.table.html"=result.table.html))
}
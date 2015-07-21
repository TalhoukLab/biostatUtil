#' Summary table in HTML format
#' @export
##############################################################
# generate summary table as html table 
# assume d is an array of numbers
summaryAsHTML <- function(d) {
  col.th.style <- COL.TH.STYLE
  s.table <- summary(d)
  result <- "<table>"
  result <- paste0(result, "<tr><th style='", col.th.style, "'>",
                   paste(names(s.table),
                         collapse = paste0("</th><th style='",
                                           col.th.style, "'>")), "</th></tr>")
  result <- paste0(result, "<tr><td>",
                   paste(s.table, collapse = "</td><td>"), "</td></tr>")
  result <- paste0(result, "</table>")
  return(result)
}
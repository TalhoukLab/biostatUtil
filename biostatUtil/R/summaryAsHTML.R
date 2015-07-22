#' Summary table in HTML format
#' 
#' Generate summary table as an HTML table
#' 
#' @param d assume \code{d} is an array of numbers
#' @return summary table with annotated HTMl code
#' @author Samuel Leung
#' @export
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
#' Row percentages in HTML
#' @param t a matrix
#' @param show.count logical. If \code{TRUE}, the table counts will be shown.
#' @param digits number of digits to round to
#' @param column.names (optional) vector of column names in table
#' @param html.table.border border type for the table. Defaults to 0 in HTML
#' syntax.
#' @param caption caption for the table
#' @param transpose logical. If \code{TRUE}, a single row is shown
#' as a single column. Useful if there are many columns in a single row.
#' @param banded.rows logical. If \code{TRUE}, alternating rows will have
#' different shaded colours.
#' @param css.class.name.odd how to format the odd numbered rows in CSS
#' @param css.class.name.even how to format the even numbered rows in CSS
#' @return The HTML code used to generate a table that shows row percentages.
#' @author Samuel Leung
#' @export
rowPercentAsHTML <- function(t, show.count = FALSE, digits = 4,
                             column.names = NULL, html.table.border = 0,
                             caption = NA, transpose = FALSE,
                             banded.rows = FALSE, css.class.name.odd = "odd",
                             css.class.name.even = "even", ...) {
  col.th.style <- COL.TH.STYLE
  row.th.style <- ROW.TH.STYLE
  table.values <- rowPercent(t, pretty.text = TRUE, digits = digits, ...)
  # header
  if (!is.null(column.names)) {
    colnames(table.values) <- column.names
  } else {
    column.names <- names(t)
  }
  result <- paste0("<table border=", html.table.border, ">",
                  ifelse(is.na(caption), "",
                         paste0("<caption style='", TABLE.CAPTION.STYLE, "'>",
                               addTableNumber(caption), "</caption>")))
  if (transpose) {
    tr.classes <- c() # tr classes for banded rows
    if (banded.rows) {
      for (i in 1:length(t)) {
        tr.classes <- c(tr.classes, paste0("class='",
                                           ifelse(i %% 2 == 0,
                                                  css.class.name.even,
                                                  css.class.name.odd), "'"))
      }
    } else {
      tr.classes <- ""
    }
    result <- paste0(result, "<tr><th style='", col.th.style,
                     "'></th><th style='", col.th.style, "'>")
    if (show.count) {
      result <- paste0(result,"count (%)</th></tr>")
      result <- paste0(result, paste(
        paste0("<tr ", tr.classes, "><th style='", row.th.style, "'>",
               column.names, "</th><td>",
               paste0(t, " (", table.values[2, ], ")"), "</td></tr>"),
        collapse = ""))
    } else {
      result <- paste0(result, "count</th></tr>")
      result <- paste0(result, paste(
        paste0("<tr ", tr.classes, "><th style='", row.th.style, "'>",
               column.names, "</th><td>", table.values[2, ], "</td></tr>"),
        collapse = ""))
    }
  } else {
    result <- paste0(result, "<tr><th style='", col.th.style, "'>",
                     paste(column.names,
                           collapse = paste0("</th><th style='", col.th.style,
                                             "'>")), "</th></tr>")
    # print values
    if (show.count) {
      result <- paste0(result, "<tr><td>",
                       paste(paste0(t, " (", table.values[2, ], ")"),
                             collapse = "</td><td>"), "</td></tr>")
    } else {
      result <- paste0(result, "<tr><td>",
                       paste(table.values[2, ],
                             collapse = "</td><td>"), "</td></tr>")
    }
  }
  result <- paste0(result, "</table>")
  return(result)
}
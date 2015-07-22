#' Change table to HTML format
#' 
#' Generate HTML code for a table output. Assumes table as more than one row.
#' 
#' @param t a matrix
#' @param row.names (optional) vector of row names in table
#' @param column.names (optional) vector of column names in table
#' @param html.table.border border type for the table. Defaults to 0 in HTML
#' syntax.
#' @param banded.rows logical. If \code{TRUE}, alternating rows will have
#' different shaded colours.
#' @param css.class.name.odd how to format the odd numbered rows in CSS
#' @param css.class.name.even how to format the even numbered rows in CSS
#' @param caption caption for the table
#' @author Samuel Leung
#' @export
tableAsHTML <- function(t, row.names = NULL, column.names = NULL,
                        html.table.border = 0, banded.rows = FALSE,
                        css.class.name.odd = "odd",
                        css.class.name.even = "even", caption = NA) {
  th.style <- "border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;"
  result <- paste0("<table border=", html.table.border, ">",
                  ifelse(is.na(caption), "",
                         paste0("<caption style='", TABLE.CAPTION.STYLE, "'>",
                                addTableNumber(caption), "</caption>")))
  if (!is.null(row.names)) {
    rownames(t) <- row.names
  } else {
    row.names <- rownames(t)
  }
  if (!is.null(column.names)) {
    colnames(t) <- column.names
  } else {
    column.names <- colnames(t)
  }
  result <- paste0(result, "<tr><th style='", th.style, "'></th><th style='",
                  th.style, "'>", paste(column.names,
                                        collapse = paste0("</th><th style='",
                                                          th.style, "'>")),
                  "</th></tr>")
  for (i in 1:nrow(t)) {
    tr.class <- ifelse(banded.rows,
                       paste0(" class='", ifelse(i %% 2 == 0,
                                                 css.class.name.even,
                                                 css.class.name.odd), "'"), "")
    result <- paste0(result, "<tr", tr.class, "><th>", row.names[i],
                     "</th><td>", paste(t[i, ], collapse = "</td><td>"),
                     "</td></tr>")
  }
  result <- paste0(result, "</table>")
  return(result)
}
#' Row and column percentages in HTML format
#' 
#' @param t a matrix
#' @param show.count logical. If \code{TRUE}, the table counts will be shown.
#' @param row.names (optional) vector of row names in table
#' @param column.names (optional) vector of column names in table
#' @param html.table.border border type for the table. Defaults to 0 in HTML
#' syntax.
#' @param banded.rows logical. If \code{TRUE}, alternating rows will have
#' different shaded colours.
#' @param css.class.name.odd how to format the odd numbered rows in CSS
#' @param css.class.name.even how to format the even numbered rows in CSS
#' @param ... additional arguments to \code{rowColPercent}
#' @return The HTML code used to generate a table that shows row percentages.
#' @author Samuel Leung, Derek Chiu
#' @export
rowColPercentAsHTML <- function(
  t, show.count = FALSE, row.names = NULL, column.names = NULL,
  html.table.border = 0, banded.rows = FALSE,
  css.class.name.odd = "odd", css.class.name.even = "even", caption = NA, ...) {
  
  col.th.style <- COL.TH.STYLE
  row.th.style <- ROW.TH.STYLE
  table.values <- rowColPercent(t, keep = show.count, ...)
  
  # print header
  if (!is.null(column.names)) {
    colnames(table.values) <- column.names
  } else {
    column.names <- colnames(table.values)
  }
  
  if (!is.null(row.names)) {
    rownames.mat <- stringr::str_split_fixed(rownames(table.values), " ", 2)
    rownames.mat[, 1] <- rep(row.names, each = ifelse(show.count, 3, 2))
    rownames(table.values) <- apply(rownames.mat, 1, paste, collapse = " ")
  } else {
    row.names <- unique(gsub(" .+", "", rownames(table.values)))
  }
  
  result <- paste0(
    paste0("<table border=", html.table.border, ">",
           ifelse(is.na(caption), "",
                  paste0("<caption style='", TABLE.CAPTION.STYLE, "'>",
                         addTableNumber(caption), "</caption>"))
    ), "<tr><th style='", col.th.style,
    "' colspan=2></th><th style='", col.th.style, "'>",
    paste(column.names, collapse = paste0("</th><th style='",
                                          col.th.style, "'>")
    ), "</th></tr>")
  # print values
  i <- 1
  while (i <= nrow(table.values)) {
    tr.class <- ifelse(banded.rows,
                       paste0(" class='",
                              ifelse((floor(i / 3) + 1) %% 2 == 0,
                                     css.class.name.even, css.class.name.odd),
                              "'"), "")
    result <- paste0(result, "<tr", tr.class, "><th style='", row.th.style,
                     "' rowspan=3>", row.names[floor(i / 3) + 1], "</th>")
    if (show.count) {
      result <- paste0(result, "<th style='", row.th.style, "'>count</th><td>",
                       paste(table.values[i, ], collapse = "</td><td>"),
                       "</td></tr>")	
    } else {
      result <- paste0(result, "<tr", tr.class, "><th style='", row.th.style,
                       "'>row %</th><td>",
                       paste(table.values[i, ], collapse = "</td><td>"),
                       "</td></tr>")
    }
    i <- i + 1
    if (show.count) {
      result <- paste0(result, "<tr", tr.class, "><th style='", row.th.style,
                       "'><i>row %</i></th><td><i>",
                       paste(table.values[i, ], collapse = "</i></td><td><i>"),
                       "</td></tr>")
      
    } else {
      result <- paste0(result, "<tr", tr.class, "><th style='", row.th.style,
                       "'>col %</th><td>",
                       paste(table.values[i, ], collapse = "</td><td>"),
                       "</td></tr>")
    }
    i <- i + 1
    if (show.count) {
      result <- paste0(result, "<tr", tr.class, "><th style='", row.th.style,
                       "'><i>col %</i></th><td><i>",
                       paste(table.values[i, ], collapse = "</i></td><td><i>"),
                       "</td></tr>")
      i <- i + 1
    }
  }
  result <- paste0(result, "</table>")
  return(result)
}
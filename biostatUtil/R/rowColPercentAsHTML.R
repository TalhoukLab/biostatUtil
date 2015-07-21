#' Row and Column percentages in HTML format
#' @export
####################################################
# same as row.col.percent but show table in html
rowColPercentAsHTML <- function(t,show.count = FALSE, row.names = NULL,
  column.names = NULL, html.table.border = 0, banded.rows = FALSE,
  css.class.name.odd = "odd", css.class.name.even = "even", ...) {
  
  col.th.style <- COL.TH.STYLE
  row.th.style <- ROW.TH.STYLE
  table.values <- rowColPercent(t, ...)
  result <- paste0("<table border=", html.table.border, ">")
  # print header
  if (!is.null(column.names)) {
    colnames(table.values) <- column.names
  } else {
    column.names <- colnames(table.values)
  }
  if (!is.null(row.names)) {
    rownames(table.values)[rep(c(TRUE, FALSE, FALSE),
                               nrow(table.values) / 3)] <- row.names
  } else {
    row.names <- rownames(table.values)[rep(c(TRUE, FALSE, FALSE),
                                            nrow(table.values) / 3)]
  }
  result <- paste0(result, "<tr><th style='", col.th.style,
                   "' colspan=2></th><th style='", col.th.style, "'>",
                  paste(column.names, collapse = paste0("</th><th style='",
                                                        col.th.style, "'>")),
                  "</th></tr>")
  # print values
  i <- 1
  num.row.per.cat <- ifelse(show.count, 3, 2)
  while (i <= nrow(table.values)) {
    tr.class <- ifelse(banded.rows,
                       paste0(" class='",
                              ifelse((floor(i / 3) + 1) %% 2 == 0,
                                     css.class.name.even, css.class.name.odd),
                              "'"), "")
    result <- paste0(result, "<tr", tr.class, "><th style='", row.th.style,
                     "' rowspan=", num.row.per.cat, ">",
                     row.names[floor(i / 3) + 1], "</th>")
    if (show.count) {
      result <- paste0(result, "<th style='", row.th.style, "'>count</th><td>",
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
                      "'>row %</th><td>",
                      paste(table.values[i, ], collapse = "</td><td>"),
                      "</td></tr>")
    }
    i <- i + 1
    if (show.count) {
      result <- paste0(result, "<tr", tr.class, "><th style='", row.th.style,
                      "'><i>col %</i></th><td><i>",
                      paste(table.values[i, ], collapse = "</i></td><td><i>"),
                      "</td></tr>")
    } else {
      result <- paste0(result, "<tr", tr.class, "><th style='", row.th.style,
                      "'>col %</th><td>",
                      paste(table.values[i, ], collapse = "</td><td>"),
                      "</td></tr>")
    }
    i <- i + 1
  }
  result <- paste0(result, "</table>")
  return(result)
}
## TODO: Function does not work because 'COL.TH.SCALE' AND 'ROW.TH.STYLE'
## are undefined

rowPercentHTML <- function(t, show.count = FALSE, row.names = NULL,
                           column.names = NULL, html.table.border = 0,
                           banded.rows=FALSE, css.class.name.odd = "odd",
                           css.class.name.even = "even", ...) {
  # Prints a table with row percentages as a nice HTML format
  
  col.th.style <- COL.TH.STYLE
  row.th.style <- ROW.TH.STYLE
  table.values <- rowPercent(t, pretty.text = pretty.text, digits = digits,
                             ...)
  result <- paste("<table border=", html.table.border, ">", sep = "")
  
  # Print header
  if (!is.null(column.names)) {
    colnames(table.values) <- column.names
  }
  if (!is.null(row.names)) {
    rownames(table.values) <- row.names
  } else {
    row.names <- rownames(table.values)
  }
  result <- paste(result, "<tr><th style='", col.th.style,"'>",
                  paste(names(table.values),
                        collapse = paste("</th><th style='",
                                         col.th.style,"'>",
                                         sep = "")),
                  "</th></tr>", sep = "")
  
  # Print values
  if (length(dim(table.values)) == 1) {
    if (show.count) {
      result <- paste(result, "<tr><td>",
                      paste(paste(t," (", table.values,")", sep = ""),
                            collapse = "</td><td>"), "</td></tr>", sep = "")
    } else {
      result <- paste(result, "<tr><td>",
                      paste(table.values,
                            collapse = "</td><td>"), "</td></tr>", sep = "")
    }
  } else {
    # For tables with > 1 row, there will be need to be row names
    for (i in 1:nrow(table.values)) {
      tr.class <- ifelse(banded.rows,
                         paste(" class='", ifelse(i %% 2 == 0,
                                                  css.class.name.even,
                                                  css.class.name.odd ),
                               "'", sep = ""), "")
      result <- paste(result, "<tr", tr.class, "><th style='", row.th.style,
                      "'>", row.names[i], "</th>", sep = "")
      if (show.count) {
        result <- paste(result, "<td>",
                        paste(paste(t[i, ]," (",
                                    table.values[i, ], ")", sep = ""),
                              collapse = "</td><td>"), "</td></tr>",sep = "") 
      } else {
        result <- paste(result, "<td>", paste(table.values[i, ],
                                              collapse = "</td><td>"),
                        "</td></tr>", sep = "")
      }
    }
  }
  result <- paste(result, "</table>", sep = "")
  return(result)
}
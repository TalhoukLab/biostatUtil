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
  
  result <- paste0(HTML(paste0(
    tags$caption(style = TABLE.CAPTION.STYLE,
                 ifelse(is.na(caption), "", addTableNumber(caption))),
    tags$tr(HTML(paste0(
      tags$th(style = col.th.style, colspan = 2),
      tags$th(style = col.th.style, column.names[1]),
      tags$th(style = col.th.style, column.names[2]))))
  )))
  
  if (show.count) {
    i <- 1
    while (i <= nrow(table.values)) {
      tr.class <- ifelse(banded.rows,
                         paste0(" class='",
                                ifelse((floor(i / 3) + 1) %% 2 == 0,
                                       css.class.name.even, css.class.name.odd),
                                "'"), "")
      result <- paste0(HTML(paste0(
        result,
        tags$tr(HTML(paste0(
          tr.class,
          tags$th(style = row.th.style, rowspan = 3, row.names[floor(i / 3) + 1]),
          tags$th(style = row.th.style, "count"),
          tags$td(table.values[i, 1]), tags$td(table.values[i, 2])))),
        tags$tr(HTML(paste0(
          tr.class,
          tags$th(style = row.th.style, tags$i("row %")),
          tags$td(tags$i(table.values[i + 1, 1])),
          tags$td(tags$i(table.values[i + 1, 2]))
        ))),
        tags$tr(HTML(paste0(
          tr.class,
          tags$th(style = row.th.style, tags$i("col %")),
          tags$td(tags$i(table.values[i + 2, 1])),
          tags$td(tags$i(table.values[i + 2, 2]))
        )))
      )))
      i <- i + 3
    }
  } else {
    i <- 1
    while (i <= nrow(table.values)) {
      tr.class <- ifelse(banded.rows,
                         paste0(" class='",
                                ifelse((floor(i / 3) + 1) %% 2 == 0,
                                       css.class.name.even, css.class.name.odd),
                                "'"), "")
      result <- paste0(HTML(paste0(
        result,
        tags$tr(HTML(paste0(
          tr.class,
          tags$th(style = row.th.style, rowspan = 3, row.names[floor(i / 3) + 1])
        ))),
        tags$tr(HTML(paste0(
          tr.class,
          tags$th(style = row.th.style, "row %"),
          tags$td(table.values[i, 1]),
          tags$td(table.values[i, 2])
        ))),
        tags$tr(HTML(paste0(
          tr.class,
          tags$th(style = row.th.style, "col %"),
          tags$td(table.values[i + 1, 1]),
          tags$td(table.values[i + 1, 2])
        )))
      )))
      i <- i + 2
    }
  }
  result <- paste0(tags$table(border = html.table.border, HTML(result)))
  return(result)
}
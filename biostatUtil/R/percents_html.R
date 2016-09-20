#' Functions ending in \code{AsHTML} output a character string that can be 
#' parsed as HTML using functions such as \code{\link[htmlTable]{htmlTable}}
#' 
#' Using \code{\link[htmlTable]{htmlTable}} with a result from 
#' \code{rowColPercentAsHTML} gives a nice HTML table display.
#' 
#' @param row.names (optional) vector of row names in table
#' @param column.names (optional) vector of column names in table
#' @param html.table.border border type for the table. Defaults to 0 in HTML 
#'   syntax.
#' @param banded.rows logical; if \code{TRUE}, alternating rows will have 
#'   different shaded colours.
#' @param col.odd if \code{banded.rows = TRUE}, colour to use for odd numbered 
#'   rows
#' @param col.even if \code{banded.rows = TRUE}, colour to use for even numbered
#'   rows
#' @return HTML-based functions return the code used to generate a
#'   table that shows row and/or column percentages.
#' @rdname percents
#' @export
#' @examples 
#' 
#' library(htmlTable)
#' set.seed(13)
#' B <- matrix(rbinom(16, size = 20, prob = 0.3), nrow = 4,
#' dimnames = list(paste0("Row", 1:4), paste0("Col", 1:4)))
#' htmlTable(rowColPercentAsHTML(B, keep = TRUE, banded.rows = TRUE,
#' col.odd = "yellow", col.even = "green", caption = "Example Table",
#' html.table.border = 2)
rowColPercentAsHTML <- function(
  t, keep = TRUE, row.names = NULL, column.names = NULL,
  html.table.border = 0, banded.rows = FALSE,
  col.odd = "none", col.even = "lightgrey", caption = NA, ...) {
  
  col.th.style <- COL.TH.STYLE
  row.th.style <- ROW.TH.STYLE
  table.values <- rowColPercent(t, keep = keep, ...)
  
  if (!is.null(column.names)) {
    colnames(table.values) <- column.names
  } else {
    column.names <- colnames(table.values)
  }
  if (!is.null(row.names)) {
    rownames.mat <- stringr::str_split_fixed(rownames(table.values), " ", 2)
    rownames.mat[, 1] <- rep(row.names, each = ifelse(keep, 3, 2))
    rownames(table.values) <- apply(rownames.mat, 1, paste, collapse = " ")
  } else {
    row.names <- unique(gsub(" .+", "", rownames(table.values)))
  }
  
  result <- paste0(HTML(paste0(
    tags$caption(style = TABLE.CAPTION.STYLE,
                 ifelse(is.na(caption), "", addTableNumber(caption))),
    tags$tr(HTML(paste0(
      tags$th(style = col.th.style, colspan = 2),
      paste(unlist(lapply(column.names, function(x)
        paste(tags$th(style = col.th.style, x)))), collapse = ""))))
  )))
  
  if (keep) {
    i <- 1
    while (i <= nrow(table.values)) {
      if (banded.rows) {
        row.col <- paste0(
          "background-color: ",
          ifelse(((i + 2) %/% 3) %% 2 == 1, col.odd, col.even), ";")
      } else {
        row.col <- ""
      }
      result <- paste0(HTML(paste0(
        result,
        tags$tr(style = row.col, HTML(paste0(
          tags$th(style = row.th.style, rowspan = 3,
                  row.names[floor(i / 3) + 1]),
          tags$th(style = row.th.style, "count"),
          paste(unlist(lapply(table.values[i, ], function(x)
            paste(tags$td(x)))), collapse = "")
        ))),
        tags$tr(style = row.col, HTML(paste0(
          tags$th(style = row.th.style, tags$i("row %")),
          paste(unlist(lapply(table.values[i + 1, ], function(x)
            paste(tags$td(tags$i(x))))), collapse = "")
        ))),
        tags$tr(style = row.col, HTML(paste0(
          tags$th(style = row.th.style, tags$i("col %")),
          paste(unlist(lapply(table.values[i + 2, ], function(x)
            paste(tags$td(tags$i(x))))), collapse = "")
        )))
      )))
      i <- i + 3
    }
  } else {
    i <- 1
    while (i <= nrow(table.values)) {
      if (banded.rows) {
        row.col <- paste0(
          "background-color: ",
          ifelse(((i + 1) %/% 2) %% 2 == 1, col.odd, col.even), ";")
      } else {
        row.col <- ""
      }
      result <- paste0(HTML(paste0(
        result,
        tags$tr(style = row.col, HTML(paste0(
          tags$th(style = row.th.style, rowspan = 3,
                  row.names[floor(i / 3) + 1])
        ))),
        tags$tr(style = row.col, HTML(paste0(
          tags$th(style = row.th.style, "row %"),
          paste(unlist(lapply(table.values[i, ], function(x)
            paste(tags$td(tags$i(x))))), collapse = "")
        ))),
        tags$tr(style = row.col, HTML(paste0(
          tags$th(style = row.th.style, "col %"),
          paste(unlist(lapply(table.values[i + 1, ], function(x)
            paste(tags$td(tags$i(x))))), collapse = "")
        )))
      )))
      i <- i + 2
    }
  }
  result <- paste0(tags$table(border = html.table.border, HTML(result)))
  return(result)
}
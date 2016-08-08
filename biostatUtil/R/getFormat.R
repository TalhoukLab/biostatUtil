#' Get date format from character text
#' 
#' Get the POSIX standard date formats from character text formats.
#' 
#' @param date character string of date
#' @param char.format character text format of date. Either one of 
#'   \code{"MM.DD.YYYY"}, \code{"MMMM.DD.YYYY"}, \code{"DD.MM.YYYY"}, 
#'   \code{"DD.MMM.YYYY"}, \code{"YYYY.MM.DD"}, or \code{"YYYY.MMM.DD"}.
#' @param sep character string separating \code{date}
#' @return A character string representing the POSIX standard date format 
#'   equivalent of the string in \code{char.format}.
#' @author Derek Chiu
#' @export
#' @examples 
#' getFormat("12/09/1993", "MM.DD.YYYY")
#' getFormat("2005-09-13", "YYYY.MM.DD")
getFormat <- function(date, char.format, sep = "") {
  if (length(strsplit(date, "-")[[1]]) > 1)
    sep <- "-"
  else if (length(strsplit(date, "/")[[1]]) > 1)
    sep <- "/"
  else if (length(strsplit(date, "|")[[1]]) > 1)
    sep <- "|"
  switch(char.format,
         MM.DD.YYYY = paste0("%m", sep, "%d", sep, "%Y"),
         MMM.DD.YYYY = paste0("%b", sep, "%d", sep, "%Y"),
         DD.MM.YYYY = paste0("%d", sep, "%m", sep, "%Y"),
         DD.MMM.YYYY = paste0("%d", sep, "%b", sep, "%Y"),
         YYYY.MM.DD = paste0("%Y", sep, "%m", sep, "%d"),
         YYYY.MMM.DD = paste0("%Y", sep, "%b", sep, "%d"))
}
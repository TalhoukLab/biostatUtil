#' Generate a Frequency Table
#' 
#' A frequency table emulating the SPSS FREQ output is generated.
#'
#' @param x vector of values to show frequencies for
#' @param levels (optional) vector for order of levels in \code{x}
#' @param missing vector of levels for which we consider missing and don't count
#' in the valid percentage
#' @param description (optional) description for each level of \code{x}.
#' Must be same length and order as \code{levels}
#' @param round number of digits to round percentages to
#' @param plot logical; if \code{TRUE}, a barplot is produced.
#'
#' @return A data frame with the following columns
#' \item{Class}{Tells you which scores are valid and which are missing}
#' \item{Score}{Different levels}
#' \item{Frequency}{Count for each score}
#' \item{Percent}{Percent of Frequency out of the grand total}
#' \item{Valid Percent}{Percent of Frequency out of the Valid scores}
#' \item{Cumulative Percent}{Accumulated Percent of Frequency out of the Valid Scores}
#' \item{Description}{If \code{description} is given, a description for each level}
#' 
#' @author Derek Chiu
#' @export
#' 
#' @examples 
#' # Create vector of randomly reordered alphabet with various frequencies
#' # for each letter
#' set.seed(123)
#' n <- sample(10, length(letters), replace = TRUE)
#' x <- sample(rep.int(letters, times = n))
#' freqTable(x, plot = TRUE)
#' 
#' # Treat vowels as missing
#' freqTable(x, missing = c("a", "e", "i", "o", "u"), round = 2)
freqTable <- function(x, levels = sort(unique(x)), missing = NULL, description = NULL,
                      round = 1, plot = FALSE) {
  . <- Class <- Frequency <- Score <- `Valid Percent` <- NULL
  tab <- descr::freq(x, user.missing = missing, plot = plot) %>% 
    as.data.frame() %>% 
    rbind(c(sum(.$Frequency[!is.na(.$`Valid Percent`) & rownames(.) != "Total"]),
            sum(.$Percent[!is.na(.$`Valid Percent`) & rownames(.) != "Total"]), 100)) %>% 
    cbind(Score = factor(rownames(.), c(levels, "Total", "")), .) %>% 
    cbind(Class = factor(ifelse(is.na(.$`Valid Percent`), "Missing",
                                ifelse(grepl("Total", .$Score), "Total", "Valid")),
                         c("Valid", "Missing", "Total")), .) %>% 
    arrange(Class, Score) %>% 
    mutate(Score = ifelse(Class == "Total" & Score == "Total", "",
                          ifelse(is.na(Score), "Total", as.character(Score))),
           `Valid Percent` = ifelse(Class == "Total", NA, `Valid Percent`),
           `Cumulative Percent` = ifelse(!is.na(`Valid Percent`) & Score != "Total",
                                         cumsum(Frequency) / max(Frequency[Class == "Valid"]) * 100, NA),
           Class = ifelse(duplicated(Class), "", as.character(Class))) %>% 
    mutate_each(funs(ifelse(sprintf(paste0("%.", round, "f"), .) == "NA", "",
                            sprintf(paste0("%.", round, "f"), .))),
                grep("Percent", names(.)))
  if (is.null(missing))
    tab <- tab %>% 
      extract(-which(.$Score == "Total"), ) %>% 
      select(-`Valid Percent`)
  if (!is.null(description))
    tab <- tab %>%
      mutate(Description = c(append(description, "", which(Score == "Total") - 1), ""))
  return(tab)
}

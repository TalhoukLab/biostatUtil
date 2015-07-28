#' Grade trend barplot
#' 
#' @param input.d input \code{data.frame}
#' @param title title of barplot
#' @param legend.space amount of space between legend and edge of plot
#' @return a barplot showing the change of Grade over time
#' @author Samuel Leung
#' @export
doGrdTrendBarplot <- function(input.d, title, legend.space = 0.2) {
  legend.space <- round(legend.space * nrow(input.d))
  grade.trend <- c()
  grade.trend.n <- c()
  temp.d <- input.d
  dx.years <- names(table(temp.d$dx.year))
  use.prop <- TRUE
  for (dx.year in dx.years) {
    select.cases <- temp.d$dx.year == dx.year
    n <- length(temp.d$Tumour.Grade[select.cases]) # no missing
    grade1 <- sum(temp.d$Tumour.Grade[select.cases] == "Grade 1")	
    grade2 <- sum(temp.d$Tumour.Grade[select.cases] == "Grade 2")
    grade3 <- sum(temp.d$Tumour.Grade[select.cases] == "Grade 3")
    grade <- c(grade1, grade2, grade3)
    if (use.prop) {
      grade <- grade / n * 100
    }
    grade.trend <- cbind(grade.trend, grade)
    grade.trend.n <- c(grade.trend.n, n)
  }
  colnames(grade.trend) <- dx.years
  if (length(dx.years) > 20) {
    colnames(grade.trend)[which(rep(c(FALSE, TRUE), 11))] <- ""
  }
  if (sum(dx.years == 2000) > 0) {
    colnames(grade.trend)[dx.years == 2000] <- 2000
  }
  if (sum(dx.years == 1999) > 0) {
    colnames(grade.trend)[dx.years == 1999] <- ""
  }
  if (sum(dx.years == 2001) > 0) {
    colnames(grade.trend)[dx.years == 2001] <- ""
  }
  rownames(grade.trend) <- c("Grade 1", "Grade 2", "Grade 3")
  
  # add dummy bar for legend
  grade.trend <- cbind(grade.trend, rep("", nrow(grade.trend)))
  colnames(grade.trend)[ncol(grade.trend)] <- ""
  grade.trend.n <- c(grade.trend.n, legend.space)
  
  barplot(
    grade.trend, width = grade.trend.n, 
    legend.text = T, args.legend = list(x = "right"),
    col = c("red4", "red3", "red"),
    las = 2,
    main = title,
    ylab = "Percentage of cases",
    xlab = "Year of diagnosis",
    sub = "(width of bar proportional to the number of patients)"
  )
}
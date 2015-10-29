# Continuous function summaries
contSumFunc <- function(x, digits) {
  round(c(mean = mean(x, na.rm = TRUE),
          s = sd(x, na.rm = TRUE), 
          median = median(x, na.rm = TRUE),
          IQR = IQR(x, na.rm = TRUE),
          range = range(x, na.rm = TRUE),
          missing = sum(is.na(x))), digits = digits)
}

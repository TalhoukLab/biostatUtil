# Continuous function summaries
contSumFunc <- function(x, digits, stats = c("mean", "sd", "median", "IQR",
                                             "range", "missing")) {
  stat.names <- stats
  if ("mean" %in% stats)
    mn <- mean(x, na.rm = TRUE)
  else
    mn <- NULL
  if ("sd" %in% stats)
    s <- sd(x, na.rm = TRUE)
  else
    s <- NULL
  if ("median" %in% stats)
    md <- median(x, na.rm = TRUE)
  else
    md <- NULL
  if ("IQR" %in% stats)
    iqr <- IQR(x, na.rm = TRUE)
  else
    iqr <- NULL
  if ("range" %in% stats) {
    min <- min(x, na.rm = TRUE)
    max <- max(x, na.rm = TRUE)
    range.ind <- which(stats == "range")
    stat.names <- append(stats, values = c("min", "max"),
                         range.ind)[-range.ind]
  } else {
    min <- max <- NULL
  }
  if ("missing" %in% stats)
    miss <- sum(is.na(x))
  else
    miss <- NULL
  all.stats <- c(mean = mn, sd = s, median = md, IQR = iqr,
                 min = min, max = max, missing = miss)
  return(round(all.stats[stat.names], digits = digits))
}

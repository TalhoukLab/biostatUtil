# Verifies that the input data for ratioMethod are valid
check_data <- function(Y2, R1, R2) {
  if (class(Y2) %in% c("matrix", "data.frame")) {
    if (class(R1) %in% c("matrix", "data.frame")) {
      if (class(R2) %in% c("matrix", "data.frame")) {
        if (ncol(Y2) == ncol(R1) & ncol(R1) == ncol(R2)) {
          return(TRUE)
        } else {
          stop("All data objects must have the same number of columns.")
        }
      } else {
        stop("R2 is not of class 'matrix' or 'data.frame'.")
      }
    } else {
      stop("R1 is not of class 'matrix' or 'data.frame'.")
    }
  } else {
    stop("Y2 is not of class 'matrix' or 'data.frame'.")
  }
}

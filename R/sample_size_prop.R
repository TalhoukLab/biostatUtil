#' Sample Size for Multiple Proportions in One-Way Design
#'
#' Calculates power and sample size in a one-way design for differences among
#' response proportions of those groups.
#'
#' Specify exactly one of `power` or `n` in order to determine the value of the
#' other unspecified parameter.
#'
#' @param props response proportion in each group
#' @param power desired power
#' @param n sample size in each group
#' @param alpha significance level
#' @references
#'   https://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/PASS/Tests_for_Multiple_Proportions_in_a_One-Way_Design.pdf
#' @export
#' @examples
#' ## Examples from PASS Sample Size Software reference
#' # Example 1
#' sample_size_prop(props = c(0.4, 0.2, 0.2), n = 20)
#' sample_size_prop(props = c(0.4, 0.2, 0.2), n = 40)
#' sample_size_prop(props = c(0.4, 0.2, 0.2), n = 60)
#' sample_size_prop(props = c(0.4, 0.2, 0.2), n = 80)
#' sample_size_prop(props = c(0.4, 0.2, 0.2), n = 100)
#'
#' # Example 2
#' sample_size_prop(props = c(0.4, 0.2, 0.2), power = 0.8)
#' sample_size_prop(props = c(0.4, 0.2, 0.2), power = 0.9)
#'
#' # Example 3
#' sample_size_prop(power = 0.9, props = c(0.4, 0.1, 0.1))
#' sample_size_prop(power = 0.9001, props = c(0.4, 0.2, 0.2))
#' sample_size_prop(power = 0.9004, props = c(0.4, 0.3, 0.3))
#' sample_size_prop(power = 0.9038, props = c(0.4, 0.3, 0.1))
#'
#' # Example 4
#' sample_size_prop(props = c(0.475, 0.2, 0.2, 0.2), n = 25)
sample_size_prop = function(props, power = NULL, n = NULL, alpha = 0.05) {
  assertthat::assert_that(is.null(power) & !is.null(n) |
                            !is.null(power) &
                            is.null(n), msg = "Specify only one of `power` or `n`")
  mug <- props
  G <- length(mug)
  p <- 1 - alpha
  df <- G - 1
  C <- stats::qchisq(p, df, lower.tail = TRUE)
  if (is.null(n)) {
    D <- multiCA::cnonct(x = C, p = 1 - power, df = df)
    mu0 <- mean(mug)
    D_stat <- (mug * (log(mu0) - log(mug)) + (1 - mug) * (log(1 - mu0) - log(1 - mug)))
    Ng <- ceiling(D / (-2 * G * mean(D_stat)))
    N <- sum(rep(Ng, G))
    V <- sqrt(D / (N * (G - 1)))
    return(list(Ng = Ng, N = N, V = V))
  } else if (is.null(power)) {
    Ng <- rep(n, G)
    N <- sum(Ng)
    mu0 <- mean(mug)
    V2 <- -2 * sum((mug * (log(mu0) - log(mug)) + (1 - mug) * (log(1 - mu0) - log(1 - mug))) * Ng / (N * (G - 1)))
    D <- N * (G - 1) * V2
    power <- 1 - stats::pchisq(C, df, ncp = D, lower.tail = TRUE)
    return(list(power = power, V = sqrt(V2)))
  }
}

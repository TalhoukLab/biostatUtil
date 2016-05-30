#' Custom combat_p
#'
#' Based on combat_p function from package \code{ber}
#' @param Y A data matrix
#' @param b Vector indicating batches
#' @param prior.plots If \code{TRUE}, prior plots to compare kernel density estimate and
#' parametric estimate are produced.
#' @author Marco Giordan, Aline Talhouk
#' @importFrom stats dnorm qqnorm qqline rgamma qqplot density model.matrix
#' @importFrom graphics lines title
#' @export
mycombat_p <- function(Y, b, prior.plots = F) {
  if (missing(Y)) {
    stop("Argument 'Y' missing, with no default\n")
  }
  if (missing(b)) {
    stop("Argument 'b' missing, with no default\n")
  }
  if (class(Y) != "matrix") {
    stop("'Y' must be of class 'matrix'\n")
  }
  if (class(b) != "factor") {
    stop("'b' must be of class 'factor'\n")
  }
  if (any(is.na(Y))) {
    stop("NA values are not allowed in 'Y'\n")
  }
  if (any(is.na(b))) {
    stop("NA values are not allowed in 'b'\n")
  }
  if (length(b) != nrow(Y)) {
    stop("length(b) is different from nrow(Y)\n")
  }
  if (any(apply(Y, 2, mode) != "numeric")) {
    stop("Array expression columns contain non-numeric values!\n")
  }

  n <- nrow(Y)
  g <- ncol(Y)
  m1 <- nlevels(b)
  n.b <- summary(b)
  Xdes <- model.matrix(~b)
  Xdes_batch <- model.matrix(~-1 + b, b)
  n_batches <- as.numeric(summary(b))
  mat_nbatches <- matrix(rep(n_batches, g), ncol = g)
  B.hat <- MASS::ginv(Xdes) %*% Y
  alpha.hat <- t(n.b/n) %*% B.hat[1:m1, ]
  mat_alpha.hat <- matrix(rep(alpha.hat, n), byrow = T, nrow = n)
  var.pooled <- matrix(rep(1/n, n), nrow = 1) %*% ((Y - Xdes %*%
                                                      B.hat)^2)
  mat_var <- matrix(rep(var.pooled, n), byrow = T, nrow = n)
  Z_num <- Y - mat_alpha.hat
  Z <- Z_num/sqrt(mat_var)
  gamma_bg.hat <- (t(Xdes_batch) %*% Z)/mat_nbatches
  gamma_b.hat <- apply(gamma_bg.hat, 1, mean)
  tau_b.hat <- apply(gamma_bg.hat, 1, var)
  Zgamma <- (Z - Xdes_batch %*% gamma_bg.hat)^2
  delta_bg.hat <- (t(Xdes_batch) %*% Zgamma)/(mat_nbatches -
                                                1)
  V_b.hat <- apply(delta_bg.hat, 1, mean)
  S_b.hat <- apply(delta_bg.hat, 1, var)
  lambda_b.hat <- (V_b.hat^2 + 2 * S_b.hat)/S_b.hat
  theta_b.hat <- (V_b.hat^3 + V_b.hat * S_b.hat)/S_b.hat
  post.gamma_bg <- function(n_b, tau_b, gamma_bg, delta_bg,
                            gamma_b) {
    post.gamma_bg <- (n_b * tau_b * gamma_bg + delta_bg *
                        gamma_b)/(n * tau_b + delta_bg)
  }
  post.delta_bg <- function(Z_bg, n_b, theta_b, lambda_b, gamma_bg) {
    post.delta_bg <- (theta_b + 0.5 * sum((Z_bg - gamma_bg)^2))/(0.5 *
                                                                   n_b + lambda_b - 1)
  }
  it.sol <- function(n_b, tau_b, gamma_bg, delta_bg, gamma_b,
                     Z_bg, theta_b, lambda_b, conv = 1e-04) {
    g.old <- gamma_bg
    d.old <- delta_bg
    change <- 1
    while (change > conv) {
      g.new <- post.gamma_bg(n_b, tau_b, gamma_bg, d.old,
                             gamma_b)
      d.new <- post.delta_bg(Z_bg, n_b, theta_b, lambda_b,
                             g.new)
      change <- max(abs((g.new - g.old)/g.old), abs((d.new -
                                                       d.old)/d.old))
      g.old <- g.new
      d.old <- d.new
    }
    adjust <- c(g.new, d.new)
    names(adjust) <- c("g.star", "d.star")
    return(adjust)
  }
  mat_gamma.star <- matrix(rep(0, m1 * g), ncol = g)
  mat_delta.star <- matrix(rep(0, m1 * g), ncol = g)
  for (i in 1:m1) {
    ind_batch <- which(b == (levels(b)[i]))
    Z_b <- Z[ind_batch, ]
    n_b <- length(ind_batch)
    for (j in 1:g) {
      temp <- it.sol(n_b, tau_b.hat[i], gamma_bg.hat[i,
                                                     j], delta_bg.hat[i, j], gamma_b.hat[i], Z_b[,
                                                                                                 j], theta_b.hat[i], lambda_b.hat[i])
      mat_gamma.star[i, j] <- temp[1]
      mat_delta.star[i, j] <- temp[2]
    }
  }
  if (prior.plots) {
    par(mfrow = c(2, 2))
    tmp <- density(gamma_bg.hat[1, ])
    plot(tmp, type = "l", main = "Density Plot")
    xx <- seq(min(tmp$x), max(tmp$x), length = 100)
    lines(xx, dnorm(xx, gamma_b.hat[1], sqrt(tau_b.hat[1])),
          col = 2)
    qqnorm(gamma_bg.hat[1, ])
    qqline(gamma_bg.hat[1, ], col = 2)
    tmp <- density(delta_bg.hat[1, ])
    invgam <- 1/rgamma(ncol(delta_bg.hat), lambda_b.hat[1],
                       theta_b.hat[1])
    tmp1 <- density(invgam)
    plot(tmp, typ = "l", main = "Density Plot", ylim = c(0,
                                                         max(tmp$y, tmp1$y)))
    lines(tmp1, col = 2)
    qqplot(delta_bg.hat[1, ], invgam, xlab = "Sample Quantiles",
           ylab = "Theoretical Quantiles")
    lines(c(0, max(invgam)), c(0, max(invgam)), col = 2)
    title("Q-Q Plot")
  }
  Zc_gamma.star <- Z - Xdes_batch %*% mat_gamma.star
  Zc_gamma.star_var <- sqrt(mat_var) * Zc_gamma.star
  Zc_gamma.star_var_delta.star <- Zc_gamma.star_var * (Xdes_batch %*%
                                                         (1/sqrt(mat_delta.star)))
  Y.star <- Zc_gamma.star_var_delta.star + mat_alpha.hat
  return(Y.star)
}

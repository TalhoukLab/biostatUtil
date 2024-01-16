data(cancer, package = "survival")

test_that("collapse_var combines unique elements of string", {
  x <- c("A", "A", "B", "C", "C")
  expect_equal(nchar(collapse_var(x)), 11)
  expect_equal(nchar(collapse_var(x, collapse = " ; ")), 9)
})

test_that("n_missing counts number of missing elements", {
  x <- c(1, 3, NA, 5)
  expect_equal(1, n_missing(x))
})

test_that("formatNA returns a factor", {
  y <- c(1:10, "Unk", 12)
  expect_type(formatNA(y), "integer")
  expect_equal(NA_character_, as.character(formatNA(y)[11]))
})

test_that("g_legend adds ggplot legend", {
  df <- data.frame(gp = factor(rep(letters[1:3], each = 10)),
                   y = rnorm(30))
  p <- ggplot(df, aes(x = gp, y = y)) +
    geom_point(aes(color = gp))
  expect_s3_class(g_legend(p), c("gtable", "gTree", "grob", "gDesc"))
})

test_that("getPval extracts p-value from survdiff objects", {
  x <- survdiff(Surv(time, status) ~ pat.karno + strata(inst), data = lung)
  expect_type(getPval(x), "double")
})

test_that("sem calculates standard error of the mean", {
  set.seed(1)
  x <- rnorm(100, 5, 2)
  expect_type(sem(x), "double")
  x[6] <- 0
  expect_type(sem(x, missing.value = 0), "double")
})

dev.off()
if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")

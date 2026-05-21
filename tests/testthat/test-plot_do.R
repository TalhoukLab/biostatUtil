mtcars$cyl <- as.factor(mtcars$cyl)

test_that("doBarplot works", {
  expect_error(doBarplot(mtcars, "cyl", "title = Number of cylinders"), NA)
})

test_that("doBoxplotAmongSubtypes works with correct var names", {
  expect_error(doBoxplotAmongSubtypes(mtcars, "Boxplot of qsec vs. gear",
                                      "qsec", "QSEC",
                                      "gear", "GEAR"), NA)
  expect_error(doBoxplotAmongSubtypes(mtcars, "Boxplot of qsec vs. gear",
                                      "qc", "QSEC",
                                      "gar", "GEAR"))
})

test_that("doBoxplotAmongSubtypes works with factors", {
  expect_warning(doBoxplotAmongSubtypes(mtcars, "Boxplot of qsec vs. cyl",
                                        "qsec", "QSEC",
                                        "cyl", "CYL"), NA)
})

test_that("doHist works with or without title", {
  expect_error(doHist(mtcars, "mpg", title = "Distribution of MPG"), NA)
  expect_error(doHist(mtcars, "mpg", show.title = FALSE), NA)
})

test_that("doJitterplotAmongSubtypes works with factors", {
  expect_warning(doJitterplotAmongSubtypes(mtcars, "Boxplot of qsec vs. cyl",
                                           "qsec", "QSEC",
                                           "cyl", "CYL"), NA)
})

dev.off()
if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")

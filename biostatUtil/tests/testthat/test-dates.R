
context("Date functions")

x1 <- "2014-07-08"
x2 <- "2014/07/08"
x3 <- "2014|07|08"
x4 <- "20140708"

test_that("Any date format works with any separator", {
  expect_identical("%m-%d-%Y", getFormat(x1, "MM.DD.YYYY"))
  expect_identical("%b-%d-%Y", getFormat(x1, "MMM.DD.YYYY"))
  expect_identical("%d-%m-%Y", getFormat(x1, "DD.MM.YYYY"))
  expect_identical("%d-%b-%Y", getFormat(x1, "DD.MMM.YYYY"))
  expect_identical("%Y-%m-%d", getFormat(x1, "YYYY.MM.DD"))
  expect_identical("%Y-%b-%d", getFormat(x1, "YYYY.MMM.DD"))
  
  expect_identical("%m/%d/%Y", getFormat(x2, "MM.DD.YYYY"))
  expect_identical("%b/%d/%Y", getFormat(x2, "MMM.DD.YYYY"))
  expect_identical("%d/%m/%Y", getFormat(x2, "DD.MM.YYYY"))
  expect_identical("%d/%b/%Y", getFormat(x2, "DD.MMM.YYYY"))
  expect_identical("%Y/%m/%d", getFormat(x2, "YYYY.MM.DD"))
  expect_identical("%Y/%b/%d", getFormat(x2, "YYYY.MMM.DD"))

  expect_identical("%m|%d|%Y", getFormat(x3, "MM.DD.YYYY"))
  expect_identical("%b|%d|%Y", getFormat(x3, "MMM.DD.YYYY"))
  expect_identical("%d|%m|%Y", getFormat(x3, "DD.MM.YYYY"))
  expect_identical("%d|%b|%Y", getFormat(x3, "DD.MMM.YYYY"))
  expect_identical("%Y|%m|%d", getFormat(x3, "YYYY.MM.DD"))
  expect_identical("%Y|%b|%d", getFormat(x3, "YYYY.MMM.DD"))
})

test_that("Custom separator can be added if '-', '/', '|' not used", {
  expect_identical("%m-%d-%Y", getFormat(x4, "MM.DD.YYYY", sep = "-"))
})

test_that("Error if invalid format provided", {
  expect_error(getFormat(x1, "MM.DD.FFFF"))
})

test_that("Format date works for any format", {
  expect_identical("07/08/2011", formatDate(8, 7, 2011))
  expect_identical("Jan-10-2015", formatDate(10, 1, 2015, date.format = "MMM.DD.YYYY", sep = "-"))
  expect_identical("08-07-2011", formatDate(8, 7, 2011, date.format = "DD.MM.YYYY", sep = "-"))
  expect_identical("08|Jul|2011", formatDate(8, 7, 2011, date.format = "DD.MMM.YYYY", sep = "|"))
  expect_identical("2011/07/08", formatDate(8, 7, 2011, date.format = "YYYY.MM.DD"))
  expect_identical("2016/Sep/11", formatDate(11, 9, 2016, date.format = "YYYY.MMM.DD"))
})

test_that("Numeric dates can be reformatted for certain original formats", {
  expect_identical("1991/Sep/11", cleanDate(11091991, original.format = "DD.MM.YYYY", preferred.format = "YYYY.MMM.DD"))
  expect_identical("1991/Sep/11", cleanDate(09111991, original.format = "MM.DD.YYYY", preferred.format = "YYYY.MMM.DD"))
  expect_identical("1991/Sep/11", cleanDate(19910911, original.format = "YYYY.MM.DD", preferred.format = "YYYY.MMM.DD"))
  expect_error(cleanDate(19910911, original.format = "YYYY.MMM.DD"))
})

test_that("Character dates can be reformatted for certain original formats", {
  expect_identical("1991/Sep/13", cleanDate("13/09/1991", original.format = "DD.MM.YYYY", preferred.format = "YYYY.MMM.DD"))
  expect_identical("1991/Sep/13", cleanDate("09/13/1991", original.format = "MM.DD.YYYY", preferred.format = "YYYY.MMM.DD"))
  expect_identical("1991/Sep/11", cleanDate("09/11/1991", original.format = "MM.DD.YYYY", preferred.format = "YYYY.MMM.DD"))
  expect_identical("1991/Nov/09", cleanDate("09/11/1991", original.format = "DD.MM.YYYY", preferred.format = "YYYY.MMM.DD"))
  expect_identical("1991/Sep/11", cleanDate("1991/09/11", original.format = "YYYY.MM.DD", preferred.format = "YYYY.MMM.DD"))
  expect_error(cleanDate("09/11/1991", original.format = "YYYY.MMM.DD"))
})

test_that("Reformatting dates handles erroneous inputs", {
  expect_equal("Missing", cleanDate(NA, return.missing.code = "Missing"))
  expect_equal(x4, cleanDate(x4, existing.missing.codes = x4))
  expect_equal(NA, cleanDate("This is not a date"))
})

test_that("Adding date works for allowed units", {
  expect_identical("2014/07/18", addToDate(x2, 10, date.format = "YYYY.MM.DD", units = "days"))
  expect_identical("2014/09/16", addToDate(x2, 10, date.format = "YYYY.MM.DD", units = "weeks"))
  expect_identical("2015/05/09", addToDate(x2, 10, date.format = "YYYY.MM.DD", units = "months"))
  expect_identical("2024/07/07", addToDate(x2, 10, date.format = "YYYY.MM.DD", units = "years"))
})

test_that("Adding date handles erroneous inputs", {
  expect_equal(NA, addToDate(NA, 10))
  expect_equal(NA, addToDate(x2, NA))
  expect_equal("Missing", addToDate(x2, 10, existing.missing.codes = x2, return.missing.code = "Missing"))
})

x5 <- "1992/01/27"
x6 <- "2003/03/21"

test_that("Subtracting date works for allowed units", {
  expect_equal(4071, diffDate(x6, x5, date.format = "YYYY.MM.DD", units = "days"))
  expect_equal(581.5714, diffDate(x6, x5, date.format = "YYYY.MM.DD", units = "weeks"), tolerance = .002)
  expect_equal(133.4754, diffDate(x6, x5, date.format = "YYYY.MM.DD", units = "months"), tolerance = .002)
  expect_equal(11.14607, diffDate(x6, x5, date.format = "YYYY.MM.DD", units = "years"), tolerance = .002)
})

test_that("Subtracting date handles erroneous inputs", {
  expect_equal(NA, diffDate(x6, NA))
  expect_equal(NA, diffDate(NA, x5))
  expect_equal("Missing", diffDate(x6, x5, existing.missing.codes = x5, return.missing.code = "Missing"))
  expect_equal("Missing", diffDate(x6, x5, existing.missing.codes = x6, return.missing.code = "Missing"))
})

test_that("Comparing dates works for any two valid dates", {
  expect_equal(-1, compareDate("01/22/1949", "04/13/1950", date.format = "MM.DD.YYYY"))
  expect_equal(0, compareDate("04/13/1950", "04/13/1950", date.format = "MM.DD.YYYY"))
  expect_equal(1, compareDate("04/13/1959", "04/13/1950", date.format = "MM.DD.YYYY"))
  expect_equal(NA, compareDate(NA, "04/13/1950", date.format = "MM.DD.YYYY"))
})

test_that("Wrapper around numeric method for as.Date uses 1970 as default origin", {
  expect_identical(as.Date("1970-01-11"), numericToDate(10))
  expect_identical(as.Date("2000-09-21"), numericToDate(10, "2000-09-11"))
})

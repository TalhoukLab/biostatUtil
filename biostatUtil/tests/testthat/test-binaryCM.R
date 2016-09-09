library(testthat)
# Aline Talhouk
# The test is performed on the binaryCM function using the same example from that function

test_binaryCM <- function(x,y, pcond){
bcm <- binaryCM(x, y, pcond = pcond)
# Check that entering a categorical variable with more than 2 categories will result in an error
z <-
  apply(rmultinom(n , size = 1, prob = c(0.2, 0.4, 0.4)), 2, function(x) {
    which(x == 1)
  })
expect_error(binaryCM(x, z))

# Check that the confusion Matrix is such that positive condition is in the (1,1) position of the table
expect_equal(sum(x == pcond & y == pcond), bcm$CM[1,1])

# Check that the output of the table corresponds to the individual elements
expect_true(all(bcm$table["Accuracy",] == bcm$Accuracy))
expect_true(all(bcm$table["Sensitivity",] == bcm$Sensitivity))
expect_true(all(bcm$table["Specificity",] == bcm$Specificity))
expect_true(all(bcm$table["PPV",] == bcm$PPV))
expect_true(all(bcm$table["NPV",] == bcm$NPV))
expect_true(all(bcm$table["kappa",] == bcm$kappa))

# Check that the point estimate is included in the 95 bootstrap interval
expect_true(all(bcm$table[, "Point Estimate"] > bcm$table[, "Lower CI"]) &
              all(bcm$table[, "Point Estimate"] < bcm$table[, "Upper CI"]))

# Check that the accuracy and a PPV and Specificity are correctly calculated

## Defined as sum of diagonal over total
expect_equal(bcm$Accuracy[, "PointEst"], (sum(x==pcond & y==pcond)+ sum(x!=pcond &y!=pcond)) / length(x))

## PPV is defined as sum of true positives/ predicted positive
expect_equal(bcm$PPV[,"PointEst"],sum(x == pcond & y == pcond) / sum(y == pcond), tolerance = .0002)

## Specificity is defined as sum of true negatives/ sum of true condition negative
expect_equal(bcm$Specificity[,"PointEst"],sum(x != pcond & y != pcond) / sum(x != pcond), tolerance = .0002)
}


set.seed(547)
n <- 80
x <- rbinom(n, size = 1, prob = 0.6) # Ref
xf <- factor(x, labels = c("positive","negative"))

y <- rbinom(n, size = 1, prob = 0.4) # Pred
yf <- factor(y, labels = c("negative","positive"))

pcondf <- "positive"
# Check that it works for numeric values (0/1)
test_binaryCM(x,y,pcond = 1)

# Check that it works with factors and labels
test_binaryCM(xf,yf,pcond = "positive")

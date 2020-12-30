context("make_random")
library(OKplan)
library(testthat)

test_that("Initializing seed TRUE/FALSE", {


# Add column with random variables
x <- as.data.frame(c(1:10))
seed <- 12345

# Initialize with seed first time used
x <- make_random(x, seed = seed, init_seed = TRUE)

# Do not initialize the seed thereafter to avoid overlapping
x <- make_random(x, colname = "random2", seed = seed, init_seed = FALSE)

# If you initialize again you get overlapping seeds
x <- make_random(x, colname = "random3", seed = seed, init_seed = TRUE)

# And now you get repetition of random2
x <- make_random(x, colname = "random4", seed = seed, init_seed = FALSE)

expect_identical(x$random, x$random3)
expect_identical(x$random2, x$random4)

})


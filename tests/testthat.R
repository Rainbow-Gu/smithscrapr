# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(smithscrapr)

test_check("smithscrapr")

expected_df <- data.frame(
  Requirement = c("Application", "Capstone", "Communication", "Core",
                     "Programming", "Statistics"),
  Must = c(NA, !NA, NA, !NA, NA, NA),
  Chose = c(!NA, NA, !NA, NA, !NA, !NA)
)

test_that("My data frame looks right", {
  expect_equal(req_df(sds), expected_df)
}
  )

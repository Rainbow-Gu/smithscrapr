
sds_requirements <- req_df("sds")

test_that("My data frame looks right", {
  expect_equal(nrow(sds_requirements), 6)
  expect_equal(ncol(sds_requirements), 3)
  expect_equal(colnames(sds_requirements), c("Requirement", "Must", "Choose"))
  expect_equal(sds_requirements$Requirement, c("Application", "Capstone",
                                                "Communication", "Core",
                                                "Programming", "Statistics"))
  expect_equal(sds_requirements$Must == "",
               c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE))
  expect_equal(sds_requirements$Choose == "",
               c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE))
})

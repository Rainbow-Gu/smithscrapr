
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

bio_t5_requirements <- bio_track_matcher(5)

test_that("My data frame looks right", {
  expect_equal(nrow(bio_t5_requirements), 7)
  expect_equal(ncol(bio_t5_requirements), 3)
  expect_equal(colnames(bio_t5_requirements), c("Requirements", "Must", "Choose.1"))
  expect_equal(bio_t5_requirements$Requirement, c("core", "chem", "sds", "course", "lab",
                                                  "education",
                                                  "Outside of major requirement (for license)"))
  expect_equal(bio_t5_requirements$Must == "",
               c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(bio_t5_requirements$Choose.1 == "",
               c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
})

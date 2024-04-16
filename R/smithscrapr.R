
library(rvest)
library(stringr)
library(dplyr)

sds <- read_html("https://www.smith.edu/academics/statistical-data-sciences#statistical-and-data-sciences-major")

sds_core <- sds |>
  html_elements("#statistical-and-data-sciences-major li li span") |>
  html_text2()

sds_core <- unique(sds_core)

# combine three indexes
combined <- paste(c(sds_core[(5 - 1):(5 + 1)]), collapse = " ")
sds_core[(5 - 1)] <- combined
sds_core <- sds_core[-c(5, 5 + 1)]

sds_programming <- sds |>
  html_elements("#statistical-and-data-sciences-major p+ ol > li:nth-child(2) .code_bubble") |>
  html_text2()

sds_statistics <- sds |>
  html_elements("#statistical-and-data-sciences-major .text > ol > li:nth-child(3) span") |>
  html_text2()

# remove duplicates
sds_statistics <- unique(sds_statistics)

# remove everything before "a"
sds_statistics <- str_remove(sds_statistics, pattern = ".+(?=a)")

sds_communication <- sds |>
  html_elements("p+ ol > li:nth-child(4) .code_bubble") |>
  html_text2()

sds_application <- sds |>
  html_elements("#statistical-and-data-sciences-major li+ li li") |>
  html_text2()

sds_capstone <- sds |>
  html_elements("ol li:nth-child(6) .code_bubble") |>
  html_text2()

sds_list <- list("core (take all)" = sds_core,
           "programming depth (take one)" = sds_programming,
           "statistics depth (take one)" = sds_statistics,
           "communication (take one)" = sds_communication,
           "application domain (take one)" = sds_application,
           capstone = sds_capstone
           )


# input NA values to make equal length
for (i in seq_along(sds_list)) {
  sds_list[[i]] <- c(sds_list[[i]], rep(NA, 7 - length(sds_list[[i]])))
}

# convert the list to a data frame
sds_df <- data.frame(sds_list)

# function that prints the df
sds_req <- function() {
  print(knitr::kable(sds_df))
}

sds_req()


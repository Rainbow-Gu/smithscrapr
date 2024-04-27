library(rvest)
library(stringr)
library(dplyr)
library(tidyr)

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

sds_list <- list(Core = sds_core,
                 Programming = sds_programming,
                 Statistics = sds_statistics,
                 Communication = sds_communication,
                 Application = sds_application,
                 Capstone = sds_capstone)


# function
req_df <- function (list, must) {
  max_length <- max(sapply(list, length))
  for (i in seq_along(list)) {
    length(list[[i]]) <- max_length}
  df <- data.frame(list, check.names = FALSE)

   pivot_longer(
    df,
    cols = everything(),
    names_to = "Requirement",
    values_to = "Class"
  ) |>
    mutate(Must = ifelse(Requirement %in% must, Class, NA_character_),
           Class = replace(Class, Class == Must, NA_character_)
    ) |>
    select(Requirement, Must, Choose = Class) |>
    filter(!(is.na(Must) & is.na(Choose))) |>
    group_by(Requirement) |>
    summarize(Must = paste(Must[!is.na(Must)], collapse = ", "),
              Choose = paste(Choose[!is.na(Choose)], collapse = ", ")
    )
}

sds_df <- req_df(sds_list, c("Core", "Capstone"))


usethis::use_data(sds_df, overwrite = TRUE)

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


# input NA values to make equal length
for (i in seq_along(sds_list)) {
  sds_list[[i]] <- c(sds_list[[i]], rep(NA, 7 - length(sds_list[[i]])))
}

# convert the list to a data frame
sds_df <- data.frame(sds_list)

# convert the data frame
pivot_longer(
  sds_df,
  cols = everything(),
  names_to = "Requirement",
  values_to = "Class"
) |>
  mutate(Must = ifelse(Requirement %in% c("Core", "Capstone"), Class, NA_character_),
         Class = replace(Class, Class == Must, NA_character_)
         ) |>
  select(Requirement, Must, `Choose One` = Class) |>
  filter(!(is.na(Must) & is.na(`Choose One`))) |>
  group_by(Requirement) |>
  summarize(Must = paste(Must[!is.na(Must)], collapse = ", "),
            `Choose One` = paste(`Choose One`[!is.na(`Choose One`)], collapse = ", ")
  ) |>
  arrange(Requirement)

# function

pivot_df <- function (df, must) {
  choose <- c("econ_df", "ast_df")
  choose_one <- c("sds_df", "cs_df", "biochem_df")
  pivot_longer(
    df,
    cols = everything(),
    names_to = "Requirement",
    values_to = "Class"
  ) |>
    mutate(Must = ifelse(Requirement %in% must, Class, NA_character_),
           Class = replace(Class, Class == Must, NA_character_)
    ) |>
    select(Requirement, Must, `Choose One` = Class) |>
    filter(!(is.na(Must) & is.na(`Choose One`))) |>
    group_by(Requirement) |>
    summarize(Must = paste(Must[!is.na(Must)], collapse = ", "),
              `Choose One` = paste(`Choose One`[!is.na(`Choose One`)], collapse = ", ")
    )
  if (df %in% choose) {
    colnames[3] <- "Choose"
  }

}

sds_df <- pivot_df(sds_df, c("Core", "Capstone"))
ast_df <- pivot_df(ast_df, "Core")



## Computer Science
cs <- read_html("https://www.smith.edu/academics/computer-science#computer-science-courses")
cs_intro <- cs |>
  html_elements("#computer-science-major li:nth-child(1) li") |>
  html_text2()
# Remove "(S/U only)" from each string
cs_intro <- str_remove(cs_intro, pattern = " \\(S/U only\\)")

cs_core <- cs |>
  html_elements("#computer-science-major li:nth-child(2) li") |>
  html_text2()
## Maybe instead of #computer-science-major, we can just use the unique
## function variables so that we can create a function since they are
## similar??
cs_math <- cs |>
  html_elements("li:nth-child(3) li .sc_courseinline:nth-child(1) .code_bubble") |>
  html_text2()

cs_thoery <- cs |>
  html_elements("li:nth-child(4) li:nth-child(1)") |>
  html_text2()
cs_thoery<- cs_thoery[2]
cs_thoery <- str_remove(cs_thoery, pattern = "One CSC or SDS theory course: ")
# Split the remaining string into a list
cs_thoery <- str_split(cs_thoery, pattern = ",\\s*")[[1]]

cs_programming <- cs |>
  html_elements("li:nth-child(4) li:nth-child(2)") |>
  html_text2()
cs_programming<- cs_programming[2]

cs_programming <- str_remove(cs_programming, pattern = "One CSC or SDS programming course: ")
cs_programming <- str_split(cs_programming, pattern = ",\\s*")[[1]]
## I am sure we can combine this to make the pattern be similar but i cannot for
## the life of me figure out how

cs_system <- cs |>
  html_elements("#computer-science-major li li~ li+ li .sc_courseinline+ .sc_courseinline .code_bubble")|>
  html_text2()

cs_200 <- cs |>
  html_elements("li li:nth-child(4)") |>
  html_text2()
cs_200 <- cs_200[6]

cs_300 <- cs |>
  html_elements("#computer-science-major li:nth-child(5)") |>
  html_text2()
## Function to turn our data.frames into list then into data frames???, making
## sure our data frames are also same length as well
cs_list <- list(Introduction = cs_intro,
                Core = cs_core,
                Mathematics = cs_math,
                Theory = cs_thoery,
                Programming = cs_programming,
                System = cs_system,
                Level_200 = cs_200,
                Level_300 = cs_300
)

max_length <- max(sapply(cs_list, length))

for (i in seq_along(cs_list)) {
  cs_list[[i]] <- `length<-`(cs_list[[i]], max_length)
}
cs_df <- data.frame(cs_list)

x <- pivot_longer(
  cs_df,
  cols = everything(),
  names_to = "Requirement",
  values_to = "Class"
) |>
  mutate(Must = ifelse(Requirement %in% c("Introduction", "Core", "Mathematics"), Class, NA_character_),
         Class = replace(Class, Class == Must, NA_character_)
  ) |>
  select(Requirement, Must, `Choose` = Class) |>
  filter(!(is.na(Must) & is.na(`Choose`))) |>
  group_by(Requirement) |>
  summarize(Must = paste(Must[!is.na(Must)], collapse = ", "),
            `Choose` = paste(`Choose`[!is.na(`Choose`)], collapse = ", ")
  )
## For computer science I used a lot of the same code, so hopper is right about there being repetative code

## For quantative economics

econ <- read_html("https://www.smith.edu/academics/economics#advisers-1")
econ_core <- econ |>
  html_elements("p+ ol > li > ol > li:nth-child(4) , ol:nth-child(10) ol ol li+ li , ol:nth-child(10) ol .code_bubble") |>
  html_text2()
econ_core <- econ_core[!(econ_core %in% c("SDS 201", "SDS 220", "ECO 250", "ECO 253"))]
econ_core <- unique(econ_core)
econ_core <- str_remove(econ_core, "taken at Smith,?\\s*")

econ_upper <- econ |>
  html_elements("#economics-major p+ ol > li+ li .code_bubble") |>
  html_text2()

econ_electives <- econ |>
  html_elements("ol:nth-child(10) > li:nth-child(3)") |>
  html_text2()

econ_seminar <- econ |>
  html_elements("p+ ol > li:nth-child(4)") |>
  html_text2()
econ_list <- list(Core = econ_core,
                  Upper_level = econ_upper,
                  Electives = econ_electives,
                  Seminar = econ_seminar
)

max_length <- max(sapply(econ_list, length))

for (i in seq_along(econ_list)) {
  econ_list[[i]] <- `length<-`(econ_list[[i]], max_length)
}
econ_df <- data.frame(econ_list)

# function
get_same_length <- function(list) {
  max_length <- max(sapply(list, length))
  for (i in seq_along(list)) {
    length(list[[i]]) <- max_length
  }
  return(list)
}

get_same_length(econ_list)

# function
list_to_df <- function(list) {
  df <- data.frame(list)
  return(df)
}

#

econ_df <- list_to_df(econ_list)

econ_df <- pivot_longer(
  econ_df,
  cols = everything(),
  names_to = "Requirement",
  values_to = "Class"
) |>
  mutate(Must = ifelse(Requirement %in% "Core", Class, NA_character_),
         Class = replace(Class, Class == Must, NA_character_)
  ) |>
  select(Requirement, Must, `Choose` = Class) |>
  filter(!(is.na(Must) & is.na(`Choose`))) |>
  group_by(Requirement) |>
  summarize(Must = paste(Must[!is.na(Must)], collapse = ", "),
            `Choose` = paste(`Choose`[!is.na(`Choose`)], collapse = ", ")
  )

# astronomy
ast <- read_html("https://www.smith.edu/academics/astronomy")

ast_core <- ast |>
  html_elements("li .code_bubble") |>
  html_text2()

ast_core <- unique(ast_core)

ast_core <- c(paste(ast_core[1:2], collapse = " or "), ast_core[-(1:2)])

ast_200 <- ast |>
  html_elements("li:nth-child(4) span") |>
  html_text2()

ast_200 <- ast_200[3]

ast_300 <- ast |>
  html_elements("#astronomy-major li:nth-child(5) span") |>
  html_text2()

ast_200_or_300 <- ast |>
  html_elements("li:nth-child(7)") |>
  html_text2()

ast_200_or_300 <- ast_200_or_300[6]

ast_list <- list(Core = ast_core,
                 "200" = ast_200,
                 "300" = ast_300,
                 "200/300" = ast_200_or_300)

max_length_ast <- max(sapply(ast_list, length))
for (i in seq_along(ast_list)) {
  ast_list[[i]] <- c(ast_list[[i]], rep(NA, max_length_ast - length(ast_list[[i]])))
}

ast_df <- data.frame(ast_list, check.names = FALSE)

ast_df <- pivot_longer(
  ast_df,
  cols = everything(),
  names_to = "Level",
  values_to = "Class"
) |>
  mutate(Must = ifelse(Level == "Core", Class, NA_character_),
         Class = replace(Class, Class == Must, NA_character_)
  ) |>
  select(Level, Must, `Choose` = Class) |>
  filter(!(is.na(Must) & is.na(`Choose`))) |>
  group_by(Level) |>
  summarize(Must = paste(Must[!is.na(Must)], collapse = ", "),
            `Choose` = paste(`Choose`[!is.na(`Choose`)], collapse = ", ")
  ) |>
  arrange(Level)

# biochem
biochem <- read_html("https://www.smith.edu/academics/biochemistry#biochemistry-major")

biochem_fdn_bio <- biochem |>
  html_elements("li:nth-child(1) li") |>
  html_text2()

biochem_fdn_bio <- biochem_fdn_bio[8:10]
biochem_fdn_bio <- str_remove(biochem_fdn_bio, "or.*")

biochem_fdn_gen_chem <- biochem |>
  html_elements("li:nth-child(2) li") |>
  html_text2()

biochem_fdn_gen_chem <- biochem_fdn_gen_chem[11:12]
biochem_fdn_gen_chem[1] <- paste0("either ", biochem_fdn_gen_chem[1])
biochem_fdn_gen_chem <- str_remove(biochem_fdn_gen_chem, ", this.*")
biochem_fdn_gen_chem <- paste(biochem_fdn_gen_chem, collapse = " ")

biochem_fdn_org_chem <- biochem |>
  html_elements("li:nth-child(3) .code_bubble") |>
  html_text2()

biochem_fdn_org_chem <- biochem_fdn_org_chem[3:6]

biochem_fnd_biochem <- biochem |>
  html_elements("li:nth-child(4) .code_bubble") |>
  html_text2()

biochem_fnd_biochem <- c(paste(biochem_fnd_biochem, collapse = "/"))

biochem_physiology <- biochem |>
  html_elements("li:nth-child(5) .code_bubble") |>
  html_text2()

biochem_physiology <- c(paste(biochem_physiology[2:3], collapse = "/"), biochem_physiology[-(2:3)])
biochem_physiology <- c(paste(biochem_physiology[4:5], collapse = "/"), biochem_physiology[-(4:5)])

biochem_upper_biochem <- biochem |>
  html_elements("li:nth-child(6) li") |>
  html_text2()

biochem_upper_biochem <- biochem_upper_biochem[7:8]

biochem_elective <- biochem |>
  html_elements("li:nth-child(7)") |>
  html_text2()

biochem_elective <- biochem_elective[5]
biochem_elective <- str_extract(biochem_elective, "A.*")

biochem_list <- list("Foundation Bio" = biochem_fdn_bio,
                     "Foundation General Chem" = biochem_fdn_gen_chem,
                     "Foundation Organic Chem" = biochem_fdn_org_chem,
                     "Foundation Biochem" = biochem_fnd_biochem,
                     Physiology = biochem_physiology,
                     "Upper-level Biochem" = biochem_upper_biochem,
                     Elective = biochem_elective
)

max_length_biochem <- max(sapply(biochem_list, length))
for (i in seq_along(biochem_list)) {
  biochem_list[[i]] <- c(biochem_list[[i]], rep(NA, max_length_biochem - length(biochem_list[[i]])))
}

biochem_df <- data.frame(biochem_list, check.names = FALSE)

biochem_df <- pivot_longer(
  biochem_df,
  cols = everything(),
  names_to = "Requirment",
  values_to = "Class"
) |>
  mutate(Must = ifelse(Requirment %in% c("Foundation Bio", "Foundation General Chem",
                                         "Foundation Organic Chem", "Foundation Biochem",
                                         "Upper-level Biochem"), Class, NA_character_),
         Class = replace(Class, Class == Must, NA_character_)
  ) |>
  select(Requirment, Must, `Choose one` = Class) |>
  filter(!(is.na(Must) & is.na(`Choose one`))) |>
  group_by(Requirment) |>
  summarize(Must = paste(Must[!is.na(Must)], collapse = ", "),
            `Choose one` = paste(`Choose one`[!is.na(`Choose one`)], collapse = ", ")
  ) |>
  arrange(Requirment)

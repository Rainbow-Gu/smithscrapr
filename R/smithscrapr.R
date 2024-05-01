#' @title Make tables for major requirements
#'
#' @description
#' Given a list of web-scraped major requirements, turn it into a cleaned data frame
#'
#' @importFrom dplyr mutate select filter group_by summarise
#' @importFrom tidyr pivot_longer
#'
#' @param major A character vector of a major's abbreviation from below: "sds", "csc", "eco",
#' "bch", "ast", or "chm". Please put quotation marks around the abbreviations.
#'
#' @return A data frame that categorizes courses for major requirements under must or choose to
#' take columns
#'
#' Note that for each row, courses appears in one column
#'
#' @examples
#' req_df("sds")
#'
#' @export
#'

req_df <- function(major) {
  if (major %in% "sds") {
    list <- sds()
    must <- c("Core", "Capstone")
  } else if (major %in% "chm") {
    list <- chm()
    must <- c("Intro (Choice A)", "Intro (Choice B)")
  } else if (major %in% "bch") {
    list <- bch()
    must <- c("Foundation Bio", "Foundation General Chem",
              "Foundation Organic Chem", "Foundation Biochem", "Upper-level Biochem")
  } else if (major %in% "ast") {
    list <- ast()
    must <- c("Core")
  } else if (major %in% "csc") {
    list <- csc()
    must <- c("Introduction", "Core", "Mathematics")
  } else if (major %in% "eco") {
    list <- eco()
    must <- c("Core")
  } else {
    stop("Input argument doesn't exist, check the help page for available majors")
  }

  max_length <- max(sapply(list, length))
  for (i in seq_along(list)) {
    length(list[[i]]) <- max_length
  }
  df <- data.frame(list, check.names = FALSE)

  pivot_longer(
               df,
               cols = everything(),
               names_to = "Requirement",
               values_to = "Class") |>
    mutate(Must = ifelse(Requirement %in% must, Class, NA_character_),
      Class = replace(Class, Class == Must, NA_character_)
    ) |>
    select(Requirement, Must, Choose = Class) |>
    filter(!(is.na(Must) & is.na(Choose))) |>
    group_by(Requirement) |>
    summarise(Must = paste(Must[!is.na(Must)], collapse = ", "),
      Choose = paste(Choose[!is.na(Choose)], collapse = ", ")
    )
}

#' @importFrom rvest html_elements html_text2 read_html
#' @importFrom stringr str_remove str_split str_extract

sds <- function() {
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

  list(Core = sds_core,
       Programming = sds_programming,
       Statistics = sds_statistics,
       Communication = sds_communication,
       Application = sds_application,
       Capstone = sds_capstone)
}

#' @importFrom rvest html_elements html_text2 read_html
#' @importFrom stringr str_remove str_split str_extract

csc <- function() {
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
  cs_thoery <- cs_thoery[2]
  cs_thoery <- str_remove(cs_thoery, pattern = "One CSC or SDS theory course: ")
  # Split the remaining string into a list
  cs_thoery <- str_split(cs_thoery, pattern = ",\\s*")[[1]]

  cs_programming <- cs |>
    html_elements("li:nth-child(4) li:nth-child(2)") |>
    html_text2()
  cs_programming <- cs_programming[2]

  cs_programming <- str_remove(cs_programming, pattern = "One CSC or SDS programming course: ")
  cs_programming <- str_split(cs_programming, pattern = ",\\s*")[[1]]
  ## I am sure we can combine this to make the pattern be similar but i cannot for
  ## the life of me figure out how

  cs_system <- cs |>
    html_elements("#computer-science-major li li~ li+ li .sc_courseinline+ .sc_courseinline .code_bubble") |>
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
  list(Introduction = cs_intro,
       Core = cs_core,
       Mathematics = cs_math,
       Theory = cs_thoery,
       Programming = cs_programming,
       System = cs_system,
       Level_200 = cs_200,
       Level_300 = cs_300)
}

#' @importFrom rvest html_elements html_text2 read_html
#' @importFrom stringr str_remove str_split str_extract

eco <- function() {
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
  list(Core = econ_core,
       Upper_level = econ_upper,
       Electives = econ_electives,
       Seminar = econ_seminar)
}


#' @importFrom rvest html_elements html_text2 read_html
#' @importFrom stringr str_remove str_split str_extract

ast <- function() {
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

  list(Core = ast_core,
       "200" = ast_200,
       "300" = ast_300,
       "200/300" = ast_200_or_300)
}

#' @importFrom rvest html_elements html_text2 read_html
#' @importFrom stringr str_remove str_split str_extract

bch <- function() {
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

  biochem_physiology <- c(paste(biochem_physiology[2:3], collapse = "/"),
                          biochem_physiology[-(2:3)])
  biochem_physiology <- c(paste(biochem_physiology[4:5], collapse = "/"),
                          biochem_physiology[-(4:5)])

  biochem_upper_biochem <- biochem |>
    html_elements("li:nth-child(6) li") |>
    html_text2()

  biochem_upper_biochem <- biochem_upper_biochem[7:8]

  biochem_elective <- biochem |>
    html_elements("li:nth-child(7)") |>
    html_text2()

  biochem_elective <- biochem_elective[5]
  biochem_elective <- str_extract(biochem_elective, "A.*")

  list("Foundation Bio" = biochem_fdn_bio,
       "Foundation General Chem" = biochem_fdn_gen_chem,
       "Foundation Organic Chem" = biochem_fdn_org_chem,
       "Foundation Biochem" = biochem_fnd_biochem,
       Physiology = biochem_physiology,
       "Upper-level Biochem" = biochem_upper_biochem,
       Elective = biochem_elective)
}

#' @importFrom rvest html_elements html_text2 read_html
#' @importFrom stringr str_remove str_split str_extract

chm <- function() {
  chem <- read_html("https://www.smith.edu/academics/chemistry#chemistry-major")

  chem_intro_1a <- chem |>
    html_elements("li ol li:nth-child(1)") |>
    html_text2() |>
    unique()

  chem_intro_1a <- chem_intro_1a[-2]
  chem_intro_1a <- unlist(strsplit(chem_intro_1a, ", "))
  chem_intro_1a <- c(chem_intro_1a[1], unlist(strsplit(chem_intro_1a[2], " and ")))
  chem_intro_1a[3] <- gsub("\\n", "", chem_intro_1a[3])
  chem_intro_1a[3] <- gsub(" or", "", chem_intro_1a[3])

  chem_intro_1b <- chem |>
    html_elements("li ol li+ li") |>
    html_text2() |>
    unique()

  chem_intro_1b <- chem_intro_1b[-2]
  chem_intro_1b <- unlist(strsplit(chem_intro_1b, " and "))

  # choose 3
  chem_intermediate <- chem |>
    html_elements("#chemistry-major p+ ol > li:nth-child(2)") |>
    html_text2() |>
    unique()

  chem_intermediate <- str_remove(chem_intermediate, "(.*): ")
  chem_intermediate <- unlist(strsplit(chem_intermediate, ", "))
  chem_intermediate <- c(chem_intermediate[1], unlist(strsplit(chem_intermediate[2], " and ")))

  # choose 2
  chem_adv_lab <- chem |>
    html_elements("li~ li+ li span .sc_courseinline .code_bubble") |>
    html_text2()

  # choose 2 or 3
  chem_electives <- chem |>
    html_elements("#chemistry-major ul li") |>
    html_text2()

  chem_electives <- c(chem_electives[1], unlist(strsplit(chem_electives[2], ", ")),
                      chem_electives[3])
  chem_electives <-  c(chem_electives[1:5], unlist(strsplit(chem_electives[6], " or ")),
                       chem_electives[7])

  list("Intro (Choice A)" = chem_intro_1a,
       "Intro (Choice B)" = chem_intro_1b,
       "Courses (choose 3)" = chem_intermediate,
       "Advanced Lab (choose 2)" = chem_adv_lab,
       "Electives (2-3 to reach 10)" = chem_electives)
}

#' @title Make tables for Biology's major requirements
#'
#' @description
#' Given a list of web-scraped major requirements for each five of Biology's tracks, turn it into a
#' cleaned data frame
#'
#' @importFrom rvest html_elements html_text2 read_html
#' @importFrom stringr str_remove str_split str_extract str_match
#' @importFrom dplyr mutate select filter group_by summarise
#' @importFrom tidyr pivot_longer
#'
#' @param track A numeric value 1-5 that represent which track of biology the user is looking for
#'
#' @return A data frame that categorizes courses for major requirements under must or choose to take
#' columns
#'
#' Note that for each row, courses appears in one column
#'
#' @examples
#' track_1 <- bio_track_matcher(1)
#' track_1
#'
#' @export
#'

bio_track_matcher <- function(track) {
  bio <- read_html("https://www.smith.edu/academics/biological-sciences")
  if (track == 1 | track == 2 | track == 3 | track == 4 | track == 5) {

    # must take
    bio_core <- bio |>
      html_elements("li:nth-child(1) li~ li+ li , ol:nth-child(6) ol:nth-child(1) .code_bubble") |>
      html_text2()

    bio_core <- bio_core[7:9]

    bio_core[3] <- str_remove(bio_core[3], pattern = "[:punct:]")

    # choose 1
    bio_chem_req <- bio |>
      html_elements("ol:nth-child(6) > li:nth-child(2) .code_bubble") |>
      html_text2()
    bio_sds_req <- bio |>
      html_elements("ol:nth-child(24) li~ li+ li > .sc_courseinline .code_bubble") |>
      html_text2()

    if (track != 5) {

      # choose 2
      bio_elective <- bio |>
        html_elements("ol:nth-child(6) li:nth-child(5)") |>
        html_text2()

      # must
      bio_lab <- bio |>
        html_elements("ol:nth-child(3) span") |>
        html_text2() |>
        unique()
      bio_lab_base <- bio_lab[1:2] # use this for table

      if (track == 1) {
        # NA
        bio_upper <- bio |> # row name must include "one course from each of Tracks 2-4"
          html_elements("ol:nth-child(2) span") |>
          html_text2()

        # choose 2
        bio_upper_300 <- c(str_remove(bio_upper[1], pattern = "^[A-Za-z]{3} "), "from tracks 2-4")
        bio_upper_300 <- paste(bio_upper_300, collapse = " ")
        # choose 3
        bio_upper_200 <- c(str_remove(bio_upper[2], pattern = "^[A-Za-z]{3} "), "from tracks 2-4")
        bio_upper_200 <- paste(bio_upper_200, collapse = " ")
        # choose 1
        bio_lab_300 <- c(str_remove(bio_lab[3], pattern = "^[A-Za-z]{3} "), "from tracks 2-4")
        bio_lab_300 <- paste(bio_lab_300, collapse = " ")
        # choose 2
        bio_lab_200 <- c(str_remove(bio_lab[4], pattern = "^[A-Za-z]{3} "), "from tracks 2-4")
        bio_lab_200 <- paste(bio_lab_200, collapse = " ")
      }

      if (track == 2) {
        # choose 2
        bio_upper_300 <- bio |>
          html_elements(".sc_courseinline:nth-child(24) .code_bubble , .sc_courseinline+ span .sc_courseinline .code_bubble , .sc_courseinline+ span:nth-child(23) , .sc_courseinline:nth-child(22) .code_bubble , .sc_courseinline:nth-child(20) .code_bubble , .sc_courseinline:nth-child(18) .code_bubble , .sc_courseinline:nth-child(16) .code_bubble , .sc_courseinline:nth-child(14) .code_bubble") |>
          html_text2()

        bio_upper_300 <- c(bio_upper_300[1:5], strsplit(bio_upper_300[6], ", ")[[1]],
                           bio_upper_300[7:8])
        bio_upper_300 <- bio_upper_300[-6]

        # choose 3
        bio_upper_200 <- bio |>
          html_elements("p+ p > .sc_courseinline+ .sc_courseinline:nth-child(2) .code_bubble , p:nth-child(11) .sc_courseinline:nth-child(4) .code_bubble , p:nth-child(11) .sc_courseinline:nth-child(6) .code_bubble , p:nth-child(11) .sc_courseinline:nth-child(8) .code_bubble , p > .sc_courseinline:nth-child(10) .code_bubble , p > .sc_courseinline:nth-child(12) .code_bubble , p:nth-child(11) > .sc_courseinline:nth-child(1) .code_bubble") |>
          html_text2()

        bio_upper_200 <- c(bio_upper_200, bio_upper_300)

        # choose 1
        bio_lab_300 <- bio |>
          html_elements("#docs-internal-guid-5bdd37a7-7fff-3e74-b1f3-d28628ab7848 .sc_courseinline:nth-child(8) .code_bubble , #docs-internal-guid-5bdd37a7-7fff-3e74-b1f3-d28628ab7848 .sc_courseinline:nth-child(7) .code_bubble , #docs-internal-guid-5bdd37a7-7fff-3e74-b1f3-d28628ab7848 .sc_courseinline:nth-child(6) .code_bubble") |>
          html_text2()

        # choose 2
        bio_lab_200 <- bio |>
          html_elements("#docs-internal-guid-5bdd37a7-7fff-3e74-b1f3-d28628ab7848 .sc_courseinline:nth-child(5) .code_bubble , #docs-internal-guid-5bdd37a7-7fff-3e74-b1f3-d28628ab7848 .sc_courseinline:nth-child(4) .code_bubble , #docs-internal-guid-5bdd37a7-7fff-3e74-b1f3-d28628ab7848 .sc_courseinline:nth-child(3) .code_bubble , #docs-internal-guid-5bdd37a7-7fff-3e74-b1f3-d28628ab7848 .sc_courseinline:nth-child(2) .code_bubble , #docs-internal-guid-5bdd37a7-7fff-3e74-b1f3-d28628ab7848 .sc_courseinline:nth-child(1) .code_bubble") |>
          html_text2()

        bio_lab_200 <- c(bio_lab_200, bio_lab_300)
      }

      if (track == 3) {
        # choose 2
        bio_upper_300 <- bio |>
          html_elements("p:nth-child(15) span:nth-child(26) , p:nth-child(15) span:nth-child(22) , p:nth-child(15) .sc_courseinline:nth-child(25) .code_bubble , p:nth-child(15) .sc_courseinline:nth-child(23) .code_bubble , p:nth-child(15) .sc_courseinline:nth-child(21) .code_bubble , p:nth-child(15) .sc_courseinline:nth-child(19) .code_bubble , p:nth-child(15) .sc_courseinline:nth-child(17) .code_bubble , p:nth-child(15) span:nth-child(16) , p:nth-child(15) .sc_courseinline:nth-child(15) .code_bubble , p:nth-child(15) .sc_courseinline:nth-child(13) .code_bubble , p:nth-child(15) .sc_courseinline:nth-child(11) .code_bubble") |>
          html_text2()

        bio_upper_300 <- c(bio_upper_300[1:3],
          str_remove(bio_upper_300[4], pattern = ", "),
          bio_upper_300[5:7],
          strsplit(bio_upper_300[8], ", ")[[1]],
          bio_upper_300[9:10],
          str_match(bio_upper_300[11], pattern = "a .*[0-9]")
        )
        bio_upper_300 <- bio_upper_300[-8]

        # choose 3
        bio_upper_200 <- bio |>
          html_elements("p:nth-child(15) .sc_courseinline:nth-child(9) .code_bubble , p:nth-child(15) .sc_courseinline:nth-child(7) .code_bubble , p:nth-child(15) .sc_courseinline:nth-child(5) .code_bubble , p:nth-child(15) .sc_courseinline:nth-child(3) .code_bubble , p:nth-child(15) .sc_courseinline:nth-child(1) .code_bubble") |>
          html_text2()

        bio_upper_200 <- c(bio_upper_200, bio_upper_300)

        # choose 1
        bio_lab_300 <- bio |>
          html_elements("p+ p > .sc_courseinline~ .sc_courseinline+ .sc_courseinline .code_bubble") |>
          html_text2()

        # choose 2
        bio_lab_200 <- bio |>
          html_elements("#docs-internal-guid-5bdd37a7-7fff-3e74-b1f3-d28628ab7848 .code_bubble") |>
          html_text2()
      }

      if (track == 4) {
        # choose 2
        bio_upper_300 <- bio |>
          html_elements(".sc_courseinline:nth-child(27) .code_bubble , p:nth-child(20) span:nth-child(26) , p:nth-child(20) .sc_courseinline:nth-child(25) .code_bubble , p:nth-child(20) .sc_courseinline:nth-child(23) .code_bubble , p:nth-child(20) .sc_courseinline:nth-child(21) .code_bubble , p:nth-child(20) .sc_courseinline:nth-child(19) .code_bubble") |>
          html_text2()

        bio_upper_300 <- c(bio_upper_300[1:4],
                           str_match(bio_upper_300[5], pattern = "a.+[0-9]"),
                           bio_upper_300[6])

        # choose 3
        bio_upper_200 <- bio |>
          html_elements("p:nth-child(20) span") |>
          html_text2() |>
          unique()

        bio_upper_200 <- c(bio_upper_200[1],
                           bio_upper_200[3:14],
                           str_match(bio_upper_200[15], pattern = "a.+[0-9]"),
                           bio_upper_200[16])

        # choose 1
        bio_lab_300 <- bio |>
          html_elements("span .sc_courseinline:nth-child(13) .code_bubble , span .sc_courseinline:nth-child(12) .code_bubble , span .sc_courseinline:nth-child(11) .code_bubble , span .sc_courseinline:nth-child(10) .code_bubble , span .sc_courseinline:nth-child(9) .code_bubble , span+ span .sc_courseinline:nth-child(8) .code_bubble") |>
          html_text2()

        # choose 2
        bio_lab_200 <- bio |>
          html_elements("#docs-internal-guid-f072ab10-7fff-49f5-1466-9b0c21f6f0eb .code_bubble") |>
          html_text2()

      }

      # function for track 1-4
      bio_t_df <- data.frame(
        Requirements = c("core", "chem", "sds", "upper-level", "electives", "labs"),
        Must = I(list(bio_core, NA, NA, NA, NA, bio_lab_base)),
        `Choose 1` = I(list(NA, bio_chem_req, bio_sds_req, NA, NA, bio_lab_300)),
        `Choose 2` = I(list(NA, NA, NA, bio_upper_300, bio_elective, bio_lab_200)),
        `Choose 3` = I(list(NA, NA, NA, bio_upper_200, NA, NA))
      )

      if (track == 1) {
        # do the row name
        rownames(bio_t_df)[4] <- "(one course from each of Tracks 2-4)"
        return(bio_t_df)
      } else {
        return(bio_t_df)
      }

    }

    if (track == 5) {

      bio_t5_courses_must <- bio |>
        html_elements("li:nth-child(4) ol:nth-child(1) li:nth-child(1)") |>
        html_text2()

      bio_t5_courses_choose <- bio |>
        html_elements("li:nth-child(4) ol:nth-child(1) li+ li") |>
        html_text2()

      bio_t5_lab_must <- bio |>
        html_elements("li+ li ol:nth-child(1) .code_bubble") |>
        html_text2()

      bio_t5_lab_choose <- bio |>
        html_elements("li:nth-child(5) li+ li") |>
        html_text2() |>
        unique()
      bio_t5_lab_choose <- bio_t5_lab_choose[4:5]

      bio_edu_must <- bio |>
        html_elements("li span .sc_courseinline:nth-child(5) .code_bubble , li span .sc_courseinline:nth-child(4) .code_bubble , li span .sc_courseinline:nth-child(1) .code_bubble") |>
        html_text2()
      bio_edu_choose <- bio |>
        html_elements("li span .sc_courseinline:nth-child(3) .code_bubble , li span .sc_courseinline:nth-child(2) .code_bubble") |>
        html_text2()

      bio_nonbio <- bio |>
        html_elements("ol+ p .code_bubble") |>
        html_text2()

      # function for track 5
      bio_t5_df <- data.frame(
        Requirements = c("core", "chem", "sds", "course", "lab", "education",
                         "Outside of major requirement (for license)"),
        Must = I(list(bio_core, "", "", bio_t5_courses_must, bio_t5_lab_must, bio_edu_must,
                      bio_nonbio)),
        `Choose 1` = I(list("", bio_chem_req, bio_sds_req, bio_t5_courses_choose, bio_t5_lab_choose,
                            bio_edu_choose, ""))
      )
      return(bio_t5_df)
    }
  } else {
    stop("Input Argument must be between 1-5")
  }
}

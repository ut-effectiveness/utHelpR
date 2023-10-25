#' Fake student enrollment data
#'
#' This is a fictional data set for users to use as they explore the tools
#' available in in utHelpR.
#'
#' @format A data frame with 10,000 rows and 32 columns:
#' \describe{
#'   \item{first_name}{First name}
#'   \item{last_name}{Last name}
#'   \item{middle_name}{Middle name}
#'   \item{previous_last_name}{previous last name}
#'   \item{previous_first_name}{previous first name}
#'   \item{preferred_first_name}{preferred first name}
#'   \item{preferred_middle_name}{preferred middle name}
#'   \item{local_address_zip_code}{The student local zip code.}
#'  \item{mailing_address_zip_code}{Students mail zip code}
#'  \item{us_citizenship_code}{citizenship code}
#'  \item{first_admit_county_code}{the county the student applied from}
#'  \item{first_admit_state_code}{the state the student applied from}
#'  \item{first_admit_country_code}{the country the student applied from}
#'  \item{residential_housing_code}{the student's housing code}
#'  \item{student_id}{student id}
#'  \item{previous_student_id}{previous student id}
#'  \item{birth_date}{birth date}
#'  \item{gender_code}{gender}
#'  \item{is_hispanic_latino_ethnicity}{}
#'  \item{is_asian}{}
#'  \item{is_black}{}
#'  \item{is_american_indian_alaskan}{}
#'  \item{is_hawaiian_pacific_islander}{}
#'  \item{is_white}{}
#'  \item{is_international}{}
#'  \item{is_other_race}{}
#'  \item{primary_major_cip_code}{}
#'  \item{student_type_code}{}
#'  \item{primary_level_class_id}{}
#'  \item{primary_degree_id}{}
#'  \item{institutional_cumulative_credits_earned}{}
#' }
#' @source this script

library(tidyverse)
library(charlatan)
library(rlang)

sample_size <- 10000

source(here::here('data-raw', 'country_codes.R'))
source(here::here('data-raw', 'state_codes.R'))
source(here::here('data-raw', 'county_codes.R'))
source(here::here('data-raw', 'code_lists.R'))

fake_bool <- function(size) {

  size <- as.integer(size)

  if (is.na(size)) {
    rlang::abort("Your input couldn't be converted to an integer. Please check the input.")
  }

  sample(c(TRUE, FALSE), size, replace = TRUE)
}


credits <- as.integer(rnorm(sample_size, mean = 120, sd = 40)) +
  sample(c(rep(NA, 10), c(rep(1, 100)), c(rep(.5, 50)), c(rep(.3, 50))  ),
         sample_size, replace = TRUE)

names <-  tibble(
  delete_name = charlatan::ch_name(15000, locale = 'en_US'),
  middle_name = sample( c('Bob', 'Sue', rep(NA, 10)), 15000, replace = TRUE),
  previous_last_name = sample( c('Smith', 'Jones', rep(NA, 3)), 15000, replace = TRUE ),
  previous_first_name = sample( c('Danny', 'Erin', rep(NA, 5)), 15000, replace = TRUE ),
  preferred_first_name = sample( c('Joe', 'Deb', rep(NA, 2)), 15000, replace = TRUE ),
  preferred_middle_name = sample( c('Walter', rep(NA, 100)), 15000, replace = TRUE )
) %>%
  separate(delete_name, into = c('first_name', 'last_name')) %>%
  filter(nchar(first_name) >= 4) %>%
  filter(nchar(last_name) >= 5) %>%
  head(10000)

local_address <- tibble(
  zip_1 = stringr::str_pad(sample(1:99999, sample_size, replace = TRUE), 5, pad = '0'),
  zip_2 = stringr::str_pad(sample(1:9999, sample_size, replace = TRUE), 4, pad = '0')
) %>%
  unite(local_address_zip_code, c('zip_1', 'zip_2'), sep = '-')

mailing_address <- tibble(
  zip_1 = stringr::str_pad(sample(1:99999, sample_size, replace = TRUE), 5, pad = '0'),
  zip_2 = stringr::str_pad(sample(1:9999, sample_size, replace = TRUE), 4, pad = '0')
) %>%
  unite(mailing_address_zip_code, c('zip_1', 'zip_2'), sep = '-')

code_stuff <- tibble(
  us_citizenship_code = sample(c('1', '2', '3', '4', '5', '6', '9', NA), sample_size, replace = TRUE),
  first_admit_county_code = sample(first_admit_county_code_list, sample_size, replace = TRUE),
  first_admit_state_code = sample(first_admit_state_code_list, sample_size, replace = TRUE),
  first_admit_country_code = sample(first_admit_country_code_list, sample_size, replace = TRUE),
  residential_housing_code = sample(residential_housing_code_list, sample_size, replace = TRUE)
)


demographic <- tibble(
  student_id = stringr::str_pad(sample(1:999999, sample_size), 8, pad = '0'),
  previous_student_id = NA,
  birth_date = sample(seq(as.Date('1978/01/01'), as.Date('2022/01/01'), by="day"), sample_size),
  gender_code = sample(gender_code_list, sample_size, replace = TRUE ),
  is_hispanic_latino_ethnicity = fake_bool(sample_size),
  is_asian                     = fake_bool(sample_size),
  is_black                     = fake_bool(sample_size),
  is_american_indian_alaskan   = fake_bool(sample_size),
  is_hawaiian_pacific_islander = fake_bool(sample_size),
  is_white                     = fake_bool(sample_size),
  is_international             = fake_bool(sample_size),
  is_other_race                = fake_bool(sample_size)
)

major <- tibble(
  primary_major_cip_code = stringr::str_pad(sample(1:99999, sample_size, replace = TRUE), 5, pad = '0'),
  student_type_code = sample(student_type_code_list, sample_size, replace = TRUE),
  primary_level_class_id = sample(primary_level_class_id_list, sample_size, replace = TRUE),
  primary_degree_id = sample(primary_degree_id_list, sample_size, replace = TRUE),
  institutional_cumulative_credits_earned = sample(credits, sample_size, replace = TRUE)
)

fake_enrollment <- bind_cols(
  names,
  local_address,
  mailing_address,
  code_stuff,
  demographic,
  major
)

usethis::use_data(fake_enrollment, overwrite = TRUE)

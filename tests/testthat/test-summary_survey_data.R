test_that("summary_survey_data works", {

  random_dates <-
    sample(x = as_date(c("1982-10-28", "1986-06-22", "1989-10-29", "1993-06-06",
                         "1996-03-03", "2000-03-12", "2004-03-14", "2008-03-09",
                         "2011-11-20", "2015-12-20", "2016-06-26", "2019-04-28",
                         "2019-11-10", "2023-07-24")), size = 3,
           replace = FALSE)

  # no missing surveys
  expect_equal(summary_survey_data(type_elec = "congress",
                                     date = random_dates,
                                     short_version = FALSE,
                                     verbose = FALSE) |>
                 filter(is.na(id_survey)) |>
                 nrow(), 0)

  expect_error(summary_survey_data(year = 2023,
                                   filter_media = 700))
  expect_error(summary_survey_data(year = 2023,
                                   filter_media = 700))
  expect_error(summary_survey_data(type_elec = "congress", year = 2023,
                                   short_version = "yes"))
  expect_error(summary_survey_data(year = 2018, short_version = FALSE))
})

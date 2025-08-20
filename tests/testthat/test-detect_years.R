test_that("detect_years() works", {

  expect_null(detect_years())

  # Random dates
  random_dates <-
    sample(x = as_date(c("1982-10-28", "1986-06-22", "1989-10-29",
                         "1993-06-06", "1996-03-03", "2000-03-12",
                         "2004-03-14", "2008-03-09", "2011-11-20",
                         "2015-12-20", "2016-06-26", "2023-07-24")), size = 3,
           replace = FALSE)
  expect_s3_class(detect_years(date = random_dates), "tbl_df")
  expect_true(all(c("cod_elec", "type_elec", "year", "date") %in%
                    names(detect_years(date = random_dates))))
  expect_setequal(unique(detect_years(date = random_dates)$year), year(random_dates))
  expect_identical(unique(detect_years(date = random_dates)$type_elec), "congress")

  # Random years
  random_years <-
    sample(x = c(1982, 1986, 1989, 1993, 1996, 2000, 2004, 2008,
                 2011, 2015, 2016, 2023), size = 3,
           replace = FALSE)
  expect_s3_class(detect_years(year = random_years), "tbl_df")
  expect_true(all(c("cod_elec", "type_elec", "year", "date") %in%
                    names(detect_years(year = random_years))))
  expect_identical(unique(detect_years(year = random_years)$type_elec), "congress")

  # Ambiguity resolved thanks to the date
  res_2019 <- detect_years(year = 2019, date = "2019-04-28")
  expect_equal(nrow(res_2019), 1L)
  expect_equal(as.Date("2019-04-28"), res_2019$date)

  # Non-existent election year
  expect_error(detect_years(year = 1900))

})

test_that("detect_years() works", {

  expect_null(detect_years())

  #Particular years
  res_year <- detect_years(year = c(2015, 2016))
  expect_s3_class(res_year, "tbl_df")
  expect_true(all(c("cod_elec", "type_elec", "year", "date") %in% names(res_year)))
  expect_setequal(unique(res_year$year), c(2015, 2016))
  expect_identical(unique(res_year$type_elec), "congress")

  # Particular dates
  res_date <- detect_years(date = c("2008-03-09", "2011-11-20"))
  expect_equal(as.Date(c("2008-03-09", "2011-11-20")), res_date$date)
  expect_identical(unique(res_date$type_elec), "congress")

  # Ambiguity resolved thanks to the date
  res_2019 <- detect_years(year = 2019,
                           date = "2019-04-28")
  expect_equal(nrow(res_2019), 1L)
  expect_equal(as.Date("2019-04-28"), res_2019$date)

  # Non-existent election year
  res_bad <- detect_years(year = 1900)
  expect_s3_class(res_bad, "tbl_df")
  expect_equal(nrow(res_bad), 0L)
})

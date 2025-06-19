test_that("extract INE code", {
  expect_equal(extract_code("01-04-003-01-004-B", level = "mun"), "003")
  expect_equal(extract_code("01-04-003-01-004-B", level = "mun_district",
                            full_cod = TRUE), "01-04-003-01")
  expect_equal(extract_code("01-04-003-01-004-B", level = "prov",
                            full_cod = TRUE), "01-04")
  expect_equal(extract_code("11-25-001-01-024-B", level = "sec"), "024")
  expect_error(extract_code("1-04-003-01-004-B", level = "prov",
                            full_cod = TRUE))
  expect_error(extract_code("11-24-03-01-004-B", level = "prov",
                            full_cod = TRUE))
  expect_error(extract_code("11-24-003-01-004-BBB", level = "prov",
                            full_cod = TRUE))
  expect_error(extract_code(id_INE_poll_station, level = "prov", full_cod = false))
  expect_error(extract_code(id_INE_poll_station, level = "province", full_cod = TRUE))
  expect_error(extract_code(id_INE_poll_station, level = "muni", full_cod = "all"))
  expect_error(extract_code(id_INE_poll_station, level = "poll", full_cod = TRUE))
  expect_error(extract_code(id_INE_poll_station, level = "district", full_cod = TRUE))
})


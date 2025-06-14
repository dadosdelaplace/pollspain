test_that("import candidacies data", {
  expect_error(import_candidacies_data(type_elec = "national", year = 2019, verbose = FALSE))
  expect_error(import_candidacies_data(type_elec = "congress", year = 2018, verbose = FALSE))
  expect_error(import_candidacies_data(type_elec = "congress", date = "26-06-2016", verbose = FALSE))
  expect_error(import_candidacies_data(type_elec = "congress", year = 2023, verbose = FALSE, short_version = "yes"))
})

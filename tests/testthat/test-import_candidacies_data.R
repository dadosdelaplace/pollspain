test_that("import candidacies data", {
  expect_error(import_candidacies_data(type_elec = "national", year = 2019))
  expect_error(import_candidacies_data(type_elec = "congress", 2018))
  expect_error(import_candidacies_data(type_elec = "congress", date = "26-06-2016"))
  expect_error(import_candidacies_data(type_elec = "congress", year = 2023, short_version = "yes"))
})

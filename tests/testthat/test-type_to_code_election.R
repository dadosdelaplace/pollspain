test_that("type_to_code_election works", {
  expect_equal(type_to_code_election(type_elec = "referendum"), "01")
  expect_equal(type_to_code_election(type_elec = "congress"), "02")
  expect_equal(type_to_code_election(type_elec = "senate"), "03")
  expect_equal(type_to_code_election(type_elec = "local"), "04")
  expect_equal(type_to_code_election(type_elec = "cabildo"), "06")
  expect_equal(type_to_code_election(type_elec = "EU"), "07")
})


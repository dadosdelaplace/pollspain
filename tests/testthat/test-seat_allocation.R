test_that("seat_allocation works", {
  candidacies <- c("PP", "PSOE", "VOX")
  ballots <- c(sample(200:400, size = 1), sample(250:500, size = 1),
               sample(50:200, size = 1))
  n_seats <- sample(1:25, size = 1)
  threshold <- sample(1:10, size = 1)/100
  blank_ballots <- sample(20:250, size = 1)
  method <-
    sample(x = c("hondt"), size = 1) #"hamilton", "vinton", "webster",
                 #"sainte-lague", "hill", "huntington-hill",
                 #"dean", "adams"), size = 3)
  expect_equal(seat_allocation(candidacies, ballots,
                               blank_ballots, n_seats,
                               method, threshold,
                               short_version = TRUE) |>
                 summarise("diff" = sum(seats) - n_seats) |>
                 pull(diff), 0)

  candidacies <- c("PP", "PSOE", "VOX")
  ballots <- c(200, 450, 150)
  n_seats <- sample(1:20, size = 1)
  expect_error(seat_allocation(candidacies, ballots,
                               blank_ballots = 50, n_seats = n_seats,
                               method = "hondt", threshold = 30))
  expect_error(seat_allocation(candidacies, ballots,
                               blank_ballots = 50, n_seats = n_seats,
                               method = "hondt", threshold = -1))
  expect_error(seat_allocation(candidacies, ballots, n_seats,
                               method = c("hondt", NA), threshold = 0.04))
})


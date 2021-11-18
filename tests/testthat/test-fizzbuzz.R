test_that("multiplication works", {
  expect_equal(2 * 2, 4)
  expect_equal(1, 1+1e-7, tolerance = 1e-4)
  expect_identical(1, 1+1e-7)
})

test_that("basic fizzbuzz", {

  expect_equal(fizzbuzz(1), "1")
  expect_equal(fizzbuzz(3), "Fizz")
  expect_equal(fizzbuzz(5), "Buzz")
  expect_equal(fizzbuzz(15), "FizzBuzz")

  expect_equal(fizzbuzz(1:3), c("1", "2", "Fizz"))
})

test_that("bad fizzbuzz inputs", {
  expect_error(fizzbuzz(-1))
  expect_error(fizzbuzz(Inf))
  expect_error(fizzbuzz(1.5), regexp = "integer")

  expect_snapshot_error(fizzbuzz(1.2))
})

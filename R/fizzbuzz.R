#' @title Fizzbuzz function
#'
#' @description Function implementing the fizzbuzz function.
#'
#' The function takes a vector of numbers as inputs and replaces values that are
#' evenly divisible by 3 with "Fizz", values that are evenly divisible by 5 with "Buzz",
#' and values evenly divisible by both with "FizzBuzz".
#'
#' @param x A numeric vector
#'
#' @details The input vector is expected to be numeric and contain only finite values,
#' that are greater than or equal to 0 and must be representable as an integer. Any violations
#' of these assumptions will result in an error.
#'
#' @examples
#'
#' fizzbuzz(1:3)
#' fizzbuzz(10:1)
#'
#' \dontrun{
#' # The following will throw an error
#' fizzbuzz(-1)
#' }
#' @export

fizzbuzz = function(x) {
  stopifnot(is.numeric(x))
  stopifnot(all(is.finite(x)))
  stopifnot(all(x >= 0))
  stopifnot(all(x == as.integer(x)))

  dplyr::case_when(
    x %% 3 == 0 & x %% 5 == 0 ~ "FizzBuzz",
    x %% 3 == 0 ~ "Fizz",
    x %% 5 == 0 ~ "Buzz",
    TRUE ~ as.character(x)
  )
}

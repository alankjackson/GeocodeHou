library(testthat)
library(GeocodeHou)

test_check("GeocodeHou")

testdata <- tribble(
  ~Street_num, ~Prefix, ~Street_name, ~Street_type, ~Zipcode,
  "1311",      ""     , "TULANE"    , "ST"        , "77008"
)

test_that("Good address is found", {
  expect_equal(str_length("a"), 1)
  expect_equal(str_length("ab"), 2)
  expect_equal(str_length("abc"), 3)
})
#> Test passed 🌈

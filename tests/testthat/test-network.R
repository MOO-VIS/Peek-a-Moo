# Test the lexical_sort function from R/network.R
context("network")

test_that("`%||%` works", {
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% 2, 2)
})


test_that("adjacency_to_long works", {
  set.seed(123)
  A <- matrix(sample(0:2, 16, replace = TRUE),
                           nrow = 4, dimnames = list(letters[1:4], letters[1:4]))
  B <- tibble(to = c(rep("a",3), rep("b",3), rep("c",4), rep("d",3)),
              from = c("a","b","c","a","b","d", "a","b","c","d", "a","b","c"),
              value = as.integer(c(rep(2L,4), rep(1L,2), 2, rep(1L,2), 2L, rep(1L,3))))
  expect_equal(adjacency_to_long(A), B)
})


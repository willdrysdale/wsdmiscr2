

sequence = c(1, 1, 1, 2, 2, 3)

output = dplyr::tibble(
  lenghts = 3:1,
  values = 1:3,
  idx_start = c(1, 4, 6),
  idx_end = c(3, 5, 6)
)

test_that("success", {
  expect_identical(tidy_rle(rle(sequence)), output)
})

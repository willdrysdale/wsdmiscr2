sequence = c(1, 1, 1, 2, 2, 3)

output = data.frame(
  idx = 1:6,
  id = c(1,1,1,2,2,3)
)

test_that("success", {
  expect_equal(index_from_tidy_rle(tidy_rle(rle(sequence))), output)
})


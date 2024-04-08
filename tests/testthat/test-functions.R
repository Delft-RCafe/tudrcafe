test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("returns tibble",{
next_session <- upcoming_sessions(n = 1)
expect_s3_class(next_session, class = "tbl_df")
expect_equal(nrow(next_session), 1)
})

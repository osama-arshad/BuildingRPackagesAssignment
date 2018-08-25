test_that("test fars_summarize_years", {
  summary_2013 <- fars_summarize_years(2013)
  expect_that(round(mean(summary_2013$`2013`)), equals(2517))
})

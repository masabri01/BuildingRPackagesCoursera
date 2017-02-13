context("Summarize FARS Data")
test_that("the summarized data is a tbl_df for a year", {
    x2014 <- fars_summarize_years(2014)
    expect_that(x2014, is_a("tbl_df"))
})
test_that("the summarized data is a tbl_df for multiple years", {
    x <- fars_summarize_years(2013:2015)
    expect_that(x, is_a("tbl_df"))
})
# test_that("the number of rows summarized for the year 2013 are the same as in the original dataset", {
#     data2013 <- read.csv("accident_2013.csv.bz2")
#     x <- fars_summarize_years(2013:2015)
#     expect_that(sum(x[2]), equals(nrow(data2013)))
# })
# test_that("the number of rows summarized for the year 2014 are the same as in the original dataset", {
#     data2014 <- read.csv("accident_2014.csv.bz2")
#     x <- fars_summarize_years(2013:2015)
#     expect_that(sum(x[3]), equals(nrow(data2014)))
# })
# test_that("the number of rows summarized for the year 2015 are the same as in the original dataset", {
#     data2015 <- read.csv("accident_2015.csv.bz2")
#     x <- fars_summarize_years(2013:2015)
#     expect_that(sum(x[4]), equals(nrow(data2015)))
# })

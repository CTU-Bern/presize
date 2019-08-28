context("version increment")

return_code <- system("git diff", intern = TRUE)

# only if there are diffs this check should be made
# i.e. users of the package should not be confronted
# with this test
if (length(return_code) == 0) {
  skip("No diffs in git.")
}

grep_version <- grep("Version: \\d\\.\\d\\.\\d", return_code)

# should return length 2 if the Version number has changed
# this will only check if the developer locally runs the tests
# travis will skip this
test_that("Version number has been incremented", {
  expect_true(length(grep_version) >= 2)
})

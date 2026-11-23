context("Repeated Measures ANOVA -- unit tests")

test_that("Test that .rmCellsAsNumeric uses numeric labels and refuses text labels", {

  # with preloadData, an ordinal cell column arrives as a factor whose levels are its labels
  dataset <- data.frame(
    cellA = factor(c("1", "5", "3", NA), levels = c("1", "3", "5"), ordered = TRUE),
    cellB = c(2.5, 3.5, 4.5, 5.5)
  )
  options <- list(repeatedMeasuresCells = list("cellA", "cellB"))

  result <- jaspAnova:::.rmCellsAsNumeric(dataset, options)
  expect_equal(result$cellA, c(1, 5, 3, NA))
  expect_equal(result$cellB, dataset$cellB)

  # text labels hide the values, so the analysis asks for a scale column instead of reading the labels as missing
  dataset$cellA <- factor(c("never", "often", "sometimes", NA), levels = c("never", "sometimes", "often"), ordered = TRUE)
  expect_error(jaspAnova:::.rmCellsAsNumeric(dataset, options), "cellA has labels that are not numbers. Set its type to Scale", fixed = TRUE)
})

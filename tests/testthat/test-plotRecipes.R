test_that("posterior producers store environment-free recipes and preserve redraws", {
  withr::local_options(lifecycle_verbosity = "quiet")
  # Plot assignment normally needs the native analysis runtime. Capture that
  # boundary so this unit test checks the producer without starting an analysis.
  testthat::local_mocked_bindings(
    createJaspPlot = function(plot = NULL, ...) {
      result <- new.env(parent = emptyenv())
      result$plotObject <- plot
      result$setError <- function(...) NULL
      result
    },
    .package = "jaspAnova"
  )
  x <- seq(-3, 3, length.out = 40)
  densities <- array(0, c(40, 2, 2),
                     dimnames = list(NULL, c("group-A", "group-B"), c("x", "y")))
  densities[, , "x"] <- x
  densities[, 1, "y"] <- dnorm(x)
  densities[, 2, "y"] <- dnorm(x, .5)
  cris <- matrix(c(-1, 1, -.5, 1.5), 2, byrow = TRUE)
  set.seed(42)
  for (grouped in c(FALSE, TRUE)) {
    container <- new.env(parent = emptyenv())
    jaspAnova:::.BANOVAfillPosteriorPlotContainer(
      container, densities, cris, groupParameters = grouped
    )
    for (name in ls(container)) {
      recipe <- container[[name]]$plotObject
      expect_true(jaspGraphs::isJaspPlotRecipe(recipe))
      restored <- unserialize(serialize(recipe, NULL))
      expect_identical(restored, recipe)
      rng <- .Random.seed
      first <- ggplot2::ggplot_build(jaspGraphs::materializeJaspPlotRecipe(restored))$data
      second <- ggplot2::ggplot_build(jaspGraphs::materializeJaspPlotRecipe(restored))$data
      expect_identical(first, second)
      expect_identical(.Random.seed, rng)
    }
  }
})

test_that("summary drawing recipes preserve factor levels, intervals and edits", {
  withr::local_options(lifecycle_verbosity = "quiet")
  summary <- data.frame(
    descriptivePlotHorizontalAxis = factor(c("B", "A"), levels = c("B", "A")),
    descriptivePlotSeparateLines = factor(c("one", "one")),
    barPlotHorizontalAxis = factor(c("B", "A"), levels = c("B", "A")),
    dependent = c(3, 5), ci = c(.5, .7), ciLower = c(2.5, 4.3), ciUpper = c(3.5, 5.7)
  )
  recipes <- list(
    jaspGraphs::createJaspPlotRecipe("jaspAnova:::.BANOVAdrawDescriptives",
      list(summaryStatSubset = summary, options = list(descriptivePlotSeparateLines = "",
        descriptivePlotHorizontalAxis = "Group", covariates = character()),
        plotErrorBars = TRUE, yLabel = "Value")),
    jaspGraphs::createJaspPlotRecipe("jaspAnova:::.BANOVAdrawBar",
      list(summaryStatSubset = summary, plotErrorBars = TRUE, yBreaks = 0:6,
        yLabel = "Value", xLabel = "Group"))
  )
  for (recipe in recipes) {
    restored <- unserialize(serialize(recipe, NULL))
    expect_identical(restored, recipe)
    plot <- jaspGraphs::materializeJaspPlotRecipe(restored)
    expect_true(ggplot2::is_ggplot(plot))
    expect_identical(plot$data$dependent, c(3, 5))
    expect_identical(levels(plot$data$barPlotHorizontalAxis), c("B", "A"))
    edits <- jaspGraphs::plotEditingOptions(recipe)
    edits$xAxis$settings$title <- "Edited group"
    edits$xAxis$settings$titleType <- "character"
    edited <- unserialize(serialize(jaspGraphs::plotEditing(recipe, edits), NULL))
    expect_identical(jaspGraphs::plotEditingOptions(
      jaspGraphs::materializeJaspPlotRecipe(edited))$xAxis$settings$title, "Edited group")
  }
})

test_that("single-model Q-Q and R-squared producers store summaries and mathematical labels", {
  withr::local_options(lifecycle_verbosity = "quiet")
  testthat::local_mocked_bindings(
    createJaspPlot = function(plot = NULL, ...) {
      result <- new.env(parent = emptyenv())
      result$plotObject <- plot
      result$dependOn <- function(...) NULL
      result
    }, .package = "jaspAnova"
  )
  container <- new.env(parent = emptyenv())
  container$getError <- function() FALSE
  x <- seq(-2, 2, length.out = 30)
  model <- list(residSumStats = cbind(mean = x, `cri.2.5%` = x - .2,
    `cri.97.5%` = x + .2), rsqDens = list(x = seq(0, 1, length.out = 30),
    y = dbeta(seq(0, 1, length.out = 30), 2, 3)), rsqCri = c(.1, .8))
  jaspAnova:::.BANOVAsmiQqPlot(container, list(singleModelQqPlot = TRUE), model)
  jaspAnova:::.BANOVAsmiRsqPlot(container, list(singleModelRsqPlot = TRUE), model)
  for (name in c("QQplot", "smirsqplot")) {
    recipe <- container[[name]]$plotObject
    expect_true(jaspGraphs::isJaspPlotRecipe(recipe))
    restored <- unserialize(serialize(recipe, NULL))
    expect_identical(restored, recipe)
    plot <- jaspGraphs::materializeJaspPlotRecipe(restored)
    expect_true(ggplot2::is_ggplot(plot))
    if (name == "smirsqplot")
      expect_identical(plot$scales$get_scales("x")$name, expression(R^2))
  }
})

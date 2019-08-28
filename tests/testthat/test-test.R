
context ("testApp")

test_that("testApp works", {
  calls <- list()
  # Tracks the working directory we were in as of the last call
  wd <- NULL
  sourceStub <- function(...){
    calls[[length(calls)+1]] <<- list(...)
    wd <<- getwd()
    NULL
  }

  # Temporarily opt-in to R/ file autoloading
  orig <- getOption("shiny.autoload.r", NULL)
  options(shiny.autoload.r=TRUE)
  on.exit({options(shiny.autoload.r=orig)}, add=TRUE)

  testSpy <- rewire(testApp, sourceUTF8 = sourceStub)

  testSpy(test_path("../test-helpers/app1-standard"))

  # Should have seen three calls -- first to global then to the helpers
  expect_length(calls, 2)
  expect_match(calls[[1]][[1]], "runner1\\.R$", perl=TRUE)
  expect_match(calls[[2]][[1]], "runner2\\.R$", perl=TRUE)

  # Check environments
  # Each should be loaded into an isolated env that's a child of emptyenv
  env1 <- calls[[1]]$envir
  expect_identical(parent.env(env1), emptyenv())

  env2 <- calls[[2]]$envir
  expect_identical(parent.env(env2), emptyenv())

  expect_true(!identical(env1, env2))

  # Check working directory
  expect_equal(normalizePath(wd), normalizePath(
    file.path(test_path("../test-helpers/app1-standard"), "tests")))
})

test_that("testApp handles the absence of tests", {
  expect_error(testApp(test_path("../test-helpers/app2-nested")), "No tests directory found")
  expect_message(testApp(test_path("../test-helpers/app6-empty-tests")), "No test runners found in")
})

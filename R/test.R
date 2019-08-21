
#' Check to see if the given text is a shinytest
#' Scans for the magic string of `app <- ShinyDriver$new(` as an indicator that this is a shinytest.
#' Brought in from shinytest to avoid having to export this function.
#' @noRd
isShinyTest <- function(text){
  lines <- grepl("app\\s*<-\\s*ShinyDriver\\$new\\(", text, perl=TRUE)
  any(lines)
}

#' Runs the tests associated with this Shiny app
#'
#' Sources the `.R` files in the top-level of `tests/` much like `R CMD check`.
#' These files are typically simple runners for tests nested in other
#' directories under `tests/`.
#'
#' TODO: add in filters for specific tests, or high-level toggles for particular types of tests. Or should that just be a filter on the runners?
#' TODO: make shinytest suggestable. Right now we'd have to import it.
#'
#' @details First checks to see if the `.R`` files in the `tests/` directory are
#' all [shinytest](https://rstudio.github.io/shinytest/)s; if so, just calls out
#' to [shinytest::testApp()].
#' @export
testApp <- function(appDir="."){
  testsDir <- file.path(appDir, "tests")
  if (!dirExists(testsDir)){
    stop("No tests directory found: ", testsDir)
  }
  runners <- list.files(testsDir, pattern="\\.r$", ignore.case = TRUE)

  if (length(runners) == 0){
    message("No test runners found in ", testsDir)
    return()
  }

  # Inspect each runner to see if it appears to be a shinytest
  isST <- vapply(runners, function(r){
    text <- readLines(file.path(testsDir, runners))
    isShinyTest(text)
  }, logical(1))

  if (all(isST)){
    # just call out to shinytest
    # We don't need to message/warn here since shinytest already does it.
    if (!requireNamespace("shinytest", quietly=TRUE) ){
      stop("It appears that the .R files in ", testsDir,
           " are all shinytests, but shinytest is not installed.")
    }

    shinytest::testApp(appDir)
    return()
  }

  # Otherwise source all the runners.
  lapply(runners, sourceUTF8)
}

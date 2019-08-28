
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
#' @param appDir The base directory for the application.
#'
#' @details Historically, [shinytest](https://rstudio.github.io/shinytest/)
#'   recommended placing tests at the top-level of the `tests/` directory. In
#'   order to support that model, `testApp` first checks to see if the `.R`
#'   files in the `tests/` directory are all shinytests; if so, just calls out
#'   to [shinytest::testApp()].
#' @export
testApp <- function(appDir="."){
  testsDir <- file.path(appDir, "tests")
  if (!dirExists(testsDir)){
    stop("No tests directory found: ", testsDir)
  }
  runners <- list.files(testsDir, pattern="\\.r$", ignore.case = TRUE)

  if (length(runners) == 0){
    # TODO: should this be an error?
    message("No test runners found in ", testsDir)
    # TODO: what should we return?
    return(NULL)
  }

  # Inspect each runner to see if it appears to be a shinytest
  isST <- vapply(runners, function(r){
    text <- readLines(file.path(testsDir, r), warn = FALSE)
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

  oldwd <- getwd()
  on.exit({
    setwd(oldwd)
  }, add=TRUE)

  setwd(testsDir)

  # Otherwise source all the runners -- each in their own environment.
  lapply(runners, function(r){
    env <- new.env(parent=emptyenv())
    sourceUTF8(r, envir=env)
  })

  # TODO: trycatch the runs so we can return something more meaningful
  NULL
}

#' Increase package version in DESCRIPTION
#' @param sep character separating major.minor.patch version from build
#'   identifier
#' @importFrom utils packageVersion
#' @export
#'
increaseDevVersion <- function(sep = "-") {
  # Extract
  description <- readLines("DESCRIPTION")
  for (line in 1:length(description)) {
    if (substr(description[line], 1, 7) == "Version") {
      str <- description[line]
      break
    }
  }
  cat("Updated from", str)
  regSep      <- paste0("\\", sep)
  str.split   <- strsplit(str, regSep)[[1]]
  dev.version <- as.numeric(str.split[length(str.split)])

  # Update
  dev.version.updated <- dev.version + 1

  # Reassemble
  fixed.str   <- str.split[1:(length(str.split) - 1)]
  str.updated <- paste(c(fixed.str, dev.version.updated), collapse = sep)
  description[line] <- str.updated
  cat(" to", str.updated)

  # Output
  writeLines(description, "DESCRIPTION")
}

# .onLoad <- function(libname, pkgname){
#   packageStartupMessage("This package is under development!")
#   packageStartupMessage("Loaded version ", packageVersion("LWB17"))
# }

validate_date <- function(date) {
  can_coerce <- tryCatch(
    {
      as.Date(date)
      return(TRUE)
    },
    error = function(e) return(FALSE)
  )

  if (!isTRUE(can_coerce) || !inherits(date, "Date")) {
    abort(
      c(
        "Date validation failed.",
        "i" = "Please make sure that all of your date fields are either:",
        "i" = " 1. A date object",
        "i" = " 2. Coercible to a date, such as '2020-01-01'"
      )
    )
  }
}

#' Tell dad jokes
#'
#' Function returns a dad joke either by printing it to the console or
#'     by returning a is as a character vector
#'
#' @source https://www.fatherhood.gov/for-dads/dad-jokes
#'
#' @param n.jokes integer specifying number of jokes to return.
#'     Defaults to 1.
#' @param as.character logical indicating whether or not to return a character vector.
#'     Defaults to \code{FALSE}
#' @param random logical indicating whether or not to choose \code{n.jokes} at random.
#'     Defaults to \code{TRUE}
#'
#' @return A (printable) list of jokes (default) or a character vector (if \code{as.character = TRUE})
#' @export
#'
#' @importFrom curl curl_fetch_memory
#' @importFrom jsonlite fromJSON
#'
#' @examples \dontrun{
#' tell_joke()
#' tell_joke(as.character = TRUE)
#' }
tell_joke <- function(n.jokes = 1L, as.character = FALSE, random = TRUE) {

  # query API
  resp <- curl_fetch_memory("https://www.fatherhood.gov/jsonapi/node/dad_jokes")

  stopifnot(
    "Sorry, there occured an error when calling the dad jokes API!" = resp$status_code == 200L
    , "Sorry, couldn't load dad jokes data!" = "content" %in% names(resp)
  )

  # parse content
  parsed <- tryCatch(
    fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
    , error = function(err) err
  )

  if (inherits(parsed, "error"))
    stop("Sorry, couldn't parse dad jokes data!")

  # extract joke openeners and punch lines
  jokes <- lapply(
    lapply(parsed$data, `[[`, "attributes")
    , `[`
    , c("field_joke_opener", "field_joke_response")
  )

  # validate
  check <- vapply(lapply(jokes, names), identical, FALSE, y = c("field_joke_opener", "field_joke_response"))

  if (!any(check))
    stop("Sorry, couldn't parse dad jokes data!")

  # subset to valide elements
  jokes <- jokes[check]

  # ensure that n.jokes does not exceed number of jokes returned from API
  if (n.jokes > length(jokes)) {
    n.jokes <- length(jokes)
    wrn <- sprintf("Sorry, there are currently only %d jokes available. Cannot tell more than that.",  n.jokes)
    warning(wrn, immediate. = TRUE)
  }

  # subset to n.jokes (and shuffle, if desired)
  jokes <- if (random) {
    jokes[sample(seq_along(jokes), n.jokes)]
  } else {
    jokes[1:n.jokes]
  }

  # return character vector (if desired)
  if (as.character) return(vapply(jokes, paste, NA_character_, collapse = " -- " ))

  # otherwise, print jokes:
  jokes <- lapply(jokes, structure, class = c("joke", "list"))

  class(jokes) <- c("jokes", "list")

  jokes
}

#'@rdname tell_joke
#'@export
print.joke <- function(x, ...) {
  cat(paste0("\n\033[3m", x$field_joke_opener, "\033[23m\n"))
  cat(paste0("\033[1m", x$field_joke_response, "\033[22m\n"))
}

#'@rdname tell_joke
#'@export
print.jokes <- function(x, ...) {
  invisible(lapply(x, print.joke))
}

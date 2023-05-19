#' @title Add new column with random numbers
#' @description Adds new column with random numbers. The function is built to be
#'     able to use it in piping.
#'
#' @details To make reproducible random numbers the seed can be initialized with a
#'     specific value. The first time the seed is used, set \code{init_seed = TRUE}.
#'     Thereafter, use \code{init_seed = FALSE} if more random numbers are generated
#'     in the session to avoid overlapping random numbers.
#'
#' @param data [\code{data.frame}]\cr
#'     Data to which a column with random number should be added.
#' @param colname [\code{character(1)}]\cr
#'     The name of the new column with the random number. Defaults to "random".
#' @param seed [\code{numeric(1)}]\cr
#'     The initializing seed. Defaults to -1, see
#'     \ifelse{html}{\code{\link[base]{set.seed}}}{\code{base::set.seed}}.
#' @param init_seed [\code{logical(1)}]\cr
#'     Should the seed be initialized. Defaults to \code{FALSE}.
#'
#' @return \code{data.frame} with a new column with random numbers.
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#' @export
#' @examples
#' \dontrun{
#' # Add column with random variables
#' x <- as.data.frame(c(1:10))
#' seed <- 12345
#'
#' # Initialize with seed first time used
#' x <- make_random(x, seed = seed, init_seed = TRUE)
#'
#' # Do not initialize the seed thereafter to avoid overlapping
#' x <- make_random(x, seed = seed, init_seed = FALSE)
#'
#' # If you initialize again you get overlapping seeds
#' x <- make_random(x, seed = seed, init_seed = TRUE)
#' }
#'
make_random <- function(data, colname = "random", seed = -1, init_seed = FALSE) {

  # Argument checking
  checks <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, add = checks)
  checkmate::assert_character(colname, len = 1, min.chars = 1, any.missing = FALSE, add = checks)
  checkmate::assert_number(seed, add = checks)
  checkmate::assert_logical(init_seed, add = checks)
  checkmate::reportAssertions(checks)

  # To make it possible to control initialization of the seed
  if (init_seed == TRUE) {set.seed(seed)}

  # Generates new column with random numbers
  data[, colname] <- stats::runif(n = dim(data)[1])

  return(data)
}

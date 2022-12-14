#' @title Deprecated Functions in Package OKplan
#' @description These functions are provided for compatibility with older
#'     versions of OKplan only, and may be defunct as soon as the next release.
#'     When possible, alternative functions are mentioned. Help pages for
#'     deprecated functions are available at \code{help("<function>-deprecated")}.
#' @details \code{check_OK_selection} was deprecated 2022-12-15 to replace it
#'     with check_ok_selection with a standardised function name with lower case
#'     letters for OK. Use check_ok_selection with the additional parameters
#'     \code{purpose =} and \code{plan_aar =}.
#'
#' @param \dots (arguments)
#' @return (results)
#' @name OKplan-deprecated
#' @keywords internal
#'
#' @author Petter Hopp Petter.Hopp@@vetinst.no
#'
#' @examples
#' \dontrun{
#' check_OK_selection(...) ### -- use \code{\link{check_ok_selection}} instead.
#' }
NULL

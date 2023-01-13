% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_ok_selection.R
\name{check_ok_selection}
\alias{check_ok_selection}
\title{Checks the standard output data frame with the OK selection}
\usage{
check_ok_selection(
  input = system.file("templates", "check_ok_selection.Rmd", package = "OKplan"),
  output_file = paste0("Kontroll av okplan for ", purpose, " ", format(Sys.Date(),
    "\%Y\%m\%d"), ".html"),
  output_dir = NULL,
  data = NULL,
  purpose = NULL,
  plan_aar = as.numeric(format(Sys.Date(), "\%Y")) + 1,
  display = "browser",
  ...
)
}
\arguments{
\item{input}{The path to the rmarkdown document with the checks.
Defaults to "check_ok_selection.Rmd" in the \code{OKplan}.}

\item{output_file}{\[\code{character(1)}\]. The name of the output
file.}

\item{output_dir}{\[\code{character(1)}\]. The directory to save
the output file. Defaults to \code{NULL}.}

\item{data}{\[\code{data.frame}\].  The table with data describing
the selection for a OK programme.}

\item{purpose}{\[\code{character(1)}\]. String with descriptive
text to be used in file name and heading of the report.}

\item{plan_aar}{\[\code{numeric(1)}\]. The year for which the
selection is planned. Defaults to next year.}

\item{display}{\[\code{logical(1)} | \code{character(1)}\]. Set
"browser" for the default browser or "viewer" for the R studio
 viewer. `TRUE` equals "browser". If `FALSE`, don't display
 the results file. Defaults to "browser".}

\item{\dots}{Other arguments to be passed to `NVIbatch::output_rendered`.}
}
\value{
Generates an html-file with the results of the checks to be displayed in the browser.
}
\description{
Standard checks by performing descriptive statistics of
    variables in the standard output data frame with OK selection.
    \code{check_OK_selection} is a wrapper for \code{NVIbatch::output_rendered}.
}
\details{
Gives descriptive statistics of the OK selection. This should
    used to check if the number of selected units per category are
    in accord with the design of the surveillance programme. If any
    mistakes are found, one must correct in the script that generates
    the selection.

    The check must be performed on a data frame with standardised
    column names. This is ensured by using column names as defined for
    "okplan" in \code{data(OKplan::OK_column_standards)}.

    The default behaviour is to display the resulting html-file in the
    browser. To save the result in a permanent file, use a permanent
    directory as input to \code{output_dir = }. The resulting file
    can also be sent by email by using additional arguments, see
    \code{NVIbatch::output_rendered}.

    If checks are missing, are unnecessary or the headings are to
    cryptic, please give input to improve the rmarkdown file
    "check_ok_selection.Rmd".
}
\examples{
\dontrun{
# Checking OK selection data

purpose = "ok_virus_svin"
plan_aar = 2023

# Check
check_OK_selection(data = okplan_svin,
                   purpose = purpose,
                   plan_aar = plan_aar)
}

}
\author{
Petter Hopp Petter.Hopp@vetinst.no
}
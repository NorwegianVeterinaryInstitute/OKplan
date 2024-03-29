#' @title Data: Column standards for OK sampling plans.
#'
#' @description A data frame with the column standards for data frames
#'    and Excel sheets produced when planning the sampling schemes for
#'    the Norwegian surveillance programmes. The raw data for the column
#'    standards can be edited in the original Excel table. The code for
#'    preparing of the data frame is written in
#'    "./data-raw/generate_OK_column_standards". The
#'    \code{OK_column_standards} is used as input for
#'    \ifelse{html}{\code{\link[NVIdb]{standardize_columns}}}{\code{NVIdb::standardize_columns}}.
#'
#' @format A data frame with 14 variables:
#' \describe{
#'   \item{db}{the database, either OK_planlegging or OK_kontroll}
#'   \item{table_db}{name of source table / data frame}
#'   \item{colname_db}{name of variable in source table}
#'   \item{colname}{name of variable in working table, usually the same as in source table}
#'   \item{label_1_no}{label (column name) used when writing to Excel, one line header. Generated from label_no and spec_no. For OK-planning
#'         usually the same as label_no}
#'   \item{label_no}{label in short form}
#'   \item{spec_no}{specification of label}
#'   \item{label_1_en}{English label (column name) used when writing to Excel, one line header. Generated from label_no and spec_no. For
#'         OK-planning usually the same as label_no}
#'   \item{label_en}{English label in short form}
#'   \item{spec_en}{specification of label}
#'   \item{colwidth_Excel}{column width used in Excel tables given in Excel units}
#'   \item{colwidth_DT}{column width used in DT tables, currently not relevant for OK-planning}
#'   \item{colclasses}{column class usedto import character strings correctly}
#'   \item{colorder}{column order when saving standard data and reporting}
#' }
#' @source "colnames_translation_table.xlsx" at NVI's internal net.
#' @examples
#' standards <- data(OK_column_standards, package = "OKplan")
"OK_column_standards"

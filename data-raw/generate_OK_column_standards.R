# generate_OK_column_standards

# GENERATE COLUMN STANDARDS FOR DATA FRAMES AND EXCELSHEETS USED IN MANAGEMENT OF THE NORWEGIAN SURVEILLANCE (OK-) PROGRAMMES ----

# DESIGN ----
# Set up R-environment
# Read Excel sheet with column standars
# Select data for OK-programmes
# Transform data
# Save data to ./data/OK_column_standards.rds

library(openxlsx)
library(dplyr)
library(tidyr)
library(purrr)
library(NVIdb)
library(usethis)


# READS AND TRANSFORMS EXCEL SHEET WITH COLUMN STANDARD INFORMATION ----
OK_column_standards <- read.xlsx(xlsxFile = paste0(set_dir_NVI("ProgrammeringR"), "standardization/colnames_translation_table.xlsx")) %>%
  # Selects only information used in OK-planning
  dplyr::filter(db == "OK_planlegging") %>%
  # Generates column labels based on label and spec for no and en
  # Use exactly same transformations as for column_standards
  dplyr::rename(label_no = collabel_no, label_en = collabel_en) %>%
  dplyr::mutate(label_1_no = dplyr::case_when(is.na(spec_no) ~ label_no,
                                       spec_no %in% c("dato", "geometrisk middel 3") ~ paste(label_no, spec_no),
                                       spec_no %in% c("kg", "kjennelse", "tid") ~ label_no,
                                       spec_no %in% c("antall undersøkt") ~ paste(spec_no, label_no),
                                       TRUE ~ spec_no)) %>%
  dplyr::mutate(label_1_en = dplyr::case_when(is.na(spec_en) ~ label_en,
                                       spec_en %in% c("date") ~ paste(label_en, spec_en),
                                       spec_en %in% c("kg", "time", "determination") ~ label_en,
                                       spec_en %in% c("No. tested") ~ paste(spec_en, label_en),
                                       TRUE ~ spec_en)) %>%
  # selects, renames and orders columns
  dplyr::select(db, table_db, colname_db, colname, label_1_no, label_no, spec_no, label_1_en, label_en, spec_en,
                colwidth_Excel = colwidth_excel, colwidth_DT = colwidth_dt_tables, colclasses, colorder) %>%
  distinct()


# Generate data frame with all table names
db_tables <- as.data.frame(unique(OK_column_standards$table_db)) %>%
  rename(tables = 1) %>%
  mutate(table = strsplit(tables, split = ",")) %>%
  dplyr::mutate_if(is.list, purrr::simplify_all) %>%    # flatten each list element internally
  tidyr::unnest(cols = "table")# expand

# Generate table with each table name on one line
OK_column_standards <- OK_column_standards %>%
  dplyr::left_join(db_tables, by = c("table_db" = "tables")) %>%
  dplyr::mutate(table_db = trimws(table)) %>%
  dplyr::select(-table)

# unique_colnames <- unique(column_standards[, c("colname_db", "colname")]) %>%
#   dplyr::add_count(colname_db, name = "unique_colnames") %>%
#   dplyr::mutate(unique_colnames = dplyr::case_when(unique_colnames == 1 ~ 1,
#                                                    TRUE ~ 0))
#
# OK_column_standards <- OK_column_standards %>%
#   dplyr::left_join(unique_colnames, by = c("colname_db" = "colname_db", "colname" = "colname"))

# SAVE IN PACKAGE DATA ----
usethis::use_data(name = OK_column_standards, overwrite = TRUE, internal = FALSE)

# write.csv2(OK_column_standards,
#            file = paste0(set_dir_NVI("ProgrammeringR"), "standardization/OK_column_standards.csv"),
#            row.names = FALSE,
#            fileEncoding = "UTF-8")


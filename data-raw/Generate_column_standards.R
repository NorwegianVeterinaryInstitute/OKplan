
library(openxlsx)
library(dplyr)
library(tidyr)
library(purrr)
library(NVIdb)

column_standards <- read.xlsx(xlsxFile = paste0(set_dir_NVI("ProgrammeringR"), "standardization/colnames_translation_table.xlsx"))

column_standards <- column_standards %>%
  # mutate(colname = dplyr::coalesce(colname_ph, colname)) %>%
  rename(label_no = collabel_no, label_en = collabel_en) %>%
  mutate(label_1_no = case_when(is.na(spec_no) ~ label_no,
                                   spec_no %in% c("dato", "geometrisk middel 3") ~ paste(label_no, spec_no),
                                   spec_no %in% c("kg", "kjennelse", "tid") ~ label_no,
                                   spec_no %in% c("antall undersøkt") ~ paste(spec_no, label_no),
                                   TRUE ~ spec_no)) %>%
  mutate(label_1_en = case_when(is.na(spec_en) ~ label_en,
                                   spec_en %in% c("date") ~ paste(label_en, spec_en),
                                   spec_en %in% c("kg", "time", "determination") ~ label_en,
                                   spec_en %in% c("No. tested") ~ paste(spec_en, label_en),
                                   TRUE ~ spec_en)) %>%
  # mutate(label_en = coalesce(label_en, label_no)) %>%
  dplyr::select(db, table_db, colname_db, colname, label_1_no, label_no, spec_no, label_1_en, label_en, spec_en,
                colwidth_Excel = colwidth_excel, colwidth_DT = colwidth_dt_tables, colclasses, colorder) %>%
  distinct()



db_tables <- as.data.frame(unique(column_standards$table_db))
colnames(db_tables) <- "tables"
db_tables <- db_tables %>%
  mutate(table = strsplit(tables, split = ",")) %>%
  mutate_if(is.list, simplify_all) %>%    # flatten each list element internally
  unnest(cols = "table")    # expand

column_standards <- column_standards %>%
  left_join(db_tables, by = c("table_db" = "tables")) %>%
  mutate(table_db = trimws(table)) %>%
  select(-table)

unique_colnames <- unique(column_standards[, c("colname_db", "colname")]) %>%
  add_count(colname_db, name = "unique_colnames") %>%
  mutate(unique_colnames = case_when(unique_colnames == 1 ~ 1,
                                     TRUE ~ 0))

column_standards <- column_standards %>%
  left_join(unique_colnames, by = c("colname_db" = "colname_db", "colname" = "colname"))

OK_column_standards <- subset(column_standards, column_standards$db == "OK_planlegging")

write.csv2(OK_column_standards,
           file = "//vetinst.no/dfs-felles/StasjonK/FAG/Tverrfaglig/Programmering/R/standardization/OK_column_standards.csv",
           row.names = FALSE,
           fileEncoding = "UTF-8")


column_standards <- subset(column_standards, column_standards$db != "OK_planlegging")

write.csv2(column_standards,
           file = "//vetinst.no/dfs-felles/StasjonK/FAG/Tverrfaglig/Programmering/R/standardization/column_standards.csv",
           row.names = FALSE,
           fileEncoding = "UTF-8")


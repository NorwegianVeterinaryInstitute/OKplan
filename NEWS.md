# OKplan 0.8.0 - (2024-12-19)

## New features:

- created `save_okplan` to save the selection information in a standard format.

- `write_ok_selection_list` now accepts `data.frame`, `list` and csv-file as input to the argument `column_standards`.


## Other changes:

- `OK_column_standards` is updated with column standards used in 2025, i.e. including orgnr in selection lists and generating  "ok_ai_art_fjorfe".

- created pdf reference manual "OKplan.pdf" as a vignette.


# OKplan 0.7.1 - (2024-11-12)

## Bug fixes

- `append_sum_line` now doesn't give an error when trying to sum first column and put a pretext in the column to the left.


## Other changes

- Updated internal dependencies. It is now dependent on R v4.1.0 and NVIpjsr.


# OKplan 0.7.0 - (2024-01-05)

## New features

- Created `get_holiday` to get the "non-workdays" or "workdays" within one year.


## Bug fixes

- Fixed several bugs in `get_tested_herds`. 


# OKplan 0.6.1 - (2023-10-09)

## Bug fixes

- The argument `year` in `get_tested_herds` now accepts more than one year as input. 

- `write_ok_selection_list` now accepts the argument `column` which will be passed to `append_sum_line`.


## Other changes

- Updated help section in README


# OKplan 0.6.0 - (2023-05-30)

## New features

- Created `get_tested_herds` to find herds tested in last year(s) surveillance programmes.

- `write_ok_selection_list` now accepts a list with column formatting information as input to `column_standards`.


## Other changes

- Updated translation table for column names to sampling plans.

- Improved function help by including links and standardising argument description.


# OKplan 0.5.0 - (2022-12-15)

## New features

- `write_ok_selection_list` now includes the argument `add_worksheet` that makes it possible to add a new worksheet to an existing workbook.

- `adjust_samples_to_budget` includes the argument `group` so that sample size can be adjusted per group.

- `check_ok_selection` now calls an R markdown file, generates a html-report with descriptive statistics and displays the report in the browser. The function is renamed (all lower case letters) and includes several new arguments. `check_OK_selection` is deprecated.

## Other changes

- README updated with improved installation guide.


## BREAKING CHANGES

  - Renamed to `check_ok_selection` from `check_OK_selection`. `check_OK_selection` is deprecated.


# OKplan 0.4.2 - (2022-01-20)

## Bug fixes

- Fixed problems with package dependencies during installation.


# OKplan 0.4.1 - (2021-12-17)

## Bug fixes

- `adjust_samples_to_budget` generates the new column 'adjusted_sample' also when initial sample is correct


## Other changes

- Included package level documentation.


# OKplan 0.4.0 - (2021-12-09)

## New features

- `style_sum_line` styles the row with sum of samples.

- `write_ok_selection_list` writes the selection list based on selected data from okplan file and uses `NVIdb::standardize_columns` to select, format and style columns.


## Other changes

- extended `OK_column_standards` with tables for samples taken at abattoirs and the surveillance in pigs.


# OKplan 0.3.0 - (2021-11-29)

## New features

- `adjust_sample_number` adjusts the sample numbers per unit up or down to reach the total sample size in accord with the budget.

- `append_sum_line` appends a line with the sum of samples to the table in Excel.


## Bug fixes

- Included reference to `NVIdb` at GitHub to correct potential problems when installing the package.


## Other changes

  - Updated licence to BSD_3_clause and the copyright holder to Norwegian Veterinary Institute.

  - Updated documentation and help.


## BREAKING CHANGES

  - Renamed to `append_date_generated_line` from `include_generated_date`.


# OKplan 0.2.0 - (2021-01-29)

## New features

  - Included a preliminary version of control_OK_selection for control of standard files with OK selection data.


# OKplan 0.1.1 - (2021-01-01)

## Other changes

  - included argument `checking` based on checkmate


# OKplan 0.1.0 - (2020-12-30)

## First release

  - `include_generated_date` adds two rows in a data frame and includes text with generated date in the first column

  - `make_random` adds a column with random numbers

  - `OK_column_standards` keeps column standards for OK data frames for input to `NVIdb::standardize_columns`

# Sort data frames
# benefits from sort being S3-function

sort.data.frame <- function(x, decreasing = FALSE, by = 1, ...) {
  f <- function(...) order(..., decreasing = decreasing)
  i <- do.call(f, x[by])
  x[i, , drop = FALSE]
}

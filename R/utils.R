add_per_group <- function(data, values, group, new_column, FUN, keep_order = FALSE, ...) {
  df_agg <- aggregate(x = data[, values],
                      by = data[, group],
                      FUN = FUN, ...)

  colnames(df_agg)[c((length(group) + 1):(length(group) + length(values)))] <- new_column

  if (keep_order) {data$agg_original_sort_order <- seq_len(nrow(data))}

  data <- merge(data, df_agg, by = group, all.x = TRUE)

  # Sorts data in original order and removes sort key
  if (keep_order) {
    data <- data[order(data$agg_original_sort_order), ]
    data$agg_original_sort_order <- NULL
  }

  return(data)
}

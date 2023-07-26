#' features_to_long
#' 
#' Convenience wrapper function for \link[tidyr]{pivot_longer} to to pivot a data.frame with one column per feature into its long form.
#' 
#' @param df data.frame
#' @param features vector containing column names storing features, 
#' @importFrom tidyr pivot_longer
#' @export
features_to_long <- function(df, features) {
  if (!all(sapply(df[, features], class) == "logical")) {
    stop("All features displayed in pedigree point must be logical")
  }
  as.data.frame(
    pivot_longer(
      df,
      cols = all_of(features),
      names_to = "feature.name",
      values_to = "feature.value",
      values_drop_na = F
    )
  )
}


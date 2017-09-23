#' @export
id_separate <- function(d, remove = TRUE) {
  d %>%
    separate(id, c("year", "quarter", "ship", "gear", "haulno"),
             remove = remove,
             convert = TRUE)
}

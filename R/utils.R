
#' Squash all text provided with a common separator
#' @param ... a collection of text objects, which can
#' exist as a mixture of lists, vectors, or named
#' arguments.
#' @param .separator the separator used between
#' each of the text components.
squash_all_text <- function(..., .separator = "\n") {

  list(...) %>%
    rlang::squash_chr() %>%
    paste(collapse = .separator) %>%
    trimws()
}

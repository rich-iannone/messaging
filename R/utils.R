#' Squash all text provided with a common separator
#' @param ... a collection of text objects, which can
#' exist as a mixture of lists, vectors, or named
#' arguments.
#' @param .separator the separator used between
#' each of the text components.
#' @noRd
squash_all_text <- function(..., .separator = "\n") {

  list(...) %>%
    rlang::squash_chr() %>%
    paste(collapse = .separator) %>%
    trimws()
}

#' Filter the input component list to just those
#' components that are named and numeric
#' @param input_components a list of objects named
#' and unnamed, that are single-length vectors.
#' @importFrom rlang is_named
#' @noRd
get_named_numeric_input_components <- function(input_components) {

  # Get a list of named input components
  for (i in seq(input_components)) {

    if (i == 1) {
      named_numeric_input_indices <-
        vector(length = length(input_components))
    }

    named_numeric_input_indices[i] <-
      ifelse(rlang::is_named(input_components[i]) &&
               is.numeric(input_components[[i]]), TRUE, FALSE)
  }

  input_components[named_numeric_input_indices]
}

#' Filter the input component list to just those
#' components that are named and nonnumeric
#' @param input_components a list of objects named
#' and unnamed, that are single-length vectors.
#' @importFrom rlang is_named
#' @noRd
get_named_nonnumeric_input_components <- function(input_components) {

  # Get a list of named input components
  for (i in seq(input_components)) {

    if (i == 1) {
      named_nonnumeric_input_indices <-
        vector(length = length(input_components))
    }

    named_nonnumeric_input_indices[i] <-
      ifelse(rlang::is_named(input_components[i]) &&
               !is.numeric(input_components[[i]]), TRUE, FALSE)
  }

  input_components[named_nonnumeric_input_indices]
}

#' Filter the input component list to just those
#' components that not named
#' @param input_components a list of objects named
#' and unnamed, that are single-length vectors.
#' @importFrom rlang is_named
#' @noRd
get_unnamed_input_components <- function(input_components) {

  # Get a list of named input components
  for (i in seq(input_components)) {

    if (i == 1) {
      unnamed_input_indices <-
        vector(length = length(input_components))
    }

    unnamed_input_indices[i] <-
      ifelse(!rlang::is_named(input_components[i]), TRUE, FALSE)
  }

  input_components[unnamed_input_indices]
}

#' Process the format string, which is a template for
#' the output messages
#' @param .format the format string which may or may
#' not be supplied by the user.
#' @noRd
process_format_str <- function(.format = .format) {

  if (!is.null(.format) && is.character(.format)) {

    format_str <- .format[1]

  } else if (is.null(.format)) {

    format_str <- "{.f_name}: {text}"
  }

  format_str
}

#' Process the function name to be used in the
#' output messages
#' @param .f_name the name of the calling function as
#' provided by the user.
#' @param calling_fcn the name of the calling function
#' as determined by deparsing the function call stack.
#' @importFrom stringr str_replace_all
#' @noRd
process_function_name <- function(.f_name = .f_name,
                                  calling_fcn = calling_fcn) {

  if (!is.null(.f_name) && is.logical(.f_name) && isTRUE(.f_name)) {

    .f_name <-
      stringr::str_replace_all(
        calling_fcn,
        pattern = "([a-z0-9_]*)(.*)",
        replacement = "\\1")

    .f_name <- paste0("`", .f_name, "()`")

  } else if (is.null(.f_name) ||
             !is.null(.f_name) && is.logical(.f_name) && !isTRUE(.f_name)) {

    .f_name <- ""

  } else if (!is.null(.f_name) && is.character(.f_name)) {

    .f_name <- paste0("`", .f_name[1], "()`")
  }

  .f_name
}

#' Reprocess the grammar in the output messages
#' @param format_str a `format_str` object that is
#' to undergo processing.
#' @param named_numeric_input_components a list of
#' named, numeric input components.
#' @param named_nonnumeric_input_components a list of
#' named, nonnumeric input components.
#' @importFrom stringr str_detect str_extract_all str_replace fixed
#' @importFrom dplyr mutate tibble case_when
#' @noRd
reprocess_grammar <- function(format_str,
                              named_numeric_input_components,
                              named_nonnumeric_input_components) {

  # Create binding for a global variable
  plural <- NULL

  # Get a vector of names for all named, numeric inputs
  numeric_refs <- names(named_numeric_input_components)

  # Get a vector of names for all named, nonnumeric inputs
  nonnumeric_refs <- names(named_nonnumeric_input_components)

  if (format_str %>% stringr::str_detect(pattern = "\\([a-zA-Z]+\\)")) {

    sing_plu_tbl <-
      format_str %>%
      stringr::str_extract_all(pattern = "\\([a-zA-Z/]+\\)") %>%
      unlist()

    sing_plu_tbl <-
      dplyr::tibble(alternates = sing_plu_tbl) %>%
      dplyr::mutate(singular = case_when(
        stringr::str_detect(
          string = alternates,
          pattern = "/") ~ stringr::str_replace(
            string = alternates,
            pattern = "\\(([a-zA-Z0-9_]*?)/.*",
            replacement = "\\1"),
        !stringr::str_detect(
          string = alternates,
          pattern = "/") ~ "")) %>%
      dplyr::mutate(plural = case_when(
        stringr::str_detect(
          string = alternates,
          pattern = "/") ~ stringr::str_replace(
            string = alternates,
            pattern = "\\(.*/([a-zA-Z0-9_]*?)\\)",
            replacement = "\\1"),
        !stringr::str_detect(
          string = alternates,
          pattern = "/") ~ stringr::str_replace(
            string = alternates,
            pattern = "\\(([a-zA-Z0-9_]*?)\\)",
            replacement = "\\1")))

    # Defer to the plural form if there are no
    # numerical references available
    if (length(numeric_refs) == 0) {

      sing_plu_tbl <-
        sing_plu_tbl %>%
        dplyr::mutate(ref = NA_character_) %>%
        dplyr::mutate(form = plural)
    }

    # if there is only one numerical reference
    # available, apply that to all alternate forms
    if (length(numeric_refs) == 1) {

      sing_plu_tbl <-
        sing_plu_tbl %>%
        dplyr::mutate(ref = numeric_refs) %>%
        dplyr::mutate(value = named_numeric_input_components[[numeric_refs]]) %>%
        dplyr::mutate(form = case_when(
          value == 1 ~ singular,
          value != 1 ~ plural))
    }

    # For each of the alternate forms, modify
    # `format_str` to use the preferred forms
    for (i in 1:nrow(sing_plu_tbl)) {

      format_str <-
        stringr::str_replace_all(
          string = format_str,
          pattern = stringr::fixed(sing_plu_tbl[i, ]$alternates),
          replacement = sing_plu_tbl[i, ]$form)
    }

    for (i in 1:nrow(sing_plu_tbl)) {

      format_str <-
        stringr::str_replace_all(
          string = format_str,
          pattern = stringr::fixed(sing_plu_tbl[i, ]$alternates),
          replacement = sing_plu_tbl[i, ]$form)
    }
  }

  # Insert named numerical values into `format_str`
  if (length(numeric_refs) > 0) {

    for (i in seq(numeric_refs)) {

      numeric_ref_name <- numeric_refs[i]

      pattern <- paste0("\\{", numeric_refs[i], "\\}")

      replacement <-
        named_numeric_input_components[[i]] %>% as.character()

      format_str <-
        format_str %>%
        stringr::str_replace_all(
          pattern = pattern,
          replacement = replacement)
    }
  }

  # Insert named textual values into `format_str`
  if (length(nonnumeric_refs) > 0) {

    for (i in 1:length(nonnumeric_refs)) {

      nonnumeric_ref_name <- nonnumeric_refs[i]

      pattern <- paste0("\\{", nonnumeric_refs[i], "\\}")

      replacement <-
        named_nonnumeric_input_components[[i]] %>% as.character()

      format_str <-
        format_str %>%
        stringr::str_replace_all(
          pattern = pattern,
          replacement = replacement)
    }
  }

  format_str
}

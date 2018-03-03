#' Provide a message with a consistent format
#'
#' Create a message using a message body that
#' can be formed from multiple text objects.
#' If a single text object is provided then the
#' message will appear on a single line. If
#' multiple text fragments are provided then
#' they will be separated by newlines. A custom
#' format could be provided by providing a
#' messaging template that uses the notation
#' scheme of the \code{glue} package.
#' @param ... a collection of string expressions
#' and named arguments for string interpolation.
#' @param .format a format template for a
#' message. If not provided, the default template
#' of \code{"`{.f_name}()` INFO: {text}"} will
#' be used.
#' @param .f_name the name of the function that
#' relates to the messsage.
#' @importFrom glue glue
#' @importFrom dplyr tibble mutate
#' @importFrom stringr str_replace_all str_replace str_detect
#' @importFrom rlang is_named
#' @export
emit_message <- function(...,
                         .format = NULL,
                         .f_name = TRUE) {

  # Collect the list of input components
  input_components <- list(...)

  # Get a list of named input components
  for (i in seq(input_components)) {

    if (i == 1) {
      named_numeric_input_indices <-
        vector(length = length(input_components))

      named_nonnumeric_input_indices <-
        vector(length = length(input_components))

      unnamed_input_indices <-
        vector(length = length(input_components))
    }

    if (rlang::is_named(input_components[i]) &&
        is.numeric(input_components[[i]])) {

      named_numeric_input_indices[i] <- TRUE
    } else {
      named_numeric_input_indices[i] <- FALSE
    }

    if (rlang::is_named(input_components[i]) &&
        !is.numeric(input_components[[i]])) {

      named_nonnumeric_input_indices[i] <- TRUE
    } else {
      named_nonnumeric_input_indices[i] <- FALSE
    }

    unnamed_input_indices[i] <- !rlang::is_named(input_components[i])
  }

  named_numeric_input_components <-
    input_components[named_numeric_input_indices]

  named_nonnumeric_input_components <-
    input_components[named_nonnumeric_input_indices]

  unnamed_input_components <-
    input_components[unnamed_input_indices]

  numeric_refs <- names(named_numeric_input_components)

  nonnumeric_refs <- names(named_nonnumeric_input_components)

  # Get all text into a single-length character object
  .text <- squash_all_text(unnamed_input_components)

  # If a custom `.format` provided, then use that as
  # the `format_str` object; otherwise, use the default
  # format string
  if (!is.null(.format) && is.character(.format)) {

    format_str <- .format[1]

  } else if (is.null(.format)) {

    format_str <- "{.f_name}INFO: {.text}"
  }

  # If `.f_name` is TRUE then obtain the function name
  # that is the caller of this function
  if (!is.null(.f_name) && is.logical(.f_name) && isTRUE(.f_name)) {

    calling_fcn <- deparse(sys.call(-1))

    .f_name <-
      stringr::str_replace_all(
        calling_fcn,
        pattern = "([a-z0-9_]*)(.*)",
        replacement = "\\1") %>%
      paste0("`", .) %>%
      paste0("()` ")

  } else if (is.null(.f_name) ||
             !is.null(.f_name) && is.logical(.f_name) && !isTRUE(.f_name)) {

    .f_name <- ""

  } else if (!is.null(.f_name) && is.character(.f_name)) {

    .f_name <- .f_name[1] %>% paste0("`", .) %>% paste0("()` ")
  }

  # Incorporate the message text into the format string
  format_str <-
    glue::glue(format_str) %>%
    as.character()

  # If there is syntax for singular and plural noun forms,
  # modify the `format_str` object to finalize the wording
  if (format_str %>% stringr::str_detect(pattern = "\\([a-zA-Z]+\\)")) {

    sing_plu_tbl <-
      format_str %>%
      stringr::str_extract_all(pattern = "\\([a-zA-Z/]+\\)") %>%
      unlist() %>%
      dplyr::tibble(alternates = .) %>%
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
        mutate(ref = NA_character_) %>%
        mutate(form = plural)
    }

    # if there is only one numerical reference
    # available, apply that to all alternate forms
    if (length(numeric_refs) == 1) {

      sing_plu_tbl <-
        sing_plu_tbl %>%
        mutate(ref = numeric_refs) %>%
        mutate(value = named_numeric_input_components[[numeric_refs]]) %>%
        mutate(form = case_when(
          value == 1 ~ singular,
          value != 1 ~ plural))
    }

    # For each of the alternate forms, modify
    # `format_str` to use the preferred forms
    for (i in 1:nrow(sing_plu_tbl)) {

      format_str <-
        stringr::str_replace_all(
          string = format_str,
          pattern = fixed(sing_plu_tbl[i, ]$alternates),
          replacement = sing_plu_tbl[i, ]$form)
    }


    for (i in 1:nrow(sing_plu_tbl)) {

      format_str <-
        stringr::str_replace_all(
          string = format_str,
          pattern = fixed(sing_plu_tbl[i, ]$alternates),
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

  # Issue the message
  format_str %>%
    as.character() %>%
    message()
}

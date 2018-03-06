#' Provide a message with a consistent format
#'
#' Create a message using a message body that
#' can be formed from multiple text objects.
#' If a single text object is provided then the
#' message will appear on a single line. If
#' multiple text fragments are provided then
#' they will be separated by newlines (in the
#' order provided). Custom formatting is
#' possible by providing a messaging template
#' that uses the string expression scheme used
#' in the \code{glue} package.
#' @param ... a collection of string expressions
#' and named arguments for string interpolation.
#' @param .format a format template for a
#' message. If not provided, the default template
#' of \code{"{.f_name}: {text}"} will be used.
#' @param .f_name the name of the function that
#' relates to the message.
#' @param .issue a logical value that indicates
#' whether the message should be issued at all.
#' This is to control for the verbosity of
#' messages under certain circumstances.
#' @importFrom glue glue
#' @importFrom stringr str_replace_all
#' @export
emit_message <- function(...,
                         .format = NULL,
                         .f_name = TRUE,
                         .issue = TRUE) {

  # If `.issue` evaluates to TRUE then
  # follow through to formatting and emitting
  # a message
  if (.issue) {

    # Collect the list of input components
    input_components <- list(...)

    # Get a list of named, numeric input components
    named_numeric_input_components <-
      get_named_numeric_input_components(
        input_components = input_components)

    # Get a list of named, nonnumeric input components
    named_nonnumeric_input_components <-
      get_named_nonnumeric_input_components(
        input_components = input_components)

    # Get a list of unnamed input components
    unnamed_input_components <-
      get_unnamed_input_components(
        input_components = input_components)

    # Get a vector of names for all named, numeric inputs
    numeric_refs <- names(named_numeric_input_components)

    # Get a vector of names for all named, nonnumeric inputs
    nonnumeric_refs <- names(named_nonnumeric_input_components)

    # Get all text into a single-length character object
    text <- squash_all_text(unnamed_input_components)

    # If a custom `.format` provided, then use that as
    # the `format_str` object; otherwise, use the default
    # format string
    if (!is.null(.format) && is.character(.format)) {

      format_str <- .format[1]

    } else if (is.null(.format)) {

      format_str <- "{.f_name}: {text}"
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
        paste0("()`")

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
    format_str <-
      format_str %>%
      reprocess_grammar(
        named_numeric_input_components = named_numeric_input_components,
        named_nonnumeric_input_components = named_nonnumeric_input_components)

    # Issue the message
    format_str %>%
      as.character() %>%
      message()
  }
}

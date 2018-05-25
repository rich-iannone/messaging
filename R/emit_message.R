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
#' Bare strings will be pasted together and
#' separated by newlines. Named arguments are
#' used to provide values for string interpolation
#' in the message.
#' @param .format a format template for a
#' message. If not provided, the default template
#' of \code{"{.f_name}: {text}"} will be used.
#' @param .f_name the name of the function that
#' relates to the message. If not provided, the
#' function name will be obtained from the
#' function call stack.
#' @param .issue a logical value that indicates
#' whether the message should be issued at all.
#' This is to control for the verbosity of
#' messages under certain circumstances.
#' @examples
#' # Write a function that yields a message with
#' # the requested number of info lines
#' yield_a_message <- function(msgs) {
#'
#'   if (msgs > 3) msgs <- 3
#'
#'   # Create some strings can serve as additional
#'   # info for the message
#'   message_components <-
#'     c("* message info 1",
#'       "* message info 2",
#'       "* message info 3")
#'
#'   # Generate and emit a formatted message
#'   emit_message(
#'     "There (is/are) {number} thing(s) to note",
#'     message_components[1:msgs],
#'     number = msgs,
#'     .format = "{.f_name} info: {text}")
#' }
#'
#' # When that function is called, a formatted
#' # message will appear; here are some examples:
#' yield_a_message(msgs = 3)
#'
#' yield_a_message(msgs = 2)
#'
#' yield_a_message(msgs = 1)
#' @importFrom glue glue
#' @export
emit_message <- function(...,
                         .format = NULL,
                         .f_name = TRUE,
                         .issue = TRUE) {

  # Get the calling function
  calling_fcn <- deparse(sys.call(-1))

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

    # Process the format string (`format_str`)
    format_str <- process_format_str(.format = .format)

    # Process the function name to be used in `format_str`
    .f_name <-
      process_function_name(
        .f_name = .f_name,
        calling_fcn = calling_fcn)

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

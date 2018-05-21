
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/rich-iannone/messaging.svg?branch=master)](https://travis-ci.org/rich-iannone/messaging)
[![Coverage
status](https://codecov.io/gh/rich-iannone/messaging/branch/master/graph/badge.svg)](https://codecov.io/github/rich-iannone/messaging?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/messaging)](https://cran.r-project.org/package=messaging)

# The **messaging** Package

The goal of **messaging** is to provides a toolset for creating and
issuing nicely-formatted text within R diagnostic messages and those
messages given during warnings and errors. The formatting of the
messages can be customized using templating features. Issues with
singular and plural forms can be handled through specialized syntax.

## Example for Emitting Messages

For for the purpose of demonstrating how **messaging** can help you
format and issue messages, we can create a function
(`yield_a_message()`) that uses the `emit_message()` function for the
sole purpose of displaying a message.

``` r
library(messaging)

yield_a_message <- function() {
  
  # This function's only aim is to produce
  # a message; throughout the function body, we
  # can generate objects that can be assembled
  # into a customizable message with `emit_message()`
  
  # Create some strings can serve as additional
  # info for the message 
  important_message_parts <- 
    c("* message info part 1",
      "* message info part 2",
      "* message info part 3")
  
  # Get a numeric value to use in the message
  n_info_lines <- length(important_message_parts)
  
  # Generate and emit a formatted message
  emit_message(
    "There (is/are) {number} thing(s) to note", # on line 1; singular/plural syntax
    important_message_parts, # 3 additional lines (all lines are separated with `/n`)
    number = n_info_lines, # a named argument; `n_info_lines` inserted into {number}
    .format = "{.f_name} info: {text}") # an optional template
}
```

Within the main function body, there’s the creation of character
(`important_message_parts`) and numeric (`number_of_info_lines`) values
that will help us compose our message. This combination of
function-scope objects will work alongside templated expression strings
in the `emit_message()` function. Multiple expression strings can be
provided to `emit_message()` to serve as components of the message
(newlines will separate each of the expressions provided).

Using curly braces, we can enable text interpolation (e.g., `The number
is {number} and this is considered {level}`) where named arguments
(e.g., `number = 3`, `level = "high"`, etc.) provide the values for the
message components.

If there is interpolation of numerical values, there is also the option
to provide singular and plural forms for words associated with the the
values. Simply enclose the singular and plural alternatives in
parentheses and the function will finalize the statement based on the
associated numerical values. Some syntax examples are: `(is/are)`,
`analys(is/es)`, `result(s)`.

Upon calling `yield_a_message()` we see that a message does appear and
that the text on the first line has been processed such that it’s
grammatically correct.

``` r
# Call the `yield_a_message()` function
yield_a_message()
#> `yield_a_message()` info: There are 3 things to note
#> * message info part 1
#> * message info part 2
#> * message info part 3
```

We can also emit warnings and errors by using the `emit_warning()` and
`emit_error()` functions within those functions of our own design.
Here’s a function that might provide a warning (if the sum of x and y
is greater than 100):

``` r
might_warn_you <- function(x, y) {
  
  sum_xy <- x + y

  if (sum_xy > 100) {
    emit_warning(
      "This value ({sum_xy}) is greater than 100.",
      sum_xy = sum_xy)
  }
  
  return(sum_xy)
}
```

``` r
might_warn_you(40, 50)
#> [1] 90
```

``` r
might_warn_you(60, 50)
#> Warning: `might_warn_you()`: This value (110) is greater than 100.
#> [1] 110
```

We can exit a function early with a message using the `emit_error()`
function:

``` r
will_stop_if_not_careful <- function(x, y) {
  
  sum_xy <- x + y

  if (sum_xy > 100) {
    emit_error(
      "This value ({sum_xy}) is greater than 100. That is bad.",
      sum_xy = sum_xy)
  }
  
  return(sum_xy)
}
```

``` r
will_stop_if_not_careful(60, 50)
```

Here is a reproduction of the error
    message:

    Error: `will_stop_if_not_careful()`: This value (110) is greater than 100. That is bad.

## Installation of the package

**messaging** is used in an R environment. If you don’t have an R
installation, it can be obtained from the [**Comprehensive R Archive
Network (CRAN)**](https://cran.r-project.org/).

You can install the development version of **messaging** from **GitHub**
using the **devtools** package.

``` r
devtools::install_github("rich-iannone/messaging")
```

If you encounter a bug, have usage questions, or want to share ideas to
make this package better, feel free to file an
[issue](https://github.com/rich-iannone/messaging/issues).

## Code of Conduct

[Contributor Code of
Conduct](https://github.com/rich-iannone/messaging/blob/master/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.

## License

MIT © Richard Iannone

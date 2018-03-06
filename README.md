
<!-- README.md is generated from README.Rmd. Please edit that file -->
The **messaging** Package
=========================

The goal of **messaging** is to provides a toolset for creating and issuing nicely-formatted text within R diagnostic messages and those messages given during warnings and errors. The formatting of the messages can be customized using templating features. Issues with singular and plural forms can be handled through specialized syntax.

Example for Emitting Messages
-----------------------------

Below is a function that uses the `emit_message()` function to just display a message. Within the main function body, we can create/collect character and numeric values and compose our message with a combination of these function-scope objects and templating strings in the `emit_message()` function. Unnamed argument values are components of the message, and several of these can be included (newlines will separate them). Named arguments (e.g., `number = 3`, `level = "high"`, etc.) provide values (numerical or textual) for the message components. Using curly braces, we can provide placeholders for interpolation of those named values (e.g., `The number is {number} and this is considered {level}`).

If there is interpolation of numerical values, there is also the option to provide singular and plural forms of words in the relevant statements. Simply enclose the singular and plural alternatives in parentheses and the function will finalize the statement based on the associated numerical value. Some syntax examples are: `(is/are)`, `analys(is/es)`, `result(s)`.

Here is the `yield_a_message()` function.

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
  number_of_info_lines <- length(important_message_parts)
  
  # Generate and emit a formatted message
  emit_message(
    "There (is/are) {number} thing(s) to note", # on line 1; singular/plural syntax
    important_message_parts, # 2 additional lines (all lines are separated with `/n`)
    number = number_of_info_lines, # named argument; `number` inserted above
    .format = "{.f_name} info: {text}") # optional template
}
```

Upon calling `yield_a_message()` we see that a message does appear and that the text on the first line has been processed such that it's grammatically correct.

``` r
# Call the `yield_a_message()` function
yield_a_message()
#> `yield_a_message()` info: There are 3 things to note
#> * message info part 1
#> * message info part 2
#> * message info part 3
```

### Installation of the package

**messaging** is used in an R environment. If you don't have an R installation, it can be obtained from the [**Comprehensive R Archive Network (CRAN)**](https://cran.r-project.org/).

You can install the development version of **messaging** from **GitHub** using the **devtools** package.

``` r
devtools::install_github("rich-iannone/messaging")
```

If you encounter a bug, have usage questions, or want to share ideas to make this package better, feel free to file an [issue](https://github.com/rich-iannone/messaging/issues).

Code of Conduct
---------------

[Contributor Code of Conduct](https://github.com/rich-iannone/messaging/blob/master/CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

License
-------

MIT © Richard Iannone

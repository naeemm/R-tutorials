#' ---
#' title: "The S3 Object System for R"
#' author: "Michael Hahsler"
#' output:
#'  html_document:
#'    toc: true
#' ---

#' ![CC](https://i.creativecommons.org/l/by/4.0/88x31.png)
#' This work is licensed under the
#' [Creative Commons Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/). For questions please contact
#' [Michael Hahsler](http://michael.hahsler.net).
#'

#'
#' S3 does not use formal class definitions. Only a class attribute is set.

o <- sample(LETTERS[c(1:4,6)], 10, replace = TRUE)
class(o) <- "grades"

o
str(o)

#' ## Generic functions and methods
#' S3 uses a dispatching mechanism for **generic functions**
#' (see, e.g., `? print`) and **methods**. Methods can be easily implemented
#' for new classes. _Note:_ the signature has to match the generic function
#' definition!
#'
#' Here is the generic for print
print

#' Often there is a default method
print.default

#' Write a custom print method:
print.grades <- function(x, ...) {
  cat("Object of class", class(x), "with", length(x), "grades\n")
  print(unclass(x))
}

o


#' Write a custom mean method:
mean.grades <- function(x, ...) {
    x <- factor(x, levels = LETTERS[c(1:4,6)])
    x <- 5 - as.numeric(x)
    mean(x)
  }

mean(o)

#' ## Defining Generic functions
#'
#'  R has many generic functions already defined. Sometimes it is convenient
#'  to define your own.
#'  **Note:** Make sure it does not exist!
exists("print_gpa")

print_gpa <- function (x, ...) {
  UseMethod("print_gpa", x)
}

print_gpa

print_gpa.grades <- function(x, ...) {
    cat("Your grade point average is: ", mean(x), "\n")
}

print_gpa(o)

#' ## Inheritance
#' Inheritance can be implemented by using a vector of class names. Methods
#' are chosen left to right.

class(o) <- c("grades", "letters")
str(o)
is(o, "grades")
is(o, "letters")

#' ## Constructors
#'
#' Best practice is to provide a constructor function.

grades <- function(x) {
  if(!is.character(x)) stop("Only characters allowed!")
  if(any(is.na(match(x, LETTERS[c(1:4,5)]))))
    stop("Illegal grade!")

  class(x) <- "grades"
  x
}


grades(c("A", "A", "A"))
try(
  grades(c(1, 2, 3))
)

try(
  grades(c("A", "I"))
)

#' More information on S3 can be found using `? S3` and at
#' http://adv-r.had.co.nz/S3.html
#'
#' **Final note:** R also has a formal class system called S4 (`? S4`) and reference classes (`? ReferenceClasses`)
#'


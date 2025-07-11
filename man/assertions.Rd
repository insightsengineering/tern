% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_checkmate.R
\name{assertions}
\alias{assertions}
\alias{assert_list_of_variables}
\alias{assert_df_with_variables}
\alias{assert_valid_factor}
\alias{assert_df_with_factors}
\alias{assert_proportion_value}
\title{Additional assertions to use with \code{checkmate}}
\usage{
assert_list_of_variables(x, .var.name = checkmate::vname(x), add = NULL)

assert_df_with_variables(
  df,
  variables,
  na_level = NULL,
  .var.name = checkmate::vname(df),
  add = NULL
)

assert_valid_factor(
  x,
  min.levels = 1,
  max.levels = NULL,
  null.ok = TRUE,
  any.missing = TRUE,
  n.levels = NULL,
  len = NULL,
  .var.name = checkmate::vname(x),
  add = NULL
)

assert_df_with_factors(
  df,
  variables,
  min.levels = 1,
  max.levels = NULL,
  any.missing = TRUE,
  na_level = NULL,
  .var.name = checkmate::vname(df),
  add = NULL
)

assert_proportion_value(x, include_boundaries = FALSE)
}
\arguments{
\item{x}{(\code{any})\cr object to test.}

\item{.var.name}{[\code{character(1)}]\cr
Name of the checked object to print in assertions. Defaults to
the heuristic implemented in \code{\link[checkmate]{vname}}.}

\item{add}{[\code{AssertCollection}]\cr
Collection to store assertion messages. See \code{\link[checkmate]{AssertCollection}}.}

\item{df}{(\code{data.frame})\cr data set to test.}

\item{variables}{(named \code{list} of \code{character})\cr list of variables to test.}

\item{na_level}{(\code{string})\cr the string you have been using to represent NA or
missing data. For \code{NA} values please consider using directly \code{\link[=is.na]{is.na()}} or
similar approaches.}

\item{min.levels}{[\code{integer(1)}]\cr
Minimum number of factor levels.
Default is \code{NULL} (no check).}

\item{max.levels}{[\code{integer(1)}]\cr
Maximum number of factor levels.
Default is \code{NULL} (no check).}

\item{null.ok}{[\code{logical(1)}]\cr
If set to \code{TRUE}, \code{x} may also be \code{NULL}.
In this case only a type check of \code{x} is performed, all additional checks are disabled.}

\item{any.missing}{[\code{logical(1)}]\cr
Are vectors with missing values allowed? Default is \code{TRUE}.}

\item{n.levels}{[\code{integer(1)}]\cr
Exact number of factor levels.
Default is \code{NULL} (no check).}

\item{len}{[\code{integer(1)}]\cr
Exact expected length of \code{x}.}

\item{include_boundaries}{(\code{flag})\cr whether to include boundaries when testing
for proportions.}
}
\value{
Nothing if assertion passes, otherwise prints the error message.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Additional assertion functions which can be used together with the \code{checkmate} package.
}
\section{Functions}{
\itemize{
\item \code{assert_list_of_variables()}: Checks whether \code{x} is a valid list of variable names.
\code{NULL} elements of the list \code{x} are dropped with \code{Filter(Negate(is.null), x)}.

\item \code{assert_df_with_variables()}: Check whether \code{df} is a data frame with the analysis \code{variables}.
Please notice how this produces an error when not all variables are present in the
data.frame while the opposite is not required.

\item \code{assert_valid_factor()}: Check whether \code{x} is a valid factor (i.e. has levels and no empty
string levels). Note that \code{NULL} and \code{NA} elements are allowed.

\item \code{assert_df_with_factors()}: Check whether \code{df} is a data frame where the analysis \code{variables}
are all factors. Note that the creation of \code{NA} by direct call of \code{factor()} will
trim \code{NA} levels out of the vector list itself.

\item \code{assert_proportion_value()}: Check whether \code{x} is a proportion: number between 0 and 1.

}}
\examples{
x <- data.frame(
  a = 1:10,
  b = rnorm(10)
)
assert_df_with_variables(x, variables = list(a = "a", b = "b"))

x <- ex_adsl
assert_df_with_variables(x, list(a = "ARM", b = "USUBJID"))

x <- ex_adsl
assert_df_with_factors(x, list(a = "ARM"))

assert_proportion_value(0.95)
assert_proportion_value(1.0, include_boundaries = TRUE)

}
\keyword{internal}

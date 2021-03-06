% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{string_replace}
\alias{string_replace}
\title{Test or Apply string replacements}
\usage{
string_replace(Input, Var, Reg_expression, Replacement, Mode = "Test")
}
\arguments{
\item{Input}{Input tibble}

\item{Reg_expression}{Regular expression to apply to input}

\item{Replacement}{String to use for replacement}

\item{Mode}{Test/Apply = test will return just unique changes}

\item{Output}{Variable to out result into}
}
\value{
A tibble will be returned with the:
\itemize{
\item Input$Var (where a change occurred)
\item Output
}
}
\description{
Apply a regular expression and return the original and the changed
value, only the unique values
}

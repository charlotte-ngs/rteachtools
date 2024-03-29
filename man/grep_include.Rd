% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deploy.R
\name{grep_include}
\alias{grep_include}
\title{Grep Rmd Source File For Includes}
\usage{
grep_include(
  pvec_src,
  ps_grep_pattern,
  pvec_repl_pattern = c("\\\\(path = ", "\\"", "\\\\)")
)
}
\arguments{
\item{pvec_src}{vector containing Rmd source}

\item{ps_grep_pattern}{pattern that matches the lines with include statments}

\item{pvec_repl_pattern}{vector of characters that are removed from include statements}
}
\value{
vec_result vector with files that are included
}
\description{
In a vector containing a Rmd source file, all files which are included are searched
and returned in a result vector. What is recognised as include is defined in the
search pattern given by the argument \code{ps_grep_pattern}.
}

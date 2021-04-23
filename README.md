
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rteachtools

<!-- badges: start -->

<!-- badges: end -->

The package website can be found at
<https://charlotte-ngs.github.io/rteachtools/>

The goal of rteachtools is to provide basic functionality to help in
teaching courses

## Installation

You can install the released version from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("charlotte-ngs/rteachtools")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rteachtools)
## creating a new exercise document
rteachtools::create_exercise(ps_ex_path    = "ex/gel_ex01/gel_ex01.Rmd",
                             pn_nr_problem = 2,
                             pl_data       = list(course_name    = "Genetic Evaluation",
                                                  exercise_count = 1,
                                                  creation_date  = format(Sys.Date(), "%Y-%m-%d"),
                                                  author         = "Peter von Rohr"))
```

-----

*Latest Changes: 2021-04-17, 13:27:59 ( peter )*

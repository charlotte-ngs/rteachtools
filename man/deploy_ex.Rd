% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deploy.R
\name{deploy_ex}
\alias{deploy_ex}
\title{Deploy Exercise}
\usage{
deploy_ex(
  ps_ex_path,
  ps_ex_out_dir,
  ps_sol_out_dir,
  ps_rexpf_src = NULL,
  ps_rexpf_trg = NULL,
  pb_debug = FALSE,
  pobj_rtt_logger = NULL
)
}
\arguments{
\item{ps_ex_path}{path to the source RMarkdown file}

\item{ps_ex_out_dir}{directory for exercise output}

\item{ps_sol_out_dir}{directory for the solution output}

\item{ps_rexpf_src}{source directory for exercise Rmd file to deploy to rexpf}

\item{ps_rexpf_trg}{target directory to where exercise Rmd is to be deployed to}

\item{pb_debug}{flag to determine debugging status}

\item{pobj_rtt_logger}{log4j logger object}
}
\description{
An parametrized and unified Rmd source file containing both exericse problems
and solutions to these problems  is rendered two times once to produce a pdf-output
that is used as an excercise. The second pdf-output corresponds to the solution
to the exercises. This parametrized double-rendering only works, if the solutios
do not contain any function calles of \code{knitr::include_graphics()} because
this function adds a comment after the include statement in the md-output.
This causes the commented out solution sections to end. In that case the
deployment has to be done with the function \code{deploy_src_to_ex_sol()}.
In case the Rmarkdown notebook used on the exercise platform (rexpf)
is available it is deployed to the target directory where the material for rexpf
is stored. The path to the rexpf material is given by the argument ps_rexpf_trg.
The Rmarkdown notebook used on rexpf can be produced with the function convert_ex_to_nb().
}
\details{
The call to \code{rmarkdown::render()} takes the value for the parameters which
are used to produce the exercise or the solution document.
}
\examples{
\dontrun{
deploy_ex(ps_ex_path     = 'ex/asm_ex02.Rmd',
          ps_ex_out_dir  = 'docs/ex',
          ps_sol_out_dir = 'docs/sol')
}

}

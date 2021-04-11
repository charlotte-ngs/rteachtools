## -- Deployment Using Parametrized Knitting ----------------------------------
#'
#'
#' @title Deploy Exercise
#'
#' @description
#' An parametrized and unified Rmd source file containing both exericse problems
#' and solutions to these problems  is rendered two times once to produce a pdf-output
#' that is used as an excercise. The second pdf-output corresponds to the solution
#' to the exercises. This parametrized double-rendering only works, if the solutios
#' do not contain any function calles of \code{knitr::include_graphics()} because
#' this function adds a comment after the include statement in the md-output.
#' This causes the commented out solution sections to end. In that case the
#' deployment has to be done with the function \code{deploy_src_to_ex_sol()}.
#' In case the Rmarkdown notebook used on the exercise platform (rexpf)
#' is available it is deployed to the target directory where the material for rexpf
#' is stored. The path to the rexpf material is given by the argument ps_rexpf_trg.
#' The Rmarkdown notebook used on rexpf can be produced with the function convert_ex_to_nb().
#'
#' @details
#' The call to \code{rmarkdown::render()} takes the value for the parameters which
#' are used to produce the exercise or the solution document.
#'
#' @param ps_ex_path path to the source RMarkdown file
#' @param ps_ex_out_dir directory for exercise output
#' @param ps_sol_out_dir directory for the solution output
#' @param ps_rexpf_src source directory for exercise Rmd file to deploy to rexpf
#' @param ps_rexpf_trg target directory to where exercise Rmd is to be deployed to
#' @param pb_debug flag to determine debugging status
#' @param pobj_rtt_logger log4j logger object
#'
#' @examples
#' \dontrun{
#' deploy_ex(ps_ex_path     = 'ex/asm_ex02.Rmd',
#'           ps_ex_out_dir  = 'docs/ex',
#'           ps_sol_out_dir = 'docs/sol')
#' }
#'
#' @export deploy_ex
deploy_ex <- function(ps_ex_path,
                      ps_ex_out_dir,
                      ps_sol_out_dir,
                      ps_rexpf_src    = NULL,
                      ps_rexpf_trg    = NULL,
                      pb_debug        = FALSE,
                      pobj_rtt_logger = NULL){
  # init logging for this function
  if (pb_debug){
    if (is.null(pobj_rtt_logger)){
      rtt_logger <- get_rtt_logger(ps_logfile = 'deploy_rtt.log', ps_level = 'INFO')
    } else {
      rtt_logger <- pobj_rtt_logger
    }
  }
  # check for file extension
  if (tolower(tools::file_ext(ps_ex_path)) == 'rmd'){
    s_ex_path <- ps_ex_path
  } else {
    s_ex_path <- paste(ps_ex_path, '.Rmd', sep = '')
  }
  if (pb_debug)
    rtt_log_info(plogger = rtt_logger,
                 ps_caller = 'deploy_ex',
                 ps_msg = paste0(" * Setting source path to: ", s_ex_path, collapse = ''))
  if (!file.exists(s_ex_path))
    stop(" *** [deploy_ex] ERROR: CANNOT FIND exercise source path: ", s_ex_path)
  # assign directory of s_ex_path, for deployment of solution to rexpf
  s_ex_dir <- dirname(s_ex_path)

  # determine path for output files
  if (!dir.exists(ps_ex_out_dir)){
    s_log_msg <- paste0(" * Cannot find ex dir: ", ps_ex_out_dir, ' ==> create it', collapse = '')
    dir.create(path = ps_ex_out_dir)
  } else {
    s_log_msg <- paste0(" * Found ex dir: ", ps_ex_out_dir, collapse = '')
  }
  if (pb_debug)
    rtt_log_info(plogger = rtt_logger, ps_caller = 'deploy_ex', ps_msg = s_log_msg)

  # render source file to exercise version
  s_ex_name <- tools::file_path_sans_ext(basename(s_ex_path))
  s_ex_out_path <- file.path(ps_ex_out_dir, paste(s_ex_name, '.pdf', sep = ''))
  if (pb_debug)
    rtt_log_info(plogger = rtt_logger,
                 ps_caller = 'deploy_ex',
                 ps_msg = paste0(" * Rendering ex source to: ", s_ex_out_path, collapse = ''))
  rmarkdown::render(input = s_ex_path, output_file = s_ex_out_path, params = list(doctype = 'exercise'))

  # render source file to solution document
  if (!dir.exists(ps_sol_out_dir)){
    s_log_msg <- paste0(" * Cannot find sol out dir: ", ps_sol_out_dir, " ==> create it", collapse = '')
    dir.create(path = ps_sol_out_dir, recursive = TRUE)
  } else {
    s_log_msg <- paste0(" * Found sol out dir: ", ps_sol_out_dir, collapse = '')
  }
  if (pb_debug)
    rtt_log_info(plogger = rtt_logger, ps_caller = 'deploy_ex', ps_msg = s_log_msg)
  # path to solution and render
  s_sol_out_path <- file.path(ps_sol_out_dir, paste(s_ex_name, '_sol.pdf', sep = ''))
  if (pb_debug)
    rtt_log_info(plogger = rtt_logger,
                 ps_caller = 'deploy_ex',
                 ps_msg = paste0(" * Rendering ex source to: ", s_sol_out_path, collapse = ''))
  rmarkdown::render(input = s_ex_path, output_file = s_sol_out_path, params = list(doctype = 'solution'))

  # deploy exercise material to rexpf
  if (!is.null(ps_rexpf_src) && !is.null(ps_rexpf_trg)){
    if (pb_debug)
      rtt_log_info(plogger = rtt_logger,
                   ps_caller = 'deploy_ex',
                   ps_msg = paste0(" * Deploy ex from source: ", ps_rexpf_src,
                                   " to rexpf target: ", ps_rexpf_trg, collapse = ''))
    # deploy exercise nb
    s_ex_new_path <- file.path(ps_rexpf_trg, 'ex', s_ex_name)
    if (dir.exists(s_ex_new_path)) fs::dir_delete(path = s_ex_new_path)
    fs::dir_copy(path = file.path(ps_rexpf_src, s_ex_name), new_path = s_ex_new_path)
    # deploy solution
    s_sol_new_path <- file.path(ps_rexpf_trg, 'sol', s_ex_name)
    if (dir.exists(s_sol_new_path)) fs::dir_delete(path = s_sol_new_path)
    fs::dir_copy(path = s_ex_dir, new_path = s_sol_new_path)

  }

  return(invisible(TRUE))
}


## -- Deployment of Unified Rmd Source File To Solution and Reduced Version To Exercise ----
#'
#'
#' @title Deployment of Solutions From Unified Rmd File and of Exercise from Reduced Rmd Source
#'
#' @description
#' This function takes as input the unified Rmd source files containing exercise problem
#' questions and solutions to these problems. From the unified Rmd file the solution pdf
#' document is produced. The solution section in the unified Rmd source file must be tagged
#' by a given html-comment. These tags are used to produce a reduced Rmd source file which
#' does not contain the solutions to the exercise problems. The reduced Rmd file is used
#' to produce the exercise pdf document.
#'
#' @details
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export deploy_src_to_ex_sol
deploy_src_to_ex_sol <- function(ps_uni_src_path){

}


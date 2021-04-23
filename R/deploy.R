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
      rtt_logger <- get_rtt_logger(ps_logfile = 'deploy_ex_rtt.log', ps_level = 'INFO')
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
#' @param ps_uni_src_path path to unified Rmd source file
#' @param ps_ex_out_dir exercise output directory
#' @param ps_sol_out_dir solution output directory
#' @param ps_nb_src_dir notebook source directory
#' @param ps_nb_out_dir notebook output directory
#' @param ps_rexpf_trg target for r exercise platform
#' @param pl_master_solution_tags list of tags for master solution
#' @param pl_aug_info_tags list of tags for augmented solutions
#' @param pb_keep_src flag whether to keep source file
#' @param pb_debug debug flag
#' @param pobj_rtt_logger logger object
#'
#' @examples
#' \dontrun{
#' s_uni_src_path <- system.file("extdata", "asm_ex06.Rmd", package = "rteachtools")
#' s_ex_src_name <- tools::file_path_sans_ext(basename(s_uni_src_path))
#' deploy_src_to_ex_sol(ps_uni_src_path = s_uni_src_path,
#'                      ps_ex_out_dir = file.path("docs","ex"),
#'                      ps_sol_out_dir = file.path("docs","sol"),
#'                      ps_nb_src_dir = file.path("nb", s_ex_src_name),
#'                      ps_nb_out_dir = file.path("docs", "nb"))
#' }
#'
#' @export deploy_src_to_ex_sol
deploy_src_to_ex_sol <- function(ps_uni_src_path,
                                 ps_ex_out_dir,
                                 ps_sol_out_dir,
                                 ps_nb_src_dir,
                                 ps_nb_out_dir,
                                 ps_rexpf_trg     = NULL,
                                 pl_master_solution_tags = list(start = "master-solution-start",
                                                                end   = "master-solution-end"),
                                 pl_aug_info_tags = list(start="your-solution-start", end = "your-solution-end"),
                                 pb_keep_src      = FALSE,
                                 pb_debug         = FALSE,
                                 pobj_rtt_logger  = NULL){
  # init logging for this function
  if (pb_debug){
    if (is.null(pobj_rtt_logger)){
      rtt_logger <- get_rtt_logger(ps_logfile = paste0(format(Sys.time(), "%Y%m%d%H%M%S"),
                                                       '_deploy_src_to_ex_sol_rtt.log', collapse = ""),
                                   ps_level = 'INFO')
    } else {
      rtt_logger <- pobj_rtt_logger
    }
  }
  # check for file extension
  if (tolower(tools::file_ext(ps_uni_src_path)) == 'rmd'){
    s_uni_src_path <- ps_uni_src_path
  } else {
    s_uni_src_path <- paste(ps_uni_src_path, '.Rmd', sep = '')
  }
  if (pb_debug)
    rtt_log_info(plogger = rtt_logger,
                 ps_caller = 'deploy_ex',
                 ps_msg = paste0(" * Setting source path to: ", s_uni_src_path, collapse = ''))
  if (!file.exists(s_uni_src_path))
    stop(" *** [deploy_ex] ERROR: CANNOT FIND exercise source path: ", s_uni_src_path)

  # read unified source file into a vector
  vec_uni_src <- readLines(con = s_uni_src_path)

  # remove sections containing the solution to have first version of source
  # file with exercise problem question and with augmented information
  vec_ex_aug <- remove_section(pvec_src = vec_uni_src, pl_tag = pl_master_solution_tags)

  # create Rmd source file for exercise document by removing the augmented information
  vec_ex_src <- remove_section(pvec_src = vec_ex_aug, pl_tag = pl_aug_info_tags)

  # finalise the source with augmented information to result in the source for
  # the notebook
  vec_ex_nb <- remove_line(pvec_src = vec_ex_aug, pvec_pattern = c(pl_aug_info_tags$start,
                                                                   pl_aug_info_tags$end))
  # modify the yaml header
  l_yml_fm_uni_src <- rmarkdown::yaml_front_matter(input = s_uni_src_path)
  l_yml_fm_nb <- list(title = gsub(pattern = '`r tools::toTitleCase(params$doctype)`',
                                   replacement = 'Notebook', l_yml_fm_uni_src$title, fixed = TRUE),
                      author = l_yml_fm_uni_src$author,
                      date = l_yml_fm_uni_src$date,
                      output = 'html_notebook')
  # get yaml boundaries
  vec_yaml_bound <- grep('---', vec_ex_nb, fixed = TRUE)

  # put together nb source
  vec_ex_nb <- c('---',
                 paste('title: ', l_yml_fm_nb$title, sep = ''),
                 paste('author: ', l_yml_fm_nb$author, sep = ''),
                 paste('date: ', l_yml_fm_nb$date, sep = ''),
                 paste('output: ', l_yml_fm_nb$output, sep = ''),
                 '---',
                 vec_ex_nb[(vec_yaml_bound[2]+1):length(vec_ex_nb)],
                 paste0("\n\n```{r, echo=FALSE, results='asis'}\ncat('\\n---\\n\\n _Latest Changes: ', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), ' (', Sys.info()['user'], ')_\\n', sep = '')\n```\n", collapse = ""))

  # do the rendering of exercise pdf, start with checking whether the output directory exists
  if (!dir.exists(ps_ex_out_dir)){
    dir.create(ps_ex_out_dir, recursive = TRUE)
  }
  # write exercise source to source file
  s_ex_src_name <- tools::file_path_sans_ext(basename(s_uni_src_path))
  s_ex_src_path <- file.path(ps_ex_out_dir, paste0(s_ex_src_name, "_ex_src.Rmd"))
  cat(paste0(vec_ex_src, collapse = "\n"), "\n", file = s_ex_src_path)
  # render exercise source to pdf
  s_ex_out_path <- file.path(ps_ex_out_dir, paste(s_ex_src_name, '.pdf', sep = ''))
  rmarkdown::render(input = s_ex_src_path, output_file = s_ex_out_path, params = list(doctype = 'exercise'))
  # clean up ex_src
  if (!pb_keep_src) fs::file_delete(path = s_ex_src_path)

  # render source file to solution document
  if (!dir.exists(ps_sol_out_dir)){
    dir.create(path = ps_sol_out_dir, recursive = TRUE)
  }
  # path to solution and render
  s_sol_out_path <- file.path(ps_sol_out_dir, paste(s_ex_src_name, '_sol.pdf', sep = ''))
  rmarkdown::render(input = s_uni_src_path, output_file = s_sol_out_path, params = list(doctype = 'solution'))

  # do the rendering of nb
  if (!dir.exists(ps_nb_src_dir)) dir.create(ps_nb_src_dir)
  s_nb_src_path <- file.path(ps_nb_src_dir, paste0(s_ex_src_name, "_nb_src.Rmd"))
  cat(paste0(vec_ex_nb, collapse = "\n"), "\n", file = s_nb_src_path)
  # render the nb
  rmarkdown::render(input = s_nb_src_path, output_dir = ps_nb_out_dir)

  # deploy to rexpf
  if (!is.null(ps_rexpf_trg)){
    # copy nb source as exercise
    fs::dir_copy(path = file.path(ps_nb_src_dir), new_path = file.path(s_rexpf_dir, "ex"))
    # copy ex source as solution
    fs::dir_copy(path = dirname(s_uni_src_path), new_path = file.path(s_rexpf_dir, "sol"))

  }

  # return invisibly
  return(invisible(TRUE))

}

## --- Helper Functions -------------------------------------------------------
#'
#'
#' @title Remove Section From Document
#'
#' @description
#' A section is removed from a source document where the
#' section to be removed is indicated by a start and end tag.
#'
#' @param pvec_src Rmd source vector
#' @param pl_tag List with section tags
#'
#' @return vec_result vector with source without section to be removed
remove_section <- function(pvec_src, pl_tag){
  # pvec_src = vec_uni_src; pl_tag = list(start="master-solution-start", end = "master-solution-end")
  # determine the start and the end positions of the section to be cut out
  vec_start <- grep(pattern = pl_tag$start, pvec_src)
  n_nr_tag <- length(vec_start)
  vec_end <- grep(pattern = pl_tag$end, pvec_src)
  # check that start and end positions have the same length
  if (n_nr_tag != length(vec_end))
    stop(" *** Error: number of start and end-tags not equal")
  # put together the result
  vec_result <- pvec_src[1:(vec_start[1]-1)]
  # add the remaining sections
  if (n_nr_tag > 1){
    for (idx in 2:n_nr_tag){
      vec_result <- c(vec_result,
                      pvec_src[(vec_end[idx-1]+1):(vec_start[idx]-1)])
    }
  }
  # if after last end-tag there are any further lines, add them
  n_src_len <- length(pvec_src)
  if (n_src_len > vec_end[n_nr_tag]){
    vec_result <- c(vec_result,
                    pvec_src[(vec_end[n_nr_tag]+1):n_src_len])
  }
  return(vec_result)
}


#' Remove a Line From a Document
#'
#' @description
#' A line identified by a given pattern is removed from a
#' document.
#'
#' @param pvec_src vector containing source document
#' @param pvec_pattern pattern which identifies line to be removed
#'
#' @return vec_result vector containing source without line to be removed
remove_line <- function(pvec_src, pvec_pattern){
  # vector with line indices where the pattern is found
  vec_line_idx <- as.vector(sapply(pvec_pattern, function(x) grep(pattern = x, pvec_src), USE.NAMES = FALSE))
  # set the result
  vec_result <- pvec_src[-vec_line_idx]
  return(vec_result)
}


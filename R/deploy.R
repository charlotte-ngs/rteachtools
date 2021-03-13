#'
#'
#' @title Deploy Exercise
#'
#' @description
#' An parametrized Rmd source file is rendered two times once to produce a pdf-output
#' that is used as an excercise. The second pdf-output corresponds to the solution
#' to the exercises.
#'
#' @details
#' The call to \code{rmarkdown::render()} takes the value for the parameters which
#' are used to produce the exercise or the solution document.
#'
#' @param ps_ex_path path to the source RMarkdown file
#' @param ps_ex_out_dir directory for exercise output
#' @param ps_sol_out_dir directory for the solution output
#' @param pb_debug flag to determine debugging status
#' @param pobj_rtt_logger log4j logger object
#'
#' @examples
#' \dontrun{
#' deploy_ex(ps_ex_path = 'ex/asm_ex02.Rmd', ps_ex_out_dir = 'docs/ex', ps_sol_out_dir = 'docs/sol')
#' }
#'
#' @export deploy_ex
deploy_ex <- function(ps_ex_path,
                      ps_ex_out_dir,
                      ps_sol_out_dir,
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
  s_ex_out_path <- file.path(ps_ex_out_dir, paste(tools::file_path_sans_ext(basename(ps_ex_path)), '.pdf', sep = ''))
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
  s_sol_out_path <- file.path(ps_sol_out_dir, paste(tools::file_path_sans_ext(basename(s_ex_path)), '_sol.pdf', sep = ''))
  if (pb_debug)
    rtt_log_info(plogger = rtt_logger,
                 ps_caller = 'deploy_ex',
                 ps_msg = paste0(" * Rendering ex source to: ", s_sol_out_path, collapse = ''))
  rmarkdown::render(input = s_ex_path, output_file = s_sol_out_path, params = list(doctype = 'solution'))

  return(invisible(TRUE))
}



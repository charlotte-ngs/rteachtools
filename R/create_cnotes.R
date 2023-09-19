## -- Course Notes Creation Functionality -----------------------------------------
#'
#'
#' @title Create Course Notes
#'
#' @description
#' Wrapper function around rmarkdown::draft to create customized course notes.
#'
#' @details
#' The function takes care of directory management and replaces placeholders in
#' templates using data specified in pl_repl_data. The replacement is done using
#' whisker::whisker.render().
#'
#' @param ps_cn_dir course notes directory
#' @param ps_template template for course notes
#' @param ps_package package containing course notes template
#' @param pb_edit edit created course notes index file
#' @param pl_repl_data list with replacement data
#' @param pb_force force to create course notes directory, if it already exists
#' @param pb_debug run function in debug mode and write debug info to logfile
#' @param pobj_rtt_logger logger object to write loginfo to
#'
#' @export create_cnotes
create_cnotes <- function(ps_cn_dir,
                          ps_template = "cnotesdown",
                          ps_package  = "rteachtools",
                          pb_edit     = FALSE,
                          pl_repl_data,
                          pb_force        = FALSE,
                          pb_debug        = FALSE,
                          pobj_rtt_logger = NULL){
  # init logging for this function
  if (pb_debug){
    if (is.null(pobj_rtt_logger)){
      rtt_logger <- get_rtt_logger(ps_logfile = paste0(format(Sys.time(), "%Y%m%d%H%M%S"),
                                                       '_create_cnotes.log', collapse = ""),
                                   ps_level = 'INFO')
    } else {
      rtt_logger <- pobj_rtt_logger
    }
  }
  s_cn_dir <- ps_cn_dir
  # remove .Rmd if needed
  if (tolower(fs::path_ext(s_cn_dir)) == "rmd"){
    s_cn_dir <- fs::path_ext_remove(s_cn_dir)
    if (pb_debug)
      rtt_log_info(plogger   = rtt_logger,
                   ps_caller = "create_cnotes",
                   ps_msg    = paste0(" * Removed extension from to cn path: ", s_cn_dir))
  }
  # if the path already exists, either remove it or stop here
  if (fs::dir_exists(s_cn_dir)){
    if (pb_force){
      fs::dir_copy(path = s_cn_dir, new_path = paste0(s_cn_dir, ".", format(Sys.time(), "%Y%m%d%H%M%S"), collapse = ""))
      fs::dir_delete(s_cn_dir)
      if (pb_debug)
        rtt_log_info(plogger   = rtt_logger,
                     ps_caller = "create_cnotes",
                     ps_msg    = paste0(" * Removed existing cn path: ", s_cn_dir))
    } else {
      if (pb_debug)
        rtt_log_error(plogger = rtt_logger,
                      ps_caller = "create_cnotes",
                      ps_msg    = " *** Error cn path already exists. Either remove existing path or set option pb_force = TRUE")
      stop(" *** Error cn path already exists. Either remove existing path or set option pb_force = TRUE")
    }
  }
  # get the basename directory of the course notes path
  s_cn_bn <- basename(s_cn_dir)
  # draft the template into a temp dir
  s_cn_tmp_dir <- file.path(tempdir(), s_cn_bn)
  rmarkdown::draft(file     = s_cn_tmp_dir,
                   template = ps_template,
                   package  = ps_package,
                   edit     = FALSE)
  if (pb_debug)
    rtt_log_info(plogger   = rtt_logger,
                 ps_caller = "create_cnotes",
                 ps_msg    = paste0(" * Drafted cn into: ", s_cn_tmp_dir))





}


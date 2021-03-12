### #
### #
### #
### #   Purpose:   Functions Related to Logging
### #   started:   2019-10-03 (pvr)
### #
### # ############################################## ###

#' @title Create log4r Logger for package
#'
#' @param ps_logfile name of the logfile
#' @param ps_level logger level
#'
#' @return rtt_logger
#' @export get_rtt_logger
#'
#' @examples
#' \dontrun{
#' rtt_logger <- get_rtt_logger()
#' }
get_rtt_logger <- function(ps_logfile = 'rtt.log', ps_level = 'FATAL'){
  rtt_logger <- log4r::create.logger(logfile = ps_logfile, level = ps_level)
  return(rtt_logger)
}


#' @title Wrapper for log4r info
#'
#' @param plogger log4r logger object
#' @param ps_caller function from which we are called
#' @param ps_msg logging message
#'
#' @export rtt_log_info
#'
#' @examples
#' \dontrun{
#' rtt_logger <- get_rtt_logger()
#' rtt_log_level(rtt_logger, 'INFO')
#' rtt_log_info(rtt_logger, 'Examples', 'test message')
#' }
rtt_log_info <- function(plogger, ps_caller, ps_msg){
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::info(logger = plogger, message = s_msg)
}


#' @title Wrapper for log4r debug
#'
#' @param plogger log4r logger object
#' @param ps_caller function from which we are called
#' @param ps_msg logging message
#'
#' @export rtt_log_debug
#'
#' @examples
#' \dontrun{
#' rtt_logger <- get_rtt_logger()
#' rtt_log_level(rtt_logger, 'DEBUG')
#' rtt_log_debug(rtt_logger, 'Examples', 'test message')
#' }
rtt_log_debug <- function(plogger, ps_caller, ps_msg){
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::debug(logger = plogger, message = s_msg)
}


#' @title Wrapper for log4r warn
#'
#' @param plogger log4r logger object
#' @param ps_caller function from which we are called
#' @param ps_msg logging message
#'
#' @export rtt_log_warn
#'
#' @examples
#' \dontrun{
#' rtt_logger <- get_rtt_logger()
#' rtt_log_level(rtt_logger, 'WARN')
#' rtt_log_warn(rtt_logger, 'Examples', 'test message')
#' }
rtt_log_warn <- function(plogger, ps_caller, ps_msg){
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::warn(logger = plogger, message = s_msg)
}


#' @title Wrapper for log4r error
#'
#' @param plogger log4r logger object
#' @param ps_caller function from which we are called
#' @param ps_msg logging message
#'
#' @export rtt_log_error
#'
#' @examples
#' \dontrun{
#' rtt_logger <- get_rtt_logger()
#' rtt_log_level(rtt_logger, 'ERROR')
#' rtt_log_error(rtt_logger, 'Examples', 'test message')
#' }
rtt_log_error <- function(plogger, ps_caller, ps_msg){
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::error(logger = plogger, message = s_msg)
}


#' @title Wrapper for log4r fatal
#'
#' @param plogger log4r logger object
#' @param ps_caller function from which we are called
#' @param ps_msg logging message
#'
#' @export rtt_log_fatal
#'
#' @examples
#' \dontrun{
#' rtt_logger <- get_rtt_logger()
#' rtt_log_level(rtt_logger, 'FATAL')
#' rtt_log_fatal(rtt_logger, 'Examples', 'test message')
#' }
rtt_log_fatal <- function(plogger, ps_caller, ps_msg){
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::fatal(logger = plogger, message = s_msg)
}


#' @title Wrapper to set the level of a logger
#'
#' @param plogger log4r logger object
#' @param ps_level new level of plogger
#'
#' @export rtt_log_level
#'
#' @examples
#' \dontrun{
#' rtt_logger <- get_rtt_logger()
#' rtt_log_level(rtt_logger, 'INFO')
#' }
rtt_log_level <- function(plogger, ps_level = c('DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL')){
  if (!missing(ps_level) & length(ps_level) > 1) stop(" *** ERROR in level(): only one 'level' allowed.")
  ps_level <- match.arg(ps_level)
  log4r::level(plogger) <- ps_level
}

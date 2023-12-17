## -- Creation of Student Info -------------------------------------------------
#' @title Extract Student Info for R Exercise Platform
#'
#' @description
#' Take an xlsx input file downloaded from the student information platform
#' edoz and extract all information relevant for the R exercise platform.
#'
#' @details
#' Usernames are created based on the part of the E-Mail address before the
#' at sign. By default, the xlsx student input file is searched in the
#' subdirectory "students" of the current RStudio-project where the path to
#' this project is determined by the result of \code{here::here()}.
#'
#' @param ps_bl_xlsx directory or path where input xlsx file can be found
#' @param pvec_col_keep columns that are kept from original data
#' @param pn_start_port start of mapped port on host
#'
#' @return tibble with relevant student information
#' @export extract_student_info
#'
#' @examples
#' \dontrun{
#' s_bl_xlsx <- file.path(here::here(), "students")
#' extract_student_info(ps_bl_xlsx = s_bl_xlsx)
#'
#' }
extract_student_info <- function(ps_bl_xlsx    = file.path(here::here(), "students"),
                                 pvec_col_keep = c("Familienname",
                                                   "Rufname",
                                                   "Nummer",
                                                   "E-Mail"),
                                 pvec_username = NULL,
                                 pn_start_port = 10087){
  # if ps_bl_xlsx is a directory take xlsx file as input path
  s_bl_xlsx <- ps_bl_xlsx
  if (fs::dir_exists(s_bl_xlsx)){
    s_bl_xlsx <- list.files(path = s_bl_xlsx, pattern = "xlsx", full.names = TRUE)
  }
  # check whether xlsx input path was found
  if (length(s_bl_xlsx) != 1L) stop(" *** ERROR: xlsx input path empty")
  # check whether xlsx input file exists
  if (!fs::file_exists(s_bl_xlsx)) stop(" *** ERROR: CANNOT FIND: ", s_bl_xlsx)
  # read xlsx input
  tbl_student_result <- readxl::read_excel(path = s_bl_xlsx)
  # filter for columns to be kept
  tbl_student_result <- tbl_student_result[,pvec_col_keep]
  # create additional fields based on e-mail
  if (!is.element("E-Mail", colnames(tbl_student_result)))
    stop(" *** ERROR: CANNOT FIND column with e-mails of students ...")
  # Username
  vec_username <- pvec_username
  if (is.null(vec_username)){
    vec_username <- sapply(tbl_student_result$`E-Mail`,
                          function(x) unlist(strsplit(x, "@", fixed = T))[1] ,
                          USE.NAMES = FALSE)
  }
  tbl_student_result$Username <- vec_username
  # Ports
  tbl_student_result$Port <- c(pn_start_port:(pn_start_port + nrow(tbl_student_result) - 1))
  # return result
  return(tbl_student_result)
}


## -- Random Student-ID Number -------------------------------------------------


#' Generate Random Student ID Number
#'
#' @description
#' The student ID number consists of three parts. These parts are integer numbers
#' out of some assumed ranges. From these ranges numbers are sampled from a uniform
#' distribution
#'
#'
#' @param l_year_limit limits for first part
#' @param l_middle_limit limits for middle part
#' @param l_last_limit limits of last part
#'
#' @return generated student ID
#' @export get_random_student_id
#'
#' @examples
#' \dontrun{
#' get_random_student_id()
#' }
get_random_student_id <- function(pl_year_limit = list(low = as.integer(format(Sys.Date(), "%y"))-4L,
                                                       up = as.integer(format(Sys.Date(), "%y"))-2L),
                                  pl_middle_limit = list(low = 900,
                                                         up  = 999),
                                  pl_last_limit = list(low = 100, up = 199)){
  # generate three parts
  n_year <- floor(runif(1, min = pl_year_limit$low, max = pl_year_limit$up))
  n_middle <- floor(runif(1, min = pl_middle_limit$low, max = pl_middle_limit$up))
  n_last <- floor(runif(1, min = pl_last_limit$low, max = pl_last_limit$up))
  # return result
  s_result_number <- paste0(c(n_year, n_middle, n_last), collapse = "-")
  return(s_result_number)
}



## -- Exam Creation Functionality -----------------------------------------
#'
#' TODO: tex-directory from template is not copied
#'
#' @title Create New Exam Document
#'
#' @description
#' A new exam document is created based on a predefined template that comes
#' with package \code{rteachtools}.
#'
#' @param ps_exam_path path to new exam document
#' @param pl_data list of data for replacement of placeholders
#' @param pb_edit flag whether new document should directly be opened
#' @param pb_force flag whether old version should be overwritten
#'
#' @export create_exam
create_exam <- function(ps_exam_path,
                        pl_data = NULL,
                        pb_edit = FALSE,
                        pb_force = FALSE){

  # get path and directory
  s_exam_path <- ps_exam_path
  # check whether old version exists
  if (fs::file_exists(s_exam_path)){
    if (pb_force){
      fs::file_delete(s_exam_path)
    } else {
      stop(" *** ERROR: FOUND old version of exam: ", s_exam_path)
    }
  }
  s_exam_dir <- dirname(s_exam_path)
  s_exam_file <- basename(s_exam_path)
  # check file extension
  if (tolower(fs::path_ext(s_exam_file)) != "rmd"){
    s_exam_file <- paste(fs::path_ext_remove(s_exam_file), ".Rmd", sep = "")
    s_exam_path <- file.path(s_exam_dir, s_exam_file)
  }
  # name of exam file
  s_exam_name <- fs::path_ext_remove(s_exam_file)

  # get draft from template
  s_tmp_exam_dir <- file.path(tempdir(), s_exam_name)
  rmarkdown::draft(file = s_tmp_exam_dir,
                   template = "cexamdown",
                   package = "rteachtools",
                   edit = FALSE)

  vec_exam_template <- readLines(con = file.path(s_tmp_exam_dir, s_exam_file))
  # clean up
  fs::dir_delete(s_tmp_exam_dir)

  # replace placeholders with data in pl_data
  if (is.null(pl_data)){
    vec_exam_out <- vec_exam_template
  } else {
    vec_exam_out <- whisker::whisker.render(template = vec_exam_template,
                                          data = pl_data)
  }
  # check whether s_exam_dir exists
  if (!dir.exists(s_exam_dir)) dir.create(s_exam_dir, recursive = TRUE)

  # write output to s_ex_path
  cat(paste0(vec_exam_out, collapse = "\n"), "\n", file = s_exam_path)

  # edit document directly
  if (pb_edit) usethis::edit_file(path = s_exam_path)

  return(invisible(TRUE))

}

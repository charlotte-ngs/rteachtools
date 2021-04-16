## -- Exercise Creation Functionality -----------------------------------------
#'
#'
#' @title Create Course Exercise From Template
#'
#' @description
#' The template rttexercise in the package rteachtools is used to create a new
#' exercise. The use of this template-based creation approach assures that the
#' structure of the source document is consistent with the requirements of the
#' deployment tools.
#'
#' @details
#' The function \code{rmarkdown::draft()} is used to convert the template into
#' the target document. Further, the placeholders in the template are replaced
#' by specific values using the function \code{whisker::whisker_render()}.
#'
#' @param ps_ex_path path to output file where exercise should be written to
#' @param pn_nr_problem number of problems in exercise
#' @param pl_data list of replacement data for replacing placeholder
#'
#' @examples
#' \dontrun{
#' create_exercise(ps_ex_path    = "ex/gel_ex01",
#'                 pn_nr_problem = 2,
#'                 pl_data       = list(course_name = "Genetic Evaluation",
#'                                      exercise_count = 1,
#'                                      creation_date = format(Sys.Date(), "%Y-%m-%d"),
#'                                      author = "Peter von Rohr"))
#' }
#' @export create_exercise
create_exercise <- function(ps_ex_path, pn_nr_problem, pl_data){
  # get the file name of the current exercise
  s_ex_path <- ps_ex_path
  s_ex_dir <- dirname(s_ex_path)
  s_ex_file <- basename(ps_ex_path)
  # check whether it ends in .Rmd, otherwise add it
  if (tolower(fs::path_ext(s_ex_file)) != "rmd"){
    s_ex_file <- paste0(fs::path_ext_remove(s_ex_file), ".Rmd")
    s_ex_path <- file.path(s_ex_dir, s_ex_file)
  }
  # get name of exercise without extension
  s_ex_name <- fs::path_ext_remove(s_ex_file)

  # draft from template to tempdir
  s_ex_tmp_path <- file.path(tempdir(), s_ex_name)
  rmarkdown::draft(file = s_ex_tmp_path,
                   template = "rttexercise",
                   package = "rteachtools",
                   edit = FALSE)
  vec_ex_template <- readLines(con = file.path(s_ex_tmp_path, s_ex_file))
  # clean up
  fs::dir_delete(s_ex_tmp_path)

  # if the exercise should contain more than one problem, the template is extended
  if (pn_nr_problem > 1){
    n_problem_start_idx <- grep(pattern = "## Problem", vec_ex_template, fixed = TRUE)
    # check whether problem start was found
    if (length(n_problem_start_idx) != 1L)
      stop(" *** ERROR in create_exercise(): CANNOT find unique Problem-Start line ...")
    n_problem_end_idx <- length(vec_ex_template)
    # determine template for problem section
    vec_problem <- vec_ex_template[n_problem_start_idx:n_problem_end_idx]
    for (pidx in 2:pn_nr_problem){
      vec_ex_template <- c(vec_ex_template, "",vec_problem)
    }
  }
  # insert numbering of problems
  vec_problem_line <- grep(pattern = "## Problem", vec_ex_template, fixed = TRUE)
  if (length(vec_problem_line) != pn_nr_problem)
    stop(" *** ERROR in create_exercise(): Inconsistent number of problems ...")
  for (idx in 1:pn_nr_problem){
    vec_ex_template[vec_problem_line[idx]] <- gsub(pattern = "Problem",
                                                   replacement = paste0("Problem ", idx),
                                                   vec_ex_template[vec_problem_line[idx]],
                                                   fixed = TRUE)
  }

  # replace placeholders with data in pl_data
  vec_ex_out <- whisker::whisker.render(template = vec_ex_template,
                                        data = pl_data)
  # check whether s_ex_dir exists
  if (!dir.exists(s_ex_dir)) dir.create(s_ex_dir, recursive = TRUE)

  # write output to s_ex_path
  cat(paste0(vec_ex_out, collapse = "\n"), "\n", file = s_ex_path)

  return(invisible(TRUE))

}


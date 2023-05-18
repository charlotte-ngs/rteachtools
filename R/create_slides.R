## -- Slide Creation Functionality -----------------------------------------
#'
#'
#' Create New Beamer Slides
#'
#' @description
#' A new document in the format of beamer slides are created. The document
#' is based on a template that can be chosen using a template name and the
#' package where the template is defined. The default is to use the 'cbeamerdown'
#' template of package 'rteachtools'.
#'
#' @details
#' The rmarkdown document is created using the function \code{rmarkdown::draft()}.
#' Then some placeholders are replaced using the function \code{whisker::whisker.render()}.
#'
#'
#' @param ps_sl_path path of the new rmarkdown document
#' @param ps_template template name to be used
#' @param ps_package package that defines the template
#' @param pb_edit should newly created document be directly opened for editing
#' @param pl_data list of place-holder-value pairs for replacement.
#'
#' @export create_slides
#'
#' @examples
#' \dontrun{
#' create_slides(ps_sl_path = "sl/sl_l01_intro/sl_l01_intro.Rmd",
#'               pl_data = list(title = "Introduction to Livestock Breeding and Genomics",
#'                              author = "Peter von Rohr"))
#' }
create_slides <- function(ps_sl_path,
                          ps_template = "cbeamerdown",
                          ps_package = "rteachtools",
                          pb_edit = FALSE,
                          pl_data){
  s_sl_path <- ps_sl_path
  # add .Rmd, if it is not already done
  if (tolower(fs::path_ext(s_sl_path)) != "rmd"){
    s_sl_path <- paste0(fs::path_ext_remove(s_sl_path), ".Rmd")
  }
  # get filename
  s_sl_file <- basename(s_sl_path)
  # check whether parent dir of s_sl_path exists
  s_sl_dir <- dirname(s_sl_path)
  if (!dir.exists(s_sl_dir)) dir.create(path = s_sl_dir, recursive = TRUE)
  # use rmarkdown draft to get to the template
  s_sl_name <- fs::path_ext_remove(s_sl_file)
  # draft from template to tempdir
  s_sl_tmp_dir <- file.path(tempdir(), s_sl_name)
  rmarkdown::draft(file = s_sl_tmp_dir,
                   template = ps_template,
                   package = ps_package,
                   edit = FALSE)
  # read the template into a vector
  vec_sl_tmpl <- readLines(con = file.path(s_sl_tmp_dir, s_sl_file))
  # delete template file, as it is not longer needed
  fs::dir_delete(path = s_sl_tmp_dir)
  # replace placeholders with data in pl_data
  vec_sl_out <- whisker::whisker.render(template = vec_sl_tmpl,
                                        data = pl_data)

  # write content to file
  cat(paste0(vec_sl_out, collapse = "\n"), "\n", file = s_sl_path, append = FALSE)
  # open newly created file
  if (pb_edit) usethis::edit_file(path = s_sl_path)
  return(invisible(TRUE))
}




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
  # use rmarkdown draft to get to the template
  s_sl_tmpl <- paste0(s_sl_path, ".template")
  rmarkdown::draft(file = s_sl_tmpl,
                   template = ps_template,
                   package = ps_package,
                   edit = pb_edit)
  # read the template into a vector
  vec_sl_tmpl <- readLines(s_sl_tmpl)
  # delete template file, as it is not longer needed
  fs::file_delete(path = s_sl_tmpl)
  # replace placeholders with data in pl_data
  vec_sl_out <- whisker::whisker.render(template = vec_sl_tmpl,
                                        data = pl_data)

  # check whether parent dir of s_sl_path exists
  s_sl_dir <- dirname(s_sl_path)
  if (!dir.exists(s_sl_dir)) dir.create(path = s_sl_dir, recursive = TRUE)
  # write content to file
  cat(paste0(vec_sl_out, collapse = "\n"), "\n", file = s_sl_path, append = FALSE)
  return(invisible(TRUE))
}

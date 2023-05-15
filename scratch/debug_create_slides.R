#' ---
#' title: Debug Slide Creation
#' date:  2023-05-15 (pvr)
#' ---
#'
#' ## Disclaimer
#' Debug slide creator function
#'
rm(list = ls())

# arguments
ps_sl_path = "sl/l_11_lasso/l_11_lasso.Rmd"
ps_template = "cbeamerdown"
ps_package = "rteachtools"
pb_edit = FALSE
pl_data = list(title = "Least Absolute Shrinkage and Selection Operator (LASSO)",
               author = "Peter von Rohr")

# first processing from ps_sl_path to s_sl_path
s_sl_path <- ps_sl_path
# add .Rmd, if it is not already done
if (tolower(fs::path_ext(s_sl_path)) != "rmd"){
  s_sl_path <- paste0(fs::path_ext_remove(s_sl_path), ".Rmd")
}
s_sl_path

# create directory, if it does not exist
# check whether parent dir of s_sl_path exists
s_sl_dir <- dirname(s_sl_path)
if (!dir.exists(s_sl_dir)) dir.create(path = s_sl_dir, recursive = TRUE)


# use rmarkdown draft to get to the template
s_sl_tmpl <- paste0(s_sl_path, ".template")
s_sl_tmpl

rmarkdown::draft(file = s_sl_tmpl,
                 template = ps_template,
                 package = ps_package,
                 create_dir = FALSE,
                 edit = pb_edit)

vec_sl_tmpl <- readLines(s_sl_tmpl)
# delete template file, as it is not longer needed
fs::file_delete(path = s_sl_tmpl)
# replace placeholders with data in pl_data
vec_sl_out <- whisker::whisker.render(template = vec_sl_tmpl,
                                      data = pl_data)

# write content to file
cat(paste0(vec_sl_out, collapse = "\n"), "\n", file = s_sl_path, append = FALSE)

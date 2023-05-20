#' ---
#' title: Debug Error of Missing Directories and Files not Copied
#' date:  2023-05-19 (pvr)
#' ---
#'
#'
work_dir <- here::here()
setwd(work_dir)

tbl_dir_content <- tibble::tibble(dir_content = list.files(),
                                  is_dir = fs::is_dir(dir_content))
tbl_dir_content

# the following must be copied as directories
tbl_dir_content$dir_content[tbl_dir_content$is_dir]

# the following must be copied as files
tbl_dir_content$dir_content[!tbl_dir_content$is_dir]


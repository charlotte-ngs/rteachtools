## --- Conversion Functions ---------------------------------------------------
#'
#' @title Convert Exercise To Rmarkdown Notebook
#'
#' @description
#' Based on the input of a Rmarkdown source file containing exercise problems
#' and solutions, a Rmarkdown notebook is created that contains only the
#' exercise problems.
#'
#' @details
#' This function assumes that the solutions are marked with surrounding R-code
#' junks with the tags comment-start and comment-end.
#'
#' @examples
#' \dontrun{
#' convert_ex_to_nb(ps_ex_path = 'ex/asm_ex02.Rmd',
#'                  ps_nb_out_dir = 'nb',
#'                  ps_nb_deploy_dir = 'docs/nb')
#' }
#'
#' @param ps_ex_path path to the Rmarkdown source file
#' @param ps_nb_out_dir output directory for Rmd notebook
#' @param ps_nb_deploy_dir deployment directory of rendered nb
#' @param pn_comment_line_offset number of lines taken by R-code junk to produce md-comment
#' @param pb_force force output even if output file already exists
#' @param pb_debug debugging flag
#' @param pobj_rtt_logger log4r logger object
#'
#' @export convert_ex_to_nb
convert_ex_to_nb <- function(ps_ex_path,
                             ps_nb_out_dir,
                             ps_nb_deploy_dir,
                             pn_comment_line_offset = 2,
                             pb_force        = FALSE,
                             pb_debug        = FALSE,
                             pobj_rtt_logger = NULL){
  # init logging for this function
  if (pb_debug){
    if (is.null(pobj_rtt_logger)){
      rtt_logger <- get_rtt_logger(ps_logfile = 'convert_rtt.log', ps_level = 'INFO')
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
                 ps_caller = 'convert_ex_to_nb',
                 ps_msg = paste0(' * Setting exercise path to: ', s_ex_path, collapse = ''))
  # check whether s_ex_path exists
  if (!file.exists(s_ex_path))
    stop(" *** [convert_ex_to_nb] ERROR: CANNOT FIND exercise source path: ", s_ex_path)

  # determine output path for nb
  s_nb_out_path <- file.path(ps_nb_out_dir, paste(tools::file_path_sans_ext(basename(ps_ex_path)), '_nb.Rmd', sep = ''))
  if (!file.exists(s_nb_out_path) || pb_force){

    # read exercise source to a vector
    con_ex_src <- file(description = s_ex_path, open = 'r')
    vec_ex_src <- readLines(con = con_ex_src)
    close(con = con_ex_src)

    # adapt the yample front matter
    l_yml_fm_ex <- rmarkdown::yaml_front_matter(input = s_ex_path)
    l_yml_fm_nb <- list(title = gsub(pattern = '`r tools::toTitleCase(params$doctype)`',
                                     replacement = 'Notebook', l_yml_fm_ex$title, fixed = TRUE),
                        author = l_yml_fm_ex$author,
                        date = l_yml_fm_ex$date,
                        output = 'html_notebook')

    # get yaml boundaries
    vec_yaml_bound <- grep('---', vec_ex_src, fixed = TRUE)

    # get vector of comment starts and ends
    vec_comment_start <- grep(pattern = "comment-start", vec_ex_src)
    vec_comment_end <- grep(pattern = "comment-end", vec_ex_src)

    # put together the nb source
    vec_nb_src <- c('---',
                    paste('title: ', l_yml_fm_nb$title, sep = ''),
                    paste('author: ', l_yml_fm_nb$author, sep = ''),
                    paste('date: ', l_yml_fm_nb$date, sep = ''),
                    paste('output: ', l_yml_fm_nb$output, sep = ''),
                    '---',
                    vec_ex_src[(vec_yaml_bound[2]+1):(vec_comment_start[1]-1)])
    if (length(vec_comment_start) > 1){
      for (idx in 2:length(vec_comment_start)){
        vec_nb_src <- c(vec_nb_src,
                        vec_ex_src[(vec_comment_end[idx-1]+pn_comment_line_offset):(vec_comment_start[idx]-1)])
      }
    }

    #  add all lines below latest solution section
    if (length(vec_ex_src) > (vec_comment_end[length(vec_comment_end)]+pn_comment_line_offset))
      vec_nb_src <- c(vec_nb_src, vec_ex_src[ (vec_comment_end[length(vec_comment_end)]+pn_comment_line_offset):length(vec_ex_src) ])

    # check whether ps_nb_out_dir exists, if not create it
    if (!dir.exists(ps_nb_out_dir)){
      if (pb_debug)
        rtt_log_info(plogger = rtt_logger,
                     ps_caller = 'convert_ex_to_nb',
                     ps_msg = paste0(' * Cannot find nb_outdir: ', ps_nb_out_dir, ' ==> create it', collapse = ''))
      dir.create(ps_nb_out_dir, recursive = TRUE)
    }
    # write modified exercise to nb out path
    cat(paste0(vec_nb_src, collapse = '\n'), '\n', file = s_nb_out_path)
  }
  # render to deployment directory
  if (!dir.exists(ps_nb_deploy_dir)) dir.create(ps_nb_deploy_dir)
  rmarkdown::render(input = s_nb_out_path, output_dir = ps_nb_deploy_dir)

  return(invisible(TRUE))



}


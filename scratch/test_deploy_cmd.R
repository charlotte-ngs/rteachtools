

s_uni_src_path <- system.file("extdata", "asm_ex06.Rmd", package = "rteachtools")
s_ex_src_name <- tools::file_path_sans_ext(basename(s_uni_src_path))
rteachtools::deploy_src_to_ex_sol(ps_uni_src_path = s_uni_src_path,
                     ps_ex_out_dir = file.path(here::here(), "docs","ex"),
                     ps_sol_out_dir = file.path(here::here(), "docs","sol"),
                     ps_nb_src_dir = file.path(here::here(), "nb", s_ex_src_name),
                     ps_nb_out_dir = file.path(here::here(), "docs", "nb"))




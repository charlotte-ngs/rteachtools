## -- Seed Functionality -----------------------------------------
#'
#' @title Create a Uniformly Distributed Random Seed
#'
#' @description
#' Random uniform seed of four digits is created using the
#' uniform random number
#' @param pn_min lower bound of seed
#' @param pn_max upper bound of seed
#' @param pn_seed pre-defined seed for debugging
#'
#' @export get_seed
get_seed <- function(pn_min = 1000L,
                     pn_max = 9999L,
                     pn_seed = NULL){
  # for debugging make it possible to re-generate the same seed
  if (!is.null(pn_seed)) set.seed(seed = pn_seed)
  # get random uniform between min and max
  n_result_seed <- runif(1,min = pn_min, max = pn_max)
  # take floor and convert to integer and return
  return(as.integer(floor(n_result_seed)))
}

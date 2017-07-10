
#' Initiate master problem
#' 
#' \code{initiate_master} copy the AMPL files into the temporary 
#' folder of the expansion planning optimisation and create the other input
#' and output file of the master problem
#' 
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param exp_options 
#'   list of options related to the expansion planning, as returned
#'   by the function \code{\link{read_options}}
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return This function does not return anything.
#' 
#' @importFrom antaresRead simOptions
#' @importFrom assertthat assert_that
#' 
initiate_master <- function(candidates = read_candidates(opts), exp_options = read_options(opts), opts = antaresRead::simOptions())
{
  # ampl file names (stored in inst folder)
  run_file <- "ampl/master_run.ampl"
  mod_file <- "ampl/master_mod.ampl"
  dat_file <- "ampl/master_dat.ampl"
  
  # master input/output files (interface with AMPL is ensured with .txt files)
  in_out_files <- list()
  in_out_files$mc <- "in_mc.txt"
  in_out_files$w <- "in_week.txt"
  in_out_files$candidates <- "in_candidates.txt"
  in_out_files$iterations  <- "in_iterations.txt"
  in_out_files$z0 <- "in_z0.txt"
  in_out_files$avg_rentability <- "in_avgrentability.txt"
  in_out_files$yearly_rentability <- "in_yearlyrentability.txt"
  in_out_files$weekly_rentability <- "in_weeklyrentability.txt"
  in_out_files$avg_cuts <- "in_avgcuts.txt"
  in_out_files$yearly_cuts <- "in_yearlycuts.txt"
  in_out_files$weekly_cuts <- "in_weeklycuts.txt"
  in_out_files$options <- "in_options.txt"
  in_out_files$sol_master <- "out_solutionmaster.txt"
  in_out_files$underestimator <- "out_underestimator.txt"
  in_out_files$log <- "out_log.txt"
  in_out_files$dual_averagecut <- "out_dualaveragecut.txt"
  in_out_files$dual_yearlycut <- "out_dualyearlycut.txt"
  in_out_files$dual_weeklycut <- "out_dualweeklycut.txt"
  in_out_files$theta <- "out_theta.txt"
  
  
  
  # check if temporary folder exists, if not create it
  tmp_folder <- paste0(opts$studyPath,"/user/expansion/temp")
  if(!dir.exists(tmp_folder))
  {
    dir.create(tmp_folder)
  }
  
  # copy AMPL files into the temporary folder
  run_file <- system.file(run_file, package = "antaresXpansion")
  mod_file <- system.file(mod_file, package = "antaresXpansion")
  dat_file <- system.file(dat_file, package = "antaresXpansion")
  run_path_file <- system.file(run_file, package = "antaresXpansion")
  mod_path_file <- system.file(mod_file, package = "antaresXpansion")
  dat_path_file <- system.file(dat_file, package = "antaresXpansion")
  
  
  assertthat::assert_that(file.copy(from = run_file, to = tmp_folder, overwrite = TRUE))
  assertthat::assert_that(file.copy(from = mod_file, to = tmp_folder, overwrite = TRUE))
  assertthat::assert_that(file.copy(from = dat_file, to = tmp_folder, overwrite = TRUE))
  
  # create empty in_out files
  lapply(in_out_files, FUN = function(x, folder){file.create(paste0(folder, "/", x))}, folder = tmp_folder)
  
  # fill files which will be similar for every iteration of the benders decomposition
  # 1 - in_nmc.txt
  mc <- get_playlist(opts)
  write(paste0(mc, collapse = " "), file = paste0(tmp_folder, "/", in_out_files$mc))
  
  # 2 - in_nw.txt
  n_w <- floor((opts$parameters$general$simulation.end - opts$parameters$general$simulation.start + 1)/7)
  n_w <- min(n_w, 52)
  #     number of week does not necessarily starts at 1
  #     it depends on the beginning of the simulated period (opts$parameters$general$simulation.start)
  weeks <- (ceiling((opts$parameters$general$simulation.start - 1) /7) + 1) : (ceiling((opts$parameters$general$simulation.start - 1) /7) + n_w) 
  
  write(paste0(weeks, collapse = " "), file = paste0(tmp_folder, "/", in_out_files$w))
  
  # 3 - in_candidates.txt
  script <- ""
  for(i in 1:length(candidates))
  {
    script <- paste0(script, candidates[[i]]$name, " ", 
                     candidates[[i]]$cost, " ", 
                     candidates[[i]]$unit_size, " ", 
                     candidates[[i]]$max_unit, " ",
                     tolower(as.character(candidates[[i]]$relaxed)))
    if(i != length(candidates))
    {
      script <- paste0(script, "\n")
    }
  }
  write(script, file = paste0(tmp_folder, "/", in_out_files$candidates))
  
  # 4 - in_options.txt (has been shifted to function solve)
  #if(exp_options$master == "relaxed")
  #{
  #  write("option relax_integrality 1;", file = paste0(tmp_folder, "/", in_out_files$options))
  #}
}


initiate_master_path <- function(candidates, exp_options = read_options(opts), opts,directory_path)
{
  # ampl file names (stored in inst folder)
  run_path_file <- "ampl/master_path_run.ampl"
  mod_path_file <- "ampl/master_path_mod.ampl"
  dat_path_file <- "ampl/master_path_dat.ampl"
  
  # master input/output files (interface with AMPL is ensured with .txt files)
  in_out_files <- list()
  in_out_files$mc <- "in_mc.txt"
  in_out_files$w <- "in_week.txt"
  in_out_files$candidates <- "in_candidates.txt"
  in_out_files$iterations  <- "in_iterations.txt"
  in_out_files$z0 <- "in_z0.txt"
  in_out_files$avg_rentability <- "in_avgrentability.txt"
  in_out_files$yearly_rentability <- "in_yearlyrentability.txt"
  in_out_files$weekly_rentability <- "in_weeklyrentability.txt"
  in_out_files$avg_cuts <- "in_avgcuts.txt"
  in_out_files$yearly_cuts <- "in_yearlycuts.txt"
  in_out_files$weekly_cuts <- "in_weeklycuts.txt"
  in_out_files$options <- "in_options.txt"
  in_out_files$sol_master <- "out_solutionmaster.txt"
  in_out_files$underestimator <- "out_underestimator.txt"
  in_out_files$log <- "out_log.txt"
  in_out_files$dual_averagecut <- "out_dualaveragecut.txt"
  in_out_files$dual_yearlycut <- "out_dualyearlycut.txt"
  in_out_files$dual_weeklycut <- "out_dualweeklycut.txt"
  in_out_files$theta <- "out_theta.txt"
  
  
  
  # check if temporary folder exists, if not create it
  tmp_folder <- paste0(directory_path,"/temp")
  if(!dir.exists(tmp_folder))
  {
    dir.create(tmp_folder)
  }
  
  # copy AMPL files into the temporary folder
  run_path_file <- system.file(run_path_file, package = "antaresXpansion")
  mod_path_file <- system.file(mod_path_file, package = "antaresXpansion")
  dat_path_file <- system.file(dat_path_file, package = "antaresXpansion")
  
  
  assertthat::assert_that(file.copy(from = run_path_file, to = tmp_folder, overwrite = TRUE))
  assertthat::assert_that(file.copy(from = mod_path_file, to = tmp_folder, overwrite = TRUE))
  assertthat::assert_that(file.copy(from = dat_path_file, to = tmp_folder, overwrite = TRUE))
  
  # create empty in_out files
  lapply(in_out_files, FUN = function(x, folder){file.create(paste0(folder, "/", x))}, folder = tmp_folder)
  
  # fill files which will be similar for every iteration of the benders decomposition
  # 1 - in_nmc.txt
  mc <- get_playlist(opts)
  write(paste0(mc, collapse = " "), file = paste0(tmp_folder, "/", in_out_files$mc))
  
  # 2 - in_nw.txt
  n_w <- floor((opts$parameters$general$simulation.end - opts$parameters$general$simulation.start + 1)/7)
  n_w <- min(n_w, 52)
  #     number of week does not necessarily starts at 1
  #     it depends on the beginning of the simulated period (opts$parameters$general$simulation.start)
  weeks <- (ceiling((opts$parameters$general$simulation.start - 1) /7) + 1) : (ceiling((opts$parameters$general$simulation.start - 1) /7) + n_w) 
  
  write(paste0(weeks, collapse = " "), file = paste0(tmp_folder, "/", in_out_files$w))
  
  # 3 - in_candidates.txt
  script <- ""
  for(i in 1:length(candidates))
  {
    script <- paste0(script, candidates[[i]]$name, " ", 
                     candidates[[i]]$cost, " ", 
                     candidates[[i]]$unit_size, " ", 
                     candidates[[i]]$max_unit, " ",
                     tolower(as.character(candidates[[i]]$relaxed)))
    if(i != length(candidates))
    {
      script <- paste0(script, "\n")
    }
  }
  write(script, file = paste0(tmp_folder, "/", in_out_files$candidates))
  
  # 4 - in_options.txt (has been shifted to function solve)
  #if(exp_options$master == "relaxed")
  #{
  #  write("option relax_integrality 1;", file = paste0(tmp_folder, "/", in_out_files$options))
  #}
}

#' Solver master problem
#' 
#' \code{solver_master} execute the AMPL file master_run.ampl
#' located in the temporary folder of the current expansion 
#' planning optimisation
#' 
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param relax_integrality
#'   logical, indicating whether (TRUE) or not (FALSE) the integer variables
#'   should be relaxed
#' @return This function does not return anything.
#' 
#' @importFrom antaresRead simOptions
#' @importFrom assertthat assert_that
#' 
solve_master <- function(opts = antaresRead::simOptions(), relax_integrality = FALSE)
{
  tmp_folder <- paste(opts$studyPath,"/user/expansion/temp",sep="")
  
  if(relax_integrality)
  {
    write("option relax_integrality 1;", file = paste0(tmp_folder, "/in_options.txt"))
  }
  else
  {
    write("option relax_integrality 0;", file = paste0(tmp_folder, "/in_options.txt"))
  }

  
  assertthat::assert_that(file.exists(paste0(tmp_folder, "/master_run.ampl")))
  assertthat::assert_that(file.exists(paste0(tmp_folder, "/master_mod.ampl")))
  assertthat::assert_that(file.exists(paste0(tmp_folder, "/master_dat.ampl")))
  
  cmd <- paste0('', substr(tmp_folder, 1, 2), ' & cd "', tmp_folder, '" & ampl "', tmp_folder, '/master_run.ampl" ')

  a <- shell(cmd, wait = TRUE, intern = TRUE)
  #cat(a)
}
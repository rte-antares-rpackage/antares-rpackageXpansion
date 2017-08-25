#' Launch benders decomposition
#' 
#' 
#' @param path_solver
#'   Character containing the Antares Solver path
#' @param display
#'   Logical. If \code{TRUE} the advancement of the benders decomposition
#'   if displayed in the console
#' @param report
#'   Logical. If \code{TRUE} an html report of the expansion results will
#'   be generated.
#' @param clean
#'   Logical. If \code{TRUE} the output of the ANTARES simulations run by the
#'   package will be deleted (except for the output of the simulation which brings
#'   to the best solution).
#' @param parallel
#'   Logical. If \code{TRUE} the ANTARES simulations will be run in parallel mode (Work
#'   only with ANTARES v6.0.0 or more). In that case, the number of cores used by the simulation
#'   is the one set in advanced_settings/simulation_cores (see ANTARES interface).
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return 
#' 
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions readAntares setSimulationPath getAreas
#' @importFrom rmarkdown render
#' @importFrom utils packageVersion
#' @export
#' 
benders <- function(path_solver, display = TRUE, report = TRUE, clean = TRUE, parallel = TRUE, opts = antaresRead::simOptions())
{
  # ---- 0. initialize benders iteration ----

  # save current settings of the ANTARES study into a temporary file
  assertthat::assert_that(file.exists(paste0(opts$studyPath, "/settings/generaldata.ini")))
  assertthat::assert_that(file.copy(from = paste0(opts$studyPath, "/settings/generaldata.ini"), 
            to = paste0(opts$studyPath, "/settings/generaldata_tmpsvg.ini"),
            overwrite = TRUE))

  # read expansion planning options
  option_file_name <- paste0(opts$studyPath,"/user/expansion/settings.ini")
  exp_options <- read_options(option_file_name)
  
  # read investment candidates file
  candidates_file_name <- paste0(opts$studyPath,"/user/expansion/candidates.ini")
  candidates <- read_candidates(candidates_file_name,opts)
  n_candidates <- length(candidates)
  assertthat::assert_that(n_candidates > 0)
  
  # if all investments are distributed (no integer variables), relax master problem
  if(all(sapply(candidates, FUN = function(c){return(c$relaxed)})))
  {
    exp_options$master <- "relaxed"
  }
  
  # set ANTARES study options
  set_antares_options(exp_options, candidates, opts)
  
  # check that the study is appropriately set for the expansion problem
  assertthat::assert_that(benders_check(opts))
  
  # initiate text files to communicate with master problem
  # and copy AMPL file into the temporary file 
  initiate_master(candidates, exp_options, opts)
  
  # initiate a few parameters
  first_sim_week <- 1 + ceiling((opts$parameters$general$simulation.start - 1)/7)
  n_w <- floor((opts$parameters$general$simulation.end - opts$parameters$general$simulation.start + 1)/7) # number of weeks 
  weeks <- first_sim_week:(first_sim_week + n_w - 1) # identifier of weeks to simulate for all expansion planning optimisation
  mc_years <- get_playlist(opts) # identifier of mc years to simulate for all expansion planning optimisation
  n_mc <- length(mc_years) # number of mc_years
  has_converged <- FALSE # has the benders decomposition converged ? not yet
  best_solution <- NA  # best solution identifier
  tmp_folder <- paste(opts$studyPath,"/user/expansion/temp",sep="")   # temporary folder
  relax_integrality <- exp_options$master %in% c("relaxed", "integer")
  unique_key <- paste(sample(c(0:9, letters), size = 3, replace = TRUE),collapse = "")
  all_areas <- antaresRead::getAreas(opts = opts)
  
  # create output structure 
  x <- list()
  x$invested_capacities <- data.frame()
  x$overall_costs <- numeric()
  x$investment_costs <- numeric()
  x$operation_costs <- numeric()
  x$rentability <- data.frame()
  x$iterations <- list()
  x$digest <- list()
  x$digest$lole <- data.frame()
  
  # create iteration structure
  current_it <- list()
  current_it$n <- 1  # iteration number
  current_it$id <- "it1"  # iteration identifier
  current_it$full <- TRUE  # is it an iteration in which we simulate all weeks and all MC years ?
  current_it$mc_years <- mc_years # identidier of mc years to simulate at this current iteration
  current_it$weeks <- weeks # identidier of weeks to simulate at this current iteration
  current_it$cut_type <- exp_options$cut_type # type of cut for this iteration (average, weekly, yearly)
  current_it$need_full <- FALSE # is a complete iteration needed for next step ?
  current_it$last_full <- 1 # last iteration with full simulation
  
  # prepare cuts tables
  # cuts <- list()
  # cuts$avg <- data.table(name = character(), candidate = character(), lambda = double())
  # cuts$yearly <- data.table(name = character(), mc_year = integer(), candidate = character(), lambda = double())
  # cuts$weekly <- data.table(name = character(), mc_year = integer(), week = integer(), candidate = character(), lambda = double())
  # cuts$avg_cost <- data.table(name = character(), cost = double())
  # cuts$yearly_cost <- data.table(name = character(), mc_year = integer(), cost = double())
  # cuts$weekly_cost <- data.table(name = character(), mc_year = integer(), week = integer(), cost = double())
  # 

  # set initial value to each investment candidate 
  # (here put to closest multiple of unit-size below max_invest/2)
  x$invested_capacities <- data.frame( it1 = sapply(candidates, FUN = function(c){
    if(c$unit_size > 0)
    {
      out <- floor(c$max_invest/2/c$unit_size) * c$unit_size
      out <- max(0, min(c$max_invest, out))
    }
    else
    { out <- c$max_invest/2}
    #return(out)}))
  return(0)}))
  row.names(x$invested_capacities) <- sapply(candidates, FUN = function(c){c$name})
  
  
  
  # ----
  # iterate until convergence or until the max number of iteration has been reached
  while(!has_converged && current_it$n <= exp_options$max_iteration)
  {
    # ---- 0. Initiate iteration ----
    
    # not much to do here
    
    current_it$id <- paste0("it", current_it$n)
    
    # ---- 1. Select weeks to simulate at this iteration ----
    
    # a smart selection of weeks is performed at each iteration in order to
    # accelerate computation time by simulating only the weeks whose cuts are
    # more likely to be activated in the master problem
   
    current_it <- week_selection(current_it, mc_years, weeks, tmp_folder, exp_options)
    
    # set simulation period
    set_simulation_period(current_it$weeks, opts)
    # set playlist
    set_playlist(current_it$mc_years, opts)
    
    
    if(current_it$full & display){
      cat("--- ITERATION ", current_it$n, " (complete iteration, ", n_w * n_mc, " simulated weeks) ---\n", sep="")
    }
    if(!current_it$full & display){
      cat("--- ITERATION ", current_it$n, " (partial iteration, ", length(current_it$mc_years) * length(current_it$weeks), " simulated weeks) ---\n", sep="")
    }
    
    
    # ---- 2. Set installed capacities ---- 
    
    # update study with current invested capacities on links
    
    for(c in candidates)
    {
      update_link(c$link, "direct_capacity", c$link_profile*x$invested_capacities[c$name, current_it$id ] , opts)
      update_link(c$link, "indirect_capacity", c$link_profile*x$invested_capacities[c$name, current_it$id ], opts)
    }
    
    
    
    
    # ---- 3. Simulate ---- 
    
    # run the ANTARES simulation, load the path related to this
    # simulation and read the outputs
    
    simulation_name <- paste0("expansion-benders-", unique_key, "-", current_it$id)
    if(display){  cat("   ANTARES simulation running ... ", sep="")}
    run_simulation(simulation_name, mode = "economy", path_solver, wait = TRUE, show_output_on_console = FALSE, parallel = parallel, opts)
    if(display){  cat("[done] \n", sep="")}
    
    output_antares <- antaresRead::setSimulationPath(paste0(opts$studyPath, "/output/", get_whole_simulation_name(simulation_name, opts)))
    
    # read output of the simulation, for links and areas, 
    # with synthetic visions and detailed annual and weekly results
    # to avoid the sum of numeric approximations, it is advised to use the most aggregated output of ANTARES
    # (e.g. to use annual results of ANTARES instead of the sum of the weekly results)

    if(utils::packageVersion("antaresRead") > "0.14.9" )
    {
      # hourly results
      if (length(with_profile(candidates)) > 0 )
      {
        output_link_h = readAntares(areas = NULL, links = with_profile(candidates), mcYears = current_it$mc_years, 
                                    timeStep = "hourly", opts = output_antares, showProgress = FALSE)
        output_link_h_s = readAntares(areas = NULL, links = with_profile(candidates), mcYears = NULL, 
                                    timeStep = "hourly", opts = output_antares, showProgress = FALSE)
      }
      # weekly results
      output_area_w = antaresRead::readAntares(areas = "all", links = NULL, mcYears = current_it$mc_years, 
                                  timeStep = "weekly", opts = output_antares, showProgress = FALSE)
      output_link_w = antaresRead::readAntares(areas = NULL, links = "all", mcYears = current_it$mc_years, 
                                  timeStep = "weekly", opts = output_antares, showProgress = FALSE)
      
      # yearly results
      output_area_y = antaresRead::readAntares(areas = "all", links = NULL, mcYears = current_it$mc_years, 
                                  timeStep = "annual", opts = output_antares, showProgress = FALSE)
      output_link_y = antaresRead::readAntares(areas = NULL, links = "all", mcYears = current_it$mc_years, 
                                  timeStep = "annual", opts = output_antares, showProgress = FALSE)
      
      # synthetic results
      output_area_s = antaresRead::readAntares(areas = "all", links = NULL, mcYears = NULL, 
                                  timeStep = "annual", opts = output_antares, showProgress = FALSE)
      output_link_s = antaresRead::readAntares(areas = NULL, links = "all", mcYears = NULL, 
                                  timeStep = "annual", opts = output_antares, showProgress = FALSE)
    }
    else  # old package version with synthesis arguments
    {
      # hourly results
      output_link_h = readAntares(areas = NULL, links = "all", synthesis = FALSE, 
                                  timeStep = "hourly", opts = output_antares, showProgress = FALSE)
      
      # weekly results
      output_area_w = antaresRead::readAntares(areas = "all", links = NULL, synthesis = FALSE, 
                                  timeStep = "weekly", opts = output_antares, showProgress = FALSE)
      output_link_w = antaresRead::readAntares(areas = NULL, links = "all", synthesis = FALSE, 
                                  timeStep = "weekly", opts = output_antares, showProgress = FALSE)
      
      # yearly results
      output_area_y = antaresRead::readAntares(areas = "all", links = NULL, synthesis = TRUE,
                                  timeStep = "annual", opts = output_antares, showProgress = FALSE)
      output_link_y = antaresRead::readAntares(areas = NULL, links = "all", synthesis = TRUE,
                                  timeStep = "annual", opts = output_antares, showProgress = FALSE)
      
      # synthetic results
      output_area_s = antaresRead::readAntares(areas = "all", links = NULL, synthesis = TRUE,
                                  timeStep = "annual", opts = output_antares, showProgress = FALSE)
      output_link_s = antaresRead::readAntares(areas = NULL, links = "all", synthesis = TRUE,
                                  timeStep = "annual", opts = output_antares, showProgress = FALSE)
    }
    

    # ---- 4. Assess system costs and marginal rentability of each investment candidate ---- 
    
    # analyse some outputs of the just finished ANTARES simulation
    
    
    # compute system costs (can only be assessed if a complete
    # simulation - with all weeks and all mc - has been run)
    if(current_it$full)
    {
      if (exp_options$uc_type == "relaxed_fast")
      {
        # in that case, non-linear cost has to be removed because they are computed in a post-processing and are not
        # part of the ANTARES optimization
        op_cost <-  sum(as.numeric(output_area_s$"OV. COST"))  + sum(as.numeric(output_link_s$"HURDLE COST")) -
                    sum(as.numeric(output_area_s$"NP COST"))
      }
      else
      {
        op_cost <-  sum(as.numeric(output_area_s$"OV. COST"))  + sum(as.numeric(output_link_s$"HURDLE COST")) 
      }
      inv_cost <- sum(sapply(candidates, FUN = function(c){c$cost * x$invested_capacities[c$name, current_it$id]}))
      inv_cost <- inv_cost * n_w / 52 # adjusted to the period of the simulation
      ov_cost <-  op_cost + inv_cost
    }
    else
    {
      op_cost <- NA
      inv_cost <- sum(sapply(candidates, FUN = function(c){c$cost * x$invested_capacities[c$name, current_it$id]}))
      ov_cost <- NA
    }
    
    # update output structure
    x$investment_costs<- c(x$investment_costs, inv_cost)
    x$operation_costs <- c(x$operation_costs, op_cost)
    x$overall_costs <- c(x$overall_costs, ov_cost)
    
    if(current_it$full)
    {
      # check if the current iteration provides the best solution
      if(ov_cost <= min(x$overall_costs, na.rm = TRUE)) {best_solution = current_it$n}
    }

    # compute average rentability of each candidate (can only
    # be assessed if a complete simulation has been run)
    # + compute LOLE for each area
    if(current_it$full)
    {
      get_avg_rentability <- function(c)
      {
        if(c$has_link_profile)
        {
          return(sum(as.numeric(subset(output_link_h_s, link == c$link)$"MARG. COST")*c$link_profile) - c$cost * n_w / 52) 
        }
        else 
        {
          return(sum(as.numeric(subset(output_link_s, link == c$link)$"MARG. COST")) - c$cost * n_w / 52)
        }
      }
      average_rentability <- sapply(candidates, FUN = get_avg_rentability)
      lole <- sapply(all_areas, FUN = function(a){as.numeric(subset(output_area_s, area == a)$"LOLD")}) 
    }
    else
    {
      average_rentability <- rep(NA, n_candidates)
      lole <- rep(NA, length(all_areas))
    }
    
    # update output structure
    if(current_it$n == 1)
    {
      x$rentability <- data.frame(it1 = average_rentability)
      row.names(x$rentability) <- sapply(candidates, FUN = function(c){c$name})
      x$digest$lole <- data.frame(it1 = lole)
      row.names(x$digest$lole) <- all_areas
    }
    else 
    {
      x$rentability[[current_it$id]] <- average_rentability
      x$digest$lole[[current_it$id]] <- lole
    }
  
    
    
    # print results of the ANTARES simulation
    if(display & current_it$full)
    {
      for (c in candidates){cat( "     . ", c$name, " -- ", x$invested_capacities[c$name, current_it$id], " invested MW -- rentability = ", round(x$rentability[c$name, current_it$id]/1000), "ke/MW \n" , sep="")}
      cat("--- op.cost = ", op_cost/1000000, " Me --- inv.cost = ", inv_cost/1000000, " Me --- ov.cost = ", ov_cost/1000000, " Me ---\n")
    }
    
    
    
    
    # ---- 5. Update cuts ---- 
    
    # update cuts of the benders master problem, based on the marginal
    # rentability of each investment candidates and on the obtained system
    # costs
    # cuts can be averaged on all MC years, yearly or weekly
    
    
    # update iteration file
    write(current_it$id, file = paste0(tmp_folder, "/in_iterations.txt"), append = TRUE )  
    
    # write current invested capacity in in_z0.txt
    script <-  ""
    for (c in 1:n_candidates)
    {
      script <- paste0(script, current_it$id, " ", candidates[[c]]$name, " ", x$invested_capacities[candidates[[c]]$name, current_it$id])
      if (c != n_candidates) {script <- paste0(script, "\n")}
    }
    write(script, file = paste0(tmp_folder, "/in_z0.txt"), append = TRUE )  
    
    # write costs and cuts files 
    if(current_it$cut_type == "average")
    {
      assert_that(current_it$full)
      update_average_cuts(current_it, candidates, output_link_s, output_link_h_s, ov_cost, n_w, tmp_folder, exp_options)
    }
    if(current_it$cut_type == "yearly")
    {
      assert_that(all(current_it$weeks == weeks))
      update_yearly_cuts(current_it,candidates, output_area_y, output_link_y, output_link_h, inv_cost, n_w, tmp_folder, exp_options)
    }
    if(current_it$cut_type == "weekly")
    {
      update_weekly_cuts(current_it, candidates, output_area_w, output_link_w, output_link_h, inv_cost, tmp_folder, exp_options)
    }
    
    
    
    # ---- 6. Solve master problem ---- 
    
    # solve master optimisation problem (using AMPL) and read results of
    # this problem
    
    # if option "integer" has been chosen, should the integrality be added ?
    if(exp_options$master == "integer" && current_it$n > 1 && relax_integrality)
    {
      if(convergence_relaxed(best_sol = min(x$overall_costs, na.rm = TRUE), best_under_estimator, exp_options))
      {
        relax_integrality <- FALSE
        # reintialize ov.cost and op.costs (which are not admissible because computed with relaxed investments decisions)
        x$operation_costs <- rep(NA, current_it$n)
        x$overall_costs <- rep(NA, current_it$n)
        current_it$need_full <- TRUE
        
        if (display){cat("--- ADDITION of INTEGER variables into investment decisions --- \n")}
      }
    }
    
    # run AMPL with system command
    log <- solve_master(opts, relax_integrality)
    
    # load AMPL output
    #     - underestimator
    x$under_estimator  <-  unname(unlist(read.table(paste0(tmp_folder,"/out_underestimator.txt"), header = FALSE)))
    best_under_estimator <-  max(x$under_estimator)
    
    #    - investment solution
    benders_sol <-  read.table(paste0(tmp_folder,"/out_solutionmaster.txt"), sep =";")[,2]
   
    if(display)
    {
      cat("--- lower bound on ov.cost = ", best_under_estimator/1000000 ," Me --- best solution (it ", best_solution, ") = ", x$overall_costs[best_solution]/1000000   ,"Me \n")
    }
 
    
    # ---- 7. Check convergence ---- 
    
    # check convergence of the benders decomposition
    
    # if difference between the under estimator and the best solution
    # is lower than the optimality gap, then the convergence has been reached
    if(!all(is.na(x$overall_costs)))
    {
      if(convergence(best_sol = min(x$overall_costs, na.rm = TRUE), best_under_estimator, exp_options)) 
      {
        has_converged <- TRUE
      }
      
      # if master problem solution didn't evolve at this (full) iteration, then the decomposition has
      # converged
      
      if(all(abs(benders_sol - x$invested_capacities[[current_it$id]]) <= 0.1) )
      {
        if(current_it$full)
        { 
          has_converged <- TRUE  
        }
        else
        {
          current_it$need_full <- TRUE
        }
      }
    }  
    
    # if option integer has been chosen and integer has not yet been used, convergence cannot be reached
    if(exp_options$master == "integer" && relax_integrality)
    {
      has_converged <- FALSE
    }
    
    
    # display end messages
    if(has_converged & display)
    { 
        cat("--- CONVERGENCE within optimality gap: best solution = it ", best_solution, " --- ov.cost = ", min(x$overall_costs, na.rm = TRUE)/1000000 ," Me --- Best Lower Bound = ",best_under_estimator/1000000 , " Me \n")
    }
    if(display & current_it$n >= exp_options$max_iteration)
    { 
      cat("--- END, the maximum number of iteration (", exp_options$max_iteration, ") has been reached \n", sep ="")
    }
    
    # go to next iteration
    x$iterations[[current_it$n]] <- current_it
    current_it$n = current_it$n +1
    
    
    # ---- 8. Update investment decisions ---- 
    
    # update investment decision to prepare next iteration
    
    if(!has_converged && current_it$n <= exp_options$max_iteration)
    {
        x$invested_capacities[[paste0("it", current_it$n)]] <- benders_sol
    }
    
    
    # ---- 9. Clean ANTARES output ----
    if(clean) { clean_output_benders(best_solution, unique_key, opts)}

  }
  
  
  
  # add information in the output file
  option_file_name_2 <- paste0(opts$studyPath,"/user/expansion/settings.ini")
  x$expansion_options <- read_options(option_file_name_2)
  x$study_options <- opts
  x$candidates <- read_candidates(candidates_file_name,opts)
  
  # reset options of the ANTARES study to their initial values
  assertthat::assert_that(file.remove(paste0(opts$studyPath, "/settings/generaldata.ini")))
  assertthat::assert_that(file.rename(from = paste0(opts$studyPath, "/settings/generaldata_tmpsvg.ini"), 
                                    to = paste0(opts$studyPath, "/settings/generaldata.ini")))


  # set link capacities to their optimal value
  for(c in candidates)
  {
    update_link(c$link, "direct_capacity", x$invested_capacities[c$name, paste0("it", best_solution)] , opts)
    update_link(c$link, "indirect_capacity", x$invested_capacities[c$name, paste0("it", best_solution)], opts)
  }
  
  
  # save output file
  # copy the benders_out into a Rdata in the temporary folder
  tmp_folder <- paste(opts$studyPath,"/user/expansion/temp",sep="")
  if(!dir.exists(tmp_folder))
  {
    dir.create(tmp_folder)
  }
  
  saveRDS(x, file = paste0(tmp_folder, "/data_for_report.RDS"))
  
  # create report
  if(report)
  {
    if(display)
    {
      cat("Write report in user/expansion/report directory \n")
    }
    
    rmarkdown::render(input = system.file("rmd/report.Rmd", package = "antaresXpansion"), 
                      output_file = default_report_file(opts), params = x, quiet = TRUE)
  }
}
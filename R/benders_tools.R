#' Set ANTARES study options related to the current benders decomposition
#' 
#' 
#' @param benders_options
#'   list of benders decomposition options, as returned by
#'   \code{\link{read_options}}.
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return nothing
#' 
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions getLinks getAreas
#' 
set_antares_options <- function(benders_options, candidates, opts = antaresRead::simOptions())
{
  # 1 - set output filtering options
  enable_custom_filtering(TRUE, opts)
  enable_year_by_year(TRUE, opts)
  filter_output_areas(areas = antaresRead::getAreas(opts = opts), filter = c("weekly", "annual"), type = c("year-by-year", "synthesis"), opts = opts)
  filter_output_links(links = antaresRead::getLinks(opts = opts), filter = c("weekly", "annual"), type = c("year-by-year", "synthesis"), opts = opts)
  if (length(with_profile(candidates)) > 0 )
  {
    filter_output_links(links = with_profile(candidates), filter = c("hourly", "weekly", "annual"), type = c("year-by-year", "synthesis"), opts = opts)
  }
  # 2 - set unit-commitment mode
  if(benders_options$uc_type == "accurate")
  {
    set_uc_mode(mode = "accurate", opts = opts)
    enable_uc_heuristic(enable = TRUE, opts = opts)
  }
  if(benders_options$uc_type == "fast")
  {
    set_uc_mode(mode = "fast", opts = opts)
    enable_uc_heuristic(enable = TRUE, opts = opts)
  }
  if(benders_options$uc_type == "relaxed_fast")
  {
    set_uc_mode(mode = "fast", opts = opts)
    enable_uc_heuristic(enable = FALSE, opts = opts)
  }
  
  # 3 - set week and initial day
  #    we need to ensure the consistency between the weekly optimisation and the weekly
  #    aggregation of the output
  
  month_name <- c("january", "december", "november", "october", "september", "august", "july", "june", "may", "april", "march", "february")
  day_per_month <- c(0, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 28)
  
  day_name <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  if (opts$parameters$general$leapyear)
  {
    day_per_month[12] <- 29
  }
  month_id <- which(month_name == opts$parameters$general$"first-month-in-year")
  assertthat::assert_that(length(month_id) == 1)
  n_day <- (-sum(day_per_month[1:month_id]) + opts$parameters$general$simulation.start - 1) %% 7
  
  first_day_week <- day_name[((which(day_name == opts$parameters$general$january.1st) + n_day - 1) %% 7 ) +1]
  set_week(first_day = first_day_week, opts)
}


#' Update costs and rentability files of the average cuts
#' Create files in_avgrentability.txt and in_avgcuts.txt which
#' will be later read by AMPL as input of the master problem
#' 
#' 
#' @param current_it
#'   list of current iteration characteristics
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param output_link_s
#'   antaresDataList of all the links of the study with a yearly time step
#'   and averaged on all MC years
#' @param output_link_h_s
#'   antaresDataList of all the links of the study with an hourly time step
#'   and averaged on all MC years
#' @param ov_cost
#'   overall costs of this iteration
#' @param n_w
#'   number of weeks per year
#' @param tmp_folder
#'   temporary folder in which to write the files
#' @param benders_options
#'   list of benders decomposition options, as returned by
#'   \code{\link{read_options}}.
#'   
#' @return nothing
#' 

update_average_cuts <- function(current_it, candidates, output_link_s, output_link_h_s, ov_cost, n_w, tmp_folder, benders_options)
{
  n_candidates <- length(candidates)
  
  #write in_avgcuts.txt file 
  script  <-  paste0(current_it$id, " ", ov_cost)
  write(script, file = paste0(tmp_folder, "/in_avgcuts.txt"), append = TRUE )      
  
  # write in_avgrentability.txt file
  script  <-  ""
  for (c in 1:n_candidates)
  {
    if(candidates[[c]]$has_link_profile)
    {
      len = length(subset(output_link_h_s, link == candidates[[c]]$link)$"MARG. COST")
      tmp_rentability <- sum(as.numeric(subset(output_link_h_s, link == candidates[[c]]$link)$"MARG. COST")*candidates[[c]]$link_profile[1:len, 1]) - candidates[[c]]$cost * n_w / 52
    }
    else
    {
      tmp_rentability <- sum(as.numeric(subset(output_link_s, link == candidates[[c]]$link)$"MARG. COST")) - candidates[[c]]$cost * n_w / 52
    }
    script <- paste0(script, current_it$id, " ", candidates[[c]]$name, " ", tmp_rentability)
    if (c != n_candidates) { script <- paste0(script, "\n")}
  }
  write(script, file = paste0(tmp_folder, "/in_avgrentability.txt"), append = TRUE )
}

#' Update costs and rentability files of the yearly cuts
#' Create files in_yearlyrentability.txt and in_yearlycuts.txt which
#' will be later read by AMPL as input of the master problem
#' 
#' 
#' @param current_it
#'   list of current iteration characteristics
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param output_area_y
#'   antaresDataList of all the areas of the study with a yearly time step
#' @param output_link_y
#'   antaresDataList of all the links of the study with a yearly time step
#' @param output_link_h
#'   antaresDataList of all the links of the study with an hourly time step
#' @param inv_cost
#'   investments costs of this iteration
#' @param n_w
#'   number of weeks per year
#' @param tmp_folder
#'   temporary folder in which to write the files
#' @param benders_options
#'   list of benders decomposition options, as returned by
#'   \code{\link{read_options}}.
#'   
#' @return nothing
#' 
#' 
update_yearly_cuts <- function(current_it,candidates, output_area_y,output_link_y, output_link_h, inv_cost, n_w, tmp_folder, benders_options)
{
  # compute a few intermediate variables
  n_candidates <- length(candidates)
  last_y <- current_it$mc_years[length(current_it$mc_years)]
  
  # initiate scripts
  script_rentability  <-  ""
  script_cost <- ""
  
  # for every mc years and every week
  for(y in current_it$mc_years)
  {
    if(benders_options$uc_type == "relaxed_fast")
    {
      # in that case, non-linear cost has to be removed because they are computed in a post-processing and are not
      # part of the ANTARES optimization  
      y_cost <- sum(as.numeric(subset(output_area_y, mcYear == y)$"OV. COST")) +
          sum(as.numeric(subset(output_link_y, mcYear == y)$"HURDLE COST")) -
          sum(as.numeric(subset(output_area_y, mcYear == y)$"NP COST")) +
          inv_cost
    }
    else
    {
      y_cost <- sum(as.numeric(subset(output_area_y, mcYear == y)$"OV. COST")) +
          sum(as.numeric(subset(output_link_y, mcYear == y)$"HURDLE COST")) +
          inv_cost
    }
    
    
    script_cost <- paste0(script_cost, current_it$id, " ", y , " ",y_cost)
    
    if (y != last_y) {script_cost <- paste0(script_cost, "\n")}
    
    for(c in 1:n_candidates)
    {
      if(candidates[[c]]$has_link_profile)
      {
        len = length(subset(output_link_h, link == candidates[[c]]$link & mcYear == y)$"MARG. COST")
        tmp_rentability <- sum(as.numeric(subset(output_link_h, link == candidates[[c]]$link & mcYear == y)$"MARG. COST")*candidates[[c]]$link_profile[1:len, 1]) - candidates[[c]]$cost * n_w / 52
      }
      else
      {
        tmp_rentability <- sum(as.numeric(subset(output_link_y, link == candidates[[c]]$link & mcYear == y)$"MARG. COST")) - candidates[[c]]$cost * n_w / 52
      }
      script_rentability <- paste0(script_rentability, current_it$id, " ", y, " ", candidates[[c]]$name, " ", tmp_rentability)
      if (c != n_candidates || y != last_y)
      {
        script_rentability <- paste0(script_rentability, "\n")
      }
    }
  }
  write(script_rentability, file = paste0(tmp_folder, "/in_yearlyrentability.txt"), append = TRUE )
  write(script_cost, file = paste0(tmp_folder, "/in_yearlycuts.txt"), append = TRUE )
}



#' Update costs and rentability files of the weekly cuts
#' Create files in_weeklyrentability.txt and in_weeklycuts.txt which
#' will be later read by AMPL as input of the master problem
#' 
#' 
#' @param current_it
#' list of current iteration characteristics
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param output_area_w
#'   antaresDataList of all the areas of the study with a weekly time step
#' @param output_link_w
#'   antaresDataList of all the links of the study with a weekly time step
#' @param output_link_h
#'   antaresDataList of all the links of the study with a weekly time step
#' @param inv_cost
#'   investments costs of this iteration
#' @param tmp_folder
#'   temporary folder in which to write the files
#' @param benders_options
#'   list of benders decomposition options, as returned by
#'   \code{\link{read_options}}.
#'   
#' @return nothing
#' 
#' 
update_weekly_cuts <- function(current_it, candidates, output_area_w, output_link_w, output_link_h, inv_cost, tmp_folder, benders_options)
{
  
  # compute a few intermediate variables
  n_candidates <- length(candidates)
  last_w <- current_it$weeks[length(current_it$weeks)]
  last_y <- current_it$mc_years[length(current_it$mc_years)]
  
  # initiate scripts
  script_rentability  <-  ""
  script_cost <- ""

  # for every mc years and every week
  for(y in current_it$mc_years)
  {
    for(w in current_it$weeks)
      
    {
      
      if(benders_options$uc_type == "relaxed_fast")
      {
        # in that case, non-linear cost has to be removed because they are computed in a post-processing and are not
        # part of the ANTARES optimization  
        w_cost <- sum(as.numeric(subset(output_area_w, mcYear == y & timeId == w)$"OV. COST")) +
            sum(as.numeric(subset(output_link_w, mcYear == y & timeId == w)$"HURDLE COST")) -
            sum(as.numeric(subset(output_area_w, mcYear == y & timeId == w)$"NP COST")) +
            inv_cost/52
      }
      else
      {
        w_cost <- sum(as.numeric(subset(output_area_w, mcYear == y & timeId == w)$"OV. COST")) +
            sum(as.numeric(subset(output_link_w, mcYear == y & timeId == w)$"HURDLE COST")) +
            inv_cost/52
      }
      
      script_cost <- paste0(script_cost, current_it$id, " ", y , " ", w, " ", w_cost)
      
      
      
      
      if (y != last_y || w != last_w) {script_cost <- paste0(script_cost, "\n")}
      

      for(c in 1:n_candidates)
      {
        if(candidates[[c]]$has_link_profile)
        {
            first_h <- 7*24*(w-1)+1
            last_h <- 7*24*w
          tmp_rentability <- sum(as.numeric(subset(output_link_h, link == candidates[[c]]$link & mcYear == y & timeId >= first_h & timeId <= last_h)$"MARG. COST")* candidates[[c]]$link_profile[first_h:last_h,1]) - candidates[[c]]$cost /52
          #tmp_rentability <- sum(as.numeric(subset(output_link_h, link == candidates[[c]]$link & mcYear == y & timeId >= first_h & timeId <= last_h)$"MARG. COST")* candidates[[c]]$link_profile[first_h:last_h,1])
        }
        else
        {
          tmp_rentability <- sum(as.numeric(subset(output_link_w, link == candidates[[c]]$link & mcYear == y & timeId == w)$"MARG. COST")) - candidates[[c]]$cost /52
          #tmp_rentability <- sum(as.numeric(subset(output_link_w, link == candidates[[c]]$link & mcYear == y & timeId == w)$"MARG. COST"))
        }
        script_rentability <- paste0(script_rentability, current_it$id, " ", y , " ", w , " ", candidates[[c]]$name, " ", tmp_rentability)
        
        if (c != n_candidates || y != last_y || w != last_w)
        {
           script_rentability <- paste0(script_rentability, "\n")
        }
      }
    }
  }
  write(script_rentability, file = paste0(tmp_folder, "/in_weeklyrentability.txt"), append = TRUE )
  write(script_cost, file = paste0(tmp_folder, "/in_weeklycuts.txt"), append = TRUE )
}



#' Check the convergence of the relaxed expansion problem
#' 
#' 
#' @param best_sol
#' cost of best solution
#' @param best_under
#' cost of best under estimator
#' @param benders_options
#'   list of benders decomposition options, as returned by
#'   \code{\link{read_options}}.
#'   
#' @return logical, indicating weither or not the relaxed problem has converged within the optimality gap
#' 
#' @importFrom assertthat assert_that
#' 
convergence_relaxed <-  function(best_sol, best_under, benders_options)
{
  # is the optimality gap given in percentage or absolute value ?
  if(grepl("%$", benders_options$optimality_gap))
  {
    # if yes
    # remove the % at the end, convert in numeric, and multiply by the best current solution
    opt_gap <- as.numeric(gsub("%$", "", benders_options$optimality_gap)) * best_sol / 100 
  }
  else
  {
    # if no, optimality gap is already given in absolute value
    opt_gap <- benders_options$optimality_gap
  }
  
  # is the relaxed optimality gap given in percentage or absolute value ?
  if(grepl("%$", benders_options$relaxed_optimality_gap))
  {
    opt_gap_r <- as.numeric(gsub("%$", "", benders_options$relaxed_optimality_gap)) * best_sol / 100
  }
  else
  {
    opt_gap_r <- benders_options$relaxed_optimality_gap
  }
  
  # check that optimality gaps are not NA
  assertthat::assert_that(!is.na(opt_gap))
  assertthat::assert_that(!is.na(opt_gap_r))
  
  # is convergence of the relaxed problem reached ?
  return((best_sol - best_under) <= max(opt_gap * 1.1, opt_gap_r))
}



#' Check the convergence of the expansion problem
#' 
#' 
#' @param best_sol
#' cost of best solution
#' @param best_under
#' cost of best under estimator
#' @param benders_options
#'   list of benders decomposition options, as returned by
#'   \code{\link{read_options}}.
#'   
#' @return logical, indicating weither or not the expansion problem has converged within the optimality gap
#' 
#' @importFrom assertthat assert_that
#' 
convergence <-  function(best_sol, best_under, benders_options)
{
  # is the optimality gap given in percentage or absolute value ?
  if(grepl("%$", benders_options$optimality_gap))
  {
    # if yes
    # remove the % at the end, convert in numeric, and multiply by the best current solution
    opt_gap <- as.numeric(gsub("%$", "", benders_options$optimality_gap)) * best_sol / 100
  }
  else
  {
    # if no, optimality gap is already given in absolute value
    opt_gap <- benders_options$optimality_gap
  }
  
  # check that optimality gap is not NA
  assertthat::assert_that(!is.na(opt_gap))

  # is convergence of the relaxed problem reached ?
  return((best_sol - best_under) <= opt_gap)
}

#' Update costs and rentability files of the weekly cuts
#' Create files in_weeklyrentability.txt and in_weeklycuts.txt which
#' will be later read by AMPL as input of the master problem
#' 
#' 
#' @param current_it
#' list of current iteration characteristics
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param output_area_w
#'   antaresDataList of all the areas of the study with a weekly time step
#' @param output_link_w
#'   antaresDataList of all the links of the study with a weekly time step
#' @param output_link_h
#'   antaresDataList of all the links of the study with a weekly time step
#' @param inv_cost
#'   investments costs of this iteration
#' @param tmp_folder
#'   temporary folder in which to write the files
#' @param benders_options
#'   list of benders decomposition options, as returned by
#'   \code{\link{read_options}}.
#'   
#' @return nothing
#' 
#' 
update_weekly_cuts_path <- function(current_it, simulated_year, candidates, output_area_w, output_link_w, output_link_h, inv_cost, tmp_folder, benders_options)
{
  #INV COST TO BE REMOVER FROM THE FUNCTION
  # compute a few intermediate variables
  n_candidates <- length(candidates)
  last_w <- current_it$weeks[length(current_it$weeks)]
  last_y <- current_it$mc_years[length(current_it$mc_years)]
  
  # initiate scripts
  script_rentability  <-  ""
  script_cost <- ""
  
  # for every mc years and every week
  for(y in current_it$mc_years)
  {
    for(w in current_it$weeks)
      
    {
      
      if(benders_options$uc_type == "relaxed_fast")
      {
        # in that case, non-linear cost has to be removed because they are computed in a post-processing and are not
        # part of the ANTARES optimization  
        w_cost <- sum(as.numeric(subset(output_area_w, mcYear == y & timeId == w)$"OV. COST")) +
          sum(as.numeric(subset(output_link_w, mcYear == y & timeId == w)$"HURDLE COST")) -
          sum(as.numeric(subset(output_area_w, mcYear == y & timeId == w)$"NP COST"))
      }
      else
      {
        w_cost <- sum(as.numeric(subset(output_area_w, mcYear == y & timeId == w)$"OV. COST")) +
          sum(as.numeric(subset(output_link_w, mcYear == y & timeId == w)$"HURDLE COST"))
      }
      
      script_cost <- paste0(script_cost, simulated_year, " ", current_it$id, " ", y , " ", w, " ", w_cost)
      

      if (y != last_y || w != last_w) {script_cost <- paste0(script_cost, "\n")}
      
      
      for(c in 1:n_candidates)
      {
        if(candidates[[c]]$has_link_profile)
        {
          first_h <- 7*24*(w-1)+1
          last_h <- 7*24*w
          #tmp_rentability <- sum(as.numeric(subset(output_link_h, link == candidates[[c]]$link & mcYear == y & timeId >= first_h & timeId <= last_h)$"MARG. COST")* candidates[[c]]$link_profile[first_h:last_h,1]) - candidates[[c]]$cost /52
          tmp_rentability <- sum(as.numeric(subset(output_link_h, link == candidates[[c]]$link & mcYear == y & timeId >= first_h & timeId <= last_h)$"MARG. COST")* candidates[[c]]$link_profile[first_h:last_h,1])
        }
        else
        {
          #tmp_rentability <- sum(as.numeric(subset(output_link_w, link == candidates[[c]]$link & mcYear == y & timeId == w)$"MARG. COST")) - candidates[[c]]$cost /52
          tmp_rentability <- sum(as.numeric(subset(output_link_w, link == candidates[[c]]$link & mcYear == y & timeId == w)$"MARG. COST"))
        }
        script_rentability <- paste0(script_rentability, simulated_year, " ", current_it$id, " ", y , " ", w , " ", candidates[[c]]$name, " ", tmp_rentability)
        
        if (c != n_candidates || y != last_y || w != last_w)
        {
          script_rentability <- paste0(script_rentability, "\n")
        }
      }
    }
  }
  write(script_rentability, file = paste0(tmp_folder, "/in_weeklyrentability.txt"), append = TRUE )
  write(script_cost, file = paste0(tmp_folder, "/in_weeklycuts.txt"), append = TRUE )
}
#' Update costs and rentability files of the average cuts
#' Create files in_avgrentability.txt and in_avgcuts.txt which
#' will be later read by AMPL as input of the master problem
#' 
#' 
#' @param current_it
#'   list of current iteration characteristics
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param output_link_s
#'   antaresDataList of all the links of the study with a yearly time step
#'   and averaged on all MC years
#' @param output_link_h_s
#'   antaresDataList of all the links of the study with an hourly time step
#'   and averaged on all MC years
#' @param op_cost
#'   operation costs of this iteration
#' @param n_w
#'   number of weeks per year
#' @param tmp_folder
#'   temporary folder in which to write the files
#' @param benders_options
#'   list of benders decomposition options, as returned by
#'   \code{\link{read_options}}.
#'   
#' @return nothing
#' 

update_average_cuts_path <- function(current_it, simulated_year, candidates, output_link_s, output_link_h_s, op_cost, n_w, tmp_folder, benders_options)
{
  n_candidates <- length(candidates)
  
  #write in_avgcuts.txt file 
  script  <-  paste0(simulated_year," ", current_it$id, " ", op_cost)
  write(script, file = paste0(tmp_folder, "/in_avgcuts.txt"), append = TRUE )      
  
  # write in_avgrentability.txt file
  script  <-  ""
  for (c in 1:n_candidates)
  {
    if(candidates[[c]]$has_link_profile)
    {
      len = length(subset(output_link_h_s, link == candidates[[c]]$link)$"MARG. COST")
      #tmp_rentability <- sum(as.numeric(subset(output_link_h_s, link == candidates[[c]]$link)$"MARG. COST")*candidates[[c]]$link_profile[1:len, 1]) - candidates[[c]]$cost * n_w / 52
      tmp_rentability <- sum(as.numeric(subset(output_link_h_s, link == candidates[[c]]$link)$"MARG. COST")*candidates[[c]]$link_profile[1:len, 1])
    }
    else
    {
      #tmp_rentability <- sum(as.numeric(subset(output_link_s, link == candidates[[c]]$link)$"MARG. COST")) - candidates[[c]]$cost * n_w / 52
      tmp_rentability <- sum(as.numeric(subset(output_link_s, link == candidates[[c]]$link)$"MARG. COST")) 
    }
    script <- paste0(script, simulated_year," ", current_it$id, " ", candidates[[c]]$name, " ", tmp_rentability)
    if (c != n_candidates) { script <- paste0(script, "\n")}
  }
  write(script, file = paste0(tmp_folder, "/in_avgrentability.txt"), append = TRUE )
}
#' Update costs and rentability files of the yearly cuts
#' Create files in_yearlyrentability.txt and in_yearlycuts.txt which
#' will be later read by AMPL as input of the master problem
#' 
#' 
#' @param current_it
#'   list of current iteration characteristics
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param output_area_y
#'   antaresDataList of all the areas of the study with a yearly time step
#' @param output_link_y
#'   antaresDataList of all the links of the study with a yearly time step
#' @param output_link_h
#'   antaresDataList of all the links of the study with an hourly time step
#' @param inv_cost
#'   investments costs of this iteration
#' @param n_w
#'   number of weeks per year
#' @param tmp_folder
#'   temporary folder in which to write the files
#' @param benders_options
#'   list of benders decomposition options, as returned by
#'   \code{\link{read_options}}.
#'   
#' @return nothing
#' 
#' 
update_yearly_cuts_path <- function(current_it, simulated_year, candidates, output_area_y,output_link_y, output_link_h, inv_cost, n_w, tmp_folder, benders_options)
{
  # compute a few intermediate variables
  n_candidates <- length(candidates)
  last_y <- current_it$mc_years[length(current_it$mc_years)]
  
  # initiate scripts
  script_rentability  <-  ""
  script_cost <- ""
  
  # for every mc years and every week
  for(y in current_it$mc_years)
  {
    if(benders_options$uc_type == "relaxed_fast")
    {
      # in that case, non-linear cost has to be removed because they are computed in a post-processing and are not
      # part of the ANTARES optimization  
      y_cost <- sum(as.numeric(subset(output_area_y, mcYear == y)$"OV. COST")) +
        sum(as.numeric(subset(output_link_y, mcYear == y)$"HURDLE COST")) -
        sum(as.numeric(subset(output_area_y, mcYear == y)$"NP COST"))
    }
    else
    {
      y_cost <- sum(as.numeric(subset(output_area_y, mcYear == y)$"OV. COST")) +
        sum(as.numeric(subset(output_link_y, mcYear == y)$"HURDLE COST"))
    }
    
    
    script_cost <- paste0(script_cost," ", simulated_year, " ", current_it$id, " ", y , " ",y_cost)
    
    if (y != last_y) {script_cost <- paste0(script_cost, "\n")}
    
    for(c in 1:n_candidates)
    {
      if(candidates[[c]]$has_link_profile)
      {
        len = length(subset(output_link_h, link == candidates[[c]]$link & mcYear == y)$"MARG. COST")
        #tmp_rentability <- sum(as.numeric(subset(output_link_h, link == candidates[[c]]$link & mcYear == y)$"MARG. COST")*candidates[[c]]$link_profile[1:len, 1]) - candidates[[c]]$cost * n_w / 52
        tmp_rentability <- sum(as.numeric(subset(output_link_h, link == candidates[[c]]$link & mcYear == y)$"MARG. COST")*candidates[[c]]$link_profile[1:len, 1])
      }
      else
      {
        #tmp_rentability <- sum(as.numeric(subset(output_link_y, link == candidates[[c]]$link & mcYear == y)$"MARG. COST")) - candidates[[c]]$cost * n_w / 52
        tmp_rentability <- sum(as.numeric(subset(output_link_y, link == candidates[[c]]$link & mcYear == y)$"MARG. COST"))
      }
      script_rentability <- paste0(script_rentability, simulated_year, " ", current_it$id, " ", y, " ", candidates[[c]]$name, " ", tmp_rentability)
      if (c != n_candidates || y != last_y)
      {
        script_rentability <- paste0(script_rentability, "\n")
      }
    }
  }
  write(script_rentability, file = paste0(tmp_folder, "/in_yearlyrentability.txt"), append = TRUE )
  write(script_cost, file = paste0(tmp_folder, "/in_yearlycuts.txt"), append = TRUE )
}
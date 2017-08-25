#' Smart selection of the weeks to simuate
#' 
#' week_selection is a function which select the MC years and 
#' weeks which will be simulated during this iteration
#' 
#' @param current_it
#'   list of current iteration characteristics
#' @param mc_years
#'   vector of all the Monte Carlo years identifier 
#' @param weeks
#'   vector of all the weeks identifier
#' @param tmp_folder
#'   temporary folder of the benders decomposition
#' @param exp_options 
#'   list of options related to the expansion planning, as returned
#'   by the function \code{\link{read_options}}
#'   
#' @return 
#' updated current iteration characteristics
#' 
#' @importFrom dplyr filter rename mutate select
#' @importFrom utils read.table
#' 
week_selection <- function(current_it, mc_years, weeks, tmp_folder, exp_options)
{
  make_full_iteration <- FALSE
  # some constant parameters of the function
  max_iterations_between_full <- 5
  

  # if we are in the first iteration, run all the weeks
  if(current_it$n == 1)
  {
    make_full_iteration <- TRUE
  }
  
  # if week_selection option is not activated, run all the weeks
  if(!exp_options$week_selection)
  {
    make_full_iteration <- TRUE
  }
  
  # if full iteration was needed, run all the weeks
  if(current_it$need_full)
  {
    make_full_iteration <- TRUE
  }
  
  # if full iteration wasn't run in the last iterations, run all weeks
  if(current_it$last_full + max_iterations_between_full <  current_it$n)
  {
    make_full_iteration <- TRUE
  } 
  
  # if cut_type is average
  if(current_it$cut_type == "average")
  {
    make_full_iteration <- TRUE
  } 
  
  
  # ### temporary
  # current_it$cut_type <- sample(c("average", "yearly", "weekly"),1)
  # 
  # if(current_it$cut_type == "average")
  # {
  #   make_full_iteration <- TRUE
  # } 
  # 
  # ### enf of temporary
  
  # full iteration
  if(make_full_iteration)
  {
    current_it$full <- TRUE
    current_it$weeks <- weeks
    current_it$mc_years <- mc_years
    current_it$need_full <- FALSE
    current_it$last_full <- current_it$n
    return(current_it)
  }
  
  # read theta values
  theta <- utils::read.table(paste0(tmp_folder, "/out_theta.txt"), sep =";", dec = ".", col.names = c("it", "year", "week", "theta"))
  
  # look for negative values of theta in the last iteration
  neg_theta <- dplyr::filter(theta, it == (current_it$n - 1), theta < 0)
  
  # if there is negative values, focus simulation on those weeks
  if(nrow(neg_theta) > 0)
  {
    to_simulate <- dplyr::filter(theta, it == (current_it$n - 1))
    to_simulate$weight <- 0
    to_simulate$weight[to_simulate$theta < 0] <- 1

    # convert (year,week) couples into a playlist and/or a period selection
    converted <- convert(to_simulate, mc_years, weeks)
    
    # if there is too many weeks, year we go for a full iteration
    # too many weeks = more than 75% of all weeks
    if(length(converted$weeks) * length(converted$mc_years) >= 0.75 * length(mc_years) * length(weeks))
    {
      current_it$full <- TRUE
      current_it$weeks <- weeks
      current_it$mc_years <- mc_years
      current_it$need_full <- FALSE
      current_it$last_full <- current_it$n
      return(current_it)
    }
    # otherwise, we return a partial iteration
    else
    {
      current_it$full <- FALSE
      current_it$weeks <- converted$weeks
      current_it$mc_years <- converted$mc_years
      if(current_it$cut_type == "yearly")
      {
        current_it$weeks <- weeks
      }
      current_it$need_full <- FALSE
      return(current_it)
    }
  }
  
  # look at the differences between thetas at two consecutives iterations
  if(current_it$n >= 3)
  {
    # compute the difference between two iterations
    diff_theta <- merge(x = filter(theta, it == (current_it$n - 1)),
          y = dplyr::rename(filter(theta, it == (current_it$n - 2)), theta_previous = theta),
          by = c("year", "week"))
    diff_theta <- dplyr::mutate(diff_theta, diff = theta - theta_previous)
    
    avg_diff <- mean(diff_theta$diff)
    max_diff <- max(diff_theta$diff)
    
    
    
    # for now, really basic weight construction 
    # could further be improved
    
    # add column weight
    diff_theta$weight <- 1
    
    # weight is put to zero if diff_theta is below average:
    diff_theta$weight[diff_theta$diff < 2* avg_diff] <- 0
    
    if(sum(diff_theta$weight) == 0)
    {
      diff_theta$weight[diff_theta$diff < avg_diff] <- 0
    }
    
    
    
    
    # convert (year,week) couples into a playlist and/or a period selection
    to_simulate <- dplyr::select(diff_theta, it.x, year, week, weight)
    
    # write.table(to_simulate, paste0(tmp_folder, "/week_select.csv"), col.names = c("it", "year", "week", "weight"), sep = ";", append = TRUE)
    
    converted <- convert(to_simulate, mc_years, weeks)
    
    # if there is too many weeks, year we go for a full iteration
    # too many weeks = more than 75% of all weeks
    if(length(converted$weeks) * length(converted$mc_years) >= 0.75 * length(mc_years) * length(weeks))
    {
      current_it$full <- TRUE
      current_it$weeks <- weeks
      current_it$mc_years <- mc_years
      current_it$need_full <- FALSE
      current_it$last_full <- current_it$n
      return(current_it)
    }
    # otherwise, we return a partiel iteration
    else
    {
      current_it$full <- FALSE
      current_it$weeks <- converted$weeks
      current_it$mc_years <- converted$mc_years
      if(current_it$cut_type == "yearly")
      {
        current_it$weeks <- weeks
      }
      current_it$need_full <- FALSE
      return(current_it)
    }
  }
  
  # if none of the previous condition is fullfilled, by default we go for a complete iteration
  {
    current_it$full <- TRUE
    current_it$weeks <- weeks
    current_it$mc_years <- mc_years
    current_it$need_full <- FALSE
    current_it$last_full <- current_it$n
    return(current_it)
  }
  
}

#' Convert to_simulate file into independent mc_years and weeks list
#' 
#' to_simulate contains pair (mc_year, week) with their weight
#' the weight is between zero and one :
#'    - a null weight implies that the pair (mc_year, week) shouldn't be
#'      simulated in the next benders iteration
#'    - a weight equal to 1 implies that the pair is particularly relevant
#'      and should be simulated in the next benders iteration
#'      
#' this function therefore choses a mc_years list and a weeks list which  
#' covers most of the weeks with strictly positive weight, but not too many 
#' weeks with a weight equal to zero
#' 
#' @param to_simulate
#'   data.frame with week, year and weight column
#' @param mc_years
#'   all mc_years of the study
#' @param weeks 
#'   all weeks of the study
#'   
#' @return 
#' return a list of output containing the playlist of mc years (out$mc_years)
#' and the playlist of weeks (out$weeks)
#' 
#' @importFrom assertthat assert_that
#' @importFrom dplyr  group_by summarise filter
#' @importFrom magrittr %>%
#' 

convert <- function(to_simulate, mc_years, weeks)
{
  out <- list()
  # general parameters (could be customized)
  # (should we integrate them in the options or is it too specific ?)
  
  alpha_years <- 0.10  
  
  # mc_years is kept only if its weight is above alpha_years*average weight of all mc_years
  # if alpha_years == 0, all the mc_years with a at least on positive weight will be kept
  

    
  # 1 - start by selecting weeks
  # sum of all weights
  total_weight <- sum(to_simulate$weight)
  assertthat::assert_that(total_weight>0)
  weight_per_week <- to_simulate %>%  dplyr::group_by(week) %>% dplyr::summarise(sum_over_week = sum(weight))
  
  # compute consecutive weeks which wrap all positive weights
  weeks_with_positive_weight <- dplyr::filter(weight_per_week, sum_over_week > 0)
  first_week <- weeks_with_positive_weight$week[1]
  last_week <- weeks_with_positive_weight$week[nrow(weeks_with_positive_weight)]
  
  # for now, we use a naive selection of weeks, we take all the consecutive ones 
  # which are required to cover all positive weights :
  # 
  # this approach could be improved because we will also include many weeks 
  # which are not required, the worst case been the one with only week 1 and 52
  # with positive weights
  out$weeks <- first_week:last_week
  
  
  
  # see how many weeks with zero weight are included in this interval 
  # weeks_with_null_weight <- filter(weight_per_week, sum_over_week == 0, week >= first_week, week <= last_week) %>% 
  #                          select(week)
  # n_weeks_Iwth_null_weight <- nrow(weeks_with_null_weight)

  
  # 2 - select mc_years
  weight_per_mc_year <- to_simulate %>%  dplyr::group_by(year) %>% dplyr::summarise(sum_over_year = sum(weight))
  
  if(alpha_years <= 0)
  {
    out$mc_years <- weight_per_mc_year$year
  }
  else
  {
    selection_criterion <-  alpha_years * total_weight / length(mc_years)
    out$mc_years <- dplyr::filter(weight_per_mc_year, sum_over_year >= selection_criterion)$year
  }
  
  return(out)
  
}

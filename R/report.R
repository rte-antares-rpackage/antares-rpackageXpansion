#' Write report which summarizes the results of the expansion
#' planning
#' 
#' 
#' @param benders_out
#'   Output file of the benders decomposition function, as returned by
#'   \code{\link{benders}}. If default value \code{last} is used, the function
#'   will use the results of the last launched benders decomposition
#' @param file
#'   File name of the report. By default the report will be stored in the
#'   directory_path of the current ANTARES study
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return 
#' 
#' @importFrom antaresRead simOptions
#' @importFrom rmarkdown render
#' @importFrom assertthat assert_that
#' @export
#' 
report <- function(benders_out = "last", file = default_report_file(opts), opts = antaresRead::simOptions())
{
  # copy the benders_out into a Rdata in the temporary folder
  # except if benders_out == "last", in that case we use the last
  # written data_for_report.RDS file
  if(length(benders_out) == 1)
  {
    if(benders_out == "last")
    {
      tmp_folder <- paste(opts$studyPath,"/user/expansion/temp",sep="")
      assertthat::assert_that(file.exists(paste0(tmp_folder, "/data_for_report.RDS")))
      benders_out <- readRDS(file = paste0(tmp_folder, "/data_for_report.RDS"))
    } 
  }

  x <- benders_out
  
  # launch Rmarkdown file
  rmarkdown::render(input = system.file("rmd/report.Rmd", package = "antaresXpansion"), output_file = file, params = x)
}

                 

#' Return the default file name for the report
#' 
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @return 
#' name of the first area that the link connects
#' 
#' @importFrom antaresRead simOptions
#' @importFrom lubridate now
#'                                       
default_report_file_path <- function(directory_path)
{
  # check that report folder exists, if not create it
  report_folder <- paste(directory_path,"/report",sep="")
  if(!dir.exists(report_folder))
  {
    dir.create(report_folder)
  }
  
  # return default file name
  paste0(directory_path, "/report/" , as.character.Date(lubridate::now(), format = "%Y%m%d-%H%M") ,"_report.html") 
}


#' Return the default file name for the report
#' 
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @return 
#' name of the first area that the link connects
#' 
#' @importFrom antaresRead simOptions
#' @importFrom lubridate now
#'                                       
default_report_file <- function(opts)
{
  # check that report folder exists, if not create it
  report_folder <- paste0(opts$studyPath, "/user/expansion/report")
  if(!dir.exists(report_folder))
  {
    dir.create(report_folder)
  }
  
  # return default file name
  paste0(opts$studyPath, "/user/expansion/report/" , as.character.Date(lubridate::now(), format = "%Y%m%d-%H%M") ,"_report.html") 
}
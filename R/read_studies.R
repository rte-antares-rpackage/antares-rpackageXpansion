#' Read studies with their characteristics
#' 
#' \code{read_studies} is a function which stores the links of the studies in a vector indexed on the selected years.
#' The information is stored in the file: antaresStudyPath/user/expansion/studies.ini.
#'   
#' @param studies
#'   list of the paths of the studies returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return 
#' Returns a list containing the different paths of the studies
#' WARNING: / may have to be changed to backslash 
#' 
#' @importFrom assertthat assert_that
#' @export
#' 


read_studies <- function(studies_file_name)
{
  #change the working directory while executing the function to the studies.ini directory
  #enables to work with relative paths
  current_wd<-getwd()
  on.exit(setwd(current_wd))
  setwd(dirname(studies_file_name))
  
  studies<-list()
  studies$n_simulated_years<-0
  studies$simulated_years<-c()
  studies$opts<-list()
  first_line=0
  
  #read file
  #param_data <- scan(studies_file_name, what=character(), sep="", quiet = TRUE)
  param_data <- readLines(basename(studies_file_name))
  
  # go through every line of the file from the first non-empty line (identified by id_year) to the end
  for(line in 1:length(param_data))
  {
    if (param_data[line]==""){
      first_line<-first_line+1
      next
    }
    
    option_name <-strsplit(param_data[line], "=")[[1]][1]
    option_value <- strsplit(param_data[line], "=")[[1]][2]
    
    # remove white spaces in the beginning and the end
    option_name <- sub("^\\s+", "", option_name)
    option_name <- sub("\\s+$", "", option_name)
    option_value <- sub("^\\s+", "", option_value)
    option_value <- sub("\\s+$", "", option_value)
    
    #option_value<-gsub(\,/,option_value)
    #assertthat::assert_that(file.exists(option_value))
    
    studies$n_simulated_years<-studies$n_simulated_years+1
    studies$simulated_years<-c(studies$simulated_years,option_name)
    
    id_year=line-first_line
    #id_year=1 for the first non empty line
    studies$opts[[id_year]]<-setSimulationPath(option_value,simulation=0)
    
    assertthat::assert_that(file.exists(studies$opts[[id_year]]$studyPath))
    
  }
  return(studies)
}

#TEST<-read_studies("D:/testdavid.ini")

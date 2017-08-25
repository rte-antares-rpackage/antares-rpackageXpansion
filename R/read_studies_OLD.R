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
#' Returns a data frame containing the different paths of the studies:
#' WARNING: / may have to be changed to backslash 
#' simulated_years    paths
#'1            2020   D:/Users/manthube/Documents/Antares_work
#'2            2040   D:/Users/manthube/Documents
#'3            2050   D:/Users/manthube
#' @importFrom assertthat assert_that
#' @export
#' 


read_studies_OLD <- function(studies_file_name)
{
  #read file
  param_data <- scan(studies_file_name, what=character(), sep="", quiet = TRUE)
  
  #initialisation du data frame 
  #search of the first line of the studies.ini file
  
  empty_line=TRUE
  first_line=1
  
  while(empty_line&&first_line<=length(param_data)){
    option_name <- strsplit(param_data[first_line], "=")[[1]][1]
    if(option_name == ""){
      first_line<-first_line+1
    } # empty line
    else{
      option_value <- strsplit(param_data[first_line], "=")[[1]][2]
      empty_line=FALSE
    }
  }
  # remove white spaces in the beginning and the end
  option_name <- sub("^\\s+", "", option_name)
  option_name <- sub("\\s+$", "", option_name)
  option_value <- sub("^\\s+", "", option_value)
  option_value <- sub("\\s+$", "", option_value)
  
  #assrt..
  
  #initialisation of the data frame
  studies<-data.frame(option_name,option_value,0)
  names(studies)<-c("simulated_years","paths","n_simulated_years")
  
  
  # go through every line of the file from the first non-empty line to the end
  for(line in (first_line+1):length(param_data))
  {
    # read option and value
    option_name <- strsplit(param_data[line], "=")[[1]][1]
    if(option_name == ""){next} # empty line
    
    option_value <- strsplit(param_data[line], "=")[[1]][2]
    
    # remove white spaces in the beginning and the end
    option_name <- sub("^\\s+", "", option_name)
    option_name <- sub("\\s+$", "", option_name)
    option_value <- sub("^\\s+", "", option_value)
    option_value <- sub("\\s+$", "", option_value)
    
    #assert.
    
    df<-data.frame(option_name,option_value,0)
    names(df)<-c("simulated_years","paths","n_simulated_years")
    
    studies<-rbind(df,studies)
  }
  return(studies)
}

#TEST<-read_studies("D:/testdavid.ini")

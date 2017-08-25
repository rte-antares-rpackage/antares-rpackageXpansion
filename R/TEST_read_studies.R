read_studies <- function(studies_file_name)
{
  # read file
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
  
  #initialisation of the data frame
  studies<-data.frame(option_name,option_value)
  names(studies)<-c("simulated_years","paths")
  
  
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
    
    df<-data.frame(option_name,option_value)
    names(df)<-c("simulated_years","paths")
    
    studies<-rbind(df,studies)
  }
  return(studies)
}

TEST<-read_studies("D:/testdavid.ini")

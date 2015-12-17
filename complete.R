complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases

  file_names <- vector(mode = "character", length=length(id))
  id_name <- vector(mode = "character", length=length(id))
  i<-1
  for(i in 1:length(id)) {
    id_name[i] <- formatC(id[i], width ="3",format = "d", flag = "0")
    file_names[i] <- paste0(directory,"/", id_name[i],".csv")
  }
  
  x<-1
  nobs <- vector("numeric")
  for(x in 1:length(id)) {
    p = read.csv(file_names[x])
    s <- p$sulfate
    n <- p$nitrate
    na_data = !is.na(n) & !is.na(s)
    p = p[na_data,]
    nobs <- c(nobs,length(p$Date))
  }
  df = data.frame(id, nobs)
  return(df)
  
}
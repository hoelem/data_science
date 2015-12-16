pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
    file_names = vector(mode = "character", length=length(id))
    id_name = vector(mode = "character", length=length(id))
    pollutantdata = vector(mode = "numeric", length=length(id))
    i<-1
    for(i in in 1:length(id) {
      id_name[i] = formatC(id[i], width ="3",format = "d", flag = "0")
      file_names[i] = paste0(directory,"/", id_name[i],".csv")
    }
    
    x<-1
    for(x in 1:length(id)) {
      p = read.csv(file_names[x])
      p <- p[[pollutant]]
      na_data = is.na(p)
      pollutantdata[x] <- mean(p[!na_data])
      
      #print(pollutantdata[x])
    }
    print(pollutantdata)
    return(mean(pollutantdata)) 
  
  }
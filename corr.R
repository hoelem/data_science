corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        
        data <- complete(directory)
        coorrelation <- vector("numeric")
        thrsh <- data$nobs>threshold
        if(any(thrsh == TRUE)) {
                
                id_greater_thresh <- data$id[data$nobs>threshold]
                
                
                i<-1
                for(i in 1:length(id_greater_thresh)) {
                        id_name <- formatC(id_greater_thresh[i], width ="3",format = "d", flag = "0")
                        file_names <- paste0(directory,"/", id_name,".csv")
                        
                        p <- read.csv(file_names)
                        na_data = !is.na(p$sulfate) & !is.na(p$nitrate)
                        p = p[na_data,]
                        c <- cor(p$nitrate,p$sulfate)
                        coorrelation = c(coorrelation,c)
                        
                        
                }

  }  
    
    
  return(coorrelation)
  
}
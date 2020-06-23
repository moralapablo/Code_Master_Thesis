preprocesing <- function(data,scale_method="0,1"){
  
  if (scale_method=="0,1"){
    #### Scale the data in the [0,1] interval and separate train and test ####
    
    maxs <- apply(data, 2, max) #obtain the max of each variable
    mins <- apply(data, 2, min) #obtain the min of each variable
    scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
    
    index <- sample(1:nrow(data),round(0.75*nrow(data))) #75% of train data, 25% test
    train <- scaled[index,]
    test <- scaled[-index,]
    
    output <- vector(mode = "list", length = 2)
    output[[1]]=train
    output[[2]]=test
    names(output)=c("train","test")
    
  } else if (scale_method=="-1,1"){
    #### Scale the data in the [-1,1] interval and separate train and test ####
    
    maxs <- apply(data, 2, max) #obtain the max of each variable
    mins <- apply(data, 2, min) #obtain the min of each variable
    scaled <- as.data.frame(scale(data, center = mins+(maxs-mins)/2, scale = (maxs - mins)/2))
    
    index <- sample(1:nrow(data),round(0.75*nrow(data))) #75% of train data, 25% test
    train <- scaled[index,]
    test <- scaled[-index,]
    
    output <- vector(mode = "list", length = 2)
    output[[1]]=train
    output[[2]]=test
    names(output)=c("train","test")
    
  } else if (scale_method=="standardize"){
    #### Scale the data to have mean=0 and sd=1 and separate train and test ####
    
    scaled <- as.data.frame(scale(data, center = TRUE, scale = TRUE))
    
    index <- sample(1:nrow(data),round(0.75*nrow(data))) #75% of train data, 25% test
    train <- scaled[index,]
    test <- scaled[-index,]
    
    output <- vector(mode = "list", length = 2)
    output[[1]]=train
    output[[2]]=test
    names(output)=c("train","test")
    
  } else {
    print("Non valid method")
    output="Non valid method"
  }
  
  
  return(output)
}
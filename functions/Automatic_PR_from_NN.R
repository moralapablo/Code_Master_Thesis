Automatic_PR_from_NN = function(train,test,nn,fun,q_taylor){
  #get the dimension p:
  p=dim(test)[2]-1
  
  # obtain the weights from the NN model:
  w=t(nn$weights[[1]][[1]]) # we transpose the matrix to match the desired dimensions
  v=nn$weights[[1]][[2]]
  
  #Obtain the vector with the derivatives of the activation function up to the given degree:
  g=rev(taylor(fun, 0, q_taylor))
  
  #Apply the formula
  coeff=betas_from_weights(w,v,g)
  
  #Obtain the predicted values for the test data with our Polynomial Regression
  n_test=length(test$Y)
  PR.prediction=rep(0,n_test)
  
  for(i in 1:n_test){
    PR.prediction[i]=evaluate_PR(test[i,seq(p)],coeff)
  }
  
  #Obtain the predicted values with the NN
  NN.prediction <- predict(nn, test)
  
  #plots to compare results:
  
  df.plot=data.frame(test$Y,PR.prediction,NN.prediction)
  
  plot1<-ggplot(df.plot,aes(x=NN.prediction,y=test$Y))+geom_point()+geom_abline(slope = 1,intercept = 0,color = "red")+labs(y = "Original Y")+labs(x = "Predicted Y with NN")+ ggtitle("NN vs Y")+theme_cowplot(12)+ theme(plot.title = element_text(hjust = 0.5,size=10))+ theme(axis.text=element_text(size=8), axis.title=element_text(size=8))
  
  plot2<-ggplot(df.plot,aes(x=PR.prediction,y=NN.prediction))+geom_point()+geom_abline(slope = 1,intercept = 0,color = "red")+labs(y = "Predicted Y with NN")+labs(x = "Predicted Y with PR") + ggtitle("PR vs NN") +theme_cowplot(12)+ theme(plot.title = element_text(hjust = 0.5,size=10))  + theme(axis.text=element_text(size=8), axis.title=element_text(size=8))
  
  plot=plot_grid(plot1, plot2, labels = c("a)","b)"))
  
  #MSE:
  
  NN.MSE <- sum((test$Y - NN.prediction)^2)/n_test
  PR.MSE <- sum((test$Y - PR.prediction)^2)/n_test
  
  #MSE between NN and PR (because PR is actually approximating the NN, not the actual response Y)
  MSE.NN.vs.PR <- sum((NN.prediction - PR.prediction)^2)/n_test
  
  #R squared for the PR, using the train data:
  
  #Obtain the predicted values for the test data with our Polynomial Regression
  n_train=length(train$Y)
  PR.prediction.train=rep(0,n_train)
  
  for(i in 1:n_train){
    PR.prediction.train[i]=evaluate_PR(train[i,seq(p)],coeff)
  }
  
  mean.Y=mean(train$Y)
  SST=sum((train$Y - mean.Y)^2)
  SSR=sum((train$Y - PR.prediction.train)^2)
  
  PR.R2=1-(SSR/SST)
  
  terms=length(coeff)-1
  PR.R2.adjusted=1-(1-PR.R2)*(n_train-1)/(n_train-terms-1)
  
  
  
  #Output:
  
  output <- vector(mode = "list", length = 14)
  output[[1]]=train
  output[[2]]=test
  output[[3]]=g
  output[[4]]=nn
  output[[5]]=coeff
  output[[6]]=NN.prediction
  output[[7]]=PR.prediction
  output[[8]]=NN.MSE
  output[[9]]=PR.MSE
  output[[10]]=PR.prediction.train
  output[[11]]=PR.R2
  output[[12]]=PR.R2.adjusted
  output[[13]]=plot
  output[[14]]=MSE.NN.vs.PR
  
  
  names(output)<-c("train","test","g","nn","coeff","NN.prediction","PR.prediction","NN.MSE","PR.MSE","PR.prediction.train","PR.R2","PR.R2.adjusted","plot","MSE.NN.vs.PR")
  
  return(output)
  
}
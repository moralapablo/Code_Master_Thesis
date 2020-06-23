
plot_Taylor_and_weights = function(example,fun,x,tol,q_taylor,title){
    
    test=example$test  
  
    # obtain the weights from the NN model:
    w=t(example$nn$weights[[1]][[1]]) # we transpose the matrix to match the desired dimensions
    
    # get the dimensions
    p=dim(w)[2]-1
    h_1=dim(w)[1]
    
    # We need to add a column with 1's to the test data to multiply them by the biases w_0
    n_test=dim(test)[1]
    x.values=cbind(rep(1,n_test),test[,-(p+1)])
    
    # Compute the activation values first as a matrix, to compute each neuron separately
    activation.values=matrix(0,n_test,h_1)
    for(j in 1:h_1){
      for(i in 1:n_test){
        activation.values[i,j]=sum(w[j,]*x.values[i,])
      }
    }
    # Now join all the values in a vector we don't care in which neuron we have the problems as long as there is one.
    activation.values=as.vector(activation.values)
    
    # create data frame and create an empty plot with only the density of those values
    df.density=as.data.frame(activation.values)
    names(df.density)=c("x")
    
    plot.density <- ggplot(df.density) + aes(x=x) +
    geom_density(linetype = "dashed") + 
    xlim(x[1], x[length(x)]) + 
    theme_void()
    
    #### Taylor graph ######
    
    #compute the true function
    yf <- fun(x)
    #compute the Taylor approximation
    pol <- taylor(fun, 0, q_taylor)
    yp <- polyval(pol, x)
    #compute the erro as the absolute value of the difference
    error <- abs(yf-yp)
    
    #get points to place error bars for error <= tol
    ind=which(error<=tol)
    error1=x[ind[1]]
    error2=x[ind[length(ind)]]
    
    #Now we create the Taylor plot and add the density behind it.
    df.plot <- data.frame(x,yf,yp,error)
    
    plot.Taylor <- ggplot()+geom_line(data=df.plot,aes(x,yf))+
      geom_line(data=df.plot,aes(x,yp),color = "red")+
      geom_line(data=df.plot,aes(x,error),color = "blue")+
      geom_hline(yintercept = 0,color = "gray", linetype ="dashed")+
      labs(x = "x")+ labs(y = "y")+
      geom_vline(xintercept = error1,color = "gray", linetype ="dashed")+
      geom_vline(xintercept = error2,color = "gray", linetype ="dashed")+
      annotation_custom(ggplotGrob(plot.density),ymin=0, ymax=3)+
      theme_cowplot(12) + 
      ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5,size=10)) +
      theme(axis.text=element_text(size=10), axis.title=element_text(size=10))
    
    return(plot.Taylor)
}

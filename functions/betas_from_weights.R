betas_from_weights=function(w,v,g){
  # w matrix of size h_1*p+1, such that the elements are w_ji
  # v vector of length h_1+1 such that the elements are v_j
  # g vector of length q+1 such that g=(g(0),g'(0),g''(0),...,g^{(q)}(0))
  
  # To follow our original notation we need to set uo the values for q,p and h_1. 
  # note that this is because the vector starts at 0 in our notation
  
  q=length(g)-1 
  h_1=dim(w)[1] 
  p=dim(w)[2]-1
  
  # We define the vector that will contain all the coefficents (betas) and their associated indexes (labels)
  
  betas=c(0)
  labels=c("0")
  
  #Now we apply the formulas described previously.
  
  # beta 0 (special case)
  
  betas[1]=v[1] # first get v[1]=v_0 in our notation
  
  # Now obtain the summation:
  for (j in 1:h_1){
    aux=0 # this auxiliar variable will store the inner summation
    for (n in 0:q){
      aux=aux+g[n+1]*w[j,1]^n # we have to use g[n+1] to obtain g^(n)/n!, because the function taylor already includes the term 1/n!
    }
    betas[1]=betas[1]+v[j+1]*aux # we have to use v[j+1] to obtain v_j
  }
  
  
  # The rest of the betas:
  
  #For each t from 1 to q, wehre t is the number of subindexes in the coefficent:
  
  for (t in 1:q){
    
    #We need to find all the possible combinations (order does not matter) of length t with elements from 1 to p (the input variables) with repetition.
    indexes <- combinations(p,t,repeats.allowed = TRUE) # needs library(gtools)
    indexes.rows=nrow(indexes) #store the number of all possible combinations for a given t
    
    #Now we create temporal betas and labels for the given t that we will include in the final result vector
    betas_t=rep(0,indexes.rows)
    labels_betas_t=rep("label",indexes.rows)
    
    # Loop over all the number of possible combinations
    for(combination_index in 1:indexes.rows){
      
      #Obtain a vector containing the indexes, l_1,l_2,...,l_t in our notation:
      l_values=indexes[combination_index,]
      
      #Create the label as a string of the form "l_1,l_2,...,l_t"
      labels_betas_t[combination_index]=paste(as.character(indexes[combination_index,]),collapse = ",")
      
      #Now we can obtain the vector m=(m_1,...,m_p). The value for m_0 changes with n and will be added later in the summation
      m=rep(0,p)
      for (i in 1:p){
        m[i]=sum(l_values==i)
      }
      
      #Finally we can apply the general formula:
      for (j in 1:h_1){
        aux=0
        for (n in t:q){
          m_n=c(n-t,m)
          aux=aux+g[n+1]*factorial(n)/prod(factorial(m_n))*prod(w[j,]^m_n)
        }
        betas_t[combination_index]=betas_t[combination_index]+v[j+1]*aux
      }
    }
    
    #Include all the values and the labels for each t in the final vector 
    betas=c(betas,betas_t)
    labels=c(labels,labels_betas_t)
  }
  
  #Finally set the betas vector as a row matrix and use the labels as the column names
  betas=t(as.matrix(betas))
  colnames(betas)=labels
  return(betas)
}
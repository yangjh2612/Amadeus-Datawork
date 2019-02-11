# Script to perform cross valudation with given distributions (Levy alpha stable and AEP/Subbotin implemented)

# Loading local dependencies                        #TODO: Is this the correct way of loading local dependencies?
# fitting functions from fitting.levy package
devtools::load_all("../fittinglevy")
# fitting functions from fitting.AEP package
devtools::load_all("../fittingAEP")

## cross validation  
CV_fun <- function(n_fold, n_rep, uni_data, distribution="Levy"){
  # Cross validation function
  # Arguments:
  #     n_fold:         int             - number of subsets for cross-validation
  #     n_rep:          int             - number of repetitions 
  #     uni_data:       list of float   - data to fit
  #     distribution:   string          - Indicator which distribution to fit
  # Returns:
  #     List of float of length n_rep   - log-likelihood sums of out of sample values for, 
  #                                       summed over all subsets, list values for each of the
  #                                       repetitions
  
  # Prepare result lists
  test_list <- list()
  test_list_all <- list()
  
  # Loop over repetitions
  for(k in 1:n_rep){ 
    
    # Prepate data for this repetition
    # Permutate sequence
    uni_data_s <- sample(uni_data)
    # Split in n_fold subsets
    folds <- cut(seq(1, length(uni_data_s)), breaks = n_fold, labels=FALSE)
    
    # Loop over subsets
    for(i in 1:n_fold){ 
      print(paste(k, "and", i))
      
      # Assign training and test set for out-of-sample validation with current subset
      testID <- which(folds==i,arr.ind=TRUE) 
      testD <- uni_data_s[testID]
      trainD <- uni_data_s[-testID]
      
      # Obtain fit on training set and compute log likelihood of test subset
      if (distribution=="Levy") {           # Levy alpha-stable
        trained <- Levy_fun_QT(trainD)
        test_result <- sum(dstable(testD, trained[1], trained[2], trained[3], trained[4], log = T)) # log-likelihood of levy
      } else if (distribution=="AEP") {     # Asymmetric exponential power (Subbotin)
        trained <- Sub_fun_LM(trainD)
        test_result <- sum(log(pdfaep4(testD, trained)))  # log-likelihood of Sub
      } else {                              # Catch unregognized strings
        print(paste("Unrecognized distribution requested", distribution, sep=" "))
        quit(status=1)
      }
      
      # Record result
      test_list[[i]] <- test_result
    }
    
    # Record sum over all validations (for each subset)
    test_list_all[[k]] <- sum(unlist(test_list)) # the sum of all n-fold likelihood
    
  }
  
  print(test_list_all)
  return(test_list_all)
}

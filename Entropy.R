# Entropy  Mike Lennon


##### Question 1 #####
#Create a function entropy() that takes a vector d as input and returns a single
#numeric value that is the entropy E(d) of the vector:

entropy <- function(inputVector){
  
  x<- unique(inputVector) # print(x)
  total<-length(inputVector) # print(total)
  
  numOccur <- numeric()
  for(i in 1:length(x)){
    count <- 0
    for(j in 1: length(inputVector))
      if(x[i] == inputVector[j])
        count <- count + 1
    numOccur[length(numOccur)+1]   <- count
  }
  #print(numOccur)
  sum <- 0
  probability <- numeric()
  for(k in 1:length(numOccur)){
    probability[k] <- numOccur[k]/total #print(probability)
    sum<- sum + probability[k]*log2(probability[k])
  }
  print(sum)
  entropy <- -1* sum

  entropy
  
}


#testEntropy<- c("a","a","a","a","a","a","b","b","b","b")
#entropy(testEntropy)

infogain <- function(d,a){
  
  entropyOriginal <-entropy(d)
  totalN <- length(d)
  
  uniqueD <- unique(d) #unique elements in d vector
  uniqueA <- unique(a) #unique elements in the partition vector
  
  numOccurEachA <- numeric()# vector total number times each a element occurs
  for(i in 1:length(uniqueA)){
    count <- 0
    for(j in 1: length(a))
      if(uniqueA[i] == a[j])
        count <- count + 1
    numOccurEachA[i] <- count
    #numOccurEachA[length(numOccurEachA)+1]   <- count
  }
  # for every a go to parallel d list and note each d value tally and sum (Matrix)
  test <- matrix(0,length(uniqueA),length(uniqueD)) # numUniqueA X numUniqueD matrix
  #test <- matrix(0,nrow=2,ncol=3)
  # fill the matrix rows get a columns get d value count
  for(i in 1:length(uniqueA)){
    for(j in 1:length(a)){
      if(a[j]==uniqueA[i]){ #if the a vector element matches the a row we are filling
        test[i,match(d[j],uniqueD)]<- test[i,match(d[j],uniqueD)] + 1 # add one to the count
      }
    }
  }
  sumsOfRows <- apply(test,1,sum) #sum up rows of matrix the Nj
  sumsofRowsDivByTotal <- sumsOfRows/totalN #This gives Nj/N
  
  entropyAddedEachRow <- numeric()
  for( i in 1:length(uniqueA)){
    sumEnt <- 0
    for(j in  1:length(uniqueD)){
     if(test[i,j] != 0){
       p <- test[i,j]/sumsOfRows[i]
       sumEnt <- p * log2(p) + sumEnt
       # mult by the nj/n factor now
     }
     sumEnt <- -1* (numOccurEachA[i]/totalN)*sumEnt
     entropyAddedEachRow[i] <- sument
     #entropyAddedEachRow[length(entropyAddedEachRow)+1] <- sument
    }
  }
  #find element in unique that is the col num
  #match('Patricia',queue)
  
  sumPartitionsEntropy <- 0
  for(i in 1:length(entropyAddedEachRow))
    sumPartitionsEntropy <- sumPartitionsEntropy + entropyAddedEachRow[i]
  
  infoAdded <- entropyOriginal - sumPartitionsEntropy
  infoAdded
  
  
}



decide <- function(inputFrame,columnTarget){
  
  result <- list()
  infoGains <- numeric()
  
  for(i in 1:ncol(inputFrame)){
    if(i != columnTarget){
      d1 <- inputFrame[, c(i)]
      infoGains[i] <- infogain(d1)
    }
    if(i== columnTarget)
      infoGains[i] <- 0
  }
  
  maxIndex<- 1
  for(i in 1:length(infoGains)){
    if(infoGains[i] > infoGains[maxIndex])
      maxIndex <- i
  }
  
  result[1] <- list(max = maxIndex)
  result[2] <- infoGains
  
  result
  
}

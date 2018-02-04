# Applied Statistical Programing 
# Homework 3 - Problem Set 2
# Gangyi Sun (441748)


# QUESTION 1

# The DigitDist function takes as input x, which is a matrix or vector of election returns.
# The function returns a matrix containing the frequency of each integer occruing in the first significant digit in 
# all elements of x (the full digit distribution). 
DigitDist<-function(x){
  x<-as.character(x)        # re-casts numeric vector/matrix x as a character matrix
  x<-strsplit(x,"")         # splits each number in x into a sub-string
  y<-NULL
  for (i in 1:length(x)){   # obtains the integer in the first significant digit of each element of x 
    y[i]<-x[[i]][1]
  }
  y<-as.numeric(y)          # re-casts the characters into numerics 
  IntCount<-matrix(0,nrow=9,ncol=1)   # crates a matrix IntCount, which will contain the integer frequency information 
  rownames(IntCount)<-seq(1:9)
  colnames(IntCount)<-'Frequency'
  for (i in 1:9){           # obtains integer frequencies in y 
    count<-0
    for (n in 1:length(y)){
      if (y[n]==i){
        count<-count+1
      }
    }
    IntCount[i]<-count
  }
  return(IntCount)          # returns IntCount as the output of the function
}

# The StatM function takes as input x, which is a matrix of first significant digit integer 
# frequencies (output from DigistDist).
# The function returns the Leemis' m statistic.  
StatM<-function(x){
  Mi<-NA
  for (i in 1:9){           # evaluates Xi-log10(1+1/i) for each integer i, stored in vector Mi
    Xi<-IntCount[i]/sum(IntCount)     # Xi is the proportional frequency of integer i observed in x 
    Mi[i]<-Xi-log10(1+1/i)
  }
  m<-max(Mi)                # the Leemis' m statistic is the maximum value contained in vector Mi
  return(m)                 # returns m as the output of the function 
}

# The StatD function takes as input x, which is a matrix of first significant digit integer 
# frequencies (output from DigistDist).
# The function returns the Cho-Gains' d statistic.  
StatD<-function(x){
  Di<-NA
  for (i in 1:9){           # evaluates (Xi-log10(1+1/i))^2 for each integer i, stored in vector Mi
    Xi<-IntCount[i]/sum(IntCount)     # Xi is the proportional frequency of integer i observed in x 
    Di[i]<-(Xi-log10(1+1/i))^2
  }
  d<-sqrt(sum(Di))          # the Cho-Gains' d statistic is the sum of all values contained in vector Di
  return(d)                 # returns d as the output of the function 
}

x<-c(1234,125356,1,234,222,3,34567,4567,498,5555,5987,61234,671234,71,74,88,80,95,99999,9)
xDist<-DigitDist(x)
StatM(xDist)
StatD(xDist)


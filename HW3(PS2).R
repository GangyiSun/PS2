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
StatM<-function(IntCount){
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
StatD<-function(IntCount){
  Di<-NA
  for (i in 1:9){           # evaluates (Xi-log10(1+1/i))^2 for each integer i, stored in vector Mi
    Xi<-IntCount[i]/sum(IntCount)     # Xi is the proportional frequency of integer i observed in x 
    Di[i]<-(Xi-log10(1+1/i))^2
  }
  d<-sqrt(sum(Di))          # the Cho-Gains' d statistic is the sum of all values contained in vector Di
  return(d)                 # returns d as the output of the function 
}

# The CalcBenfordLaw function is the answer to question 1. 
# The CalcBenfordLaw function takes as input x, m and d. 
# x is a matrix or vector of election returns.
# m is a boolean. if m==true, the function will calculate and report the Leemis' m statistic.
# d is a boolean. if d==true, the function will calculate and report the Cho-Gains' d statistic.
# The function returns the full first-digit distribution of elector returns x, the m statistic if m==true,
# and the d statistic if d==true. 
CalcBenfordLaw<-function(x,m,d){
  Lm<-NULL
  Cd<-NULL
  Dist<-DigitDist(x)        # obtains first-digit integer distribution of x 
  if (m==TRUE){             # if m==true, calculate m statistic, save as Lm
    m<-StatM(Dist)
    Lm<-c(Lm,m)
  } 
  if (d==TRUE){
    d<-StatD(Dist)
    Cd<-c(Cd,d)             # if d==true, calculate d statistic, save as Cd
  }
  result<-list(Lm,Cd,Dist)
  names(result)<-c("Leemis' m statistic","Cho-Gains' d statistic", "Full Digit Distribution")
  return(result)            # returns the m statistic, d statistic and full digit distribution as a list
}

# Sample data used to check that the functions work. 
x<-c(1234,125356,1,234,222,3,34567,4567,498,5555,5987,61234,671234,71,74,88,80,95,99999,9)
xDist<-DigitDist(x)
xDist
StatM(xDist)
StatD(xDist)
CalcBenfordLaw(x,T,T)



# QUESTION 2

# The function mSig takes as input m, which is the Leemis' m stastic.
# The function returns characters containing *s that indicate the level of significance of the m statistic.
mSig<-function(m){
  mSig<-NULL
  if (m>0.851 & m<=0.967){    # rules to determine the level of significance of the m statistic
    mSig<-'*'
  }else if (m>0.967 & m<=1.212){
    mSig<-'**'
  }else if (m>1.212){
    mSig<-'***'
  }
  return(mSig)                # returns the level of significance of the m statistic, indicated by *s
                              # returns an empty string if p-value greater than 0.10. 
}

# The function dSig takes as input d, which is the Cho-Gains' d stastic.
# The function returns characters containing *s that indicate the level of significance of the d statistic.
dSig<-function(d){
  dSig<-NULL
  if (d>1.212 & d<=1.330){    # rules to determine the level of significance of the d statistic
    dSig<-'*'
  }else if (d>1.330 & d<=1.569){
    dSig<-'**'
  }else if (d>1.569){
    dSig<-'***'
  }
  return(dSig)                # returns the level of significance of the d statistic, indicated by *s
                              # returns an empty string if p-value greater than 0.10. 
}

# The print.benfords function is the answer to part 1 of qestion 2. 
# The print.benfords function takes as input x, which is a matrix or vector of election returns.
# The function prints a table containing the values of the m and d statistic and information regarding each 
# statistics' level of significance.
print.benfords<-function(x){
  Dist<-DigitDist(x)          # obtains first-digit integer distribution of x 
  m<-round(StatM(Dist),4)     # obtains the Leemis' m statistic, rounded to 4 d.p.
  d<-round(StatD(Dist),4)     # obtains the Cho-Gains' d statistic, rounded to 4 d.p.
  mSigValue<-mSig(m)          # obtains the level of significance of the m statistic
  dSigValue<-dSig(d)          # obtains the level of significance of the d statistic
  mFinal<-paste(m,mSigValue,sep="")     # combines m statistic and its level of significance as a character
  dFinal<-paste(d,dSigValue,sep="")     # combines d statistic and its level of significance as a character
  
  table<-as.data.frame(c(mFinal,dFinal))    # creates a dataframe containing the m and d statistics, and 
                                            # their level of significance
  rownames(table)<-c("Leemis' m statistic,","Cho-Gains' d statistic,")     
  colnames(table)<-',Value'                 # labels the table, the commas are used to split information 
                                            # into columns in the csv output
  print(table)                              # prints the table required containing m and d statistics 
  cat("*** p<0.01; ** p<0.05; * p<0.1")     # prints the legend required explaning the asterisk's 
}

# The output.benfords function is the answer to qestion 2. 
# The output.benfords function takes as input x and directory.
# x is a matrix or vector of election returns.
# directory is a string indicating the location of a folder on a computer.
# The function creates a csv file in the folder indicated by directory. The csv file contains the values of the 
# m and d statistic and information regarding each statistics' level of significance.
output.benfords<-function(x,directory){
  sink(directory)           # creates csv file in directory indicated
  print.benfords(x)         # prints the required table and caption in the csv file
  sink()                    
}

# Sample data used to check that the functions work. 
x<-c(rep(9,2000),1,2,3,4,5,6,7,8)
print.benfords(x)
output.benfords(x,"~/Documents/GitHub/PS2/Table.csv")


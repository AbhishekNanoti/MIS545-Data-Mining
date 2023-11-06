userInput <- strtoi(readline("Enter any positive integer : "))

primeCheck <- 2L
primesVector <- c(primeCheck)

while(length(primesVector) < userInput){
  primeCheck <- primeCheck+1
  isPrime <- TRUE
  for (i in 2:(primeCheck ^ 0.5)){
    if (primeCheck %% i == 0){
      isPrime <- FALSE
    }
  }
  if(isPrime == TRUE){
    primesVector <- c(primesVector,primeCheck)
  }
}
print(primeCheck)
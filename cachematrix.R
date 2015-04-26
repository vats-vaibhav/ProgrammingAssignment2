##--------------------------------------------------------------------
## GENERAL INTRODUCTION
##--------------------------------------------------------------------

##  The Purpose of these programs is to store the computed inverse of matrix 
## in cache so that it can be accessed faster and computation time is saved as
## computation time is very high for large datasets.

##--------------------------------------------------------------------
## FUNCTION EXPLANATION
##--------------------------------------------------------------------
##  makeCacheMatrix Function is collection of Four function. 
##  setcache -> This function set the value in the Cache.
##  getcache -> This function retrives the value from the cache.
##  setmatrix ->This function set the value of matrix. 
##  getmatrix ->This function retrives the matrix value in numeric format.Since it 
##              returns a numeric matrix we can use the SOLVE function on the output
##              of this matrix. 
##
##  cachesolve function returns the cache value, if the value is already present else
##  value is updated in cache.
##  For reference these functions are similar to getter and setter 
##  methods of JAVA programming.
##
##
## In the assignment,it is written that running a code is not required 
## but for a better understanding of how the program works i have attached 
## the output as well  

##----------------------------------------------------------------------
## OUTPUT OF PROGRAM FOR REFERENCE AND BETTER UNDERSTANDING OF CODE
##---------------------------------------------------------------------
## a <- makeCacheMatrix( matrix(c(7,8,9,10), nrow = 2, ncol = 2) );
## cacheSolve(a)
## new matrix updaing cache
##[,1] [,2]
##[1,]   -5  4.5
##[2,]    4 -3.5
## cacheSolve(a)
##getting cached data
##[,1] [,2]
##[1,]   -5  4.5
##[2,]    4 -3.5
##--------------------------------------------------------------------------

## makeCacheMatrix Function

makeCacheMatrix <- function(x = matrix()) {

  #Initilised a varilable cache value to store the inverse of matix
  cachevalue <- NULL
  
  #Store the value in the cachevalue.
  # x is the Inversed matrix that is passed from cachesolve function.
  setcache<-function(x){
    cachevalue<<-x
  }
  
  #returns the cachedvalue
  getcache <-function() {
    cachevalue
  }
  
  # store the matix
  setmatrix <-function(newmatrix) {
    x <<- newmatrix
  }
  
  #returns the stored matrix
  getmatrix <-function() {
    x
  }
  
  #Function returns the list. Each element named is a function.
  list(setcache=setcache, getcache=getcache,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
  
}


## The below function determines the Inverse of matrix and retrives the value
## from Cache if it is present else it is stored in the cache for next reference.

cacheSolve <- function(x, ...) {
  
  ## Check if the inverse if matrix is present in cache.
  cachevalue <-x$getcache()
  
  ## If the data is present in cache then return this data.
  if(!is.null(cachevalue)){
    message("getting cached data")
    return(cachevalue);
  }
  
  ## If data is not present then update the cache
  message("new matrix updaing cache")
  
  ##get the matrix which needs to be inversed. This function will return the numeric
  ## matrix on which solve function can work.
  z <- x$getmatrix()
  
  ## Do the inverse of matrix, Update the cache with new value and return the inverse.
  cachevalue<-solve(z, ...)
  x$setcache(cachevalue)
  return(cachevalue);
  
}

## End of Assignment.
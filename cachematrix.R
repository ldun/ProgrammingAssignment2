## Put comments here that give an overall description of what your
## functions do

## This will create a matrix that can cache an inverse of itself


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL                                  ## creating a placeholder
  
  set<-function(y){                          ## copies the calling environment and sets it to the function environment                
    x<<-y
    m<<-NULL
  }
  
  get<-function() x                          ## returns the matrix x
  setmatrix<-function(solve) m<<- solve      ## passes the inverse as an argument
  getmatrix<-function() m                    ## returns the value of m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Computes the inverse of the matrix created in makeCacheMatrix

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()                          ## if above is not NULL, it will return the cached inverse
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()                           ## will be used if matrix is not cached
  m<-solve(matrix, ...)
  x$setmatrix(m)                            ## will calculate inverse ofmatrix
  m                                         ## returns the inverse
}


## Return a matrix that is the inverse of 'x'


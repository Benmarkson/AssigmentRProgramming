
library(matlib)
MakeCacheMatrix <- function(matx = matrix()){
  if(ncol(matx) == nrow(matx) && det(matx) != 0){
    set <- function(u){
      matx <<- u
      m <<- NULL
    }
    get <- function() matx
    setinverse <- function() m <<- inv(matx)
    getinverse <- function() m
    list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }else{
    return (message("matrix is not inversible"))
  }
}

CacheSolve <- function(matx, ...){
  m <- matx$getinverse
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }else{
    data <- matx$get
    m <- inv(data, ...)
    matx$setinverse(m)
    m
  }
}
## This function creates a matrix object that is able to cache its inverse.
## Basically a list containing functions to set/get the value of a matrix, or set/get the inverse value of a matrix

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setinvmatrix<-function(solve) m<<- solve
getinvmatrix<-function() m
list(set=set, get=get,
   setinvmatrix=setinvmatrix,
   getinvmatrix=getinvmatrix)
}

## This function checks to see if a cached inverted matrix already exists, if so, it will return that value.  If not, it will compute the inverse and cache it.

cacheSolve <- function(x, ...) {
    m<-x$getinvmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setinvmatrix(m)
    m
}
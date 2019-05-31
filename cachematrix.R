## Computing a inversion of Matrix is a costly in terms of
## computing cost. So here we are going to use the the concept
## Caching. In caching once we have calculated the inverse of 
## matrix we can store the result in cache for future requirement
## rather than recomputing it. 

## For Caching the inverse of matrix we need two function:

## 1.makeCacheMatrix : for creating a "matrix" object that can
##   cache its inverse
## 2.cacheSolve: for providing retrieved inverse from cache memory
##   in case the matrix has not changed or if the matrix has changed
##   then it will calculate the inverse of matrix


## makeCacheMatrix will create a Matrix object for caching the inversion of Matrix

makeCacheMatrix <- function(x = matrix()) {
                  inversion<-NULL
                  set<-function(y){
                    x<<-y
                    inversion<<-NULL
                  }
                  get<-function() x
                  setinversion<-function(solve) inversion<<-solve
                  getinversion<-function() inversion
                  list(set=set,get=get,
                       setinversion=setinversion,
                       getinversion=getinversion)

}


## It will return the inverse of the given matrix
## or it will recompute the inverse if the values of matrix have changed


cacheSolve <- function(x, ...) {
              inversion<-x$get()
              if(!is.null(inversion)){
                  message("getting the cached inversion of matrix")
                  return(inversion)
              }
              a_data<-x$get()
              inversion<-solve(a_data,...)
              inversion
}
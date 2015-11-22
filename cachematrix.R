## This function creates a special "matrix" object that can cache its inverse.
## The list below contains functions that:
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse

## x= a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
    # "<<-" is used to assign a value to an object in an environment 
    # different from the current environment.
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  ##this list is used as the input to cacheSolve()
}



##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache which is faster.

cacheSolve <- function(x, ...) {
  ## x = output of makeCacheMatrix()
  ## Below is the inverse of the original matrix input to makeCacheMatrix()
         
         inv = x$getinv()
         
           # checks if the inverse has already been calculated
           if (!is.null(inv)){
                 # return = from the cache and skips resolving again. 
                   return(inv)
             }
         
           # This solves the inverse 
           mat.data = x$get()
           inv = solve(mat.data, ...)
           
           # This sets the value of the inverse in the cache 
             x$setinv(inv)
          
           return(inv)
      }

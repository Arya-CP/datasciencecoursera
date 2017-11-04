## Assignment: Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

## initialize the cached matrix object ('inv') to NULL  
  inv <- NULL
  
# set and get the value of a matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL         ## the matrix has changed: reinitialize to NULL
    }
  get <- function() x
# set and get the value of a cached inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
# list of all get,set,getinverse, setinverse functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix.  If the inverse has already been calculated
## (and the matrix has not changed),then cacheSolve will retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()     # check if a cached object is available
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)              # return the inverse
  }
  # if thereee is no cached object
  mat <- x$get()
  inv <- solve(mat, ...)          # calculate the inverse 
  x$setinverse(inv)
  inv                        # Return inverse matrix
}
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
my_matrix$getInverse()

cacheSolve(my_matrix)
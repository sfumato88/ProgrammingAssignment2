## This function cache the results of matrix computation and allow the cache to be retrieved


## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function () x
  set_inv_matrix <- function(solve) m <<- solve
  get_inv_matrix <- function() m
  list( set = set, get = get,
        set_inv_matrix = set_inv_matrix,
        get_inv_matrix = get_inv_matrix)
  
}


## This function computes the inverse of special matrix returned by makeCacheMatrix. 
## If the inverse has arleady been calculated (and the matrix has no changed), 
## the the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inv_matrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  inv_matrix <- x$get()
  m <- solve(inv_matrix, ...)
  x$set_inv_matrix(m)
  m
}

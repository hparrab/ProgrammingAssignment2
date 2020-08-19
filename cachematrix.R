## The script contains two functions, the first makeCacheMatrix creates an R 
## object that stores a matrix and its inverse and the second, cacheSolve calculates and/or retrieves the inverse of the matrix from an object of type makeCacheMatrix()

## This function builds and returns four functions (set(),get(),setsolve(),getsolve())) within a list to the parent environment. It also returns two data objects x and i

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function sees if a matrix with the inverse of x is in the cache and if so, its returns it with a message before, and if it's not there, its calculates the inverse of matrix x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
}

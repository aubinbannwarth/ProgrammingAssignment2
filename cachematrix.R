## The makeCacheMatrix function takes a square matrix as an input, and returns 
## a list of 4 functions that can then be passed as an argument to cacheSolve.
## cacheSolve then returns the inverse of the matrix, and saves it to
## the cache. If we call cacheSolve again on the same matrix, it gets the answer
## from the cache.
## Note that in order for cacheSolve to work, the matrix must be invertible.

## We can simply modify the code in the example for the mean of a vector.
## We use makeVector as a template for makeCacheMatrix, 
## and cachemean as a template for cacheSolve.
## We will simply replace the variable "m" for mean with "I" for inverse,
## replace "setmean" and "getmean" with "setinv" and "getinv",
## and in the cacheSolve function, we replace mean() with solve()

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinv <- function(inv) I <<- inv
  getinv <- function() I
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Our code for cacheSolve is simply adapted from cachemean in the example.

## Example: if we want to compute the inverse of matrix(c(1,1,1,0), 2, 2),
## we can first use makeCacheMatrix and save it as a variable "M":

## M <- makeCacheMatrix(matrix(c(1,1,1,0), 2, 2))

## We then use cacheSolve to compute the inverse:

## cacheSolve(M)

## If we run this last command for the first time, the inverse will be computed
## and saved to the cache. Every time we run it again, cacheSolve will get the 
## inverse from the cache and print the message "getting cached data".

cacheSolve <- function(x, ...) {
  I <- x$getinv()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinv(I)
  I
}

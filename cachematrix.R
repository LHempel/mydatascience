# As matrix inversion can be a costly computation, it may be benificial 
# to cache the inverse of matrix rather than compute the inverse repeatedly.
# By creating two special functions this benefit can be realized


# The first function, makeCacheMatrix, creates an inverted matrix 
# and caches this inversion. 
# It does that by creating a list containing a function to
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inversed matrix
# 4. Get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
    
}


# The second function, cacheSolve, creates the inverse of the matrix 'x'
# created by the first function.
# However, it first checks to see if the inverted matrix has been 
# created before. If so, it gets the inverted matrix from the cache
# and skips the creation of it. 
# Otherwise, it inverts the matrix and sets its value in the cache
# via the setinv function

cacheSolve <- function(x, ...) {

  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)  #inverting the matrix
  x$setinv(inv)         #caching the inverted matrix
  inv 
  
}

# As matrix inversion require a lot of CPU cycles the following two functions will try to use cached values.

# Like the makeVector function described in the assignment description does the
# makeCacheMatrix function create a list to

# 1. set the value of a matrix
# 2. get the value of a matrix
# 3. set the value of inverse of a matrix
# 4. get the value of inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# Assuming that the matrix is always invertible, the below function returns the inverse of that matrix.
# It first checks if the inverse is already been cached. If yes, it immediatly retrieves the value
# If not, it computes the inverse and calls the setinverse function to set the value.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

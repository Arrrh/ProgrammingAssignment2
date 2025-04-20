## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

### Creates a special "matrix" object that can cache its inverse
### Contains 4 function : get, set, setinverse & getinverse


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

### Computes the inverse of special matrix and checks if inverse already cached
### If cached, returns the cached value with message 
### But if not then computes inverse, caches it and returns it


# Create a test matrix
test_matrix <- matrix(c(4,3,3,2), nrow=2, ncol=2)

# Create cacheable matrix object
cache_matrix <- makeCacheMatrix(test_matrix)

# First run computes inverse
cacheSolve(cache_matrix)

# Second run retrieves cached inverse
cacheSolve(cache_matrix)  # Should show "getting cached data" message


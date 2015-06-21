## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # is is our inverse
  i <- NULL
  set <- function(y) {
    # If we set a new matrix, clean any old cache of the inverse that may
    # have been stored
    x <<- y # our cache of the matrix
    i <<- NULL # our cache of the inverse, initialized as NULL
  }
  get <- function() x # get simply returns the matrix
  setinverse <- function(matInv) i <<- matInv # just stores the inverse
  getinverse <- function() i # just returns the inverse
  # returns a list with references to the functions defined above
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Try to get the inverse from the cache
  i <- x$getinverse()
  if(!is.null(i)) {
    # Cache hit! Returns cached data
    return(i)
  }
  # Cache miss! compute inverse, stores it, and returns
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

## Here's a test function that executes solve and cacheSolve 100000 times each
## and prints timing information
## Just source this file and call testIt()
testIt <- function() {
  m <- matrix(c(2,3,4,6,2,
                4,5,6,6,7,
                8,9,3,3,4,
                5,4,3,1,3,
                7,3,5,6,3),nrow=5,ncol=5)
  start <- Sys.time ()
  for (t in 1:100000) {
    i <- solve(m)
  }
  timeWithoutCache = Sys.time () - start
  message(sprintf("without cache = %f\n",timeWithoutCache))
  cm = makeCacheMatrix(m)
  start <- Sys.time ()
  for (t in 1:100000) {
    ci = cacheSolve(cm)
  }  
  timeWithCache = Sys.time () - start
  message(sprintf("with cache = %f\n",timeWithCache))
}
 
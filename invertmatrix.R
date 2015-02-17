makeCacheMatrix <- function(m = matrix(c(1:4), nrow=2, ncol=2)) {

  i <- NULL
  
  set <- function(y) {
    m <<- y
    i <<- NULL
  }

  get <- function() m
  
  setinvert <- function(solve) i <<- solve
  
  getinvert <- function() i
  
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)

}  


cacheSolve <- function(m, ...) {
  
  i <- m$getinvert()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- m$get()
  
  i <- solve(data, ...)
  
  m$setinvert(i)

  i

}


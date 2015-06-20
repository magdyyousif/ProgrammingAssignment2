## Put comments here that give an overall description of what your
## functions do

## a function to create a special matrix that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  changed <- FALSE
  set <- function(y) {
    if (!identical(x,y))
    {
      x <<- y
      i <<- NULL
      changed <- TRUE
    }
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  IsChanged <- function() changed
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       IsChanged = IsChanged)
}


## a function to check for casedcached inverse and return it 
## or calculate it and cache it

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i) && !x$IsChanged()) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

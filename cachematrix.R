## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) { ##sets the value of the matrix
    x <<- y
    s <<- NULL
  }
  get <- function() { ##gets the value of the matrix
    x
  } 
  setinverse <- function(solve) {  ##set the value of the inverse
    s <<- solve
  }
  getinverse <- function() s ##gets the value of the inverse
  list(set = set, get = get,  ##creates special matrix type list of functions
       setinverse = setinverse,
       getinverse = getinverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) { ##checks if "s" is null, if yes then the inverse hasn't cached
    message("getting cached data")  
    return(s)       ## exits without parsing the following code, returning s
  }
  data <- x$get()   ## if s is non-null, puts cached inverse into data
  s <- solve(data, ...)  ## solves matrix, creating inversion
  x$setinverse(s)  ##caches the inverse
  s   ##returns the mean
  
}

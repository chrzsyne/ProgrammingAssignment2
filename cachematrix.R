## Cache the inverse of a matrix using functions


## Use makeCacheMatrix function to create an object that can cache the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##Set the cache inverse of the matrix x using the cacheSolve function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("solving for cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat, ....)
  x$setInverse(j)
  j
}


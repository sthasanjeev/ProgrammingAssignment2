## This function set Matrix, get Matrix, set Inverse of the matrix and get Invers ## of the matrix provided.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse , getinverse = getinverse)
}


## This function computes the inverse of the matrix created by makeCacheMatrix function.
## The function first checks whether the inverse has already been calculated or not.
## If not cached it will calculate inverse and return.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse matrix.....")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

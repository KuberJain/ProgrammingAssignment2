## makeCacheMatrix creates a special matrix object, and then what the cacheSolve does is it
## calculates the inverse of the matrix.If the matrix inverse has already been calculated, it will instead
## find it in the cache and return it, and not calculate it again.

makeCacheMatrix <- function(x = matrix()) {
nv <- NULL
  set <- function(y) {
    mat <<- y
    inv <<- NULL
  }
  get <- function() mat
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
inv <- mat$getinverse()
if(!is.null(inv)) {
message("Get cache data.")
return(inv)
}
data <- mat$get()
inv <- solve(data)
mat$setinverse(inv)
inv

## The function cacheSolve returns the inverse of the matrix created with the makeCacheMatrix function.

## If the cached inverse is available, cacheSolve retrieves it, and if not, it computes, caches, and returns it


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        elc <- x$getreverse()
  if (!is.null(elc)) {
    message("getting cached reverse matrix")
    return(elc)
  } else {
    elc <- solve(x$get())
    x$setreverse(elc)
    return(elc)
  }
}

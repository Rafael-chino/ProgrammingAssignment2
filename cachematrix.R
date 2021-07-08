## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## I started making this function by putting makeCacheMatrix at the start
## After that I tried to store the function: set, get, setInverse, getInverse
## To my Null in order to run properly
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function
## The main function here is the cacheSolve. I used the same variable so that I will know that it will work just fine
## As you can see my input is to solve the inverse and getting the cached data
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv ## Return a matrix that is the inverse of 'x'
}

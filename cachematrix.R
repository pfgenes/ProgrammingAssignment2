## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function creates a special "matrix" 
## It is really a list containing a function that:

## sets the value of the matrix
## gets the value of the matrix
## sets the value of the inverse
## gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix<-NULL
  
  set <- function(y) {
    x <<- y
    inverse_matrix<<-NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inverse_matrix <<- inverse
  get_inverse <- function() inverse_matrix
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Write a short comment describing this function
## The function below solves the inverse of the special "matrix"
## It first checks to see if the inverse was already solved. 
## If it has been solved, it gets the inverse from the cache
## Otherwise, it solves the inverse and sets the inverse value in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$get_inverse()
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
    
  }
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$set_inverse(inverse_matrix)
  inverse_matrix
}

## These functions create a special "matrix" object that can set a matrix, 
## get the matrix, and cache its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) matrix_inverse <<- inverse
  get_inverse <- function() matrix_inverse
  list(set=set, get=get, set_inverse=set_inverse,
       get_inverse=get_inverse)
}


## This function computes & returns the inverse of the special "matrix" 
## object if the inverse has not yet been computed; else it returnes the
## cached inverse.
## If the matrix is not inversible, NULL is returned along with a message. 

cacheSolve <- function(x, ...) {
  matrix_inverse <- x$get_inverse()
  if(!is.null(matrix_inverse)) {
    message('Getting cached inverse...')
  }
  matrix <- x$get()
  inverse <- tryCatch({
    solve(matrix)
  },
  error = function(e) {
    message(e$message)
    NULL
  }
  )
  x$set_inverse(inverse)
  inverse
}

## cacheSolve and makeCacheMatrix calculate the inverse of a square matrix
## and store it in cache so it can be retrieved without recalculating.

## Usage note: Matrix must be square.

## matrix -> matrix
## makeCacheMatrix creates a matrix object that can store its inverse. 
makeCacheMatrix <- function(orig_mx = matrix()) {
  
  # resets the stored matrix
  stored_mx <- NULL
  
  # sets the matrix object to the passed matrix
  set <- function (passed_mx) {
    orig_mx <<- passed_mx
    stored_mx <<- NULL
  }
  
  # returns the original matrix
  get <- function () {
    return(orig_mx)
  }
  
  # stores the passed inverse matrix
  setinversemx <- function (solved_mx) {
    stored_mx <<- solved_mx
  }
  
  # returns the stored inverse matrix
  getinversemx <- function () {
    return(stored_mx)
  }
  
  list(set = set, get = get,
       setinversemx = setinversemx,
       getinversemx = getinversemx)
}


## matrix -> matrix
## cacheSolve returns the inverse of the given matrix. 
## Retrieves matrix from cache if it has been already calculated.
cacheSolve <- function(cachemade_mx, ...) {
  
  # check cache for a stored result
  inv_mx <- cachemade_mx$getinversemx()
  
  # if found: inverse matrix is returned
  if(!is.null(inv_mx)) {
    return(inv_mx)
  }
  
  # if not found: inverse matrix is calculated, stored and returned
  else {
    mx_tosolve <- cachemade_mx$get()
    inv_mx <- solve(mx_tosolve, ...)
    
    cachemade_mx$setinversemx(inv_mx)    
    return(inv_mx) 
  }
}

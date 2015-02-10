## Kay Dee: 10 Feb 2015

# The function makeCacheMatrix takes a matrix input and 
# creates a list of 4 items; given below description of the 4 list items
# set:    sets the data to input data matrix 
#         and resets stored inverse in parent environ
# get:    returns the value of input data matrix to the caller
# setinv: sets the stored inverse in parent environ to soln
# getinv: returns stored inverse from parent environ to the caller

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL # sets the placeholder 'inverse'
  set <- function(matx) {
    mat <<- matx
    inv <<- NULL
  }
  get <- function() 
    mat
  setinv <- function(soln) 
    inv <<- soln
  getinv <- function() 
    inv
  list(set = set, get = get, getinv = getinv, setinv = setinv)
}


## Kay Dee: 10 Feb 2015

# The function cacheSolve takes output of makeCacheMatrix (dmat) as input and 
# returns the inverse of the data matrix stored in dmat
# If the inverse was calculated earlier, it returns the caled value
# else it return the freshly calculated inverse


cacheSolve <- function(dmat, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- dmat$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- dmat$get()
  inv <- solve(data, ...)
  dmat$setinv(inv)
  inv
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {      #Here is defined the x variable (empty)
  inv <- NULL
  set <- function(y) {
    x <<- y                                      #This is assigned a value from some parental env to x variable
    inv <<- NULL
  }
  get <- function() x                            #This function only retrieves the x value
  setsolve <- function(solve) inv <<- solve      #This function assigns the solve value to the "inv" variable somewhere in parent env
  getsolve <- function() inv                     #This line retrieves the the inverted matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()
  if(!is.null(inv)) {                           #If inverted matrix already exist (cached), the message and matrix will be retrieved
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)                       #Here the matrix is inverted
  x$setsolve(inv)                               #Storing inverted matrix
  inv
}

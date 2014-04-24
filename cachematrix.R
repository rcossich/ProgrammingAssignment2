## Put comments here that give an overall description of what your
## functions do
## Commented by Ricardo Cossich 2014-04-22 21:05 (GMT -6)
## Function makeCacheMatrix creates a special object
## that receives a matrix and provides methods to perform
## the following actions:
## (1) set a new value for the matrix
## (2) get the matrix value
## (3) set the value of the inverse matrix (manual call)
## (4) get the value of the stored inverse matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL  ## variable to save inverse matrix (if possible)
  set <- function(y) { ##called to replace original matrix
    x <<- y 
    i <<- NULL ## make sure inverse matrix is set to null
  }
  get <- function() x  ## returns matrix inside the object
  setsolve <- function(solve) i <<- solve ## to assign a solve variable within the object
  getsolve <- function() i  ## returns the inverse matrix in the object
  list(set = set, get = get,  ## creating the list for future references
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
## commented by Ricardo Cossich 2014-04-22 09:20 (-6 GMT)
## This function uses the object created by makeCacheMatrix
## to check if a solution (inverse matrix), has been saved into
## the object structure, if so, it returns this value and echoes
## than the returned value is the cached one,
## otherwise it performs the calculation and stores it within
## the object (if matrix is not invertible it sends a message)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getsolve()  ## get the cached solution
  data <- x$get()  ## get the matrix within this object
  if(!is.null(inverse_matrix))   {  ##solution has already been calculated?
    message("getting cached data") ## feedback about returning value is the cached one
    return(inverse_matrix)  ## return the cached solution (no message displayed)
    
  }
  ## inverse matrix has not been calculated yet
  inversible<-try(inverse_matrix<-solve(data, ...),silent=TRUE) ##use try to catch errors
  if (class(inversible) =="matrix")  {  ##no problems detected when calculating inverse matrix
    x$setsolve(inverse_matrix)  ##set the solution in the object
    inverse_matrix ##echoes the solution
  }  
  else message("This matrix cannot be inverted!!") ##feedback that the matrix cannot be inverted
  
}

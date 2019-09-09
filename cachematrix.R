## This code contains two functions, one that creates a matrix object that allows for caching of the matrix inverse
## the second function returns the invers if cached and calculates it and stores it if it is not yet cached

##This first fucntion sets the matrix object that allows for caching of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInv <- function(newInv) inv <<- newInv
    getInv <- function() inv
    list(set=set, get=get, setInv = setInv, getInv=getInv)
}


## This function calculates the inverse or returns the already cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInv()
      if(!is.null(inv)){
        message("getting cached inverse")
        return(inv)
      }
      matrix <- x$get()
      inv <- solve(matrix)
      x$setInv(inv)
      inv
}

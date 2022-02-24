## Put comments here that give an overall description of what your
## functions do

# These two functions allows us to calculate and store an inverse matrix in a cache for faster access to that data

## Write a short comment describing this function

# This function creates an object with getter and setter functions (through Lexical scoping) that will allow it to first store the 
# original matrix and afterwards also its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

# This function looks at the makeCacheMatrix object using the get and getinv functions, and depending on whether the inverse matrix 
# was already stored in the i variable, it either returns it directly or calculates it and stores it in the object with the setinv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

myMatrix <- makeCacheMatrix(matrix(c(2,2,2,3),2,2))
myMatrix$get()
#     [,1] [,2]
#[1,]    4    2
#[2,]    2    2
cacheSolve(myMatrix)
#     [,1] [,2]
#[1,]  0.5 -0.5
#[2,] -0.5  1.0
cacheSolve(myMatrix)
#getting cached data
#     [,1] [,2]
#[1,]  0.5 -0.5
#[2,] -0.5  1.0


##  functions: makeCacheMatrix  and cacheSolve   all to create an inverse matrix
##  with demo-ing caching results ofpotentially time-consuming computations        
##  makeCacheMatrix creates an matrix object/list initialising the cache 
#   cacheSolve returns the inverse matrix using/mutating the cache  

#the first function, makeCacheMatrix creates a special "matrix",
# which is really a list containing a function to:

#set the value of the matrix
#get the value of the matrix
#set the value of the reverse matrix
#get the value of the reverse matrix

# call  x <- makeCacheMatrix(inpuntmatrix) makes x that special matrix with the data 
#       and call-able functions like methods of an object against that data


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

# cacheSolve    
#     is going to return the inverse matix:
#           checks if it is already in cache 
#               IF SO   gets it out of cache
#               IF NOT  calulates the inverse matrix and stores is into cache
#           returns  de Inverse matrix
                



cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

#   SAMPLE calling code:
#   zz <- matrix(1:4,2,2)
#   xyy <- makeCacheMatrix(zz)
#   xr <- cacheSolve(xyy)
#   xr <- cacheSolve(xyy)  # demo of usng cache
#   xr

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ##this function is making squar matrix

		m <- NULL       #first m will be Null
        set <- function(y) { 
                x <<- y
                m <<- NULL
        }
        get <- function() # for getting the x matrix
		x
        setinversion <- function(solve)
		m <<- solve
        getinversion <- function() 
		m
        list(set = set, get = get,   #list which consist of functions
             setinversion = setinversion ,
             getinversion = getinversion)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

		m <- x$getinversion()
        if(!is.null(m)) {       #it controls to see if the m is empty or not
                message("getting cached data") #if it is not empty the inverse will be get from m
                return(m) #m will be returned
        }
        data <- x$get()  #if the m is empty the inverse will be calculated and will be put in m
        m <- solve(data, ...)
        x$setinversion(m)
        m
}

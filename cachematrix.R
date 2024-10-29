## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
         # check if x is a matrix
    if(is.matrix(x)) {
        print("It is a matix, continue...")
        m <- NULL
        #print(paste0(cat("m1: "), m))
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
    } else {
        print("x is not a matrix... input a matrix to use the function...")
        return(message("The matrix is'n invertible."))
    }

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getinverse()
    print(paste0(cat("cacheSolve m1: "), m))
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    print(paste0(cat("cacheSolve data: "), data))
    m <- solve(data, ...)
    print(paste0(cat("cacheSolve m1: "), m))
    x$setinverse(m)
    print(paste0(cat("cacheSolve x$setinverse(m): "), x$setinverse(m)))
    m
    print(paste0(cat("cacheSolve m: "), m))
}


# make matrix
# Create a special matrix with initial values
m <- makeCacheMatrix(matrix(c(1,2,3,4),ncol=2,nrow=2))

# Calculate and cache the inverse
inv1 <- cacheSolve(m)
print(inv1)  # This will compute the inverse and output it

# Get the cached inverse
inv2 <- cacheSolve(m)
print(inv2)  # This will fetch the cached inverse and output it


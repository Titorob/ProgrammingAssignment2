## Put comments here that give an overall description of what your
## functions do

# Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
# Initializate the inverse property
        inv <- NULL
        
# Function to set the matrix
        set <- function(matrix) {
                x <<- matrix
                inv <<- NULL
        }

# Function to get the matrix
        get <- function() {
                x
        }

# Function to set the inverse of the matrix
        setInv <- function(inverse) {
                inv <<- inverse
        }

# Function to get the inverse of the matrix
        getInv <- function() {
                inv
        }

# Return a list with the previous functions
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


#Compute the inverse of the special matrix returned by "makeCacheMatrix"
# above. If the inverse has already been calculated (and the matrix has not
# changed), then the "cachesolve" should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        
# Return a matrix that is the inverse of x
        m <- x$getInv()
        
# Return the inverse if its already set
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

# Get the matrix from our object
        data <- x$get()

# Calculate the inverse using matrix multiplication
        m <- sovle(data) %*% data
        
# Set the inverse of the object
        x$setInv(m)
        
# Return the matrix
        m
}

# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# setMatrix      set the value of a matrix
# getMatrix      get the value of a matrix
# setInverseMatrix   	get the cached value (inverse of the matrix)
# getInverseMatrix     	get the cached value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
# empty cachedinvmatrix
		cachedinvmatrix <- NULL
# function to set a matrix
        set <- function(y) {
				# matrix is assigned to x
                x <<- y
				# cachedinvmatrix nullified because matrix got a new value
                cachedinvmatrix <<- NULL
        }
# function to get a matrix
        get <- function(){ 
			x
		}
# function to set Inverse Matrix
        setInverseMatrix <- function(inverse){
			cachedinvmatrix <<- inverse
		}
# function to get Inverse Matrix
        getInverseMatrix <- function(){
			cachedinvmatrix
		}
# returns a list, each argument is a function
        list(set = set,
             get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}

# function to get inverse of the matrix created with makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## get the cached inverse matrix
		cachedinvmatrix <- x$getInverseMatrix()
		## if matrix exists in cached display it
        if (!is.null(cachedinvmatrix)) {
                message("getting cached data")
                return(cachedinvmatrix)
        }
		## get the matrix
        mat <- x$get()
		## calculate the inverse
        cachedinvmatrix <- solve(mat, ...)
		## set the inverse matrix 
        x$setInverseMatrix(cachedinvmatrix)
		## display the matrix
        cachedinvmatrix
}

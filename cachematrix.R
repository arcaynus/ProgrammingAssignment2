## cachematrix.R:   Functions below create a special "matix" object that can generate and cache
##                  its inverse

## Description: creates a special "matrix" object that can cache its inverse.
## Params: x: an invertable matrix, defaulted to empty matrix
## Returns: A list with functions:
##          set: set the inversable matrix
##          get: get the inversable matrix
##          setInv: set the inverse of the matrix
##          getInv: get the inverse of the matrix ** does not calculate it

makeCacheMatrix <- function(x = matrix()) {
    # define variables
    invMatrix <- NULL       # set the inverse to null
    
    # define set
    set <- function (newMatrix){
        x <<- newMatrix     # assign the new matrix 
        invMatrix <<- NULL  # reset inverse to NULL as previus value is invalid
    }
    # define get
    get <- function (){
        return(x)
    }
    # define setInv
    setInv <- function(inverseMatrix){
        invMatrix <<- inverseMatrix
    }
    # define getInv
    getInv <-function(){
        return(invMatrix)
    }
    # Create list of functions that is returned by the function
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv
    )
}


## Description: computes the inverse of the special "matrix" returned by
## makeCacheMatrix above.  If the inverse has already been calculated
## and the matrix has not changed, then cacheSolve will retrieve
## the inverse from the matrix
## Params: x: an invertable special matrix, creted using makeCacheMatrix
##          ...: Additional params used for the solve function
## Returns: A list with functions:
##          set: set the inversable matrix
##          get: get the inversable matrix
##          setInv: set the inverse of the matrix
##          getInv: get the inverse of the matrix ** does not calculate it 
## Assumptions: matrix supplied is always invertable

cacheSolve <- function(x, ...) {
        
        #use the getInv method from the passed in special matrix
        invMatrix <- x$getInv()
        
        #check to see if the inverse is not NULL, 
        if(!is.null(invMatrix)){
            message("getting cached data")
            return(invMatrix)
        }
        else{ #otherwise recalculate/generate the inverted matrix
            message("generating data")
            newMatrix <- x$get() # Use the get function from the list to get the matrix data
            invMatrix <- solve(newMatrix, ...) # Get the inverted matrix
            x$setInv(invMatrix) # Use the setInv fuction from the list to set the inv matrix
        }
    
        ## Return a special matrix that is the inverse of 'x'
        return(x)
}
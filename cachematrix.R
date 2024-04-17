## makeCacheMatrix will create a default (1,1) matrix with NA value if no
##  matrix is provided.
##  
##  Variables
##    inv: inverse matrix
##    mat: original matrix
##
##  Functions:
##    $set() can set the matrix to a given value
##    $get() returns the matrix
##    $set_inv(inv_mat) can be used to set the inverse of the matrix explicitly
##    $get_inv() returns the inverse of the matrix stored, NULL if not set
##
##  Returns a list of functions referring to global variables `mat`, `inv`

makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    
    get <- function() mat
    
    set <- function(y) {
      mat <<- y
      inv <<- NULL
    }
    
    set_inv <- function(inv_mat) inv <<- inv_mat
    
    get_inv <- function() inv
    
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
} 


## Usable with the object returned by $makeCacheMatrix()
## Takes a list of functions returned by $makeCacheMatrix() as an input `mat`
## Checks if inverse has been set globally and returns if yes, if not calculates the inverse and returns it

cacheSolve <- function(mat, ...) {
    ## Return a matrix that is the inverse of 'mat'
    inv <- mat$get_inv()
    
    if(!is.null(inv)) {
      message("returning the inverse from cache")
      return(inv)
    }
    
    d <- mat$get()
    inv <- solve(d)
    mat$set_inv(inv)
    inv
}

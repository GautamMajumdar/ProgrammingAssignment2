## The function makCacheMatrix has 4 functions  
## 1. to set the matrix                 2. to get the matrix 
## 3. to set the inverse of matrix      4. to get the inverse of matrix 
## It uses the superassignment operator "<<-" to set the value of variables x and inv_mat
## which are set in the parent environment of the function.  These variables have lexical scope
## and so can be used to retain values beyond the dynamic scope of the function ( caching )


makeCacheMatrix <- function(x = matrix()) {
	inv_mat <- NULL
	set_mat <- function(y) {
		x <<- y
		inv_mat <<- NULL
		}
	get_mat <- function() x
	set_inv <- function(mat) inv_mat <<- mat
	get_inv <- function() inv_mat
	list(set_mat = set_mat, get_mat = get_mat,set_inv = set_inv,get_inv = get_inv)

}


## The function cacheSolve checks to see if the inverse of the matrix is already present in the 
## cacche , if present it is retrieved , else the inverse is calculated and the variable in the 
## parent environment ( cached )

cacheSolve <- function(x = matrix()) {
        ## Return a matrix that is the inverse of 'x'
	inv_mat <- x$get_inv()
	if(!is.null(inv_mat)) {
		message("getting cached data")
		return(inv_mat)
	}
	data <- x$get_mat()
	inv_mat <- solve(data)
	x$set_inv(inv_mat)
	inv_mat
}

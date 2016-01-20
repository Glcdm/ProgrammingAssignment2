# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()){
	a<-NULL
	set<-function(y){
		x <<- y
		a <<- NULL
	}
	get<-function() x
	setinv<-function(inverse) a<<- inverse
	getinv<- function() a
	list(set=set, 
		get=get, 
		setinv=setinv, 
		getinv=getinv)
}
# The following function calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse matrix from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets it in the cache via the setinv() function.
cacheSolve <-function(x, ...) {
        a <- x$getinv()
        if(!is.null(a)) {
                message("getting cached data")
                return(a)
        }
        data <- x$get()
        a<- solve(data, ...)
        x$setinv(a)
        a
}
#Test
# source("cachematrix.R")
# matrix_test <- makeCacheMatrix(matrix(c(2,0,0,0,3,0,0,0,3), byrow=T, nrow=3))
# cacheSolve(matrix_test)
# result      [,1]      [,2]      [,3]
# [1,]  0.5 0.0000000 0.0000000
# [2,]  0.0 0.3333333 0.0000000
# [3,]  0.0 0.0000000 0.3333333

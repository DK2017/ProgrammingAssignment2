##Overall the makeCacheMatrix and cacheSolve functions utilize a cache to find the inverse matrix 
##This way would be faster than calculating the inverse matrix everytime you need it. 
##Instead it sees if it has been calculated from before and uses that stored inverse matrix 

##The makeCacheMaxtrix function creates a special "matrix" object that can cache its inverse.
#Contains functions that can be assessed using $
#The default of the arguement 'x' is matrix()
makeCacheMatrix <- function(x = matrix()) {
        
        # a new m variable is set as Null
        m <- NULL
        
        #when the set function is executed the y variable is stored in x and m becomes null
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #get function returns the x matrix
        get <- function() {
                x
        }
        
        #setmatrix function sets the matrix variable in to the global m variable
        setmatrix <- function(matrix) {
                m <<- matrix
        } 
        
        #getmatrix returns the m variable
        getmatrix <- function() {
                m
        }
        
        #store the methods in a list so that they can be accessed using $
        list(set = set, 
             get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


##The CacheSolve function finds the inverse of the special "matrix" returned by makeCacheMatrix above. 
##Sees if the inverse of the matrix has been calculated from before and returns that cached inverse matrix

#cacheSolve checks to see if the inverse matrix can be used from the cache, if not then it will calculate it and put it in the cache as well
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #gets the cached inverse matrix if it exists, assigns it to m, and returns that m if it exists
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        #else do the below functions to (1) calcuate the inverse matrix, (2) place it into the cache, and (3) display it
        data <- x$get()
        
        #(1) Calcuate the inverse of the matrix
        m <- solve(data, ...)
        
        #(2) place it into the cache
        x$setmatrix(m)
        
        #(3) Display it
        m
}

## R Programming Assignment 2 - smithdm3 - August 2015 

## The following functions provide a method of caching the results of what could be a
## compute intensive operation (finding the inverse of a matrix) so that it does not
## have to be repeatedly computed once the matrix and inverse are known
## 	makeCacheMatrix defines the object and the functions used to interact with it
##	 get, set for the matrix itself and getinverse, setinverse for the inverse
## 	cacheSolve determines the inverse of a matrix (of the aforementioned object) and 
##   by determining if it has already been derived and cached or by calculating it and
##   then storing it in the 'cache'


## makeCacheMatrix creates a matrix object with get/set functions for the matrix 
## and the inverse of the matrix, both of which are stored in the object

makeCacheMatrix <- function(matx = numeric()) {
		## initializes inverse of matrix object to be null until calculated --  	
		## note that assignment <- is used since the item inv is in the scope of 
		## the makeCacheMatrix function
	
        inv <- NULL
        
    	## function: set - creates matrix object - takes matrix maty, 
    	## stores matrix in matx and sets inverse as NULL as it hasn't been 	
    	## calculated yet
    
        set <- function(maty) {
        
        		## since matx and inv are objects of the parent function, we use 
        		## the <<- assignment
                
                matx <<- maty
                inv <<- NULL
        }
        
    ## function: get - retrieves matrix object      
    
        get <- function() matx
    ## function: setinverse - stores inverse in object - takes inverse and 	
    ## stores in inv -- note that 
    
        setinverse <- function(inverse) inv <<- inverse
    
    ## function: getinverse - retrieves inverse from object 
    
        getinverse <- function() inv
        
    ## creates list of functions so that they can be used via a$b notation 
    
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve determines whether the inverse of the matrix has been calculated 
## already and if so, fetches it, otherwise it calculated it and stores it in 
## the object (caching it for later use)

cacheSolve <- function(matx, ...) {

		## attempt to retrieve inverse of matrix 

        inv <- matx$getinverse()
        
        ## check to see whether the inverse has been cached and if so, 
        ## retrieve and return it
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## function only gets here if inverse was not cached, so calculate it 
        ## by first getting the matrix from the object
        
        data <- matx$get()
        
        ## calculate the inverse (assuming that it is invertible)
        
        inv <- solve(data)
        
        ## cache the inverse so we don't have to calculate it in the future
        
        matx$setinverse(inv)
        
        ## Return a matrix that is the inverse of 'x'
        
        inv
}
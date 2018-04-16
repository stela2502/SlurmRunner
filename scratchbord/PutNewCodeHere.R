running <- function(x) {
	## check if the scripts are still working
	if ( length( x@running_local ) > 0 ){
		active <- read.table(pipe( 'ps' ), header=T)
		m <- match( x@running_local,active[,'PID'] )
		x@running_local <- x@running_local[-which(is.na(m))]
	}
	if ( length( x@running_slurm ) > 0 ){
		active <- read.table(pipe( paste('squeue -u',x@uname ) ), header=T)
		browser()
	}
	
}

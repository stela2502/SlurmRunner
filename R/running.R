#' @name running
#' @aliases running,SlurmRunner-method
#' @rdname running-methods
#' @docType methods
#' @description Check which of the internally stored pids are still active
#' @param x the SlurmRunner object
#' @title fetch the still running pid's
#' @export 
setGeneric('running', ## Name
	function (x) { ## Argumente der generischen Funktion
		standardGeneric('running') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('running', signature = c ('SlurmRunner'),
	definition = function (x) {
	## check if the scripts are still working
	if ( length( x$running_local ) > 0 ){
		z <- pipe( 'ps' )
		active <- read.table(z, header=T)
		#close(z)
		m <- match( x$running_local,active[,'PID'] )
		if ( length(which(is.na(m))) > 0){
			x$running_local <- x$running_local[-which(is.na(m))]
		}
	}
	if ( length( x$running_slurm ) > 0 ){
		z <- pipe( paste('squeue -u',x$uname ) )
		active <- read.table(z, header=T)
		#close(z)
		m <- match( x$running_slurm,active[,'JOBID'] )
		if ( length(which(is.na(m))) > 0){
			x$running_slurm <- x$running_slurm[-which(is.na(m))]
		}
	}
	c( x$running_slurm, x$running_local)
} )

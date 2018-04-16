#' @name running
#' @aliases running,SlurmRunner-method
#' @rdname running-methods
#' @docType methods
#' @description 
#' @param x  TEXT MISSING
#' @title description of function running
#' @export 
setGeneric('running', ## Name
	function (x) { ## Argumente der generischen Funktion
		standardGeneric('running') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('running', signature = c ('SlurmRunner'),
	definition = function (x) {
	## check if the scripts are still working
	if ( length( x@running_local ) > 0 ){
		active <- read.table(pipe( 'ps' ), header=T)
		m <- match( x@running_local,active[,'PID'] )
		if ( length(which(is.na(m))) > 0){
			x@running_local <- x@running_local[-which(is.na(m))]
		}
	}
	if ( length( x@running_slurm ) > 0 ){
		active <- read.table(pipe( paste('squeue -u',x@uname ) ), header=T)
		m <- match( x@running_slurm,active[,'JOBID'] )
		if ( length(which(is.na(m))) > 0){
			x@running_slurm <- x@running_slurm[-which(is.na(m))]
		}
	}
	c( x@running_slurm, x@running_local)
} )

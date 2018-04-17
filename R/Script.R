#' @name Script
#' @aliases Script,SlurmRunner-method
#' @rdname Script-methods
#' @docType methods
#' @description use the cmd as script workload and built the SLURM header for the script using the internal settings hash
#' @param filename the filename to save the R script to (has to be unique for the analysis!)
#' @param cmd The R cmd to run
#' @title Create the script files to run the R cmd
#' @return the SLURM/batch script to run the R code
#' @export 
setGeneric('Script',
		function (x, filename, cmd,... ){
			standardGeneric('Script')
		}
)
setMethod('Script', signature = c ('SlurmRunner'),
		definition = function ( x, filename, cmd, ...   ) {
			if ( ! file.exists(x$tmp.path)) {
				dir.create( x$tmp.path )
			}
			wp <- file.path( x$tmp.path, filename )
			rscript <-  paste(wp, '.R', sep='')
			SLURMscript <-  paste(wp, '.sh', sep='')
			
			fileConn<-file( rscript )
			writeLines (cmd, con=fileConn )
			close(fileConn)
			
			fileConn<-file( SLURMscript )
			if ( is.null(x$settings$t)) { x$settings$t = '02:00:00' }
			
			l <- c( '#! /bin/bash',
					paste('#SBATCH -n ',x$settings$n),
					paste('#SBATCH -N ',x$settings$N),
					paste('#SBATCH -t ', x$settings$t),
					paste("#SBATCH -J '", filename,"'",sep=''),
					paste("#SBATCH -o '", filename,"_omp_%j.out'",sep=''),
					paste("#SBATCH -e '", filename,"_omp_%j.err'",sep=''),
					paste("#SBATCH -A ",x$settings$A )
			)
			if ( length(grep( "^lu", x$settings$A)) ){
				l <- c( l, "#SBATCH -p lu")
			}else if ( ! is.null(x$settings$p)){
				l <- c( l, paste("#SBATCH -p", x$settings$p ))
			}
			if ( ! is.null(x$settings$w ) ){
				l <- c( l, paste("#SBATCH -w, --",x$settings$w , sep=''))
			}
			cmd <- paste('R CMD BATCH --no-save --no-restore --no-readline --max-ppsize=500000 --', rscript ) 

			writeLines ( c(l,cmd ), con=fileConn )
			close(fileConn)
			
			SLURMscript
			
		}
)
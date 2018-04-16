#' @name Run
#' @aliases Run,SlurmRunner-method
#' @rdname Run-methods
#' @docType methods
#' @description run the script using either a SLUEM backend or the local computer
#' @param x the SlurmRunner object
#' @param filename the filename to save the R script to (has to be unique for the analysis!)
#' @param cmd the R cmd to run (use absolute paths in there!)
#' @param local true to run using bash (sbatch default)
#' @title Run the R cmd as script and keep track of the process
#' @return the downstream PID of the spawned R script 
#' @export 
setGeneric('Run',
		function (x, filename, cmd, local=FALSE, ... ){
			standardGeneric('Run')
		}
)
setMethod('Run', signature = c ('SlurmRunner'),
		definition = function ( x, filename, cmd, local=FALSE, ...  ) {
			#print ( paste( "Run =",run)) 
			scriptF = Script( x, filename, cmd, ... )
			pid = 0
			if ( ! x@debug ) {
				if ( local ) {
					print ("Please check this PID manually or feed it into <thisObject>@running_local")
					system( paste("bash",scriptF,'&' ) )
					#pid = as.numeric(scan(pipe( 'echo $!')), what=character())
					#x@running_local <- c(x@running_local, pid )
				}else {
					pid = as.numeric(scan(pipe( paste("sbatch",scriptF)), what=character())[4])
					x@running_slurm <- c(x@running_slurm, pid )
				}
			}
			pid
		}
)

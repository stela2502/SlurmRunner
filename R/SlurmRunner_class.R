#' @name SlurmRunner
#' @title SlurmRunner
#' @docType package
#' @description  An S4 class to run SLURM scripts and keep track of the running ones
#' @slot running_local the local still running scripts
#' @slot running_slurm the slurm running scripts
#' @slot settings the slurm settings to be used in the scripts
#' @slot tmp.path the run folder where all slurm scripts are placed
#' @slot debug if true the scripts are not run (default FALSE)
#' @slot uname the username of the active user (default whoami output)
#' @exportClass SlurmRunner
setClass(
		Class = 'SlurmRunner',
		representation =  representation (
				running_local = 'numeric',
				running_slurm = 'numeric',
				settings = 'list',
				tmp.path= 'character',
				debug = 'logical',
				uname = 'character'
		),
		prototype( running_local=NA_integer_ , running_slurm=NA_integer_ , settings = list(), 
				tmp.path = file.path(getwd(),'tmp') , debug = FALSE	, uname= scan(pipe('whoami'), what=character()))
)

#' @name SlurmRunner
#' @aliases SlurmRunner,SlurmRunner-method
#' @rdname SlurmRunner-methods
#' @docType methods
#' @description  create a new SlurmRunner object. The clustering will be performed on the columns of the data.frame.
#' @param settings the settings for the SLURM process (only 'A', 't' and 'p' are used)
#' @param tmp.path where to store the temporaray files
#' @return A new SlurmRunner object
#' @title description of function SlurmRunner
#' @export 
setGeneric('SlurmRunner', ## Name
		function ( settings=list(), tmp.path= '', debug=FALSE) { ## Argumente der generischen Funktion
			standardGeneric('SlurmRunner') ## der Aufruf von standardGeneric sorgt für das Dispatching
		}
)

setMethod('SlurmRunner', signature = c ('list'),
		definition = function ( settings=list(), tmp.path= '', debug=FALSE ) {
			if ( tmp.path == '' ){
				tmp.path = file.path(pwd(), 'tmp')
			}
			if ( length(grep( '^/', tmp.path, perl=T)) == 0 ){
				stop( 'I need the absolute path for the temp path' )
			}
			if ( ! file.exists(tmp.path)){
				dir.create( tmp.path )
			}
			new ( 'SlurmRunner', settings=settings, tmp.path=tmp.path, debug=debug )
			
		} 
)


require('R6')

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
SlurmRunner <- #withFormalClass(
		R6Class(
				'SlurmRunner',
				class = TRUE,
				public = list ( 
						running_local = NULL,
						running_slurm = NULL,
						settings = NULL,
						tmp.path= './',
						debug = FALSE,
						uname = NULL,
						print = function (...) {
							running(self)
							cat (paste("An object of class", paste(collapse="::",rev(class(self))),"\n" ) )
							cat("named ",self$name,"\n")
							cat (paste( 'with',nrow(self$dat),'genes and', ncol(self$dat),' samples.'),"\n")
							if ( length( self$running_local) > 0) {
								cat (paste( "with",length( self$running_local),"local processes"))
								if (length( self$running_slurm) > 0 ) {
									cat (paste( "and",length( self$running_slurm),"slurm processes"))			
								}
							}else if ( length( self$running_slurm) > 0 ){
								cat (paste( "with",length( self$running_slurm),"slurm processes"))					
							}
						},
						initialize = function ( settings, debug=FALSE , tmp.path = file.path(getwd(),'tmp'), uname =NULL ){
							
							error = NULL;
							## check the required settings
							for (n in c('A', 't', 'N','n' ) ){
								if ( is.null(settings[[n]])){
									error = paste( error, paste("SLURM option '", n, "' is missing", sep=""),sep="\n" )
								}
							}
							if ( ! is.null(error) ) {
								stop(error)
							}
							
							self$settings = settings
							self$running_local = c()
							self$running_slurm = c()
							
							if ( is.null(uname) ){
								z = pipe('whoami')
								uname = scan(z, what=character())
								close(z)
							}
							self$uname = uname
							
							self$tmp.path = tmp.path
							self$debug = debug
							
							self
							
						}
				)
		)




.onAttach <- function(libname, pkgname) {
	#packageStartupMessage("Welcome to my package BioData")
	where <- as.environment("package:SlurmRunner")
	clss <- list(
			c("SlurmRunner", "R6")
	)
	## Ensure clean initial state for subsequent package loads
	## while developing //
	sapply(clss, function(cls) {
				idx <- sapply(cls, isClass )
				suppressWarnings(try(sapply(cls[idx], removeClass,
										where = where), silent = TRUE))
			})
	## Set formal class equivalent //
	sapply(clss, function(cls) {
				try(setOldClass(cls, where = where), silent = TRUE)
			})
#	r6x::formalizeClasses()
}

#library(SlurmRunner)


obj = SlurmRunner$new( settings= list( 'A' = 'lsens2017-3-2', 't' = '00:00:20', p='dell','N'=1, 'n'=1), tmp.path=file.path(getwd(),'tmp') )

expect_true( obj$debug == FALSE, 'no debug by default' )

obj$debug=TRUE

expect_true ( Run(obj, cmd="Sys.sleep(10)", file="sleep2" ) == 0, "not run pid == 0" )

obj$debug=FALSE

expect_true ( (pid= Run(obj, cmd="Sys.sleep(10)", file="sleep2" )) != 0, paste( "run pid != 0 (",pid,")") )
Sys.sleep(2) ## takes some time to register the script...
expect_equal ( running( obj ),  c(pid), info ="running pid is stored" )

Sys.sleep(20)

expect_true ( length(running( obj ) ) == 0, "pid is removed after process has finished" )


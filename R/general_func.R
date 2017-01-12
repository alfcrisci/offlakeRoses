#' strip legend from ggplot object
#' 
#' @param obj ggplot object.
g_legend<-function(obj){ 
  tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(obj)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  dev.off()
  return(legend)
} 

#' query of AWS owenslake database
#' 
#' @param query String. Postgres query for database.
query_owens <- function(query){
  usr <- readLines("~/system/credentials/airsci_db_cred.txt")[3]
  psswrd <- readLines("~/system/credentials/airsci_db_cred.txt")[4]
  hst <- "airdbo1.cwxikzzkese5.us-west-2.rds.amazonaws.com"
  prt <- "5432"
  con <- RPostgreSQL::dbConnect("PostgreSQL", host=hst, port=prt, 
                                dbname="owenslake", user=usr, password=psswrd)
  dat <- RPostgreSQL::dbGetQuery(con, query)
  RPostgreSQL::dbDisconnect(con)
  dat
}

#' query of AWS owenslake database
#' 
#' @param x. Number to round.
#' @param base. Base to round to.
mround <- function(x, base){
  base * round(x/base)
}


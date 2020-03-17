library(DBI)
library(dbplyr)

#---- General data ----
Data <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
					   user = "USERNAME",
					   host = "HOST",
					   dbname = "NAME OF THE DATABASE",
					   password = "PASSWORD")

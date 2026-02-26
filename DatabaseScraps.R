# Mon Feb 23 17:47:44 2026 ------------------------------
# database creation

con <- dbConnect(duckdb::duckdb(), 'database_bottleneck.duckdb')

dbWriteTable(con, 'bottleneck_tabel_2025', data)


result <- dbGetQuery(con, "
                     SELECT * 
                     FROM bottleneck_tabel_2025
                     WHERE doy = 1
                     ")


dbDisconnect(con, shutdown=TRUE)


library(DBI)
library(duckdb)

# create / connect to database file
drv <- duckdb(dbdir = "database_bottleneck_2025.duckdb")
con <- dbConnect(drv)

## write a table to it
# dbWriteTable(con, "iris", iris)

## and disconnect
dbDisconnect(con, shutdown=TRUE)

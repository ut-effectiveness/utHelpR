#' Set up a database connection
#'
#' @param dsn A DSN entry: edify, PROD, REPT, BRPT, etc...
#'
#' @return a database connection
#' @export
#'
#' @examples
#'

get_conn <- function(dsn) {
  # Server-side db connection with RStudio Connect
  if ( DBI::dbCanConnect(odbc::odbc(), DSN=dsn) ) {
    conn <- DBI::dbConnect(odbc::odbc(), DSN=dsn)
  }
  else if ( DBI::dbCanConnect(RPostgres::Postgres(), DSN=dsn) ) {
    conn <- DBI::dbConnect(RPostgres::Postgres(), DSN=dsn)
  }
  # Local db connection
  else if ( dsn == "edify" ) {
    conn <- DBI::dbConnect( RPostgres::Postgres(),
                            dbname="analytics",
                            host="dixie.db.edh.eab.com",
                            port=51070,
                            user=keyring::key_get("edify", "username"),
                            password=keyring::key_get("edify", "password") )
  }
  else {
    conn <- DBI::dbConnect( odbc::odbc(),
                            DSN=dsn,
                            UID=keyring::key_get("sis_db", "username"),
                            PWD=keyring::key_get("sis_db", "password") )
  }
  return(conn)
}

#' Create a DSLite login object that can be used for testing
#'
#' @param assign_method A string specifying the name of the custom assign method to be added
#'   to the DSLite server. If `NULL`, no additional assign method is added. Default is `NULL`.
#' @param aggregate_method A string specifying the name of the custom aggregate method to be
#'   added to the DSLite server. If `NULL`, no additional aggregate method is added. Default is `NULL`.
#' @param tables A named list of tables to be made available on the DSLite server. Default is `NULL`.
#'
#' @return A DataSHIELD login object containing the necessary connection information for the DSLite server.
#'
#' @examples
#' \dontrun{
#' # Prepare a DSLite server with default methods and custom assign/aggregate methods
#' login_data <- .prepare_dslite(
#'   assign_method = "customAssign",
#'   aggregate_method = "customAggregate",
#'   tables = list(mtcars = mtcars, mtcars_group = mtcars_group)
#'   )
#'
#' @importFrom DSLite newDSLiteServer
#' @importFrom DSI newDSLoginBuilder
#' @export
.prepare_dslite <- function(assign_method = NULL, aggregate_method = NULL, tables = NULL) {
  options(datashield.env = environment())
  dslite.server <- DSLite::newDSLiteServer(tables = tables)
  dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse")))
  dslite.server$aggregateMethod("exists", "base::exists")
  dslite.server$aggregateMethod("classDS", "dsBase::classDS")
  dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
  dslite.server$aggregateMethod("dsListDisclosureSettings", "dsTidyverse::dsListDisclosureSettings")

  if (!is.null(assign_method)) {
    dslite.server$assignMethod(assign_method, paste0("dsTidyverse::", assign_method))
  }

  if (!is.null(aggregate_method)) {
    dslite.server$aggregateMethod(aggregate_method, paste0("dsTidyverse::", aggregate_method))
  }

  builder <- DSI::newDSLoginBuilder()
  builder$append(server = "server_1", url = "dslite.server", driver = "DSLiteDriver")
  builder$append(server = "server_2", url = "dslite.server", driver = "DSLiteDriver")
  builder$append(server = "server_3", url = "dslite.server", driver = "DSLiteDriver")
  login_data <- builder$build()
  return(login_data)
}

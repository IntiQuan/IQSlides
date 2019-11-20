
# Split vector "1_bla", "2_bla", "3_usw" into
# number part (prefix) and character part (name)
splitNumNames <- function(x) {
  prefix__ <- utils::type.convert(sapply(x, function(f__) strsplit(f__, "_")[[1]][1]))
  filenames__ <- sapply(x, function(f__) paste(strsplit(f__, "_")[[1]][-1], collapse = "_"))
  if (is.numeric(prefix__)) nmax__ <- max(prefix__) else nmax__ <- 0
  OK_prefix__ <- is.numeric(prefix__)
  list(prefix = prefix__, name = filenames__, nmax = nmax__, OK_prefix = OK_prefix__, original = x)
}


genComplianceFooter <- function(outputfilename) {

  # Check if compliance mode active
  if (!IQRtools::is_enabled_complianceMode()) return()

  # Check if filename given
  if (is.null(outputfilename)) return()

  # Check if COMPLIANCE_MODE_SCRIPT_NAME exists in globalenv()
  e__ <- globalenv()

  if (!("COMPLIANCE_MODE_SCRIPT_NAME" %in% ls(e__)))
    stop("Compliance mode is enabled but the COMPLIANCE_MODE_SCRIPT_NAME variable has not\nbeen defined in the global environment (by the user). You can use the function IQRinitCompliance to do so")

  # Get the compliance "script" name
  COMPLIANCE_MODE_SCRIPT_NAME <- e__[["COMPLIANCE_MODE_SCRIPT_NAME"]]

  ## get information for the logfile
  logt__ <- "<TT>   File generation log"
  logfilepath__ <- paste(outputfilename,'.log',sep='')
  outputfilename <- gsub('//','/',outputfilename)
  userrow__ <- Sys.info()[['user']]
  timerow__ <- Sys.time()

  content__ <- paste0(COMPLIANCE_MODE_SCRIPT_NAME, " | User: ", userrow__, " | Date: ", timerow__)


  return(content__)

}


#' Remove Package
#' 
#' I was frustrated with the base uninstall.packages function, because I had to specify a lib path.  For this function, I search where the package is stored and delete all copies of it.
#' @param packageName An installed package 
#' @export
#' @examples
#' install.packages("ape")
#' removePackage("ape")

removePackage <- function(packageName) {
  whichPackages <- which(rownames(installed.packages()) == packageName)
  if(length(whichPackages) == 0)
    return("No packages found")
  for(i in sequence(length(whichPackages))){
    libPath <- installed.packages()[whichPackages[i],][2]
    version <- installed.packages()[whichPackages[i],][3]
    remove.packages(packageName, libPath)
    message(paste("Removed", packageName, version))
  }
}
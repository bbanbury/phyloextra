removePackage <- function(packageName) {
#This one will remove all packages with the package name, no matter which lib they are stored...
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

GetPackageVersion <- function(packageName, update=FALSE){
  whichPackages <- which(rownames(installed.packages()) == packageName)
  if(length(whichPackages) == 0)
    return("No packages found")
  for(i in sequence(length(whichPackages))){
    libPath <- installed.packages()[whichPackages[i],][2]
    version <- installed.packages()[whichPackages[i],][3]
    message(paste("Using", packageName, version))
  
  if(update)
    
}


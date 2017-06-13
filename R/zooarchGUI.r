#' @name zooaRchGUI-package
#' @docType package
#' @aliases zooaRchGUI
#' @title Analytical Tools for Zooarchaeological Data
#' @author Erik Otarola-Castillo, Jesse Wolfhagen, John Brian Rapes and Max D. Price
#'
#' @description Functions in this package allow users to interactively read, manage, analyze and visualize
#' zooarchaeological data - the faunal remains recovered from archaeological sites.
NULL

#' @import car
NULL

#' @import coda
NULL

#' @import foreign
NULL

#' @import ggplot2
NULL

#' @import geomorph
NULL

#' @import graphics
NULL

#' @import grDevices
NULL

#' @import MASS
NULL

#' @import pgirmess
NULL

#' @importFrom raster shapefile
NULL

#' @import readxl
NULL

#' @import rjags
NULL

#' @import sp
NULL

#' @import spdep
NULL

#' @importFrom splancs as.points polymap pointmap khat Kenv.csr getpoly
NULL

#' @import stats
NULL

#' @import tcltk
NULL

#' @import tcltk2
NULL

#' @import tkrplot
NULL

#' @import utils
NULL

#' @import vegan
NULL

#' Spatial Polygon Example
#'
#' @name spatPOLY
#' @docType data
#' @author Erik Otarola-Castillo
#' @keywords datasets
NULL

#' Syracuse Polygon Data
#'
#' @name syraPoly
#' @docType data
#' @author Erik Otarola-Castillo
#' @keywords datasets
NULL

#' Random Spatial Points Example for spatial statistics and autocorrelation example
#'
#' @name randPTS
#' @docType data
#' @author Erik Otarola-Castillo
#' @keywords datasets
NULL

#' Clustered Spatial Points Example for spatial statistics and autocorrelation example
#'
#' @name clusterPTS
#' @docType data
#' @author Erik Otarola-Castillo
#' @keywords datasets
NULL

#' Survival data from Marj Rabba, using Payne's (1973) age classes
#'
#' @name marjRab
#' @docType data
#' @author M.D. Price, M. Buckley, Y.M. Rowan, and M. Kersel
#' @references Payne, S. 1973  Kill - off Patterns in Sheep and Goats: The Mandibles from Asvan Kale. Anatolian Studies 23:281 - 303.
#' @references Price, M.D., Buckley, M., Rowan, Y.M., Kersel, M., 2013. Animal Management Strategies during the Chalcolithic in the Lower Galilee: New Data from Marj Rabba, Paleorient 39, 183 - 200.
#' @keywords datasets
NULL

#' Survival data labels from Marj Rabba, using Payne's (1973) age classes
#'
#' @name marjRlabels
#' @docType data
#' @author M.D. Price, M. Buckley, Y.M. Rowan, and M. Kersel
#' @references Payne, S. 1973  Kill - off Patterns in Sheep and Goats: The Mandibles from Asvan Kale. Anatolian Studies 23:281 - 303.
#' @references Price, M.D., Buckley, M., Rowan, Y.M., Kersel, M., 2013. Animal Management Strategies during the Chalcolithic in the Lower Galilee: New Data from Marj Rabba, Paleorient 39, 183 - 200.
#' @keywords datasets
NULL

#' Forager databse from Binford 2001, Constructing Frames of Reference
#'
#' @name LRBdata
#' @docType data
#' @author Binford, Lewis R.
#' @references Binford, Lewis R. 2001. Constructing Frames of Reference: An Analytical Method for Archaeological Theory Building Using Ethnographic and Environmental Data Sets. Berkeley: University of California Press.
#' @references Available from http://ajohnson.sites.truman.edu/data-and-program/
#' @keywords datasets
NULL

#' Data frame describing the variables in the LRB data frame. Part of the datasets used to calculate Binford's environmental and hunter-gatherer frames of reference variables
#'
#' @name LRBkey
#' @docType data
#' @author Binford, Lewis R.
#' @references Binford, Lewis R. 2001. Constructing Frames of Reference: An Analytical Method for Archaeological Theory Building Using Ethnographic and Environmental Data Sets. Berkeley: University of California Press.
#' @references Available from http://ajohnson.sites.truman.edu/data-and-program/
#' @keywords datasets
NULL

#' Fusion Survival data from Marj Rabba
#'
#' @docType data
#' @author M.D. Price, M. Buckley, Y.M. Rowan, and M. Kersel
#' @references Price, M.D., Buckley, M., Rowan, Y.M., Kersel, M., 2013. Animal Management Strategies during the Chalcolithic in the Lower Galilee: New Data from Marj Rabba, Paleorient 39, 183 - 200.
#' @keywords datasets
#' @name marjRab.fuse
NULL

#' Fusion Survival data for cattle remains from the
#' Winslow site, a colonial period farm near Boston, MA.
#'
#' @name winslow.fuse
#' @docType data
#' @author D. Landon
#' @references Landon, David B. 1993	Feeding Colonial Boston: A Zooarchaeological Study. Historical Archaeology 30:i-vii, 1-153
#' @keywords datasets
NULL

#' Bison survival data from Speth 1983
#'
#' @name speth83
#' @docType data
#' @author John D. Speth
#' @references Speth, J. D. 1983  Bison Kills and Bone Counts:
#' Decision Making by Ancient Hunters. University of Chicago Press, London.
#' @keywords datasets
NULL

#' Bison survival labels from Speth 1983
#'
#' @name speth83labels
#' @docType data
#' @author John D. Speth
#' @references Speth, J. D. 1983  Bison Kills and Bone Counts:
#' Decision Making by Ancient Hunters. University of Chicago Press, London.
#' @keywords datasets
NULL

#' Simulated Site x Species data
#'
#' @name myData
#' @docType data
#' @author eoc
#' @references eoc
#' @keywords datasets
NULL

#' Clarkson data for chi squared
#'
#' @name ClarksonLimbLoc
#' @docType data
#' @author Erik Otarola-Castillo
#' @references Otarola-Castillo, Erik 2010 Differences between NISP and MNE in Cutmark Analysis of Highly Fragmented Faunal Assemblages. Journal of Archaeological Science 37:1-12.
#' @keywords datasets
NULL

#' Standards for values
#'
#' @name standards
#' @docType data
#' @author EOC, MP, JW
#' @keywords datasets
NULL

#' zooaRchGUI
#' @examples
#' # Example 1: Binford data
#'
#'  data(LRBdata)
#'  head(LRBdata)
#' @export
zooaRch_GUI<-function() {
    tt <- tktoplevel(background='white')
    txt <- tktext(tt, height=0,width=107,background='white') #adjust window size
    tkimage.create ( "photo" , "image1" , file = system.file("img","zooarchLOGOsmall.gif",package="zooaRchGUI") )
    label <- ttklabel (tt, image = "image1", compound = "image",padding = c(0, 0, 0, 0),background='white')
    tkpack(label,padx=50)
    tkpack(txt)#adjust window size
    tktitle(tt) <- "zooaRch GUI 1.0.2"
    createMenu(tt)
    tkfocus(tt)
}

setWorkDir<-function() {
    frame_files <- lapply(sys.frames(), function(x) x$ofile)
    frame_files <- Filter(Negate(is.null), frame_files)
    dir <- dirname(frame_files[[length(frame_files)]])
    setwd(dir)
}

# dfs function
dfs.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(dfs) if (class(get(dfs))[1] == "data.frame") c(unlist(dfs)))),"Load User File")

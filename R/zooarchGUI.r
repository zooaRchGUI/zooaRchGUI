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
  tt <- tktoplevel()
  txt <- tktext(tt, height=0,width=107) #adjust window size
  tkimage.create ( "photo" , "image1" , file = system.file("img","zooarchLOGOsmall.gif",package="zooaRchGUI") )
  label <- ttklabel (tt, image = "image1",text = "Statistics for Zooarchaeologists" , compound = "top" ,padding = c(3, 3, 12, 12))
  tkpack(label)
  tkpack(txt)#adjust window size
  tktitle(tt) <- "zooaRch GUI 1.0"
  
  #File Menu
  topMenu <- tkmenu(tt)
  tkconfigure(tt, menu = topMenu)
  fileMenu <- tkmenu(topMenu, tearoff = FALSE)  # TOP menu
  fileFun  <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  fileFun2 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  fileFun3 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  fileFun4 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  tkadd(fileFun, "command", label = ".csv",
        command = function() getcsvfile())
  tkadd(fileFun, "command", label = ".txt",
        command = function() gettxtfile())
  tkadd(fileFun, "command", label = "Excel",
        command = function() getxlfile())
  tkadd(fileFun, "command", label = "SPSS",
        command = function() getspssfile())
  tkadd(fileFun, "command", label = "Shapefile",
        command = function() getshapefile())
  tkadd(fileMenu, "cascade", label = "Open File", menu = fileFun)
  tkadd(fileFun4, "command", label = "Save as",
        command = function() saveas_fun())
  tkadd(fileMenu, "cascade", label = "Save", menu = fileFun4)
  tkadd(fileMenu, "command", label = "Exit zooaRch", command = function() tkdestroy(tt))
  tkadd(topMenu, "cascade", label = "File", menu = fileMenu)
  
  #Edit Menu
  editMenu <- tkmenu(topMenu, tearoff = FALSE)
  editMenu <- tkmenu(topMenu, tearoff = FALSE)  # TOP menu
  editFun  <- tkmenu(topMenu, tearoff = FALSE)   # cascaded menu
  editFun2 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  editFun3 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  editFun4 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  editFun5 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  editFun6 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  editFun7 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  tkadd(editFun, "command", label = "Sort by Column",
        command = function() sort_fun())
  tkadd(editMenu, "cascade", label = "Sort", menu = editFun)
  tkadd(topMenu, "cascade", label = "Edit", menu = editMenu)
  
  #Plotting Menu
  plotMenu <- tkmenu(topMenu, tearoff = FALSE)# cascaded menu
  tkadd(plotMenu, "command", label = "Histogram",
        command = function() hist_fun())
  tkadd(plotMenu, "command", label = "Multiway Histogram",
        command = function() multi_histfun())
  tkadd(plotMenu, "command", label = "Bivariate",
        command = function() bivplot_fun())
  tkadd(plotMenu, "command", label = "Boxplot",
        command = function() simp_box_fun())
  tkadd(plotMenu, "command", label = "Multiway Boxplot",
        command = function() multi_boxplotfun())
  tkadd(topMenu, "cascade", label = "Plotting",  menu = plotMenu)
  
  #Counting Menu
  countMenu <- tkmenu(topMenu, tearoff = FALSE)  # TOP menu
  countFun  <- tkmenu(topMenu, tearoff = FALSE)   # cascaded menu
  countFun2 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  countFun3 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  countFun4 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  countFun5 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  countFun6 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  countFun7 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  tkadd(countFun, "command", label = "MNI",
        command = function() fun_na())
  tkadd(countFun, "command", label = "MNE",
        command = function() fun_na())
  tkadd(countFun, "command", label = "MAU",
        command = function() fun_na())
  tkadd(countFun, "command", label = "%MAU",
        command = function() fun_na())
  tkadd(countMenu, "cascade", label = "Tabulation", menu = countFun)
  tkadd(countFun2, "command", label = "LP",
        command = function() fun_na())
  tkadd(countFun2, "command", label = "LP2",
        command = function() fun_na())
  tkadd(countMenu, "cascade", label = "LP-Index", menu = countFun2)
  tkadd(countFun3, "command", label = "ABCML",
        command = function() fun_na())
  tkadd(countFun3, "command", label = "ABCML",
        command = function() fun_na())
  tkadd(countMenu, "cascade", label = "ABCML", menu = countFun3)
  tkadd(countFun4, "command", label = "Regression",
        command = function() fun_na())
  tkadd(countFun4, "command", label = "DFA",
        command = function() fun_na())
  tkadd(countFun4, "command", label = "Kernel Density",
        command = function() fun_na())
  tkadd(countFun4, "command", label = "Mixing-Model",
        command = function() fun_na())
  tkadd(countMenu, "cascade", label = "Sexing", menu = countFun4)
  tkadd(topMenu, "cascade", label = "Counting", menu = countMenu)
  
  #Zooarchaeological Size Standardization Function
  size_std<-function(){
    getstd <- function() {
      name <- tclvalue(tkgetOpenFile(
        filetypes = "{{.csv} *}"))
      if (name == "") return;
      data <- read.csv(name)
      pos <- 1
      envir = as.environment(pos)
      assign("std_data", data, envir = envir)
    }
    getdata <- function() {
      name <- tclvalue(tkgetOpenFile(
        filetypes = "{{.csv} *}"))
      if (name == "") return;
      data <- read.csv(name)
      pos<-1
      envir <- as.environment(pos)
      assign("sizedata", data, envir = envir)
    }
    tt <- tktoplevel()
    tkwm.title(tt, "Size STD")
    tkwm.geometry(tt, "300x150+300+300") 
    onOK <- function(){
      Xvar <- tclvalue(Xvar)
      Yvar <- tclvalue(Yvar)
      if(Xvar=="Load User File"){getdata()
      }
      if(Yvar=="user_standard"){getstd()
      }
      if(Xvar!="Load User File"){
        sizedat<-get(Xvar,.GlobalEnv)
        pos<-1
        envir <- as.environment(pos)
        assign("sizedata", sizedat, envir = envir)
      }
      if(Yvar!="user_standard"){
        standards<-NULL
        data(standards, envir = environment())
        std<-get(Yvar, standards)
        pos <- 1
        envir = as.environment(pos)
        assign("std_data", std, envir = envir)
      }
      #rm(list=c("x"))
      tkdestroy(tt)
    }
    label1 <- tklabel(tt, text="Input Size Data Frame")
    label2 <- tklabel(tt, text="Select Standard")
    dfs<-dfs.fun()
    std<-c("bos", "capra", "ovis_popkin", "ovis_uerpman", "sus","user_standard")
    
    # Default Selections for the Two Comboboxes
    Xvar <- tclVar("Choose one")
    Yvar <- tclVar("Choose one")
    # 1st box
    combo.1 <- ttkcombobox(tt, values=dfs, textvariable=Xvar, state="readonly")
    # 2nd box
    combo.2 <- ttkcombobox(tt, values=std, textvariable=Yvar, state="readonly") 
    OK.but <- tkbutton(tt,text="   Select Files", command = onOK)
    tkpack(label1, combo.1)
    tkpack(label2, combo.2)
    tkpack(OK.but)
    tkfocus(tt)
    
    tkwait.window(tt)
  }
  LSI <- function(base=exp(1)){
    size_std()
    sizedata<-sizedata
    std_data<-std_data
    
    #Proportional Standard Function
    msres <- which(std_data$Measure%in%colnames(sizedata))
    prop <- matrix(NA, nrow = nrow(sizedata), ncol = length(msres))
    for(i in 1:ncol(prop)){
      #Matching Values
      match <- which(sizedata$Anatomy == as.character(std_data$Anatomy[msres[i]]))
      prop[match,i] <- sizedata[match,colnames(sizedata)==as.character(std_data$Measure[msres[i]])]/std_data$Value[msres[i]]
    }
    return(data.frame(sizedata, LSIValue = log(rowMeans(prop,na.rm=T), base=base)))
    rm("std_data","sizedata")
  }
  VSI <- function(){
    size_std()
    sizedata<-sizedata
    std_data<-std_data
    msres <- which(std_data$Measure%in%colnames(sizedata))
    vals <- matrix(NA, nrow = nrow(sizedata), ncol = length(msres))
    for(i in 1:ncol(vals)){
      match <- which(sizedata$Anatomy == as.character(std_data$Anatomy[msres[i]]))
      vals[match,i] <- 50*(sizedata[match,colnames(sizedata)==as.character(std_data$Measure[msres[i]])]-std_data$Value[msres[i]])/(2*std_data$SD[msres[i]])
    }
    return(data.frame(sizedata, VSIValue = rowMeans(vals,na.rm=T)))
    rm("std_data","sizedata")
  }
  #Transformation Menu
  transMenu <- tkmenu(topMenu, tearoff = FALSE)
  transFun <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  transFunZoo <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  tkadd(transFun, "command", label = "Mathematical",
        command = function() trans_fun())
  tkadd(transFunZoo, "command", label = "LSI",
        command = function() LSI())
  tkadd(transFunZoo, "command", label = "VSI",
        command = function() VSI())
  tkadd(transMenu, "cascade", label = "Mathematical", menu = transFun)
  tkadd(transMenu, "cascade", label = "Zooarchaeological", menu = transFunZoo)
  tkadd(topMenu, "cascade", label = "Transformations", menu = transMenu)
  
  #Sampling Menu
  sampMenu <- tkmenu(topMenu, tearoff = FALSE)
  sampMenu <- tkmenu(topMenu, tearoff = FALSE)
  sampFunZoo <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  tkadd(sampFunZoo, "command", label = "Single Sample Rarefaction",
        command = function() s_rarefun())
  tkadd(sampFunZoo, "command", label = "Multiple Sample Rarefaction",
        command = function() s_rarefun())
  tkadd(topMenu, "cascade", label = "Sampling", menu = sampFunZoo)
  
  #Diversity Menu
  divMenu <- tkmenu(topMenu, tearoff = FALSE)
  divFun <-  tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  tkadd(divFun, "command", label = "Indices",
        command = function() divind_fun())
  tkadd(divMenu, "cascade", label = "Diversity", menu = divFun)
  tkadd(topMenu, "cascade", label = "Diversity", menu = divMenu)
  
  #Univariate Menu
  univMenu <- tkmenu(topMenu, tearoff = FALSE) # TOP menu
  univFun  <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  univFun2 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  univFun3 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  univFun4 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  univFun5 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  univFun6 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  univFun7 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  tkadd(univFun, "command", label = "Summary Dialog",
        command = function() summar_fun())
  # tkadd(univFun, "command", label = "Detailed",
  #      command = function() summar_fun())
  tkadd(univMenu, "cascade", label = "Summary Statistics", menu = univFun)
  tkadd(univFun2, "command", label = "Shapiro-Wilk's Test",
        command = function() shapirofun())
  tkadd(univFun2, "command", label = "Kolmogorov-Smirnov Test",
        command = function() ksfun())
  tkadd(univMenu, "cascade", label = "Normality Tests", menu = univFun2)
  tkadd(univFun3, "command", label = "One Sample t-test",
        command = function() onesam_tfun())
  tkadd(univFun3, "command", label = "Two Sample t-test",
        command = function() tfun())
  # tkadd(univFun3, "command", label = "Diversity t-test",
  #        command = function() univindex(myData, index="margalef"))
  tkadd(univFun3, "command", label = "One-way ANOVA",
        command = function() oneway_fun())
  tkadd(univFun3, "command", label = "Factorial ANOVA",
        command = function() fact_fun())
  tkadd(univMenu, "cascade", label = "Means", menu = univFun3)
  #tkadd(univMenu, "cascade", label = "Counts", menu = univFun2)
  #tkadd(univFun4, "command", label = "One Sample Proportion t-test",
  #      command = function() univindex(myData, index="shannon"))
  #tkadd(univFun4, "command", label = "Binomial Test",
  #      command = function() univindex(myData, index="simpson"))
  tkadd(univFun4, "command", label = "Chi Squared (row-by-column tests)",
        command = function() chisq_fun())
  tkadd(univFun4, "command", label = "Fisher's Exact Test",
        command = function() Fisher_exact_fun())
  tkadd(univMenu, "cascade", label = "Proportions", menu = univFun4)
  tkadd(univFun5, "command", label = "F-test",
        command = function() F_var_fun())
  tkadd(univFun5, "command", label = "Bartlett's Test",
        command = function() bartlettsfun())
  tkadd(univFun5, "command", label = "Levene's",
        command = function() LevenesVarTestFun())
  tkadd(univMenu, "cascade", label = "Variances", menu = univFun5)
  tkadd(univFun6, "command", label = "Correlation",
        command = function() Assoc_fun())
  tkadd(univFun6, "command", label = "Simple Linear Regression",
        command = function() simp_lmfun())
  tkadd(univFun6, "command", label = "Generalized Linear Model",
        command = function() glmfun())
  tkadd(univMenu, "cascade", label = "Association", menu = univFun6)
  tkadd(univFun7, "command", label = "Survivorship",
        command = function() death_age_fun())
  tkadd(univMenu, "cascade", label = "Survivorship", menu = univFun7)
  tkadd(topMenu, "cascade", label = "Univariate Statistics", menu = univMenu)
  
  #Multivariate Menu
  multivMenu <- tkmenu(topMenu, tearoff = FALSE) # TOP menu
  multivFun <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  multivFun2 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  multivFun3 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  multivFun4 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  multivFun5 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  multivFun6 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  multivFun7 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  tkadd(multivFun, "command", label = "D-Matrices",
        command = function() dist_fun())
  tkadd(multivMenu, "cascade", label = "D-Matrices", menu = multivFun)
  tkadd(multivFun2, "command", label = "One Sample",
        command = function() fun_na())
  tkadd(multivFun2, "command", label = "Two Sample",
        command = function() NPHotellingt_fun())
  tkadd(multivMenu, "cascade", label = "Hotelling T Tests", menu = multivFun2)
  tkadd(multivFun3, "command", label = "MANOVA",
        command = function() parMANOVA_fun())
  tkadd(multivFun3, "command", label = "NP-MANOVA",
        command = function() NPMANOVA_fun())
  tkadd(multivMenu, "cascade", label = "MANOVA", menu = multivFun3)
  tkadd(multivFun4, "command", label = "2-way Mantel",
        command = function() mantel2way_fun)
  tkadd(multivFun4, "command", label = "3-way Mantel",
        command = function() mantel3_fun())
  tkadd(multivMenu, "cascade", label = "Mantel Tests", menu = multivFun4)
  tkadd(multivFun5, "command", label = "Seriation",
        command = function() fun_na())
  tkadd(multivFun5, "command", label = "PCA",
        command = function() prcomp_fun())
  tkadd(multivFun5, "command", label = "PCoA",
        command = function() PCoA_fun())
  tkadd(multivFun5, "command", label = "NMDS",
        command = function() NMDS_fun())
  tkadd(multivFun5, "command", label = "CA",
        command = function() correspondence_fun())
  tkadd(multivFun5, "command", label = "Vector Fitting",
        command = function() multivarfit_fun())
  tkadd(multivMenu, "cascade", label = "Ordination", menu = multivFun5)
  tkadd(multivFun6, "command", label = "Hierarchical",
        command = function() cluster_fun())
  tkadd(multivFun7, "command", label = "Single K-means",
        command = function() kmeans_single_fun())
  tkadd(multivFun7, "command", label = "Multi K-means",
        command = function() kmeans_multi_fun())
  tkadd(multivFun6,"cascade", label = "K-means", menu = multivFun7)
  tkadd(multivMenu, "cascade", label = "Clustering", menu = multivFun6)
  tkadd(topMenu, "cascade", label = "Multivariate Statistics", menu = multivMenu)
  
  #Modeling Menu
  #  modMenu <- tkmenu(topMenu, tearoff = FALSE) # TOP menu
  #  modFun <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  #  modFun2 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  #  modFun3 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  #  modFun4 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  #  modFun5 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  #  modFun6 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  #  tkadd(modFun, "command", label = "GLM",
  #        command = function() glmfun())
  #  tkadd(modMenu, "cascade", label = "GLM", menu = modFun)
  #  tkadd(modFun2, "command", label = "GLMM Platform",
  #       command = function() fun_na())
  #  tkadd(modMenu, "cascade", label = "GLMM", menu = modFun2)
  #  tkadd(modFun3, "command", label = "GLiM Platform",
  #        command = function() fun_na())
  #  tkadd(modMenu, "cascade", label = "GliM", menu = modFun3)
  #  tkadd(modFun4, "command", label = "GLiMM Platform",
  #        command = function() fun_na())
  #  tkadd(modMenu, "cascade", label = "GLiMM", menu = modFun4)
  #  tkadd(modFun5, "command", label = "GLS Platform",
  #        command = function() fun_na())
  #  tkadd(modMenu, "cascade", label = "GLS", menu = modFun5)
  #  tkadd(modFun6, "command", label = "tGLM Platform",
  #        command = function() fun_na())
  #  tkadd(modMenu, "cascade", label = "tGLM", menu = modFun6)
  #  tkadd(topMenu, "cascade", label = "Modeling", menu = modMenu)
  
  #Spatial Menu
  spatMenu <- tkmenu(topMenu, tearoff = FALSE)  # TOP menu
  spatFun <- tkmenu(topMenu, tearoff = FALSE)   # cascaded menu
  spatFun2 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  spatFun3 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  spatFun4 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  spatFun5 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  spatFun6 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  spatFun7 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  #  tkadd(spatFun, "command", label = "Univariate Point Patterns",
  #        command = function() fun_na())
  #  tkadd(spatFun, "command", label = "Multivariate Point Patterns",
  #        command = function() fun_na())
  #  tkadd(spatFun, "command", label = "Aggregation Index",
  #        command = function() fun_na())
  tkadd(spatFun, "command", label = "Ripley's K",
        command = function() ripleys_K_fun())
  tkadd(spatMenu, "cascade", label = "Point Patterns", menu = spatFun)
  #  tkadd(spatFun2, "command", label = "Contiguous Connections",
  #        command = function() fun_na())
  tkadd(spatFun2, "command", label = "Spatially Defined Connections",
        command = function() fun_na())
  tkadd(spatMenu, "cascade", label = "Spatial Connections", menu = spatFun2)
  #  tkadd(spatFun3, "command", label = "Global Moran's I",
  #        command = function() fun_na())
  #  tkadd(spatFun3, "command", label = "Geary's C",
  #        command = function() fun_na())
  tkadd(spatFun3, "command", label = "Global Moran's I/Geary's C",
        command = function() moran_geary_fun())
  tkadd(spatFun3, "command", label = "LISA",
        command = function() fun_na())
  tkadd(spatFun3, "command", label = "Getis-Ord Gi*",
        command = function() fun_na())
  tkadd(spatMenu, "cascade", label = "Autocorrelation tests", menu = spatFun3)
  tkadd(spatFun5, "command", label = "Plot Spatial Covariance",
        command = function() fun_na())
  tkadd(spatFun5, "command", label = "Plot Spatial Semi-Variogram",
        command = function() fun_na())
  tkadd(spatFun5, "command", label = "Fit Semi-Variogram",
        command = function() fun_na())
  tkadd(spatMenu, "cascade", label = "Variogram Modeling", menu = spatFun5)
  tkadd(spatFun7, "command", label = "Kriging",
        command = function() fun_na())
  tkadd(spatFun7, "command", label = "TPS",
        command = function() fun_na())
  tkadd(spatFun7, "command", label = "IDW",
        command = function() fun_na())
  tkadd(spatMenu, "cascade", label = "Interpolation", menu = spatFun7)
  tkadd(topMenu, "cascade", label = "Spatial", menu = spatMenu)
  
  #Bayesian Menu
  BayesMenu <- tkmenu(topMenu, tearoff = FALSE) # TOP menu
  BayesFun <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  BayesFun2 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  BayesFun3 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  BayesFun4 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  #  BayesFun5 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  #  BayesFun6 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  tkadd(BayesFun, "command", label = "Bayesian 1 sample t-test",
        command = function() Bayes_onesam_tfun())
  #  tkadd(BayesMenu, "cascade", label = "GLM", menu = BayesFun)
  tkadd(BayesFun, "command", label = "Bayesian 2 sample t-test",
        command = function() Bayes_2tfun())
  #  tkadd(BayesMenu, "cascade", label = "GLMM", menu = BayesFun2)
  tkadd(BayesFun, "command", label = "Bayesian binomial test",
        command = function() Bayes_binom_fun())
  tkadd(BayesMenu, "cascade", label = "Bayesian Tests", menu = BayesFun)
  #  tkadd(BayesFun4, "command", label = "GLiMM Platform",
  #        command = function() fun_na())
  #  tkadd(BayesMenu, "cascade", label = "GLiMM", menu = BayesFun4)
  #  tkadd(BayesFun5, "command", label = "GLS Platform",
  #        command = function() fun_na())
  #  tkadd(BayesMenu, "cascade", label = "GLS", menu = BayesFun5)
  #  tkadd(BayesFun6, "command", label = "tGLM Platform",
  #        command = function() fun_na())
  #  tkadd(BayesMenu, "cascade", label = "tGLM", menu = BayesFun6)
  tkadd(topMenu, "cascade", label = "Bayesian", menu = BayesMenu)
  
  #Morphometrics Menu
  morphoMenu <- tkmenu(topMenu, tearoff = FALSE)  # TOP menu
  morphoFun <- tkmenu(topMenu, tearoff = FALSE)   # cascaded menu
  morphoFun2 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  morphoFun3 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  morphoFun4 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  morphoFun5 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  morphoFun6 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  morphoFun7 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  #tkadd(morphoFun, "command", label = ".csv",
  #      command = function() fun_na())
  #tkadd(morphoFun, "command", label = ".tps",
  #      command = function() fun_na())
  tkadd(morphoMenu, "command", label = "Import Data",
        command = function() readland_fun())
  #tkadd(morphoMenu, "cascade", label = "Read Data", menu = morphoFun)
  tkadd(morphoFun2, "command", label = "2D-Digitize",
        command = function() fun_na())
  # tkadd(morphoFun2, "command", label = "3D-Template",
  #       command = function() fun_na())
  tkadd(morphoFun2, "command", label = "3D-Digitize",
        command = function() fun_na())
  tkadd(morphoMenu, "cascade", label = "Digitize", menu = morphoFun2)
  #tkadd(morphoFun3, "command", label = "Procrustes",
  #      command = function() fun_na())
  #tkadd(morphoFun3, "command", label = "TPS",
  #      command = function() fun_na())
  tkadd(morphoMenu, "command", label = "GPA", 
        command = function() gpagen_fun())
  tkadd(morphoFun4, "command", label = "Tangent Space",
        command = function() fun_na())
  tkadd(morphoFun4, "command", label = "Deformation",
        command = function() fun_na())
  tkadd(morphoFun4, "command", label = "Reference-Target Deformation",
        command = function() fun_na())
  tkadd(morphoFun4, "command", label = "Analyses",
        command = function() fun_na())
  tkadd(morphoMenu, "cascade", label = "Plot", menu = morphoFun4)
  tkadd(topMenu, "cascade", label = "GMM", menu = morphoMenu)
  
  #Help Menu
  helpMenu <- tkmenu(topMenu, tearoff = FALSE)  # TOP menu
  helpFun <- tkmenu(topMenu, tearoff = FALSE)   # cascaded menu
  helpFun2 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  helpFun3 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  helpFun4 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  helpFun5 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  helpFun6 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  helpFun7 <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  helpFunB <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  helpFunC <- tkmenu(topMenu, tearoff = FALSE)  # cascaded menu
  #tkadd(helpMenu, "cascade", label = "About", menu = helpFunB)
  #tkadd(helpMenu, "cascade", label = "zooaRch User Guide", menu = helpFunC)
  tkadd(helpFun2, "command", label = "Syracuse Polygon",
        command = function() data("syraPoly", envir = environment()))
  tkadd(helpFun2, "command", label = "Spatial Polygon",
        command = function() data("spatPOLY", envir = environment()))
  tkadd(helpFun2, "command", label = "Random Spatial Points",
        command = function() data("randPTS", envir = environment()))
  tkadd(helpFun2, "command", label = "Clustered Spatial Points",
        command = function() data("clusterPTS", envir = environment()))
  tkadd(helpFun, "cascade", label = "Spatial", menu = helpFun2)
  tkadd(helpFun3, "command", label = "Binford's Data",
        command = function() data("LRBdata", envir = environment()))
  tkadd(helpFun3, "command", label = "Binford's Variable Key",
        command = function() data("LRBkey", envir = environment()))
  tkadd(helpFun3, "command", label = "Site-x-Species",
        command = function() data("myData", envir = environment()))
  tkadd(helpFun, "cascade", label = "Multivariate", menu = helpFun3)
  tkadd(helpFun4, "command", label = "Clarkson Deer Data",
        command = function() data("ClarksonLimbLoc", envir = environment()))
  tkadd(helpFun, "cascade", label = "Chi Squared", menu = helpFun4)
  tkadd(helpFun5, "command", label = "Garnsey Survival",
        command = function() data("speth83", envir = environment()))
  tkadd(helpFun5, "command", label = "Garnsey Labels",
        command = function() data("speth83labels", envir = environment()))
  tkadd(helpFun5, "command", label = "Marj Rabba Survival",
        command = function() {data("marjRab", envir = environment());print("Survival data from Marj Rabba, using Payne's (1973) age classes")})
  tkadd(helpFun5, "command", label = "Marj Rabba Labels",
        command = function() data("marjRlabels", envir = environment()))
  tkadd(helpFun5, "command", label = "Marj Rabba Fusion",
        command = function() data("marjRab.fuse", envir = environment()))
  tkadd(helpFun5, "command", label = "Winslow Fusion",
        command = function() data("winslow.fuse", envir = environment()))
  tkadd(helpFun, "cascade", label = "Survivorship", menu = helpFun5)
  tkadd(helpMenu, "cascade", label = "Example Data", menu = helpFun)
  tkadd(topMenu, "cascade", label = "Help", menu = helpMenu)
  
  tkfocus(tt)
}

# ZooArch Functions

# Not Available Function
fun_na <- function(){
  tkmessageBox(message = "Function not available. Check back soon!", icon = "info", type = "ok")
}

# dfs function
dfs.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(dfs) if (class(get(dfs))[1] == "data.frame") c(unlist(dfs)))),"Load User File")

# getshapefile function
getshapefile <- function() {
  name <- tclvalue(tkgetOpenFile(
    filetypes = "{{ESRI Shapefiles} {.shp}}"))
  if (name == "") return(data.frame());
  data <- raster::shapefile(name)
  file.name <- sub(x = basename(name), pattern = ".shp",replacement = "",ignore.case = TRUE)
  pos<-1
  envir <- as.environment(pos)
  assign(file.name, data, envir = envir)
}

# gettxtfile function
gettxtfile <- function() {
  name <- tclvalue(tkgetOpenFile(
    filetypes = "{{Space delimited files} {.txt}}"))
  if (name == "") return(data.frame());
  data <- as.data.frame(read.table(name,row.names = 1, sep = " "))
  file.name <- sub(x = basename(name), pattern = ".txt",replacement = "",ignore.case = TRUE)
  pos<-1
  envir <- as.environment(pos)
  assign(file.name, data, envir = envir)
}

# getxlfile function
getxlfile <- function() {
  name <- tclvalue(tkgetOpenFile(
    filetypes = "{{Excel files} {.xls .xlsx}}"))
  if (name == "") return(data.frame());
  data <- as.data.frame(read_excel(name,col_names = TRUE))
  file.name <- sub(x = basename(name), pattern = paste(".",tail(unlist(strsplit(basename(name),"\\.")),1),sep="")
                   ,replacement = "",ignore.case = TRUE)
  pos<-1
  envir <- as.environment(pos)
  assign(file.name, data, envir = envir)
}

# getspssfile function
getspssfile <- function() {
  name <- tclvalue(tkgetOpenFile(
    filetypes = "{{SPSS files} {.sav}}"))
  if (name == "") return(data.frame());
  data <- as.data.frame(read.spss(name, to.data.frame=TRUE))
  file.name <- sub(x = basename(name), pattern = ".sav",replacement = "",ignore.case = TRUE)
  pos<-1
  envir <- as.environment(pos)
  assign(file.name, data, envir = envir)
}

# getcsvfile function
getcsvfile <- function() {
  name <- tclvalue(tkgetOpenFile(
    filetypes = "{{Comma separated files} {.csv}}"))
  if (name == "") return(data.frame());
  data <- as.data.frame(read.csv(name,row.names = 1))
  file.name <- sub(x = basename(name), pattern = ".csv",replacement = "",ignore.case = TRUE)
  pos<-1
  envir <- as.environment(pos)
  assign(file.name, data, envir = envir)
}

# death_age_fun
death_age_fun<-function(){
  conf.level<-Fusion.groups<-UpperCI<-LowerCI<-Count<-NULL
  
  #Data Model: environment called "e"
  e <- new.env()
  e$data <- tclVar("Choose one"); 
  e$labels <- tclVar("Choose one");
  e$models <- tclVar("NULL"); 
  e$func <- tclVar("Survivorship"); #Selects Survivorship for the "Functions:" secetion (Defaults to Survivorship)
  e$conf.level <- tclVar(95); 
  e$iter <- tclVar(" ")
  
  # Wrapper for 3 Survival Functions
  runFunc <- function(Data=NULL,func=NULL,labels=NULL, models=NULL, ci=NULL, plot=TRUE, iter=NULL) {
    
    #Survivorship Function
    if (func=="Survivorship") {
      surv.func<-function(SurviveData=Data, labels=labels, models=models, ci=conf.level, plot=TRUE, iter=iter, usermod=NULL){
        data<-SurviveData$Ageclass 
        N.ages<-max(data)
        if(!is.null(labels)){
          Labels.ageclass <-labels
        }  
        if(is.null(labels)){
          Labels.ageclass <-1:N.ages
        }  
        if(!is.null(usermod) & !is.list(usermod)){
          stop("usmod MUST be entered as a list")
        }  
        survivorcurve.Eq4 <- function(data){
          vector <- rep(1, N.ages+1)   
          for(i in 1:N.ages+1) {
            vector[i] <- vector[i-1]*(sum(data >= (i-1)) - sum(data == (i-1)))/sum(data >= (i-1)) 
          }
          vector[is.na(vector)] <- 0
          round(vector,4) 
        }
        survivorcurve.Eq4(data)
        survive.matrix <- matrix(NA, ncol = N.ages+1, nrow = iter)
        survive.matrix[1,] <- survivorcurve.Eq4(data)
        for(i in 2:iter){
          bootstrap <- sample(1:length(data), length(data), replace = TRUE)
          survive.matrix[i,] <- survivorcurve.Eq4(data[bootstrap])
        }  
        ci<-ci/100
        upCI<-apply(survive.matrix[,-1], MARGIN = 2, FUN = quantile, 
                    probs = ci+((1-ci)/2))
        loCI<-apply(survive.matrix[,-1], MARGIN = 2, FUN = quantile, 
                    probs = ((1-ci)/2))
        if (plot==TRUE){
          plot(x = (1:N.ages), y = survive.matrix[1,-1], type = "n", xlab = "Age Class", 
               ylab = "Proportion Survived", axes = FALSE, ylim = c(0,1))
          polygon(c(1:N.ages,rev(1:N.ages)),c(loCI,rev(upCI)),col="light gray",border=NA)
          
          #Formating the Axes
          axis(side = 1, at = 1:N.ages, labels = Labels.ageclass)
          axis(side = 2)   
          lines(x = (1:N.ages), y = survive.matrix[1,-1], lwd = 2)
          
          #Plotting the Confidence Intervals
          lines(x = 1:N.ages, y = loCI, lty = "dashed", ylim = c(0,1))  #Plotting the lower confidence interval (2.5%)  
          lines(x = 1:N.ages, y = upCI, lty = "dashed", ylim = c(0,1)) #Plotting the upper confidence interval (97.5%)
          if(!is.null(models) & is.null(usermod)){
            surv.model<-list(Security = c(.904, .904, .645, .38, .25, .239, .171, .118, 0),
                             Milk = c(.47, .42, .39, .35, .28, .23, .19, .1, 0),
                             Wool = c(.85, .75, .65, .63, .57, .50, .43, .20, 0),
                             Catastrophic = c(.86, .73, .60, .49, .39, .31, .23, .16, .11, .07, .03, .01, 0),
                             Attritional = c(.76, .53, .48, .46, .45, .42, .39, .34, .29, .20, .11, .03, 0))
            for(j in 1:length(models)){ 
              i<-models[j]
              lines(x = 1:N.ages, y = surv.model[[i]], col = j+1, lwd = 2, lty = j)
            }    
            legend(x = "topright", cex = .75, lwd = 2,lty = c(1,2,1:length(models)), 
                   col = c(1,1,2:(length(models)+1)),
                   legend = c("Survivorship", paste(ci,"% Confidence Interval",sep=""), names(surv.model)[models]))
          }    
          if(!is.null(usermod)){
            surv.model<-usermod
            models<-1:length(surv.model)
            for(j in 1:length(models)){ 
              i<-models[j]
              lines(x = 1:N.ages, y = surv.model[[i]], col = j+1, lwd = 2, lty = j)
            }    
            legend(x = "topright", cex = .75, lwd = 2,lty = c(1,2,1:length(models)), 
                   col = c(1,1,2:(length(models)+1)),
                   legend = c("Survivorship", paste(ci,"% Confidence Interval",sep=""), names(surv.model)[models]))
          } 
        }  
        data.UpperCI <- upCI
        data.PointValue <- survive.matrix[1,-1]
        data.LowerCI <- loCI
        Taxon <- rep(unique(SurviveData$Genus), times = N.ages)
        Output.Matrix <- data.frame(Taxon = Taxon, AgeClassLabs = Labels.ageclass, LowerCI = data.LowerCI, 
                                    PointValue = data.PointValue, UpperCI = data.UpperCI)
        return(Output.Matrix)
      }
      Output.Matrix<- surv.func(SurviveData=Data, labels=labels, models=models, ci=ci, plot=TRUE, iter=iter, usermod=NULL)
    }
    #Mortality Function
    if (func == "Mortality") {
      mort.func<-function(mortData=Data,labels=labels, models=models, ci=conf.level, plot=TRUE, iter=iter, usermod=NULL,lsize=.01,...){
        data<-mortData$Ageclass 
        N.ages<-max(data)
        if(!is.null(labels)){
          Labels.ageclass <-labels
        }  
        if(is.null(labels)){
          Labels.ageclass <-1:N.ages
        }  
        if(!is.null(usermod) & !is.list(usermod)){
          stop("usmod MUST be entered as a list")
        }  
        mortprof <- function(data){
          vector <- rep(NA, N.ages)   
          for(i in 1:N.ages) {
            vector[i] <- sum(data==i)/length(data)
          }
          vector[is.na(vector)] <- 0
          round(vector,4) 
        }  
        mortprof(data)
        mortality.matrix <- matrix(NA, ncol = N.ages, nrow = iter)
        mortality.matrix[1,] <- mortprof(data)
        for(i in 2:iter){
          bootstrap <- sample(1:length(data), length(data), replace = TRUE)
          mortality.matrix[i,] <- unlist(mortprof(data[bootstrap]))
        }  
        ci<-ci/100  
        upCI<-apply(mortality.matrix[,], MARGIN = 2, FUN = quantile, 
                    probs = ci+((1-ci)/2))
        loCI<-lo<-apply(mortality.matrix[,], MARGIN = 2, FUN = quantile, 
                        probs = ((1-ci)/2))
        if (plot==TRUE){
          ylab="Mortality"
          xlab = "Age Class"
          if(is.null(models) & is.null(usermod)){
            bar<-barplot(mortality.matrix[1,],ylim=c(0,(max(upCI)+.1)),
                         names=Labels.ageclass,ylab=ylab,
                         xlab=xlab,beside=T)
            g<-(max(bar)-min(bar))/110
            for (i in 1:length(bar)){
              lines(c(bar[i],bar[i]),c(upCI[i],loCI[i]))
              lines(c(bar[i]-g,bar[i]+g),c(upCI[i],upCI[i]))
              lines(c(bar[i]-g,bar[i]+g),c(loCI[i],loCI[i]))
            }
          }
          if(!is.null(models) & is.null(usermod)){      
            mort.model<-list(Security = c(.096, 0, .259, .265, .13, .011, .068, .053, .118),
                             Milk = c(.53, .05, .03, .04, .07, .05, .04, .09, .1),
                             Wool = c(.15, .10, .10, .02, .06, .07, .07, .23, .20),
                             Catastrophic = c(.14, .13, .13, .11, .10, .08, .08, .07, .05, .04, .04, .02, .01),
                             Attritional = c(.24, .23, .05, .02, .01, .03, .03, .05, .05, .09, .09, .08, .03))
            dat<-rbind(mortality.matrix[1,], 
                       do.call(rbind,lapply(mort.model[models],matrix,
                                            ncol=length(mortality.matrix[1,]),
                                            byrow=TRUE)))      
            par(mfrow=c(length(models),1))
            ylab="Mortality"
            xlab = "Age Class"      
            for(j in 2:dim(dat)[1]){
              bar<-barplot(rbind(mortality.matrix[1,],dat[j,]),ylim=c(0,(max(c(upCI,dat))+.1)),names=Labels.ageclass,ylab=ylab,
                           xlab=xlab,beside=T,col=c("gray","black"))
              bar<-bar[1,]
              g<-(max(bar)-min(bar))/110        
              for (i in 1:length(bar))         {
                lines(c(bar[i],bar[i]),c(upCI[i],loCI[i]))
                lines(c(bar[i]-g,bar[i]+g),c(upCI[i],upCI[i]))
                lines(c(bar[i]-g,bar[i]+g),c(loCI[i],loCI[i]))
              } 
              #lines(x = bar, y = dat[j,],  lwd = 2,col="red")
              legend(x = "topright",
                     fill=c(NA,"gray","black"),border=c(NA,1,1),lty=c(1,0,0),
                     ncol=3,cex=.75,y.intersp=lsize,
                     legend = c(paste(ci*100,"% CI",sep=""), "Observed",names(mort.model)[models][j-1]))
            }
            par(mfrow=c(1,1))
          }    
          if(!is.null(usermod)){
            mort.model<-usermod
            models<-1:length(mort.model)
            dat<-rbind(mortality.matrix[1,], 
                       do.call(rbind,lapply(mort.model[models],matrix,
                                            ncol=length(mortality.matrix[1,]),
                                            byrow=TRUE)))      
            par(mfrow=c(length(models),1))
            ylab="Mortality"
            xlab = "Age Class"      
            for(j in 2:dim(dat)[1]){
              bar<-barplot(rbind(mortality.matrix[1,],dat[j,]),ylim=c(0,(max(c(upCI,dat))+.1)),names=Labels.ageclass,ylab=ylab,
                           xlab=xlab,beside=T,col=c("gray","black") )
              bar<-bar[1,]
              g<-(max(bar)-min(bar))/110        
              for (i in 1:length(bar))         {
                lines(c(bar[i],bar[i]),c(upCI[i],loCI[i]))
                lines(c(bar[i]-g,bar[i]+g),c(upCI[i],upCI[i]))
                lines(c(bar[i]-g,bar[i]+g),c(loCI[i],loCI[i]))
              } 
              #lines(x = bar, y = dat[j,],  lwd = 2,col="red")
              legend(x = "topright",
                     fill=c(NA,"gray","black"),border=c(NA,1,1),lty=c(1,0,0),
                     ncol=3,cex=.75,y.intersp=lsize,
                     legend = c(paste(ci*100,"% CI",sep=""), "Observed",names(mort.model)[models][j-1]))
            }
            par(mfrow=c(1,1))
          } 
        }  
        data.LowerCI <-loCI
        data.PointValue <- mortality.matrix[1,]
        data.UpperCI <- upCI
        Taxon <- rep(unique(mortData$Genus), times = N.ages)
        Output.Matrix <- data.frame(Taxon = Taxon, AgeClassLabs = Labels.ageclass, LowerCI = data.LowerCI, 
                                    PointValue = data.PointValue, UpperCI = data.UpperCI)
        return(Output.Matrix)
      }
      Output.Matrix<- mort.func(mortalityData=Data, labels=labels, models=models, ci=ci, plot=TRUE, iter=iter, usermod=NULL)
    }
    #Epiphyseal Fusion Function
    if (func == "Epiphyseal Fusion") {
      fuse.func<-function(data=Data,iter=1000,ci=95,plotci=TRUE,plot.title=NULL){
        cat(paste("Enter number of fusion groups"), "\n")
        ans<-readLines(n = 1)
        ans <- as.numeric(ans)
        fu.grps<-LETTERS[1:ans]
        ske.n<-numeric(length(fu.grps))
        for(i in 1:length(ske.n)){
          cat(paste("Enter number of skeletal elements for fusion group"), fu.grps[i],"\n")
          ans<-readLines(n = 1)
          ske.n[i] <- as.numeric(ans)
        }
        ele.list<-as.list(rep(NA,length(ske.n)))
        names(ele.list)<-fu.grps
        for(i in 1: length(ske.n)){
          cat(paste("Enter the", ske.n[i],"names of skeletal elements for fusion group"), fu.grps[i],"then press enter","\n")
          ele.list[[i]]<-readLines(n = ske.n[i])
        }  
        pctfuse<-function(dat){
          pct.ufu<-n<-numeric(length(ele.list))
          names(pct.ufu)<-fu.grps
          wh<-function(it){which(dat$Element==ele.list[[i]][it])} 
          for (i in 1:length(pct.ufu)){
            tab<-table(dat$Fusion[unlist(lapply(1:ske.n[i],wh))])
            fu<-tab[which(names(table(dat$Fusion[unlist(lapply(1:ske.n[i],wh))] ))=="Fused")]
            ufu<-tab[which(names(table(dat$Fusion[unlist(lapply(1:ske.n[i],wh))] ))=="Unfused")]
            if (is.nan(fu/(fu+ufu))){
              pct.ufu[i]<-0} else {pct.ufu[i]<-fu/(fu+ufu)}
            n[i]<-(fu+ufu)    
          }
          return(list(pct.ufu,n))
        }  
        #Bootstrapping
        boot <- matrix(NA, ncol = length(ele.list), nrow = iter) 
        boot[1,] <- pctfuse(data)[[1]]
        for(i in 2:iter){
          data.boot<-data[sample(1:dim(data)[1],dim(data)[1],replace=T),]
          boot[i,] <- pctfuse(data.boot)[[1]]
        }
        ci<-ci/100
        upCI<-ci+((1-ci)/2)
        loCI<-((1-ci)/2)
        
        #Creating a Table of the Bootstrap Results
        quantilematrix <- matrix(NA, ncol = 2, nrow = length(fu.grps))
        for(i in 1:ncol(boot)){
          quantilematrix[i,] <- quantile(boot[,i], probs = c(loCI, upCI), na.rm = T)
        }
        outputtable <- data.frame(Fusion.groups = fu.grps, 
                                  Data = round(boot[1,],2), 
                                  LowerCI = round(quantilematrix[,1],2), UpperCI = round(quantilematrix[,2],2), 
                                  Count = pctfuse(data)[[2]])
        
        #Plotting the Fusion data
        ciplot<-ggplot(outputtable, aes(x = Fusion.groups, y = Data))+
          geom_point(size = 3)+ #Adding the Points
          geom_errorbar(aes(ymax = UpperCI, ymin = LowerCI), width = 0.2)+  #adding the 95% confidence interval bars
          geom_text(aes(x = Fusion.groups, y = rep(1.05, length(Fusion.groups)), label = Count))+ #add in the sample size label for each fusion group
          theme(panel.background = element_blank(), #There is no background color for this plot
                panel.grid.minor = element_blank(),
                panel.border = element_rect(fill=NA, color = "black"),
                axis.title = element_text(color = "black", size = 20), #Font Size
                axis.text = element_text(color = "black", size = 15),
                axis.ticks = element_line(color = "black", size = 0.75), #The Line Thickness
                plot.title = element_text(color = "black", size = 24))+
          
          #Formatting the Output
          ylab("%Fused")+xlab("Fusion Group")+
          ggtitle(plot.title)
        if(plotci==TRUE){print(ciplot)}
        Ouput.Matrix<-list(Output = outputtable, Bootstrap.Data = boot)
        return(Ouput.Matrix)
      }
    }
    return(Output.Matrix)
  }
  #onOkSurv Function
  onOK <- function() {
    data <- tclvalue(e$data)
    if (data == "Load User File"){
      data <- getcsvfile()
      assign("data", data, envir = e)
    }
    if(data != "Load User File"){
      data <- get(data, .GlobalEnv)
      assign("data", data, envir = e)
    }
    labels <- tclvalue(e$labels)
    if (labels == "Load User File"){
      labels <- getcsvfile()
      assign("labels", labels, envir = e)
    }
    if(labels != "Load User File"){
      labels <- get(labels,  envir = .GlobalEnv)
      assign("labels", labels, envir = e)
    }
    models <- tclvalue(e$models)
    if (models == "NULL"){
      models <- NULL   #
    } else { models=as.numeric(unlist(models))}
    iter<-as.numeric(tclvalue(e$iter))
    assign("iter", iter, envir = e)
    conf.level<-as.numeric(tclvalue(e$conf.level))
    assign("conf.level", conf.level, envir = e)
    funcSelect <- tclvalue(e$func)
    func <- switch(funcSelect, "Survivorship" = funcSelect, "Mortality" = funcSelect, "Epiphyseal Fusion" = funcSelect)
    assign("func", funcSelect, envir = e)
    out<-runFunc(Data=e$data,func=e$func,labels=unlist(e$labels), models=models, ci=e$conf.level, plot=TRUE, iter=e$iter)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    print(out)
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "Survivorship Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Surviorship", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = "e")
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0)
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$data)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable(e$data)
  
  #Labels Combobox
  put_label(label_frame, "Labels:",1,0)
  data_labels <- ttkcombobox(label_frame, state = "readonly", 
                             values = dfs.fun(), 
                             textvariable = e$labels)
  tkgrid(data_labels, row = 1, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels) 
  
  #Models Combobox
  put_label(label_frame, "Model:",2,0)
  data_models <- ttkcombobox(label_frame, state = "readonly", 
                             values = c(1:5), 
                             textvariable = e$models)
  tkgrid(data_models, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_models) 
  
  #Conf.level Slider
  put_label(label_frame, "conf.level:", 3, 0)
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 3, column = 1, columnspan = 2, 
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame, 
                               from = 75, to = 100,  
                               variable = e$conf.level)
  #Conf.level Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 75, to = 100, increment = 1, 
                               textvariable = e$conf.level, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
  
  #Function Radiobutton
  put_label(label_frame, "Functions:",4,0)
  rb_frame <- ttkframe(label_frame)
  sapply(c("Survivorship","Mortality","Epiphyseal Fusion"), function(i) {
    radio_button <- tk2radiobutton(rb_frame, variable = e$func,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 4, column = 1, sticky = "w")
  
  #Iterations Spinbox
  put_label(label_frame, "Iterations:",5,0)
  iter_frame <- tkspinbox(label_frame, from = 10, to = 10000, increment = 10, textvariable = e$iter, width = 6)
  tkgrid(iter_frame, row = 5, column = 1, sticky="w", padx = 2)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw OK Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = onOK)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")               # add a spring
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = onOK)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# srarefun
s_rarefun<-function(){
  
  
  rarefac.fun <- function(data=e$dataframe, m=e$mval, ci = e$conf.level){
    k <- data[data > 0] #species frequencies
    n <- sum(k) #total site size
    f0.est <- function(f1,f2){
      ifelse(f2>0,(f1^2)/(2*f2),(f1*(f1-1)))
    }
    f1 <- sum(k==1)
    f2 <- sum(k==2)
    f0 <- f0.est(f1,f2)
    S.est <- length(k)+f0
    alphakm <- function(k,m,n){
      res <- c()
      for(i in 1:length(k)){
        res[i] <- ifelse(n - m < k[i],0,exp((lfactorial(n-k[i])+lfactorial(n-m))-(lfactorial(n)+lfactorial(n-k[i]-m))))
      }
      res
    }
    se.rare <- function(k,m,n,S.est){
      sqrt(sum((1-alphakm(k,m,n))^2)-(sum(1-alphakm(k,m,n)))^2/S.est)
    }
    extrap <- function(f0,f1, mstar, n){
      f0*(1 - (1 - (f1/(n*f0)))^mstar)
    }
    se.extrap <- function(k, mstar, n, S.est){
      f1.2.low <- matrix(c(sum(k==1),sum(k==2)),nrow=max(k),ncol=2,byrow=TRUE)
      f1.2.high <- matrix(c(sum(k==1),sum(k==2)),nrow=max(k),ncol=2,byrow=TRUE)
      f1.2.low[1,1] <- max(f1.2.low[1,1]-1,0)
      f1.2.low[2,2] <- max(f1.2.low[2,2]-1,0)
      f1.2.high[1,1] <- f1.2.high[1,1]+1
      f1.2.high[2,2] <- f1.2.high[2,2]+1
      ds.df <- c()
      for(i in 1:max(k)){
        
        #Low Estimate
        f0.low <- f0.est(f1.2.low[i,1],f1.2.low[i,2])
        S.est.low <- length(k)-1+f0.low
        S.low <- length(k)-1+extrap(f0.low,f1.2.low[i,1],mstar,n)
        
        #High Estimate
        f0.high <- f0.est(f1.2.high[i,1],f1.2.high[i,2])
        S.est.high <- length(k)+1+f0.high
        S.high <- length(k)+1+extrap(f0.high,f1.2.high[i,1],mstar,n)
        ds.df[i] <- (S.high - S.low)/2
      }
      #Calculating the Variances
      if(length(k) == 1){
        var.extrap <- (ds.df[1]^2)*(1 - k/S.est)
      }
      if(length(k) > 1){
        offdag <- c()
        for(i in 1:max(k)-1){
          for(j in (i+1):max(k)){
            offdag <- c(offdag, (ds.df[i]*ds.df[j])*(-sum(k==i)*sum(k==j)/S.est))
          }
        }
        dag <- c()
        for(i in 1:max(k)){
          dag <- c(dag,(ds.df[i]^2)*sum(k==i)*(1 - sum(k==i)/S.est))
        }
        var.extrap <- sum(dag)+2*sum(offdag)
      }
      sqrt(var.extrap)
    }
    outpt <- data.frame(m = 0, S.ind = 0, SE = 0, CImin = 0, CImax = 0)
    for(i in 1:m){
      if(i < n){ #rarefaction
        outpt[(i+1),1] <- i
        outpt[(i+1),2] <- sum(1-alphakm(k,i,n)) #estimate
        outpt[(i+1),3] <- se.rare(k,i,n,S.est) #standard error
        outpt[(i+1),4] <- outpt[(i+1),2]+qnorm((1-ci)/2)*outpt[(i+1),3]
        outpt[(i+1),5] <- outpt[(i+1),2]+qnorm(1-(1-ci)/2)*outpt[(i+1),3]
      }
      if(i >= n){ #extrapolation
        mstar <- i - n
        outpt[(i+1),1] <- i
        outpt[(i+1),2] <- length(k) + extrap(f0,f1,mstar,n) #estimate
        outpt[(i+1),3] <- se.extrap(k, mstar, n, S.est) #standard error
        outpt[(i+1),4] <- outpt[(i+1),2]+qnorm((1-ci)/2)*outpt[(i+1),3]
        outpt[(i+1),5] <- outpt[(i+1),2]+qnorm(1-(1-ci)/2)*outpt[(i+1),3]
      }
    }
    return(outpt)
  }
  #Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$variablename<-tclVar("Choose Variable")
  e$variablevalue<-tclVar("Choose Variable")
  e$plot<-tclVar(FALSE)
  e$mval<-tclVar(50)
  e$conf.level<-tclVar(95)
  
  #OK Function
  OKfun<-function(){
    e$variablename<-tclvalue(e$variablename)
    variablevalue <- get(e$variablename,e$dataframe,inherits=TRUE)
    assign("variablevalue", variablevalue, envir = e)
    e$mval<-as.numeric(tclvalue(e$mval))
    e$conf.level<-as.numeric(tclvalue(e$conf.level))*.01
    out<-rarefac.fun (data=e$dataframe, m=e$mval, ci = e$conf.level)
    #out$data.name<-e$variablename
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    print(out)
    
    #Plotting
    if(tclvalue(e$plot)>0){
      plot(out$m,out$S.ind,type="n" 
           ,main=paste("Rarefaction of ",e$variablename)
           ,ylim=range(c(out$CImin,out$CImax))
           ,ylab="S",xlab="m")
      polygon(c(out$m,rev(out$m)),c(out$CImin,rev(out$CImax)),col="light gray",border=NA)
      lines(out$m,out$S.ind,col="red",lwd=2,lty=2)
      lines(out$m,out$CImin,col="black",lwd=2,lty=2)
      lines(out$m,out$CImax,col="black",lwd=2,lty=2)#
    }
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "Rarefaction Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Single Sample Rarefaction", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = "e")
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0)
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Variable Combobox
  put_label(label_frame, "Variable:",1,0)
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variablename)
  tkgrid(data_labels1, row = 1, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #M Value Slider
  put_label(label_frame, "m value:", 2, 0)
  m_value_frame <- ttkframe(label_frame)
  tkgrid(m_value_frame, row = 2, column = 1, columnspan = 2, 
         sticky = "w")
  m_value_scale <- ttkscale(m_value_frame, 
                            from = 0, to = 1000,  
                            variable = e$mval)
  #M Value Spinbox 
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  m_value_spin <- tkspinbox(m_value_frame, 
                            from = 0, to = 1000, increment = 1, 
                            textvariable = e$mval, width = 5)
  tkpack(m_value_scale, side = "left")
  tkpack(m_value_spin, side = "left")
  
  #Conf.level Slider
  put_label(label_frame, "conf.level:", 3, 0)
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 3, column = 1, columnspan = 2, 
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame, 
                               from = 75, to = 100,  
                               variable = e$conf.level)
  #Conf.level Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 75, to = 100, increment = 1, 
                               textvariable = e$conf.level, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
  
  #Plot Checkbox
  put_label ( label_frame , "plot:" , 4 , 0)
  ecdfplot_check <-ttkcheckbutton (label_frame , variable = e$plot)
  tkgrid (ecdfplot_check , row = 4 , column = 1 , stick = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# ksfun
ksfun<-function(){
  
  
  #Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$variablename<-tclVar("Choose Variable")
  e$variablevalue<-tclVar("Choose Variable")
  e$ecdf<-tclVar(FALSE)
  
  #OK Function
  OKfun<-function(){
    e$variablename<-tclvalue(e$variablename)
    variablevalue <- get(e$variablename,e$dataframe,inherits=TRUE)
    assign("variablevalue", variablevalue, envir = e)
    out<-suppressWarnings(ks.test(e$variablevalue,y="pnorm")) # suppross warnings for now
    out$data.name<-e$variablename
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    print(out)
    
    #Plotting
    if(tclvalue(e$ecdf)>0){
      x<-e$variablevalue
      ref<-rnorm(10000,mean(x),sd(x))
      plot(ecdf(x),xlim=range(c(x,ref)),main=paste("ecdf of ",e$variablename))
      lines(ecdf(ref),col="red",lwd=2)
    }
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "KS test Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Kolmogorov-Smirnov Test", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = "e")
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0)
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Variable Combobox
  put_label(label_frame, "Variable:",1,0)
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variablename)
  tkgrid(data_labels1, row = 1, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #ecdfplot Checkbox
  put_label ( label_frame , "ecdf plot:" , 2 , 0)
  ecdfplot_check <-ttkcheckbutton (label_frame , variable = e$ecdf)
  tkgrid (ecdfplot_check , row = 2 , column = 1 , stick = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# tfun
tfun<-function(){
  
  
  #Data Model: environment called "e"
  e <- new.env()
  e$data <- tclVar("Choose one"); 
  e$variable1 <- tclVar("Choose one");
  e$variable2 <- tclVar("Choose one");
  e$alternative <- tclVar("two.sided"); 
  e$paired<- tclVar(FALSE);
  e$var <- tclVar(FALSE); 
  e$conf.level <- tclVar(95); 
  
  #Ok Function
  OKfun <- function() {
    variable1name<-tclvalue(e$variable1)
    variable1 <- get(variable1name,e$data,inherits=TRUE)
    assign("variable1", variable1, envir = e)
    variable2name<-tclvalue(e$variable2)
    variable2 <- get(variable2name,e$data,inherits=TRUE)
    assign("variable2", variable2, envir = e)
    paired <- as.numeric(tclvalue(e$paired))
    assign("paired", paired, envir = e)
    var<-as.numeric(tclvalue(e$var))
    assign("var", var, envir = e)
    conf.level<-NULL
    conf.level<-as.numeric(tclvalue(e$conf.level))*.01
    assign("conf.level", conf.level, envir = e)
    altselect <- tclvalue(e$alternative)
    alternative <- switch(altselect , "two.sided" = altselect , "less" = altselect , "greater" = altselect )
    assign("alternative", altselect, envir = e)
    out<-t.test(data= e$data, x=e$variable1, y=e$variable2, conf.level = e$conf.level, alternative = e$alternative, paired = e$paired, var.equal = e$var)
    out$data.name<-paste(variable1name, "and", variable2name)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    print(out)
    tkdestroy(window)
  }
  
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "2-Sample t-test Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "2-Sample t-test", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = "e")
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0)
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$data)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable(e$data)
  
  data <- tclvalue(e$data)
  if (data == "Load User File"){
    data <- getcsvfile()
    assign("data", data, envir = e)
  }
  if(data != "Load User File"){
    data <- get(data, .GlobalEnv)
    assign("data", data, envir = e)
  }
  #Variable 1 Combobox
  put_label(label_frame, "Variable 1:",1,0)
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = colnames(e$data), 
                              textvariable = e$variable1)
  tkgrid(data_labels1, row = 1, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #Variable 2 Combobox
  put_label(label_frame, "Variable 2:",2,0)
  data_labels2 <- ttkcombobox(label_frame, state = "readonly", 
                              values = colnames(e$data), 
                              textvariable = e$variable2)
  tkgrid(data_labels2, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  
  #Conf.level Slider
  put_label(label_frame, "conf.level:", 3, 0)
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 3, column = 1, columnspan = 2, 
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame, 
                               from = 75, to = 100,  
                               variable = e$conf.level)
  #Conf.level Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 75, to = 100, increment = 1, 
                               textvariable = e$conf.level, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
  
  #Function Radiobutton
  put_label(label_frame, "Alternative:",4,0)
  rb_frame <- ttkframe(label_frame)
  sapply(c("two.sided","less","greater"), function(i) {
    radio_button <- tk2radiobutton(rb_frame, variable = e$alternative,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 4, column = 1, sticky = "w")
  
  #Equal Variance Checkbox
  put_label ( label_frame , "var.equal:" , 5 , 0)
  var_equal_check <-ttkcheckbutton ( label_frame , variable = e$var)
  tkgrid ( var_equal_check , row = 5 , column = 1 , stick = "w" ,padx = 2)
  
  #Paired or Independent Checkbox
  put_label ( label_frame , "paired:" , 6 , 0)
  paired_check <-ttkcheckbutton (label_frame , variable = e$paired)
  tkgrid (paired_check , row = 6 , column = 1 , stick = "w" ,padx = 2)
  
  #Iterations # for when resampling is added
  #put_label(label_frame, "Iterations:",6,0)
  #iter_frame <- tkspinbox(label_frame, from = 10, to = 10000, increment = 10, textvariable = e$iter, width = 6)
  #tkgrid(iter_frame, row = 6, column = 1, sticky="w", padx = 2)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")               # add a spring
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
}

# onesam_tfun
onesam_tfun<-function(){
  
  
  #Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$variable1 <- tclVar("Choose one");
  e$alternative <- tclVar("two.sided"); 
  #e$paired<- tclVar(FALSE);
  #e$var <- tclVar(FALSE); 
  e$conf.level <- tclVar(95); 
  e$mu <- tclVar(0);
  
  #Ok Function
  OKfun <- function() {
    variable1name<-tclvalue(e$variable1)
    variable1 <- get(variable1name,e$dataframe,inherits=TRUE)
    assign("variable1", variable1, envir = e)
    conf.level<-as.numeric(tclvalue(e$conf.level))*.01
    assign("conf.level", conf.level, envir = e)
    altselect <- tclvalue(e$alternative)
    alternative <- switch(altselect , "two.sided" = altselect , "less" = altselect , "greater" = altselect )
    assign("alternative", altselect, envir = e)
    out<- t.test(data= e$dataframe, x=variable1, mu=as.numeric(tclvalue(e$mu)), conf.level = e$conf.level, alternative = e$alternative)
    out$data.name<-paste(variable1name)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    print(out)
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "One Sample t-test Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "One Sample t-test", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = "e")
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0)
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable(e$dataname)
  
  dataname <-tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Variable Combobox
  put_label(label_frame, "Variable:",1,0)
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable1)
  tkgrid(data_labels1, row = 1, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #Mu Entry
  put_label(label_frame, "Mu:",2,0)
  data_labels2 <- ttkentry(label_frame,
                           textvariable = e$mu, width=5)
  tkgrid(data_labels2, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  
  #Conf.level Slider
  put_label(label_frame, "conf.level:", 3, 0)
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 3, column = 1, columnspan = 2, 
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame, 
                               from = 75, to = 100,  
                               variable = e$conf.level)
  #Conf.level Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 75, to = 100, increment = 1, 
                               textvariable = e$conf.level, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
  
  #Function Radiobutton
  put_label(label_frame, "Alternative:",4,0)
  rb_frame <- ttkframe(label_frame)
  sapply(c("two.sided","less","greater"), function(i) {
    radio_button <- tk2radiobutton(rb_frame, variable = e$alternative,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 4, column = 1, sticky = "w")
  
  #Iterations for when resampling is added
  #put_label(label_frame, "Iterations:",6,0)
  #iter_frame <- tkspinbox(label_frame, from = 10, to = 10000, increment = 10, textvariable = e$iter, width = 6)
  #tkgrid(iter_frame, row = 6, column = 1, sticky="w", padx = 2)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")               # add a spring
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
}

# shapirofun
shapirofun<-function(){
  
  
  #Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$variablename<-tclVar("Choose Variable")
  e$variablevalue<-tclVar("Choose Variable")
  e$qq<-tclVar(FALSE)
  
  #OK Function
  OKfun<-function(){
    e$variablename<-tclvalue(e$variablename)
    variablevalue <- get(e$variablename,e$dataframe,inherits=TRUE)
    assign("variablevalue", variablevalue, envir = e)
    out<-shapiro.test(e$variablevalue)
    out$data.name<-e$variablename
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    print(out)
    
    #Plotting
    if(tclvalue(e$qq)>0){
      #graph viewer
      # hscale <- 2    # Horizontal scaling
      #vscale <- 2   # Vertical scaling
      # plot.shap <- function() {
      #  qqnorm(e$variablevalue)
      #  qqline(e$variablevalue, col = 2)
      #}
      #graph <- tkrplot(label_frame, fun = plot.shap,
      #                 hscale = hscale, vscale = vscale)
      #tkgrid(graph, row = 3, column = 1, rowspan = 1, sticky = "w", padx = 2)
      qqnorm(e$variablevalue)
      qqline(e$variablevalue, col = 2)
    }
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "Shapiro-Wilk's test Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Shapiro-Wilk's Test", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = "e")
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0)
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Variable Combobox
  put_label(label_frame, "Variable:",1,0)
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variablename)
  tkgrid(data_labels1, row = 1, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #qqplot Checkbox
  put_label ( label_frame , "qqplot:" , 2 , 0)
  qqplot_check <-ttkcheckbutton (label_frame , variable = e$qq)
  tkgrid (qqplot_check , row = 2 , column = 1 , stick = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# bartlettsfun
bartlettsfun<-function(){
  
  
  #Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$variable1name<-tclVar("Choose Variable")
  e$variable1value<-tclVar("Choose Variable")
  e$variable2name<-tclVar("Choose Variable")
  e$variable2value<-tclVar("Choose Variable")
  e$plot<-tclVar(FALSE)
  
  #OK Function
  OKfun<-function(){
    e$variable1name<-tclvalue(e$variable1name)
    variable1value <- get(e$variable1name,e$dataframe,inherits=TRUE)
    assign("variable1value", variable1value, envir = e)
    e$variable2name<-tclvalue(e$variable2name)
    variable2value <- get(e$variable2name,e$dataframe,inherits=TRUE)
    assign("variable2value", variable2value, envir = e)
    out<-bartlett.test(list(x=variable1value, x2=variable2value))
    out$data.name<-paste(e$variable1name, "and", e$variable2name)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    print(out)
    
    #Plotting
    if(tclvalue(e$plot)>0){
      #graph viewer
      # hscale <- 2    # Horizontal scaling
      #vscale <- 2   # Vertical scaling
      # plot.shap <- function() {
      ###plotting functions
      #}
      #graph <- tkrplot(label_frame, fun = plot.shap,
      #                 hscale = hscale, vscale = vscale)
      #tkgrid(graph, row = 3, column = 1, rowspan = 1, sticky = "w", padx = 2)
      vals<-c(e$variable1value, e$variable2value)
      names<-as.factor(c(rep(e$variable1name,length(e$variable1value)),rep(e$variable2name,length(e$variable2value))))
      boxplot(vals ~ names)
      stripchart(vals ~ names,pch=16, vert=TRUE, add=TRUE,cex=1)
    }
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "Bartlett's test Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Bartlett's Variance Test", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = "e")
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0)
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Variable 1 Combobox
  put_label(label_frame, "Variable 1:",1,0)
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable1name)
  tkgrid(data_labels1, row = 1, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #Variable 2 Combobox
  put_label(label_frame, "Variable 2:",2,0)
  data_labels2 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable2name)
  tkgrid(data_labels2, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  
  #Plot Checkbox
  put_label ( label_frame , "plot:" , 3 , 0)
  plot_check <-ttkcheckbutton (label_frame , variable = e$plot)
  tkgrid (plot_check , row = 3 , column = 1 , stick = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# F_var_fun
F_var_fun<-function(){
  
  
  #Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$variable1name<-tclVar("Choose Variable")
  e$variable1value<-tclVar("Choose Variable")
  e$variable2name<-tclVar("Choose Variable")
  e$variable2value<-tclVar("Choose Variable")
  e$plot<-tclVar(FALSE)
  e$alternative<-tclVar("two.sided")
  e$conf.level<-tclVar(95)
  
  #OK Function
  OKfun<-function(){
    e$variable1name<-tclvalue(e$variable1name)
    variable1value <- get(e$variable1name,e$dataframe,inherits=TRUE)
    assign("variable1value", variable1value, envir = e)
    e$variable2name<-tclvalue(e$variable2name)
    variable2value <- get(e$variable2name,e$dataframe,inherits=TRUE)
    assign("variable2value", variable2value, envir = e)
    conf.level<-NULL 
    conf.level<-as.numeric(tclvalue(e$conf.level))*.01
    assign("conf.level", conf.level, envir = e)
    altselect <- tclvalue(e$alternative)
    alternative <- switch(altselect , "two.sided" = altselect , "less" = altselect , "greater" = altselect )
    assign("alternative", altselect, envir = e)
    out<-var.test(x=variable1value, y=variable2value,alternative=e$alternative, conf.level = e$conf.level)
    out$data.name<-paste(e$variable1name, "and", e$variable2name)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    print(out)
    
    #Plotting
    if(tclvalue(e$plot)>0){
      #graph viewer
      # hscale <- 2    # Horizontal scaling
      #vscale <- 2   # Vertical scaling
      # plot.shap <- function() {
      ###plotting functions
      #}
      #graph <- tkrplot(label_frame, fun = plot.shap,
      #                 hscale = hscale, vscale = vscale)
      #tkgrid(graph, row = 3, column = 1, rowspan = 1, sticky = "w", padx = 2)
      vals<-c(e$variable1value, e$variable2value)
      names<-as.factor(c(rep(e$variable1name,length(e$variable1value)),rep(e$variable2name,length(e$variable2value))))
      boxplot(vals ~ names)
      stripchart(vals ~ names,pch=16, vert=TRUE, add=TRUE,cex=1)
    }
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "F Variance Test Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "F Variance Test", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = "e")
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0)
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Variable 1 Combobox
  put_label(label_frame, "Variable 1:",1,0)
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable1name)
  tkgrid(data_labels1, row = 1, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #Variable 2 Combobox
  put_label(label_frame, "Variable 2:",2,0)
  data_labels2 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable2name)
  tkgrid(data_labels2, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  
  #Conf.level Slider
  put_label(label_frame, "conf.level:", 3, 0)
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 3, column = 1, columnspan = 2, 
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame, 
                               from = 75, to = 100,  
                               variable = e$conf.level)
  #Conf.level Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 75, to = 100, increment = 1, 
                               textvariable = e$conf.level, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
  
  #Hypothesis Radiobuttons
  put_label(label_frame, "Alternative:",4,0)
  rb_frame <- ttkframe(label_frame)
  sapply(c("two.sided","less","greater"), function(i) {
    radio_button <- tk2radiobutton(rb_frame, variable = e$alternative,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 4, column = 1, sticky = "w")
  
  #Plot Checkbox
  put_label ( label_frame , "plot:" , 5 , 0)
  plot_check <-ttkcheckbutton (label_frame , variable = e$plot)
  tkgrid (plot_check , row = 5 , column = 1 , stick = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# LevenesVarTestFun
LevenesVarTestFun<-function(){
  #Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$variable1name<-tclVar("Choose Variable")
  e$variable1value<-tclVar("Choose Variable")
  e$variable2name<-tclVar("Choose Variable")
  e$variable2value<-tclVar("Choose Variable")
  e$plot<-tclVar(FALSE)
  e$center<-tclVar("mean")
  
  #OK Function
  OKfun<-function(){
    e$variable1name<-tclvalue(e$variable1name)
    variable1value <- get(e$variable1name, e$dataframe,inherits=TRUE)
    assign("variable1value", variable1value, envir = e)
    e$variable2name<-tclvalue(e$variable2name)
    variable2value <- get(e$variable2name,e$dataframe,inherits=TRUE)
    assign("variable2value", variable2value, envir = e)
    centselect <- tclvalue(e$center)
    center <- switch(centselect , "mean" = centselect , "median" = centselect )
    assign("center", centselect, envir = e)
    yvars<-c(e$variable1value, e$variable2value)
    factor<-as.factor(c(rep(e$variable1name,length(e$variable1value)),rep(e$variable2name,length(e$variable2value))))
    a<-paste("leveneTest(", "yvars", " ~ ", "factor , ", "center=",center,")",sep = "")
    out<-eval(parse(text = a))
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    print(out)
    
    #Plotting
    if(tclvalue(e$plot)>0){
      #graph viewer
      # hscale <- 2    # Horizontal scaling
      #vscale <- 2   # Vertical scaling
      # plot.shap <- function() {
      ###plotting functions
      #}
      #graph <- tkrplot(label_frame, fun = plot.shap,
      #                 hscale = hscale, vscale = vscale)
      #tkgrid(graph, row = 3, column = 1, rowspan = 1, sticky = "w", padx = 2)
      boxplot(yvars ~ factor)
      stripchart(yvars ~ factor,pch=16, vert=TRUE, add=TRUE,cex=1)
    }
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "Levene's Variance Test Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Levene's Variance Test", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = "e")
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0)
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable(e$dataname)
  
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = tclvalue(e$dataname), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)
  dataname <-tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Variable 1 Combobox
  put_label(label_frame, "Variable 1:",1,0)
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable1name)
  tkgrid(data_labels1, row = 1, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #Variable 2 Combobox
  put_label(label_frame, "Variable 2:",2,0)
  data_labels2 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable2name)
  tkgrid(data_labels2, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  
  #Hypothesis Radiobuttons
  put_label(label_frame, "Center:",3,0)
  rb_frame <- ttkframe(label_frame)
  sapply(c("mean","median"), function(i) {
    radio_button <- tk2radiobutton(rb_frame, variable = e$center,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 3, column = 1, sticky = "w")
  
  #Plot Checkbox
  put_label ( label_frame , "plot:" , 5 , 0)
  plot_check <-ttkcheckbutton (label_frame , variable = e$plot)
  tkgrid (plot_check , row = 5 , column = 1 , stick = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# summar_fun
summar_fun<-function(){
  
  skew<-function (x, na.rm = TRUE) {
    if (is.matrix(x)) 
      apply(x, 2, skew, na.rm = na.rm)
    else if (is.vector(x)) {
      if (na.rm) 
        x <- x[!is.na(x)]
      n <- length(x)
      (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/n)^(3/2)
    }
    else if (is.data.frame(x)) 
      sapply(x, skew, na.rm = na.rm)
    else skew(as.vector(x), na.rm = na.rm)
  }
  kurt<-function (x, na.rm = TRUE) {
    if (is.matrix(x)) 
      apply(x, 2, kurt, na.rm = na.rm)
    else if (is.vector(x)) {
      if (na.rm) 
        x <- x[!is.na(x)]
      n <- length(x)
      n * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2)
    }
    else if (is.data.frame(x)) 
      sapply(x, kurt, na.rm = na.rm)
    else kurt(as.vector(x), na.rm = na.rm)
  }
  #Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$variable1name<-tclVar("Choose Variable")
  e$variable1value<-tclVar("Choose Variable")
  #e$variable2name<-tclVar("Choose Variable")
  #e$variable2value<-tclVar("Choose Variable")
  #e$plot<-tclVar(FALSE)
  e$type<-tclVar("Compact")
  #e$conf.level<-tclVar(95)
  printfun<-function(k){
    summout<-c(round(var(k,na.rm=TRUE),2)
               ,round(sd(k,na.rm = TRUE),2)
               ,round(IQR(k,na.rm=TRUE),2)
               ,round(mean(k,na.rm=TRUE),2)
               ,round(median(k,na.rm=TRUE),2)
               ,round(skew(k,na.rm=TRUE),2)
               ,round(kurt(k,na.rm=TRUE),2)
    )
    names(summout)<-c("Var","SD","IQR","Mean","Median","Skewness", "Kurtosis")
    cat("\n",e$type,"summary statistics of", e$variable1name,"\n\n")
    print(summout)
    cat("\n")
    cat("Percentiles","\n")
    print(quantile(k,c(0,.25,.5,.75,.9,.95,.99,1),na.rm=TRUE))
    sumar<-c(summout,quantile(k,c(0,.25,.5,.75,.9,.95,.99,1),na.rm=TRUE))
    return(sumar)
  }
  #OK Function
  OKfun<-function(){
    e$variable1name<-tclvalue(e$variable1name)
    variable1value <- get(e$variable1name,e$dataframe,inherits=TRUE)
    assign("variable1value", variable1value, envir = e)
    typeselect <- tclvalue(e$type)
    type <- switch(typeselect , "Compact" = typeselect , "Detailed" = typeselect )
    assign("type", typeselect, envir = e)
    out<-printfun(variable1value)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "Data Summary Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Data Summary", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = "e")
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0)
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable (e$dataname)
  
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = tclvalue(e$dataname), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo) 
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Variable 1 Combobox
  put_label(label_frame, "Variable 1:",1,0)
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable1name)
  tkgrid(data_labels1, row = 1, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #Summary Radiobuttons
  put_label(label_frame, "Summary Type:",2,0)
  rb_frame <- ttkframe(label_frame)
  sapply(c("Compact","Detailed"), function(i) {
    radio_button <- tk2radiobutton(rb_frame, variable = e$type,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 2, column = 1, sticky = "w")
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# Assoc_fun
Assoc_fun<-function(){
  
  
  #Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$variable1name<-tclVar("Choose Variable")
  e$variable1value<-tclVar("Choose Variable")
  e$variable2name<-tclVar("Choose Variable")
  e$variable2value<-tclVar("Choose Variable")
  e$pearson<-tclVar(FALSE)
  e$spearman<-tclVar(FALSE)
  e$kendall<-tclVar(FALSE)
  e$cov<-tclVar(FALSE)
  #e$gkGamma<-tclVar(FALSE)
  e$alternative<-tclVar("two.sided")
  e$conf.level<-tclVar(95)
  e$plot<-tclVar(FALSE)
  
  #OK Function
  OKfun<-function(){
    e$variable1name<-tclvalue(e$variable1name)
    variable1value <- get(e$variable1name,e$dataframe,inherits=TRUE)
    assign("variable1value", variable1value, envir = e)
    e$variable2name<-tclvalue(e$variable2name)
    variable2value <- get(e$variable2name,e$dataframe,inherits=TRUE)
    assign("variable2value", variable2value, envir = e)
    pearson <- as.numeric(tclvalue(e$pearson))
    assign("pearson", pearson, envir = e)
    spearman <- as.numeric(tclvalue(e$spearman))
    assign("spearman", spearman, envir = e)
    kendall <- as.numeric(tclvalue(e$kendall))
    assign("kendall", kendall, envir = e)
    covar <- as.numeric(tclvalue(e$cov))
    assign("cov", covar, envir = e)
    #gkgamma <- as.numeric(tclvalue(e$gkGamma))
    #assign("gkgamma", gkgamma, envir = e)
    conf.level<-NULL
    conf.level<-as.numeric(tclvalue(e$conf.level))*.01
    assign("conf.level", conf.level, envir = e)
    altselect <- tclvalue(e$alternative)
    alternative <- switch(altselect , "two.sided" = altselect , "less" = altselect , "greater" = altselect )
    assign("alternative", altselect, envir = e)
    if(e$pearson==1){
      out1<- cor.test(x=variable1value, y=variable2value,
                      alternative = e$alternative,
                      method = "pearson",
                      exact = NULL, conf.level = e$conf.level)
      out1$data.name<-paste(e$variable1name, "and", e$variable2name)
      pos<-1
      envir <- as.environment(pos)
      assign("Pearson_Results", out1, envir = envir) 
      print(out1)
    }
    if(e$spearman==1){
      out2<- cor.test(x=variable1value, y=variable2value,
                      alternative = e$alternative,
                      method = "spearman",
                      exact = TRUE, conf.level = e$conf.level)
      out2$data.name<-paste(e$variable1name, "and", e$variable2name)
      pos<-1
      envir <- as.environment(pos)
      assign("Spearman_Results", out2, envir = envir) 
      print(out2)
    }
    if(e$kendall==1){
      out3<- cor.test(x=variable1value, y=variable2value,
                      alternative = e$alternative,
                      method = "kendall",
                      exact = TRUE, conf.level = e$conf.level)
      out3$data.name<-paste(e$variable1name, "and", e$variable2name)
      pos<-1
      envir <- as.environment(pos)
      assign("Kendall_Results", out3, envir = envir) 
      print(out3)
    }
    if(e$cov==1){
      out4<- cov(x=variable1value, y=variable2value, use = "everything",
                 method = "pearson")
      names(out4)<-paste("Covariance of ",e$variable1name, "and", e$variable2name)
      pos<-1
      envir <- as.environment(pos)
      assign("Covariance_Results", out4, envir = envir) 
      print(out4)
    }
    #Plotting
    if(as.numeric(tclvalue(e$plot))>0){
      #graph viewer
      # hscale <- 2    # Horizontal scaling
      #vscale <- 2   # Vertical scaling
      # plot.shap <- function() {
      ###plotting functions
      #}
      #graph <- tkrplot(label_frame, fun = plot.shap,
      #                 hscale = hscale, vscale = vscale)
      #tkgrid(graph, row = 3, column = 1, rowspan = 1, sticky = "w", padx = 2)
      plot(x=variable1value, y=variable2value,xlab=e$variable1name, ylab=e$variable2name)
    }
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "Association Test Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Association Tests", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = "e")
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0)
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable (e$dataname)
  
  data_combo <- ttkcombobox(label_frame, state = "readonly",
                            values = tclvalue(e$dataname),
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Variable 1 Combobox
  put_label(label_frame, "Variable 1:",1,0)
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable1name)
  tkgrid(data_labels1, row = 1, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #Variable 2 Combobox
  put_label(label_frame, "Variable 2:",2,0)
  data_labels2 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable2name)
  tkgrid(data_labels2, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  
  #Conf.level Slider
  put_label(label_frame, "conf.level:", 3, 0)
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 3, column = 1, columnspan = 2, 
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame, 
                               from = 75, to = 100,  
                               variable = e$conf.level)
  #Conf.level Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 75, to = 100, increment = 1, 
                               textvariable = e$conf.level, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
  
  #Hypothesis Radiobuttons
  put_label(label_frame, "Alternative:",4,0)
  rb_frame <- ttkframe(label_frame)
  sapply(c("two.sided","less","greater"), function(i) {
    radio_button <- tk2radiobutton(rb_frame, variable = e$alternative,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 4, column = 1, sticky = "w")
  
  #Pearson's R Checkbox
  put_label ( label_frame , "Pearson's r:" , 5 , 0)
  pearson_check <-ttkcheckbutton (label_frame , variable = e$pearson)
  tkgrid (pearson_check , row = 5 , column = 1 , stick = "w" ,padx = 0)
  
  #Spearman's Rho Checkbox
  put_label ( label_frame , "Spearman's rho:" , 5 , 1)
  spearman_check <-ttkcheckbutton (label_frame , variable = e$spearman)
  tkgrid (spearman_check , row = 5 , column = 2 , stick = "w" ,padx = 0)
  
  #Kendall's Tau Checkbox
  put_label ( label_frame , "Kendall's tau:" , 7 , 0)
  kendall_check <-ttkcheckbutton (label_frame , variable = e$kendall)
  tkgrid (kendall_check , row = 7 , column = 1 , stick = "w" ,padx = 0)
  
  #Covariance Checkbox
  put_label ( label_frame , "Covariance:" , 7 , 1)
  cov_check <-ttkcheckbutton (label_frame , variable = e$cov)
  tkgrid (cov_check , row = 7 , column = 2 , stick = "w" ,padx = 0)
  
  #Plotting Checkbox
  put_label ( label_frame , "Plot:" , 8 , 0)
  plot_check <-ttkcheckbutton (label_frame , variable = e$plot)
  tkgrid (plot_check , row = 8 , column = 1 , stick = "w" ,padx = 0)
  
  # Goodman and Kruskal's Gamma Checkbox
  #put_label ( label_frame , "GK's gamma:" , 7 , 1)
  #gkGamma_check <-ttkcheckbutton (label_frame , variable = e$gkGamma)
  #tkgrid (gkGamma_check , row = 7 , column = 2 , stick = "w" ,padx = 0)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# Fisher_exact_fun
Fisher_exact_fun<-function(){
  
  
  #Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$variable1name<-tclVar("Choose Variable")
  e$variable1value<-tclVar("Choose Variable")
  e$variable2name<-tclVar("Choose Variable")
  e$variable2value<-tclVar("Choose Variable")
  e$plot<-tclVar(FALSE)
  e$alternative<-tclVar("two.sided")
  e$conf.level<-tclVar(95)
  e$simulate<-tclVar(FALSE)
  e$nsims<-tclVar(0)
  
  #OK Function
  OKfun<-function(){
    e$variable1name<-tclvalue(e$variable1name)
    variable1value <- get(e$variable1name,e$dataframe,inherits=TRUE)
    assign("variable1value", variable1value, envir = e)
    e$variable2name<-tclvalue(e$variable2name)
    variable2value <- get(e$variable2name,e$dataframe,inherits=TRUE)
    assign("variable2value", variable2value, envir = e)
    conf.level<-NULL
    conf.level<-as.numeric(tclvalue(e$conf.level))*.01
    assign("conf.level", conf.level, envir = e)
    altselect <- tclvalue(e$alternative)
    alternative <- switch(altselect , "two.sided" = altselect , "less" = altselect , "greater" = altselect )
    assign("alternative", altselect, envir = e)
    nsims<-tclvalue(e$nsims)
    assign("nsims", nsims, envir = e)
    simulate<-as.logical(as.numeric(tclvalue(e$simulate)))
    assign("simulate", simulate, envir = e)
    if(e$nsims>0){
      e$simulate<-TRUE
    }
    tab<-table(x=variable1value, y=variable2value)
    names(attributes(tab)$dimnames)<-c(e$variable1name,e$variable2name)
    out<-fisher.test(tab,alternative = e$alternative, conf.int = TRUE, conf.level = e$conf.level, simulate.p.value=e$simulate, B=e$nsims)
    out$data.name<-paste(e$variable1name, "and", e$variable2name)
    out<-list(Table=tab, Test=out)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    cat("\nTabulated Data\n")
    print(tab)
    cat("\n")
    print(out$Test)
    
    #Plotting
    if(tclvalue(e$plot)>0){
      #graph viewer
      # hscale <- 2    # Horizontal scaling
      #vscale <- 2   # Vertical scaling
      # plot.shap <- function() {
      ###plotting functions
      #}
      #graph <- tkrplot(label_frame, fun = plot.shap,
      #                 hscale = hscale, vscale = vscale)
      #tkgrid(graph, row = 3, column = 1, rowspan = 1, sticky = "w", padx = 2)
      vals<-c(e$variable1value, e$variable2value)
      names<-as.factor(c(rep(e$variable1name,length(e$variable1value)),rep(e$variable2name,length(e$variable2value))))
      boxplot(vals ~ names)
      stripchart(vals ~ names,pch=16, vert=TRUE, add=TRUE,cex=1)
    }
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "Fisher's Exact Test Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Fisher's Exact Test", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = "e")
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0)
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Variable 1 Combobox
  put_label(label_frame, "Variable 1:",1,0)
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable1name)
  tkgrid(data_labels1, row = 1, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #Variable 2 Combobox
  put_label(label_frame, "Variable 2:",2,0)
  data_labels2 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable2name)
  tkgrid(data_labels2, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  
  #Conf.level Slider
  put_label(label_frame, "conf.level:", 3, 0)
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 3, column = 1, columnspan = 2, 
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame, 
                               from = 75, to = 100,  
                               variable = e$conf.level)
  #Conf.Level Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 75, to = 100, increment = 1, 
                               textvariable = e$conf.level, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
  
  #Hypothesis Radiobuttons
  put_label(label_frame, "Alternative:",4,0)
  rb_frame <- ttkframe(label_frame)
  sapply(c("two.sided","less","greater"), function(i) {
    radio_button <- tk2radiobutton(rb_frame, variable = e$alternative,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 4, column = 1, sticky = "w")
  
  #Monte Carlo Slider
  put_label(label_frame, "Monte Carlo:", 5, 0)
  MC_frame <- ttkframe(label_frame)
  tkgrid(MC_frame, row = 5, column = 1, columnspan = 2, 
         sticky = "w")
  MC_scale <- ttkscale(MC_frame, 
                       from = 0, to = 10000,  
                       variable = e$nsims)
  #Monte Carlo Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  MC_spin <- tkspinbox(MC_frame, 
                       from = 0, to = 10000, increment = 1, 
                       textvariable = e$nsims, width = 5)
  tkpack(MC_scale, side = "left")
  tkpack(MC_spin, side = "left")
  
  #Plot Checkbox
  #put_label ( label_frame , "plot:" , 6 , 0)
  #plot_check <-ttkcheckbutton (label_frame , variable = e$plot)
  #tkgrid (plot_check , row = 6 , column = 1 , stick = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# chisq_fun
chisq_fun<-function(){
  
  
  #Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$variable1name<-tclVar("Choose Variable")
  e$variable1value<-tclVar("Choose Variable")
  e$variable2name<-tclVar("Choose Variable")
  e$variable2value<-tclVar("Choose Variable")
  e$plot<-tclVar(FALSE)
  e$correct<-tclVar(FALSE)
  e$simulate<-tclVar(FALSE)
  e$nsims<-tclVar(0)
  
  #OK Function
  OKfun<-function(){
    e$variable1name<-tclvalue(e$variable1name)
    variable1value <- get(e$variable1name,e$dataframe,inherits=TRUE)
    assign("variable1value", variable1value, envir = e)
    e$variable2name<-tclvalue(e$variable2name)
    variable2value <- get(e$variable2name,e$dataframe,inherits=TRUE)
    assign("variable2value", variable2value, envir = e)
    correct<-as.logical(as.numeric(tclvalue(e$correct)))
    assign("correct", correct, envir = e)
    nsims<-as.numeric(tclvalue(e$nsims))
    assign("nsims", nsims, envir = e)
    simulate<-as.logical(as.numeric(tclvalue(e$simulate)))
    assign("simulate", simulate, envir = e)
    if(e$nsims>0){
      e$simulate<-TRUE
    }
    tab<-table(x=variable1value, y=variable2value)
    names(attributes(tab)$dimnames)<-c(e$variable1name,e$variable2name)
    out<-chisq.test(tab, correct=e$correct, simulate.p.value=e$simulate, B=e$nsims,rescale.p = FALSE)
    out$pvals<-out$stdres
    out$pvals[out$stdres<0]<-pnorm(out$stdres[out$stdres<0],lower.tail = TRUE)
    out$pvals[out$stdres>0]<-pnorm(out$stdres[out$stdres>0],lower.tail = FALSE)
    out$data.name<-paste(e$variable1name, "and", e$variable2name)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    cat("\nObserved Data\n")
    print(tab)
    cat("\n")
    print(out)
    
    #Plotting
    if(tclvalue(e$plot)>0){
      #graph viewer
      # hscale <- 2    # Horizontal scaling
      #vscale <- 2   # Vertical scaling
      # plot.shap <- function() {
      ###plotting functions
      #}
      #graph <- tkrplot(label_frame, fun = plot.shap,
      #                 hscale = hscale, vscale = vscale)
      #tkgrid(graph, row = 3, column = 1, rowspan = 1, sticky = "w", padx = 2)
      vals<-c(e$variable1value, e$variable2value)
      names<-as.factor(c(rep(e$variable1name,length(e$variable1value)),rep(e$variable2name,length(e$variable2value))))
      boxplot(vals ~ names)
      stripchart(vals ~ names,pch=16, vert=TRUE, add=TRUE,cex=1)
    }
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "Pearson's Chi-squared Test Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Pearson's Chi-squared Test", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = "e")
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0)
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Variable 1 Combobox
  put_label(label_frame, "Variable 1:",1,0)
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable1name)
  tkgrid(data_labels1, row = 1, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #Variable 2 Combobox
  put_label(label_frame, "Variable 2:",2,0)
  data_labels2 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable2name)
  tkgrid(data_labels2, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  
  #Monte Carlo Slider
  put_label(label_frame, "Monte Carlo:", 3, 0)
  MC_frame <- ttkframe(label_frame)
  tkgrid(MC_frame, row = 3, column = 1, columnspan = 2, 
         sticky = "w")
  MC_scale <- ttkscale(MC_frame, 
                       from = 0, to = 10000,  
                       variable = e$nsims)
  #Monte Carlo Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  MC_spin <- tkspinbox(MC_frame, 
                       from = 0, to = 10000, increment = 1, 
                       textvariable = e$nsims, width = 5)
  tkpack(MC_scale, side = "left")
  tkpack(MC_spin, side = "left")
  
  #Yate's Correction Checkbox
  put_label ( label_frame , "Yate's Correction:" , 4 , 0)
  yates_check <-ttkcheckbutton (label_frame , variable = e$correct)
  tkgrid (yates_check , row = 4 , column = 1 , stick = "w" ,padx = 2)
  
  #Plot Checkbox
  #put_label ( label_frame , "plot:" , 6 , 0)
  #plot_check <-ttkcheckbutton (label_frame , variable = e$plot)
  #tkgrid (plot_check , row = 6 , column = 1 , stick = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# oneway_fun
oneway_fun<-function(){
  
  
  
  #Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$responsevariablename<-tclVar("Choose Response Variable")
  e$responsevariablevalue<-tclVar("Choose Variable")
  e$variable1name<-tclVar("Choose Predictor Variable 1")
  e$variable1value<-tclVar("Choose Variable")
  e$plot<-tclVar(FALSE)
  
  #OK Function
  OKfun<-function(){
    f1<-paste(tclvalue(e$responsevariablename),"~",
              tclvalue(e$variable1name))
    f1<-eval(parse(text=f1))
    out<-lm(f1,data=e$dataframe)
    out$ANOVAtab<-anova(out)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    print(anova(out))
    print(summary(out))
    
    #Plotting function bells and whistles after the analytical stuff is up to "snuff"
    if(tclvalue(e$plot)>0){
      #graph viewer
      # hscale <- 2    # Horizontal scaling
      #vscale <- 2   # Vertical scaling
      # plot.shap <- function() {
      ###plotting functions
      #}
      #graph <- tkrplot(label_frame, fun = plot.shap,
      #                 hscale = hscale, vscale = vscale)
      #tkgrid(graph, row = 3, column = 1, rowspan = 1, sticky = "w", padx = 2)
      ci.bars<-function(ybar,ci,label,ylab) {
        xv<-barplot(ybar,ylim=c(0,max(ybar+ci)),names=label,ylab=ylab,beside=T)
        g<-(max(xv)-min(xv))/50
        for (i in 1:length(xv)){
          lines(c(xv[i],xv[i]),c(ybar[i]+ci[i],ybar[i]-ci[i]))
          lines(c(xv[i]-g,xv[i]+g),c(ybar[i]+ci[i], ybar[i]+ci[i]))
          lines(c(xv[i]-g,xv[i]+g),c(ybar[i]-ci[i], ybar[i]-ci[i]))
        }   
      }
      me<-aggregate(f1,data=e$dataframe,FUN=mean)
      gnames<-as.character(me[,1])
      gmeans<-(me[,2])
      ci<- (aggregate(f1,data=e$dataframe,FUN=sd)[,2]/aggregate(f1,data=e$dataframe,FUN=length)[,2])*1.96
      ci.bars(gmeans,ci,gnames,ylab="response")
    }
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "One-way ANOVA Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "One-way ANOVA", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column,sticky) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky=sticky)
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0,sticky="e")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname,width=25)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable(e$dataname)
  
  dataname <-tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Independent Variable Combobox
  put_label(label_frame, "Response:",row=1,column=1,sticky="n")
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$responsevariablename, width=25)
  tkgrid(data_labels1, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #Variable 1 Combobox
  put_label(label_frame, "Variable 1:",row=1,column=2,sticky="n")
  data_labels2 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable1name,width=25)
  tkgrid(data_labels2, row = 2, column = 2, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  
  #Plot Checkbox # not yet operational (within OK function)
  put_label ( label_frame , "plot:" , 5 , 0,sticky="e")
  plot_check <-ttkcheckbutton (label_frame , variable = e$plot)
  tkgrid (plot_check , row = 5 , column = 1 , stick = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# fact_fun
fact_fun<-function(){
  
  
  
  #Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$responsevariablename<-tclVar("Choose Response Variable")
  e$responsevariablevalue<-tclVar("Choose Variable")
  e$variable1name<-tclVar("Choose Factor 1")
  e$variable1value<-tclVar("Choose Variable")
  e$variable2name<-tclVar("Choose Factor 2")
  e$variable2value<-tclVar("Choose Variable")
  e$interaction<-tclVar("NULL")
  e$variable2value<-tclVar("Choose")
  e$plot<-tclVar(FALSE)
  
  #OK Function
  OKfun<-function(){
    if(tclvalue(e$interaction)!="NULL" & tclvalue(e$variable2name)!="Choose Factor 2"){
      f1<-paste(tclvalue(e$responsevariablename),"~",
                tclvalue(e$variable1name),
                tclvalue(e$interaction),
                tclvalue(e$variable2name))
      f1<-eval(parse(text=f1))
    }
    if(tclvalue(e$interaction)=="NULL"){
      f1<-paste(tclvalue(e$responsevariablename),"~",
                tclvalue(e$variable1name))
      f1<-eval(parse(text=f1))
    }
    out<-lm(f1,data=e$dataframe)
    out$ANOVAtab<-anova(out)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    print(anova(out))
    print(summary(out))
    
    #plotting function bells and whistles after the analytical stuff is up to "snuff"
    #if(tclvalue(e$plot)>0){
    #graph viewer
    # hscale <- 2    # Horizontal scaling
    #vscale <- 2   # Vertical scaling
    # plot.shap <- function() {
    ###plotting functions
    #}
    #graph <- tkrplot(label_frame, fun = plot.shap,
    #                 hscale = hscale, vscale = vscale)
    #tkgrid(graph, row = 3, column = 1, rowspan = 1, sticky = "w", padx = 2)
    # boxplot(yvars ~ factor)
    #stripchart(yvars ~ factor,pch=16, vert=TRUE, add=TRUE,cex=1)
    #}
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "Factorial ANOVA Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Two way ANOVA", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column,sticky) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky=sticky)
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0,sticky="e")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname,width=25)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable(e$dataname)
  
  dataname <-tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Independent Variable Combobox
  put_label(label_frame, "Response:",row=1,column=1,sticky="n")
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$responsevariablename, width=25)
  tkgrid(data_labels1, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #Variable 1 Combobox
  put_label(label_frame, "Variable 1:",row=1,column=2,sticky="n")
  data_labels2 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable1name,width=25)
  tkgrid(data_labels2, row = 2, column = 2, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  
  #Interaction Combobox
  put_label(label_frame, "Interaction:",row=1,column=3,sticky="n")
  data_labels3 <- ttkcombobox(label_frame, state = "readonly", 
                              values = c("NULL"," +"," *"), 
                              textvariable = e$interaction,width=5)
  tkgrid(data_labels3, row = 2, column = 3, sticky="ew", padx = 2)
  tkfocus(data_labels3) 
  
  #Variable 2 Combobox
  put_label(label_frame, "Variable 2:",row=1,column=4,sticky="n")
  data_labels4 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable2name,width=25)
  tkgrid(data_labels4, row = 2, column = 4, sticky="ew", padx = 2)
  tkfocus(data_labels4) 
  
  #Plot Checkbox #not yet operational (within OK function)
  put_label ( label_frame , "plot:" , 5 , 0,sticky="e")
  plot_check <-ttkcheckbutton (label_frame , variable = e$plot)
  tkgrid (plot_check , row = 5 , column = 1 , stick = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# simp_lmfun
simp_lmfun<-function(){
  #Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$responsevariablename<-tclVar("Choose Response Variable")
  e$responsevariablevalue<-tclVar("Choose Variable")
  e$variable1name<-tclVar("Choose Predictor Variable")
  e$variable1value<-tclVar("Choose Variable")
  e$interaction<-tclVar("NULL")
  e$plot<-tclVar(FALSE)
  
  #OK Function
  OKfun<-function(){
    f1<-eval(paste(tclvalue(e$responsevariablename),"~",
                   tclvalue(e$variable1name)))
    out<-lm(f1,data=e$dataframe)
    out$ANOVAtab<-anova(out)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    print(anova(out))
    print(summary(out))
    #plotting function bells and whistles after the analytical stuff is up to "snuff"
    if(as.numeric(tclvalue(e$plot))>0){
      #graph viewer
      # hscale <- 2    # Horizontal scaling
      #vscale <- 2   # Vertical scaling
      # plot.shap <- function() {
      ###plotting functions
      #}
      #graph <- tkrplot(label_frame, fun = plot.shap,
      #                 hscale = hscale, vscale = vscale)
      #tkgrid(graph, row = 3, column = 1, rowspan = 1, sticky = "w", padx = 2)
      e$dataframe$pred<-predict(out,se.fit = TRUE)$fit
      e$dataframe$hici<-e$dataframe$pred + 1.96*predict(out,se.fit = TRUE)$se.fit
      e$dataframe$loci<-e$dataframe$pred - 1.96*predict(out,se.fit = TRUE)$se.fit
      e$dataframe<-e$dataframe[order(e$dataframe$pred),]
      x<-e$dataframe[,which(names(e$dataframe)==tclvalue(e$variable1name))]
      y<-e$dataframe[,which(names(e$dataframe)==tclvalue(e$responsevariablename))]
      xpred<-data.frame(xpred=seq(min(x),max(x),length.out = 1000))
      colnames(xpred)<-tclvalue(e$variable1name)
      predi<-predict(out , se.fit=TRUE,newdata = xpred)
      ypred<-predi$fit
      ypredlo<-ypred - predi$se.fit*1.96
      ypredhi<-ypred + predi$se.fit*1.96
      plot(xpred[,1],ypred,type="n",xlab=tclvalue(e$variable1name),
           ylab=tclvalue(e$responsevariablename),ylim=range(c(ypredlo,ypredhi)))
      polygon(c(xpred[,1],rev(xpred[,1])),c(ypredlo,rev(ypredhi))
              ,col="light gray",border=NA)
      points(x, y,bg=" dark gray",pch=21,cex=1)
      lines(xpred[,1], ypred,col="black",lwd=2)
      #ggplot(e$dataframe, aes(x)) +
      # geom_point(aes(x=x,y=y), colour="black", size=2) +
      #geom_ribbon(aes(ymin=loci, ymax=hici), alpha=0.7,fill="dark gray")+
      #geom_line(aes(y=pred), colour="red",size=1.5) + 
      #ylab(tclvalue(e$responsevariablename))+xlab(tclvalue(e$variable1name))+
      #theme_bw() + 
      #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
      #     panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "Simple Linear Regression Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Simple Linear Regression", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column,sticky) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky=sticky)
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0,sticky="e")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname,width=25)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable(e$dataname)
  
  dataname <-tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Independent Variable Combobox
  put_label(label_frame, "Response:",row=1,column=1,sticky="n")
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$responsevariablename, width=25)
  tkgrid(data_labels1, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #Predictor Variable Combobox
  put_label(label_frame, "Predictor Variable:",row=1,column=2,sticky="n")
  data_labels2 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable1name,width=25)
  tkgrid(data_labels2, row = 2, column = 2, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  
  #Plot Checkbox
  put_label ( label_frame , "plot:" , 5 , 0,sticky="e")
  plot_check <-ttkcheckbutton (label_frame , variable = e$plot)
  tkgrid (plot_check , row = 5 , column = 1 , stick = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# glmfun to be used for simple two-variable regression and two-way anova with interaction effects
glmfun<-function(){
  #Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$responsevariablename<-tclVar("Choose Response Variable")
  e$responsevariablevalue<-tclVar("Choose Variable")
  e$variable1name<-tclVar("Choose Predictor Variable 1")
  e$variable1value<-tclVar("Choose Variable")
  e$variable2name<-tclVar("Choose Predictor Variable 2")
  e$variable2value<-tclVar("Choose Variable")
  e$interaction<-tclVar("NULL")
  e$variable2value<-tclVar("Choose")
  e$distfx<-tclVar("gaussian")
  e$linkfx<-tclVar("identity")
  e$plot<-tclVar(FALSE)
  
  #OK Function
  OKfun<-function(){
    if(tclvalue(e$interaction)!="NULL" & tclvalue(e$variable2name)!="Choose Predictor Variable 2"){
      f1<-paste(tclvalue(e$responsevariablename),"~",
                tclvalue(e$variable1name),
                tclvalue(e$interaction),
                tclvalue(e$variable2name))
      f1<-eval(parse(text=f1))
    }
    if(tclvalue(e$interaction)=="NULL"){
      f1<-paste(tclvalue(e$responsevariablename),"~",
                tclvalue(e$variable1name))
      f1<-eval(parse(text=f1))
    }
    #Link Function Inside Distribution
    lin<-eval(parse(text=paste(tclvalue(e$distfx),"(link=",tclvalue(e$linkfx),")",sep="")))
    out<-glm(f1,data=e$dataframe,family=lin)
    out$anovatab<-anova(out)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    print(anova(out))
    print(summary(out))
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "Generalized Linear Model Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Generalized Linear Model",
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column,sticky) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky=sticky)
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0,sticky="e")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname,width=25)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable(e$dataname)
  
  dataname <-tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Independent Variable Combobox
  put_label(label_frame, "Response:",row=1,column=1,sticky="n")
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$responsevariablename, width=25)
  tkgrid(data_labels1, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #Variable 1 Combobox
  put_label(label_frame, "Variable 1:",row=1,column=2,sticky="n")
  data_labels2 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable1name,width=25)
  tkgrid(data_labels2, row = 2, column = 2, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  put_label(label_frame, "Interaction:",row=1,column=3,sticky="n")
  data_labels3 <- ttkcombobox(label_frame, state = "readonly", 
                              values = c("NULL"," +"," *"), 
                              textvariable = e$interaction,width=5)
  tkgrid(data_labels3, row = 2, column = 3, sticky="ew", padx = 2)
  tkfocus(data_labels3) 
  
  #Variable 2 Combobox
  put_label(label_frame, "Variable 2:",row=1,column=4,sticky="n")
  data_labels4 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable2name,width=25)
  tkgrid(data_labels4, row = 2, column = 4, sticky="ew", padx = 2)
  tkfocus(data_labels4) 
  
  #Distribution Combobox (Family)
  put_label(label_frame, "Distribution:",row=3,column=1,sticky="n")
  data_labels5 <- ttkcombobox(label_frame, state="readonly",
                              values = c("gaussian","binomial","poisson", "Gamma"),
                              textvariable = e$distfx,width=25)
  tkgrid(data_labels5, row = 4, column = 1, sticky = "ew", padx = 2)
  tkfocus(data_labels5)
  
  #Link Combobox (transform function)
  put_label(label_frame, "Link:",row=3,column=2,sticky="n")
  data_labels5 <- ttkcombobox(label_frame, state="readonly",
                              values = c("identity", "logit","log","inverse"),
                              textvariable = e$linkfx,width=25)
  tkgrid(data_labels5, row = 4, column = 2, sticky = "ew", padx = 2)
  tkfocus(data_labels5)
  
  #Plot Checkbox # not yet operational (within OK function)
  put_label ( label_frame , "plot:" , 5 , 0,sticky="e")
  plot_check <-ttkcheckbutton (label_frame , variable = e$plot)
  tkgrid (plot_check , row = 5 , column = 1 , stick = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# saveas_fun
saveas_fun<-function(){
  #Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose dataframe"); 
  e$dataframe <- tclVar("NULL"); 
  e$format<-tclVar("csv")
  e$dir<-tclVar("Working Directory")
  e$dirval<-tclVar(getwd())
  e$filename<-tclVar("Same as Dataframe Name")
  e$savename<-tclVar("")
  
  #OK Function
  OKfun<-function(){
    if (tclvalue(e$dir)=="Working Directory"){
      e$dir<-e$dirval
    } else { e$dir<-tclVar(choose.dir(default = getwd()))}
    if (tclvalue(e$filename)=="Same as Dataframe Name"){
      e$filename<-e$dataname
    }
    e$savename<-paste(tclvalue(e$dir),"/",tclvalue(e$filename),".",sep="")
    switch(tclvalue(e$format),
           csv =  write.csv(x = e$dataframe, file = paste(e$savename,"csv",sep="")),
           txt =  write.table(x = e$dataframe, file = paste(e$savename,"txt",sep=""), sep="\t"),
           #xlsx = xlsx::write.xlsx(x = e$dataframe, file = paste(e$savename,"xlsx",sep="")),
           spss = suppressWarnings(foreign::write.foreign(df = e$dataframe, paste(e$savename,"sps",sep=""), paste(e$savename,"sps",sep=""), package="SPSS")) ,
           stata = suppressWarnings(foreign::write.dta(dataframe = e$dataframe, paste(e$savename,"dta",sep=""))) 
    )
    pos<-1
    envir <- as.environment(pos)
    assign(tclvalue(e$filename), e$dataframe, envir = envir)
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "Save Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Save as:", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column,sticky) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky=sticky)
  }
  #Data Combobox
  put_label(label_frame, "Dataframe Name:",0,0,sticky="e")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname,width=25)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable(e$dataname)
  
  dataname <-tclvalue(e$dataname)
  dataframe <- get(dataname, .GlobalEnv)
  assign("dataframe", dataframe, envir = e)
  
  #Name of File
  put_label(label_frame, "Name of file:",2,0,sticky="e")
  data_labels2 <- ttkentry(label_frame,
                           textvariable = e$filename, width=10)
  tkgrid(data_labels2, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  
  #Directory Radiobuttons
  put_label(label_frame, "Save to:",3,0,sticky="e")
  rb_frame <- ttkframe(label_frame)
  sapply(c("Working Directory","Choose other Directory"), function(i) {
    radio_button <- tk2radiobutton(rb_frame, variable = e$dir,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 3, column = 1, sticky = "w")
  
  #Data Type Radiobuttons
  put_label(label_frame, "Save as:",4,0,sticky="e")
  rb_frame2 <- ttkframe(label_frame)
  sapply(c("csv","txt","spss","stata"), function(i) {
    radio_button <- tk2radiobutton(rb_frame2, variable = e$format,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame2, row = 4, column = 1, sticky = "w")
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# sort_fun
sort_fun<-function(){
#Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$variable1name<-tclVar("Choose Variable")
  e$variable1value<-tclVar("Choose Variable")
  e$variable2name<-tclVar("Choose Variable")
  e$variable2value<-tclVar("Choose Variable")
  e$variable3name<-tclVar("Choose Variable")
  e$variable3value<-tclVar("Choose Variable")
  e$direct1<-tclVar("Increasing");
  e$direct2<-tclVar("Increasing");
  e$direct3<-tclVar("Increasing");
  
  #OK Function
  OKfun<-function(){
    nams<-c(tclvalue(e$variable1name), tclvalue(e$variable2name), tclvalue(e$variable3name))
    cnames<-c(tclvalue(e$variable1name), tclvalue(e$variable2name), tclvalue(e$variable3name)) !="Choose Variable"
    nams<-nams[cnames]
    direct<-c(tclvalue(e$direct1), tclvalue(e$direct2), tclvalue(e$direct3))[cnames]=="Decreasing"
    str<-NULL
    if(length(nams)>1){
      for(i in 1:length(nams)){
        str<-c( str,paste("xtfrm(dataframe","$",nams[i],")",sep="") )
      }
    } else {str<-paste("xtfrm(dataframe","$",nams[1],")",sep="")
    }
    if (sum(direct)>=1){
      str[which(direct==TRUE)]<-paste("-",str[which(direct==TRUE)],sep="")
    }
    str<-paste(str,collapse = ",")
    obj<-paste("dataframe","[order(",str,"),]",sep="")
    #obj
    dataframe<-eval(parse(text=obj))
    pos<-1
    envir <- as.environment(pos)
    assign(tclvalue(e$dataname), dataframe, envir)
    View(as.name(tclvalue(e$dataname)))
    cat("Display Heads ","\n")
    print(head(dataframe))
    cat(" ","\n")
    cat("Display Tails ","\n")
    print(tail(dataframe))
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "Sort Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Sort:", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column,sticky) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky=sticky)
  }
  #Data Combobox
  put_label(label_frame, "Dataframe Name:",0,0,sticky="e")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable(e$dataname)
  
  dataname <-tclvalue(e$dataname)
  dataframe <- get(dataname, .GlobalEnv)
  assign("dataframe", dataframe, envir = e)
  
  #Variable 1 Combobox
  put_label(label_frame, "Variable 1:",row=1,column=0,sticky="n")
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable1name, width=25)
  tkgrid(data_labels1, row = 1, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1)  
  
  #Direction Radiobuttons
  #put_label(label_frame, "Direction:",row=1,column=2,sticky="e")
  rb_frame <- ttkframe(label_frame)
  sapply(c("Increasing","Decreasing"), function(i) {
    radio_button <- tk2radiobutton(rb_frame, variable = e$direct1,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 1, column = 2, sticky = "w")
  
  #Variable 2 Combobox
  put_label(label_frame, "Variable 2:",row=2,column=0,sticky="n")
  data_labels2 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable2name, width=25)
  tkgrid(data_labels2, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels2)  
  
  #Direction Radiobuttons
  #put_label(label_frame, "Direction:",row=1,column=2,sticky="e")
  rb_frame2 <- ttkframe(label_frame)
  sapply(c("Increasing","Decreasing"), function(i) {
    radio_button <- tk2radiobutton(rb_frame2, variable = e$direct2,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame2, row = 2, column = 2, sticky = "w")
  
  #Variable 3 Combobox
  put_label(label_frame, "Variable 3:",row=3,column=0,sticky="n")
  data_labels3 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable3name, width=25)
  tkgrid(data_labels3, row = 3, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels3)  
  
  #Direction Radiobuttons
  #put_label(label_frame, "Direction:",row=1,column=2,sticky="e")
  rb_frame3 <- ttkframe(label_frame)
  sapply(c("Increasing","Decreasing"), function(i) {
    radio_button <- tk2radiobutton(rb_frame3, variable = e$direct3,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame3, row = 3, column = 2, sticky = "w")
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# hist_fun
hist_fun<-function(){
#Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$variable1name<-tclVar("Choose Variable")
  e$variable1value<-tclVar("Choose Variable")
  e$freq<-tclVar("Frequency")
  
  #OK Function
  OKfun<-function(){
    e$variable1name<-tclvalue(e$variable1name)
    variable1value <- get(e$variable1name,e$dataframe,inherits=TRUE)
    assign("variable1value", variable1value, envir = e)
    altselect <- tclvalue(e$freq)
    freq <- switch(altselect , "Frequency" = altselect , "Density" = altselect)
    assign("freq", altselect, envir = e)
    freq<-freq=="Frequency"
    out<-hist(variable1value, freq = freq, xlab=e$variable1name, main=paste("Histogram of ",e$variable1name))
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    cat("Breaks: ",out$breaks, "\n")
    cat("Counts: ",out$counts, "\n")
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "Histogram Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Histogram", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column,sticky) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky=sticky)
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0,sticky="e")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname,width=25)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable(e$dataname)
  
  dataname <-tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Variable Combobox
  put_label(label_frame, "Variable:",row=1,column=1,sticky="n")
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable1name, width=25)
  tkgrid(data_labels1, row = 1, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #Y Axis Radiobutton
  put_label(label_frame, "Y-Axis:",2,0,sticky="w")
  rb_frame <- ttkframe(label_frame)
  sapply(c("Frequency","Density"), function(i) {
    radio_button <- tk2radiobutton(rb_frame, variable = e$freq,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 2, column = 1, sticky = "w")
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# simp_box_fun
simp_box_fun<-function(){
#Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$variable1name<-tclVar("Choose Variable")
  e$variable1value<-tclVar("Choose Variable")
  e$plot<-tclVar(TRUE)
  
  #OK Function
  OKfun<-function(){
    e$variable1name<-tclvalue(e$variable1name)
    variable1value <- get(e$variable1name,e$dataframe,inherits=TRUE)
    assign("variable1value", variable1value, envir = e)
    out<-boxplot(variable1value,  main=paste("Boxplot of ",e$variable1name))
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    cat("Quantiles: ",quantile(variable1value, na.rm = TRUE), "\n")
    cat("IQR: ",IQR(variable1value, na.rm = TRUE), "\n")
    if(tclvalue(e$plot)>0){
      stripchart(variable1value,pch=16, vert=TRUE, add=TRUE,cex=1)
    }
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "Boxplot Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Boxplot", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column,sticky) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky=sticky)
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0,sticky="e")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname,width=25)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable(e$dataname)
  
  dataname <-tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Variable Combobox
  put_label(label_frame, "Variable:",row=1,column=1,sticky="n")
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable1name, width=25)
  tkgrid(data_labels1, row = 1, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #Plot Data Checkbox
  put_label ( label_frame , "plot data:" , 2 , 0,sticky = "w")
  plot_check <-ttkcheckbutton (label_frame , variable = e$plot)
  tkgrid (plot_check , row = 2 , column = 1 , sticky = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# bivplot_fun
bivplot_fun<-function(){
#Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$variable1name<-tclVar("Choose Variable")
  e$variable1value<-tclVar("Choose Variable")
  e$variable2name<-tclVar("Choose Variable")
  e$variable2value<-tclVar("Choose Variable")
  e$asp<-tclVar(0)
  
  #OK Function
  OKfun<-function(){
    e$variable1name<-tclvalue(e$variable1name)
    variable1value <- get(e$variable1name,e$dataframe,inherits=TRUE)
    assign("variable1value", variable1value, envir = e)
    e$variable2name<-tclvalue(e$variable2name)
    variable2value <- get(e$variable2name,e$dataframe,inherits=TRUE)
    assign("variable2value", variable2value, envir = e)
    if(as.numeric(tclvalue(e$asp))>0){
      plot(variable1value,variable2value
           ,xlab=e$variable1name,ylab=e$variable2name
           ,asp=1
           , pch=21,bg="gray")
    } else {
      plot(variable1value,variable2value
           ,xlab=e$variable1name,ylab=e$variable2name
           ,pch=21,bg="gray")
    }
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "Bivariate plot Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Bivariate plot", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column,sticky) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky=sticky)
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0,sticky="e")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname,width=25)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable(e$dataname)
  
  dataname <-tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #X Variable Combobox
  put_label(label_frame, "X-variable:",row=1,column=0,sticky="e")
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable1name, width=25)
  tkgrid(data_labels1, row = 1, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #Y Variable Combobox
  put_label(label_frame, "Y-variable:", row=2, column=0,sticky="e")
  data_labels2 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable2name)
  tkgrid(data_labels2, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  
  #ASP Ratio
  put_label ( label_frame , "equal aspect ratio:" , 3 , 0,sticky = "e")
  plot_check <-ttkcheckbutton (label_frame , variable = e$asp)
  tkgrid (plot_check , row = 3 , column = 1 , sticky = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# divind_fun
divind_fun<-function(){
#Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$variable1name<-tclVar("Choose Variable")
  e$variable1value<-tclVar("Choose Variable")
  e$index<-tclVar("shannon")
  e$indval<-tclVar(0)
  e$append<-tclVar(FALSE)
  
  #OK Function
  OKfun<-function(){
    divindex<-function (A=dataframe, index=tclvalue(e$index)) {
      INDICES <- c("shannon", "simpson", "isimpson", "margalef", "menhinick")
      index <- match.arg(index, INDICES)
      if (length(dim(A)) > 1) {
        M<-A/rowSums(A)
      }
      d<-switch(index,
                shannon = -rowSums(M*log(M),na.rm=T),
                simpson = 1 - rowSums(M*M, na.rm = TRUE),
                isimpson = 1/rowSums(M*M, na.rm = TRUE),
                margalef = (rowSums(A>0)-1)/log(rowSums(A, na.rm = TRUE)),
                menhinick = rowSums(A>0)/sqrt(rowSums(A, na.rm = TRUE)))
      k<-switch(index,
                shannon = assign("indval", d, envir = e),
                simpson = assign("indval", d, envir = e),
                isimpson = assign("indval", d, envir = e),
                margalef = assign("indval", d, envir = e),
                menhinick = assign("indval", d, envir = e) )
    }
    out<-divindex()
    cat(tclvalue(e$index),out, "\n")
    if(tclvalue(e$append)>0){
      eval(parse(text=paste("dataframe$",tclvalue(e$index),"<-out",sep="")))
      pos<-1
      envir <- as.environment(pos)
      assign(tclvalue(e$dataname), dataframe, envir=envir)
    } else {pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    }
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "Diversity Index Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Diversity Index", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column,sticky) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky=sticky)
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0,sticky="e")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname,width=25)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable(e$dataname)
  
  dataname <-tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Variable Combobox
  #put_label(label_frame, "Variable:",row=1,column=1,sticky="n")
  #data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
  #                            values = sort(colnames(e$dataframe)), 
  #                            textvariable = e$variable1name, width=25)
  #tkgrid(data_labels1, row = 1, column = 1, sticky="ew", padx = 2)
  #tkfocus(data_labels1) 
  
  #Div Index Radiobutton
  put_label(label_frame, "Index:",1,0,sticky="e")  
  rb_frame <- ttkframe(label_frame)
  sapply(c("shannon", "simpson", "isimpson", "margalef", "menhinick"), function(i) {
    radio_button <- tk2radiobutton(rb_frame, variable = e$index,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 1, column = 1, sticky = "w")
  
  #Append Checkbox
  put_label ( label_frame , "Append:" , 2 , 0,sticky="e")
  plot_check <-ttkcheckbutton (label_frame , variable = e$append)
  tkgrid (plot_check , row = 2 , column = 1 , stick = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# transf_fun
trans_fun<-function(){
  
  
  
  #Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$variable1name<-tclVar("Choose Variable")
  e$variable1value<-tclVar("Choose Variable")
  e$index<-tclVar("log")
  e$indval<-tclVar(0)
  e$append<-tclVar(FALSE)
  
  #OK Function
  OKfun<-function(){
    e$variable1name<-tclvalue(e$variable1name)
    variable1value <- get(e$variable1name,e$dataframe,inherits=TRUE)
    assign("variable1value", variable1value, envir = e)
    transfun <- function(x=variable1value, index = tclvalue(e$index)){
      trans <- switch(index,
                      log = log(x),
                      log10 = log(x, base = 10),
                      sqrt = sqrt(x),
                      arcsin = asin(x),
                      prop = asin(sqrt(x)),
                      Zscore= scale(x, scale=TRUE, center=TRUE))
    }
    out<-transfun()
    #cat(tclvalue(e$index),out, "\n")
    if(tclvalue(e$append)>0){
      eval(parse(text=paste("dataframe$",tclvalue(e$index),"_",e$variable1name,"<-out",sep="")))
      pos<-1
      envir <- as.environment(pos)
      assign(tclvalue(e$dataname), dataframe, envir=envir)
    } else {pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) }
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "Tranformation Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Tranformation", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column,sticky) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky=sticky)
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0,sticky="e")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname,width=25)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable(e$dataname)
  
  dataname <-tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Variable Combobox
  put_label(label_frame, "Variable:",row=1,column=0,sticky="n")
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable1name, width=25)
  tkgrid(data_labels1, row = 1, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #Div Index Radiobutton
  put_label(label_frame, "Transformation:",2,0,sticky="e")  
  rb_frame <- ttkframe(label_frame)
  sapply(c("log","log10","sqrt","arcsin","prop", "Zscore"), function(i) {
    radio_button <- tk2radiobutton(rb_frame, variable = e$index,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 2, column = 1, sticky = "w")
  
  #Append Checkbox
  put_label ( label_frame , "Append:" , row=3 , column=0,sticky="e")
  plot_check <-ttkcheckbutton (label_frame , variable = e$append)
  tkgrid (plot_check , row = 3 , column = 1 , stick = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# multi_histfun
multi_histfun<-function(){
  #Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$responsevariablename<-tclVar("Choose Variable")
  e$responsevariablevalue<-tclVar("Choose Variable")
  e$variable1name<-tclVar("Choose Factor")
  e$variable1value<-tclVar("Choose Variable")
  e$freq<-tclVar("Frequency")
  
  #Histogram Function
  hist.formula <- function(formula, data, cols=NULL, rows=NULL,freq=TRUE, ...){
    rows<-rows
    cols<-cols
    DF <- model.frame(formula, data = data, ...)
    DF[[2]]<-as.factor(DF[[2]])
    DF.split <- split(DF[[1]], DF[[2]])
    if(is.null(cols)|is.null(rows)){
      rows<-ceiling(length(levels(DF[[2]]))/2)
      cols<-2 #length(levels(DF[[2]]))
    }
    par(mfrow = c(rows,cols))
    for (i in names(DF.split)){
      Col <- DF.split[[i]]
      hist(Col, main = i, xlab=names(DF)[1],freq=freq)
      #lines(density(Col), col = "red")
      #rug(Col)
    }
    par(mfrow = c(1, 1))
  }
  #OK Function
  OKfun<-function(){
    f1<-eval(paste(tclvalue(e$responsevariablename),"~",
                   tclvalue(e$variable1name)))
    altselect <- tclvalue(e$freq)
    freq <- switch(altselect , "Frequency" = altselect , "Density" = altselect)
    assign("freq", altselect, envir = e)
    freq<-freq=="Frequency"
    hist.formula(f1,data=dataframe, cols=NULL, rows=NULL,freq=freq)
    #assign("Results", out, envir = .GlobalEnv) 
    #print(anova(out))
    #print(summary(out))
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "Multiway Histogram Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Multiway Histogram", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column,sticky) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky=sticky)
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0,sticky="e")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname,width=25)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable(e$dataname)
  
  dataname <-tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Plot Variable Combobox
  put_label(label_frame, "Plot Variable:",row=1,column=1,sticky="n")
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$responsevariablename, width=25)
  tkgrid(data_labels1, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #Factor Variable Combobox
  put_label(label_frame, "by Factor:",row=1,column=2,sticky="n")
  data_labels2 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable1name,width=25)
  tkgrid(data_labels2, row = 2, column = 2, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  
  #Y Axis Radiobutton
  put_label(label_frame, "Plot Y-Axis as:",3,0,sticky="w")
  rb_frame <- ttkframe(label_frame)
  sapply(c("Frequency","Density"), function(i) {
    radio_button <- tk2radiobutton(rb_frame, variable = e$freq,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 3, column = 1, sticky = "w")
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# multi_boxplotfun
multi_boxplotfun<-function(){
  #Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$responsevariablename<-tclVar("Choose Variable")
  e$responsevariablevalue<-tclVar("Choose Variable")
  e$variable1name<-tclVar("Choose Factor")
  e$variable1value<-tclVar("Choose Variable")
  e$hor<-tclVar("0")
  e$varwid<-tclVar("0")
  e$notch<-tclVar("0")
  e$plot<-tclVar("0")
  
  #OK Function
  OKfun<-function(){
    f1<-eval(paste(tclvalue(e$responsevariablename),"~",
                   tclvalue(e$variable1name)))
    out<-boxplot(as.formula(f1),
                 data=dataframe, 
                 horizontal = as.logical(as.numeric(tclvalue(e$hor)))
                 ,varwidth=as.logical(as.numeric(tclvalue(e$varwid)))
                 ,outline=TRUE, notch = as.logical(as.numeric(tclvalue(e$notch))))
    if(as.numeric(tclvalue(e$plot))>0){
      stripchart(as.formula(f1)
                 ,data=dataframe
                 ,vertical=as.logical(1-as.logical(as.numeric(tclvalue(e$hor))))
                 ,pch=16, add=TRUE,cex=1)
    }
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    print("Boxplot statstistics in 'Results' object")
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "Multiway Boxplot Dialog")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Multiway Boxplot", 
                               padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", 
         padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column,sticky) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky=sticky)
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0,sticky="e")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname,width=25)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable(e$dataname)
  
  dataname <-tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Variable 1 Combobox
  put_label(label_frame, "Plot Variable:",row=1,column=1,sticky="n")
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$responsevariablename, width=25)
  tkgrid(data_labels1, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #Variable 2 Combobox
  put_label(label_frame, "by Factor:",row=1,column=2,sticky="n")
  data_labels2 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable1name,width=25)
  tkgrid(data_labels2, row = 2, column = 2, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  
  #Plot Data Checkbox
  put_label ( label_frame , "plot data:" , 3 , 0,sticky = "w")
  plot_check1 <-ttkcheckbutton (label_frame , variable = e$plot)
  tkgrid (plot_check1 , row = 3 , column = 1 , sticky = "w" ,padx = 2)
  
  #Plot Notch Checkbox
  put_label ( label_frame , "Notch:" , 3 , 1,sticky = "n")
  plot_check2 <-ttkcheckbutton (label_frame , variable = e$notch)
  tkgrid (plot_check2 , row = 3 , column = 1 , sticky = "e" ,padx = 2)
  
  #Plot N Checkbox
  put_label ( label_frame , "N width:" , 3 , 2,sticky = "w")
  plot_check3 <-ttkcheckbutton (label_frame , variable = e$varwid)
  tkgrid (plot_check3 , row = 3 , column = 2 , sticky = "n" ,padx = 2)
  
  #Plot Horizontal Checkbox
  put_label ( label_frame , "Horizontal:" , 3 , 2,sticky = "e")
  plot_check4 <-ttkcheckbutton (label_frame , variable = e$hor)
  tkgrid (plot_check4 , row = 3 , column = 3 , sticky = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# parMANOVA Function
parMANOVA_fun<-function(){ #EOC + JBR
  #Data Model: enviroment called "e"
  e<-new.env()
  e$dataname <- tclVar("Choose Dataframe");
  e$dataframe <- tclVar("NULL"); 
  e$permutations<-tclVar(99)
  e$type<-tclVar("Wilks")
  e$function_type<-tclVar("Additive")
  
  #Ok Function
  Okfun<-function(){
    #Using Selected Values
    Value_Y <- input1[as.numeric(tkcurselection(list1)) + 1]
    Value_X <- input1[as.numeric(tkcurselection(list2)) + 1]
    
    #Error Checking
    if(sum(sapply(e$dataframe[,Value_Y], is.numeric))==FALSE) { 
      tkdestroy(window)
      stop("Response matrix must not contain characters!")
    }
    if(sum(sapply(e$dataframe, is.na))==TRUE) {
      tkdestroy(window)
      stop("The data set is missing values!")
    }
    if(length(Value_X)<2){
      f1<-eval(paste("as.matrix(e$dataframe[,Value_Y]) ~", as.name(Value_X)))
    }
    if(length(Value_X)>2) {
      tkdestroy(window)
      stop("There can be no more than two X variables selected!")
    }
    if(length(Value_X)==2) {
      Xvals<-e$dataframe[,Value_X]
      assign(colnames(Xvals)[1],Xvals[,1])
      assign(colnames(Xvals)[2],Xvals[,2])
      if(tclvalue(e$function_type)=="Additive") {
        f1<-eval(paste("as.matrix(e$dataframe[,Value_Y]) ~",
                       as.name(colnames(Xvals)[1]),
                       "+"
                       ,as.name(colnames(Xvals)[2]),sep=""))
      }
      else {
        f1<-eval(paste("as.matrix(e$dataframe[,Value_Y]) ~",
                       as.name(colnames(Xvals)[1]),
                       "*"
                       ,as.name(colnames(Xvals)[2]),sep=""))
      }
    }
    out<-summary(manova(formula = as.formula(f1), data = e$dataframe),test=tclvalue(e$type))
    print("MANOVA")
    print(out)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    tkdestroy(window)
  }
  #Begin GUI Setup
  window<-tktoplevel()
  tkwm.title(window, "MANOVA")
  frame<-ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame<-ttklabelframe(frame, text = "MANOVA", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label<-function(parent, text, row, column, sticky, width) {
    label<-ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = sticky)
  }
  #Data Combobox
  put_label(label_frame, "Dataframe:",0,0,sticky="w")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo)                      
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Variable Y Listbox
  list1 <- tk2listbox(label_frame, height = 4, selectmode = "extended")
  put_label(label_frame, "Response Matrix:",row=4,column=0,sticky="n")
  tkgrid(list1, padx = 10, pady = c(5, 10),row = 5, column = 0)
  input1 <- sort(colnames(e$dataframe))
  for (inputs in input1){
    tkinsert(list1, "end", inputs)
  }
  tkselection.set(list1, 0)
  
  #Variable X Listbox
  list2<-tk2listbox(label_frame, height = 4, selectmode = "extended")
  put_label(label_frame, "Predictor Matrix:", row = 4, column = 1, sticky = "n")
  tkgrid(list2, padx = 10, pady = c(5, 10), row = 5, column = 1)
  input2<-sort(colnames(e$dataframe))
  for(inputs in input2){
    tkinsert(list2, "end", inputs)
  }
  tkselection.set(list2, 0)
  
  #X-Matrix Interactions Radiobuttons
  put_label(label_frame, "Predictor Interaction:", 6,0, sticky="w")
  rb_frame<-ttkframe(label_frame)
  sapply(c("Additive", "Multiplicative"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$function_type, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 6, column = 1, sticky = "w")
  
  #Test Method Selection
  put_label(label_frame, "Test:", 7,0, sticky="w")
  rb_frame<-ttkframe(label_frame)
  sapply(c("Wilks", "Pillai", "Hotelling-Lawley", "Roy"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 7, column = 1, sticky = "w")
  
  #Permutation Slider
  #put_label(label_frame, "Permutations:", 8, 0, sticky = "w")
  #conf_level_frame <- ttkframe(label_frame)
  #tkgrid(conf_level_frame, row = 8, column = 1, columnspan = 2, 
  #       sticky = "w")
  #conf_level_scale <- ttkscale(conf_level_frame, 
  #                             from = 99, to = 10000,
  #                             variable = e$permutations)
  #Permutation Spinbox
  #tkspinbox <- function(parent, ...)
  #  tkwidget(parent, "tk::spinbox", ...)
  #conf_level_spin <- tkspinbox(conf_level_frame, 
  #                             from = 99, to = 10000, increment = 1,
  #                             textvariable = e$permutations, width = 5)
  #tkpack(conf_level_scale, side = "left")
  #tkpack(conf_level_spin, side = "left")
  
  #Draw Cancel Button
  button_frame<-ttkframe(frame)
  cancel_button<-ttkbutton(button_frame, text = "Cancel")
  
  #Draw Ok Button
  ok_button<-ttkbutton(button_frame, text = "Ok", command = Okfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = Okfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# NPMANOVA Function 
NPMANOVA_fun<-function(){ #EOC + JBR
  #Data Model: enviroment called "e"
  e<-new.env()
  e$dataname <- tclVar("Choose Dataframe");
  e$dataframe <- tclVar("NULL"); 
  e$permutations<-tclVar(99)
  e$type<-tclVar("Euclidean")
  e$function_type<-tclVar("Additive")
  
  #Ok Function
  Okfun<-function(){
    
    #Using Selected Values
    Value_Y <- input1[as.numeric(tkcurselection(list1)) + 1]
    Value_X <- input1[as.numeric(tkcurselection(list2)) + 1]
    
    #Error Checking
    if(sum(sapply(e$dataframe[,Value_Y], is.numeric))==FALSE) {
      tkdestroy(window)
      stop("Response matrix must not contain characters!")
    }
    if(sum(sapply(e$dataframe, is.na))==TRUE) {
      tkdestroy(window)
      stop("The data set is missing values!")
    }
    if(length(Value_X)<2){
      f1<-eval(paste("as.matrix(e$dataframe[,Value_Y]) ~", as.name(Value_X)))
    }
    if(length(Value_X)>2) {
      tkdestroy(window)
      stop("There can be no more than two X variables selected!")
    }
    if(length(Value_X)==2) {
      Xvals<-e$dataframe[,Value_X] # no need
      x1<-Xvals[,1]# no need
      x2<-Xvals[,2]# no need
      if(tclvalue(e$function_type)=="Additive") {
        f1<-eval(paste("e$dataframe[,Value_Y] ~", as.name(Value_X[1]), "+", as.name(Value_X[2])))
      }
      else {
        f1<-eval(paste("e$dataframe[,Value_Y] ~", as.name(Value_X[1]), "*", as.name(Value_X[2])))
      }
    }
    switch(tclvalue(e$type),
           Canberra = {out<-adonis(formula = as.formula(f1), data = e$dataframe, permutations = as.numeric(tclvalue(e$permutations)), method = "canberra")
           print("Canberra")
           },
           Euclidean = {out<-adonis(formula = as.formula(f1), data = e$dataframe, permutations = as.numeric(tclvalue(e$permutations)), method = "euclidean")
           print("Euclidean")
           },
           Jaccard = {out<-adonis(formula = as.formula(f1), data = e$dataframe, permutations = as.numeric(tclvalue(e$permutations)), method = "jaccard")
           print("Jaccard")
           },
           Manhattan = {out<-adonis(formula = as.formula(f1), data = e$dataframe, permutations = as.numeric(tclvalue(e$permutations)), method = "manhattan")
           print("Manhattan")}
    )
    print(out)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    tkdestroy(window)
  }
  #Begin GUI Setup
  window<-tktoplevel()
  tkwm.title(window, "NP-MANOVA")
  frame<-ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame<-ttklabelframe(frame, text = "NP-MANOVA", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label<-function(parent, text, row, column, sticky, width) {
    label<-ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = sticky)
  }
  #Data Combobox
  put_label(label_frame, "Dataframe:",0,0,sticky="w")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo)                      
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Variable Y Listbox
  list1 <- tk2listbox(label_frame, height = 4, selectmode = "extended")
  put_label(label_frame, "Response Matrix:",row=4,column=0,sticky="n")
  tkgrid(list1, padx = 10, pady = c(5, 10),row = 5, column = 0)
  input1 <- sort(colnames(e$dataframe))
  for (inputs in input1){
    tkinsert(list1, "end", inputs)
  }
  tkselection.set(list1, 0)
  
  #Variable X Listbox
  list2<-tk2listbox(label_frame, height = 4, selectmode = "extended")
  put_label(label_frame, "Predictor Matrix:", row = 4, column = 1, sticky = "n")
  tkgrid(list2, padx = 10, pady = c(5, 10), row = 5, column = 1)
  input2<-sort(colnames(e$dataframe))
  for(inputs in input2){
    tkinsert(list2, "end", inputs)
  }
  tkselection.set(list2, 0)
  
  #X-Matrix Interactions Radiobuttons
  put_label(label_frame, "Regression Behavior:", 6,0, sticky="w")
  rb_frame<-ttkframe(label_frame)
  sapply(c("Additive", "Multiplicative"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$function_type, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 6, column = 1, sticky = "w")
  
  #Method Selection Radiobuttons
  put_label(label_frame, "Method:", 7,0, sticky="w")
  rb_frame<-ttkframe(label_frame)
  sapply(c("Canberra", "Euclidean", "Jaccard", "Manhattan"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 7, column = 1, sticky = "w")
  
  #Permutation Slider
  put_label(label_frame, "Permutations:", 8, 0, sticky = "w")
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 8, column = 1, columnspan = 2, 
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame, 
                               from = 99, to = 10000,
                               variable = e$permutations)
  #Permutation Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 99, to = 10000, increment = 1,
                               textvariable = e$permutations, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
  
  #Draw Cancel Button
  button_frame<-ttkframe(frame)
  cancel_button<-ttkbutton(button_frame, text = "Cancel")
  
  #Draw Ok Button
  ok_button<-ttkbutton(button_frame, text = "Ok", command = Okfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = Okfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# NP Hotelling Function 
NPHotellingt_fun<-function(){ #EOC + JBR
  
  
  
  
  #Data Model: enviroment called "e"
  e<-new.env()
  e$dataname <- tclVar("Choose Dataframe");
  e$dataframe <- tclVar("NULL"); 
  e$permutations<-tclVar(99)
  e$type<-tclVar("Euclidean")
  e$function_type<-tclVar("Additive")
  
  #Ok Function
  Okfun<-function(){
    
    #Using Selected Values
    Value_Y <- input1[as.numeric(tkcurselection(list1)) + 1]
    Value_X <- input1[as.numeric(tkcurselection(list2)) + 1]
    
    #Error Checking
    if(sum(sapply(e$dataframe[,Value_Y], is.numeric))==FALSE) {
      tkdestroy(window)
      stop("Response matrix must not contain characters!")
    }
    if(sum(sapply(e$dataframe, is.na))==TRUE) {
      tkdestroy(window)
      stop("The data set is missing values!")
    }
    if(length(Value_X)<2){
      f1<-eval(paste("as.matrix(e$dataframe[,Value_Y]) ~", as.name(Value_X)))
    }
    if(length(Value_X)>2) {
      tkdestroy(window)
      stop("There can be no more than two X variables selected!")
    }
    if(length(Value_X)==2) {
      Xvals<-e$dataframe[,Value_X] # no need
      x1<-Xvals[,1]# no need
      x2<-Xvals[,2]# no need
      if(tclvalue(e$function_type)=="Additive") {
        f1<-eval(paste("e$dataframe[,Value_Y] ~", as.name(Value_X[1]), "+", as.name(Value_X[2])))
      }
      else {
        f1<-eval(paste("e$dataframe[,Value_Y] ~", as.name(Value_X[1]), "*", as.name(Value_X[2])))
      }
    }
    switch(tclvalue(e$type),
           Canberra = {out<-adonis(formula = as.formula(f1), data = e$dataframe, permutations = as.numeric(tclvalue(e$permutations)), method = "canberra")
           print("Canberra")
           },
           Euclidean = {out<-adonis(formula = as.formula(f1), data = e$dataframe, permutations = as.numeric(tclvalue(e$permutations)), method = "euclidean")
           print("Euclidean")
           },
           Jaccard = {out<-adonis(formula = as.formula(f1), data = e$dataframe, permutations = as.numeric(tclvalue(e$permutations)), method = "jaccard")
           print("Jaccard")
           },
           Manhattan = {out<-adonis(formula = as.formula(f1), data = e$dataframe, permutations = as.numeric(tclvalue(e$permutations)), method = "manhattan")
           print("Manhattan")}
    )
    print(out)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    tkdestroy(window)
  }
  #Begin GUI Setup
  window<-tktoplevel()
  tkwm.title(window, "zooaRch")
  frame<-ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame<-ttklabelframe(frame, text = "NP-Hotelling T", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label<-function(parent, text, row, column, sticky, width) {
    label<-ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = sticky)
  }
  #Data Combobox
  put_label(label_frame, "Dataframe:",0,0,sticky="w")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo)                      
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Variable Y Listbox
  list1 <- tk2listbox(label_frame, height = 4, selectmode = "extended")
  put_label(label_frame, "Response Matrix:",row=4,column=0,sticky="n")
  tkgrid(list1, padx = 10, pady = c(5, 10),row = 5, column = 0)
  input1 <- sort(colnames(e$dataframe))
  for (inputs in input1){
    tkinsert(list1, "end", inputs)
  }
  tkselection.set(list1, 0)
  
  #Variable X Listbox
  list2<-tk2listbox(label_frame, height = 4, selectmode = "extended")
  put_label(label_frame, "Predictor Matrix:", row = 4, column = 1, sticky = "n")
  tkgrid(list2, padx = 10, pady = c(5, 10), row = 5, column = 1)
  input2<-sort(colnames(e$dataframe))
  for(inputs in input2){
    tkinsert(list2, "end", inputs)
  }
  tkselection.set(list2, 0)
  
  #X-Matrix Interactions Radiobuttons
  put_label(label_frame, "Regression Behavior:", 6,0, sticky="w")
  rb_frame<-ttkframe(label_frame)
  sapply(c("Additive", "Multiplicative"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$function_type, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 6, column = 1, sticky = "w")
  
  #Method Selection Radiobuttons
  put_label(label_frame, "Method:", 7,0, sticky="w")
  rb_frame<-ttkframe(label_frame)
  sapply(c("Canberra", "Euclidean", "Jaccard", "Manhattan"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 7, column = 1, sticky = "w")
  
  #Permutation Slider
  put_label(label_frame, "Permutations:", 8, 0, sticky = "w")
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 8, column = 1, columnspan = 2, 
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame, 
                               from = 99, to = 10000,
                               variable = e$permutations)
  #Permutation Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 99, to = 10000, increment = 1,
                               textvariable = e$permutations, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
  
  #Draw Cancel Button
  button_frame<-ttkframe(frame)
  cancel_button<-ttkbutton(button_frame, text = "Cancel")
  
  #Draw Ok Button
  ok_button<-ttkbutton(button_frame, text = "Ok", command = Okfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = Okfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# Distance Function
dist_fun<-function(){ #EOC + JBR
  
  
  
  
  #Data Model: enviroment called "e"
  e<-new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$type<-tclVar("Euclidean")
  
  #Ok Function
  Okfun<-function(){
    
    #Error Checking
    if(sum(sapply(e$dataframe, is.numeric))==FALSE) {
      tkdestroy(window)
      stop("The data set must not contain characters!")
    }
    if(sum(sapply(e$dataframe, is.na))==TRUE) {
      tkdestroy(window)
      stop("The data set contains missing values!")
    }
    #Calculating the Distance
    Value1 <- input1[as.numeric(tkcurselection(list1)) + 1]
    switch(tclvalue(e$type),
           Canberra = {out<-as.matrix(dist(e$dataframe[,Value1],method="canberra"))
           print("Canberra")
           },
           Euclidean = {out<-as.matrix(dist(e$dataframe[,Value1],method="euclidean"))
           print("Euclidean")
           },
           Jaccard = {out<-as.matrix(vegdist(e$dataframe[,Value1],method="jaccarrd"))
           print("Jaccard")
           },
           Manhattan = {out<-as.matrix(dist(e$dataframe[,Value1],method="manhattan"))
           print("Manhattan")}
    )
    dimnames(out)<-list(row.names(e$dataframe),row.names(e$dataframe))
    out<-as.dist(out)
    pos<-1
    envir <- as.environment(pos)
    D_Matrix_Results<-NULL
    assign("D_Matrix_Results", out, envir = envir)
    tkdestroy(window)
  }
  #Begin GUI Setup
  window<-tktoplevel()
  tkwm.title(window, "Distance Matrix")
  frame<-ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame<-ttklabelframe(frame, text = "Distance Matrix", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label<-function(parent, text, row, column, sticky) {
    label<-ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = sticky)
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0,sticky="w")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Distance Type Selection Radiobuttons
  put_label(label_frame, "Distance Type", 2,0, sticky="w")
  rb_frame<-ttkframe(label_frame)
  sapply(c("Canberra", "Euclidean", "Jaccard", "Manhattan"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 2, column = 1, sticky = "w")
  
  #Data Choice Listbox
  list1 <- tk2listbox(label_frame, height = 5, selectmode = "extended")
  put_label(label_frame, "Choose Data:",row=3,column=0,sticky="w")
  tkgrid(list1, padx = 10, pady = c(5, 10),row = 3, column = 1)
  input1 <- sort(colnames(e$dataframe))
  for (inputs in input1){
    tkinsert(list1, "end", inputs)
  }
  tkselection.set(list1, 0)
  
  #Draw Cancel Button
  button_frame<-ttkframe(frame)
  cancel_button<-ttkbutton(button_frame, text = "Cancel")
  
  #Draw Ok Button
  ok_button<-ttkbutton(button_frame, text = "Ok", command = Okfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = Okfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# General Hierarchical Clustering
cluster_fun<-function(){
  dist.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(dist) if(class(get(dist))[1] == "dist")c(unlist(dist)))),"Load User File")
  D_Matrix_Results<-NULL
  
  #Data Model: enviroment called e
  e<-new.env()
  e$dataname<-tclVar("Choose Data")
  e$distancename<-tclVar("Choose Data")
  e$dataframe<-tclVar("NULL")
  e$clustertype<-tclVar("Average")
  e$distance<-tclVar("NULL")
  
  #Ok Function
  Okfun<-function() {
    if(class(e$distance)!="dist"){
      stop("Input must be a distance matrix. See ?as.dist",tkdestroy(window))
    }
    switch(tclvalue(e$clustertype),
           Single = {
             out<-hclust(e$distance,method="single")
             plot(as.dendrogram(out), main = tclvalue(e$clustertype), horiz=TRUE,lwd=4)
           },
           Complete = {
             out<-hclust(e$distance,method="complete")
             plot(as.dendrogram(out), main = tclvalue(e$clustertype), horiz=TRUE,lwd=4)
           },
           Average = {
             out<-hclust(e$distance,method="average")
             plot(as.dendrogram(out), main = tclvalue(e$clustertype), horiz=TRUE,lwd=4)
           },
           Centroid = {
             out<-hclust(e$distance,method="centroid")
             plot(as.dendrogram(out), main = tclvalue(e$clustertype), horiz=TRUE,lwd=4)
           },
           Mcquitty = {
             out<-hclust(e$distance,method="mcquitty")
             plot(as.dendrogram(out), main = tclvalue(e$clustertype), horiz=TRUE,lwd=4)
           },
           Median = {
             out<-hclust(e$distance,method="median")
             plot(as.dendrogram(out), main = tclvalue(e$clustertype), horiz=TRUE,lwd=4)
           },
           Wards = {
             out<-hclust(e$distance, method = "ward.D")
             plot(as.dendrogram(out), main = tclvalue(e$clustertype), horiz = TRUE, lwd = 4)
           },
           Wards2 = {
             out<-hclust(D_Matrix_Results, method = "ward.D2")
             plot(as.dendrogram(out), main = tclvalue(e$clustertype), horiz = TRUE, lwd = 4)
           }
    )
    pos<-1
    envir <- as.environment(pos)
    assign("Cluster_Results", out, envir = envir)
    tkdestroy(window)
  }
  #Begin GUI Setup
  window<-tktoplevel()
  tkwm.title(window, "zooaRch")
  frame<-ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame<-ttklabelframe(frame, text = "Hierarchical Clustering", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label<-function(parent, text, row, column, sticky, width) {
    label<-ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = sticky)
  }
  #Distance Matrix Combobox
  put_label(label_frame, "Distance matrix:",1,0,sticky="w")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dist.fun(), 
                            textvariable = e$distancename)
  tkgrid(data_combo, row = 1, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo)
  
  tkwait.variable (e$distancename)
  
  distancename <- tclvalue(e$distancename)
  if (distancename == "Load User File"){
    dataframe <- getcsvfile()
    assign("distance", dataframe, envir = e)
  }
  else {
    dataframe <- get(distancename, .GlobalEnv)
    assign("distance", dataframe, envir = e)
  }
  put_label(label_frame, "Linkage Type:", 2,0, sticky="w")
  rb_frame<-ttkframe(label_frame)
  rb_frame2<-ttkframe(label_frame)
  rb_frame3<-ttkframe(label_frame)
  rb_frame4<-ttkframe(label_frame)
  rb_frame5<-ttkframe(label_frame)
  rb_frame6<-ttkframe(label_frame)
  rb_frame7<-ttkframe(label_frame)
  rb_frame8<-ttkframe(label_frame)
  radio_button<-tk2radiobutton(rb_frame, variable = e$clustertype, text = "Single", value = "Single")
  tkpack(radio_button, side = "left")
  tkgrid(rb_frame, row = 2, column = 1, sticky = "w")
  radio_button2<-tk2radiobutton(rb_frame2, variable = e$clustertype, text = "Complete", value = "Complete")
  tkpack(radio_button2, side = "left")
  tkgrid(rb_frame2, row = 3, column = 1, sticky = "w")
  radio_button3<-tk2radiobutton(rb_frame3, variable = e$clustertype, text = "Average", value = "Average")
  tkpack(radio_button3, side = "left")
  tkgrid(rb_frame3, row = 4, column = 1, sticky = "w")
  radio_button4<-tk2radiobutton(rb_frame4, variable = e$clustertype, text = "Centroid", value = "Centroid")
  tkpack(radio_button4, side = "left")
  tkgrid(rb_frame4, row = 5, column = 1, sticky = "w")
  radio_button5<-tk2radiobutton(rb_frame5, variable = e$clustertype, text = "Mcquitty", value = "Mcquitty")
  tkpack(radio_button5, side = "left")
  tkgrid(rb_frame5, row = 6, column = 1, sticky = "w")
  radio_button6<-tk2radiobutton(rb_frame6, variable = e$clustertype, text = "Median", value = "Median")
  tkpack(radio_button6, side = "left")
  tkgrid(rb_frame6, row = 7, column = 1, sticky = "w")
  radio_button7<-tk2radiobutton(rb_frame7, variable = e$clustertype, text = "Wards", value = "Wards")
  tkpack(radio_button7, side = "left")
  tkgrid(rb_frame7, row = 8, column = 1, sticky = "w")
  radio_button8<-tk2radiobutton(rb_frame8, variable = e$clustertype, text = "Wards2", value = "Wards2")
  tkpack(radio_button8, side = "left")
  tkgrid(rb_frame8, row = 9, column = 1, sticky = "w")
  
  #Draw Cancel Button
  button_frame<-ttkframe(frame)
  cancel_button<-ttkbutton(button_frame, text = "Cancel")
  
  #Draw Ok Button
  ok_button<-ttkbutton(button_frame, text = "Ok", command = Okfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,fill = "y", side = "left")
  sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
  tkconfigure(ok_button, command = Okfun)
  tkconfigure(cancel_button, command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# prcomp Function
prcomp_fun<-function(){
  
  
  
  #Data Model: enviroment called "e"
  e<-new.env()
  e$dataname <- tclVar("Choose Dataframe"); 
  e$dataframe <- tclVar("NULL"); 
  e$plotcheck<-tclVar("0")
  
  #Ok Function
  Okfun<-function(){
    
    #Error Checking
    if(sum(sapply(e$dataframe, is.numeric))==FALSE) {
      tkdestroy(window)
      stop("The data set must not contain characters!")
    }
    if(sum(sapply(e$dataframe, is.na))==TRUE) {
      tkdestroy(window)
      stop("The data set contains missing values!")
    }
    #Using the prcomp Function
    Value1 <- input1[as.numeric(tkcurselection(list1)) + 1]
    out<-prcomp(dataframe[,Value1])
    if(as.numeric(tclvalue(e$plotcheck))>0) {
      if(length(Value1)>1) {
        plot(out$x[,c(1,2)])
      }
      else {
        tkdestroy(window)
        stop("There must be more than one column selected!")
      }
    }
    print(summary(out))
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    tkdestroy(window)
  }
  #Begin GUI Setup
  window<-tktoplevel()
  tkwm.title(window, "PCA Function")
  frame<-ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame<-ttklabelframe(frame, text = "PCA Function", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label<-function(parent, text, row, column, sticky) {
    label<-ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = sticky)
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0,sticky="w")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Data Choice Listbox
  list1 <- tk2listbox(label_frame, height = 4, selectmode = "extended")
  put_label(label_frame, "Choose Data:",row=3,column=0,sticky="w")
  tkgrid(list1, padx = 10, pady = c(5, 10),row = 3, column = 1)
  input1 <- sort(colnames(e$dataframe))
  for (inputs in input1){
    tkinsert(list1, "end", inputs)
  }
  tkselection.set(list1, 0)
  
  #Draw Cancel Button
  button_frame<-ttkframe(frame)
  cancel_button<-ttkbutton(button_frame, text = "Cancel")
  
  #Plot Data
  put_label ( label_frame , "plot data:" , 5 , 0,sticky = "w")
  plot_check1 <-ttkcheckbutton (label_frame , variable = e$plotcheck)
  tkgrid (plot_check1 , row = 5 , column = 1 , sticky = "w" ,padx = 2)
  
  #Draw Ok Button
  ok_button<-ttkbutton(button_frame, text = "Ok", command = Okfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = Okfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

#Correspondence Analysis Function
correspondence_fun<-function(){
  
  
  #Data Model: enviroment called "e"
  e<-new.env()
  e$dataname <- tclVar("Choose Dataframe"); 
  e$dataframe <- tclVar("NULL"); 
  e$plotcheck<-tclVar("0")
  
  #Okfunction
  Okfun<-function(){
    #Error checking
    if(sum(sapply(e$dataframe, is.numeric))==FALSE) {
      tkdestroy(window)
      correspondence_fun()
      stop("The data set must not contain characters!")
    }
    if(sum(sapply(e$dataframe, is.na))==TRUE) {
      tkdestroy(window)
      stop("The data set contains missing values!")
    }
    #Using the Correspondence Function
    # print(ncol(e$dataframe)) # this needs to be deleted
    Value1 <- input1[as.numeric(tkcurselection(list1)) + 1]
    Value2 <- input2[as.numeric(tkcurselection(list2)) + 1]
    rownames(e$dataframe)<-as.character(e$dataframe[,Value2])
    out<-corresp(e$dataframe[,Value1]
                 , nf = (as.numeric(ncol(e$dataframe[,Value1]))-1)
                 ,data=e$dataframe)
    varmat<-rbind(eig<-out$cor[-which(out$cor^2<0.0000000000000001)]^2, eig/sum(eig), cumsum(eig/sum(eig)))
    rownames(varmat)<-c("Eigenvalues", "Percent Explained", "Cum Percent Explained")
    if(as.numeric(tclvalue(e$plotcheck))>0) {
      if(length(Value1)>1) {
        plot(out$rscore[,1:2],xlim=range(c(out$cscore[,1:2],out$rscore[,1:2])), 
             ylim=range(c(out$cscore[,1:2],out$rscore[,1:2])),type="n", 
             xlab="CA Axis I", ylab="CA Axis II")
        text(out$rscore[,1:2], label=rownames(out$Freq), col="blue",cex = .75)
        text(out$cscore[,1:2], label=colnames(out$Freq), col="red", pos=3, cex=1.5)
        arrows(0,0,out$cscore[,1], out$cscore[,2],col="red", pch=19,length=.15,lwd=2)
      }
      else {
        tkdestroy(window)
        stop("There must be more than one column selected!")
      }
    }
    out$varmat<-varmat
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) # added by EOC
    cat(paste("total inertia =", round(sum(eig),3)),"\n")
    print(varmat) # Changed the printting function to varmat
    tkdestroy(window)
  }
  #Begin GUI setup
  window<-tktoplevel()
  tkwm.title(window, "Correspondence Analysis")
  frame<-ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame<-ttklabelframe(frame, text = "Correspondence Analysis", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label<-function(parent, text, row, column, sticky) {
    label<-ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = sticky)
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0,sticky="w")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Data Choice Listbox
  list1 <- tk2listbox(label_frame, height = 4, selectmode = "extended")
  put_label(label_frame, "Choose Data:",row=3,column=0,sticky="w")
  tkgrid(list1, padx = 10, pady = c(5, 10),row = 3, column = 1)
  input1 <- sort(colnames(e$dataframe))
  for (inputs in input1){
    tkinsert(list1, "end", inputs)
  }
  tkselection.set(list1, 0)
  
  #Data Label Listbox
  list2 <- tk2listbox(label_frame, height = 4, selectmode = "extended")
  put_label(label_frame, "Choose Label:",row=5,column=0,sticky="w")
  tkgrid(list2, padx = 10, pady = c(5, 10),row = 5, column = 1)
  input2 <- sort(colnames(e$dataframe))
  for (inputs in input2){
    tkinsert(list2, "end", inputs)
  }
  tkselection.set(list2, 0)
  
  #Draw Cancel Button
  button_frame<-ttkframe(frame)
  cancel_button<-ttkbutton(button_frame, text = "Cancel")
  
  #Plot Data
  put_label ( label_frame , "plot data:" , 7 , 0,sticky = "w")
  plot_check1 <-ttkcheckbutton (label_frame , variable = e$plotcheck)
  tkgrid (plot_check1 , row = 7 , column = 1 , sticky = "w" ,padx = 2)

  #Draw Ok Button
  ok_button<-ttkbutton(button_frame, text = "Ok", command = Okfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = Okfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# PCoA Function
PCoA_fun<-function(){
  
  
  
  
  #Data Model: enviroment called "e"
  e<-new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$type<-tclVar("Euclidean")
  
  #Ok Function
  Okfun<-function(){
    
    #Error Checking
    if(sum(sapply(e$dataframe, is.numeric))==FALSE) {
      tkdestroy(window)
      stop("The data set must not contain characters!")
    }
    if(sum(sapply(e$dataframe, is.na))==TRUE) {
      tkdestroy(window)
      stop("The data set contains missing values!")
    }
    #Calculating the Distance
    Value1 <- input1[as.numeric(tkcurselection(list1)) + 1]
    switch(tclvalue(e$type),
           Canberra = {out<-as.matrix(dist(e$dataframe[,Value1],method="canberra"))
           print("Canberra")},
           Euclidean = {out<-as.matrix(dist(e$dataframe[,Value1],method="euclidean"))
           print("Euclidean")},
           Jaccard = {out<-as.matrix(vegdist(e$dataframe[,Value1],method="jaccard"))
           print("Jaccard")},
           Manhattan = {out<-as.matrix(dist(e$dataframe[,Value1],method="manhattan"))
           print("Manhattan")}
    )
    suppressWarnings(out<-cmdscale(out, k = as.numeric(nrow(e$dataframe[,Value1]))-1,eig = TRUE))
    out$eig<-out$eig[-which(out$eig<10^-16)]
    varmat<-rbind(eig<-out$eig, eig/sum(eig), cumsum(eig/sum(eig)))
    rownames(varmat)<-c("Eigenvalues", "Percent Explained", "Cum Percent Explained")
    colnames(varmat)<-paste("Axis",1:length(out$eig))
    dimnames(out$points)<-list(row.names(e$dataframe),paste("Axis",1:length(out$eig)) )
    out$varmat<-varmat
    pos<-1
    envir <- as.environment(pos)
    assign("PCoA_Results", out, envir = envir)
    plot(out$points, pch=21, cex=1.5,asp=1,xlab="PCo I", ylab="PCo II")
    print(out$varmat)
    tkdestroy(window)
  }
  #Begin GUI Setup
  window<-tktoplevel()
  tkwm.title(window, "Principal Coordinates Analysis")
  frame<-ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame<-ttklabelframe(frame, text = "Principal Coordinates Analysis", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label<-function(parent, text, row, column, sticky) {
    label<-ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = sticky)
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0,sticky="w")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Distance Type Selection Radiobuttons
  put_label(label_frame, "Distance Type", 2,0, sticky="w")
  rb_frame<-ttkframe(label_frame)
  sapply(c("Canberra", "Euclidean", "Jaccard", "Manhattan"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 2, column = 1, sticky = "w")
  
  #Data Choice Listbox
  list1 <- tk2listbox(label_frame, height = 5, selectmode = "extended")
  put_label(label_frame, "Choose Data:",row=3,column=0,sticky="w")
  tkgrid(list1, padx = 10, pady = c(5, 10),row = 3, column = 1)
  input1 <- sort(colnames(e$dataframe))
  for (inputs in input1){
    tkinsert(list1, "end", inputs)
  }
  tkselection.set(list1, 0)
  
  #Draw Cancel Button
  button_frame<-ttkframe(frame)
  cancel_button<-ttkbutton(button_frame, text = "Cancel")
  
  #Draw Ok Button
  ok_button<-ttkbutton(button_frame, text = "Ok", command = Okfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = Okfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# NMDS Function
NMDS_fun<-function(){
  #Data Model: enviroment called "e"
  e<-new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$type<-tclVar("Euclidean")
  e$kval<-tclVar(3)
  e$shepard<-tclVar(FALSE)
  e$labels<-tclVar(FALSE)
  e$plot<-tclVar(TRUE)
  
  #Ok Function
  Okfun<-function(){
    #Error Checking
    if(sum(sapply(e$dataframe, is.numeric))==FALSE) {
      tkdestroy(window)
      stop("The data set must not contain characters!")
    }
    if(sum(sapply(e$dataframe, is.na))==TRUE) {
      tkdestroy(window)
      stop("The data set contains missing values!")
    }
    #Calculating the Distance
    Value1 <- input1[as.numeric(tkcurselection(list1)) + 1]
    switch(tclvalue(e$type),
           Canberra = {#dist_value<-(dist(e$dataframe[,Value1],method="canberra"))
             #PCoA_val<-cmdscale(dist_value, k = (as.numeric(nrow(e$dataframe[,Value1]))-1))
             nmds_value<-metaMDS(e$dataframe[,Value1], k = as.numeric(tclvalue(e$kval)), distance = "canberra", autotransform = F)
           },
           Euclidean = {#dist_value<-as.matrix(dist(e$dataframe[,Value1],method="euclidean"))
             #PCoA_val<-cmdscale(dist_value, k = (as.numeric(nrow(e$dataframe[,Value1]))-1))
             nmds_value<-metaMDS(e$dataframe[,Value1], k = as.numeric(tclvalue(e$kval)), distance = "euclidean", autotransform = F)
           },
           Jaccard = {#dist_value<-as.matrix(dist(e$dataframe[,Value1],method="jaccard"))
             #PCoA_val<-cmdscale(dist_value, k = (as.numeric(nrow(e$dataframe[,Value1]))-1))
             nmds_value<-metaMDS(e$dataframe[,Value1], k = as.numeric(tclvalue(e$kval)), distance = "jaccard", autotransform = F)
           },
           Manhattan = {#dist_value<-as.matrix(dist(e$dataframe[,Value1],method="manhattan"))
             #PCoA_val<-cmdscale(dist_value, k = (as.numeric(nrow(e$dataframe[,Value1]))-1))
             nmds_value<-metaMDS(e$dataframe[,Value1], k = as.numeric(tclvalue(e$kval)), distance = "manhattan", autotransform = F)}
    )
    new_value<-as.matrix(nmds_value$points)
    out<-nmds_value
    labels<-as.logical(as.numeric(tclvalue(e$labels)))
    drawplot<-as.logical(as.numeric(tclvalue(e$plot)))
    shep<-as.logical(as.numeric(tclvalue(e$shepard)))
    if(drawplot==TRUE & labels!=TRUE) {
      plot(new_value, pch = 21, cex=1.5, asp=1,xlab="NMDS 1", ylab="NMDS 2")
    }
    if(drawplot==TRUE & labels==TRUE) {
      plot(new_value, pch = 21, cex=1.5, asp=1,xlab="NMDS 1", ylab="NMDS 2",type="n")
      text(new_value,labels =row.names(e$dataframe[,Value1]))
    }
    if(shep==TRUE) {
      dev.new()
      stressplot(out)
    }
    tkdestroy(window)
  }
  #Begin GUI Setup
  window<-tktoplevel()
  tkwm.title(window, "zooaRch")
  frame<-ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame<-ttklabelframe(frame, text = "Non-Metric Multidimensional Scaling", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label<-function(parent, text, row, column, sticky) {
    label<-ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = sticky)
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0,sticky="w")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #K Value Combobox
  put_label(label_frame, "Dimensions:",1,0,sticky="w")
  kval_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = 1:(as.numeric(nrow(e$dataframe))-1), 
                            textvariable = e$kval)
  tkgrid(kval_combo, row = 1, column = 1, sticky="w", padx = 2)
  tkfocus(kval_combo)     
  
  #Distance Type Selection Radiobuttons
  put_label(label_frame, "Distance Type", 2,0, sticky="w")
  rb_frame<-ttkframe(label_frame)
  sapply(c("Canberra", "Euclidean", "Jaccard", "Manhattan"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 2, column = 1, sticky = "w")
  
  #Data Choice Listbox
  list1 <- tk2listbox(label_frame, height = 4, selectmode = "extended")
  put_label(label_frame, "Choose Data:",row=3,column=0,sticky="w")
  tkgrid(list1, padx = 10, pady = c(5, 10),row = 3, column = 1)
  input1 <- sort(colnames(e$dataframe))
  for (inputs in input1){
    tkinsert(list1, "end", inputs)
  }
  tkselection.set(list1, 0)
  
  #Plot Checkbox
  put_label ( label_frame , "NMDS plot:" , 6 , 0,sticky = "w")
  plot_check1 <-ttkcheckbutton (label_frame , variable = e$plot)
  tkgrid (plot_check1 , row = 6 , column = 1 , sticky = "w" ,padx = 2)
  
  #Labels Checkbox
  put_label ( label_frame , "Labels:" , 6 , 2,sticky = "w")
  plot_check1 <-ttkcheckbutton (label_frame , variable = e$labels)
  tkgrid (plot_check1 , row = 6 , column = 3 , sticky = "w" ,padx = 2)
  
  #Shepard Checkbox
  put_label ( label_frame , "Shepard plot:" , 6 , 4,sticky = "w")
  plot_check1 <-ttkcheckbutton (label_frame , variable = e$shepard)
  tkgrid (plot_check1 , row = 6 , column = 5 , sticky = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame<-ttkframe(frame)
  cancel_button<-ttkbutton(button_frame, text = "Cancel")
  
  #Draw Ok Button
  ok_button<-ttkbutton(button_frame, text = "Ok", command = Okfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = Okfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# 2-way Mantel Function
mantel2way_fun<-function(){
  
  
  
  
  #Data Model: enviroment called "e"
  e<-new.env()
  e$dataname <- tclVar("Choose Dataframe")
  e$dataname2<-tclVar("Choose Dataframe")
  e$dataframe <- tclVar("NULL") 
  e$dataframe2<-tclVar("NULL")
  e$permutations<-tclVar(999)
  e$type1<-tclVar("Euclidean")
  e$type2<-tclVar("Euclidean")
  
  #Ok Function
  Okfun<-function(){
    
    #Using Selected Values
    Value_First <- input1[as.numeric(tkcurselection(list1)) + 1]
    Value_Second <- input2[as.numeric(tkcurselection(list2)) + 1]
    
    #Error Checking
    if(sum(sapply(e$dataframe[,Value_First], is.numeric))==FALSE) { 
      print("First matrix must not contain characters!")
      tkdestroy(window)
      mantel2way_fun()
      stop(call. = FALSE)
    }
    if(sum(sapply(e$dataframe, is.na))==TRUE) {
      print("The first data set is missing values!")
      tkdestroy(window)
      mantel2way_fun()
      stop(call. = FALSE)
    }
    if(sum(sapply(e$dataframe2[,Value_Second], is.numeric))==FALSE) { 
      print("Second matrix must not contain characters!")
      tkdestroy(window)
      mantel2way_fun
      stop(call. = FALSE)
    }
    if(sum(sapply(e$dataframe2, is.na))==TRUE) {
      print("The second data set is missing values!")
      tkdestroy(window)
      mantel2way_fun
      stop(call. = FALSE)
    }
    #Distance Functions
    switch(tclvalue(e$type1),
           Canberra = {dist1<-as.matrix(dist(e$dataframe[,Value_First ],method="canberra"))
           },
           Euclidean = {dist1<-as.matrix(dist(e$dataframe[,Value_First ],method="euclidean"))
           },
           Jaccard = {dist1<-as.matrix(vegdist(e$dataframe[,Value_First ],method="jaccard"))
           },
           Manhattan = {dist1<-as.matrix(dist(e$dataframe[,Value_First ],method="manhattan"))
           }
    )
    switch(tclvalue(e$type2),
           Canberra = {dist2<-as.matrix(dist(e$dataframe2[,Value_Second],method="canberra"))
           },
           Euclidean = {dist2<-as.matrix(dist(e$dataframe2[,Value_Second],method="euclidean"))
           },
           Jaccard = {dist2<-as.matrix(vegdist(e$dataframe2[,Value_Second],method="jaccard"))
           },
           Manhattan = {dist2<-as.matrix(dist(e$dataframe2[,Value_Second],method="manhattan"))}
    )
    dist1[is.na(dist1)]<-0
    dist2[is.na(dist2)]<-0
    out<-mantel(xdis=dist1,ydis=dist2, permutations=as.numeric(tclvalue(e$permutations)),na.rm = TRUE)
    print(out)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    tkdestroy(window)
  }
  #Begin GUI Setup
  window<-tktoplevel()
  tkwm.title(window, "2 Matrix Mantel Test")
  frame<-ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame<-ttklabelframe(frame, text = "2 Matrix Mantel", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label<-function(parent, text, row, column, sticky, width) {
    label<-ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = sticky)
  }
  #First Data Combobox
  put_label(label_frame, "First Dataframe:",0,0,sticky="w")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo)
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Second Data Combobox
  put_label(label_frame, "Second Dataframe:",2,0,sticky="w") 
  data_combo2 <- ttkcombobox(label_frame, state = "readonly", 
                             values = dfs.fun(), 
                             textvariable = e$dataname2)
  tkgrid(data_combo2, row = 2, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo2)                      
  
  tkwait.variable (e$dataname2)
  
  dataname2 <- tclvalue(e$dataname2)
  if (dataname2 == "Load User File"){
    dataframe2 <- getcsvfile()
    assign("dataframe2", dataframe2, envir = e)
  }
  if(dataname2 != "Load User File"){
    dataframe2 <- get(dataname2, .GlobalEnv)
    assign("dataframe2", dataframe2, envir = e)
  }
  #First Dataframe Listbox
  list1 <- tk2listbox(label_frame, height = 4, selectmode = "extended")
  put_label(label_frame, "First Matrix:",row=4,column=0,sticky="n")
  tkgrid(list1, padx = 10, pady = c(5, 10),row = 5, column = 0)
  input1 <- sort(colnames(e$dataframe))
  for (inputs in input1){
    tkinsert(list1, "end", inputs)
  }
  tkselection.set(list1, 0)
  
  #Second Dataframe Listbox
  list2<-tk2listbox(label_frame, height = 4, selectmode = "extended")
  put_label(label_frame, "Second Matrix:", row = 4, column = 1, sticky = "n")
  tkgrid(list2, padx = 10, pady = c(5, 10), row = 5, column = 1)
  input2<-colnames(e$dataframe2)
  for(inputs in input2){
    tkinsert(list2, "end", inputs)
  }
  tkselection.set(list2, 0)
  
  #Method Selection
  #First Matrix Radiobuttons
  put_label(label_frame, "First Matrix Method:", 7,0, sticky="w")
  rb_frame<-ttkframe(label_frame)
  sapply(c("Canberra", "Euclidean", "Jaccard", "Manhattan"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type1, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 7, column = 1, sticky = "w")
  
  #Second Matrix Radiobuttons
  put_label(label_frame, "Second Matrix Method:", 8,0, sticky="w")
  rb_frame<-ttkframe(label_frame)
  sapply(c("Canberra", "Euclidean", "Jaccard", "Manhattan"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type2, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 8, column = 1, sticky = "w")
  
  #Permutation Slider
  put_label(label_frame, "Permutations:", 9, 0, sticky = "w")
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 9, column = 1, columnspan = 2, 
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame, 
                               from = 99, to = 10000,  
                               variable = e$permutations)
  #Permutation Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 99, to = 10000, increment = 1, 
                               textvariable = e$permutations, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
  
  #Draw Cancel Button
  button_frame<-ttkframe(frame)
  cancel_button<-ttkbutton(button_frame, text = "Cancel")
  
  #Draw Ok Button
  ok_button<-ttkbutton(button_frame, text = "Ok", command = Okfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = Okfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# 3 Way Mantel Function
mantel3_fun<-function(){
  
  
  
  
  #Data Model: enviroment called "e"
  e<-new.env()
  e$dataname <- tclVar("Choose Dataframe")
  e$dataname2<-tclVar("Choose Dataframe")
  e$dataname3<-tclVar("Choose Dataframe")
  e$dataframe <- tclVar("NULL") 
  e$dataframe2<-tclVar("NULL")
  e$dataframe3<-tclVar("NULL")
  e$permutations<-tclVar(999)
  e$type1<-tclVar("Euclidean")
  e$type2<-tclVar("Euclidean")
  e$type3<-tclVar("Euclidean")
  
  #Ok Function
  Okfun<-function(){
    
    #Using Selected Values
    Value_First <- input1[as.numeric(tkcurselection(list1)) + 1]
    Value_Second <- input2[as.numeric(tkcurselection(list2)) + 1]
    Value_Third <- input3[as.numeric(tkcurselection(list3)) + 1]
    
    #Error Checking
    if(sum(sapply(e$dataframe[,Value_First], is.numeric))==FALSE) { 
      print("First matrix must not contain characters!")
      tkdestroy(window)
      mantel3_fun()
      stop(call. = FALSE)
    }
    if(sum(sapply(e$dataframe, is.na))==TRUE) {
      print("The first data set is missing values!")
      tkdestroy(window)
      mantel3_fun()
      stop(call. = FALSE)
    }
    if(sum(sapply(e$dataframe2[,Value_Second], is.numeric))==FALSE) { 
      print("Second matrix must not contain characters!")
      tkdestroy(window)
      mantel3_fun()
      stop(call. = FALSE)
    }
    if(sum(sapply(e$dataframe2, is.na))==TRUE) {
      print("The second data set is missing values!")
      tkdestroy(window)
      mantel3_fun()
      stop(call. = FALSE)
    }
    if(sum(sapply(e$dataframe3[,Value_Third], is.numeric))==FALSE) {
      print("Third matrix must not contain characters!")
      tkdestroy(window)
      mantel3_fun()
      stop(call. = FALSE)
    }
    if(sum(sapply(e$dataframe3, is.na))==TRUE) {
      print("The third data set is missing values!")
      tkdestroy(window)
      mantel3_fun()
      stop(call. = FALSE)
    }
    #Distance Functions
    switch(tclvalue(e$type1),
           Canberra = {dist1<-as.matrix(dist(e$dataframe[,Value_First ],method="canberra"))
           },
           Euclidean = {dist1<-as.matrix(dist(e$dataframe[,Value_First ],method="euclidean"))
           },
           Jaccard = {dist1<-as.matrix(vegdist(e$dataframe[,Value_First ],method="jaccard"))
           },
           Manhattan = {dist1<-as.matrix(dist(e$dataframe[,Value_First ],method="manhattan"))
           }
    )
    switch(tclvalue(e$type2),
           Canberra = {dist2<-as.matrix(dist(e$dataframe2[,Value_Second],method="canberra"))
           },
           Euclidean = {dist2<-as.matrix(dist(e$dataframe2[,Value_Second],method="euclidean"))
           },
           Jaccard = {dist2<-as.matrix(vegdist(e$dataframe2[,Value_Second],method="jaccard"))
           },
           Manhattan = {dist2<-as.matrix(dist(e$dataframe2[,Value_Second],method="manhattan"))}
    )
    switch(tclvalue(e$type3),
           Canberra = {dist3<-as.matrix(dist(e$dataframe3[,Value_Third],method="canberra"))
           },
           Euclidean = {dist3<-as.matrix(dist(e$dataframe3[,Value_Third],method="euclidean"))
           },
           Jaccard = {dist3<-as.matrix(vegdist(e$dataframe3[,Value_Third],method="jaccard"))
           },
           Manhattan = {dist3<-as.matrix(dist(e$dataframe3[,Value_Third],method="manhattan"))}
    )
    dist1[is.na(dist1)]<-0
    dist2[is.na(dist2)]<-0
    dist3[is.na(dist3)]<-0
    out<-mantel.partial(xdis = dist1,ydis = dist2,zdis = dist3, permutations=as.numeric(tclvalue(e$permutations)), na.rm = TRUE)  
    print(out)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    tkdestroy(window)
  }
  #Begin GUI Setup
  window<-tktoplevel()
  tkwm.title(window, "3 Matrix Mantel Test")
  frame<-ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame<-ttklabelframe(frame, text = "3 Matrix Mantel", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label<-function(parent, text, row, column, sticky, width) {
    label<-ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = sticky)
  }
  #First Data Combobox
  put_label(label_frame, "First Dataframe:",0,0,sticky="w")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo)
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Second Data Combobox
  put_label(label_frame, "Second Dataframe:",2,0,sticky="w") 
  data_combo2 <- ttkcombobox(label_frame, state = "readonly", 
                             values = dfs.fun(), 
                             textvariable = e$dataname2)
  tkgrid(data_combo2, row = 2, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo2)                      
  
  tkwait.variable (e$dataname2)
  
  dataname2 <- tclvalue(e$dataname2)
  if (dataname2 == "Load User File"){
    dataframe2 <- getcsvfile()
    assign("dataframe2", dataframe2, envir = e)
  }
  if(dataname2 != "Load User File"){
    dataframe2 <- get(dataname2, .GlobalEnv)
    assign("dataframe2", dataframe2, envir = e)
  }
  #Third Data Combobox
  put_label(label_frame, "Third Dataframe:",3,0,sticky="w") 
  data_combo3 <- ttkcombobox(label_frame, state = "readonly", 
                             values = dfs.fun(), 
                             textvariable = e$dataname3)
  tkgrid(data_combo3, row = 3, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo3)                      
  
  tkwait.variable (e$dataname3)
  
  dataname3 <- tclvalue(e$dataname3)
  if (dataname3 == "Load User File"){
    dataframe3 <- getcsvfile()
    assign("dataframe3", dataframe3, envir = e)
  }
  if(dataname3 != "Load User File"){
    dataframe3 <- get(dataname3, .GlobalEnv)
    assign("dataframe3", dataframe3, envir = e)
  }
  #First Dataframe Listbox
  list1 <- tk2listbox(label_frame, height = 4, selectmode = "extended")
  put_label(label_frame, "First Matrix:",row=4,column=0,sticky="n")
  tkgrid(list1, padx = 10, pady = c(5, 10),row = 5, column = 0)
  input1 <- sort(colnames(e$dataframe))
  for (inputs in input1){
    tkinsert(list1, "end", inputs)
  }
  tkselection.set(list1, 0)
  
  #Second Dataframe Listbox
  list2<-tk2listbox(label_frame, height = 4, selectmode = "extended")
  put_label(label_frame, "Second Matrix:", row = 4, column = 1, sticky = "n")
  tkgrid(list2, padx = 10, pady = c(5, 10), row = 5, column = 1)
  input2<-colnames(e$dataframe2)
  for(inputs in input2){
    tkinsert(list2, "end", inputs)
  }
  tkselection.set(list2, 0)
  
  #Third Dataframe Listbox
  list3<-tk2listbox(label_frame, height = 4, selectmode = "extended")
  put_label(label_frame, "Third Matrix:", row = 4, column = 2, sticky = "n")
  tkgrid(list3, padx = 10, pady = c(5, 10), row = 5, column = 2)
  input3<-colnames(e$dataframe3)
  for(inputs in input3){
    tkinsert(list3, "end", inputs)
  }
  tkselection.set(list3, 0)
  
  #Method Selection
  #Matrix 1 Radiobuttons
  put_label(label_frame, "Third Matrix Method:", 7,0, sticky="w")
  rb_frame<-ttkframe(label_frame)
  sapply(c("Canberra", "Euclidean", "Jaccard", "Manhattan"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type1, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 7, column = 1, sticky = "w")
  
  #Matrix 2 Radiobuttons
  put_label(label_frame, "Second Matrix Method:", 8,0, sticky="w")
  rb_frame<-ttkframe(label_frame)
  sapply(c("Canberra", "Euclidean", "Jaccard", "Manhattan"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type2, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 8, column = 1, sticky = "w")
  
  #Matrix 3 Radiobuttons
  put_label(label_frame, "Third Matrix Method:", 9,0, sticky="w")
  rb_frame<-ttkframe(label_frame)
  sapply(c("Canberra", "Euclidean", "Jaccard", "Manhattan"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type3, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 9, column = 1, sticky = "w")
  
  #Permutation Slider
  put_label(label_frame, "Permutations:", 10, 0, sticky = "w")
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 10, column = 1, columnspan = 2, 
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame, 
                               from = 99, to = 10000,  
                               variable = e$permutations)
  #Permutation Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 99, to = 10000, increment = 1, 
                               textvariable = e$permutations, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
  
  #Draw Cancel Button
  button_frame<-ttkframe(frame)
  cancel_button<-ttkbutton(button_frame, text = "Cancel")
  
  #Draw Ok Button
  ok_button<-ttkbutton(button_frame, text = "Ok", command = Okfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")              
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = Okfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# Ripleys K Function
ripleys_K_fun<-function() {
  sppdfs.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(SpatialPointsDataFrame) if(class(get(SpatialPointsDataFrame))[1] == "SpatialPointsDataFrame")c(unlist(SpatialPointsDataFrame)))), "Load User File", "Load Shape File")
  list.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(list) if (class(get(list))[1] == "list") c(unlist(list)))), "Load User File", "Load Shape File")
  
  #Data Model: enviroment called "e"
  e<-new.env()
  e$dataname<-tclVar("Choose Data")
  e$dataframe<-tclVar("NULL")
  e$permutations<-tclVar(999)
  e$bins<-tclVar(10)
  e$polygon<-tclVar("Custom")
  e$shape<-tclVar("NULL")
  e$polynum<-tclVar("NULL")
  e$shapefile<-tclVar("NULL")
  
  #Ok Function
  Okfun<-function() {
    if(class(e$dataframe)=="list"){
      e$dataframe<-unlist(e$dataframe)
    }
    if(class(e$dataframe)=="data.frame"){
      e$dataframe<-unlist(e$dataframe)
      e$dataframe<-as.matrix(e$dataframe)
      fundata<-cbind(e$dataframe[,1], e$dataframe[,2])
      fundata.pts<-splancs::as.points(fundata[,1],fundata[,2])
      print(class(fundata.pts))
      summary(fundata.pts)
      pointmap(fundata.pts,pch="+",cex=2)
    }
    if(class(e$dataframe)=="SpatialPointsDataFrame"){
      fundata<-e$dataframe@coords
      fundata.pts<-e$dataframe@coords
      print(class(fundata.pts))
      summary(fundata.pts)
      splancs::pointmap(fundata.pts)
    }
    #Creating Bounding Polygon
    switch(tclvalue(e$polygon),
           Custom = {e$shape<-splancs::getpoly(quiet=TRUE)
           splancs::polymap(e$shape, add=TRUE)
           splancs::pointmap(fundata.pts, add=T, pch=20)},
           Load = {getshapefile <- function() {
             name <- tclvalue(tkgetOpenFile(
               filetypes = "{{ESRI Shapefiles} {.shp}}"))
             if (name == "") return(data.frame());
             e$shapefile <- raster::shapefile(name)
             e$polynum<-length(grep("Polygon",x=e$shapefile@polygons[[1]]@Polygons))
             e$shape<-e$shapefile@polygons[[1]]@Polygons[[1]]@coords
             if (e$polynum != 1) {
               tkdestroy(window)
               stop("The shapefile cannot contain more than one polygon!")
             }
             splancs::polymap(poly = e$shape, add=FALSE)
             splancs::pointmap(pts = fundata.pts, add=TRUE, pch=20)
           }
           getshapefile()}
    )
    #K Function
    n.fundata<-dim(fundata)[1]
    fundata.k.dists<-seq(from=0,to=max(dist(fundata.pts)),by=as.numeric(tclvalue(e$bins)))
    fundata.k<-splancs::khat(fundata.pts,e$shape,fundata.k.dists,checkpoly=TRUE)  #RIPLEY'S K
    fundata.K.env<-splancs::Kenv.csr(n.fundata,e$shape,as.numeric(tclvalue(e$permutations)),fundata.k.dists)
    plot(fundata.k.dists,fundata.k,type='l', main="K-Function",ylab="Estimated K",xlab="Distance",ylim=range(c(fundata.K.env$up,fundata.K.env$low)))
    lines(fundata.k.dists,fundata.K.env$upper,col=2,lty=2)
    lines(fundata.k.dists,fundata.K.env$lower,col=3,lty=2)
    lines(fundata.k.dists,pi*(fundata.k.dists)^2,col=2)
    
    #L Function
    dev.new()
    fundata.l<-sqrt(fundata.k/pi)-fundata.k.dists
    plot(fundata.k.dists,fundata.l,type="l",
         ylim=range(c(sqrt(fundata.K.env$up/pi)-fundata.k.dists,sqrt(fundata.K.env$lower/pi)-fundata.k.dists)),
         main="L Function", ylab="Estimated L",xlab="Distance")
    lines(fundata.k.dists,sqrt(fundata.K.env$up/pi)-fundata.k.dists,col="red",lty=2)
    lines(fundata.k.dists,sqrt(fundata.K.env$low/pi)-fundata.k.dists,col="green",lty=2)
    lines(fundata.k.dists,sqrt((fundata.k.dists)^2)-fundata.k.dists,col="blue")
    tkdestroy(window)
  }
  #Begin GUI Setup
  window<-tktoplevel()
  tkwm.title(window, "Ripley's K_fun")
  frame<-ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame<-ttklabelframe(frame, text = "Ripleys K function", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label<-function(parent, text, row, column, sticky, width) {
    label<-ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = sticky)
  }
  #First Data Combobox
  put_label(label_frame, "Input Data:",0,0,sticky="w")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = c(sppdfs.fun()[-which(sppdfs.fun()%in%dfs.fun()==TRUE)],dfs.fun()), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo)
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if (dataname == "Load Shape File"){
    getshapefile <- function() {
      name <- tclvalue(tkgetOpenFile(
        filetypes = "{{ESRI Shapefiles} {.shp}}"))
      if (name == "") return(data.frame());
      data <- raster::shapefile(name)
      e$dataframe<-data@coords
    }
    dataframe <- getshapefile()
  }  else {
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Shape Type Radiobuttons
  put_label(label_frame, "Shape Type:", 3,0, sticky="w")
  rb_frame<-ttkframe(label_frame)
  sapply(c("Load", "Custom"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$polygon, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 3, column = 1, sticky = "w")
  
  #Bins Slider
  put_label(label_frame, "Number of Bins:", 4, 0, sticky = "w")
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 4, column = 1, columnspan = 2, 
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame, 
                               from = 1, to = 100,  
                               variable = e$bins)
  #Bins Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 1, to = 100, increment = 1, 
                               textvariable = e$bins, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
  
  #Permutation Slider
  put_label(label_frame, "Permutations:", 5, 0, sticky = "w")
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 5, column = 1, columnspan = 2, 
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame, 
                               from = 99, to = 10000,  
                               variable = e$permutations)
  #Permutation Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 99, to = 10000, increment = 1, 
                               textvariable = e$permutations, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
  
  #Draw Cancel Button
  button_frame<-ttkframe(frame)
  cancel_button<-ttkbutton(button_frame, text = "Cancel")
  
  #Draw Ok Button
  ok_button<-ttkbutton(button_frame, text = "Ok", command = Okfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,fill = "y", side = "left")
  sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
  tkconfigure(ok_button, command = Okfun)
  tkconfigure(cancel_button, command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# Spatially Defined Connections Function
#  spatially_defined_fun<-function(){
#   spdfs.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(SpatialPolygonsDataFrame) if(class(get(SpatialPolygonsDataFrame))[1] == "SpatialPolygonsDataFrame")c(unlist(SpatialPolygonsDataFrame)))), "Load User File", "Load Shape File")
#   
#   #Data Model: enviroment called e
#   e<-new.env()
#   e$dataname<-tclVar("Choose Data")
#   e$dataframe<-tclVar("NULL")
#   e$choice<-tclVar("Rook")
#   e$voronicheck<-tclVar("FALSE")
#   
#   #Ok Function
#   Okfun<-function() {
#     switch(tclvalue(e$choice),
#            Queen = {fundata.queen <- spdep::poly2nb(e$dataframe, queen = TRUE)
#            plot(e$dataframe,col="light gray")
#            title(main="Queen Connectivity")
#            plot(fundata.queen,sp::coordinates(e$dataframe),add=TRUE,col="red",lwd=2)
#            coords<-sp::coordinates(e$dataframe)
#            if(tclvalue(e$voronicheck) == 1){
#              voroni_fun(fundata.queen, coords)
#            }},
#            Rook = {fundata.rook <- spdep::poly2nb(e$dataframe, queen = FALSE)
#            plot(e$dataframe,col="light gray")
#            title(main="Rook Connectivity")
#            plot(fundata.rook,sp::coordinates(e$dataframe),add=TRUE,col="blue",lwd=2)
#            coords<-sp::coordinates(e$dataframe)
#            if(tclvalue(e$voronicheck) == 1){
#              voroni_fun(fundata.rook, coords)
#            }},
#            Delauney = {coords<-sp::coordinates(e$dataframe)
#            ID<-row.names(assign(e$dataframe, "data.frame"))
#            del.nb<-tri2nb(coords, row.names=ID)
#            plot(e$dataframe, border="grey60")
#            plot(del.nb, coords, add=TRUE, pch=".", col="red")
#            text(bbox(e$dataframe)[1,2], bbox(e$dataframe)[2,1], labels="Delauney", cex = 1.5, pos = 2)
#            if(tclvalue(e$voronicheck) == 1){
#              voroni_fun(del.nb, coords)
#            }},
#            Gabriel = {coords<-coordinates(e$dataframe)
#            ID<-row.names(assign(e$dataframe, "data.frame"))
#            gab.nb<-graph2nb(gabrielneigh(coords), row.names = ID)
#            plot(e$dataframe, border = "grey60")
#            plot(gab.nb, coords, add = TRUE, pch = ".", col= "red")
#            text(bbox(e$dataframe)[1,2], bbox(e$dataframe)[2,1], labels = "Gabriel", cex = 1.5, pos = 2)
#            if(tclvalue(e$voronicheck) == 1){
#              voroni_fun(gab.nb, coords)
#            }},
#            Relative = {coords<-coordinates(e$dataframe)
#            ID<-row.names(assign(e$dataframe, "data.frame"))
#            rel.nb<-spdep::graph2nb(spdep::relativeneigh(coords), row.names = ID)
#            plot(e$dataframe, border = "grey60")
#            plot(rel.nb, coords, add = TRUE, pch = ".", col = "red")
#            text(bbox(e$dataframe)[1,2], bbox(e$dataframe)[2,1], labels = "Relative", cex = 1.5, pos = 2)
#            if(tclvalue(e$voronicheck) == 1){
#              voroni_fun(rel.nb, coords)}}
#     )
#     tkdestroy(window)
#   }
#   #Voroni Function
#   voroni_fun<-function(input.nb, coordsin){
#     par(mfrow=c(1,3))
#     plot(input.nb, coordsin)
#     Mosaic.vm <- tripack::voronoi.mosaic(coordsin[,1],coordsin[,2])
#     plot(Mosaic.vm)
#     Polygons.vp <- tripack::voronoi.polygons(Mosaic.vm)
#     plot(Polygons.vp)
#   }
#   #Begin GUI Setup
#   window<-tktoplevel()
#   tkwm.title(window, "neighbor_fun")
#   frame<-ttkframe(window, padding = c(3,3,12,12))
#   tkpack(frame, expand = TRUE, fill = "both")
#   
#   #Layout
#   label_frame<-ttklabelframe(frame, text = "Neighboring Data Analysis", padding = 10)
#   tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
#   tkgrid.columnconfigure(label_frame, 0, weight = 1)
#   tkgrid.columnconfigure(label_frame, 1, weight = 1)
#   tkgrid.columnconfigure(label_frame, 2, weight = 1)
#   tkgrid.columnconfigure(label_frame, 1, weight = 1)
#   put_label<-function(parent, text, row, column, sticky, width) {
#     label<-ttklabel(parent, text = text)
#     tkgrid(label, row = row, column = column, sticky = sticky)
#   }
#   #First Data Combobox
#   put_label(label_frame, "Input Data:",0,0,sticky="w")
#   data_combo <- ttkcombobox(label_frame, state = "readonly", 
#                             values = spdfs.fun(), 
#                             textvariable = e$dataname)
#   tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2)
#   tkfocus(data_combo)
#   
#   tkwait.variable (e$dataname)
#   
#   dataname <- tclvalue(e$dataname)
#   if (dataname == "Load User File"){
#     dataframe <- getcsvfile()
#     assign("dataframe", dataframe, envir = e)
#   }
#   else if (dataname == "Load Shape File"){
#     getshapefile <- function() {
#       name <- tclvalue(tkgetOpenFile(
#         filetypes = "{{ESRI Shapefiles} {.shp}}"))
#       if (name == "") return(data.frame());
#       data <- shapefile(name)
#       e$dataframe<-data
#     }
#     dataframe <- getshapefile()
#   }
#   else{
#     dataframe <- get(dataname, .GlobalEnv)
#     assign("dataframe", dataframe, envir = e)
#   }
#   #Method Radiobuttons
#   put_label(label_frame, "Analysis Type:", 3,0, sticky="w")
#   rb_frame<-ttkframe(label_frame)
#   sapply(c("Queen", "Rook", "Delauney", "Gabriel", "Relative"), function(i) {
#     radio_button<-tk2radiobutton(rb_frame, variable = e$choice, text = i, value = i)
#     tkpack(radio_button, side = "left")
#   })
#   tkgrid(rb_frame, row = 3, column = 1, sticky = "w")
#   
#   #Voroni Data Checkbox
#   put_label ( label_frame , "Voroni data:" , 4 , 0,sticky = "w")
#   plot_check1 <-ttkcheckbutton (label_frame , variable = e$voronicheck)
#   tkgrid (plot_check1 , row = 4 , column = 1 , sticky = "w" ,padx = 2)
#   
#   #Draw Cancel Button
#   button_frame<-ttkframe(frame)
#   cancel_button<-ttkbutton(button_frame, text = "Cancel")
#   
#   #Draw Ok Button
#   ok_button<-ttkbutton(button_frame, text = "Ok", command = Okfun)
#   tkpack(button_frame, fill = "x", padx = 5, pady = 5)
#   tkpack(ttklabel(button_frame, text = " "), expand = TRUE,fill = "y", side = "left")
#   sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
#   tkconfigure(ok_button, command = Okfun)
#   tkconfigure(cancel_button, command = function() tkdestroy(window))
#   tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
#   tkfocus(window)
# }

# Moran's I/Geary C Function
moran_geary_fun<-function(){
  spdfs.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(SpatialPolygonsDataFrame) if(class(get(SpatialPolygonsDataFrame))[1] == "SpatialPolygonsDataFrame")c(unlist(SpatialPolygonsDataFrame)))), "Load User File", "Load Shape File")
  num.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(numeric) if(class(get(numeric))[1] == "numeric")c(unlist(numeric)))), "Load User File", "Load Shape File")
  
  #Data Model: enviroment called e
  e<-new.env()
  e$dataname<-tclVar("Choose Data")
  e$dataframe<-tclVar("NULL")
  e$variable<-tclVar("NULL")
  e$bins<-tclVar(10)
  e$method<-tclVar("Moran")
  e$displayname<-tclVar("")
  e$varname<-tclVar("Choose Variable")
  
  #OK Function
  Okfun<-function() {
    switch(tclvalue(e$method),
           Moran = {corD <- pgirmess::correlog(coordinates(e$dataframe), e$variable, method = tclvalue(e$method), nbclass = as.numeric(tclvalue(e$bins)))
           plot(corD, main = tclvalue(e$displayname))},
           Geary = {corD <- pgirmess::correlog(coordinates(e$dataframe), e$variable, method = tclvalue(e$method), nbclass = as.numeric(tclvalue(e$bins)))
           plot(corD, main = tclvalue(e$displayname))}
    )
    tkdestroy(window)
  }
  #Begin GUI Setup
  window<-tktoplevel()
  tkwm.title(window, "Moransi_fun")
  frame<-ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame<-ttklabelframe(frame, text = "Moran I Correlogram", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label<-function(parent, text, row, column, sticky, width) {
    label<-ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = sticky)
  }
  #First Data Combobox
  put_label(label_frame, "Input Data:",0,0,sticky="w")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = spdfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo)
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if (dataname == "Load Shape File"){
    getshapefile <- function() {
      name <- tclvalue(tkgetOpenFile(
        filetypes = "{{ESRI Shapefiles} {.shp}}"))
      if (name == "") return(data.frame());
      data <- shapefile(name)
      e$dataframe<-data
    }
    dataframe <- getshapefile()
  } else {
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Variable Data Combobox
  put_label(label_frame, "Input Variable:",2,0,sticky="w")
  data_combo2 <- ttkcombobox(label_frame, state = "readonly", 
                             values = colnames(as.data.frame(e$dataframe)), 
                             textvariable = e$varname)
  tkgrid(data_combo2, row = 2, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo2)
  
  tkwait.variable (e$varname)
  
  varname <- tclvalue(e$varname)
  variable <- eval(parse(text = paste("dataframe$", varname, sep = "")))
  assign("variable", variable, envir = e)
  
  #Method Selection Buttons
  put_label(label_frame, "Method:", 4,0, sticky="w")
  rb_frame<-ttkframe(label_frame)
  sapply(c("Moran", "Geary"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$method, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 4, column = 1, sticky = "w")
  
  #Bins Slider
  put_label(label_frame, "Number of Bins:", 5, 0, sticky = "w")
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 5, column = 1, columnspan = 2, 
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame, 
                               from = 1, to = 100,  
                               variable = e$bins)
  #Bins Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 1, to = 100, increment = 1, 
                               textvariable = e$bins, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
  
  #Display Entry
  put_label(label_frame, "Display Title:", 3,0, sticky="w")
  entrybox<-ttkentry(label_frame, textvariable = e$displayname)
  tkgrid(entrybox, row = 3, column = 1, sticky = "w")
  
  #Draw Cancel Button
  button_frame<-ttkframe(frame)
  cancel_button<-ttkbutton(button_frame, text = "Cancel")
  
  #Draw Ok Button
  ok_button<-ttkbutton(button_frame, text = "Ok", command = Okfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,fill = "y", side = "left")
  sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
  tkconfigure(ok_button, command = Okfun)
  tkconfigure(cancel_button, command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# Readland Function
readland_fun<-function(){
  #Data Model: enviroment called e
  e<-new.env()
  e$datatype<-tclVar(".nts")
  
  #Ok Function
  Okfun<-function() {
    switch(tclvalue(e$datatype),
           .nts = {name <- tclvalue(tkgetOpenFile(
             filetypes = "{{NTS Files} {.nts}}"))
           if (name == "") return(data.frame());
           data <- as.array(readland.nts(name))
           file.name <- sub(x = basename(name), pattern = ".nts",replacement = "",ignore.case = TRUE)
           pos<-1
           envir <- as.environment(pos)
           assign(file.name, data, envir = envir)},
           .tps = {name <- tclvalue(tkgetOpenFile(
             filetypes = "{{TPS Files} {.tps}}"))
           if (name == "") return(data.frame());
           data <- as.array(readland.tps(name))
           file.name <- sub(x = basename(name), pattern = ".tps",replacement = "",ignore.case = TRUE)
           pos<-1
           envir <- as.environment(pos)
           assign(file.name, data, envir = envir)},
           multi.nts = {name <- (choose.files(multi = TRUE, filters = as.matrix(c("NTS Files (*.nts)", "*.nts"))))
           if (name == "") return(data.frame());
           data <- as.array(readmulti.nts(name))
           pos<-1
           envir <- as.environment(pos)
           assign(x = "collective.array", value = data, envir = envir)}
    )
    tkdestroy(window)
  }
  #Begin GUI Setup
  window<-tktoplevel()
  tkwm.title(window, "readland_fun")
  frame<-ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame<-ttklabelframe(frame, text = "Read Landmark", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label<-function(parent, text, row, column, sticky, width) {
    label<-ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = sticky)
  }
  #Data Type Radiobuttons
  put_label(label_frame, "Type:", 1,0, sticky="w")
  rb_frame<-ttkframe(label_frame)
  sapply(c(".nts", ".tps", "multi.nts"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$datatype, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 1, column = 1, sticky = "w")
  
  #Draw Cancel Button
  button_frame<-ttkframe(frame)
  cancel_button<-ttkbutton(button_frame, text = "Cancel")
  
  #Draw Ok Button
  ok_button<-ttkbutton(button_frame, text = "Ok", command = Okfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,fill = "y", side = "left")
  sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
  tkconfigure(ok_button, command = Okfun)
  tkconfigure(cancel_button, command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# Multivariate Vector-fitting Function
multivarfit_fun<-function(){
  
  
  matrix.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(matrix) if(class(get(matrix))[1] == "matrix")c(unlist(matrix)))), "Load User File")
  
  #Data Model: enviroment called "e"
  e<-new.env()
  e$dataname<-tclVar("Choose Ordination")
  e$dataname2<-tclVar("Choose Variabales")
  e$dataframe<-tclVar("NULL")
  e$dataframe2<-tclVar("NULL")
  e$permutations<-tclVar(999)
  e$choice<-tclVar(2)
  
  #Ok function
  Okfun<-function(){
    if(sum(sapply(e$dataframe, is.numeric))==FALSE) {
      print("First matrix must be numeric!")
      tkdestroy(window)
      multivarfit_fun()
    }
    fit_value <- envfit(ord = e$dataframe,  env=e$dataframe2, choices= c(1:as.numeric(tclvalue(e$choice))), permutations=as.numeric(tclvalue(e$permutations)),na.rm = TRUE) 
    #    out<-fit_value
    
    #Plotting 
    plot(e$dataframe[,1], e$dataframe[,2], type="n",xlab="PCoA I",ylab="PCoA II")
    text(e$dataframe[,1], e$dataframe[,2], label=row.names(e$dataframe), cex=.55,col="gray")
    plot(fit_value,col="red",lwd=3,cex=1.3)
    out<-fit_value
    pos<-1
    envir <- as.environment(pos)
    assign("Vectorfit_Results", out, envir = envir)
    print(out)
    tkdestroy(window)
  }
  #Begin GUI setup
  window<-tktoplevel()
  tkwm.title(window, "zooaRch")
  frame<-ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame<-ttklabelframe(frame, text = "Multivariate Association Using Vector Fitting", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label<-function(parent, text, row, column, sticky, width) {
    label<-ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = sticky)
  }
  #First Data Combobox
  put_label(label_frame, "Ordination:",0,0,sticky="w")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = c(matrix.fun()[-length(matrix.fun())],dfs.fun()), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo)
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Second Data Combobox
  put_label(label_frame, "Variables:",2,0,sticky="w") 
  data_combo2 <- ttkcombobox(label_frame, state = "readonly", 
                             values = c(matrix.fun()[-length(matrix.fun())],dfs.fun()), 
                             textvariable = e$dataname2)
  tkgrid(data_combo2, row = 2, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo2)                      
  
  tkwait.variable (e$dataname2)
  
  dataname2 <- tclvalue(e$dataname2)
  if (dataname2 == "Load User File"){
    dataframe2 <- getcsvfile()
    assign("dataframe2", dataframe2, envir = e)
  }
  if(dataname2 != "Load User File"){
    dataframe2 <- get(dataname2, .GlobalEnv)
    assign("dataframe2", dataframe2, envir = e)
  }
  #Choice Combobox
  put_label(label_frame, "N Axes:", 4, 0, sticky = "w")
  choice_combo<-ttkcombobox(label_frame, state = "readonly", values = (2:ncol(e$dataframe)), textvariable = e$choice)
  tkgrid(choice_combo, row = 4, column = 1, sticky = "w", padx = 2)
  tkfocus(choice_combo)
  
  #Permutation Slider
  put_label(label_frame, "Permutations:", 7, 0, sticky = "w")
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 7, column = 1, columnspan = 2, 
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame, 
                               from = 99, to = 10000,  
                               variable = e$permutations)
  #Permutation Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 99, to = 10000, increment = 1, 
                               textvariable = e$permutations, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
  
  #Draw Cancel Button
  button_frame<-ttkframe(frame)
  cancel_button<-ttkbutton(button_frame, text = "Cancel")
  
  #Draw Ok Button
  ok_button<-ttkbutton(button_frame, text = "Ok", command = Okfun)
  
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,fill = "y", side = "left")
  sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
  
  tkconfigure(ok_button, command = Okfun)
  tkconfigure(cancel_button, command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# Gpagen Function
gpagen_fun<-function(){
  array.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(array) if(class(get(array))[1] == "array")c(unlist(array)))), "Load User File")
  matrix.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(matrix) if(class(get(matrix))[1] == "matrix")c(unlist(matrix)))), "None","Load User File")
  
  #Data Model: enviroment called e
  e<-new.env()
  e$dataname<-tclVar("Choose Data")
  e$dataname2<-tclVar("Choose Data")
  e$dataname3<-tclVar("Choose Data")
  e$array<-tclVar("NULL")
  e$curve<-tclVar("NULL")
  e$surface<-tclVar("NULL")
  e$type<-tclVar("ProcD")
  e$progress<-tclVar(1)
  
  #Ok Function
  Okfun<-function() {
    if(tclvalue(e$type) == "ProcD"){
      e$type<-TRUE
    }
    else{
      e$type<-FALSE
    }
    Y.gpa <- gpagen(e$array, curves = e$curve, surfaces = e$surface, ProcD = e$type, print.progress = tclvalue(e$progress))
    summary(Y.gpa)
    plot(Y.gpa)
    pos<-1
    envir <- as.environment(pos)
    assign("gparesults", Y.gpa, envir = envir)
    tkdestroy(window)
  }
  #Begin GUI Setup
  window<-tktoplevel()
  tkwm.title(window, "gpagen_fun")
  frame<-ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame<-ttklabelframe(frame, text = "gpagen", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label<-function(parent, text, row, column, sticky, width) {
    label<-ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = sticky)
  }
  #Data Combobox
  put_label(label_frame, "Input Data:",0,0,sticky="w")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = array.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo)
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("array", dataframe, envir = e)
  }
  else{
    dataframe <- get(dataname, .GlobalEnv)
    assign("array", dataframe, envir = e)
  }
  #Curve Combobox
  put_label(label_frame, "Input Curve:",2,0,sticky="w")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = matrix.fun(), 
                            textvariable = e$dataname2)
  tkgrid(data_combo, row = 2, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo)
  
  tkwait.variable (e$dataname2)
  
  dataname <- tclvalue(e$dataname2)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("curve", dataframe, envir = e)
  }
  else if (dataname == "None"){
    dataframe <- NULL
    assign("curve", dataframe, envir = e)
  }
  else{
    dataframe <- get(dataname, .GlobalEnv)
    assign("curve", dataframe, envir = e)
  }
  #Surface Combobox
  put_label(label_frame, "Input Surface:",3,0,sticky="w")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = matrix.fun(), 
                            textvariable = e$dataname3)
  tkgrid(data_combo, row = 3, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo)
  
  tkwait.variable (e$dataname3)
  
  dataname <- tclvalue(e$dataname3)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("surface", dataframe, envir = e)
  }
  else if (dataname == "None"){
    dataframe <- NULL
    assign("surface", dataframe, envir = e)
  }
  else{
    dataframe <- get(dataname, .GlobalEnv)
    assign("surface", dataframe, envir = e)
  }
  #Bending or ProcD Radiobuttons
  put_label(label_frame, "Type:", 4,0, sticky="w")
  rb_frame<-ttkframe(label_frame)
  sapply(c("ProcD", "Bending Energy"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 4, column = 1, sticky = "w")
  
  #Plot Progress Checkbox
  put_label ( label_frame , "Plot Progress:" , 5 , 0, sticky = "w")
  progress_check <-ttkcheckbutton (label_frame , variable = e$progress)
  tkgrid (progress_check , row = 5 , column = 1 , sticky = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame<-ttkframe(frame)
  cancel_button<-ttkbutton(button_frame, text = "Cancel")
  
  #Draw Ok Button
  ok_button<-ttkbutton(button_frame, text = "Ok", command = Okfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,fill = "y", side = "left")
  sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
  tkconfigure(ok_button, command = Okfun)
  tkconfigure(cancel_button, command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# k-means Single
kmeans_single_fun<-function(){
  
  matrix.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(matrix) if(class(get(matrix))[1] == "matrix")c(unlist(matrix)))),"Load User File")
  df.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(matrix) if(class(get(matrix))[1] == "data.frame")c(unlist(matrix)))))
  #Data Model: enviroment called e
  e<-new.env()
  e$dataname<-tclVar("Choose Data")
  e$datamatrix<-tclVar("NULL")
  e$clusternum<-tclVar(2)
  e$plot<-tclVar("NULL")
  
  Okfun<-function() {
    
    #Numeric Error Checking
    #Using Selected Values
    Value_First <- input1[as.numeric(tkcurselection(list1)) + 1]
    e$datamatrix<-e$datamatrix[,Value_First]
    #Error Checking
    if(sum(sapply(e$datamatrix[,Value_First], is.numeric))==FALSE) { 
      print("First matrix must not contain characters!")
      tkdestroy(window)
      kmeans_single_fun
      stop(call. = FALSE)
    }
    if(sum(sapply(e$datamatrix, is.na))==TRUE) {
      print("The first data set is missing values!")
      tkdestroy(window)
      kmeans_single_fun
      stop(call. = FALSE)
    }
    #K-means
    pc<-prcomp(e$datamatrix)$x
    kclusters<-kmeans(pc,as.numeric(tclvalue(e$clusternum)))
    
    ## Plot
    if(tclvalue(e$plot) == 1){
      plot(prcomp(e$datamatrix)$x[,1:2],col=kclusters$cluster,pch=19)
      points(kclusters$centers, col = 1:as.numeric(tclvalue(e$clusternum)), pch = 8, cex=2)
    }
    kclusters$PCA<-pc
    pos<-1
    envir <- as.environment(pos)
    assign("SingleKmeans", kclusters, envir = envir) 
    tkdestroy(window)
  }
  #Begin GUI Setup
  window<-tktoplevel()
  tkwm.title(window, "zooaRch")
  frame<-ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame<-ttklabelframe(frame, text = "Single K-Means Clustering", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label<-function(parent, text, row, column, sticky, width) {
    label<-ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = sticky)
  }
  #Data Combobox
  put_label(label_frame, "Input Data:",1,0,sticky="w")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = c(matrix.fun(),df.fun()), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 1, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo)
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("datamatrix", dataframe, envir = e)
  }
  else{
    dataframe <- get(dataname, .GlobalEnv)
    assign("datamatrix", dataframe, envir = e)
  }
  
  #First Dataframe Listbox
  list1 <- tk2listbox(label_frame, height = 4, selectmode = "extended")
  put_label(label_frame, "Variables:",row=2,column=0,sticky="w")
  tkgrid(list1, padx = 10, pady = c(5, 10),row = 2, column = 1,sticky="w")
  input1 <- colnames(e$datamatrix)
  for (inputs in input1){
    tkinsert(list1, "end", inputs)
  }
  tkselection.set(list1, 0)
  
  #Number of Clusters Spinbox
  put_label(label_frame, "Number of Clusters:", 3, 0, sticky = "w")
  conf_level_frame <- ttkframe(label_frame)
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 3, column = 1, columnspan = 2, 
         sticky = "w")
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 2, to = 20, increment = 1, 
                               textvariable = e$clusternum, width = 5)
  tkpack(conf_level_spin, side = "left")
  
  #Scree Plot Checkbox
  put_label ( label_frame , "Plot:" , 4 , 0, sticky = "w")
  progress_check <-ttkcheckbutton (label_frame , variable = e$plot)
  tkgrid (progress_check , row = 4 , column = 1 , sticky = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame<-ttkframe(frame)
  cancel_button<-ttkbutton(button_frame, text = "Cancel")
  
  #Draw Ok Button
  ok_button<-ttkbutton(button_frame, text = "Ok", command = Okfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,fill = "y", side = "left")
  sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
  tkconfigure(ok_button, command = Okfun)
  tkconfigure(cancel_button, command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# k-means Multi
kmeans_multi_fun<-function(){
  
  matrix.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(matrix) if(class(get(matrix))[1] == "matrix")c(unlist(matrix)))),"Load User File")
  df.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(matrix) if(class(get(matrix))[1] == "data.frame")c(unlist(matrix)))))
  
  #Data Model: enviroment called e
  e<-new.env()
  e$dataname<-tclVar("Choose Data")
  e$datamatrix<-tclVar("NULL")
  e$clustermin<-tclVar(1)
  e$clustermax<-tclVar(5)
  e$iterations<-tclVar(1000)
  e$nstart<-tclVar(100)
  e$verbose<-tclVar("NULL")
  e$plot<-tclVar("NULL")
  
  Okfun<-function() {
    #Numeric Error Checking
    #Using Selected Values
    Value_First <- input1[as.numeric(tkcurselection(list1)) + 1]
    e$datamatrix<-e$datamatrix[,Value_First]
    #Error Checking
    if(sum(sapply(e$datamatrix, is.numeric))==FALSE) { 
      print("First matrix must not contain characters!")
      tkdestroy(window)
      kmeans_single_fun
      stop(call. = FALSE)
    }
    if(sum(sapply(e$datamatrix, is.na))==TRUE) {
      print("The first data set is missing values!")
      tkdestroy(window)
      kmeans_single_fun
      stop(call. = FALSE)
    }
    ##Test Kmeans' TESS
    pc<-prcomp(e$datamatrix)$x
    tess.all<-NULL
    for (i in as.numeric(tclvalue(e$clustermin)):as.numeric(tclvalue(e$clustermax)))    	{
      tess.temp<-NULL
      km<-kmeans(pc,i,iter.max = as.numeric(tclvalue(e$iterations)), 
                 nstart= as.numeric(tclvalue(e$nstart)))
      tess.temp<-sum(km$withinss)
      tess.all<-c(tess.all,tess.temp)
      if(tclvalue(e$verbose)=="1"){
        cat(i,"Cluster", "Within SS = ",tess.temp,"\n")
      }
    }
    out<-as.matrix(cbind(1:length(tess.all),tess.all))
    colnames(out)<-c("Nclusters","WithinSS")
    pos<-1
    envir <- as.environment(pos)
    assign("MultiKmeans", out, envir = envir) 
    
    ## Scree plot
    if(tclvalue(e$plot) == "1"){
      plot(1:length(tess.all),tess.all,type="l",axes=T,bty="l",lty=2,
           xlab="Number of Clusters",ylab="Sum of Squared Errors",pch=19,
           xlim=c(1,length(tess.all)),ylim=c(0,range(tess.all)[2])
      )}
    tkdestroy(window)
  }
  #Begin GUI Setup
  window<-tktoplevel()
  tkwm.title(window, "kmeans_single_fun")
  frame<-ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame<-ttklabelframe(frame, text = "Single K-Means Clustering", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label<-function(parent, text, row, column, sticky, width) {
    label<-ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = sticky)
  }
  #Data Combobox
  put_label(label_frame, "Input Data:",1,0,sticky="w")
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = c(matrix.fun(),df.fun()), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 1, column = 1, sticky="w", padx = 2)
  tkfocus(data_combo)
  
  tkwait.variable (e$dataname)
  
  dataname <- tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("datamatrix", dataframe, envir = e)
  }
  else{
    dataframe <- get(dataname, .GlobalEnv)
    assign("datamatrix", dataframe, envir = e)
  }
  #First Dataframe Listbox
  list1 <- tk2listbox(label_frame, height = 4, selectmode = "extended")
  put_label(label_frame, "Variables:",row=2,column=0,sticky="w")
  tkgrid(list1, padx = 10, pady = c(5, 10),row = 2, column = 1,sticky="w")
  input1 <- colnames(e$datamatrix)
  for (inputs in input1){
    tkinsert(list1, "end", inputs)
  }
  tkselection.set(list1, 0)
  
  #Minimum Clusters Spinbox
  put_label(label_frame, "Min Clusters:", 6, 0, sticky = "w")
  conf_level_frame <- ttkframe(label_frame)
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 6, column = 1, columnspan = 2, 
         sticky = "w")
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 1, to = 20, increment = 1, 
                               textvariable = e$clustermin, width = 5)
  tkpack(conf_level_spin, side = "left")
  
  #Maximum Clusters Spinbox
  put_label(label_frame, "Max Clusters:", 7, 0, sticky = "w")
  conf_level_frame <- ttkframe(label_frame)
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 7, column = 1, columnspan = 2, 
         sticky = "w")
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 1, to = 50, increment = 1, 
                               textvariable = e$clustermax, width = 5)
  tkpack(conf_level_spin, side = "left")
  
  #Maximum Clusters Spinbox
  put_label(label_frame, "Max Iterations:", 8, 0, sticky = "w")
  conf_level_frame <- ttkframe(label_frame)
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 8, column = 1, columnspan = 2, 
         sticky = "w")
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 1, to = 2000, increment = 1, 
                               textvariable = e$iterations, width = 5)
  tkpack(conf_level_spin, side = "left")
  
  #nstart Spinbox
  put_label(label_frame, "nstart:", 9, 0, sticky = "w")
  conf_level_frame <- ttkframe(label_frame)
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 9, column = 1, columnspan = 2, 
         sticky = "w")
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 1, to = 200, increment = 1, 
                               textvariable = e$nstart, width = 5)
  tkpack(conf_level_spin, side = "left")
  
  #Verbose Checkbox
  put_label ( label_frame , "Verbose:" , 10 , 0, sticky = "w")
  progress_check <-ttkcheckbutton (label_frame , variable = e$verbose)
  tkgrid (progress_check , row = 10 , column = 1 , sticky = "w" ,padx = 2)
  
  #Scree Plot Checkbox
  put_label ( label_frame , "Scree Plot:" , 11 , 0, sticky = "w")
  progress_check <-ttkcheckbutton (label_frame , variable = e$plot)
  tkgrid (progress_check , row = 11 , column = 1 , sticky = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame<-ttkframe(frame)
  cancel_button<-ttkbutton(button_frame, text = "Cancel")
  
  #Draw Ok Button
  ok_button<-ttkbutton(button_frame, text = "Ok", command = Okfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,fill = "y", side = "left")
  sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
  tkconfigure(ok_button, command = Okfun)
  tkconfigure(cancel_button, command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
  tkfocus(window)
}

# Bayesian functions # all bayesian code from Baath 2016, Bayesian first aid unpublished package https://github.com/rasmusab/bayesian_first_aid
# Bayesian onesam_tfun # all bayesian code from Baath 2016, Bayesian first aid unpublished package https://github.com/rasmusab/bayesian_first_aid
Bayes_onesam_tfun<-function(){
  
  
  #Data Model: environment called "e"
  e <- new.env()
  e$dataname <- tclVar("Choose one"); 
  e$dataframe <- tclVar("NULL"); 
  e$variable1 <- tclVar("Choose one");
  e$alternative <- tclVar("two.sided"); 
  #e$paired<- tclVar(FALSE);
  #e$var <- tclVar(FALSE); 
  e$conf.level <- tclVar(95); 
  e$mu <- tclVar(0);
  
  #Ok Function
  OKfun <- function() {
    variable1name<-tclvalue(e$variable1)
    variable1 <- get(variable1name,e$dataframe,inherits=TRUE)
    assign("variable1", variable1, envir = e)
    conf.level<-NULL
    conf.level<-as.numeric(tclvalue(e$conf.level))*.01
    assign("conf.level", conf.level, envir = e)
    altselect <- tclvalue(e$alternative)
    alternative <- switch(altselect , "two.sided" = altselect , "less" = altselect , "greater" = altselect )
    assign("alternative", altselect, envir = e)
    out<- bayes.t.test(data= e$dataframe, x=variable1, mu=as.numeric(tclvalue(e$mu)), cred.mass = e$conf.level)
    out$data.name<-paste(variable1name)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    print(out)
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "zooaRch")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Bayesian One Sample t-test", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = "e")
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0)
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable(e$dataname)
  
  dataname <-tclvalue(e$dataname)
  if (dataname == "Load User File"){
    dataframe <- getcsvfile()
    assign("dataframe", dataframe, envir = e)
  }
  if(dataname != "Load User File"){
    dataframe <- get(dataname, .GlobalEnv)
    assign("dataframe", dataframe, envir = e)
  }
  #Variable Combobox
  put_label(label_frame, "Variable:",1,0)
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = sort(colnames(e$dataframe)), 
                              textvariable = e$variable1)
  tkgrid(data_labels1, row = 1, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #Mu Entry
  put_label(label_frame, "Mu:",2,0)
  data_labels2 <- ttkentry(label_frame,
                           textvariable = e$mu, width=5)
  tkgrid(data_labels2, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  
  #Conf.level Slider
  put_label(label_frame, "cred.mass:", 3, 0)
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 3, column = 1, columnspan = 2, 
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame, 
                               from = 75, to = 100,  
                               variable = e$conf.level)
  #Conf.level Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 60, to = 100, increment = 1, 
                               textvariable = e$conf.level, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
  
  #  #Function Radiobutton
  #  put_label(label_frame, "Alternative:",4,0)
  #  rb_frame <- ttkframe(label_frame)
  #  sapply(c("two.sided","less","greater"), function(i) {
  #    radio_button <- tk2radiobutton(rb_frame, variable = e$alternative,text = i, value = i)
  #    tkpack(radio_button, side = "left")
  #  })
  #  tkgrid(rb_frame, row = 4, column = 1, sticky = "w")
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")               # add a spring
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
}

# Bayes_2tfun
Bayes_2tfun<-function(){
  
  
  #Data Model: environment called "e"
  e <- new.env()
  e$data <- tclVar("Choose one"); 
  e$variable1 <- tclVar("Choose one");
  e$variable2 <- tclVar("Choose one");
  e$alternative <- tclVar("two.sided"); 
  e$paired<- tclVar(FALSE);
  e$var <- tclVar(FALSE); 
  e$conf.level <- tclVar(95); 
  
  #Ok Function
  OKfun <- function() {
    variable1name<-tclvalue(e$variable1)
    variable1 <- get(variable1name,e$data,inherits=TRUE)
    assign("variable1", variable1, envir = e)
    variable2name<-tclvalue(e$variable2)
    variable2 <- get(variable2name,e$data,inherits=TRUE)
    assign("variable2", variable2, envir = e)
    paired <- as.numeric(tclvalue(e$paired))
    assign("paired", paired, envir = e)
    var<-as.numeric(tclvalue(e$var))
    assign("var", var, envir = e)
    conf.level<-NULL
    conf.level<-as.numeric(tclvalue(e$conf.level))*.01
    assign("conf.level", conf.level, envir = e)
    altselect <- tclvalue(e$alternative)
    alternative <- switch(altselect , "two.sided" = altselect , "less" = altselect , "greater" = altselect )
    assign("alternative", altselect, envir = e)
    out<-bayes.t.test(data= e$data, x=e$variable1, y=e$variable2, cred.mass = e$conf.level, paired = e$paired, var.equal = e$var)
    out$data.name<-paste(variable1name, "and", variable2name)
    pos<-1
    envir <- as.environment(pos)
    assign("Bayes_t_Results", out, envir = envir) 
    print(out)
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "zooaRch")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "2-Sample Bayesian t-test", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = "e")
  }
  #Data Combobox
  put_label(label_frame, "Data:",0,0)
  data_combo <- ttkcombobox(label_frame, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$data)
  tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
  tkfocus(data_combo)                      # give focus
  
  tkwait.variable(e$data)
  
  data <- tclvalue(e$data)
  if (data == "Load User File"){
    data <- getcsvfile()
    assign("data", data, envir = e)
  }
  if(data != "Load User File"){
    data <- get(data, .GlobalEnv)
    assign("data", data, envir = e)
  }
  #Variable 1 Combobox
  put_label(label_frame, "Variable 1:",1,0)
  data_labels1 <- ttkcombobox(label_frame, state = "readonly", 
                              values = colnames(e$data), 
                              textvariable = e$variable1)
  tkgrid(data_labels1, row = 1, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels1) 
  
  #Variable 2 Combobox
  put_label(label_frame, "Variable 2:",2,0)
  data_labels2 <- ttkcombobox(label_frame, state = "readonly", 
                              values = colnames(e$data), 
                              textvariable = e$variable2)
  tkgrid(data_labels2, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  
  #Conf.level Slider
  put_label(label_frame, "cred. mass:", 3, 0)
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 3, column = 1, columnspan = 2, 
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame, 
                               from = 60, to = 100,  
                               variable = e$conf.level)
  #Conf.level Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 60, to = 100, increment = 1, 
                               textvariable = e$conf.level, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
  
  #  #Function Radiobutton
  #  put_label(label_frame, "Alternative:",4,0)
  #  rb_frame <- ttkframe(label_frame)
  #  sapply(c("two.sided","less","greater"), function(i) {
  #    radio_button <- tk2radiobutton(rb_frame, variable = e$alternative,text = i, value = i)
  #    tkpack(radio_button, side = "left")
  #  })
  #  tkgrid(rb_frame, row = 4, column = 1, sticky = "w")
  
  #Equal Variance Checkbox
  #  put_label ( label_frame , "var.equal:" , 5 , 0)
  #  var_equal_check <-ttkcheckbutton ( label_frame , variable = e$var)
  #  tkgrid ( var_equal_check , row = 5 , column = 1 , stick = "w" ,padx = 2)
  
  #Paired or Independent Checkbox
  put_label ( label_frame , "paired:" , 6 , 0)
  paired_check <-ttkcheckbutton (label_frame , variable = e$paired)
  tkgrid (paired_check , row = 6 , column = 1 , stick = "w" ,padx = 2)
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")               # add a spring
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
}

# Bayes_binom_fun
Bayes_binom_fun<-function(){
  
  #Data Model: environment called "e"
  e <- new.env()
  e$x <- tclVar("0"); 
  e$n <- tclVar("0"); 
  e$p <- tclVar(.5);
  e$conf.level <- tclVar(95); 
  
  #Ok Function
  OKfun <- function() {
    conf.level<-NULL
    conf.level<-as.numeric(tclvalue(e$conf.level))*.01
    assign("conf.level", conf.level, envir = e)
    out<- bayes.binom.test(x=as.numeric(tclvalue(e$x)), n=as.numeric(tclvalue(e$n)), comp.theta = as.numeric(tclvalue(e$p)),
                           cred.mass = e$conf.level, n.iter = 15000, progress.bar = "none")
    out$data_name<-"binomial test"
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    print(out)
    tkdestroy(window)
  }
  #Begin GUI Setup
  window <- tktoplevel()
  tkwm.title(window, "zooaRch")
  frame <- ttkframe(window, padding = c(3,3,12,12))
  tkpack(frame, expand = TRUE, fill = "both")
  
  #Layout
  label_frame <- ttklabelframe(frame, text = "Bayesian binomial test", padding = 10)
  tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
  tkgrid.columnconfigure(label_frame, 0, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  tkgrid.columnconfigure(label_frame, 2, weight = 1)
  tkgrid.columnconfigure(label_frame, 1, weight = 1)
  put_label <- function(parent, text, row, column) {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = "e")
  }
  #x Entry
  put_label(label_frame, "Successes:",2,0)
  data_labels2 <- ttkentry(label_frame,
                           textvariable = e$x, width=5)
  tkgrid(data_labels2, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  
  #n Entry
  put_label(label_frame, "Trials:",3,0)
  data_labels2 <- ttkentry(label_frame,
                           textvariable = e$n, width=5)
  tkgrid(data_labels2, row = 3, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels2)
  
  #Comp Entry
  put_label(label_frame, "Comp prop:",4,0)
  data_labels2 <- ttkentry(label_frame,
                           textvariable = e$p, width=5)
  tkgrid(data_labels2, row = 4, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels2)
  
  #Conf.level Slider
  put_label(label_frame, "cred.mass:", 5, 0)
  conf_level_frame <- ttkframe(label_frame)
  tkgrid(conf_level_frame, row = 5, column = 1, columnspan = 2, 
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame, 
                               from = 60, to = 100,  
                               variable = e$conf.level)
  #Conf.level Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame, 
                               from = 60, to = 100, increment = 1, 
                               textvariable = e$conf.level, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
  
  #Draw Cancel Button
  button_frame <- ttkframe(frame)
  cancel_button <- ttkbutton(button_frame, text = "cancel")
  
  #Draw Ok Button
  ok_button <- ttkbutton(button_frame, text = "ok",command = OKfun)
  tkpack(button_frame, fill = "x", padx = 5, pady = 5)
  tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
         fill = "y", side = "left")               # add a spring
  sapply(list(cancel_button, ok_button), tkpack, 
         side = "left", padx = 6)
  tkconfigure(ok_button, command = OKfun)
  tkconfigure(cancel_button, 
              command = function() tkdestroy(window))
  tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
}

# Bayes helper functions: # all bayesian code from Baath 2016, Bayesian first aid unpublished package https://github.com/rasmusab/bayesian_first_aid
HDIofMCMC<-function( sampleVec , credMass=0.95 ) {
  # code from Bayesianfirst aid unpublished package https://github.com/rasmusab/bayesian_first_aid
  # Computes highest density interval from a sample of representative values,
  #   estimated as shortest credible interval.
  # Arguments:
  #   sampleVec
  #     is a vector of representative values from a probability distribution.
  #   credMass
  #     is a scalar between 0 and 1, indicating the mass within the credible
  #     interval that is to be estimated.
  # Value:
  #   HDIlim is a vector containing the limits of the HDI
  sortedPts = sort( sampleVec )
  ciIdxInc = floor( credMass * length( sortedPts ) )
  nCIs = length( sortedPts ) - ciIdxInc
  ciWidth = rep( 0 , nCIs )
  for ( i in 1:nCIs ) {
    ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
  }
  HDImin = sortedPts[ which.min( ciWidth ) ]
  HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
  HDIlim = c( HDImin , HDImax )
  return( HDIlim )
}
reorder_coda<-function(s, param_order) {
  ## code by Baath 2016: Bayesianfirst aid unpublished package https://github.com/rasmusab/bayesian_first_aid
  s <- lapply(s, function(chain) {
    chain[, order(match(gsub("\\[.+\\]$", "", colnames(chain)), param_order))]
  })
  mcmc.list(s)
}
diagnostics.bayes_binom_test<-function(fit) {
  ## code by Baath 2016: Bayesianfirst aid unpublished package https://github.com/rasmusab/bayesian_first_aid
  print_mcmc_info(fit$mcmc_samples)  
  cat("\n")
  print_diagnostics_measures(round(fit$stats, 3))
  cat("\n")
  cat("  Model parameters and generated quantities\n")
  cat("theta: The relative frequency of success\n")
  cat("x_pred: Predicted number of successes in a replication\n")
  old_par <- par( mar=c(3.5,2.5,2.5,0.6) , mgp=c(2.25,0.7,0) )
  plot(fit$mcmc_samples)
  par(old_par)
  invisible(NULL)
}
diagnostics.bayes_one_sample_t_test<-function(fit) {
  print_mcmc_info(fit$mcmc_samples)  
  cat("\n")
  print_diagnostics_measures(round(fit$stats, 3))
  cat("\n")
  print_bayes_one_sample_t_test_params(fit)
  cat("\n")
  old_par <- par( mar=c(3.5,2.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
  plot(fit$mcmc_samples)
  par(old_par)
  invisible(NULL)
}
diagnostics.bayes_paired_t_test<-function(fit) {
  print_mcmc_info(fit$mcmc_samples)  
  cat("\n")
  print_diagnostics_measures(round(fit$stats, 3))
  cat("\n")
  print_bayes_paired_t_test_params(fit)
  cat("\n")
  old_par <- par( mar=c(3.5,2.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
  plot(fit$mcmc_samples)
  par(old_par)
  invisible(NULL)
}
diagnostics.bayes_two_sample_t_test<-function(fit) {
  print_mcmc_info(fit$mcmc_samples)  
  cat("\n")
  print_diagnostics_measures(round(fit$stats, 3))
  cat("\n")
  print_bayes_two_sample_t_test_params(fit)
  cat("\n")
  old_par <- par( mar=c(3.5,2.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
  plot(fit$mcmc_samples)
  par(old_par)
  invisible(NULL)
}
binom_model_string<-"model {\n  x ~ dbinom(theta, n)\n  theta ~ dbeta(1, 1)\n  x_pred ~ dbinom(theta, n)\n}"
mcmc_stats<-function(samples, cred_mass = 0.95, comp_val = 0) {
  ## code Baath 2016: Bayesianfirst aid unpublished package https://github.com/rasmusab/bayesian_first_aid
  samples_mat <- as.matrix(samples)
  stats <- data.frame(mean = colMeans(samples_mat))
  stats$sd <- apply(samples_mat, 2, sd)
  cred_mass <- rep(cred_mass, length.out = ncol(samples_mat))
  comp_val <- rep(comp_val, length.out = ncol(samples_mat))
  stats$"HDI%" <- cred_mass * 100
  stats$comp <- comp_val
  stats$HDIlo <- NA
  stats$HDIup <- NA
  for(i in 1:ncol(samples_mat)){
    hdi_lim <- HDIofMCMC(samples_mat[,i], credMass=cred_mass[i])
    stats$HDIlo[i] <- hdi_lim[1]
    stats$HDIup[i] <- hdi_lim[2]  
    stats$"%>comp"[i] <- mean(c(samples_mat[,i] > comp_val[i], 0, 1))
    stats$"%<comp"[i] <- mean(c(samples_mat[,i] < comp_val[i], 0, 1))
  }
  stats$"q2.5%" <- apply(samples_mat, 2, quantile,  probs= 0.025)
  stats$"q25%" <- apply(samples_mat, 2, quantile,  probs= 0.25)
  stats$median <- apply(samples_mat, 2, median)
  stats$"q75%" <- apply(samples_mat, 2, quantile,  probs= 0.75)
  stats$"q97.5%" <- apply(samples_mat, 2, quantile,  probs= 0.975)
  stats$mcmc_se <- NA
  stats$Rhat <- NA
  stats$n_eff <- NA
  if(is.mcmc.list(samples)) {
    stats$mcmc_se <- summary(samples)$statistics[,"Time-series SE"]
    stats$Rhat <- gelman.diag(samples, multivariate = FALSE)$psrf[, 1]
    stats$n_eff <- as.integer(effectiveSize(samples))
  }
  as.matrix(stats) # 'cause it's easier to index
}
run_jags<-function(model_string, data, inits, params, n.chains, n.adapt, n.update, n.iter, thin, progress.bar) {
  ## code by Baath 2016: Bayesianfirst aid unpublished package https://github.com/rasmusab/bayesian_first_aid
  if(!interactive()) {
    progress.bar <- "none"
  }
  # Set the random number generator and seed based on R's random state (through runif)
  if(is.null(inits$.RNG.seed) & is.null(inits$.RNG.name)) {
    RNGs <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", 
              "base::Super-Duper", "base::Mersenne-Twister")
    init_list <- inits
    inits <- function(chain) {
      chain_init_list <- init_list
      chain_init_list$.RNG.seed <- as.integer(runif(1, 0, .Machine$integer.max))
      chain_init_list$.RNG.name <- RNGs[ ((chain - 1) %% 4) +  1 ]
      chain_init_list
    }
  }
  jags_model <- jags.model(textConnection(model_string) , data=data , inits=inits , 
                           n.chains=n.chains , n.adapt=0, quiet=TRUE)
  adapt(jags_model, max(1, n.adapt),  progress.bar="none", end.adaptation=TRUE)
  if(n.update > 0) { 
    update( jags_model, n.update, progress.bar="none")
  }
  mcmc_samples <- coda.samples( jags_model , variable.names= params,
                                n.iter=n.iter, thin=thin, progress.bar=progress.bar)
  mcmc_samples <- reorder_coda(mcmc_samples, params)
  mcmc_samples
}
jags_binom_test<-function(x, n, n.chains=3, n.iter=5000, progress.bar="none") {
  ## code by Baath 2016: Bayesianfirst aid unpublished package https://github.com/rasmusab/bayesian_first_aid
  mcmc_samples <- run_jags(binom_model_string, data = list(x = x, n = n), inits = list(theta = (x + 1) / (n + 2)), 
                           params = c("theta", "x_pred"), n.chains = n.chains, n.adapt = 0,
                           n.update = 0, n.iter = n.iter, thin = 1, progress.bar=progress.bar)
  mcmc_samples
}
bayes.binom.test<-function (x, n, comp.theta = 0.5, alternative = NULL, cred.mass = 0.95, n.iter=15000, progress.bar="none", p, conf.level) {
  ## code by Baath 2016: Bayesianfirst aid unpublished package https://github.com/rasmusab/bayesian_first_aid
  if(! missing(alternative)) {
    warning("The argument 'alternative' is ignored by bayes.binom.test")
  }
  if(! missing(p)) {
    comp.theta <- p
  }
  if(! missing(conf.level)) {
    cred.mass <- conf.level
  }
  ### Begin code from binom.test 
  x_name <- deparse(substitute(x))
  DNAME <- x_name
  n_name <- deparse(substitute(n))
  xr <- round(x)
  if (any(is.na(x) | (x < 0)) || max(abs(x - xr)) > 1e-07) 
    stop("'x' must be nonnegative and integer")
  x <- xr
  if (length(x) == 2L) {
    n <- sum(x)
    x <- x[1L]
  }
  else if (length(x) == 1L) {
    nr <- round(n)
    if ((length(n) > 1L) || is.na(n) || (n < 1) || abs(n - 
                                                       nr) > 1e-07 || (x > nr)) 
      stop("'n' must be a positive integer >= 'x'")
    DNAME <- paste(x_name, "and", n_name)
    n <- nr
  }
  else stop("incorrect length of 'x'")
  if (!missing(comp.theta) && (length(comp.theta) > 1L || is.na(comp.theta) || comp.theta < 0 || 
                               comp.theta > 1)) 
    stop("'comp.theta' or 'p' must be a single number between 0 and 1")
  if (!((length(cred.mass) == 1L) && is.finite(cred.mass) && 
        (cred.mass > 0) && (cred.mass < 1))) 
    stop("'cred.mass' or 'conf.level' must be a single number between 0 and 1")
  ### END code from binom.test
  mcmc_samples <- jags_binom_test(x, n, n.chains = 3, n.iter = ceiling(n.iter / 3) , progress.bar=progress.bar)
  stats <- mcmc_stats(mcmc_samples, cred_mass = cred.mass, comp_val = comp.theta)
  bfa_object <- list(x = x, n = n, comp_theta = comp.theta, cred_mass = cred.mass,
                     x_name = x_name, n_name = n_name, data_name = DNAME,
                     mcmc_samples = mcmc_samples, stats = stats) 
  class(bfa_object) <- c("bayes_binom_test", "bayesian_first_aid")
  bfa_object
}
print.bayes_binom_test<-  function(x, ...) {
  s <- format_stats(x$stats)["theta",]
  cat("\n")
  cat("\tBayesian binomial test\n")
  cat("\n")
  cat("data: ", x$data_name, "\n", sep="")
  cat("number of successes = ", x$x,", number of trials = ", x$n, "\n", sep="")
  cat("Estimated relative frequency of success:\n")
  cat(" ", s["median"], "\n")
  cat(s["HDI%"],"% credible interval:\n", sep="")
  cat(" ", s[ c("HDIlo", "HDIup")], "\n")
  cat("The relative frequency of success is more than", s["comp"] , "by a probability of", s["%>comp"], "\n")
  cat("and less than", s["comp"] , "by a probability of", s["%<comp"], "\n")
  cat("\n")
  invisible(NULL)
}
print.bayes_one_sample_t_test<-function(x, ...) {
  s <- format_stats(x$stats)
  cat("\n")
  cat("\tBayesian t test (BEST) - one sample\n")
  cat("\n")
  cat("data: ", x$data_name,  ", n = ", length(x$x),"\n", sep="")
  cat("\n")
  cat("  Estimates [", s[1, "HDI%"] ,"% credible interval]\n", sep="")
  cat("mean of ",  x$x_name, ": ", s["mu", "median"], " [", s["mu", "HDIlo"], ", ", s["mu", "HDIup"] , "]\n",sep="")
  cat("sd of ",  x$x_name, ": ", s["sigma", "median"], " [", s["sigma", "HDIlo"], ", ", s["sigma", "HDIup"] , "]\n",sep="")
  cat("\n")
  cat("The mean is more than", s["mu","comp"] , "by a probability of", s["mu","%>comp"], "\n")
  cat("and less than", s["mu", "comp"] , "by a probability of", s["mu", "%<comp"], "\n")
  cat("\n")
  invisible(NULL)
}
print.bayes_paired_t_test<-function(x, ...) {
  s <- format_stats(x$stats)
  cat("\n")
  cat("\tBayesian t test (BEST) - paired samples\n")
  cat("\n")
  cat("data: ", x$data_name,  ", n = ", length(x$pair_diff),"\n", sep="")
  cat("\n")
  cat("  Estimates [", s[1, "HDI%"] ,"% credible interval]\n", sep="")
  cat("mean paired difference: ", s["mu_diff", "median"], " [", s["mu_diff", "HDIlo"], ", ", s["mu_diff", "HDIup"] , "]\n",sep="")
  cat("sd of the paired differences: ", s["sigma_diff", "median"], " [", s["sigma_diff", "HDIlo"], ", ", s["sigma_diff", "HDIup"] , "]\n",sep="")
  cat("\n")
  cat("The mean difference is more than", s["mu_diff","comp"] , "by a probability of", s["mu_diff","%>comp"], "\n")
  cat("and less than", s["mu_diff", "comp"] , "by a probability of", s["mu_diff", "%<comp"], "\n")
  cat("\n")
  invisible(NULL)
}
print.bayes_two_sample_t_test<-function(x, ...) {
  s <- format_stats(x$stats)
  cat("\n")
  cat("\tBayesian t test (BEST) - two sample\n")
  cat("\n")
  cat("data: ", x$x_name, " (n = ", length(x$x) ,") and ", x$y_name," (n = ", length(x$y) ,")\n", sep="")
  cat("\n")
  cat("  Estimates [", s[1, "HDI%"] ,"% credible interval]\n", sep="")
  cat("mean of ",  x$x_name, ": ", s["mu_x", "median"], " [", s["mu_x", "HDIlo"], ", ", s["mu_x", "HDIup"] , "]\n",sep="")
  cat("mean of ",  x$y_name, ": ", s["mu_y", "median"], " [", s["mu_y", "HDIlo"], ", ", s["mu_y", "HDIup"] , "]\n",sep="")
  cat("difference of the means: ", s["mu_diff", "median"], " [", s["mu_diff", "HDIlo"], ", ", s["mu_diff", "HDIup"] , "]\n",sep="")
  cat("sd of ",  x$x_name, ": ", s["sigma_x", "median"], " [", s["sigma_x", "HDIlo"], ", ", s["sigma_x", "HDIup"] , "]\n",sep="")
  cat("sd of ",  x$y_name, ": ", s["sigma_y", "median"], " [", s["sigma_y", "HDIlo"], ", ", s["sigma_y", "HDIup"] , "]\n",sep="")
  cat("\n")
  cat("The difference of the means is greater than", s["mu_diff","comp"] , "by a probability of", s["mu_diff","%>comp"], "\n")
  cat("and less than", s["mu_diff", "comp"] , "by a probability of", s["mu_diff", "%<comp"], "\n")
  cat("\n")
  invisible(NULL)
}
format_stats<-function(s) {
  s_char <- apply(s, c(1,2), function(x) { sign_digits(x, 2) })
  s_char[, "comp"] <- round(s[, "comp"], 3)
  s_char[, "%>comp"] <- num_to_char_with_lim(s[, "%>comp"], 0.001, 0.999,  3)
  s_char[, "%<comp"] <- num_to_char_with_lim(s[, "%<comp"], 0.001, 0.999,  3)
  s_char
}
sign_digits<-function(x,d){
  s <- format(x,digits=d)
  if(grepl("\\.", s) && ! grepl("e", s)) {
    n_sign_digits <- nchar(s) - 
      max( grepl("\\.", s), attr(regexpr("(^[-0.]*)", s), "match.length") )
    n_zeros <- max(0, d - n_sign_digits)
    s <- paste(s, paste(rep("0", n_zeros), collapse=""), sep="")
  } else if(nchar(s) < d && ! grepl("e", s)) {
    s <- paste(s, ".", paste(rep("0", d - nchar(s)), collapse=""), sep="")
  }
  s
}
num_to_char_with_lim<-function(x, low, high, digits) {
  ifelse(x > high, paste(">", round(high, digits) , sep=""),
         ifelse(x < low, paste("<", round(low, digits), sep=""),
                as.character(round(x, digits))))
}
bayes.t.test<-function(x, ...) {
  UseMethod("bayes.t.test")
}
bayes.t.test.formula<-function(formula, data, subset, na.action, ...) {
  ### Original code from t.test.formula ###
  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]), "term.labels")) != 1L)) 
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())
  data_name <- paste(names(mf), collapse = " by ")
  response_name <- names(mf)[1]
  group_name <- names(mf)[2]
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  g <- factor(mf[[-response]])
  if (nlevels(g) != 2L) 
    stop("grouping factor must have exactly 2 levels")
  DATA <- setNames(split(mf[[response]], g), c("x", "y"))
  
  ### Own code starts here ###
  bfa_object <- do.call("bayes.t.test", c(DATA, list(...)))
  bfa_object$data_name <- data_name
  bfa_object$x_name <- paste("group", levels(g)[1])
  bfa_object$y_name <- paste("group", levels(g)[2])
  if(!missing(data)) {
    data_expr <- deparse(substitute(data))
    bfa_object$x_data_expr <- 
      paste("subset(", data_expr, ", as.factor(", group_name, ") == ",
            deparse(levels(g)[1]), ", ", response_name, ", drop = TRUE)", sep="")
    bfa_object$y_data_expr <- 
      paste("subset(", data_expr, ", as.factor(", group_name, ") == ",
            deparse(levels(g)[2]), ", ", response_name, ", drop = TRUE)", sep="")
  } else {
    bfa_object$x_data_expr <- 
      paste(response_name, "[", "as.factor(", group_name, ") == ",
            deparse(levels(g)[1]),"]",sep="")
    bfa_object$y_data_expr <- 
      paste(response_name, "[", "as.factor(", group_name, ") == ",
            deparse(levels(g)[2]),"]",sep="")
  }
  bfa_object  
}
bayes.t.test.default<-function(x, y = NULL, alternative = c("two.sided", "less", "greater"), 
                               mu = 0, paired = FALSE, var.equal = FALSE, cred.mass = 0.95, n.iter = 30000, progress.bar="text", conf.level,...) {
  if(! missing(conf.level)) {
    cred.mass <- conf.level
  }
  if(var.equal) {
    var.equal <- FALSE
    warning("To assume equal variance of 'x' and 'y' is not supported. Continuing by estimating the variance of 'x' and 'y' separately.")
  }
  if(! missing(alternative)) {
    warning("The argument 'alternative' is ignored by bayes.binom.test")
  } 
  ### Original (but slighly modified) code from t.test.default ###
  alternative <- match.arg(alternative)
  if (!missing(mu) && (length(mu) != 1 || is.na(mu))) 
    stop("'mu' must be a single number")
  if (!missing(cred.mass) && (length(cred.mass) != 1 || !is.finite(cred.mass) || 
                              cred.mass < 0 || cred.mass > 1)) 
    stop("'cred.mass' or 'conf.level' must be a single number between 0 and 1")
  
  # removing incomplete cases and preparing the data vectors (x & y)
  if (!is.null(y)) {
    x_name <- deparse(substitute(x))
    y_name <- deparse(substitute(y))
    data_name <- paste(x_name, "and", y_name)
    if (paired) 
      xok <- yok <- complete.cases(x, y)
    else {
      yok <- !is.na(y)
      xok <- !is.na(x)
    }
    y <- y[yok]
  }
  else {
    x_name <- deparse(substitute(x))
    data_name <- x_name
    if (paired) 
      stop("'y' is missing for paired test")
    xok <- !is.na(x)
    yok <- NULL
  }
  x <- x[xok]
  
  # Checking that there is enough data. Even though BEST handles the case with
  # one data point it is still usefull to do these checks.
  nx <- length(x)
  mx <- mean(x)
  vx <- var(x)
  if (is.null(y)) {
    if (nx < 2) 
      stop("not enough 'x' observations")
    df <- nx - 1
    stderr <- sqrt(vx/nx)
    if (stderr < 10 * .Machine$double.eps * abs(mx)) 
      stop("data are essentially constant")
  }
  else {
    ny <- length(y)
    if (nx < 2) 
      stop("not enough 'x' observations")
    if (ny < 2) 
      stop("not enough 'y' observations")
    my <- mean(y)
    vy <- var(y)
    stderrx <- sqrt(vx/nx)
    stderry <- sqrt(vy/ny)
    stderr <- sqrt(stderrx^2 + stderry^2)
    df <- stderr^4/(stderrx^4/(nx - 1) + stderry^4/(ny - 1))
    if (stderr < 10 * .Machine$double.eps * max(abs(mx), 
                                                abs(my))) 
      stop("data are essentially constant")
  }
  ### Own code starts here ###
  if(paired) {
    mcmc_samples <- jags_paired_t_test(x, y, n.chains= 3, n.iter = ceiling(n.iter / 3), progress.bar=progress.bar)
    stats <- mcmc_stats(mcmc_samples, cred_mass = cred.mass, comp_val = mu)
    bfa_object <- list(x = x, y = y, pair_diff = x - y, comp = mu, cred_mass = cred.mass,
                       x_name = x_name, y_name = y_name, data_name = data_name,
                       x_data_expr = x_name, y_data_expr = y_name,
                       mcmc_samples = mcmc_samples, stats = stats)
    class(bfa_object) <- c("bayes_paired_t_test", "bayesian_first_aid")
  } else if(is.null(y)) {
    mcmc_samples <- jags_one_sample_t_test(x, comp_mu = mu, n.chains= 3, n.iter = ceiling(n.iter / 3), progress.bar=progress.bar)
    stats <- mcmc_stats(mcmc_samples, cred_mass = cred.mass, comp_val = mu)
    bfa_object <- list(x = x, comp = mu, cred_mass = cred.mass, x_name = x_name, x_data_expr = x_name,
                       data_name = data_name, mcmc_samples = mcmc_samples, stats = stats)
    class(bfa_object) <- c("bayes_one_sample_t_test", "bayesian_first_aid")
  } else { # is two sample t.test
    mcmc_samples <- jags_two_sample_t_test(x, y, n.chains= 3, n.iter = ceiling(n.iter / 3), progress.bar=progress.bar)
    stats <- mcmc_stats(mcmc_samples, cred_mass = cred.mass, comp_val = mu)
    bfa_object <- list(x = x, y = y, comp = mu, cred_mass = cred.mass,
                       x_name = x_name, y_name = y_name, data_name = data_name,
                       x_data_expr = x_name, y_data_expr = y_name,
                       mcmc_samples = mcmc_samples, stats = stats)
    class(bfa_object) <- c("bayes_two_sample_t_test", "bayesian_first_aid")
  }
  bfa_object
}
jags_two_sample_t_test<-function(x, y, n.adapt= 500, n.chains=3, n.update = 100, n.iter=5000, thin=1, progress.bar="text") {
  data_list <- list(
    x = x ,
    y = y ,
    mean_mu = mean(c(x, y), trim=0.2) ,
    precision_mu = 1 / (mad0(c(x, y))^2 * 1000000),
    sigma_low = mad0(c(x, y)) / 1000 ,
    sigma_high = mad0(c(x, y)) * 1000 
  )
  inits_list <- list(
    mu_x = mean(x, trim=0.2),
    mu_y = mean(y, trim=0.2),
    sigma_x = mad0(x),
    sigma_y = mad0(y),
    nuMinusOne = 4
  )
  params <- c("mu_x", "sigma_x", "mu_y", "sigma_y", "mu_diff", "sigma_diff","nu", "eff_size", "x_pred", "y_pred")  
  mcmc_samples <- run_jags(two_sample_t_model_string, data = data_list, inits = inits_list, 
                           params = params, n.chains = n.chains, n.adapt = n.adapt,
                           n.update = n.update, n.iter = n.iter, thin = thin, progress.bar=progress.bar)
  mcmc_samples
}
jags_one_sample_t_test<-function(x, comp_mu = 0,n.adapt= 500, n.chains=3, n.update = 100, n.iter=5000, thin=1, progress.bar="text") {
  data_list <- list(
    x = x,
    mean_mu = mean(x, trim=0.2) ,
    precision_mu = 1 / (mad0(x)^2 * 1000000),
    sigma_low = mad0(x) / 1000 ,
    sigma_high = mad0(x) * 1000 ,
    comp_mu = comp_mu
  )
  inits_list <- list(mu = mean(x, trim=0.2), sigma = mad0(x), nuMinusOne = 4)
  params <- c("mu", "sigma", "nu", "eff_size", "x_pred")
  mcmc_samples <- run_jags(one_sample_t_model_string, data = data_list, inits = inits_list, 
                           params = params, n.chains = n.chains, n.adapt = n.adapt,
                           n.update = n.update, n.iter = n.iter, thin = thin, progress.bar=progress.bar)   
  mcmc_samples
}
jags_paired_t_test<-function(x, y, comp_mu = 0, n.adapt= 500, n.chains=3, n.update = 100, n.iter=5000, thin=1, progress.bar="text") {
  pair_diff <- x - y
  data_list <- list(
    pair_diff = pair_diff,
    mean_mu = mean(pair_diff, trim=0.2) ,
    precision_mu = 1 / (mad0(pair_diff)^2 * 1000000),
    sigma_low = mad0(pair_diff) / 1000 ,
    sigma_high = mad0(pair_diff) * 1000 ,
    comp_mu = comp_mu
  )
  inits_list <- list(mu_diff = mean(pair_diff, trim=0.2), 
                     sigma_diff = mad0(pair_diff), 
                     nuMinusOne = 4)
  params <- c("mu_diff", "sigma_diff", "nu", "eff_size", "diff_pred")
  mcmc_samples <- run_jags(paired_samples_t_model_string, data = data_list, inits = inits_list, 
                           params = params, n.chains = n.chains, n.adapt = n.adapt,
                           n.update = n.update, n.iter = n.iter, thin = thin, progress.bar=progress.bar) 
  mcmc_samples
}
two_sample_t_model_string<-"model {\n  for(i in 1:length(x)) {\n    x[i] ~ dt( mu_x , tau_x , nu )\n  }\n  x_pred ~ dt( mu_x , tau_x , nu )\n  for(i in 1:length(y)) {\n    y[i] ~ dt( mu_y , tau_y , nu )\n  }\n  y_pred ~ dt( mu_y , tau_y , nu )\n  eff_size <- (mu_x - mu_y) / sqrt((pow(sigma_x, 2) + pow(sigma_y, 2)) / 2)\n  mu_diff <- mu_x - mu_y\n  sigma_diff <-sigma_x - sigma_y \n  \n  # The priors\n  mu_x ~ dnorm( mean_mu , precision_mu )\n  tau_x <- 1/pow( sigma_x , 2 )\n  sigma_x ~ dunif( sigma_low , sigma_high )\n\n  mu_y ~ dnorm( mean_mu , precision_mu )\n  tau_y <- 1/pow( sigma_y , 2 )\n  sigma_y ~ dunif( sigma_low , sigma_high )\n\n  # A trick to get an exponentially distributed prior on nu that starts at 1.\n  nu <- nuMinusOne+1\n  nuMinusOne ~ dexp(1/29)\n}"
one_sample_t_model_string<-"model {\n  for(i in 1:length(x)) {\n    x[i] ~ dt( mu , tau , nu )\n  }\n  x_pred ~ dt( mu , tau , nu )\n  eff_size <- (mu - comp_mu) / sigma\n\n  mu ~ dnorm( mean_mu , precision_mu )\n  tau <- 1/pow( sigma , 2 )\n  sigma ~ dunif( sigma_low , sigma_high )\n  # A trick to get an exponentially distributed prior on nu that starts at 1.\n  nu <- nuMinusOne + 1 \n  nuMinusOne ~ dexp(1/29)\n}"
paired_samples_t_model_string<-"model {\n  for(i in 1:length(pair_diff)) {\n    pair_diff[i] ~ dt( mu_diff , tau_diff , nu )\n  }\n  diff_pred ~ dt( mu_diff , tau_diff , nu )\n  eff_size <- (mu_diff - comp_mu) / sigma_diff\n  \n  mu_diff ~ dnorm( mean_mu , precision_mu )\n  tau_diff <- 1/pow( sigma_diff , 2 )\n  sigma_diff ~ dunif( sigma_low , sigma_high )\n  # A trick to get an exponentially distributed prior on nu that starts at 1.\n  nu <- nuMinusOne + 1 \n  nuMinusOne ~ dexp(1/29)\n}"
mad0<-function(..., na.rm=TRUE) {
  mad_est <- mad(..., na.rm=na.rm)
  if(mad_est != 0) {
    mad_est
  } else {
    sd(..., na.rm=na.rm)
  }
}
paired_samples_t_test_model_code<-function(pair_diff, comp_mu) {
  # The model string written in the JAGS language
  model_string <- "model {
  for(i in 1:length(pair_diff)) {
  pair_diff[i] ~ dt( mu_diff , tau_diff , nu )
  }
  diff_pred ~ dt( mu_diff , tau_diff , nu )
  eff_size <- (mu_diff - comp_mu) / sigma_diff
  mu_diff ~ dnorm( mean_mu , precision_mu )
  tau_diff <- 1/pow( sigma_diff , 2 )
  sigma_diff ~ dunif( sigma_low , sigma_high )
  # A trick to get an exponentially distributed prior on nu that starts at 1.
  nu <- nuMinusOne + 1 
  nuMinusOne ~ dexp(1/29)
}"
  # Setting parameters for the priors that in practice will result
  # in flat priors on mu and sigma.
  mean_mu = mean(pair_diff, trim=0.2)
  precision_mu = 1 / (mad(pair_diff)^2 * 1000000)
  sigma_low = mad(pair_diff) / 1000 
  sigma_high = mad(pair_diff) * 1000
  
  # Initializing parameters to sensible starting values helps the convergence
  # of the MCMC sampling. Here using robust estimates of the mean (trimmed)
  # and standard deviation (MAD).
  inits_list <- list(
    mu_diff = mean(pair_diff, trim=0.2),
    sigma_diff = mad(pair_diff),
    nuMinusOne = 4)
  data_list <- list(
    pair_diff = pair_diff,
    comp_mu = comp_mu,
    mean_mu = mean_mu,
    precision_mu = precision_mu,
    sigma_low = sigma_low,
    sigma_high = sigma_high)
  
  # The parameters to monitor.
  params <- c("mu_diff", "sigma_diff", "nu", "eff_size", "diff_pred")
  
  # Running the model
  model <- jags.model(textConnection(model_string), data = data_list,
                      inits = inits_list, n.chains = 3, n.adapt=1000)
  update(model, 500) # Burning some samples to the MCMC gods....
  samples <- coda.samples(model, params, n.iter=10000)
  
  # Inspecting the posterior
  plot(samples)
  summary(samples)  
  }
two_sample_poisson_model_code<-function(x, t) {
  # The model string written in the JAGS language
  model_string <- "model {
  for(group_i in 1:2) {
  x[group_i] ~ dpois(lambda[group_i] * t[group_i])
  lambda[group_i] ~ dgamma(0.5, 0.00001)
  x_pred[group_i] ~ dpois(lambda[group_i] * t[group_i])
  }
  rate_diff <- lambda[1] - lambda[2]
  rate_ratio <- lambda[1] / lambda[2]
}"
  # Running the model
  model <- jags.model(textConnection(model_string), data = list(x = x, t = t), n.chains = 3)
  samples <- coda.samples(model, c("lambda", "x_pred", "rate_diff", "rate_ratio"), n.iter=5000)
  
  # Inspecting the posterior
  plot(samples)
  summary(samples)  
  }
one_sample_poisson_model_code<-function(x, t) {
  # The model string written in the JAGS language
  model_string <- "model {
  x ~ dpois(lambda * t)
  lambda ~ dgamma(0.5, 0.00001)
  x_pred ~ dpois(lambda * t)
}"
  # Running the model
  model <- jags.model(textConnection(model_string), data = list(x = x, t = t), n.chains = 3)
  samples <- coda.samples(model, c("lambda", "x_pred"), n.iter=5000)
  
  # Inspecting the posterior
  plot(samples)
  summary(samples)  
  }
summary.bayes_two_sample_t_test<-function(object, ...) {
  s <- round(object$stats, 3)
  cat("  Data\n")
  cat(object$x_name, ", n = ", length(object$x), "\n", sep="")
  cat(object$y_name, ", n = ", length(object$y), "\n", sep="")
  cat("\n")
  print_bayes_two_sample_t_test_params(object)
  cat("\n")
  cat("  Measures\n" )
  print(s[, c("mean", "sd", "HDIlo", "HDIup", "%<comp", "%>comp")])
  cat("\n")
  cat("'HDIlo' and 'HDIup' are the limits of a ", s[1, "HDI%"] ,"% HDI credible interval.\n", sep="")
  cat("'%<comp' and '%>comp' are the probabilities of the respective parameter being\n")
  cat("smaller or larger than ", s[1, "comp"] ,".\n", sep="")
  cat("\n")
  cat("  Quantiles\n" )
  print(s[, c("q2.5%", "q25%", "median","q75%", "q97.5%")] )
  invisible(object$stats)
}
summary.bayes_paired_t_test<-function(object, ...) {
  s <- round(object$stats, 3)
  cat("  Data\n")
  cat(object$x_name, ", n = ", length(object$x), "\n", sep="")
  cat(object$y_name, ", n = ", length(object$y), "\n", sep="")
  cat("\n")
  print_bayes_paired_t_test_params(object)
  cat("\n")
  cat("  Measures\n" )
  print(s[, c("mean", "sd", "HDIlo", "HDIup", "%<comp", "%>comp")])
  cat("\n")
  cat("'HDIlo' and 'HDIup' are the limits of a ", s[1, "HDI%"] ,"% HDI credible interval.\n", sep="")
  cat("'%<comp' and '%>comp' are the probabilities of the respective parameter being\n")
  cat("smaller or larger than ", s[1, "comp"] ,".\n", sep="")
  cat("\n")
  cat("  Quantiles\n" )
  print(s[, c("q2.5%", "q25%", "median","q75%", "q97.5%")] )
  invisible(object$stats)
}
summary.bayes_one_sample_t_test<-function(object, ...) {
  s <- round(object$stats, 3)
  cat("  Data\n")
  cat(object$data_name, ", n = ", length(object$x), "\n", sep="")
  cat("\n")
  print_bayes_one_sample_t_test_params(object)
  cat("\n")
  cat("  Measures\n" )
  print(s[, c("mean", "sd", "HDIlo", "HDIup", "%<comp", "%>comp")])
  cat("\n")
  cat("'HDIlo' and 'HDIup' are the limits of a ", s[1, "HDI%"] ,"% HDI credible interval.\n", sep="")
  cat("'%<comp' and '%>comp' are the probabilities of the respective parameter being\n")
  cat("smaller or larger than ", s[1, "comp"] ,".\n", sep="")
  cat("\n")
  cat("  Quantiles\n" )
  print(s[, c("q2.5%", "q25%", "median","q75%", "q97.5%")] )
  invisible(object$stats)
}
model.code.bayes_one_sample_t_test<-function(fit) {  
  cat("### Model code for Bayesian estimation supersedes the t test - one sample ###\n")
  cat("##require(rjags)\n\n")
  cat("# Setting up the data\n")
  cat("x <-", fit$x_data_expr, "\n")
  cat("comp_mu <- ", fit$comp, "\n")
  cat("\n")
  pretty_print_function_body(one_sample_t_test_model_code)
  invisible(NULL)
}
model.code.bayes_paired_t_test<-function(fit) {
  cat("## Model code for Bayesian estimation supersedes the t test - paired samples ##\n")
  cat("##require(rjags)\n\n")
  cat("# Setting up the data\n")
  cat("x <-", fit$x_data_expr, "\n")
  cat("y <-", fit$y_data_expr, "\n")
  cat("pair_diff <- x - y\n")
  cat("comp_mu <- ", fit$comp, "\n")
  cat("\n")
  pretty_print_function_body(paired_samples_t_test_model_code)
  invisible(NULL)
}
model.code.bayes_two_sample_t_test<-function(fit) {
  cat("## Model code for Bayesian estimation supersedes the t test - two sample ##\n")
  cat("##require(rjags)\n\n")
  cat("# Setting up the data\n")
  cat("x <-", fit$x_data_expr, "\n")
  cat("y <-", fit$y_data_expr, "\n")
  cat("\n")
  pretty_print_function_body(two_sample_t_test_model_code)
  invisible(NULL)
}
print_bayes_two_sample_t_test_params<-function(x) {
  cat("  Model parameters and generated quantities\n")
  cat("mu_x: the mean of", x$x_name, "\n")
  cat("sigma_x: the scale of", x$x_name,", a consistent\n  estimate of SD when nu is large.\n")
  cat("mu_y: the mean of", x$y_name, "\n")
  cat("sigma_y: the scale of", x$y_name,"\n")
  cat("mu_diff: the difference in means (mu_x - mu_y)\n")
  cat("sigma_diff: the difference in scale (sigma_x - sigma_y)\n")
  cat("nu: the degrees-of-freedom for the t distribution\n")
  cat("  fitted to",x$data_name , "\n")
  cat("eff_size: the effect size calculated as \n", sep="")
  cat("  (mu_x - mu_y) / sqrt((sigma_x^2 + sigma_y^2) / 2)\n", sep="")
  cat("x_pred: predicted distribution for a new datapoint\n")
  cat("  generated as",x$x_name , "\n")
  cat("y_pred: predicted distribution for a new datapoint\n")
  cat("  generated as",x$y_name , "\n")
  invisible(NULL)
}
print_bayes_paired_t_test_params<-function(x) {
  cat("  Model parameters and generated quantities\n")
  cat("mu_diff: the mean pairwise difference between", x$x_name, "and", x$y_name, "\n")
  cat("sigma_diff: the scale of the pairwise difference, a consistent\n  estimate of SD when nu is large.\n")
  cat("nu: the degrees-of-freedom for the t distribution fitted to the pairwise difference\n")
  cat("eff_size: the effect size calculated as (mu_diff - ", x$comp ,") / sigma_diff\n", sep="")
  cat("diff_pred: predicted distribution for a new datapoint generated\n  as the pairwise difference between", x$x_name, "and", x$y_name,"\n")
}
print_bayes_one_sample_t_test_params<-function(x) {
  cat("  Model parameters and generated quantities\n")
  cat("mu: the mean of", x$data_name, "\n")
  cat("sigma: the scale of", x$data_name,", a consistent\n  estimate of SD when nu is large.\n")
  cat("nu: the degrees-of-freedom for the t distribution fitted to",x$data_name , "\n")
  cat("eff_size: the effect size calculated as (mu - ", x$comp ,") / sigma\n", sep="")
  cat("x_pred: predicted distribution for a new datapoint generated as",x$data_name , "\n")
}
print_mcmc_info<-function(mcmc_samples) {
  cat("\n", "Iterations = ", start(mcmc_samples), ":", end(mcmc_samples), "\n", sep = "")
  cat("Thinning interval =", thin(mcmc_samples), "\n")
  cat("Number of chains =", nchain(mcmc_samples), "\n")
  cat("Sample size per chain =", (end(mcmc_samples) - start(mcmc_samples))/thin(mcmc_samples) + 1, "\n")
}
print_diagnostics_measures<-function(s) {
  cat("  Diagnostic measures\n")
  print(s[, c("mean", "sd", "mcmc_se", "n_eff", "Rhat")])
  cat("\n")
  cat("mcmc_se: the estimated standard error of the MCMC approximation of the mean.\n")
  cat("n_eff: a crude measure of effective MCMC sample size.\n")
  cat("Rhat: the potential scale reduction factor (at convergence, Rhat=1).\n")
}
pretty_print_function_body<-function(fn) {
  fn_string <- deparse(fn, control="useSource")
  fn_string <- gsub("^  ", "", fn_string)
  cat(paste(fn_string[-c(1, length(fn_string))], collapse="\n"))
}
one_sample_t_test_model_code<-function(x, comp_mu) {
  # The model string written in the JAGS language
  model_string <- "model {
  for(i in 1:length(x)) {
  x[i] ~ dt( mu , tau , nu )
  }
  x_pred ~ dt( mu , tau , nu )
  eff_size <- (mu - comp_mu) / sigma
  mu ~ dnorm( mean_mu , precision_mu )
  tau <- 1/pow( sigma , 2 )
  sigma ~ dunif( sigma_low , sigma_high )
  # A trick to get an exponentially distributed prior on nu that starts at 1.
  nu <- nuMinusOne + 1 
  nuMinusOne ~ dexp(1/29)
}"
  # Setting parameters for the priors that in practice will result
  # in flat priors on mu and sigma.
  mean_mu = mean(x, trim=0.2)
  precision_mu = 1 / (mad(x)^2 * 1000000)
  sigma_low = mad(x) / 1000 
  sigma_high = mad(x) * 1000
  
  # Initializing parameters to sensible starting values helps the convergence
  # of the MCMC sampling. Here using robust estimates of the mean (trimmed)
  # and standard deviation (MAD).
  inits_list <- list(mu = mean(x, trim=0.2), sigma = mad(x), nuMinusOne = 4)
  data_list <- list(
    x = x,
    comp_mu = comp_mu,
    mean_mu = mean_mu,
    precision_mu = precision_mu,
    sigma_low = sigma_low,
    sigma_high = sigma_high)
  
  # The parameters to monitor.
  params <- c("mu", "sigma", "nu", "eff_size", "x_pred")
  
  # Running the model
  model <- jags.model(textConnection(model_string), data = data_list,
                      inits = inits_list, n.chains = 3, n.adapt=1000)
  update(model, 500) # Burning some samples to the MCMC gods....
  samples <- coda.samples(model, params, n.iter=10000)
  
  # Inspecting the posterior
  plot(samples)
  summary(samples)  
  }
two_sample_t_test_model_code<-function(x, y) {
  # The model string written in the JAGS language
  model_string <- "model {
  for(i in 1:length(x)) {
  x[i] ~ dt( mu_x , tau_x , nu )
  }
  x_pred ~ dt( mu_x , tau_x , nu )
  for(i in 1:length(y)) {
  y[i] ~ dt( mu_y , tau_y , nu )
  }
  y_pred ~ dt( mu_y , tau_y , nu )
  eff_size <- (mu_x - mu_y) / sqrt((pow(sigma_x, 2) + pow(sigma_y, 2)) / 2)
  mu_diff <- mu_x - mu_y
  sigma_diff <-sigma_x - sigma_y 
  
  # The priors
  mu_x ~ dnorm( mean_mu , precision_mu )
  tau_x <- 1/pow( sigma_x , 2 )
  sigma_x ~ dunif( sigma_low , sigma_high )
  mu_y ~ dnorm( mean_mu , precision_mu )
  tau_y <- 1/pow( sigma_y , 2 )
  sigma_y ~ dunif( sigma_low , sigma_high )
  
  # A trick to get an exponentially distributed prior on nu that starts at 1.
  nu <- nuMinusOne+1
  nuMinusOne ~ dexp(1/29)
}"
  # Setting parameters for the priors that in practice will result
  # in flat priors on the mu's and sigma's.
  mean_mu = mean( c(x, y), trim=0.2)
  precision_mu = 1 / (mad( c(x, y) )^2 * 1000000)
  sigma_low = mad( c(x, y) ) / 1000 
  sigma_high = mad( c(x, y) ) * 1000
  
  # Initializing parameters to sensible starting values helps the convergence
  # of the MCMC sampling. Here using robust estimates of the mean (trimmed)
  # and standard deviation (MAD).
  inits_list <- list(
    mu_x = mean(x, trim=0.2), mu_y = mean(y, trim=0.2),
    sigma_x = mad(x), sigma_y = mad(y),
    nuMinusOne = 4)
  data_list <- list(
    x = x, y = y,    
    mean_mu = mean_mu,
    precision_mu = precision_mu,
    sigma_low = sigma_low,
    sigma_high = sigma_high)
  
  # The parameters to monitor.
  params <- c("mu_x", "mu_y", "mu_diff", "sigma_x", "sigma_y", "sigma_diff",
              "nu", "eff_size", "x_pred", "y_pred")
  
  # Running the model
  model <- jags.model(textConnection(model_string), data = data_list,
                      inits = inits_list, n.chains = 3, n.adapt=1000)
  update(model, 500) # Burning some samples to the MCMC gods....
  samples <- coda.samples(model, params, n.iter=10000)
  
  # Inspecting the Posterior
  plot(samples)
  summary(samples)  
  }
createMenu <- function(tt) {
    topMenu <- tkmenu(tt)
    tkconfigure(tt, menu = topMenu)

    #File Menu
    fileFun <- c(
        NA, 
	NA, NA, function() tkdestroy(tt),
        function() getcsvfile(), function() gettxtfile(), function() getxlfile(), function() getspssfile(), function() getshapefile(), 
        function() configurableCmd("saveas")
    )
    fileMenu <- data.frame(
        label = c(
            "File", 
            "Open File", "Save", "Exit zooaRch",
            ".csv", ".txt", "Excel", "SPSS", "Shapefile", 
            "Save as"
        ),
        parent = c(
            0, 
            1, 1, 1,
            2, 2, 2, 2, 2, 
            3
        )
    )
    addMenu(fileMenu, fileFun, topMenu)

    #Edit Menu
    editFun <- c(NA, NA, function() configurableCmd("sort", 4))
    editMenu <- data.frame(
        label = c("Edit", "Sort", "Sort by Column"),
        parent = c(0, 1, 2)
    )  
    addMenu(editMenu, editFun, topMenu)

    #Plotting Menu  
    plottingFun <- c(
        NA,
        function() configurableCmd("hist", 2), function() configurableCmd("multi_hist", 3, "hor"), function() configurableCmd("bivplot", 3), function() configurableCmd("simp_box", 2), function() configurableCmd("multi_boxplot", 3, "hor")
    )
    plottingMenu <- data.frame(
        label = c(
            "Plotting", 
            "Histogram", "Multiway Histogram", "Bivariate", "Boxplot", "Multiway Boxplot"
        ),
        parent = c(
            0, 
            1, 1, 1, 1, 1
        )
    )  
    addMenu(plottingMenu, plottingFun, topMenu)

    #Counting Menu
    countingFun <- c(
        NA,
        NA, NA, NA, NA,
        function() fun_na(), function() fun_na(), function() fun_na(), function() fun_na(), 
        function() fun_na(), function() fun_na(), 
        function() fun_na(), function() fun_na(), 
        function() fun_na(), function() fun_na(), function() fun_na(), function() fun_na()
    )
    countingMenu <- data.frame(
        label = c(
            "Counting", 
            "Tabulation", "LP-Index", "ABCML", "Sexing", 
            "MNI", "MNE", "MAU", "%MAU",  
            "LP", "LP2",  
            "ABCML", "ABCML",
            "Regression", "DFA", "Kernel Density", "Mixing-Model"
        ),
        parent = c(
            0, 
            1, 1, 1, 1,
            2, 2, 2, 2, 
            3, 3,
            4, 4,
            5, 5, 5, 5
        )
    )  
    addMenu(countingMenu, countingFun, topMenu)

    #Transformation Menu
    transFun <- c(
        NA,
        NA, NA,
        function() configurableCmd("trans", 2), 
        function() configurableCmd("LSI", 0), function() configurableCmd("VSI", 0)
    )
    transMenu <- data.frame(
        label = c(
            "Transformations",
            "Mathematical", "Zooarchaeological",
            "Mathematical",
            "LSI", "VSI"
        ),
        parent = c(
            0, 
            1, 1, 
            2, 
            3, 3
        )
    )  
    addMenu(transMenu, transFun, topMenu)

    #Sampling Menu
    sampFun <- c(NA, function() configurableCmd("s_rare", 2), function() configurableCmd("s_rare", 2))
    sampMenu <- data.frame(
        label = c("Sampling", "Single Sample Rarefaction", "Multiple Sample Rarefaction"),
        parent = c(0, 1, 1)
    )  
    addMenu(sampMenu, sampFun, topMenu)

    #Diversity Menu
    divFun <- c(NA, NA, function() configurableCmd("divind"))
    divMenu <- data.frame(
        label = c("Diversity", "Diversity", "Indices"),
        parent = c(0, 1, 2)
    )  
    addMenu(divMenu, divFun, topMenu)

    #Univariate Menu  
    univFun <- c(
        NA,
        NA, NA, NA, NA, NA, NA, NA,    
        function() configurableCmd("summar", 2), 
        function() configurableCmd("shapiro", 2), function() configurableCmd("ks", 2), 
        function() configurableCmd("onesam_t", 2), function() configurableCmd("t", 3), function() configurableCmd("oneway", 3, "hor"), function() configurableCmd("fact", 4, "hor2"), 
        function() configurableCmd("chisq", 3), function() configurableCmd("Fisher_exact", 3), 
        function() configurableCmd("F_var", 3), function() configurableCmd("bartletts", 3), function() configurableCmd("LevenesVarTest", 3), 
        function() configurableCmd("Assoc", 3), function() configurableCmd("simp_lm", 3, "hor"), function() configurableCmd("glm", 4, "hor2"), 
        function() configurableCmd("death_age", 0)
    )
    univMenu <- data.frame(
        label = c(
            "Univariate Statistics", 
            "Summary Statistics","Normality Tests", "Means","Proportions","Variances","Association","Survivorship", 
            "Summary Dialog",
            "Shapiro-Wilk's Test", "Kolmogorov-Smirnov Test",
            "One Sample t-test", "Two Sample t-test", "One-way ANOVA", "Factorial ANOVA",
            "Chi Squared (row-by-column tests)", "Fisher's Exact Test",
            "F-test", "Bartlett's Test", "Levene's", 
            "Correlation", "Simple Linear Regression", "Generalized Linear Model", 
            "Survivorship"
        ),
        parent = c(
            0, 
            1, 1, 1, 1, 1, 1, 1,
            2,
            3, 3,
            4, 4, 4, 4,
            5, 5,
            6, 6, 6,
            7, 7, 7,
            8
        )
    )  
    addMenu(univMenu, univFun, topMenu)

    #Multivariate Menu  
    multivFun <- c(
        NA,
        NA, NA, NA, NA, NA, NA,
        function() configurableCmd("dist", 2), 
        function() fun_na(), function() configurableCmd("NPHotellingt", 3), 
        function() configurableCmd("parMANOVA", 3, "hor"), function() configurableCmd("NPMANOVA", 3, "hor"), 
        function() configurableCmd("mantel2way", 0), function() configurableCmd("mantel3", 0), 
        function() fun_na(), function() configurableCmd("prcomp", 2), function() configurableCmd("PCoA", 2), function() configurableCmd("NMDS", 0), function() configurableCmd("correspondence", 3), function() configurableCmd("multivarfit", 0),
        function() configurableCmd("cluster", 0), NA,
        function() configurableCmd("kmeans_single", 0), function() configurableCmd("kmeans_multi", 0)
    )
    multivMenu <- data.frame(
        label = c(
            "Multivariate Statistics", 
            "D-Matrices","Hotelling T Tests", "MANOVA","Mantel Tests","Ordination","Clustering", 
            "D-Matrices",
            "One Sample", "Two Sample",
            "MANOVA", "NP-MANOVA", 
            "2-way Mantel", "3-way Mantel",
            "Seriation", "PCA", "PCoA", "NMDS", "CA", "Vector Fitting",
            "Hierarchical", "K-means", 
            "Single K-means", "Multi K-means"        
        ),
        parent = c(
            0, 
            1, 1, 1, 1, 1, 1,
            2,
            3, 3,
            4, 4,
            5, 5,
            6, 6, 6, 6, 6, 6,
            7, 7, 
            22, 22
        )
    )  
    addMenu(multivMenu, multivFun, topMenu)

    #Modeling Menu

    #Spatial Menu
    spatFun <- c(
        NA,
        NA, NA, NA, NA, NA,
        function() configurableCmd("ripleys_K", 1), 
        function() fun_na(), 
        function() configurableCmd("moran_geary", 0), function() fun_na(), function() fun_na(), 
        function() fun_na(), function() fun_na(), function() fun_na(), 
        function() fun_na(), function() fun_na(), function() fun_na()
    )
    spatMenu <- data.frame(
        label = c(
            "Spatial", 
            "Point Patterns", "Spatial Connections", "Autocorrelation tests", "Variogram Modeling", "Interpolation",
            "Ripley's K",
            "Spatially Defined Connections",
            "Global Moran's I/Geary's C", "LISA", "Getis-Ord Gi*",
            "Plot Spatial Covariance", "Plot Spatial Semi-Variogram", "Fit Semi-Variogram",
            "Kriging", "TPS", "IDW"
        ),
        parent = c(
            0, 
            1, 1, 1, 1, 1,
            2,
            3,
            4, 4, 4,
            5, 5, 5,
            6, 6, 6
        )
    )  
    addMenu(spatMenu, spatFun, topMenu)

    #Bayesian Menu  
    bayesFun <- c(NA, NA, function() configurableCmd("Bayes_onesam_t", 2), function() configurableCmd("Bayes_2t", 3), function() configurableCmd("Bayes_binom", 0))
    bayesMenu <- data.frame(
        label = c("Bayesian", "Bayesian Tests", "Bayesian 1 sample t-test", "Bayesian 2 sample t-test","Bayesian binomial test"),
        parent = c(0, 1, 2, 2, 2)
    )  
    addMenu(bayesMenu, bayesFun, topMenu)

    #Morphometrics Menu  
    morphosFun <- c(
        NA, 
        function() configurableCmd("readland", 0), NA, function() configurableCmd("gpagen", 0), NA,
        function() fun_na(), function() fun_na(),
        function() fun_na(), function() fun_na(), function() fun_na(), function() fun_na()
    )
    morphoMenu <- data.frame(
        label = c(
            "GMM", 
            "Import Data", "Digitize", "GPA", "Plot",
            "2D-Digitize", "3D-Digitize",
            "Tangent Space", "Deformation", "Reference-Target Deformation", "Analyses"
        ),
        parent = c(
            0, 
            1, 1, 1, 1,
            3, 3, 
            5, 5, 5, 5
        )
    )  
    addMenu(morphoMenu, morphosFun, topMenu)

    #Help Menu
    #print("Survival data from Marj Rabba, using Payne's (1973) age classes")})  
    helpFun <- c(
        NA, 
        NA,
        NA, NA, NA, NA,
        function() data("syraPoly", envir = environment()), function() data("spatPOLY", envir = environment()), function() data("randPTS", envir = environment()), function() data("clusterPTS", envir = environment()),
        function() data("LRBdata", envir = environment()), function() data("LRBkey", envir = environment()), function() data("myData", envir = environment()),
        function() data("ClarksonLimbLoc", envir = environment()),
        function() data("speth83", envir = environment()), function() data("speth83labels", envir = environment()), function() data("marjRab", envir = environment()),
        function() data("marjRlabels", envir = environment()), function() data("marjRab.fuse", envir = environment()), function() data("winslow.fuse", envir = environment())
    )
    helpMenu <- data.frame(
        label = c(
            "Help", 
            "Example Data", 
            "Spatial", "Multivariate", "Chi Squared", "Survivorship",
            "Syracuse Polygon", "Spatial Polygon", "Random Spatial Points", "Clustered Spatial Points",
            "Binford's Data", "Binford's Variable Key", "Site-x-Species",
            "Clarkson Deer Data",        
            "Garnsey Survival", "Garnsey Labels", "Marj Rabba Survival", "Marj Rabba Labels", "Marj Rabba Fusion", "Winslow Fusion"
        ),
        parent = c(
            0, 
            1, 
            2, 2, 2, 2,
            3, 3, 3, 3,
            4, 4, 4,
            5,
            6, 6, 6, 6, 6, 6
        )
    )  
    addMenu(helpMenu, helpFun, topMenu)
}

addMenu <- function(menu, funcs, topMenu) {
    menuItem <- list()
    for (i in 1:nrow(menu)) {
        parentId = menu[i,"parent"]
        if (class(funcs[[i]]) == "function") {
            tkadd(menuItem[[parentId]], "command", label = as.character(menu[i,"label"]), command = funcs[[i]] ) 
        }
        else {
            menuItem[[i]] <- tkmenu(topMenu, tearoff = FALSE) # menu
            if(menu[i,"parent"] == 0) {
                tkadd(topMenu, "cascade", label = as.character(menu[i,"label"]), menu = menuItem[[i]])
            }
            else {
                tkadd(menuItem[[parentId]], "cascade", label = as.character(menu[i,"label"]), menu = menuItem[[i]])
            } 
        }
    }
}

# Interface method for configurable command
layout <- function(e) {
    UseMethod("layout", e)
}

# Interface method for configurable command
run <- function(e) {
    UseMethod("run", e);
}


configurableCmd<- function(name, comboNum=1, dir="ver") {
    e <- new.env()
    class(e) <- name
    createConfigureFrame(e, comboNum, dir)
}
  
# Not Available Function
fun_na <- function(){
  tkmessageBox(message = "Function not available. Check back soon!", icon = "info", type = "ok")
}


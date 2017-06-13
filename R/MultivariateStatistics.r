# parMANOVA Function
layout.parMANOVA<-function(e){ #EOC + JBR
  e$permutations<-tclVar(99)
  e$type<-tclVar("Wilks")
  e$function_type<-tclVar("Additive")
  
  #Begin GUI Setup
  tkwm.title(e$wnd, "MANOVA")
  tkconfigure(e$layout, text = "MANOVA")
  tkconfigure(e$varLabel[[1]], text = "Response Matrix:")
  tkconfigure(e$varLabel[[2]], text = "Predictor Matrix:")

  #X-Matrix Interactions Radiobuttons
  put_label(e$layout, "Predictor Interaction:", 6,0, sticky="w")
  rb_frame<-ttkframe(e$layout)
  sapply(c("Additive", "Multiplicative"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$function_type, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 6, column = 1, sticky = "w")
  
  #Test Method Selection
  put_label(e$layout, "Test:", 7,0, sticky="w")
  rb_frame<-ttkframe(e$layout)
  sapply(c("Wilks", "Pillai", "Hotelling-Lawley", "Roy"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 7, column = 1, sticky = "w")
}

run.parMANOVA<-function(e){
    #Using Selected Values
    Value_Y <- getVarName(e, 1)
    Value_X <- getVarName(e, 2)
    
    #Error Checking
    if(sum(sapply(e$dataFrame[,Value_Y], is.numeric))==FALSE) { 
      tkdestroy(e$wnd)
      stop("Response matrix must not contain characters!")
    }
    if(sum(sapply(e$dataFrame, is.na))==TRUE) {
      tkdestroy(e$wnd)
      stop("The data set is missing values!")
    }
    if(length(Value_X)<2){
      f1<-eval(paste("as.matrix(e$dataFrame[,Value_Y]) ~", as.name(Value_X)))
    }
    if(length(Value_X)>2) {
      tkdestroy(e$wnd)
      stop("There can be no more than two X variables selected!")
    }
    if(length(Value_X)==2) {
      Xvals<-e$dataFrame[,Value_X]
      assign(colnames(Xvals)[1],Xvals[,1])
      assign(colnames(Xvals)[2],Xvals[,2])
      if(tclvalue(e$function_type)=="Additive") {
        f1<-eval(paste("as.matrix(e$dataFrame[,Value_Y]) ~",
                       as.name(colnames(Xvals)[1]),
                       "+"
                       ,as.name(colnames(Xvals)[2]),sep=""))
      }
      else {
        f1<-eval(paste("as.matrix(e$dataFrame[,Value_Y]) ~",
                       as.name(colnames(Xvals)[1]),
                       "*"
                       ,as.name(colnames(Xvals)[2]),sep=""))
      }
    }
    out<-summary(manova(formula = as.formula(f1), data = e$dataFrame),test=tclvalue(e$type))
    print("MANOVA")
    print(out)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    tkdestroy(e$wnd)
}

# NPMANOVA Function 
layout.NPMANOVA<-function(e){ #EOC + JBR
  e$permutations<-tclVar(99)
  e$type<-tclVar("Euclidean")
  e$function_type<-tclVar("Additive")
  
  #Begin GUI Setup
  tkwm.title(e$wnd, "NP-MANOVA")
  tkconfigure(e$layout, text = "NP-MANOVA")
  tkconfigure(e$varLabel[[1]], text = "Response Matrix:")
  tkconfigure(e$varLabel[[2]], text = "Predictor Matrix:")

  #X-Matrix Interactions Radiobuttons
  put_label(e$layout, "Regression Behavior:", 6,0, sticky="w")
  rb_frame<-ttkframe(e$layout)
  sapply(c("Additive", "Multiplicative"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$function_type, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 6, column = 1, sticky = "w")
  
  #Method Selection Radiobuttons
  put_label(e$layout, "Method:", 7,0, sticky="w")
  rb_frame<-ttkframe(e$layout)
  sapply(c("Canberra", "Euclidean", "Jaccard", "Manhattan"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 7, column = 1, sticky = "w")
  
  #Permutation Slider
  put_label(e$layout, "Permutations:", 8, 0, sticky = "w")
  conf_level_frame <- ttkframe(e$layout)
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
}

run.NPMANOVA<-function(e){
    #Using Selected Values
    Value_Y <- getVarName(e, 1)
    Value_X <- getVarName(e, 2)
    
    #Error Checking
    if(sum(sapply(e$dataFrame[,Value_Y], is.numeric))==FALSE) {
      tkdestroy(e$wnd)
      stop("Response matrix must not contain characters!")
    }
    if(sum(sapply(e$dataFrame, is.na))==TRUE) {
      tkdestroy(e$wnd)
      stop("The data set is missing values!")
    }
    if(length(Value_X)<2){
      f1<-eval(paste("as.matrix(e$dataFrame[,Value_Y]) ~", as.name(Value_X)))
    }
    if(length(Value_X)>2) {
      tkdestroy(e$wnd)
      stop("There can be no more than two X variables selected!")
    }
    if(length(Value_X)==2) {
      Xvals<-e$dataFrame[,Value_X] # no need
      x1<-Xvals[,1]# no need
      x2<-Xvals[,2]# no need
      if(tclvalue(e$function_type)=="Additive") {
        f1<-eval(paste("e$dataFrame[,Value_Y] ~", as.name(Value_X[1]), "+", as.name(Value_X[2])))
      }
      else {
        f1<-eval(paste("e$dataFrame[,Value_Y] ~", as.name(Value_X[1]), "*", as.name(Value_X[2])))
      }
    }
    switch(tclvalue(e$type),
           Canberra = {out<-adonis(formula = as.formula(f1), data = e$dataFrame, permutations = as.numeric(tclvalue(e$permutations)), method = "canberra")
           print("Canberra")
           },
           Euclidean = {out<-adonis(formula = as.formula(f1), data = e$dataFrame, permutations = as.numeric(tclvalue(e$permutations)), method = "euclidean")
           print("Euclidean")
           },
           Jaccard = {out<-adonis(formula = as.formula(f1), data = e$dataFrame, permutations = as.numeric(tclvalue(e$permutations)), method = "jaccard")
           print("Jaccard")
           },
           Manhattan = {out<-adonis(formula = as.formula(f1), data = e$dataFrame, permutations = as.numeric(tclvalue(e$permutations)), method = "manhattan")
           print("Manhattan")}
    )
    print(out)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    tkdestroy(e$wnd)
}

# NP Hotelling Function 
layout.NPHotellingt<-function(e){ #EOC + JBR
  e$dataFrame <- tclVar("NULL"); 
  e$permutations<-tclVar(99)
  e$type<-tclVar("Euclidean")
  e$function_type<-tclVar("Additive")
 
  #Begin GUI Setup
  tkwm.title(e$wnd, "zooaRch")
  tkconfigure(e$layout, text = "NP-Hotelling T")
  tkconfigure(e$varLabel[[1]], text = "Response Matrix:")
  tkconfigure(e$varLabel[[2]], text = "Predictor Matrix:")
  
  #X-Matrix Interactions Radiobuttons
  put_label(e$layout, "Regression Behavior:", 6,0, sticky="w")
  rb_frame<-ttkframe(e$layout)
  sapply(c("Additive", "Multiplicative"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$function_type, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 6, column = 1, sticky = "w")
  
  #Method Selection Radiobuttons
  put_label(e$layout, "Method:", 7,0, sticky="w")
  rb_frame<-ttkframe(e$layout)
  sapply(c("Canberra", "Euclidean", "Jaccard", "Manhattan"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 7, column = 1, sticky = "w")
  
  #Permutation Slider
  put_label(e$layout, "Permutations:", 8, 0, sticky = "w")
  conf_level_frame <- ttkframe(e$layout)
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
}

run.NPHotellingt<-function(e){
    #Using Selected Values
    Value_Y <- getVarName(e, 1)
    Value_X <- getVarName(e, 2)
    
    #Error Checking
    if(sum(sapply(e$dataFrame[,Value_Y], is.numeric))==FALSE) {
      tkdestroy(e$wnd)
      stop("Response matrix must not contain characters!")
    }
    if(sum(sapply(e$dataFrame, is.na))==TRUE) {
      tkdestroy(e$wnd)
      stop("The data set is missing values!")
    }
    if(length(Value_X)<2){
      f1<-eval(paste("as.matrix(e$dataFrame[,Value_Y]) ~", as.name(Value_X)))
    }
    if(length(Value_X)>2) {
      tkdestroy(e$wnd)
      stop("There can be no more than two X variables selected!")
    }
    if(length(Value_X)==2) {
      Xvals<-e$dataFrame[,Value_X] # no need
      x1<-Xvals[,1]# no need
      x2<-Xvals[,2]# no need
      if(tclvalue(e$function_type)=="Additive") {
        f1<-eval(paste("e$dataFrame[,Value_Y] ~", as.name(Value_X[1]), "+", as.name(Value_X[2])))
      }
      else {
        f1<-eval(paste("e$dataFrame[,Value_Y] ~", as.name(Value_X[1]), "*", as.name(Value_X[2])))
      }
    }
    switch(tclvalue(e$type),
           Canberra = {out<-adonis(formula = as.formula(f1), data = e$dataFrame, permutations = as.numeric(tclvalue(e$permutations)), method = "canberra")
           print("Canberra")
           },
           Euclidean = {out<-adonis(formula = as.formula(f1), data = e$dataFrame, permutations = as.numeric(tclvalue(e$permutations)), method = "euclidean")
           print("Euclidean")
           },
           Jaccard = {out<-adonis(formula = as.formula(f1), data = e$dataFrame, permutations = as.numeric(tclvalue(e$permutations)), method = "jaccard")
           print("Jaccard")
           },
           Manhattan = {out<-adonis(formula = as.formula(f1), data = e$dataFrame, permutations = as.numeric(tclvalue(e$permutations)), method = "manhattan")
           print("Manhattan")}
    )
    print(out)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    tkdestroy(e$wnd)
}
  
# Distance Function
layout.dist<-function(e){ #EOC + JBR
  e$type<-tclVar("Euclidean")
  
  #Begin GUI Setup
  tkwm.title(e$wnd, "Distance Matrix")
  tkconfigure(e$layout, text = "Distance Matrix")
  tkconfigure(e$varLabel[[1]], text = "Choose Data:")

  #Distance Type Selection Radiobuttons
  put_label(e$layout, "Distance Type", 2,0, sticky="w")
  rb_frame<-ttkframe(e$layout)
  sapply(c("Canberra", "Euclidean", "Jaccard", "Manhattan"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 2, column = 1, sticky = "w")
}

run.dist<-function(e){
    
    #Error Checking
    if(sum(sapply(e$dataFrame, is.numeric))==FALSE) {
      tkdestroy(e$wnd)
      stop("The data set must not contain characters!")
    }
    if(sum(sapply(e$dataFrame, is.na))==TRUE) {
      tkdestroy(e$wnd)
      stop("The data set contains missing values!")
    }
    #Calculating the Distance
    Value1 <- getVarName(e, 1)
    switch(tclvalue(e$type),
           Canberra = {out<-as.matrix(dist(e$dataFrame[,Value1],method="canberra"))
           print("Canberra")
           },
           Euclidean = {out<-as.matrix(dist(e$dataFrame[,Value1],method="euclidean"))
           print("Euclidean")
           },
           Jaccard = {out<-as.matrix(vegdist(e$dataFrame[,Value1],method="jaccarrd"))
           print("Jaccard")
           },
           Manhattan = {out<-as.matrix(dist(e$dataFrame[,Value1],method="manhattan"))
           print("Manhattan")}
    )
    dimnames(out)<-list(row.names(e$dataFrame),row.names(e$dataFrame))
    out<-as.dist(out)
    pos<-1
    envir <- as.environment(pos)
    D_Matrix_Results<-NULL
    assign("D_Matrix_Results", out, envir = envir)
    tkdestroy(e$wnd)
}
  
# General Hierarchical Clustering
layout.cluster<-function(e){
  dist.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(dist) if(class(get(dist))[1] == "dist")c(unlist(dist)))),"Load User File")
  
  #Data Model: enviroment called e
  e$dataName<-tclVar("Choose Data")
  e$distancename<-tclVar("Choose Data")
  e$dataFrame<-tclVar("NULL")
  e$clustertype<-tclVar("Average")
  e$distance<-tclVar("NULL")
  
  #Begin GUI Setup
  tkwm.title(e$wnd, "zooaRch")
  tkconfigure(e$layout, text = "Hierarchical Clustering")
  columnConfig(e$layout)
  
  #Distance Matrix Combobox
  put_label(e$layout, "Distance matrix:",1,0,sticky="w")
  data_combo <- ttkcombobox(e$layout, state = "readonly", 
                            values = dist.fun(), 
                            textvariable = e$distancename)
  tkgrid(data_combo, row = 1, column = 1, sticky="w", padx = 2)
  tkbind(data_combo, "<<ComboboxSelected>>", function() updateDataFrame(e, e$distancename, "distance"))    
  tkfocus(data_combo)
  
  put_label(e$layout, "Linkage Type:", 2,0, sticky="w")
  rb_frame<-ttkframe(e$layout)
  rb_frame2<-ttkframe(e$layout)
  rb_frame3<-ttkframe(e$layout)
  rb_frame4<-ttkframe(e$layout)
  rb_frame5<-ttkframe(e$layout)
  rb_frame6<-ttkframe(e$layout)
  rb_frame7<-ttkframe(e$layout)
  rb_frame8<-ttkframe(e$layout)
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
}

run.cluster<-function(e) {
    D_Matrix_Results<-NULL
    if(class(e$distance)!="dist"){
      stop("Input must be a distance matrix. See ?as.dist",tkdestroy(e$wnd))
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
    tkdestroy(e$wnd)
}

# prcomp Function
layout.prcomp<-function(e){
  e$plotcheck<-tclVar("0")
  
  #Begin GUI Setup
  tkwm.title(e$wnd, "PCA Function")
  tkconfigure(e$layout, text = "PCA Function")
  tkconfigure(e$varLabel[[1]], text = "Choose Data:")

  #Plot Data
  put_label ( e$layout , "plot data:" , 5 , 0,sticky = "w")
  plot_check1 <-ttkcheckbutton (e$layout , variable = e$plotcheck)
  tkgrid (plot_check1 , row = 5 , column = 1 , sticky = "w" ,padx = 2)
}

run.prcomp<-function(e){
    #Error Checking
    if(sum(sapply(e$dataFrame, is.numeric))==FALSE) {
      tkdestroy(e$wnd)
      stop("The data set must not contain characters!")
    }
    if(sum(sapply(e$dataFrame, is.na))==TRUE) {
      tkdestroy(e$wnd)
      stop("The data set contains missing values!")
    }
    #Using the prcomp Function
    Value1 <- getVarName(e, 1)
    out<-prcomp(e$dataFrame[,Value1])
    if(as.numeric(tclvalue(e$plotcheck))>0) {
      if(length(Value1)>1) {
        plot(out$x[,c(1,2)])
      }
      else {
        tkdestroy(e$wnd)
        stop("There must be more than one column selected!")
      }
    }
    print(summary(out))
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    tkdestroy(e$wnd)
}

#Correspondence Analysis Function
layout.correspondence<-function(e){
  e$plotcheck<-tclVar("0")
 
  #Begin GUI setup
  tkwm.title(e$wnd, "Correspondence Analysis")
  tkconfigure(e$layout, text = "Correspondence Analysis")
  tkconfigure(e$varLabel[[1]], text = "Choose Data:")
  tkconfigure(e$varLabel[[2]], text = "Choose Label:")
  
  #Plot Data
  put_label ( e$layout , "plot data:" , 7 , 0,sticky = "w")
  plot_check1 <-ttkcheckbutton (e$layout , variable = e$plotcheck)
  tkgrid (plot_check1 , row = 7 , column = 1 , sticky = "w" ,padx = 2)
}

run.correspondence<-function(e){
    #Error checking
    if(sum(sapply(e$dataFrame, is.numeric))==FALSE) {
      tkdestroy(e$wnd)
      run.correspondence()
      stop("The data set must not contain characters!")
    }
    if(sum(sapply(e$dataFrame, is.na))==TRUE) {
      tkdestroy(e$wnd)
      stop("The data set contains missing values!")
    }
    #Using the Correspondence Function
    # print(ncol(e$dataFrame)) # this needs to be deleted
    Value1 <- getVarName(e, 1)
    Value2 <- getVarName(e, 2)
    rownames(e$dataFrame)<-as.character(e$dataFrame[,Value2])
    out<-corresp(e$dataFrame[,Value1]
                 , nf = (as.numeric(ncol(e$dataFrame[,Value1]))-1)
                 ,data=e$dataFrame)
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
        tkdestroy(e$wnd)
        stop("There must be more than one column selected!")
      }
    }
    out$varmat<-varmat
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) # added by EOC
    cat(paste("total inertia =", round(sum(eig),3)),"\n")
    print(varmat) # Changed the printting function to varmat
    tkdestroy(e$wnd)
}
  
# PCoA Function
layout.PCoA<-function(e){
  e$type<-tclVar("Euclidean")
  

  #Begin GUI Setup
  tkwm.title(e$wnd, "Principal Coordinates Analysis")
  tkconfigure(e$layout, text = "Correspondence Analysis")
  tkconfigure(e$varLabel[[1]], text = "Choose Data:")
  
  #Distance Type Selection Radiobuttons
  put_label(e$layout, "Distance Type", 2,0, sticky="w")
  rb_frame<-ttkframe(e$layout)
  sapply(c("Canberra", "Euclidean", "Jaccard", "Manhattan"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 2, column = 1, sticky = "w")
}

run.PCoA<-function(e){
    #Error Checking
    if(sum(sapply(e$dataFrame, is.numeric))==FALSE) {
      tkdestroy(e$wnd)
      stop("The data set must not contain characters!")
    }
    if(sum(sapply(e$dataFrame, is.na))==TRUE) {
      tkdestroy(e$wnd)
      stop("The data set contains missing values!")
    }
    #Calculating the Distance
    Value1 <- getVarName(e, 1)
    switch(tclvalue(e$type),
           Canberra = {out<-as.matrix(dist(e$dataFrame[,Value1],method="canberra"))
           print("Canberra")},
           Euclidean = {out<-as.matrix(dist(e$dataFrame[,Value1],method="euclidean"))
           print("Euclidean")},
           Jaccard = {out<-as.matrix(vegdist(e$dataFrame[,Value1],method="jaccard"))
           print("Jaccard")},
           Manhattan = {out<-as.matrix(dist(e$dataFrame[,Value1],method="manhattan"))
           print("Manhattan")}
    )
    suppressWarnings(out<-cmdscale(out, k = as.numeric(nrow(e$dataFrame[,Value1]))-1,eig = TRUE))
    out$eig<-out$eig[-which(out$eig<10^-16)]
    varmat<-rbind(eig<-out$eig, eig/sum(eig), cumsum(eig/sum(eig)))
    rownames(varmat)<-c("Eigenvalues", "Percent Explained", "Cum Percent Explained")
    colnames(varmat)<-paste("Axis",1:length(out$eig))
    dimnames(out$points)<-list(row.names(e$dataFrame),paste("Axis",1:length(out$eig)) )
    out$varmat<-varmat
    pos<-1
    envir <- as.environment(pos)
    assign("PCoA_Results", out, envir = envir)
    plot(out$points, pch=21, cex=1.5,asp=1,xlab="PCo I", ylab="PCo II")
    print(out$varmat)
    tkdestroy(e$wnd)
}
  
# NMDS Function
layout.NMDS<-function(e){
  e$dataname <- tclVar("Choose one")
  e$dataFrame <- tclVar("NULL")
  e$inpults1 <- tclVar("NULL")
  e$inpults2 <- sort(colnames(e$dataFrame))
  e$type<-tclVar("Euclidean")
  e$kval<-tclVar(3)
  e$shepard<-tclVar(FALSE)
  e$labels<-tclVar(FALSE)
  e$plot<-tclVar(TRUE)
  
  #Begin GUI Setup
  tkwm.title(e$wnd, "zooaRch")
  tkconfigure(e$layout, text = "Non-Metric Multidimensional Scaling")
  columnConfig(e$layout)
  
  #Data Combobox
  put_label(e$layout, "Data:",0,0,sticky="w")
  data_combo <- ttkcombobox(e$layout, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataname)
  tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2)
  tkbind(data_combo, "<<ComboboxSelected>>", function(){
           updateDataFrame(e, e$dataname)
           e$inputs1 = 1:(as.numeric(nrow(e$dataFrame))-1)
		   e$inputs2 = sort(colnames(e$dataFrame))
           tkdelete(e$list1, 0, "end")
		   tkdelete(e$list2, 0, "end")
		   
           for (input in e$inputs1){
               tkinsert(e$list1, "end", input)
           }
           tkselection.set(e$list1, 0)
		   
		    for (input in e$inputs2){
               tkinsert(e$list2, "end", input)
           }
           tkselection.set(e$list2, 0)
		   
    })	
  tkfocus(data_combo)                      # give focus
  
  #K Value Combobox
  put_label(e$layout, "Dimensions:",1,0,sticky="w")
  e$list1 <- smartListbox(e$layout, NULL)
  tkgrid(e$list1, row = 1, column = 1, sticky="w", padx = 2)
  tkfocus(e$list1)
  
  #Distance Type Selection Radiobuttons
  put_label(e$layout, "Distance Type", 2,0, sticky="w")
  rb_frame<-ttkframe(e$layout)
  sapply(c("Canberra", "Euclidean", "Jaccard", "Manhattan"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 2, column = 1, sticky = "w")
  
  #Data Choice Listbox
  put_label(e$layout, "Choose Data:",row=3,column=0,sticky="w")
  e$list2 <- smartListbox(e$layout, e$inpults2)
  tkgrid(e$list2, padx = 2, pady = c(5, 10),row = 3, column = 1)
  
  #Plot Checkbox
  put_label ( e$layout , "NMDS plot:" , 6 , 0,sticky = "w")
  plot_check1 <-ttkcheckbutton (e$layout , variable = e$plot)
  tkgrid (plot_check1 , row = 6 , column = 1 , sticky = "w" ,padx = 2)
  
  #Labels Checkbox
  put_label ( e$layout , "Labels:" , 6 , 2,sticky = "w")
  plot_check1 <-ttkcheckbutton (e$layout , variable = e$labels)
  tkgrid (plot_check1 , row = 6 , column = 3 , sticky = "w" ,padx = 2)
  
  #Shepard Checkbox
  put_label ( e$layout , "Shepard plot:" , 6 , 4,sticky = "w")
  plot_check1 <-ttkcheckbutton (e$layout , variable = e$shepard)
  tkgrid (plot_check1 , row = 6 , column = 5 , sticky = "w" ,padx = 2)
}

run.NMDS<-function(e){
    #Error Checking
    if(sum(sapply(e$dataFrame, is.numeric))==FALSE) {
      tkdestroy(e$wnd)
      stop("The data set must not contain characters!")
    }
    if(sum(sapply(e$dataFrame, is.na))==TRUE) {
      tkdestroy(e$wnd)
      stop("The data set contains missing values!")
    }
    #Calculating the Distance
	Value1 <- e$inputs2[as.numeric(tkcurselection(e$list2)) + 1]
	kval <- e$inputs1[as.numeric(tkcurselection(e$list1)) + 1]
    switch(tclvalue(e$type),
           Canberra = {#dist_value<-(dist(e$dataFrame[,Value1],method="canberra"))
             #PCoA_val<-cmdscale(dist_value, k = (as.numeric(nrow(e$dataFrame[,Value1]))-1))
             nmds_value<-metaMDS(e$dataFrame[,Value1], k = as.numeric(tclvalue(kval)), distance = "canberra", autotransform = F)
           },
           Euclidean = {#dist_value<-as.matrix(dist(e$dataFrame[,Value1],method="euclidean"))
             #PCoA_val<-cmdscale(dist_value, k = (as.numeric(nrow(e$dataFrame[,Value1]))-1))
             nmds_value<-metaMDS(e$dataFrame[,Value1], k = as.numeric(tclvalue(kval)), distance = "euclidean", autotransform = F)
           },
           Jaccard = {#dist_value<-as.matrix(dist(e$dataFrame[,Value1],method="jaccard"))
             #PCoA_val<-cmdscale(dist_value, k = (as.numeric(nrow(e$dataFrame[,Value1]))-1))
             nmds_value<-metaMDS(e$dataFrame[,Value1], k = as.numeric(tclvalue(kval)), distance = "jaccard", autotransform = F)
           },
           Manhattan = {#dist_value<-as.matrix(dist(e$dataFrame[,Value1],method="manhattan"))
             #PCoA_val<-cmdscale(dist_value, k = (as.numeric(nrow(e$dataFrame[,Value1]))-1))
             nmds_value<-metaMDS(e$dataFrame[,Value1], k = as.numeric(tclvalue(kval)), distance = "manhattan", autotransform = F)}
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
      text(new_value,labels =row.names(e$dataFrame[,Value1]))
    }
    if(shep==TRUE) {
      dev.new()
      stressplot(out)
    }
    tkdestroy(e$wnd)
}
  
# 2-way Mantel Function
layout.mantel2way<-function(e){
  e$dataName <- tclVar("Choose dataFrame")
  e$dataName2<-tclVar("Choose dataFrame")
  e$dataFrame <- tclVar("NULL") 
  e$dataFrame2<-tclVar("NULL")
  e$permutations<-tclVar(999)
  e$type1<-tclVar("Euclidean")
  e$type2<-tclVar("Euclidean")
  
  #Begin GUI Setup
  tkwm.title(e$wnd, "2 Matrix Mantel Test")
  tkconfigure(e$layout, text = "2 Matrix Mantel")
  columnConfig(e$layout)
  
  #First Data Combobox
  put_label(e$layout, "First dataFrame:",0,0,sticky="w")
  data_combo <- ttkcombobox(e$layout, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataName)
  tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2)
  tkbind(data_combo, "<<ComboboxSelected>>", function(){
           updateDataFrame(e, e$dataName)

           e$inputs1 = sort(colnames(e$dataFrame))
           tkdelete(e$list1, 0, "end")
           for (input in e$inputs1){
               tkinsert(e$list1, "end", input)
           }
           tkselection.set(e$list1, 0)
    })
    tkfocus(data_combo)

  #Second Data Combobox
  put_label(e$layout, "Second dataFrame:",2,0,sticky="w") 
  data_combo2 <- ttkcombobox(e$layout, state = "readonly", 
                             values = dfs.fun(), 
                             textvariable = e$dataName2)
  tkgrid(data_combo2, row = 2, column = 1, sticky="w", padx = 2)
  tkbind(data_combo2, "<<ComboboxSelected>>", function(){
           updateDataFrame(e, e$dataName2, "dataFrame2")

           e$inputs2 = sort(colnames(e$dataFrame2))
           tkdelete(e$list2, 0, "end")
           for (input in e$inputs2){
               tkinsert(e$list2, "end", input)
           }
           tkselection.set(e$list2, 0)
    })
    tkfocus(data_combo2)                      

    #First dataFrame Listbox
    put_label(e$layout, "First Matrix:",row=4,column=0,sticky="n")
    e$list1 <- smartListbox(e$layout, sort(colnames(e$dataFrame)))
    tkgrid(e$list1, padx = 10, pady = c(5, 10),row = 5, column = 0)

    #Second dataFrame Listbox
    put_label(e$layout, "Second Matrix:", row = 4, column = 1, sticky = "n")
    e$list2 <- smartListbox(e$layout, sort(colnames(e$dataFrame2)))
    tkgrid(e$list2, padx = 10, pady = c(5, 10), row = 5, column = 1)

    #Method Selection
  #First Matrix Radiobuttons
  put_label(e$layout, "First Matrix Method:", 7,0, sticky="w")
  rb_frame<-ttkframe(e$layout)
  sapply(c("Canberra", "Euclidean", "Jaccard", "Manhattan"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type1, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 7, column = 1, sticky = "w")
  
  #Second Matrix Radiobuttons
  put_label(e$layout, "Second Matrix Method:", 8,0, sticky="w")
  rb_frame<-ttkframe(e$layout)
  sapply(c("Canberra", "Euclidean", "Jaccard", "Manhattan"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type2, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 8, column = 1, sticky = "w")
  
  #Permutation Slider
  put_label(e$layout, "Permutations:", 9, 0, sticky = "w")
  conf_level_frame <- ttkframe(e$layout)
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
}

run.mantel2way<-function(e){
    #Using Selected Values
    Value_First <- e$inputs1[as.numeric(tkcurselection(e$list1)) + 1]
    Value_Second <- e$inputs2[as.numeric(tkcurselection(e$list2)) + 1]
    
    #Error Checking
    if(sum(sapply(e$dataFrame[,Value_First], is.numeric))==FALSE) { 
      print("First matrix must not contain characters!")
      tkdestroy(e$wnd)
      run.mantel2way(e)
      stop(call. = FALSE)
    }
    if(sum(sapply(e$dataFrame, is.na))==TRUE) {
      print("The first data set is missing values!")
      tkdestroy(e$wnd)
      run.mantel2way(e)
      stop(call. = FALSE)
    }
    if(sum(sapply(e$dataFrame2[,Value_Second], is.numeric))==FALSE) { 
      print("Second matrix must not contain characters!")
      tkdestroy(e$wnd)
      run.mantel2way(e)
      stop(call. = FALSE)
    }
    if(sum(sapply(e$dataFrame2, is.na))==TRUE) {
      print("The second data set is missing values!")
      tkdestroy(e$wnd)
      run.mantel2way(e)
      stop(call. = FALSE)
    }
    #Distance Functions
    switch(tclvalue(e$type1),
           Canberra = {dist1<-as.matrix(dist(e$dataFrame[,Value_First ],method="canberra"))
           },
           Euclidean = {dist1<-as.matrix(dist(e$dataFrame[,Value_First ],method="euclidean"))
           },
           Jaccard = {dist1<-as.matrix(vegdist(e$dataFrame[,Value_First ],method="jaccard"))
           },
           Manhattan = {dist1<-as.matrix(dist(e$dataFrame[,Value_First ],method="manhattan"))
           }
    )
    switch(tclvalue(e$type2),
           Canberra = {dist2<-as.matrix(dist(e$dataFrame2[,Value_Second],method="canberra"))
           },
           Euclidean = {dist2<-as.matrix(dist(e$dataFrame2[,Value_Second],method="euclidean"))
           },
           Jaccard = {dist2<-as.matrix(vegdist(e$dataFrame2[,Value_Second],method="jaccard"))
           },
           Manhattan = {dist2<-as.matrix(dist(e$dataFrame2[,Value_Second],method="manhattan"))}
    )
    dist1[is.na(dist1)]<-0
    dist2[is.na(dist2)]<-0
    out<-mantel(xdis=dist1,ydis=dist2, permutations=as.numeric(tclvalue(e$permutations)),na.rm = TRUE)
    print(out)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    tkdestroy(e$wnd)
}
  
# 3 Way Mantel Function
layout.mantel3<-function(e){
  e$dataName <- tclVar("Choose dataFrame")
  e$dataName2<-tclVar("Choose dataFrame")
  e$dataName3<-tclVar("Choose dataFrame")
  e$dataFrame <- tclVar("NULL") 
  e$dataFrame2<-tclVar("NULL")
  e$dataFrame3<-tclVar("NULL")
  e$permutations<-tclVar(999)
  e$type1<-tclVar("Euclidean")
  e$type2<-tclVar("Euclidean")
  e$type3<-tclVar("Euclidean")
  
  #Begin GUI Setup
  tkwm.title(e$wnd, "3 Matrix Mantel Test")
  tkconfigure(e$layout, text = "3 Matrix Mantel")
  columnConfig(e$layout)
  
  #First Data Combobox
  put_label(e$layout, "First dataFrame:",0,0,sticky="w")
  data_combo <- ttkcombobox(e$layout, state = "readonly", 
                            values = dfs.fun(), 
                            textvariable = e$dataName)
  tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2)
  tkbind(data_combo, "<<ComboboxSelected>>", function(){
           updateDataFrame(e, e$dataName)

           e$inputs1 = sort(colnames(e$dataFrame))
           tkdelete(e$list1, 0, "end")
           for (input in e$inputs1){
               tkinsert(e$list1, "end", input)
           }
           tkselection.set(e$list1, 0)
    })
    tkfocus(data_combo)

  #Second Data Combobox
  put_label(e$layout, "Second dataFrame:",2,0,sticky="w") 
  data_combo2 <- ttkcombobox(e$layout, state = "readonly", 
                             values = dfs.fun(), 
                             textvariable = e$dataName2)
  tkgrid(data_combo2, row = 2, column = 1, sticky="w", padx = 2)
  tkbind(data_combo2, "<<ComboboxSelected>>", function(){
           updateDataFrame(e, e$dataName2, "dataFrame2")

           e$inputs2 = sort(colnames(e$dataFrame2))
           tkdelete(e$list2, 0, "end")
           for (input in e$inputs2){
               tkinsert(e$list2, "end", input)
           }
           tkselection.set(e$list2, 0)
    })
    tkfocus(data_combo2)                      

  #Third Data Combobox
  put_label(e$layout, "Third dataFrame:",3,0,sticky="w") 
  data_combo3 <- ttkcombobox(e$layout, state = "readonly", 
                             values = dfs.fun(), 
                             textvariable = e$dataName3)
  tkgrid(data_combo3, row = 3, column = 1, sticky="w", padx = 2)
  tkbind(data_combo3, "<<ComboboxSelected>>", function(){
           updateDataFrame(e, e$dataName3, "dataFrame3")

           e$inputs3 = sort(colnames(e$dataFrame3))
           tkdelete(e$list3, 0, "end")
           for (input in e$inputs3){
               tkinsert(e$list3, "end", input)
           }
           tkselection.set(e$list3, 0)
    })
    tkfocus(data_combo3)                      

  #First dataFrame Listbox
    put_label(e$layout, "First Matrix:",row=4,column=0,sticky="n")
    e$list1 <- smartListbox(e$layout, sort(colnames(e$dataFrame)))
    tkgrid(e$list1, padx = 10, pady = c(5, 10),row = 5, column = 0)

    #Second dataFrame Listbox
    put_label(e$layout, "Second Matrix:", row = 4, column = 1, sticky = "n")
    e$list2 <- smartListbox(e$layout, sort(colnames(e$dataFrame2)))
    tkgrid(e$list2, padx = 10, pady = c(5, 10), row = 5, column = 1)

    #Third dataFrame Listbox
    put_label(e$layout, "Third Matrix:", row = 4, column = 2, sticky = "n")
    e$list3 <- smartListbox(e$layout, sort(colnames(e$dataFrame3)))
    tkgrid(e$list3, padx = 10, pady = c(5, 10), row = 5, column = 2)

  #Method Selection
  #Matrix 1 Radiobuttons
  put_label(e$layout, "Third Matrix Method:", 7,0, sticky="w")
  rb_frame<-ttkframe(e$layout)
  sapply(c("Canberra", "Euclidean", "Jaccard", "Manhattan"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type1, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 7, column = 1, sticky = "w")
  
  #Matrix 2 Radiobuttons
  put_label(e$layout, "Second Matrix Method:", 8,0, sticky="w")
  rb_frame<-ttkframe(e$layout)
  sapply(c("Canberra", "Euclidean", "Jaccard", "Manhattan"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type2, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 8, column = 1, sticky = "w")
  
  #Matrix 3 Radiobuttons
  put_label(e$layout, "Third Matrix Method:", 9,0, sticky="w")
  rb_frame<-ttkframe(e$layout)
  sapply(c("Canberra", "Euclidean", "Jaccard", "Manhattan"), function(i) {
    radio_button<-tk2radiobutton(rb_frame, variable = e$type3, text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 9, column = 1, sticky = "w")
  
  #Permutation Slider
  put_label(e$layout, "Permutations:", 10, 0, sticky = "w")
  conf_level_frame <- ttkframe(e$layout)
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
}

run.mantel3<-function(e){
    #Using Selected Values
    Value_First <- e$inputs1[as.numeric(tkcurselection(e$list1)) + 1]
    Value_Second <- e$inputs2[as.numeric(tkcurselection(e$list2)) + 1]
    Value_Third <- e$inputs3[as.numeric(tkcurselection(e$list3)) + 1]
    
    #Error Checking
    if(sum(sapply(e$dataFrame[,Value_First], is.numeric))==FALSE) { 
      print("First matrix must not contain characters!")
      tkdestroy(e$wnd)
      run.mantel3(e)
      stop(call. = FALSE)
    }
    if(sum(sapply(e$dataFrame, is.na))==TRUE) {
      print("The first data set is missing values!")
      tkdestroy(e$wnd)
      run.mantel3(e)
      stop(call. = FALSE)
    }
    if(sum(sapply(e$dataFrame2[,Value_Second], is.numeric))==FALSE) { 
      print("Second matrix must not contain characters!")
      tkdestroy(e$wnd)
      run.mantel3(e)
      stop(call. = FALSE)
    }
    if(sum(sapply(e$dataFrame2, is.na))==TRUE) {
      print("The second data set is missing values!")
      tkdestroy(e$wnd)
      run.mantel3(e)
      stop(call. = FALSE)
    }
    if(sum(sapply(e$dataFrame3[,Value_Third], is.numeric))==FALSE) {
      print("Third matrix must not contain characters!")
      tkdestroy(e$wnd)
      run.mantel3(e)
      stop(call. = FALSE)
    }
    if(sum(sapply(e$dataFrame3, is.na))==TRUE) {
      print("The third data set is missing values!")
      tkdestroy(e$wnd)
      run.mantel3(e)
      stop(call. = FALSE)
    }
    #Distance Functions
    switch(tclvalue(e$type1),
           Canberra = {dist1<-as.matrix(dist(e$dataFrame[,Value_First ],method="canberra"))
           },
           Euclidean = {dist1<-as.matrix(dist(e$dataFrame[,Value_First ],method="euclidean"))
           },
           Jaccard = {dist1<-as.matrix(vegdist(e$dataFrame[,Value_First ],method="jaccard"))
           },
           Manhattan = {dist1<-as.matrix(dist(e$dataFrame[,Value_First ],method="manhattan"))
           }
    )
    switch(tclvalue(e$type2),
           Canberra = {dist2<-as.matrix(dist(e$dataFrame2[,Value_Second],method="canberra"))
           },
           Euclidean = {dist2<-as.matrix(dist(e$dataFrame2[,Value_Second],method="euclidean"))
           },
           Jaccard = {dist2<-as.matrix(vegdist(e$dataFrame2[,Value_Second],method="jaccard"))
           },
           Manhattan = {dist2<-as.matrix(dist(e$dataFrame2[,Value_Second],method="manhattan"))}
    )
    switch(tclvalue(e$type3),
           Canberra = {dist3<-as.matrix(dist(e$dataFrame3[,Value_Third],method="canberra"))
           },
           Euclidean = {dist3<-as.matrix(dist(e$dataFrame3[,Value_Third],method="euclidean"))
           },
           Jaccard = {dist3<-as.matrix(vegdist(e$dataFrame3[,Value_Third],method="jaccard"))
           },
           Manhattan = {dist3<-as.matrix(dist(e$dataFrame3[,Value_Third],method="manhattan"))}
    )
    dist1[is.na(dist1)]<-0
    dist2[is.na(dist2)]<-0
    dist3[is.na(dist3)]<-0
    out<-mantel.partial(xdis = dist1,ydis = dist2,zdis = dist3, permutations=as.numeric(tclvalue(e$permutations)), na.rm = TRUE)  
    print(out)
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir)
    tkdestroy(e$wnd)
}
  
# Multivariate Vector-fitting Function
layout.multivarfit<-function(e){
  matrix.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(matrix) if(class(get(matrix))[1] == "matrix")c(unlist(matrix)))), "Load User File")
  
  e$dataName<-tclVar("Choose Ordination")
  e$dataName2<-tclVar("Choose Variabales")
  e$dataFrame<-tclVar("NULL")
  e$dataFrame2<-tclVar("NULL")
  e$permutations<-tclVar(999)
  e$choice<-tclVar(2)
  
  #Begin GUI setup
  tkwm.title(e$wnd, "zooaRch")
  tkconfigure(e$layout, text = "Multivariate Association Using Vector Fitting")
  columnConfig(e$layout)
  
  #First Data Combobox
  put_label(e$layout, "Ordination:",0,0,sticky="w")
  data_combo <- ttkcombobox(e$layout, state = "readonly", 
                            values = c(matrix.fun()[-length(matrix.fun())],dfs.fun()), 
                            textvariable = e$dataName)
  tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2, pady = 2)
  tkbind(data_combo, "<<ComboboxSelected>>", function(){
           updateDataFrame(e, e$dataName)

           e$inputs = (2:ncol(e$dataFrame))
           tkdelete(e$list1, 0, "end")
           for (input in e$inputs){
               tkinsert(e$list1, "end", input)
           }
           tkselection.set(e$list1, 0)
  })
  tkfocus(data_combo)

  #Second Data Combobox
  put_label(e$layout, "Variables:",2,0,sticky="w") 
  data_combo2 <- ttkcombobox(e$layout, state = "readonly", 
                             values = c(matrix.fun()[-length(matrix.fun())],dfs.fun()), 
                             textvariable = e$dataName2)
  tkgrid(data_combo2, row = 2, column = 1, sticky="w", padx = 2, pady = 2)
  tkbind(data_combo2, "<<ComboboxSelected>>", function() updateDataFrame(e, e$dataName2))    
  tkfocus(data_combo2)                      

  #Choice Combobox
  put_label(e$layout, "N Axes:", 4, 0, sticky = "w")
  e$list1 <- smartListbox(e$layout, (2:ncol(e$dataFrame)))
  #choice_combo<-ttkcombobox(e$layout, state = "readonly", values = (2:ncol(e$dataFrame)), textvariable = e$choice)
  tkgrid(e$list1, row = 4, column = 1, sticky = "w", padx = 2, pady = 2)
  tkfocus(e$list1)
  
  #Permutation Slider
  put_label(e$layout, "Permutations:", 7, 0, sticky = "w")
  conf_level_frame <- ttkframe(e$layout)
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
}

run.multivarfit<-function(e){
    if(sum(sapply(e$dataFrame, is.numeric))==FALSE) {
      print("First matrix must be numeric!")
      tkdestroy(e$wnd)
      run.multivarfit()
    }
    fit_value <- envfit(ord = e$dataFrame,  env=e$dataFrame2, choices= c(1:as.numeric(e$inputs[as.numeric(tkcurselection(e$list1)) + 1])), permutations=as.numeric(tclvalue(e$permutations)),na.rm = TRUE) 
    #    out<-fit_value
    
    #Plotting 
    plot(e$dataFrame[,1], e$dataFrame[,2], type="n",xlab="PCoA I",ylab="PCoA II")
    text(e$dataFrame[,1], e$dataFrame[,2], label=row.names(e$dataFrame), cex=.55,col="gray")
    plot(fit_value,col="red",lwd=3,cex=1.3)
    out<-fit_value
    pos<-1
    envir <- as.environment(pos)
    assign("Vectorfit_Results", out, envir = envir)
    print(out)
    tkdestroy(e$wnd)
}
  
# k-means Single
layout.kmeans_single<-function(e){
    matrix.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(matrix) if(class(get(matrix))[1] == "matrix")c(unlist(matrix)))),"Load User File")
    df.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(matrix) if(class(get(matrix))[1] == "data.frame")c(unlist(matrix)))))
    e$dataName<-tclVar("Choose Data")
    e$datamatrix<-tclVar("NULL")
    e$clusternum<-tclVar(2)
    e$plot<-tclVar("NULL")

    #Begin GUI Setup
    tkwm.title(e$wnd, "zooaRch")
    tkconfigure(e$layout, text = "Single K-Means Clustering")
    columnConfig(e$layout)

    #Data Combobox
    put_label(e$layout, "Input Data:",1,0,sticky="w")
    data_combo <- ttkcombobox(e$layout, state = "readonly", 
                              values = c(matrix.fun(),df.fun()), 
                              textvariable = e$dataName)
    tkgrid(data_combo, row = 1, column = 1, sticky="w", padx = 2)
    tkbind(data_combo, "<<ComboboxSelected>>", function(){
           updateDataFrame(e, e$dataName, "datamatrix")

           e$inputs = sort(colnames(e$datamatrix))
           tkdelete(e$list1, 0, "end")
           for (input in e$inputs){
               tkinsert(e$list1, "end", input)
           }
           tkselection.set(e$list1, 0)
    })
    tkfocus(data_combo)

    #First dataFrame Listbox
    put_label(e$layout, "Variables:",row=2,column=0,sticky="w")
    e$list1 <- smartListbox(e$layout, sort(colnames(e$datamatrix)))
    tkgrid(e$list1, padx = 2, pady = c(5, 10),row = 2, column = 1,sticky="w")

    #Number of Clusters Spinbox
    put_label(e$layout, "Number of Clusters:", 3, 0, sticky = "w")
    conf_level_frame <- ttkframe(e$layout)
    conf_level_frame <- ttkframe(e$layout)
    tkgrid(conf_level_frame, row = 3, column = 1, columnspan = 2, 
           sticky = "w")
    tkspinbox <- function(parent, ...)
        tkwidget(parent, "tk::spinbox", ...)
    conf_level_spin <- tkspinbox(conf_level_frame, 
                                 from = 2, to = 20, increment = 1, 
                                 textvariable = e$clusternum, width = 5)
    tkpack(conf_level_spin, side = "left")

    #Scree Plot Checkbox
    put_label ( e$layout , "Plot:" , 4 , 0, sticky = "w")
    progress_check <-ttkcheckbutton (e$layout , variable = e$plot)
    tkgrid (progress_check , row = 4 , column = 1 , sticky = "w" ,padx = 2)
}

run.kmeans_single<-function(e) {
    #Numeric Error Checking
    #Using Selected Values
    Value_First <- e$inputs[as.numeric(tkcurselection(e$list1)) + 1]
    e$datamatrix<-e$datamatrix[,Value_First]
    #Error Checking
    if(sum(sapply(e$datamatrix[,Value_First], is.numeric))==FALSE) { 
        print("First matrix must not contain characters!")
        tkdestroy(e$wnd)
        run.kmeans_single(e)
        stop(call. = FALSE)
    }
    if(sum(sapply(e$datamatrix, is.na))==TRUE) {
        print("The first data set is missing values!")
        tkdestroy(e$wnd)
        run.kmeans_single(e)
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
    tkdestroy(e$wnd)
}

# k-means Multi
layout.kmeans_multi<-function(e){
    matrix.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(matrix) if(class(get(matrix))[1] == "matrix")c(unlist(matrix)))),"Load User File")
    df.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(matrix) if(class(get(matrix))[1] == "data.frame")c(unlist(matrix)))))

    e$dataName<-tclVar("Choose Data")
    e$datamatrix<-tclVar("NULL")
    e$clustermin<-tclVar(1)
    e$clustermax<-tclVar(5)
    e$iterations<-tclVar(1000)
    e$nstart<-tclVar(100)
    e$verbose<-tclVar("NULL")
    e$plot<-tclVar("NULL")

    #Begin GUI Setup
    tkwm.title(e$wnd, "kmeans_single")
    tkconfigure(e$layout, text = "Single K-Means Clustering")
    columnConfig(e$layout)

    #Data Combobox
    put_label(e$layout, "Input Data:",1,0,sticky="w")
    data_combo <- ttkcombobox(e$layout, state = "readonly", 
                              values = c(matrix.fun(),df.fun()), 
                              textvariable = e$dataName)
    tkgrid(data_combo, row = 1, column = 1, sticky="w", padx = 2)
    tkbind(data_combo, "<<ComboboxSelected>>", function(){
           updateDataFrame(e, e$dataName, "datamatrix")

           e$inputs = sort(colnames(e$datamatrix))
           tkdelete(e$list1, 0, "end")
           for (input in e$inputs){
               tkinsert(e$list1, "end", input)
           }
           tkselection.set(e$list1, 0)
    })
    tkfocus(data_combo)

    #First Dataframe Listbox
    put_label(e$layout, "Variables:",row=2,column=0,sticky="w")
    e$list1 <- smartListbox(e$layout, sort(colnames(e$datamatrix)))
    tkgrid(e$list1, padx = 2, pady = c(5, 10),row = 2, column = 1,sticky="w")

    #Minimum Clusters Spinbox
    put_label(e$layout, "Min Clusters:", 6, 0, sticky = "w")
    conf_level_frame <- ttkframe(e$layout)
    conf_level_frame <- ttkframe(e$layout)
    tkgrid(conf_level_frame, row = 6, column = 1, columnspan = 2, 
           sticky = "w")
    tkspinbox <- function(parent, ...)
        tkwidget(parent, "tk::spinbox", ...)
    conf_level_spin <- tkspinbox(conf_level_frame, 
                                 from = 1, to = 20, increment = 1, 
                                 textvariable = e$clustermin, width = 5)
    tkpack(conf_level_spin, side = "left")

    #Maximum Clusters Spinbox
    put_label(e$layout, "Max Clusters:", 7, 0, sticky = "w")
    conf_level_frame <- ttkframe(e$layout)
    conf_level_frame <- ttkframe(e$layout)
    tkgrid(conf_level_frame, row = 7, column = 1, columnspan = 2, 
           sticky = "w")
    tkspinbox <- function(parent, ...)
        tkwidget(parent, "tk::spinbox", ...)
    conf_level_spin <- tkspinbox(conf_level_frame, 
                                 from = 1, to = 50, increment = 1, 
                                 textvariable = e$clustermax, width = 5)
    tkpack(conf_level_spin, side = "left")

    #Maximum Clusters Spinbox
    put_label(e$layout, "Max Iterations:", 8, 0, sticky = "w")
    conf_level_frame <- ttkframe(e$layout)
    conf_level_frame <- ttkframe(e$layout)
    tkgrid(conf_level_frame, row = 8, column = 1, columnspan = 2, 
           sticky = "w")
    tkspinbox <- function(parent, ...)
        tkwidget(parent, "tk::spinbox", ...)
    conf_level_spin <- tkspinbox(conf_level_frame, 
                                 from = 1, to = 2000, increment = 1, 
                                 textvariable = e$iterations, width = 5)
    tkpack(conf_level_spin, side = "left")

    #nstart Spinbox
    put_label(e$layout, "nstart:", 9, 0, sticky = "w")
    conf_level_frame <- ttkframe(e$layout)
    conf_level_frame <- ttkframe(e$layout)
    tkgrid(conf_level_frame, row = 9, column = 1, columnspan = 2, 
           sticky = "w")
    tkspinbox <- function(parent, ...)
        tkwidget(parent, "tk::spinbox", ...)
    conf_level_spin <- tkspinbox(conf_level_frame, 
                                 from = 1, to = 200, increment = 1, 
                                 textvariable = e$nstart, width = 5)
    tkpack(conf_level_spin, side = "left")

    #Verbose Checkbox
    put_label ( e$layout , "Verbose:" , 10 , 0, sticky = "w")
    progress_check <-ttkcheckbutton (e$layout , variable = e$verbose)
    tkgrid (progress_check , row = 10 , column = 1 , sticky = "w" ,padx = 2)

    #Scree Plot Checkbox
    put_label ( e$layout , "Scree Plot:" , 11 , 0, sticky = "w")
    progress_check <-ttkcheckbutton (e$layout , variable = e$plot)
    tkgrid (progress_check , row = 11 , column = 1 , sticky = "w" ,padx = 2)
}

run.kmeans_multi<-function(e) {
    #Numeric Error Checking
    #Using Selected Values
    Value_First <- e$inputs[as.numeric(tkcurselection(e$list1)) + 1]
    e$datamatrix<-e$datamatrix[,Value_First]
    #Error Checking
    if(sum(sapply(e$datamatrix, is.numeric))==FALSE) { 
        print("First matrix must not contain characters!")
        tkdestroy(e$wnd)
        run.kmeans_single(e)
        stop(call. = FALSE)
    }
    if(sum(sapply(e$datamatrix, is.na))==TRUE) {
        print("The first data set is missing values!")
        tkdestroy(e$wnd)
        run.kmeans_single(e)
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
    tkdestroy(e$wnd)
}

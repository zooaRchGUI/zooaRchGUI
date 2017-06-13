# hist
layout.hist<-function(e){
  e$freq<-tclVar("Frequency")
  
  tkwm.title(e$wnd, "Histogram Dialog")
  tkconfigure(e$layout, text = "Histogram")

  #Y Axis Radiobutton
  put_label(e$layout, "Y-Axis:",2,0,sticky="w")
  rb_frame <- ttkframe(e$layout)
  sapply(c("Frequency","Density"), function(i) {
    radio_button <- tk2radiobutton(rb_frame, variable = e$freq,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 2, column = 1, sticky = "w")
}

run.hist<-function(e){
    varName <- getVarName(e, 1)
    varValue <- get(varName,e$dataFrame,inherits=TRUE)
    assign("varValue", varValue, envir = e)
    altselect <- tclvalue(e$freq)
    freq <- switch(altselect , "Frequency" = altselect , "Density" = altselect)
    assign("freq", altselect, envir = e)
    freq<-freq=="Frequency"
    out<-hist(varValue, freq = freq, xlab=varName, main=paste("Histogram of ",varName))
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    cat("Breaks: ",out$breaks, "\n")
    cat("Counts: ",out$counts, "\n")
    tkdestroy(e$wnd)
}

# simp_box_fun
layout.simp_box<-function(e){
  e$plot<-tclVar(TRUE)
  
  tkwm.title(e$wnd, "Boxplot Dialog")
  tkconfigure(e$layout, text = "Boxplot")
  
  #Plot Data Checkbox
  put_label ( e$layout , "plot data:" , 2 , 0,sticky = "w")
  plot_check <-ttkcheckbutton (e$layout , variable = e$plot)
  tkgrid (plot_check , row = 2 , column = 1 , sticky = "w" ,padx = 2)
}

run.simp_box<-function(e){    
	varName <- getVarName(e, 1)
    varValue <- get(varName,e$dataFrame,inherits=TRUE)
    assign("varValue", varValue, envir = e)
    out<-boxplot(varValue,  main=paste("Boxplot of ",varName))
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    cat("Quantiles: ",quantile(varValue, na.rm = TRUE), "\n")
    cat("IQR: ",IQR(varValue, na.rm = TRUE), "\n")
    if(tclvalue(e$plot)>0){
        stripchart(varValue,pch=16, vert=TRUE, add=TRUE,cex=1)
    }
    tkdestroy(e$wnd)
}

# bivplot_fun
layout.bivplot<-function(e){
  e$asp<-tclVar(0)
  
  tkwm.title(e$wnd, "Bivariate plot Dialog")
  tkconfigure(e$layout, text = "Bivariate plot")
  tkconfigure(e$varLabel[[1]], text = "X-variable:")
  tkconfigure(e$varLabel[[2]], text = "Y-variable:")

  #ASP Ratio
  put_label ( e$layout , "equal aspect ratio:" , 3 , 0,sticky = "e")
  plot_check <-ttkcheckbutton (e$layout , variable = e$asp)
  tkgrid (plot_check , row = 3 , column = 1 , sticky = "w" ,padx = 2)
}

#OK Function
run.bivplot<-function(e){    
	varName1 <- getVarName(e, 1)   
	varValue1 <- get(varName1,e$dataFrame,inherits=TRUE)
    assign("varValue1", varValue1, envir = e)  
	
	varName2 <- getVarName(e, 2)    
	varValue2 <- get(varName2,e$dataFrame,inherits=TRUE)
    assign("varValue2", varValue2, envir = e)
    if(as.numeric(tclvalue(e$asp))>0){
        plot(varValue1,varValue2
             ,xlab=varName1,ylab=varName2
             ,asp=1
             , pch=21,bg="gray")
    } else {
        plot(varValue1,varValue2
             ,xlab=varName1,ylab=varName2
             ,pch=21,bg="gray")
    }
    tkdestroy(e$wnd)
}

# multi_histfun
layout.multi_hist<-function(e){
  e$freq<-tclVar("Frequency")
  
  tkwm.title(e$wnd, "Multiway Histogram Dialog")
  tkconfigure(e$layout, text = "Multiway Histogram")
  tkconfigure(e$varLabel[[1]], text = "Plot Variable:")
  tkconfigure(e$varLabel[[2]], text = "by Factor:")

  #Y Axis Radiobutton
  put_label(e$layout, "Plot Y-Axis as:",3,0,sticky="w")
  rb_frame <- ttkframe(e$layout)
  sapply(c("Frequency","Density"), function(i) {
    radio_button <- tk2radiobutton(rb_frame, variable = e$freq,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 3, column = 1, sticky = "w")
}

run.multi_hist<-function(e){
    #Histogram Function
    hist.formula <- function(formula, data, cols=NULL, rows=NULL,freq=TRUE, ...){
		rows<-rows
		cols<-cols
		print(formula)
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

	varName1 <- getVarName(e, 1)
	varName2 <- getVarName(e, 2)
    f1<-eval(paste(varName1,"~", varName2))
    altselect <- tclvalue(e$freq)
    freq <- switch(altselect , "Frequency" = altselect , "Density" = altselect)
    assign("freq", altselect, envir = e)
    freq<-freq=="Frequency"
    hist.formula(f1,data=e$dataFrame, cols=NULL, rows=NULL,freq=freq)
    #assign("Results", out, envir = .GlobalEnv) 
    #print(anova(out))
    #print(summary(out))
    tkdestroy(e$wnd)
  }
  
# multi_boxplotfun
layout.multi_boxplot<-function(e){
  e$hor<-tclVar("0")
  e$varwid<-tclVar("0")
  e$notch<-tclVar("0")
  e$plot<-tclVar("0")
    
  tkwm.title(e$wnd, "Multiway Boxplot Dialog")
  tkconfigure(e$layout, text = "Multiway Boxplot")
  tkconfigure(e$varLabel[[1]], text = "Plot Variable:")
  tkconfigure(e$varLabel[[2]], text = "by Factor:")
  
  #Plot Data Checkbox
  put_label ( e$layout , "plot data:" , 3 , 0,sticky = "w")
  plot_check1 <-ttkcheckbutton (e$layout , variable = e$plot)
  tkgrid (plot_check1 , row = 3 , column = 1 , sticky = "w" ,padx = 2)
  
  #Plot Notch Checkbox
  put_label ( e$layout , "Notch:" , 3 , 1,sticky = "n")
  plot_check2 <-ttkcheckbutton (e$layout , variable = e$notch)
  tkgrid (plot_check2 , row = 3 , column = 1 , sticky = "e" ,padx = 2)
  
  #Plot N Checkbox
  put_label ( e$layout , "N width:" , 3 , 2,sticky = "w")
  plot_check3 <-ttkcheckbutton (e$layout , variable = e$varwid)
  tkgrid (plot_check3 , row = 3 , column = 2 , sticky = "n" ,padx = 2)
  
  #Plot Horizontal Checkbox
  put_label ( e$layout , "Horizontal:" , 3 , 2,sticky = "e")
  plot_check4 <-ttkcheckbutton (e$layout , variable = e$hor)
  tkgrid (plot_check4 , row = 3 , column = 3 , sticky = "w" ,padx = 2)
}

run.multi_boxplot<-function(e){
    varName1 <- getVarName(e, 1)
	varName2 <- getVarName(e, 2)
	
	f1<-eval(paste(varName1, "~", varName2))
    out<-boxplot(as.formula(f1),
                 data=e$dataFrame, 
                 horizontal = as.logical(as.numeric(tclvalue(e$hor)))
                 ,varwidth=as.logical(as.numeric(tclvalue(e$varwid)))
                 ,outline=TRUE, notch = as.logical(as.numeric(tclvalue(e$notch))))
    if(as.numeric(tclvalue(e$plot))>0){
      stripchart(as.formula(f1)
                 ,data=e$dataFrame
                 ,vertical=as.logical(1-as.logical(as.numeric(tclvalue(e$hor))))
                 ,pch=16, add=TRUE,cex=1)
    }
    pos<-1
    envir <- as.environment(pos)
    assign("Results", out, envir = envir) 
    print("Boxplot statstistics in 'Results' object")
    tkdestroy(e$wnd)
}

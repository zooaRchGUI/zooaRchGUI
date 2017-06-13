# Ripleys K Function
layout.ripleys_K<-function(e) {
    sppdfs.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(SpatialPointsDataFrame) if(class(get(SpatialPointsDataFrame))[1] == "SpatialPointsDataFrame")c(unlist(SpatialPointsDataFrame)))), "Load User File", "Load Shape File")
    list.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(list) if (class(get(list))[1] == "list") c(unlist(list)))), "Load User File", "Load Shape File")

    #Data Model: enviroment called "e"
    e$dataName<-tclVar("Choose Data")
    e$dataFrame<-tclVar("NULL")
    e$permutations<-tclVar(999)
    e$bins<-tclVar(10)
    e$polygon<-tclVar("Custom")
    e$shape<-tclVar("NULL")
    e$polynum<-tclVar("NULL")
    e$shapefile<-tclVar("NULL")

    tkwm.title(e$wnd, "Ripley's K_fun")
    tkconfigure(e$layout, text = "Ripleys K function")

    columnConfig(e$layout)
    #First Data Combobox
    put_label(e$layout, "Input Data:",0,0,sticky="w")
    data_combo <- ttkcombobox(e$layout, state = "readonly", 
                              values = c(sppdfs.fun()[-which(sppdfs.fun()%in%dfs.fun()==TRUE)],dfs.fun()), 
                              textvariable = e$dataName)
    tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2)
    tkbind(data_combo, "<<ComboboxSelected>>", function() updateDataFrame(e, e$dataName))    
    tkfocus(data_combo)

    #Shape Type Radiobuttons
    put_label(e$layout, "Shape Type:", 3,0, sticky="w")
    rb_frame<-ttkframe(e$layout)
    sapply(c("Load", "Custom"), function(i) {
               radio_button<-tk2radiobutton(rb_frame, variable = e$polygon, text = i, value = i)
               tkpack(radio_button, side = "left")
                              })
    tkgrid(rb_frame, row = 3, column = 1, sticky = "w")

    #Bins Slider
    put_label(e$layout, "Number of Bins:", 4, 0, sticky = "w")
    conf_level_frame <- ttkframe(e$layout)
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
    put_label(e$layout, "Permutations:", 5, 0, sticky = "w")
    conf_level_frame <- ttkframe(e$layout)
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
}

#Ok Function
run.ripleys_K<-function(e) {
    if(class(e$dataFrame)=="list"){
        e$dataFrame<-unlist(e$dataFrame)
    }
    if(class(e$dataFrame)=="data.frame"){
        e$dataFrame<-unlist(e$dataFrame)
        e$dataFrame<-as.matrix(e$dataFrame)
        fundata<-cbind(e$dataFrame[,1], e$dataFrame[,2])
        fundata.pts<-splancs::as.points(fundata[,1],fundata[,2])
        print(class(fundata.pts))
        summary(fundata.pts)
        pointmap(fundata.pts,pch="+",cex=2)
    }
    if(class(e$dataFrame)=="SpatialPointsDataFrame"){
        fundata<-e$dataFrame@coords
        fundata.pts<-e$dataFrame@coords
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
                   tkdestroy(e$wnd)
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
    tkdestroy(e$wnd)
}

# Moran's I/Geary C Function
layout.moran_geary<-function(e){
    spdfs.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(SpatialPolygonsDataFrame) if(class(get(SpatialPolygonsDataFrame))[1] == "SpatialPolygonsDataFrame")c(unlist(SpatialPolygonsDataFrame)))), "Load User File", "Load Shape File")
    num.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(numeric) if(class(get(numeric))[1] == "numeric")c(unlist(numeric)))), "Load User File", "Load Shape File")

    #Data Model: enviroment called e
    e$dataName<-tclVar("Choose Data")
    e$dataFrame<-tclVar("NULL")
    e$variable<-tclVar("NULL")
    e$bins<-tclVar(10)
    e$method<-tclVar("Moran")
    e$displayname<-tclVar("")
    e$varname<-tclVar("Choose Variable")

    tkwm.title(e$wnd, "Moransi_fun")
    tkconfigure(e$layout, text = "Moran I Correlogram")
    columnConfig(e$layout)
	
    #First Data Combobox
    put_label(e$layout, "Data:",0,0,sticky="ew")
    data_combo <- ttkcombobox(e$layout, state = "readonly", 
                              values = spdfs.fun(), 
                              textvariable = e$dataName)
    tkgrid(data_combo, row = 0, column = 1, sticky="ew", padx = 2)
    tkbind(data_combo, "<<ComboboxSelected>>", function(){
           updateDataFrame(e, e$dataName)
           inputs = colnames(as.data.frame(e$dataFrame))
           tkdelete(e$list1, 0, "end")
           for (input in inputs){
               tkinsert(e$list1, "end", input)
           }
           tkselection.set(e$list1, 0)
    })
    tkfocus(data_combo)
	
    #First dataFrame Listbox
    put_label(e$layout, "Variable:",row=2,column=0,sticky="ew")
    e$list1 <- smartListbox(e$layout, colnames(as.data.frame(e$dataFrame)))
    tkgrid(e$list1, padx = 2, pady = c(5, 10),row = 1, column = 1,sticky="ew")
    tkbind(e$list1, "<<ComboboxSelected>>", function(){		
               varname <- e$inputs[as.numeric(tkcurselection(e$list1)) + 1]		
               variable <- eval(parse(text = paste("dataFrame$", varname, sep = "")))
               assign("variable", variable, envir = e)
    })
	
    #Method Selection Buttons
    put_label(e$layout, "Method:", 3,0, sticky="w")
    rb_frame<-ttkframe(e$layout)
    sapply(c("Moran", "Geary"), function(i) {
               radio_button<-tk2radiobutton(rb_frame, variable = e$method, text = i, value = i)
               tkpack(radio_button, side = "left")
                               })
    tkgrid(rb_frame, row = 3, column = 1, sticky = "ew")

    #Bins Slider
    put_label(e$layout, "Number of Bins:", 4, 0, sticky = "w")
    conf_level_frame <- ttkframe(e$layout)
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

    #Display Entry
    put_label(e$layout, "Display Title:", 2,0, sticky="w")
    entrybox<-ttkentry(e$layout, textvariable = e$displayname)
    tkgrid(entrybox, row = 3, column = 1, sticky = "ew")
}

#OK Function
run.moran_geary<-function(e) {
    switch(tclvalue(e$method),
           Moran = {corD <- pgirmess::correlog(coordinates(e$dataFrame), e$variable, method = tclvalue(e$method), nbclass = as.numeric(tclvalue(e$bins)))
           plot(corD, main = tclvalue(e$displayname))},
           Geary = {corD <- pgirmess::correlog(coordinates(e$dataFrame), e$variable, method = tclvalue(e$method), nbclass = as.numeric(tclvalue(e$bins)))
           plot(corD, main = tclvalue(e$displayname))}
           )
    tkdestroy(e$wnd)
}


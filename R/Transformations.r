#Zooarchaeological Size Standardization Function
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

# LSI
layout.LSI <- function(e) {
    e$okLabel <- "   Select Files"
    tkwm.title(e$wnd, "Size STD")
    tkconfigure(e$layout, text = "Size STD")

    label1 <- tklabel(e$layout, text="Input Size Data Frame")
    label2 <- tklabel(e$layout, text="Select Standard")
    dfs<-dfs.fun()
    std<-c("bos", "capra", "ovis_popkin", "ovis_uerpman", "sus","user_standard")

    # Default Selections for the Two Comboboxes
    e$Xvar <- tclVar("Choose one")
    e$Yvar <- tclVar("Choose one")
    # 1st box
    combo.1 <- ttkcombobox(e$layout, values=dfs, textvariable=e$Xvar, state="readonly")
    # 2nd box
    combo.2 <- ttkcombobox(e$layout, values=std, textvariable=e$Yvar, state="readonly") 
    tkpack(label1, combo.1)
    tkpack(label2, combo.2)
}

run.LSI <- function(e) {
    Xvar <- tclvalue(e$Xvar)
    Yvar <- tclvalue(e$Yvar)
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
    tkdestroy(e$wnd)

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
    return(data.frame(sizedata, LSIValue = log(rowMeans(prop,na.rm=T), base=exp(1))))
    rm("std_data","sizedata")
}

layout.VSI <- function(e) {
    layout.LSI(e)
}

run.VSI <- function(e){
    Xvar <- tclvalue(e$Xvar)
    Yvar <- tclvalue(e$Yvar)
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
    tkdestroy(e$wnd)

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

# transf_fun
layout.trans<-function(e){
    e$index<-tclVar("log")
    e$indval<-tclVar(0)
    e$append<-tclVar(FALSE)

    tkwm.title(e$wnd, "Tranformation Dialog")
    tkconfigure(e$layout, text = "Tranformation")

    #Div Index Radiobutton
    put_label(e$layout, "Transformation:",2,0,sticky="e")  
    rb_frame <- ttkframe(e$layout)
    sapply(c("log","log10","sqrt","arcsin","prop", "Zscore"), function(i) {
               radio_button <- tk2radiobutton(rb_frame, variable = e$index,text = i, value = i)
               tkpack(radio_button, side = "left")
                                })
    tkgrid(rb_frame, row = 2, column = 1, sticky = "w")

    #Append Checkbox
    put_label ( e$layout , "Append:" , row=3 , column=0,sticky="e")
    plot_check <-ttkcheckbutton (e$layout , variable = e$append)
    tkgrid (plot_check , row = 3 , column = 1 , stick = "w" ,padx = 2)
}

#OK Function
run.trans<-function(e){
    varName<-getVarName(e, 1)
    varValue1 <- get(varName,e$dataFrame,inherits=TRUE)
    assign("varValue1", varValue1, envir = e)
    transfun <- function(x=varValue1, index = tclvalue(e$index)){
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
        eval(parse(text=paste("dataFrame$",tclvalue(e$index),"_",varName,"<-out",sep="")))
        pos<-1
        envir <- as.environment(pos)
        assign(tclvalue(e$dataName), e$dataFrame, envir=envir)
        } else {pos<-1
        envir <- as.environment(pos)
        assign("Results", out, envir = envir) }
    tkdestroy(e$wnd)
}



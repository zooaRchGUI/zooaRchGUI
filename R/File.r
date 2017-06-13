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

# saveas
layout.saveas<-function(e){
  e$format<-tclVar("csv")
  e$dir<-tclVar("Working Directory")
  e$dirval<-tclVar(getwd())
  e$filename<-tclVar("Same as dataFrame Name")
  e$savename<-tclVar("")
  
  tkwm.title(e$wnd, "Save Dialog")
  tkconfigure(e$layout, text = "Save as:")
  
  #Name of File
  put_label(e$layout, "Name of file:",2,0,sticky="e")
  data_labels2 <- ttkentry(e$layout,
                           textvariable = e$filename, width=10)
  tkgrid(data_labels2, row = 2, column = 1, sticky="ew", padx = 2)
  tkfocus(data_labels2) 
  
  #Directory Radiobuttons
  put_label(e$layout, "Save to:",3,0,sticky="e")
  rb_frame <- ttkframe(e$layout)
  sapply(c("Working Directory","Choose other Directory"), function(i) {
    radio_button <- tk2radiobutton(rb_frame, variable = e$dir,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame, row = 3, column = 1, sticky = "w")
  
  #Data Type Radiobuttons
  put_label(e$layout, "Save as:",4,0,sticky="e")
  rb_frame2 <- ttkframe(e$layout)
  sapply(c("csv","txt","spss","stata"), function(i) {
    radio_button <- tk2radiobutton(rb_frame2, variable = e$format,text = i, value = i)
    tkpack(radio_button, side = "left")
  })
  tkgrid(rb_frame2, row = 4, column = 1, sticky = "w")
}

run.saveas<-function(e){
    if (tclvalue(e$dir)=="Working Directory"){
        e$dir<-e$dirval
    } else { e$dir<-tclVar(choose.dir(default = getwd()))}
    if (tclvalue(e$filename)=="Same as dataFrame Name"){
        e$filename<-e$dataName
    }
    e$savename<-paste(tclvalue(e$dir),"/",tclvalue(e$filename),".",sep="")
    switch(tclvalue(e$format),
           csv =  write.csv(x = e$dataFrame, file = paste(e$savename,"csv",sep="")),
           txt =  write.table(x = e$dataFrame, file = paste(e$savename,"txt",sep=""), sep="\t"),
           #xlsx = xlsx::write.xlsx(x = e$dataFrame, file = paste(e$savename,"xlsx",sep="")),
           spss = suppressWarnings(foreign::write.foreign(df = e$dataFrame, paste(e$savename,"sps",sep=""), paste(e$savename,"sps",sep=""), package="SPSS")) ,
           stata = suppressWarnings(foreign::write.dta(dataFrame = e$dataFrame, paste(e$savename,"dta",sep=""))) 
           )
    pos<-1
    envir <- as.environment(pos)
    assign(tclvalue(e$filename), e$dataFrame, envir = envir)
    tkdestroy(e$wnd)
}

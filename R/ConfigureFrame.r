smartListbox <- function(labelFrame, options) {
    listbox <- tk2listbox(labelFrame, height = 1, selectmode = "extended")
    tkbind(listbox, "<Key>", function(W, K, k) {
           if (k >=65 && k<= 90) {
               for (i in 1:length(options)) {
                   if (startsWith(options[[i]], K)) {
                       d <- (0 - length(options))
                       tkyview.scroll(W, d, "units")
                       d <- (i - 1)
                       tkyview.scroll(W, d, "units")
                       break
                   }
               }
           }
           tkfocus(W)
    })
    tkselection.set(listbox, 0)
    return(listbox)
}

put_label<-function(parent, text, row, column, sticky = "e") {
    label<-ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = sticky)
}

columnConfig<-function(labelFrame) {
    tkgrid.columnconfigure(labelFrame, 0, weight = 1)
    tkgrid.columnconfigure(labelFrame, 1, weight = 1)
    tkgrid.columnconfigure(labelFrame, 2, weight = 1)
    tkgrid.columnconfigure(labelFrame, 1, weight = 1)
}

updateDataFrame <- function(e, dataName, x = "dataFrame") {
    name <- tclvalue(dataName)
    if (name == "Load User File"){
        dataFrame <- getcsvfile()        
    } else if (dataName == "Load Shape File"){
        getshapefile <- function() {
            name <- tclvalue(tkgetOpenFile(filetypes = "{{ESRI Shapefiles} {.shp}}"))
            if (name == "") return(data.frame());
            data <- raster::shapefile(name)
            dataFrame<-data@coords
            return(dataFrame)
        }
        dataFrame <- getshapefile()
    } else {
        dataFrame <- get(name, .GlobalEnv)	
    }
    assign(x, dataFrame, envir = e)
}

resetValues <- function(e){
    updateDataFrame(e, e$dataName)
    
    e$inputs = sort(colnames(e$dataFrame))
    for (i in 1:length(e$varListbox)) {
        tkdelete(e$varListbox[[i]], 0, "end")
        for (input in e$inputs){
            tkinsert(e$varListbox[[i]], "end", input)
        }
        tkselection.set(e$varListbox[[i]], 0)
    }
}

getVarName <- function(e, id) {
    value <- e$inputs[as.numeric(tkcurselection(e$varListbox[[id]])) + 1]
    return(value)
}

comboxLayout <- function(e, comboNum, dir) {
    e$dataName <- tclVar("Choose one"); 
    e$dataFrame <- tclVar("NULL"); 

    columnConfig(e$layout)

    #Data Combobox
    e$dataLabel<-ttklabel(e$layout, text = "Data:")
    tkgrid(e$dataLabel, row = 0, column = 0, sticky = "e")
    dataCombo <- ttkcombobox(e$layout, state = "readonly", 
                               values = dfs.fun(), 
                               textvariable = e$dataName)
    tkgrid(dataCombo, row = 0, column = 1, sticky="ew", padx = 2)
    tkfocus(dataCombo)
    
    if (comboNum < 2) {
        tkbind(dataCombo, "<<ComboboxSelected>>", function() updateDataFrame(e, e$dataName))    
        return()
    }
    tkbind(dataCombo, "<<ComboboxSelected>>", function() resetValues(e))

    for (i in 1:(comboNum - 1)) {
        varLabel<-ttklabel(e$layout, text = "Variable:")

        varListbox <- smartListbox(e$layout, sort(colnames(e$dataFrame)))
        tkfocus(varListbox) 
        tkbind(varListbox, "<Return>", function(W) tcl(W, "invoke"))

        if (dir == "ver") {
            tkgrid(varLabel, row = i, column = 0, sticky = "e")
            tkgrid(varListbox, row = i, column = 1, sticky="ew", padx = 2, pady = 5)
        } else {
            col <- i

            if (dir == "hor2" && i == 3) {
                # special template for "Factorial ANOVA" and "Generalized Linear Model"
                e$interaction<-tclVar("NULL")
                put_label(e$layout, "Interaction:",row=1,column = col,sticky="n")
                interCombo <- ttkcombobox(e$layout, state = "readonly", 
                                          values = c("NULL"," +"," *"),
                                          textvariable = e$interaction, 
                                          width=5)
                tkgrid(interCombo, row = 2, column = col, sticky="ew", padx = 2)
                tkfocus(interCombo) 
                col <- (i + 1)
            }

            tkgrid(varLabel, row = 1, column = col)
            tkgrid(varListbox, row = 2, column = col, sticky="ew", padx = 2, pady = 5)
        }

        e$varLabel[i] <- list(varLabel)
        e$varListbox[i] <- list(varListbox)
    }
}

createConfigureFrame<- function(e, comboNum=1, dir) {
    e$okLabel <- "ok"
    e$cancelLabel <- "cancel"

    e$wnd <- tktoplevel()
    #tkwm.geometry(e$wnd, "300x300+300+300") 
    frame <- ttkframe(e$wnd, padding = c(3,3,12,12))
    tkpack(frame, expand = TRUE, fill = "both")
    e$layout <- ttklabelframe(frame, padding = 10)
    tkpack(e$layout, expand = TRUE, fill = "both", padx = 5, pady = 5)
    if (comboNum > 0) {
        comboxLayout(e, comboNum, dir)
    }

    layout(e);

    #Draw OK Cancel Button
    button_frame <- ttkframe(frame)
    cancel_button <- ttkbutton(button_frame, text = e$cancelLabel, command=function() tkdestroy(e$wnd))
    ok_button <- ttkbutton(button_frame, text = e$okLabel, command=function() run(e))
    tkpack(button_frame, fill = "x", padx = 5, pady = 5)
    tkpack(ttklabel(button_frame, text = " "), expand = TRUE, fill = "y", side = "left")              
    sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
    tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
    tkfocus(e$wnd)
}


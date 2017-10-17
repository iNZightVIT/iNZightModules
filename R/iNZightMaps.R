##' iNZight Mapping Module
##'
##' Opens a UI for visualising geographical data
##'
##' @title iNZight Maps Module
##'
##' @author Tom Elliott
##'
##' @export iNZightMapMod
##' @exportClass iNZightMapMod
iNZightMapMod <- setRefClass(

    # ===================================================================
    # REFERENCE DEFINITION
    #   + A character value used as a reference for the object
    #   + Best to keep it consistent with object name to avoid confusion
    # ===================================================================
    "iNZightMapMod",


    # =================================================================================
    # FIELD DEFINITION
    #   + Pre-defined fields:
    #     - GUI       : main GUI
    #     - mainGrp   : main container in which all buttons and sub-groups are defined
    #     - activeData: imported data retrieved from the main iNZight GUI
    #   + Can change names
    #   + Set as many fields as needed
    #   + Additional fields:
    #     - map.vars : extra variables stored about the map plot object
    #     - map.object : the plot itself (plus additional layers)
    #     - [R] map.type : what type of plot it is
    #     - grpTbl : sidebar gtable
    # =================================================================================
    fields = list(
        GUI         = "ANY",
        mainGrp     = "ANY",
        activeData  = "data.frame",
        map.vars    = "ANY",
        map.object  = "ANY",
        map.type    = "ANY",
        # extra.args  = "list",
        grpTbl      = "ANY"
    ),


    # ================================================================================
    # METHOD DEFINITION
    #   + Pre-defined method:
    #     - initialize(): all that relate to module window GUI should be defined here
    #   + Additonal Methods:
    #     - [R] setVars()   : set extra variables to control plot appearance
    #     - createMapObject(): Creates the map object based on details from 
    #                          initialisation dialog
    #     - numericVars() : vector of numeric variables in activeData 
    #     - characterVars() : vector of character variables in activeData
    #     - initiateModule() : initiate map module into GUI - set up sidebar, plot 
    #                          area, etc. 
    #     - [R] canIZoom() : 
    #     - createSlider() : create the facetting slider based on the variable chosen
    #                        in the dropdown box
    #     - deleteSlider() : delete a facetting slider
    #     - [R]changePlotSettings() : update plotting attributes with extra.args
    #     - updatePlot() : updates the plot with any new mappings or layers
    # ================================================================================
    methods = list(
        ## Function with all GUI specifications
        initialize = function(GUI) {
            ## GUI
            initFields(GUI = GUI)

            if (!requireNamespace("iNZightMaps2", quietly = TRUE)) {
                resp <- gconfirm("The Maps package isn't installed. Do you want to install it now?",
                                 title = "Install Maps package", icon = "question", parent = GUI$win)

                if (resp) {
                    utils::install.packages("iNZightMaps", repos = c("http://cran.stat.auckland.ac.nz",
                                                               "http://r.docker.stat.auckland.ac.nz/R"))
                    if (!requireNamespace("iNZightMaps", quietly = TRUE)) {
                        gmessage("Unable to install package. Please check the website.")
                        return(NULL)
                    }
                } else {
                    return(NULL)
                }
            }
            
            ## Configure the data / variables for mapping:
            ## activeData
            activeData <<- GUI$getActiveData()

            w <- gwindow("Define Geographical Variables", width = 400, height = 500, parent = GUI$win, visible = FALSE)
            gv <- gvbox(cont = w, expand = TRUE, fill = TRUE)
            gv$set_borderwidth(15)

            lbl <- glabel("Type of Map Data")
            font(lbl) <- list(weight = "bold", size = 12, family = "normal")
            mapType <- gradio(c("Coordinate (latitude, longitude)",
                                "Regions (country, state, county, etc.)"))
            add(gv, lbl)
            add(gv, mapType)

            coord.or.region <- any(grepl("(latitude|longitude)", colnames(activeData), TRUE))
            svalue(mapType, index = TRUE) <- if(coord.or.region) 1 else 2

            addSpace(gv, 10)
            
            #################################################

            # TODO: Fix problem of inputs moving slightly
            
            lbl <- glabel("Map Source")
            font(lbl) <- list(weight = "bold", size = 12, family = "normal")
            mapSource <- gradio(c("Use Inbuilt Map", "Import Shapefile"))
            add(gv, lbl)
            add(gv, mapSource)

            tblShapefile <- glayout()
            tblInbuiltfile <- glayout()
            
            mapSourceBrowse <- gfilebrowse(text = "Open Shapefile...", 
                                           type = "open",
                                           filter = list("All supported formats" = list(patterns = c("*.shp", "*.json", "*.geojson")),
                                                         "Shapefile" = list(patterns = c("*.shp")),
                                                         "GeoJSON" = list(patterns = c("*.json", "*.geojson")))
                                           )
            
            tblShapefile[1, 1, expand = TRUE] <- mapSourceBrowse
            
            stored.shapefiles <- list.files("~/iNZightVIT/shapefiles/", recursive = TRUE, pattern = ".shp$")
            
            offspring.files <- function(path, obj) {
                if(length(path) > 0) {
                    path.pattern <- paste0("^", paste(path, collapse = "/"), "/")
                } else {
                    path.pattern <- ""
                }
                
                files.list <- obj[grepl(path.pattern, obj)]
                
                files.list <- sub(path.pattern, "", files.list)

                slash.loc <- regexpr("/", files.list)
                has.children <- slash.loc != -1
                
                filenames <- files.list
                filenames[!has.children] <- files.list[!has.children]
                
                filenames[has.children] <- substring(files.list[has.children], 
                                                     first = 1, 
                                                     last = slash.loc[has.children] - 1)
                
                unique.ind <- !duplicated(filenames)
                
                data.frame(filename = filenames[unique.ind], has.children = has.children[unique.ind])
            }

            mapInbuiltBrowse <- gtree(offspring = offspring.files, 
                                      offspring.data = stored.shapefiles, 
                                      chosen.col = "filename",
                                      offspring.col = "has.children")

            tblInbuiltfile[1, 1, expand = TRUE, fill = "both"] <- mapInbuiltBrowse
            
            add(gv, tblShapefile, fill = "x")
            add(gv, tblInbuiltfile, expand = TRUE)
            
            visible(tblInbuiltfile) <- TRUE
            visible(tblShapefile) <- FALSE
            
            addHandlerChanged(mapSource, function(h, ...) {
              v <- svalue(mapSource, index = TRUE)
              visible(tblShapefile) <- v == 2
              visible(tblInbuiltfile) <- v == 1
            })
            
            addSpace(gv, 10)
            
            #################################################

            title <- glabel("Mapping Variables")
            font(title) <- list(weight = "bold", size = 12, family = "normal")
            add(gv, title)

            ## latitude and longitude
            tbl <- glayout(homogeneous = FALSE)
            ii <- 1

            lbl <- "Latitude :"
            latVar <- gcombobox(c("", numericVars()))
            tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 2:4, anchor = c(-1, 0), expand = TRUE] <- latVar
            ii <- ii + 1

            lbl <- "Longitude :"
            lonVar <- gcombobox(c("", numericVars()))
            tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 2:4, anchor = c(-1, 0), expand = TRUE] <- lonVar
            ii <- ii + 1

            ## try find lat/lon columns in data set:
            vars <- numericVars()
            lat.match <- grep("lat", tolower(vars))
            if (length(lat.match)) svalue(latVar, index = TRUE) <- lat.match[1] + 1
            lon.match <- grep("lon", tolower(vars))
            if (length(lon.match)) svalue(lonVar, index = TRUE) <- lon.match[1] + 1

            visible(tbl) <- coord.or.region
            visible(title) <- coord.or.region

            addSpace(gv, 10)
            add(gv, tbl, expand = TRUE, fill = TRUE)
            addSpring(gv)

            ## switch between them using radio buttons
            addHandlerChanged(mapType, function(h, ...) {
                                  v <- svalue(mapType, index = TRUE)
                                  visible(tbl) <- v == 1
                                  visible(title) <- v == 1
                              })

            ## OK Button
            btnGrp <- ggroup(cont = gv)

            addSpring(btnGrp)
            okbtn <- gbutton("OK", expand = TRUE,
                             cont = btnGrp,
                             handler = function(h, ...) {
                                 ## mapSource == 2 -> Shapefile
                                 ## mapSource == 1 -> Inbuilt
                                 ## Error checking
                                 if(svalue(mapSource, index = TRUE) == 2) {
                                     if(length(svalue(mapSourceBrowse)) == 0) {
                                         gmessage("Please select a shapefile", parent = w)
                                         return(NULL)
                                     } else {
                                         if(!file.exists(svalue(mapSourceBrowse))) {
                                             gmessage("Shapefile does not exist", parent = w)
                                             return(NULL)
                                         }
                                         
                                         shapefile <- svalue(mapSourceBrowse)
                                     }
                                 } else {
                                     # TODO: Replace with a less fixed path
                                     inbuilt.path <- paste(svalue(mapInbuiltBrowse), collapse = "/")
                                     if(grepl(".shp$", inbuilt.path)) {
                                         shapefile <- paste0("~/iNZightVIT/shapefiles/", inbuilt.path)
                                     } else {
                                         gmessage("Please choose a shapefile", parent = w)
                                         return(NULL)
                                     }
                                 }
                                 print(shapefile)
                                 
                                 if (svalue(mapType, index = TRUE) == 1) {
                                     if (svalue(latVar, TRUE) > 1 && svalue(lonVar, TRUE) > 1) {
                                         setVars(list(latitude = svalue(latVar),
                                                      longitude = svalue(lonVar),
                                                      shapefile = shapefile),
                                                 type = "points")
                                         initiateModule()
                                         dispose(w)
                                     } else {
                                         gmessage("Please select a variable for latitude and longitude", parent = w)
                                     }
                                 } else {
                                     matchingDialog(shapefile = shapefile, sender = w)
                                 }
                             })

            cnclBtn <- gbutton("Cancel", expand = TRUE, cont = btnGrp,
                               handler = function(h, ...) {
                                  dispose(w)
                               })

            visible(w) <- TRUE

        },

        ## Supplementary functions to be used in initialize()
        ##   - Can create as many as needed
        setVars = function(names, type) {
            map.vars <<- names
            map.type <<- ifelse(type == "shape", "shape", "roadmap")

            ## defaults:
            map.vars$alpha <<- 1
            map.vars$cex.pt <<- 1
            # extra.args <<- list()

            createMapObject()
        },
        createMapObject = function() {
            simplify.level <- 0.01
            map.object <<-
                if (map.type == "shape") {
                  map.obj <- sf::st_read(map.vars$shapefile)
                  map.obj <- sf::st_simplify(map.obj, dTolerance = simplify.level)
                  iNZightMaps2::iNZightMapPlot(data = activeData,
                                               map = map.obj, 
                                               type = "region",
                                               by.data = map.vars$location.var,
                                               by.map = map.vars$map.var)
                  
                } else {
                  map.obj <- sf::st_read(map.vars$shapefile)
                  map.obj <- sf::st_simplify(map.obj, dTolerance = simplify.level)
                    iNZightMaps2::iNZightMapPlot(data = activeData,
                                                 map = map.obj,
                                                 type = "point",
                                                 coord = c(map.vars$longitude, map.vars$latitude),
                                                 crs = 4326)
                }
        },
        ## get only numeric type variables
        numericVars = function() {
            colnames(activeData)[sapply(activeData, is.numeric)]
        },
        characterVars = function() {
            colnames(activeData)[!sapply(activeData, is.numeric)]
        },
        ## initiate the module only when the data has been set
        initiateModule = function(shape = FALSE) {
            GUI$initializeModuleWindow(.self)

            ## Reconfigure the Plot Toolbar:
            aboutBtn <- gimage(stock.id = "about", size = "button")
            addHandlerClicked(aboutBtn, function(h, ...) {

                                  wAb <- gwindow(parent = GUI$win, width = 400, height = 370,
                                                 title = "iNZight Maps Module")

                                  gAb <- gvbox(container = wAb, spacing = 10)
                                  addSpace(gAb, 10)
                                  labAb <- glabel("About the iNZight Maps Module")
                                  font(labAb) <- list(weight = "bold", size = 12)
                                  add(gAb, labAb, anchor = c(0, 0))

                                  aboutText <-
                                      paste("\n\nThe iNZight Mapping Module allows you to visually explore geographical",
                                            "data. When you load the module, you are presented with a window to select",
                                            "the Latitude and Longitude variables from the data set.",
                                            "\n\nOnce you've specified the locations of points, they are displayed on a map",
                                            "automatically. Using the drop downs, you can code variables and investigate",
                                            "any geographical patterns.",
                                            "\n\nYou can also plot regional data (currently restricted to countries)",
                                            "and shade countries by their value of a specified variable.",
                                            "Also feel free to send us any suggestions or problems",
                                            "you come across:\n")
                                  txtAb <- gtext(text = aboutText, width = 380, height = NULL)
                                  add(gAb, txtAb, expand = TRUE)

                                  lab <- gbutton("Contact iNZight Support")
                                  font(lab) <- list(color = "navy", weight = "bold")
                                  addHandlerClicked(lab, function(h, ...)
                                      browseURL("https://stat.auckland.ac.nz/~wild/iNZight/support/contact"))
                                  add(gAb, lab, expand = FALSE, fill = FALSE, anchor = c(0, 0))

                                  cls <- gbutton("Close", handler = function(h, ...) dispose(wAb))
                                  add(gAb, cls, anchor = c(0, 1))
                              })

            zoomBtn <- gimage(stock.id = "zoom-in", size = "button")
            zoomOutBtn <- gimage(stock.id = "zoom-out", size = "button")
            zoomFitBtn <- gimage(stock.id = "zoom-fit", size = "button")
            
            if (map.type == "shape") {                
                addHandlerClicked(zoomBtn, function(h, ...) {
                    test.pt <- grid::grid.locator("npc")
                    
                    print(test.pt)
                    
                    mp.build <- ggplot2::ggplot_build(plot(map.object))
                    
                    test.xlim <- mp.build$layout$panel_params[[1]]$x_range
                    test.ylim <- mp.build$layout$panel_params[[1]]$y_range
                    
                    test.xrange <- diff(test.xlim)
                    test.yrange <- diff(test.ylim)
                    
                    x.centre <- test.xlim[1] + as.numeric(test.pt$x) * test.xrange
                    y.centre <- test.ylim[1] + as.numeric(test.pt$y) * test.yrange
                    
                    zoom.xlim <- x.centre + c(-1, 1) * (1/2) * (1/2) * test.xrange
                    zoom.ylim <- y.centre + c(-1, 1) * (1/2) * (1/2) * test.yrange
                    
                    map.object <<- addLayer.iNZightMapPlot(map.object, 
                                                           "map", 
                                                           "coordlims",
                                                           ggplot2::coord_sf(xlim = zoom.xlim,
                                                                             ylim = zoom.ylim,
                                                                             datum = map.object$crs))
                    updateEverything()
                })
                addHandlerClicked(zoomOutBtn, function(h, ...) {
                    test.pt <- grid::grid.locator("npc")
                    
                    print(test.pt)
                    
                    mp.build <- ggplot2::ggplot_build(plot(map.object))
                    
                    test.xlim <- mp.build$layout$panel_params[[1]]$x_range
                    test.ylim <- mp.build$layout$panel_params[[1]]$y_range
                    
                    test.xrange <- diff(test.xlim)
                    test.yrange <- diff(test.ylim)
                    
                    x.centre <- test.xlim[1] + as.numeric(test.pt$x) * test.xrange
                    y.centre <- test.ylim[1] + as.numeric(test.pt$y) * test.yrange
                    
                    zoom.xlim <- x.centre + c(-1, 1) * (1/2) * (2/1) * test.xrange
                    zoom.ylim <- y.centre + c(-1, 1) * (1/2) * (2/1) * test.yrange

                    map.object <<- addLayer.iNZightMapPlot(map.object, 
                                                        "map", 
                                                        "coordlims",
                                                        ggplot2::coord_sf(xlim = zoom.xlim,
                                                                          ylim = zoom.ylim,
                                                                          datum = map.object$crs))
                    updateEverything()
                })
                
                addHandlerClicked(zoomFitBtn, function(h, ...) {
                    map.object <<- removeLayer.iNZightMapPlot(map.object, 
                                                           "map", 
                                                           "coordlims")
                    updateEverything()
                })
            } else {
                addHandlerClicked(zoomBtn, function(h, ...) {
                    zoom.point <- grid.locator()
                    print(zoom.point)
                })
                addHandlerClicked(zoomOutBtn, function(h, ...) {
                    zoom.point <- grid.locator()
                    print(zoom.point)
                })
            }

            addtoplotBtn <- gimage(stock.id="edit", size = "button",
                                         tooltip = "Add to Plot")
            addHandlerClicked(addtoplotBtn, function(h, ...) {
              
            })

            GUI$plotToolbar$update(NULL, refresh = "updatePlot", extra = list(addtoplotBtn, zoomFitBtn, zoomBtn, zoomOutBtn, aboutBtn))

            ## mainGrp
            mainGrp <<- gvbox(spacing = 5, container = GUI$moduleWindow, expand = TRUE)
            mainGrp$set_borderwidth(5)

            addSpace(mainGrp, 10)

            lbl1 <- glabel("iNZight Maps")
            font(lbl1) <- list(weight = "bold",
                               family = "normal",
                               size   = 12)
            add(mainGrp, lbl1, anchor = c(0, 0))
            addSpace(mainGrp, 10)


            tbl <- glayout(homogeneous = FALSE)
            ii <- 1
            
            ## Options common to both regions and points
            
            lbl <- glabel("Code Variables")
            font(lbl) <- list(weight = "bold", size = 11)
            tbl[ii, 1:6, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1
            
            lbl <- glabel("Colour by :")
            colVarList <- gcombobox(c("", names(GUI$getActiveData())),
                                    selected = ifelse(
                                      is.null(map.vars$colby),
                                      1, which(names(GUI$getActiveData()) ==
                                                 map.vars$colby)[1] + 1
                                    )
            )
            tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 3:6, expand = TRUE] <- colVarList
            ii <- ii + 1 

            if (map.type == "shape") {
              lbl <- glabel("Opacify by :")
              opctyVarList <- gcombobox(
                c("", numNames <- names(activeData)[sapply(activeData, is.numeric)]),
                selected = ifelse(
                  is.null(map.vars$opacity),
                  1, which(numNames == map.vars$opacity)[1] + 1
                )
              )
              tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
              tbl[ii, 3:6, expand = TRUE] <- opctyVarList
              ii <- ii + 1
            } else {
                lbl <- glabel("Size by :")
                rszVarList <- gcombobox(
                    c("", rszNames <- names(activeData)[sapply(activeData, is.numeric)]),
                    selected = ifelse(
                        is.null(map.vars$sizeby),
                        1, which(rszNames == map.vars$sizeby)[1] + 1
                        )
                    )
                tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
                tbl[ii, 3:6, expand = TRUE] <- rszVarList
                ii <- ii + 1
            }
            
            ii <- ii + 1
            ii <- ii + 1
            lbl <- glabel("Plot Options")
            font(lbl) <- list(weight = "bold", size = 11)
            tbl[ii, 1:6, anchor = c(-1, -1), expand = TRUE] <- lbl
            ii <- ii + 1

            
            if (map.type != "shape") {
                lbl <- glabel("Map type :")
                typeOpts <- c("roadmap", "satellite", "terrain", "hybrid")
                typeList <- gcombobox(typeOpts)
                tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
                tbl[ii, 3:6, expand = TRUE] <- typeList
                ii <- ii + 1
            }


            ## COLOUR
            lbl <- glabel("Colour :")
            if (map.type == "shape") {
                pointCols <- c("red", "darkblue", "darkgreen", "darkmagenta",
                               "darkslateblue", "hotpink4", "lightsalmon2",
                               "palegreen3", "steelblue3",
                               "heat", "terrain")
                symbolColList <- gcombobox(
                    pointCols,
                    selected = ifelse(
                        is.na(which(pointCols == map.vars$col.pt)[1]),
                        1,
                        which(pointCols == map.vars$col.pt)[1]),
                    editable = FALSE)

                naFillCol <- gcombobox(
                    c("grey50", "lightslategrey", "white", "black", "red"),
                    selected = ifelse(
                        is.na(which(pointCols == map.vars$col.pt)[1]),
                        1,
                        which(pointCols == map.vars$col.pt)[1]),
                    editable = FALSE)
            } else {
                pointCols <- c("", colours())
                symbolColList <- gcombobox(pointCols, editable = TRUE)
            }

            tbl[ii,  1:2, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii,  3:6, expand = TRUE] <- symbolColList
            ii <- ii + 1

            if (map.type == "shape") {
                tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("Missing value colour :")
                tbl[ii, 3:6, expand = TRUE] <- naFillCol
            }


            if (map.type != "shape") {
                ## Point sizes
                lbl <- glabel("Point size :")
                cexSlider <- gslider(from = 0.05, to = 3.5,
                                     by = 0.05, value = map.vars$cex.pt)
                tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
                tbl[ii, 3:6, expand = TRUE] <- cexSlider
                ii <- ii + 1
                
                ## Transparency
                lbl <- glabel("Transparency :")
                transpSlider <- gslider(from = 0, to = 100,
                                        by = 1, value = 100 * (1 - map.vars$alpha))
                tbl[ii, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
                tbl[ii, 3:6, expand = TRUE] <- transpSlider
                ii <- ii + 1

                ## Connect by lines
                joinPts <- gcheckbox("Connect points by lines", checked = FALSE)
                if (!is.null(map.vars$join)) svalue(joinPts) <- map.vars$join
                
                joinCols <- c("red", "black", "blue", "green4",
                              "yellow", "pink", "grey", "orange")
                joinCol <- gcombobox(joinCols)
                if (!is.null(map.vars$col.line))
                    if (map.vars$col.line %in% joinCols)
                        svalue(joinCol) <- which(joinCols == map.vars$col.line)
                enabled(joinCol) <- svalue(colVarList, TRUE) == 1
                tbl[ii, 1:3, expand = TRUE, anchor = c(-1, 0)] <- joinPts
                tbl[ii, 4:6, expand = TRUE] <- joinCol
                addHandlerChanged(joinPts, function(h, ...) updateEverything())
                addHandlerChanged(joinCol, function(h, ...) updateEverything())
            }


            ## Labels
            if (map.type == "shape") {
                ii <- ii + 1
                
                lbl <- glabel("Plot labels :")
                mapLbls <- gcombobox(c("None", paste(map.vars$location.var, "name"), "Value", "Both"))
                tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
                tbl[ii, 2, expand = TRUE] <- mapLbls
                ii <- ii + 1
                
                addHandlerChanged(mapLbls, function(h, ...) updateEverything())                                  
            }




            ## Maintain a single function that is called whenever anything is updated:
            updateEverything <- function() {
                if (map.type == "shape") {
                    map.vars$y <<- svalue(colVarList)
                    map.vars$opacity <<- svalue(opctyVarList)
                    map.vars$col <<- svalue(symbolColList)
                    map.vars$na.fill <<- svalue(naFillCol)
                    map.vars$map.labels <<- svalue(mapLbls, index = TRUE)
                } else {
                    if (svalue(colVarList, TRUE) > 1) map.vars$colby <<- svalue(colVarList) else map.vars$colby <<- NULL
                    if (svalue(rszVarList, TRUE) > 1) map.vars$sizeby <<- svalue(rszVarList) else map.vars$sizeby <<- NULL
                    # if (svalue(opctyVarList, TRUE) > 1) map.vars$opacity <<- svalue(opctyVarList) else map.vars$opacity <<- NULL
                    if (svalue(symbolColList, TRUE) > 1) map.vars$colconst <<- svalue(symbolColList) else map.vars$colconst <<- NULL
                    map.vars$col.pt <<- svalue(symbolColList)
                    map.vars$cex.pt <<- svalue(cexSlider)
                    map.vars$alpha <<- 1 - svalue(transpSlider) / 100
                    map.vars$join <<- svalue(joinPts)
                    map.vars$col.line <<- svalue(joinCol)
                    
                    map.type <<- svalue(typeList)
                }
                
                map.vars$plot.title <<- svalue(addPlotTitle)
                
                updatePlot()
            }

            ## in this case, no point in having a separate "show" button
            if (map.type == "shape") {
                addHandlerChanged(colVarList, handler = function(h, ...)  {
                    if(!is.null(map.vars$colconst)) {
                        svalue(symbolColList, index = TRUE) <- 1
                    }
                    updateEverything()
                    })
                addHandlerChanged(naFillCol, handler = function(h, ...) updateEverything())
                addHandlerChanged(opctyVarList, handler = function(h, ...) updateEverything())
            } else {
                addHandlerChanged(colVarList, handler = function(h, ...)  {
                    if(!is.null(map.vars$colconst)) {
                        svalue(symbolColList, index = TRUE) <- 1
                    }
                    updateEverything()
                })
                addHandlerChanged(rszVarList, handler = function(h, ...) updateEverything())
                addHandlerChanged(typeList, handler = function(h, ...) updateEverything())
            }

            pcoltimer <- NULL
            addHandlerChanged(symbolColList,
                              handler = function(h, ...) {
                                  if(!is.null(map.vars$colby) && svalue(symbolColList, index = TRUE) > 1) {
                                      gmessage("Colours already specified by variable")
                                      svalue(symbolColList, index = TRUE) <- 1
                                  } else {
                                      updateEverything()
                                  }
                              })
            
            if (map.type != "shape") {
                cextimer <- NULL
                addHandlerChanged(cexSlider,
                                  handler = function(h, ...) {
                                      if (!is.null(cextimer))
                                          cextimer$stop_timer()
                                      cextimer <- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
                                  })
                
                transptimer <- NULL
                addHandlerChanged(transpSlider,
                                  handler = function(h, ...) {
                                      if (!is.null(transptimer))
                                          transptimer$stop_timer()
                                      transptimer <- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
                                  })
            }


            add(mainGrp, tbl)
            
            title.tbl <- glayout()
            
            addPlotLabel <- glabel("Plot Title")
            title.tbl[1, 1:2, anchor = c(1, 0), expand = TRUE] <- addPlotLabel
            addPlotTitle <- gedit("")
            title.tbl[1, 4:5, expand = TRUE] <- addPlotTitle
            addPlotTitleBtn <- gbutton("Add Title")
            title.tbl[1, 6] <- addPlotTitleBtn
            
            addHandlerClicked(addPlotTitleBtn, handler = function(h, ...) {
                updateEverything()
            })
            
            add(mainGrp, title.tbl)
            
            addCentresBtn <- gbutton("Region Centres")
            addHandlerClicked(addCentresBtn, handler = function(h, ...) {
                # Find centres
                map.object <<- regionPoints.iNZightMapPlot(map.object)
                updateEverything()
            })
            add(mainGrp, addCentresBtn)
            
            addSpring(mainGrp)
            ## --------------------------------------------------  SLIDERS
            grpTbl <<- glayout(expand = FALSE, cont = mainGrp)
            G1box <- gcombobox(c("Select Subset Variable 1", colnames(activeData)))
            G2box <- gcombobox(c("Select Subset Variable 2", colnames(activeData)))

            grpTbl[1, 1:5, anchor = c(0, 0), expand = TRUE] <<- G1box
            grpTbl[3, 1:5, anchor = c(0, 0), expand = TRUE] <<- G2box

            ## -- Grouping Variable 1
            G1clearbtn <- gbutton("",
                                  handler = function(h,...) {
                                      svalue(G1box, index = TRUE) <- 1
                                      ## change handler will handle the rest
                                  })
            G1clearbtn$set_icon("Cancel")
            grpTbl[1, 7, anchor = c(0, 0)] <<- G1clearbtn

            ## -- Grouping Variable 2
            G2clearbtn <- gbutton("",
                                  handler = function(h,...) {
                                      svalue(G2box, index = TRUE) <- 1
                                  })
            G2clearbtn$set_icon("Cancel")
            grpTbl[3, 7, anchor = c(0, 0)] <<- G2clearbtn

            ## slider 1
            addHandlerChanged(
                G1box,
                handler = function(h, ...) {
                    if (svalue(G1box) == svalue(G2box)) {
                        svalue(G1box, index = TRUE) <- 1
                        gmessage("You cannot use the same variable in both subsetting slots.",
                                 parent = GUI$win)
                    } else {
                        deleteSlider(pos = 2)
                        if (svalue(G1box, index = TRUE) > 1) {
                            val <- svalue(G1box)
                            ds <- if (map.type == "shape") map.object$data else activeData
                            map.object <<- addFacet(map.object, val)
                            updatePlot() ############### CHECK FUNCTION
                            
                            createSlider(pos = 2, val)
                            
                        } else {
                            map.type.temp <- if(map.type == "shape") "map" else "point"
                            map.object <<- removeLayer.iNZightMapPlot(map.object, map.type.temp, "facet")
                            updatePlot()
                        }
                    }
                })

            ## slider 2
            addHandlerChanged(
                G2box,
                handler = function(h, ...) {
                    if (svalue(G2box) == svalue(G1box)) {
                        svalue(G2box, index = TRUE) <- 1
                        gmessage("You cannot use the same variable in both subsetting slots.",
                                 parent = GUI$win)
                    } else {
                        deleteSlider(pos = 4)
                        if (svalue(G2box, index = TRUE) > 1) {
                            val <- svalue(G2box)
                            ds <- if (map.type == "shape") map.object$data else activeData
                            createSlider(pos = 4, val)
                            changePlotSettings(list(
                                g2 = iNZightPlots:::convert.to.factor(
                                    ds[val][[1]]
                                    ),
                                g2.level = "_ALL",
                                varnames = list(
                                    g2 = val)
                                ))
                        } else {
                            changePlotSettings(list(g2 = NULL,
                                                    g2.level = NULL,
                                                    varnames = list(
                                                        g2 = NULL)
                                                    ), reset = TRUE)
                        }
                    }
                })


            ## close buton


            btmGrp <- ggroup(container = mainGrp)

            helpButton <- gbutton("Help", expand = TRUE, fill = TRUE,
                                  cont = btmGrp,
                                  handler = function(h, ...) {
                                      browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/add_ons/?topic=maps")
                                  })
            homeButton <- gbutton("Home", expand = TRUE, fill = TRUE,
                                cont = btmGrp,
                                handler = function(h, ...) {
                                    ## delete the module window
                                    delete(GUI$leftMain, GUI$leftMain$children[[2]])
                                    ## display the default view (data, variable, etc.)
                                    GUI$plotToolbar$restore()
                                    visible(GUI$gp1) <<- TRUE
                                })

            visible(GUI$moduleWindow) <<- TRUE

            updatePlot()
        },
        canIZoom = function() {
            err <- FALSE
            curVars <- GUI$activeModule$map.vars
            if (!is.null(curVars$g1)) {
                if (is.null(curVars$g1.level)) {
                    err <- TRUE
                } else if (curVars$g1.level == "_MULTI") {
                    err <- TRUE
                }
            }
            
            if (!is.null(curVars$g2)) {
                if (!is.null(curVars$g2.level)) {
                    if (curVars$g2.level == "_MULTI") err <- TRUE
                }
            }

            !err
        },
        createSlider = function(pos, dropdata) {
            ## make sure there is no slider at the pos
            deleteSlider(pos)

            ## create a ggroup for the slider at the specified
            ## pos in the glayout
            tbl <- grpTbl
            tbl[pos, 1:5, expand = TRUE] <- (hzGrp <- ggroup(fill = "x"))

            sliderGrp <- ggroup(horizontal = FALSE)

            ## build the level names that are used for the slider
            ds <- if (map.type == "shape") map.object$data else activeData
            grpData <- ds[dropdata][[1]]
            # grpData <- iNZightPlots:::convert.to.factor(grpData)
            if (pos == 2)
                lev <- c("_MULTI", levels(grpData))
            else
                lev <- c("_ALL", levels(grpData), "_MULTI")
            lev <- factor(lev, levels = lev)
            slider <- gslider(from = lev,
                              value = 1)
            add(sliderGrp, slider, expand = FALSE)
            if (pos == 2)
                grp = "g1"
            else
                grp = "g2"
            ## update the plot settings whenever the slider changes
            addHandlerChanged(slider, handler = function(h, ...) {
                map.vars$facet.var <<- as.character(svalue(h$obj))
                print(map.vars$facet.var)
                updatePlot()
                          })
            lbl <- levels(grpData)
            ## if the level names are too long, replace them with nr
            if (sum(nchar(lbl)) > 42)
                lbl <- 1:length(lbl)
            ## add * or _ to beginning of labels
            if (pos == 2)
                lbl <- c("_MULTI", lbl)
            else
                lbl <- c("_ALL", lbl, "_MULTI")
            ## only add label if it is short enough
            if (sum(nchar(lbl)) + 3 * length(lbl) < 50)
                add(sliderGrp, glabel(paste(lbl, collapse = "   ")))

            add(hzGrp, sliderGrp, expand = TRUE)

        },
        deleteSlider = function(pos) {
            ## get the child that is at the specified positions
            childPos <- which(sapply(grpTbl$child_positions,
                                     function(x) x$x == pos))
            while(length(childPos) > 0) {
                ##childPos <- names(ctrlGp$children[[1]]$child_positions)[[childPos]]
                ## delete all the current children of sliderGrp
                try({
                    grpTbl$remove_child(
                        grpTbl$child_positions[[childPos[1]]]$child)
                    childPos <- which(sapply(grpTbl$child_positions,
                                             function(x) x$x == pos))
                }, silent = TRUE)
            }
        },
        changePlotSettings = function(set, reset = FALSE) {
            map.vars <<- modifyList(map.vars, set$varnames)

            set$varnames <- NULL
            # if (reset)
            #     extra.args <<- set
            # else
            #     extra.args <<- iNZight:::modifyList(extra.args, set, keep.null = TRUE)

            updatePlot()
        },
        ## update plot function
        updatePlot = function() {
          args <- list(x = map.object, varnames = list())
          
          if (map.type == "shape") {
            map.object <<- iNZightMaps2::setMapping.iNZightMapPlot(map.object, 
                                                                   layer.set = "map", 
                                                                   layer.name = "baselayer", 
                                                                   aes.name = "fill", 
                                                                   aes.var = map.vars$y)
            map.object <<- iNZightMaps2::setMapping.iNZightMapPlot(map.object,
                                                                   layer.set = "map",
                                                                   layer.name = "baselayer",
                                                                   aes.name = "alpha",
                                                                   aes.var = map.vars$opacity)
          } else {
           # Point plotting updates 
            map.object <<- iNZightMaps2::setMapping.iNZightMapPlot(map.object,
                                                                   layer.set = "point",
                                                                   layer.name = "baselayer",
                                                                   aes.name = "colour",
                                                                   aes.var = map.vars$colby)
            
            # Add a fill aesthetic too - this makes the legend more informative
            map.object <<- iNZightMaps2::setMapping.iNZightMapPlot(map.object,
                                                                   layer.set = "point",
                                                                   layer.name = "baselayer",
                                                                   aes.name = "fill",
                                                                   aes.var = map.vars$colby)
            
            map.object <<- iNZightMaps2::setMapping.iNZightMapPlot(map.object,
                                                                   layer.set = "point",
                                                                   layer.name = "baselayer",
                                                                   aes.name = "size",
                                                                   aes.var = map.vars$sizeby)
            
            map.object <<- iNZightMaps2::setConstant.iNZightMapPlot(map.object,
                                                                    layer.set = "point",
                                                                    layer.name = "baselayer",
                                                                    aes.name = "colour",
                                                                    aes.val = map.vars$colconst)
            
          }
          
          if(!is.null(map.vars$plot.title)) {
              map.object <<- iNZightMaps2::addLayer.iNZightMapPlot(map.object,
                                                                   layer.set = "map",
                                                                   layer.name = "title",
                                                                   ggplot2::ggtitle(map.vars$plot.title)
              )
          }
          
          dev.hold()
          if(is.null(map.vars$facet.var)) {
              pl <- plot(map.object)
          } else {
              pl <- plot(map.object, facet = map.vars$facet.var)
          }
          dev.flush()
          
          return(invisible(pl))
        },
        matchingDialog = function(shapefile, sender) {
          map.obj <- sf::st_read(shapefile)
          
          w.match <- gwindow("Match Variables", width = 700, height = 500, visible = FALSE, parent = sender)
          gv.match <- gvbox(cont = w.match, expand = TRUE, fill = TRUE)
          gv.match$set_borderwidth(15)
          
          map.obj.vars <- as.data.frame(map.obj)[, !(colnames(map.obj) %in% "geometry")]
          
          mapvarBox <- gcombobox(items = colnames(map.obj.vars))
          datavarBox <- gcombobox(items = colnames(activeData))
          
          tblmap <- glayout()
          tblmap[1, 1] <- glabel("Map Variable: ")
          tblmap[1, 2, expand = TRUE] <- mapvarBox
          
          tbldata <- glayout()
          tbldata[1, 1] <- glabel("Data Variable: ")
          tbldata[1, 2, expand = TRUE] <- datavarBox
          
          add(gv.match, tbldata)
          add(gv.match, tblmap)
          
          #############################
          
          mapvar <- svalue(mapvarBox)
          datavar <- svalue(datavarBox)
          
          datavar.unq <- unique(as.character(unlist(activeData[, datavar])))
          mapvar.unq <-  unique(as.character(unlist(map.obj.vars[, mapvar])))
          
          mapvar.unq.tbl <- data.frame(mapname = mapvar.unq,
                                       matchvar = mapvar.unq,
                                       stringsAsFactors = FALSE)
          
          datavar.unq.tbl <- data.frame(dataname = datavar.unq, 
                                        matchvar = datavar.unq, 
                                        stringsAsFactors = FALSE)
          
          match.df <- dplyr::left_join(datavar.unq.tbl, mapvar.unq.tbl, by = "matchvar")
          
          #############################

          generateMatchDf <- function(mapvar, datavar) {
            mapvar.unq <- unique(as.character(unlist(map.obj.vars[, mapvar])))
            datavar.unq <- unique(as.character(unlist(activeData[, datavar])))

            if(any(grepl("country", c(datavar, mapvar)))) {
              matchvar <- mapvar.unq
            } else {
              matchvar <- mapvar.unq
            }

            mapvar.unq.tbl <- data.frame(mapname = mapvar.unq,
                                         matchvar = mapvar.unq,
                                         stringsAsFactors = FALSE)

            datavar.unq.tbl <- data.frame(dataname = datavar.unq,
                                          matchvar = datavar.unq,
                                          stringsAsFactors = FALSE)

            match.df <- dplyr::left_join(datavar.unq.tbl, mapvar.unq.tbl, by = "matchvar")
            match.df
          }
          
          addHandlerChanged(mapvarBox, handler = function(h, ...) {
            mapvar <- svalue(mapvarBox)
            datavar <- svalue(datavarBox)
            match.tbl[] <- generateMatchDf(mapvar, datavar)
          })
          addHandlerChanged(datavarBox, handler = function(h, ...) {
            mapvar <- svalue(mapvarBox)
            datavar <- svalue(datavarBox)
            match.tbl[] <- generateMatchDf(mapvar, datavar)
          })

          
          
          match.tbl <- gdf(match.df)
          add(gv.match, match.tbl, expand = TRUE, fill = TRUE)
          
          match.btnGrp <- ggroup(cont = gv.match)
          
          addSpring(match.btnGrp)
          
          match.confirm.btn <- gbutton("OK", handler = function(h, ...) {
            setVars(list(location.var = svalue(datavarBox),
                         map.var = svalue(mapvarBox),
                         shapefile = shapefile),
                    type = "shape")
            
            initiateModule(shape = TRUE)
            dispose(w.match)
            dispose(sender)
          })
          
          match.cancel.btn <- gbutton("Back", handler = function(h, ...) {
              dispose(w.match)
          })
          
          add(match.btnGrp, match.confirm.btn)
          add(match.btnGrp, match.cancel.btn)
          
          visible(w.match) <- TRUE
        }
  
      )


)

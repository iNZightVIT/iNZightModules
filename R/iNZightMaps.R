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
    # =================================================================================
    fields = list(
        GUI         = "ANY",
        mainGrp     = "ANY",
        activeData  = "data.frame",
        map.vars    = "ANY",
        map.object  = "ANY",
        map.type    = "ANY",
        extra.args  = "list",
        grpTbl      = "ANY"
    ),


    # ================================================================================
    # METHOD DEFINITION
    #   + Pre-defined method:
    #     - initialize(): all that relate to module window GUI should be defined here
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

            w <- gwindow("Define Geographical Variables", width = 400, height = 300, parent = GUI$win, visible = FALSE)
            gv <- gvbox(cont = w, expand = TRUE, fill = TRUE)
            gv$set_borderwidth(15)


            lbl <- glabel("Type of Map Data")
            font(lbl) <- list(weight = "bold", size = 12, family = "normal")
            mapType <- gradio(c("Coordinate (latitude, longitude)",
                                "Regions (country, state, county, etc.)"))
            add(gv, lbl)
            add(gv, mapType)

            addSpace(gv, 10)
            
            #################################################
            
            lbl <- glabel("Map Source")
            font(lbl) <- list(weight = "bold", size = 12, family = "normal")
            mapSource <- gradio(c("Shapefile", "Inbuilt Maps"))
            add(gv, lbl)
            add(gv, mapSource)
            
            mapSourceBrowse <- gfilebrowse(text = "Open Shapefile...", 
                                           type = "open",
                                           filter = list("All supported formats" = list(patterns = c("*.shp", "*.json", "*.geojson")),
                                                         "Shapefile" = list(patterns = c("*.shp")),
                                                         "GeoJSON" = list(patterns = c("*.json", "*.geojson")))
            )
            
            # offspring <- function(path=character(0), lst, ...) {
            #   if(length(path))
            #     obj <- lst[[path]]
            #   else
            #     obj <- lst
            #   nms <- names(obj)
            #   hasOffspring <- sapply(nms, function(i) {
            #     newobj <- obj[[i]]
            #     is.recursive(newobj) && !is.null(names(newobj))
            #   })
            #   data.frame(Maps=nms, hasOffspring = hasOffspring, stringsAsFactors=FALSE)
            # }
            # 
            # l <- list(world = "1", 
            #           country = list("Soviet Union" = "21", 
            #                          "United States" = "22", 
            #                          "New Zealand" = list("Regions" = "231")
            #                          )
            #           )
            # 
            # mapTreeBrowse <- gtree(offspring = offspring, offspring.data = l)
            
            add(gv, mapSourceBrowse, expand = TRUE)
            
#             addHandlerChanged(mapSource, function(h, ...) {
#               v <- svalue(mapSource, index = TRUE)
#               visible(mapTreeBrowse) <- v == 1
#             })
            
            addSpace(gv, 10)
            
            #################################################

            title <- glabel("Mapping Variables")
            font(title) <- list(weight = "bold", size = 12, family = "normal")
            # add(gv, title, anchor = c(-1, 0))
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


            ## shape file and region variable
            # tbl2 <- glayout(homogeneous = FALSE)
            # ii <- 1
# 
            # lbl <- "Map Location :"
            # mapLoc <- gcombobox(c("", "world"))
            # tbl2[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            # tbl2[ii, 2:4, anchor = c(-1, 0), expand = TRUE] <- mapLoc
            # ii <- ii + 1
# 
            # lbl <- "Location Variable :"
            # locVar <- gcombobox(c("", characterVars()))
            # tbl2[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            # tbl2[ii, 2:4, anchor = c(-1, 0), expand = TRUE] <- locVar
            # ii <- ii + 1
            # 
            # visible(tbl2) <- FALSE

            addSpace(gv, 10)
            add(gv, tbl, expand = TRUE, fill = TRUE)
            # add(gv, tbl2, expand = TRUE, fill = TRUE)
            addSpring(gv)

            ## switch between them using radio buttons
            addHandlerChanged(mapType, function(h, ...) {
                                  v <- svalue(mapType, index = TRUE)
                                  visible(tbl) <- v == 1
                                  # visible(tbl2) <- v == 2
                              })

            ## OK Button
            btnGrp <- ggroup(cont = gv)

            addSpring(btnGrp)
            okbtn <- gbutton("OK", expand = TRUE,
                             cont = btnGrp,
                             handler = function(h, ...) {
                                 ## Shapefile error checking
                                 if(length(svalue(mapSourceBrowse)) == 0) {
                                     gmessage("Please select a shapefile")
                                     return()
                                 } else {
                                     if(!file.exists(svalue(mapSourceBrowse))) {
                                         gmessage("Shapefile does not exist")
                                         return()
                                     }
                                 }

                                 if (svalue(mapType, index = TRUE) == 1) {
                                     if (svalue(latVar, TRUE) > 1 && svalue(lonVar, TRUE) > 1) {
                                         setVars(list(latitude = svalue(latVar),
                                                      longitude = svalue(lonVar),
                                                      shapefile = svalue(mapSourceBrowse)),
                                                 type = "points")
                                         initiateModule()
                                         dispose(w)
                                     } else {
                                         gmessage("Please select a variable for latitude and longitude")
                                     }
                                 } else {
                                     matchingDialog(shapefile = svalue(mapSourceBrowse))
                                     # setVars(list(location = svalue(mapLoc),
                                     #              # location.var = svalue(locVar)),
                                     #              location.var = svalue(locVar),
                                     #              shapefile = svalue(mapSourceBrowse)),
                                     #         type = "shape")
                                     # initiateModule(shape = TRUE)
                                     dispose(w)
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
            extra.args <<- list()

            createMapObject()
        },
        createMapObject = function() {
            map.object <<-
                if (map.type == "shape") {
                  map.obj <- sf::st_read(map.vars$shapefile)
                  map.obj <- sf::st_simplify(map.obj, dTolerance = 0.05)
                  iNZightMaps2::iNZightMapPlot(data = activeData,
                                               map = map.obj, 
                                               type = "region",
                                               by.data = map.vars$location.var,
                                               by.map = map.vars$map.var)
                  
                } else {
                  map.obj <- sf::st_read(map.vars$shapefile)
                  map.obj <- sf::st_simplify(map.obj, dTolerance = 0.0005)
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
            if (map.type == "shape") {                
                addHandlerClicked(zoomBtn, function(h, ...) {
                                      if (canIZoom()) {
                                          zoom <- iNZightMaps::sClickOnZoom(3/4)
                                      } else {
                                          gmessage("Cannot zoom when displaying multiple subsets.")
                                      }
                                  })
                addHandlerClicked(zoomOutBtn, function(h, ...) {
                                      if (canIZoom()) {
                                          iNZightMaps::sClickOnZoom(4/3)
                                      } else {
                                          gmessage("Cannot zoom when displaying multiple subsets.")
                                      }
                                  })
            } else {
                addHandlerClicked(zoomBtn, function(h, ...) {
                                      if (canIZoom()) {
                                          iNZightMaps::ClickOnZoom(3/4)
                                      } else {
                                          gmessage("Cannot zoom when displaying multiple subsets.")
                                      }
                                  })
                addHandlerClicked(zoomOutBtn, function(h, ...) {
                                      if (canIZoom()) {
                                          iNZightMaps::ClickOnZoom(4/3)
                                      } else {
                                          gmessage("Cannot zoom when displaying multiple subsets.")
                                      }
                                  })
            }
            
            
            GUI$plotToolbar$update(NULL, refresh = "updatePlot", extra = list(zoomBtn, zoomOutBtn, aboutBtn))

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
                pointCols <- c("grey50", "black", "darkblue", "darkgreen",
                               "darkmagenta", "darkslateblue", "hotpink4",
                               "lightsalmon2", "palegreen3", "steelblue3")
                symbolColList <- gcombobox(
                    pointCols,
                    selected = ifelse(
                        is.na(which(pointCols == map.vars$col.pt)[1]),
                        1,
                        which(pointCols == map.vars$col.pt)[1]),
                    editable = TRUE)
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
                    
                    map.vars$col.pt <<- svalue(symbolColList)
                    map.vars$cex.pt <<- svalue(cexSlider)
                    map.vars$alpha <<- 1 - svalue(transpSlider) / 100
                    map.vars$join <<- svalue(joinPts)
                    map.vars$col.line <<- svalue(joinCol)
                    
                    map.type <<- svalue(typeList)
                }
                
                updatePlot()
            }

            ## in this case, no point in having a separate "show" button
            if (map.type == "shape") {
                addHandlerChanged(colVarList, handler = function(h, ...) updateEverything())
                addHandlerChanged(naFillCol, handler = function(h, ...) updateEverything())
                addHandlerChanged(opctyVarList, handler = function(h, ...) updateEverything())
            } else {
                addHandlerChanged(colVarList, handler = function(h, ...) {
                                      enabled(joinCol) <- svalue(colVarList, TRUE) == 1
                                      updateEverything()
                                  })
                addHandlerChanged(rszVarList, handler = function(h, ...) updateEverything())
                addHandlerChanged(typeList, handler = function(h, ...) updateEverything())
            }

            pcoltimer <- NULL
            addHandlerChanged(symbolColList,
                              handler = function(h, ...) {
                                  if (!is.null(pcoltimer))
                                      pcoltimer$stop_timer()
                                  pcoltimer <- gtimer(200, function(...) {
                                                          if (nchar(svalue(symbolColList)) >= 3)
                                                              updateEverything()
                                                      }, one.shot = TRUE)
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
                            createSlider(pos = 2, val)
                            changePlotSettings(list(
                                g1 = iNZightPlots:::convert.to.factor(
                                    ds[val][[1]]
                                    ),
                                g1.level = "_MULTI",
                                varnames = list(
                                    g1 = val)
                                ))
                        } else {
                            changePlotSettings(list(g1 = NULL,
                                                    g1.level = NULL,
                                                    varnames = list(
                                                        g1 = NULL)
                                                    ), reset = TRUE)
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
            grpData <- iNZightPlots:::convert.to.factor(grpData)
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
                                  lbl <- paste(grp, "level", sep = ".")
                                  changePlotSettings(
                                      structure(list(
                                          as.character(svalue(h$obj)),
                                          structure(list(as.character(svalue(h$obj))),
                                                    .Names = lbl
                                                    )),
                                                .Names = c(lbl, "varnames")
                                                )
                                      )
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

            ## Play button
            ## playBtn <- gbutton("Play", expand = FALSE,
            ##                 handler = function(h, ...) {
            ##                     oldSet <- GUI$getActiveDoc()$getSettings()
            ##                     for (i in 1:length(levels(grpData))) {
            ##                         changePlotSettings(
            ##                             structure(list(i),
            ##                                       .Names = paste(
            ##                                           grp,
            ##                                           "level",
            ##                                           sep = ".")
            ##                                       )
            ##                             )
            ##                       # This effectively freezes the R session,
            ##                       # and therefore iNZight --- so increase with
            ##                       # discression!!!!!
            ##                         Sys.sleep(0.6)
            ##                     }
            ##                     changePlotSettings(oldSet)
            ##                 })
            add(hzGrp, sliderGrp, expand = TRUE)

            ## tbl[pos, 7, anchor = c(0, 0), expand = FALSE] <- playBtn

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
            if (reset)
                extra.args <<- set
            else
                extra.args <<- iNZight:::modifyList(extra.args, set, keep.null = TRUE)

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
            
            map.object <<- iNZightMaps2::setMapping.iNZightMapPlot(map.object,
                                                                   layer.set = "point",
                                                                   layer.name = "baselayer",
                                                                   aes.name = "size",
                                                                   aes.var = map.vars$sizeby)
            
            
          }
          
          pl <- plot(map.object)
          
          return(invisible(pl))
        },
        matchingDialog = function(shapefile) {
          map.obj <- sf::st_read(shapefile)
          
          w.match <- gwindow("Match Variables", width = 400, height = 500, visible = FALSE)
          gv.match <- gvbox(cont = w.match, expand = TRUE, fill = TRUE)
          gv.match$set_borderwidth(15)
          
          map.obj.vars <- as.data.frame(map.obj[, !(colnames(map.obj) %in% "geometry")])
          
          mapvarBox <- gcombobox(items = colnames(map.obj.vars))
          datavarBox <- gcombobox(items = colnames(activeData))
          
          tblmap <- glayout()
          tblmap[1, 1] <- glabel("Map Variable: ")
          tblmap[1, 2, expand = TRUE] <- mapvarBox
          
          tbldata <- glayout()
          tbldata[1, 1] <- glabel("Data Variable: ")
          tbldata[1, 2, expand = TRUE] <- datavarBox
          
          add(gv.match, tblmap)
          add(gv.match, tbldata)
          
          #############################
          
          mapvar <- svalue(mapvarBox)
          datavar <- svalue(datavarBox)
          
          datavar.unq <- unique(as.character(unlist(activeData[, datavar])))
          mapvar.unq <-  unique(as.character(unlist(map.obj.vars[, mapvar])))
          
          mapvar.unq.tbl <<- data.frame(mapname = mapvar.unq,
                                        matchvar = mapvar.unq, dest = "iso3c",
                                        stringsAsFactors = FALSE)
          
          datavar.unq.tbl <- data.frame(dataname = datavar.unq, 
                                        matchvar = datavar.unq, dest = "iso3c",
                                        stringsAsFactors = FALSE)
          
          match.df <- dplyr::left_join(datavar.unq.tbl, mapvar.unq.tbl, by = "matchvar")
          
          #############################
          
          addHandlerChanged(mapvarBox, handler = function(h, ...) {
            mapvar <<- svalue(mapvarBox)
            
            message("Joining on: ", mapvar, " & ", datavar)
            
            mapvar.unq <- unique(as.character(unlist(map.obj.vars[, mapvar])))
            
            if(any(grepl("country", c(datavar, mapvar)))) {
              # matchvar <- countrycode::countrycode(mapvar.unq, origin = "country.name", dest = "iso3c")
              matchvar <- mapvar.unq
            } else {
              matchvar <- mapvar.unq
            }
            
            print(matchvar)
            
            mapvar.unq.tbl <<- data.frame(mapname = mapvar.unq,
                                          matchvar = matchvar,
                                          stringsAsFactors = FALSE)
            
            match.df <- dplyr::left_join(datavar.unq.tbl, mapvar.unq.tbl, by = "matchvar")
            
            
            match.tbl[] <- match.df
          })
          
          addHandlerChanged(datavarBox, handler = function(h, ...) {
            datavar <<- svalue(datavarBox)
            
            message("Joining on: ", mapvar, " & ", datavar)
            
            datavar.unq <- unique(as.character(unlist(activeData[, datavar])))
            
            if(any(grepl("country", c(datavar, mapvar)))) {
              # matchvar <- countrycode::countrycode(datavar.unq, origin = "country.name", dest = "iso3c")
              matchvar <- datavar.unq
            } else {
              matchvar <- datavar.unq
            }
            
            print(matchvar)
            
            datavar.unq.tbl <<- data.frame(dataname = datavar.unq, 
                                           matchvar = matchvar,
                                           stringsAsFactors = FALSE)
            
            match.df <- dplyr::left_join(datavar.unq.tbl, mapvar.unq.tbl, by = "matchvar")
            
            match.tbl[] <- match.df
          })
          
          
          match.tbl <- gdf(match.df)
          add(gv.match, match.tbl, expand = TRUE, fill = TRUE)
          
          match.confirm.btn <- gbutton("OK", handler = function(h, ...) {
            setVars(list(location.var = svalue(datavarBox),
                         map.var = svalue(mapvarBox),
                         shapefile = shapefile),
                    type = "shape")
            
            initiateModule(shape = TRUE)
            dispose(w.match)
          })
          
          add(gv.match, match.confirm.btn)
          
          visible(w.match) <- TRUE
        }
  
      )


)

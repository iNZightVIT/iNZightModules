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
        grpTbl      = "ANY",
        EMPH.LEVEL  = "ANY",
        colourPalettes = "ANY",
        timer = "ANY",
        playButton = "list",
        playdelay = "numeric"
    ),


    # ================================================================================
    # METHOD DEFINITION
    #   + Pre-defined method:
    #     - initialize(): all that relate to module window GUI should be defined here
    # ================================================================================
    methods = list(
        ## Function with all GUI specifications
        initialize = function(GUI) {
            .rcb = TRUE
            .viridis = TRUE
            ## GUI
            initFields(
              GUI = GUI,
              colourPalettes = list(cat = c(
                  if (.rcb)
                    list("contrast (max 8)" =
                           function(n)
                             if (n > 8) iNZightPlots::inzpar()$col.default$cat(n)
                         else RColorBrewer::brewer.pal(n, "Set2")[1:n],
                         "bright (max 9)" =
                           function(n)
                             if (n > 9) iNZightPlots::inzpar()$col.default$cat(n)
                         else RColorBrewer::brewer.pal(n, "Set1")[1:n],
                         "light (max 12)" =
                           function(n)
                             if (n > 12) iNZightPlots::inzpar()$col.default$cat(n)
                         else RColorBrewer::brewer.pal(n, "Set3")[1:n]),
                  if (.viridis)
                    list(viridis = viridis::viridis,
                         magma = viridis::magma,
                         plasma = viridis::plasma,
                         inferno = viridis::inferno),
                  list("Colourblind Friendly" = iNZightPlots::inzpar()$col.default$cat,
                       'rainbow (hcl)' = function(n) hcl((1:n) / n * 360, c = 80, l = 50))
                ),
                cont = c(
                  if (.viridis)
                    list(viridis = viridis::viridis,
                         magma = viridis::magma,
                         plasma = viridis::plasma,
                         inferno = viridis::inferno),
                  list('rainbow (hcl)' = function(n) hcl((1:n) / n * 320 + 60, c = 100, l = 50),
                       blue =
                         function(n) colorspace::sequential_hcl(n, h = 260, c. = c(80, 10), l = c(30, 95), power = 0.7),
                       green =
                         function(n) colorspace::sequential_hcl(n, h = 135, c. = c(50, 10), l = c(40, 95), power = 0.4),
                       red =
                         function(n) colorspace::sequential_hcl(n, h = 10, c. = c(80, 10), l = c(30, 95), power = 0.7),
                       "green-yellow" =
                         function(n) colorspace::terrain_hcl(n, h = c(130, 30), c. = c(65, 0), l = c(45, 90),
                                                 power = c(0.5, 1.5)),
                       "red-blue" =
                         function(n) colorspace::terrain_hcl(n, h = c(0, -100), c. = c(80, 40), l = c(40, 75),
                                                 power = c(1, 1)),
                       terrain = colorspace::terrain_hcl,
                       heat = colorspace::heat_hcl,
                       "blue/white/pink" =
                         function(n) colorspace::diverge_hcl(n, h = c(180, 330), c = 59, l = c(75, 95), power = 1.5),
                       "blue/white/red" =
                         function(n) colorspace::diverge_hcl(n, h = c(260, 0), c = 100, l = c(50, 90), power = 1))
                ),
                emphasize = function(n, k, cat = TRUE, ncat = 5,
                                     fn = if (cat) iNZightPlots::inzpar()$col.default$cat else iNZightPlots::inzpar()$col.default$cont) {
                  cols <- fn(n)
                  if (!cat) {
                    ks <- floor(seq(1, n, length = ncat + 1))
                    k <- ks[k]:ks[k+1]
                  }
                  cols[-k] <- iNZightPlots:::shade(cols[-k], 0.7)
                  cols
                }),
              timer = NULL,
              playdelay = 0.6
            )
            
            EMPH.LEVEL <<- 0

            if (!requireNamespace("iNZightMaps", quietly = TRUE)) {
                resp <- gconfirm("The Maps package isn't installed. Do you want to install it now?",
                                 title = "Install Maps package", icon = "question", parent = GUI$win)

                if (resp) {
                    utils::install.packages("iNZightMaps", repos = c("https://r.docker.stat.auckland.ac.nz",
                                                                     "https://cran.stat.auckland.ac.nz"),
                                            dependencies = TRUE)
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


            title <- glabel("Mapping Variables")
            font(title) <- list(weight = "bold", size = 12, family = "normal")
            add(gv, title, anchor = c(-1, 0))


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
            tbl2 <- glayout(homogeneous = FALSE)
            ii <- 1

            lbl <- "Map Location :"
            mapLoc <- gcombobox(c("", "world"))
            tbl2[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl2[ii, 2:4, anchor = c(-1, 0), expand = TRUE] <- mapLoc
            ii <- ii + 1

            lbl <- "Location Variable :"
            locVar <- gcombobox(c("", characterVars()))
            tbl2[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl2[ii, 2:4, anchor = c(-1, 0), expand = TRUE] <- locVar
            ii <- ii + 1
            
            visible(tbl2) <- FALSE

            addSpace(gv, 10)
            add(gv, tbl, expand = TRUE, fill = TRUE)
            add(gv, tbl2, expand = TRUE, fill = TRUE)
            addSpring(gv)

            ## switch between them using radio buttons
            addHandlerChanged(mapType, function(h, ...) {
                                  v <- svalue(mapType, index = TRUE)
                                  visible(tbl) <- v == 1
                                  visible(tbl2) <- v == 2
                              })

            ## OK Button
            btnGrp <- ggroup(cont = gv)

            addSpring(btnGrp)
            okbtn <- gbutton("OK", expand = TRUE,
                             cont = btnGrp,
                             handler = function(h, ...) {
                                 if (svalue(mapType, index = TRUE) == 1) {
                                     if (svalue(latVar, TRUE) > 1 && svalue(lonVar, TRUE) > 1) {
                                         setVars(list(latitude = svalue(latVar),
                                                      longitude = svalue(lonVar)),
                                                 type = "points")
                                         initiateModule()
                                         dispose(w)
                                     } else {
                                         gmessage("Please select a variable for latitude and longitude")
                                     }
                                 } else {
                                     if (svalue(mapLoc, TRUE) > 1 && svalue(locVar, TRUE) > 1) {
                                         setVars(list(location = svalue(mapLoc),
                                                      location.var = svalue(locVar)),
                                                 type = "shape")
                                         initiateModule(shape = TRUE)
                                         dispose(w)
                                     } else {
                                         gmessage("Please select a map location and variable")
                                     }
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
            map.type <<- ifelse(type == "shape", "shape", "terrain")

            ## defaults:
            map.vars$alpha <<- 1
            map.vars$cex.pt <<- 1
            map.vars$col.pt <<- ifelse(type == "shape", NULL, "mediumvioletred")
            extra.args <<- list()

            createMapObject()
        },
        createMapObject = function() {
            map.object <<-
                if (map.type == "shape") {
                    iNZightMaps::iNZightShapeMap(data = activeData,
                                                 location = map.vars$location,
                                                 data.region = map.vars$location.var)
                } else {
                    iNZightMaps::iNZightMap(lat = eval(parse(text = paste("~", map.vars$latitude))),
                                            lon = eval(parse(text = paste("~", map.vars$longitude))),
                                            data = activeData,
                                            name = GUI$dataNameWidget$datName)
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
            
            GUI$plotToolbar$update(NULL, refresh = "updatePlot", extra = list(aboutBtn))
            
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
            
            font.grouptitle <- list(
              weight = "bold", 
              family = "normal", 
              size = 10
            )
            

# Plot Options ------------------------------------------------------------

            frame.plotoptions <- gframe(horizontal = FALSE)
            group.plotoptions <- ggroup(spacing = 5)
            group.plotoptions$set_borderwidth(10)
            expand.plotoptions <- gexpandgroup(text = "Extra Plot Options", horizontal = FALSE)
            font(expand.plotoptions) <- list(weight = "bold", family = "normal", size = 10)
            
            add(mainGrp, frame.plotoptions)
            add(frame.plotoptions, group.plotoptions, expand = TRUE)
            add(group.plotoptions, expand.plotoptions, expand = TRUE)
            
            visible(expand.plotoptions) <- FALSE
            
            tbl.plotoptions <- glayout()
            
            add(expand.plotoptions, tbl.plotoptions)
            ii.plotopt <- 1
            
            if (map.type != "shape") {
              lbl <- glabel("Map type :")
              typeOpts <- c("terrain", "terrain-background", "toner-lite", "toner")
              typeList <- gcombobox(typeOpts)
              tbl.plotoptions[ii.plotopt, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl
              tbl.plotoptions[ii.plotopt, 3:6, expand = TRUE] <- typeList
              ii.plotopt <- ii.plotopt + 1
              
              lbl.title <- glabel("Plot title:")
              edit.title <- gedit()
              
              tbl.plotoptions[ii.plotopt, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl.title
              tbl.plotoptions[ii.plotopt, 3:6, expand = TRUE] <- edit.title
              ii.plotopt <- ii.plotopt + 1
              
              lbl.limitxaxis <- glabel("x-axis :")
              lbl.limityaxis <- glabel("y-axis :")
              edit.limitxaxisMin <- gedit()
              edit.limitxaxisMax <- gedit()
              edit.limityaxisMin <- gedit()
              edit.limityaxisMax <- gedit()
              
              # tbl.plotoptions[ii.plotopt, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl.limitxaxis
              # tbl.plotoptions[ii.plotopt, 3, expand = TRUE] <- edit.limitxaxisMin
              # tbl.plotoptions[ii.plotopt, 6, expand = TRUE] <- edit.limitxaxisMax
              # ii.plotopt <- ii.plotopt + 1
              # 
              # tbl.plotoptions[ii.plotopt, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl.limityaxis
              # tbl.plotoptions[ii.plotopt, 3, expand = TRUE] <- edit.limityaxisMin
              # tbl.plotoptions[ii.plotopt, 6, expand = TRUE] <- edit.limityaxisMax
              # ii.plotopt <- ii.plotopt + 1
              
              lbl.plotsize <- glabel("Overall size scale:")
              slider.plotsize <- gslider(0.5, 2, 0.05, value = 1)
              
              tbl.plotoptions[ii.plotopt, 1:2, anchor = c(-1, 0), expand = TRUE] <- lbl.plotsize
              tbl.plotoptions[ii.plotopt, 3, expand = TRUE] <- slider.plotsize
              ii.plotopt <- ii.plotopt + 1
            }

# Colour Options ----------------------------------------------------------
            
            frame.colour <- gframe(horizontal = FALSE)
            group.colour <- ggroup(spacing = 5)
            group.colour$set_borderwidth(10)
            expand.colour <- gexpandgroup(text = "Colour", horizontal = FALSE)
            font(expand.colour) <- font.grouptitle
            
            add(mainGrp, frame.colour)
            add(frame.colour, group.colour, expand = TRUE)
            add(group.colour, expand.colour, expand = TRUE)
            
            visible(expand.colour) <- FALSE

            tbl.colour <- glayout()
            
            add(expand.colour, tbl.colour)
            ii.colour <- 1

            if (map.type != "shape") {
              lbl.colourstatic <- glabel("Point colour :")
              pointCols <- c("mediumvioletred", "grey50", "black", "darkblue", "darkgreen",
                             "darkmagenta", "darkslateblue", "hotpink4",
                             "lightsalmon2", "palegreen3", "steelblue3")
              symbolColList <- gcombobox(
                pointCols,
                selected = ifelse(
                  is.na(which(pointCols == map.vars$col.pt)[1]),
                  1,
                  which(pointCols == map.vars$col.pt)[1]),
                editable = TRUE)
              
            tbl.colour[ii.colour,  1:2, anchor = c(1, 0), expand = TRUE] <- lbl.colourstatic
            tbl.colour[ii.colour,  3:6, expand = TRUE] <- symbolColList
            ii.colour <- ii.colour + 1
            
            sep.colour <- gseparator()
            tbl.colour[ii.colour, 1:6, expand = TRUE] <- sep.colour
            ii.colour <- ii.colour + 1
              
              
              lbl.colour <- glabel("Colour by :")
              colVarList <- gcombobox(
                c("", names(GUI$getActiveData())),
                selected = ifelse(
                  is.null(map.vars$colby),
                  1, 
                  which(names(GUI$getActiveData()) == map.vars$colby)[1] + 1
                )
              )
              tbl.colour[ii.colour, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl.colour
              tbl.colour[ii.colour, 3:6, expand = TRUE] <- colVarList
              ii.colour <- ii.colour + 1

              lbl.palette <- glabel("Palette:")
              combobox.paletteCont <- gcombobox(names(colourPalettes$cont))
              combobox.paletteCat <- gcombobox(names(colourPalettes$cat))
              
              tbl.colour[ii.colour, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl.palette
              tbl.colour[ii.colour, 3:6, expand = TRUE] <- combobox.paletteCont
              tbl.colour[ii.colour, 3:6, expand = TRUE] <- combobox.paletteCat
              ii.colour <- ii.colour + 1

              checkbox.reverse <- gcheckbox("Reverse palette")
              checkbox.ranks <- gcheckbox("Use Percentiles")

              tbl.colour[ii.colour, 3:4, expand = TRUE] <- checkbox.reverse
              tbl.colour[ii.colour, 5:6, expand = TRUE] <- checkbox.ranks
              ii.colour <- ii.colour + 1
              
              lbl.quantilecycle <- glabel("Cycle quantiles:")
              
              lbl.quantilenumber <- glabel("# quantiles:")
              cycleN <- gspinbutton(4)
              
              cyclePrev <- iNZight:::gimagebutton(stock.id = "1leftarrow")
              cycleNext <- iNZight:::gimagebutton(stock.id = "1rightarrow")
              cycleStop <- iNZight:::gimagebutton(filename = system.file("images/icon-undo.png", package = "iNZight"))
              
              tbl.colour[ii.colour, 1, expand = TRUE] <- lbl.quantilecycle
              tbl.colour[ii.colour, 2, expand = TRUE] <- cyclePrev
              tbl.colour[ii.colour, 3, expand = TRUE] <- cycleNext
              tbl.colour[ii.colour, 4, expand = TRUE] <- cycleStop
              tbl.colour[ii.colour, 5, expand = TRUE] <- lbl.quantilenumber
              tbl.colour[ii.colour, 6, expand = TRUE] <- cycleN
              ii.colour <- ii.colour + 1
              
              controls.colour <- list(
                lbl.palette,
                combobox.paletteCont,
                combobox.paletteCat,
                checkbox.reverse,
                lbl.quantilecycle,
                cyclePrev,
                cycleNext,
                cycleStop
              )
              
              for (control in controls.colour) {
                visible(control) <- svalue(colVarList, TRUE) > 1
              }
              
              if (svalue(colVarList, TRUE) > 1) {
                if (svalue(colVarList) %in% numericVars()) {
                  svalue(lbl.quantilecycle) <- "Cycle quantiles:"
                  visible(lbl.quantilenumber) <- TRUE
                  visible(cycleN) <- TRUE
                  visible(checkbox.ranks) <- TRUE
                } else {
                  svalue(lbl.quantilecycle) <- "Cycle levels:"
                  visible(lbl.quantilenumber) <- FALSE
                  visible(cycleN) <- FALSE
                  visible(checkbox.ranks) <- FALSE
                }
              } else {
                visible(lbl.quantilenumber) <- FALSE
                visible(cycleN) <- FALSE
                visible(checkbox.ranks) <- FALSE
              }
            }

# Size Options ------------------------------------------------------------

            frame.size <- gframe(horizontal = FALSE)
            group.size <- ggroup(spacing = 5)
            group.size$set_borderwidth(10)
            expand.size <- gexpandgroup(text = "Size", horizontal = FALSE)
            font(expand.size) <- list(weight = "bold", family = "normal", size = 10)
            
            add(mainGrp, frame.size)
            add(frame.size, group.size, expand = TRUE)
            add(group.size, expand.size, expand = TRUE)
            
            visible(expand.size) <- FALSE
            
            tbl.size <- glayout()
            
            add(expand.size, tbl.size)
            ii.size <- 1
            
            if (map.type != "shape") {
              lbl.size <- glabel("Overall:")
              cexSlider <- gslider(from = 0.05, to = 3.5, by = 0.05, value = map.vars$cex.pt)
              tbl.size[ii.size, 2:3, anchor = c(1, 0), expand = TRUE] <- lbl.size
              tbl.size[ii.size, 4:6, expand = TRUE] <- cexSlider
              ii.size <- ii.size + 1
              
              lbl.sizeby <- glabel("Size by :")
              rszVarList <- gcombobox(
                c("", rszNames <- names(activeData)[sapply(activeData, is.numeric)]),
                selected = ifelse(
                  is.null(map.vars$sizeby),
                  1, which(rszNames == map.vars$sizeby)[1] + 1
                )
              )
              tbl.size[ii.size, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl.sizeby
              tbl.size[ii.size, 3:6, expand = TRUE] <- rszVarList
              ii.size <- ii.size + 1
              
              lbl.sizemethod <- glabel("Resize method:")
              combobox.sizemethod <- gcombobox(c("proportional", "emphasize"))
              tbl.size[ii.size, 2:3, anchor = c(1, 0), expand = TRUE] <- lbl.sizemethod
              tbl.size[ii.size, 4:6, expand = TRUE] <- combobox.sizemethod
              ii.size <- ii.size + 1
              
              visible(lbl.sizemethod) <- FALSE
              visible(combobox.sizemethod) <- FALSE
            }
            
# Opacity Options ---------------------------------------------------------

            frame.opacity <- gframe(horizontal = FALSE)
            group.opacity <- ggroup(spacing = 5)
            group.opacity$set_borderwidth(10)
            expand.opacity <- gexpandgroup(text = "Opacity", horizontal = FALSE)
            font(expand.opacity) <- list(weight = "bold", family = "normal", size = 10)
            
            add(mainGrp, frame.opacity)
            add(frame.opacity, group.opacity, expand = TRUE)
            add(group.opacity, expand.opacity, expand = TRUE)
            
            visible(expand.opacity) <- FALSE
            
            tbl.opacity <- glayout()
            
            add(expand.opacity, tbl.opacity)
            ii.opacity <- 1
            
            if (map.type != "shape") {
              ## Transparency
              lbl.transp <- glabel("Overall:")
              transpSlider <- gslider(from = 0, to = 100,
                                      by = 1, value = 100 * (1 - map.vars$alpha))
              tbl.opacity[ii.opacity, 2:3, anchor = c(1, 0), expand = TRUE] <- lbl.transp
              tbl.opacity[ii.opacity, 4:6, expand = TRUE] <- transpSlider
              ii.opacity <- ii.opacity + 1
              
              lbl.opacityby <- glabel("Opacify by :")
              opctyVarList <- gcombobox(
                c("", numNames <- names(activeData)[sapply(activeData, is.numeric)]),
                selected = ifelse(
                  is.null(map.vars$opacity),
                  1, which(numNames == map.vars$opacity)[1] + 1
                )
              )
              tbl.opacity[ii.opacity, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl.opacityby
              tbl.opacity[ii.opacity, 3:6, expand = TRUE] <- opctyVarList
              ii.opacity <- ii.opacity + 1
              
              checkbox.opacityrev <- gcheckbox("Reverse Opacification")
              tbl.opacity[ii.opacity, 1:4, anchor = c(1, 0), expand = TRUE] <- checkbox.opacityrev
              ii.opacity <- ii.opacity + 1
            }
            
# Shape Options ---------------------------------------------------------
            
            frame.shape <- gframe(horizontal = FALSE)
            group.shape <- ggroup(spacing = 5)
            group.shape$set_borderwidth(10)
            expand.shape <- gexpandgroup(text = "Point Symbol", horizontal = FALSE)
            font(expand.shape) <- list(weight = "bold", family = "normal", size = 10)
            
            add(mainGrp, frame.shape)
            add(frame.shape, group.shape, expand = TRUE)
            add(group.shape, expand.shape, expand = TRUE)
            
            visible(expand.shape) <- FALSE
            
            tbl.shape <- glayout()
            
            add(expand.shape, tbl.shape)
            ii.shape <- 1
            
            if (map.type != "shape") {
              symbolList <- c(
                "circle"            = 21,
                "square"            = 22,
                "diamond"           = 23,
                "triangle"          = 24,
                "inverted triangle" = 25
              )
              
              lbl.symbol <- glabel("Symbol:")
              combobox.symbol <- gcombobox(names(symbolList), selected = 1)
              
              tbl.shape[ii.shape, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl.symbol
              tbl.shape[ii.shape, 3:6, expand = TRUE] <- combobox.symbol
              ii.shape <- ii.shape + 1
              
              sep.shape <- gseparator()
              tbl.shape[ii.shape, 1:6, expand = TRUE] <- sep.shape
              ii.shape <- ii.shape + 1
              
              lbl.shapeby <- glabel("Symbol by :")
              dropdown.shape <- gcombobox(
                c("", numNames <- characterVars()),
                selected = ifelse(
                  is.null(map.vars$symbolby),
                  1, which(numNames == map.vars$symbolby)[1] + 1
                )
              )
              tbl.shape[ii.shape, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl.shapeby
              tbl.shape[ii.shape, 3:6, expand = TRUE] <- dropdown.shape
              ii.shape <- ii.shape + 1
              
              lbl.symbolwidth <- glabel("Symbol line width:")
              spin.symbolwidth <- gspinbutton(1, 4, by = 1, value = ifelse(is.null(map.vars$lwd.pt), 2, map.vars$lwd.pt))
              
              tbl.shape[ii.shape, 1:2, anchor = c(1, 0), expand = TRUE] <- lbl.symbolwidth
              tbl.shape[ii.shape, 3:4, expand = TRUE] <- spin.symbolwidth
              ii.shape <- ii.shape + 1
            }
            
# Connect Options ---------------------------------------------------------

            frame.connect <- gframe(horizontal = FALSE)
            group.connect <- ggroup(spacing = 5)
            group.connect$set_borderwidth(10)
            expand.connect <- gexpandgroup(text = "Connect Points", horizontal = FALSE)
            font(expand.connect) <- list(weight = "bold", family = "normal", size = 10)
            
            add(mainGrp, frame.connect)
            add(frame.connect, group.connect, expand = TRUE)
            add(group.connect, expand.connect, expand = TRUE)
            
            visible(expand.connect) <- FALSE
            
            tbl.connect <- glayout()
            
            add(expand.connect, tbl.connect)

            if (map.type != "shape") {
              joinPts <- gcheckbox("Connect points with lines", checked = FALSE)
              
              lbl.connectcolour <- glabel("Line colour:")
              joinCols <- c("red", "black", "blue", "green4", "yellow", "pink", "grey", "orange")
              joinCol <- gcombobox(joinCols)
              
              lbl.linewidth <- glabel("Line width:")
              slider.linewidth <- gslider(1, 10)
              
              visible(lbl.connectcolour) <- FALSE
              visible(joinCol) <- FALSE
              visible(lbl.linewidth) <- FALSE
              visible(slider.linewidth) <- FALSE
              
              if (!is.null(map.vars$join)) {
                svalue(joinPts) <- map.vars$join
                visible(lbl.connectcolour) <- map.vars$join
                visible(joinCol) <- map.vars$join
                visible(lbl.linewidth) <- map.vars$join
                visible(slider.linewidth) <- map.vars$join
              }

              if (!is.null(map.vars$col.line)) {
                if (map.vars$col.line %in% joinCols) {
                  svalue(joinCol) <- which(joinCols == map.vars$col.line)
                }
              }
              
              # enabled(joinCol) <- svalue(colVarList, TRUE) == 1
              
              tbl.connect[1, 1:6, expand = TRUE, anchor = c(-1, 0)] <- joinPts
              tbl.connect[2, 3:4, expand = TRUE] <- lbl.connectcolour
              tbl.connect[2, 5:6, expand = TRUE] <- joinCol
              tbl.connect[3, 3:4, expand = TRUE, anchor = c(-1, 0)] <- lbl.linewidth
              tbl.connect[3, 5:6, expand = TRUE] <- slider.linewidth
              
              addHandlerChanged(joinPts, function(h, ...) {
                visible(lbl.connectcolour) <- svalue(joinPts)
                visible(joinCol) <- svalue(joinPts)
                visible(lbl.linewidth) <- svalue(joinPts)
                visible(slider.linewidth) <- svalue(joinPts)
                
                updateEverything()
              })
              
              addHandlerChanged(joinCol, function(h, ...) updateEverything())
              addHandlerChanged(slider.linewidth, function(h, ...) {
                if (!is.null(timer))
                  if (timer$started) timer$stop_timer()
                timer <<- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
              })
            }

            if (map.type == "shape") {
                yVarList <- gcombobox(
                    c("Select Variable", rszNames <- names(activeData)[sapply(activeData, is.numeric)]),
                    selected = ifelse(
                        is.null(map.vars$y),
                        1, which(rszNames == map.vars$y)[1] + 1
                        )
                    )
                tbl[ii, 1:6, expand = TRUE] <- yVarList
                ii <- ii + 1
            }



            ## COLOUR
            if (map.type == "shape") {
                lbl <- glabel("Shape colour :")
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
            }


            if (map.type == "shape") {
              tbl.plotoptions[ii.plotopt, 1:2, anchor = c(1, 0), expand = TRUE] <- glabel("Missing value colour :")
              tbl.plotoptions[ii.plotopt, 3:6, expand = TRUE] <- naFillCol
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
                    map.vars$y <<- svalue(yVarList)
                    map.vars$col <<- svalue(symbolColList)
                    map.vars$na.fill <<- svalue(naFillCol)
                    map.vars$map.labels <<- svalue(mapLbls, index = TRUE)
                } else {
                    if (svalue(colVarList, TRUE) > 1) {
                      map.vars$colby <<- svalue(colVarList)

                    map.vars$col.fun <<- if (EMPH.LEVEL > 0) {
                        function(n)
                          colourPalettes$emphasize(
                            n, k = EMPH.LEVEL, cat = is.factor(map.object[[map.vars$colby]]),
                            ncat = svalue(cycleN),
                            fn = if (map.vars$colby %in% numericVars()) {
                              colourPalettes$cont[[svalue(combobox.paletteCont)]]
                            } else {
                              colourPalettes$cat[[svalue(combobox.paletteCat)]]
                            }
                      )
                    } else {
                      if (map.vars$colby %in% numericVars()) {
                        colourPalettes$cont[[svalue(combobox.paletteCont)]]
                      } else {
                        colourPalettes$cat[[svalue(combobox.paletteCat)]]
                      }
                    }
                      
                      map.vars$reverse.palette <<- svalue(checkbox.reverse)
                      map.vars$col.method <<- ifelse(svalue(checkbox.ranks), "rank", "linear")
                    } else {
                      map.vars$colby <<- NULL
                    }
                  
                    if (svalue(rszVarList, TRUE) > 1) {
                      map.vars$sizeby <<- svalue(rszVarList)
                      map.vars$resize.method <<- svalue(combobox.sizemethod)
                    } else { 
                      map.vars$sizeby <<- NULL
                      map.vars$resize.method <<- NULL
                    }
                  
                    if (svalue(opctyVarList, TRUE) > 1) {
                      map.vars$opacity <<- svalue(opctyVarList)
                      map.vars$reverse.opacity <<- svalue(checkbox.opacityrev)
                    } else {
                      map.vars$opacity <<- NULL
                      map.vars$reverse.opacity <<- NULL
                    }
                  
                    if (svalue(dropdown.shape, TRUE) > 1) {
                      map.vars$symbolby <<- svalue(dropdown.shape)
                    } else {
                      map.vars$symbolby <<- NULL
                    }
                  
                    if (isTRUE(svalue(edit.title) != "")) {
                      map.vars$main <<- svalue(edit.title)
                    } else {
                      map.vars$main <<- NULL
                    }
                  
                    map.vars$col.pt <<- svalue(symbolColList)
                    map.vars$cex.pt <<- svalue(cexSlider)
                    map.vars$alpha <<- 1 - svalue(transpSlider) / 100
                    map.vars$join <<- svalue(joinPts)
                    map.vars$col.line <<- svalue(joinCol)
                    map.vars$lwd <<- svalue(slider.linewidth)
                    
                    symbolList <- c(
                      "circle"            = 21,
                      "square"            = 22,
                      "diamond"           = 23,
                      "triangle"          = 24,
                      "inverted triangle" = 25
                    )
                    
                    map.vars$pch <<- symbolList[svalue(combobox.symbol)]
                    map.vars$lwd.pt <<- svalue(spin.symbolwidth)
                    
                    map.vars$cex <<- svalue(slider.plotsize)
                    
                    map.type <<- svalue(typeList)
                }
                
                updatePlot()
            }
            
            changeExpandTitle <- function(expandgroup, title, var, font = font.grouptitle) {
              if (var > 0) {
                expandgroup$set_names(sprintf("%s (%s)", title, var))
              } else {
                expandgroup$set_names(title)
              }
              
              font(expandgroup) <- font
            }
            
            changeVisibleControls <- function(controls, based.on) {
              for (control in controls) {
                visible(control) <- svalue(based.on, TRUE) > 1
              }
            }

            ## in this case, no point in having a separate "show" button
            if (map.type == "shape") {
                addHandlerChanged(yVarList, handler = function(h, ...) if (svalue(h$obj, TRUE) > 1) updateEverything())
                addHandlerChanged(naFillCol, handler = function(h, ...) updateEverything())
            } else {
                addHandlerChanged(colVarList, handler = function(h, ...) {
                  changeExpandTitle(expand.colour, "Colour", svalue(colVarList))

                  changeVisibleControls(controls.colour, colVarList)
                  
                  if (svalue(colVarList, TRUE) > 1) {
                    visible(combobox.paletteCont) <- svalue(colVarList) %in% numericVars()
                    visible(combobox.paletteCat) <- !(svalue(colVarList) %in% numericVars())
                    
                    if (svalue(colVarList) %in% numericVars()) {
                      svalue(lbl.quantilecycle) <- "Cycle quantiles:"
                      visible(lbl.quantilenumber) <- TRUE
                      visible(cycleN) <- TRUE
                      visible(checkbox.ranks) <- TRUE
                    } else {
                      svalue(lbl.quantilecycle) <- "Cycle levels:"
                      visible(lbl.quantilenumber) <- FALSE
                      visible(cycleN) <- FALSE
                      visible(checkbox.ranks) <- FALSE
                    }
                  } else {
                    visible(lbl.quantilenumber) <- FALSE
                    visible(cycleN) <- FALSE
                    visible(checkbox.ranks) <- FALSE
                  }

                  visible(lbl.colourstatic) <- svalue(colVarList, TRUE) == 1
                  visible(symbolColList) <- svalue(colVarList, TRUE) == 1
                  visible(sep.colour) <- svalue(colVarList, TRUE) == 1
                  
                  enabled(joinCol) <- !(svalue(colVarList, TRUE) > 1 && svalue(colVarList) %in% characterVars())
                  updateEverything()
                })
              
                addHandlerChanged(rszVarList, handler = function(h, ...) {
                  changeExpandTitle(expand.size, "Size", svalue(rszVarList))
                  
                  visible(lbl.sizemethod) <- svalue(rszVarList, TRUE) > 1
                  visible(combobox.sizemethod) <- svalue(rszVarList, TRUE) > 1
                  
                  updateEverything()
                })
                
                addHandlerChanged(opctyVarList, handler = function(h, ...) {
                  changeExpandTitle(expand.opacity, "Opacity", svalue(opctyVarList))
                  
                  updateEverything()
                })
                
                addHandlerChanged(dropdown.shape, handler = function(h, ...) {
                  visible(lbl.symbol) <- !isTRUE(svalue(dropdown.shape) != "")
                  visible(combobox.symbol) <- !isTRUE(svalue(dropdown.shape) != "")
                  visible(sep.shape) <- !isTRUE(svalue(dropdown.shape) != "")
                  
                  changeExpandTitle(expand.shape, "Point Symbol", svalue(dropdown.shape))
                  
                  updateEverything()
                })
                
                addHandlerChanged(typeList, handler = function(h, ...) updateEverything())
                addHandlerChanged(combobox.paletteCont, handler = function(h, ...) updateEverything())
                addHandlerChanged(combobox.paletteCat, handler = function(h, ...) updateEverything())
                addHandlerChanged(checkbox.reverse, handler = function(h, ...) updateEverything())
                addHandlerChanged(checkbox.ranks, handler = function(h, ...) updateEverything())
                
                addHandlerChanged(combobox.sizemethod, handler = function(h, ...) updateEverything())
                
                addHandlerChanged(edit.title, handler = function(h, ...) updateEverything())
                
                addHandlerChanged(slider.plotsize, handler = function(h, ...) {
                  if (!is.null(timer))
                    if (timer$started) timer$stop_timer()
                  timer <<- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
                })
                
                addHandlerChanged(cyclePrev, function(h, ...) {
                  nl <- if (map.vars$colby %in% characterVars()) {
                    length(levels(map.object[[map.vars$colby]]))
                  } else {
                    svalue(cycleN)
                  }
                  EMPH.LEVEL <<- ifelse(EMPH.LEVEL == 0, nl, EMPH.LEVEL - 1)
                  updateEverything()
                })
                
                addHandlerChanged(cycleNext, handler = function(h, ...) {
                    nl <- if (map.vars$colby %in% characterVars()) {
                      length(levels(map.object[[map.vars$colby]]))
                    } else { 
                      svalue(cycleN)
                    }
                    EMPH.LEVEL <<- ifelse(EMPH.LEVEL == nl, 0, EMPH.LEVEL + 1)
                    updateEverything()
                })
                
                addHandlerChanged(cycleStop, handler = function(h, ...) {
                  EMPH.LEVEL <<- 0
                  updateEverything()
                })
            }

            addHandlerChanged(symbolColList,
                              handler = function(h, ...) {
                                  if (!is.null(timer))
                                      timer$stop_timer()
                                  timer <<- gtimer(200, function(...) {
                                                          if (nchar(svalue(symbolColList)) >= 3)
                                                              updateEverything()
                                                      }, one.shot = TRUE)
                              })

            if (map.type != "shape") {
                addHandlerChanged(cexSlider,
                                  handler = function(h, ...) {
                                      if (!is.null(timer))
                                          timer$stop_timer()
                                      timer <<- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
                                  })
                
                addHandlerChanged(transpSlider,
                                  handler = function(h, ...) {
                                      if (!is.null(timer))
                                          timer$stop_timer()
                                      timer <<- gtimer(500, function(...) updateEverything(), one.shot = TRUE)
                                  })
                addHandlerChanged(checkbox.opacityrev, handler = function(h, ...) updateEverything())
                
                addHandlerChanged(combobox.symbol, handler = function(h, ...) updateEverything())
                addHandlerChanged(spin.symbolwidth, handler = function(h, ...) updateEverything())
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
            PLAY <- function(data) {
              playButton$levi <<- playButton$levi + 1
              if (playButton$levi > playButton$Nlev) {
                playButton$playtimer$stop_timer()
                playBtn$set_value(img.playicon)
                playButton$playtimer <<- NULL
              } else {
                changePlotSettings(structure(list(playButton$levi, data$varnames),
                                             .Names = c(paste(grp, "level", sep = "."), "varnames")))
                ri <- playButton$row
                tb <- slider
                blockHandlers(tb)
                ## This line creates "IA__gtk_table_attach: assertion 'child->parent == NULL' failed" error.
                svalue(tb, index = TRUE) <- playButton$levi + 1
                unblockHandlers(tb)
              }
            }
            clickPlay <- function(h, ...) {
              if (!is.null(playButton$playtimer)) {
                ## time is running - so stop the animation
                playButton$playtimer$stop_timer()
                playBtn$set_value(img.playicon)
                playButton$playtimer <<- NULL
                return()
              }
              oldSet <- GUI$getActiveDoc()$getSettings()
              playBtn$set_value(img.stopicon)
              pr <- h$obj$parent
              wc <- which(sapply(pr$child_positions, function(x) identical(h$obj, x$child)))
              playButton <<- list(playtimer = NULL, row = pr$child_positions[[wc]]$x,
                                  Nlev = length(levels(grpData)),
                                  levi = 0, oldSet = oldSet)
              PLAY(oldSet)
              playButton$playtimer <<- gtimer(playdelay * 1000, PLAY, data = oldSet, one.shot = FALSE)
            }
            img.playicon <- system.file("images/icon-play.png", package = "iNZight")
            img.stopicon <- system.file("images/icon-stop.png", package = "iNZight")
            playBtn <- gimagebutton(filename = img.playicon, size = "button", handler = clickPlay,
                                    tooltip = "Play through levels")
            
            ## Play time delay - time in milliseconds
            img.clockicon <- system.file("images/icon-clock.png", package = "iNZight")
            delayBtn <- gimagebutton(filename = img.clockicon, size = "button",
                                     tooltip = "Set play timing options",
                                     handler = function(h, ...) {
                                       w <- gwindow(title = "Play Settings", width = 200, height = 80,
                                                    parent = GUI$win)
                                       g <- gvbox(spacing = 10, container = w)
                                       g$set_borderwidth(10)
                                       
                                       g1 <- ggroup(container = g)
                                       glabel("Time delay between plots :", container = g1)
                                       spin <- gspinbutton(from = 0.1, to = 3, by = 0.1, value = playdelay, container = g1)
                                       glabel("(seconds)", container = g1)
                                       
                                       g2 <- ggroup(container = g)
                                       addSpring(g2)
                                       gbutton("OK", container = g, handler = function(h, ...) {
                                         playdelay <<- svalue(spin)
                                         dispose(w)
                                       })
                                     })
            delaySpin <- gspinbutton(from = 0.1, to = 3, by = 0.1, value = playdelay,
                                     handler = function(h, ...) playdelay <<- svalue(h$obj))
            
            add(hzGrp, sliderGrp, expand = TRUE)

            tbl[pos, 6, anchor = c(0, 0), expand = FALSE] <- delayBtn
            tbl[pos, 7, anchor = c(0, 0), expand = FALSE] <- playBtn
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
                if (!is.null(map.vars$y)) {
                    args$variable <- eval(parse(text = paste("~", map.vars$y)))
                    args$varnames$x = map.vars$y
                    args$varnames$y = map.vars$location.var
                } else return(invisible(NULL))

                switch(map.vars$col,
                       "heat" = ,
                       "terrain" = {
                           args$col.fun <- map.vars$col
                       },
                       {
                           args$col.fun <- NULL
                           args$col <- map.vars$col
                       })
                
                args$na.fill <- map.vars$na.fill

                args$name <- switch(map.vars$map.labels,
                                    "", "r", "v", "b")

            } else {
                if (!is.null(map.vars$colby)) {
                    args$colby <- activeData[[map.vars$colby]]
                    args$varnames$colby = map.vars$colby
                    args$col.fun <- map.vars$col.fun
                    args$reverse.palette <- map.vars$reverse.palette
                    args$col.method <- map.vars$col.method
                }
                if (!is.null(map.vars$sizeby)) {
                    args$sizeby <- activeData[[map.vars$sizeby]]
                    args$varnames$sizeby = map.vars$sizeby
                    args$resize.method <- map.vars$resize.method
                }
                if (!is.null(map.vars$opacity)) {
                    args$opacity <- map.vars$opacity
                    args$varnames$opacity = map.vars$opacity
                    # args$reverse.opacity <- map.vars$reverse.opacity
                }
              
                if (!is.null(map.vars$symbolby)) {
                  args$symbolby <- activeData[[map.vars$symbolby]]
                  args$varnames$symbolby <- map.vars$symbolby
                }
              
                if (!is.null(map.vars$main)) {
                  args$main <- map.vars$main
                }

                args$col.pt <- map.vars$col.pt
                args$cex.pt <- map.vars$cex.pt
                args$alpha <- map.vars$alpha
                args$join <- map.vars$join
                args$col.line <- map.vars$col.line
                args$lwd <- map.vars$lwd
                args$pch <- map.vars$pch
                args$lwd.pt <- map.vars$lwd.pt
                
                args$cex <- map.vars$cex
                
                args$type <- map.type
            }

            
            if (!is.null(map.vars$g1)) {
                args$varnames$g1 = map.vars$g1
                if (!is.null(map.vars$g1.level))
                    args$varnames$g1.level <- map.vars$g1.level
            }
            if (!is.null(map.vars$g2)) {
                args$varnames$g2 = map.vars$g2
                if (!is.null(map.vars$g2.level))
                    args$varnames$g2.level <- map.vars$g2.level
            }            

            if (!is.null(extra.args))
                args <- c(args, extra.args)

            pl <- do.call(plot, args)
            GUI$plotType <<- map.type #attr(pl, "plottype")
            return(invisible(pl))
        }
    )

)

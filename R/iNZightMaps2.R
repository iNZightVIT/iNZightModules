iNZightMap2Mod <- setRefClass(
    "iNZightMap2Mod",

    fields = list(
        GUI = "ANY",
        mainGrp = "ANY",

        activeData = "data.frame",
        mapData = "sf",
        combinedData = "ANY",
        staleMap = "logical",
        has.multipleobs = "logical",
        mapFilename = "character",

        mapName = "character",
        mapType = "ANY",
        mapVars = "ANY",
        mapSizeVar = "ANY",

        plotTitle = "ANY",
        plotAxes = "logical",
        plotXLab = "ANY",
        plotYLab = "ANY",
        plotDatumLines = "ANY",
        plotProjection = "ANY",
        plotTheme = "ANY",

        multipleObsOption = "ANY",

        codeHistory = "ANY"
    ),

    methods = list(
        initialize = function(GUI) {
            initFields(GUI = GUI)

            ## TODO: Check package name, details, etc.
            if (!requireNamespace("iNZightMaps", quietly = TRUE)) {
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
            activeData <<- GUI$getActiveData()

            mapName <<- ""
            mapType <<- NULL
            mapVars <<- NULL
            mapSizeVar <<- NULL

            plotTitle <<- ""
            plotAxes <<- FALSE
            plotXLab <<- "Longitude"
            plotYLab <<- "Latitude"
            plotDatumLines <<- FALSE
            plotProjection <<- NULL
            plotTheme <<- "Default"

            multipleObsOption <<- NULL

            mapTypeDialog()
        },

        mapTypeDialog = function() {
            w <- gwindow("Define Geographical Variables",
                         width = 400,
                         height = 500,
                         parent = GUI$win,
                         visible = FALSE)

            gv <- gvbox(cont = w, expand = TRUE, fill = TRUE)
            gv$set_borderwidth(15)

            lbl <- glabel("Type of Map Data")
            font(lbl) <- list(weight = "bold", size = 12, family = "normal")
            radioMapType <- gradio(c("Coordinate (latitude, longitude)",
                                     "Regions (country, state, county, etc.)"))
            add(gv, lbl)
            add(gv, radioMapType)

            coord.or.region <- any(grepl("(latitude|longitude)", colnames(activeData), TRUE))
            svalue(radioMapType, index = TRUE) <- if(coord.or.region) 1 else 2

            addSpace(gv, 10)

            btnFinish <- gbutton("OK")
            add(gv, btnFinish)

            addHandlerClicked(btnFinish, function(h, ...) {
                if(svalue(radioMapType, index = TRUE) == 1) {
                    print("Coordinate")
                } else {
                    dispose(w)
                    importDialog()
                }
            })

            visible(w) <- TRUE
        },
        importDialog = function() {
            GUI$initializeModuleWindow(.self)
            lbl.inzightmaps <- glabel("iNZight Maps")
            font(lbl.inzightmaps) <- list(weight = "bold",
                                          family = "normal",
                                          size   = 12)
                                        # General variables
            
            ## Variables used later on in the merge variable selection section
            nomatch.df <- data.frame(var.name = "")
            staleMap <<- FALSE
            
            ## Section heading font
            font.header <- list(weight = "bold", size = 12, family = "normal")
            
                                        # Create window, etc.
            
            ## Overall Layout
            gv.match <- gvbox(container = GUI$moduleWindow, expand = TRUE, fill = TRUE)
            gv.match$set_borderwidth(15)
            add(gv.match, lbl.inzightmaps, anchor = c(0, 0))
            addSpace(gv.match, 10)
            
            ## Expandable boxes 
            frame.import <- gframe(horizontal = FALSE)
            group.import <- ggroup(spacing = 5)
            group.import$set_borderwidth(10)
            expand.import <- gexpandgroup(text = "Select Map", horizontal = FALSE)
            font(expand.import)    <- font.header
            
            frame.variables <- gframe(horizontal = FALSE)
            group.variables <- ggroup(spacing = 5)
            group.variables$set_borderwidth(10)
            expand.variables <- gexpandgroup(text = "Select Variables", horizontal = FALSE)
            font(expand.variables) <- font.header
            
            visible(expand.variables) <- FALSE
            enabled(frame.variables) <- FALSE
            
            btn.finish <- gbutton("Finish")
            enabled(btn.finish) <- FALSE

            btn.back <- gbutton("Cancel Map Change")
            visible(btn.back) <- class(mapData) != "NULL"

            addHandlerClicked(btn.back, function(h, ...) {
                initiateModule()
            })

            tbl.buttons <- glayout()
            tbl.buttons[1, 1, expand = TRUE] <- btn.back
            tbl.buttons[1, 2, expand = TRUE] <- btn.finish
            
            ## Add all frames to window
            add(gv.match, frame.import, expand = TRUE)
            add(gv.match, frame.variables, expand = TRUE)
            add(gv.match, tbl.buttons, fill = "x")
            
                                        # Map Source Box
            ## Function definitions
            
### Read descriptions from ~/iNZightVIT/shapefiles/metadata
            read.mapmetadata <- function() {
                metadata <- scan("h:/Documents/iNZightVIT/shapefiles/metadata",
                                 what = rep("character", 3), fill = TRUE,
                                 comment.char = ";", sep = "\t",
                                 fileEncoding = 'UTF-8')
                metadata <- matrix(metadata, ncol = 3, byrow = TRUE)
                colnames(metadata) <- c("filepath", "tidy_filename", "description")
                metadata
            }
            
### Change filesystem directory names to more readable names (i.e. nzl
### to New Zealand)
### --- UNFINISHED ---
            decodeMapDir <- function(mapdir.mat) {
                mapdir.mat <- mapdir.contents
                ## Replace filenames
                
                have.tidy <- !is.na(mapdir.mat$tidy_filename)
                
                mapdir.mat$tidy_filepath[have.tidy] <- sub("^.*/([-_\\.A-z0-9]+\\.(shp|rds))$",
                                                           mapdir.mat$tidy_filename[have.tidy],
                                                           mapdir.mat$x[have.tidy])
                
                iso.matrix <- matrix(c("nzl", "New Zealand",
                                       "usa", "United States"),
                                     ncol = 2, byrow = TRUE)
                ## Replace ISO codes
                country.ind <- grepl("^countries/", dir.vect)
                country.iso <- sub("^countries/([A-z]+)/.*$", "\\1", dir.vect[country.ind])
                country.name <- iso.matrix[which(country.iso == iso.matrix[,1]), 2]
                dir.vect[country.ind] <- sub("^countries/([A-z]+)/",
                                             paste0("countries/", country.name, "/"),
                                             dir.vect[country.ind])
                
                ## Replace first directories with capitals
                sub("^([A-z])([A-z0-9]*)/", "\\U\\1\\E\\2/", dir.vect, perl = TRUE)
            }
            
### Helper function for gtree()
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
                
                data.frame(filename = filenames[unique.ind],
                           has.children = has.children[unique.ind])
            }
            
            ## Variable definitions
            stored.shapefiles <- list.files("H:/Documents/iNZightVIT/shapefiles/",
                                            recursive = TRUE,
                                            pattern = ".(shp|rds)$")
            
            metadata <- read.mapmetadata()
            
            mapdir.contents <- merge(stored.shapefiles, metadata,
                                     by.x = 1, by.y = 1, all.x = TRUE)
            
            ## Heading area
            lbl <- glabel("Map Source:")
            mapSource <- gradio(c("Use Inbuilt Map", "Import Shapefile"),
                                horizontal = TRUE)
            
            ## Inbuilt Map Data 
            tblInbuiltfile <- glayout()
            
            mapInbuiltBrowse <- gtree(offspring = offspring.files,
                                      offspring.data = mapdir.contents[, 1],
                                      chosen.col = "filename",
                                      offspring.col = "has.children")
            mapInbuiltBrowse$widget$`headers-visible` <- FALSE
            
            lbl.mapdesc <- gtext("Description: No description available.")
            font(lbl.mapdesc) <- list(weight = "bold", size = 10, family = "normal")
            
            tblInbuiltfile[1, 1, expand = TRUE, fill = "both"] <- mapInbuiltBrowse
            tblInbuiltfile[2, 1, expand = TRUE, fill = "both"] <- lbl.mapdesc
            
            enabled(lbl.mapdesc) <- FALSE
            
            ## User-imported Shapefile
            tblShapefile <- glayout()
            
            mapSourceBrowse <- gfilebrowse(text = "Open Shapefile...",
                                           type = "open",
                                           filter = list("All formats" = list(patterns = c("*.shp",
                                                                                           "*.json",
                                                                                           "*.geojson",
                                                                                           "*.rds")),
                                                         "Shapefile" = list(patterns = c("*.shp")),
                                                         "GeoJSON" = list(patterns = c("*.json",
                                                                                       "*.geojson"))))
            
            btn.import <- gbutton(text = "Import Map")
            
            tblShapefile[1, 1, expand = TRUE] <- mapSourceBrowse
            
            ## Add widgets to layout
            add(frame.import, group.import, expand = TRUE)
            add(group.import, expand.import, expand = TRUE)
            addSpace(expand.import, 15)
            add(expand.import, mapSource)
            addSpace(expand.import, 5)
            add(expand.import, tblShapefile, expand = TRUE)
            add(expand.import, tblInbuiltfile, expand = TRUE)
            addSpace(expand.import, 15)
            add(expand.import, btn.import)
            
            visible(tblInbuiltfile) <- TRUE
            visible(tblShapefile) <- FALSE
            
            ## Event handling
            
### Map source radio button
            addHandlerChanged(mapSource, function(h, ...) {
                v <- svalue(mapSource, index = TRUE)
                visible(tblShapefile) <- v == 2
                visible(tblInbuiltfile) <- v == 1
            })
            
### If user changes the map file, hide the variable merging section
### again to prevent dissonance between the two sections.
            addHandlerChanged(mapSourceBrowse, function(h, ...) {
                if(!staleMap) {
                    staleMap <<- TRUE
                    visible(expand.variables) <- FALSE
                    enabled(frame.variables) <- FALSE
                    enabled(btn.finish) <- FALSE
                }

                plot(c(0, 1), c(0, 1), ann = FALSE, bty = "n", type = "n", xaxt = "n", yaxt = "n")
                text(0.5, 0.5, "Preview unavailable for imported shapefiles")
                ## plot(iNZightMaps::retrieveMap(svalue(mapSourceBrowse))$geometry)
            })
            
### If user changes the map file, hide the variable merging section
### again to prevent dissonance between the two sections.
            addHandlerSelectionChanged(mapInbuiltBrowse, function(h, ...) {
                if(!staleMap) {
                    staleMap <<- TRUE
                    ## plot.new()
                    visible(expand.variables) <- FALSE
                    enabled(frame.variables) <- FALSE
                    enabled(btn.finish) <- FALSE
                }
            })
            
### Again, prevent dissonance between sections. Also insert the map
### description if it is present in the metadata
            addHandlerSelect(mapInbuiltBrowse, function(h, ...) {
                if(!staleMap) {
                    staleMap <<- TRUE
                    ## plot.new()
                    visible(expand.variables) <- FALSE
                    enabled(frame.variables) <- FALSE
                    visible(lbl.allmatched) <- FALSE
                    enabled(btn.finish) <- FALSE
                }
                
                mapFilename <<- paste(svalue(mapInbuiltBrowse), collapse = "/")
                chosen.desc <- mapdir.contents$description[which(mapdir.contents[, 1] == mapFilename)]
                
                if(length(chosen.desc) > 0 && !is.na(chosen.desc)) {
                    svalue(lbl.mapdesc) <- paste("Description:", chosen.desc)
                } else {
                    svalue(lbl.mapdesc) <- "Description: No description available." 
                }
                font(lbl.mapdesc) <- list(weight = "bold", size = 10, family = "normal")

                dev.hold()
                inbuilt.path <- paste(svalue(mapInbuiltBrowse), collapse = "/")
                map.filename <- paste0("H:/Documents/iNZightVIT/shapefiles/", inbuilt.path)
                plot(iNZightMaps::retrieveMap(map.filename)$geometry,
                     col = "#FFFFFF")
                dev.flush()
            })
            
### Import the map; update the relevant widgets in the variable
### merging section; hide the map selection section and unhide the
### variable merging section. Intialize the dropboxes with the pair of
### variables with the higest number of matches
            addHandlerClicked(btn.import, handler = function(h, ...) {
                ## Extract the true filename from inputs
                if(svalue(mapSource, index = TRUE) == 1) {
                    inbuilt.path <- paste(svalue(mapInbuiltBrowse), collapse = "/")
                    map.filename <- paste0("H:/Documents/iNZightVIT/shapefiles/", inbuilt.path)
                } else {
                    map.filename <- svalue(mapSourceBrowse)
                }
                
                ## Change which region has focus
                visible(expand.import) <- FALSE
                visible(expand.variables) <- TRUE
                
                ## Indicate to the user that the map is being loaded in case it is
                ## large enough to seem like it is hanging
                visible(lbl.loading) <- TRUE
                plot(c(0, 1), c(0, 1), ann = FALSE, bty = "n", type = "n", xaxt = "n", yaxt = "n")
                text(0.5, 0.5, "Please wait... Map is being imported")
                
                ## Import the map - either a shapefile or rds
                mapData <<- iNZightMaps::retrieveMap(map.filename)
                map.vars <- as.data.frame(mapData)[, !(colnames(mapData) %in% "geometry")]

                ## Only take variables in the shapefile that are unique to one
                ## region in the map file
                combobox.mapvars[] <- colnames(map.vars[, !(apply(map.vars, 2, anyDuplicated, incomparables = c(NA, "")))])
                staleMap <<- FALSE
                
                ## Find the pair of variables with the highest number of matches
                best.vars <- iNZightMaps::findBestMatch(activeData, map.vars)
                best.data.var <- best.vars[1]
                best.map.var <-  best.vars[2]
                
                ## Finished loading, so replace loading label with a blank label
                ## (prevents widgets moving around too much)
                visible(lbl.loading) <- FALSE
                visible(lbl.blank) <- TRUE
                
                ## Enable user interaction with the variable merging section now
                ## that nearly everything is done
                enabled(frame.variables) <- TRUE
                enabled(btn.finish) <- TRUE
                
                ## Initialize with the best variables from above
                svalue(combobox.datavars) <- best.data.var
                svalue(combobox.mapvars) <- best.map.var
            })
            
################################################################################
                                        # Variable Merging
            
            ## Function definitions
### Helper function that is called each time either combobox is
### changed. Updates the gtable of nonmatches.
            matchplot.colours <- c("#d95f02", "#1b9e77", "#7570b3")
            
            cb.change <- function(h, ...) {
                enabled(table.nonmatched) <- FALSE
                
                data.var <- svalue(combobox.datavars)
                map.var <- svalue(combobox.mapvars)
                
                match.list <- iNZightMaps::matchVariables(activeData[, data.var],
                                                          as.data.frame(mapData)[, map.var])
                
                table.nonmatched[] <- match.list$data.vect
                visible(table.nonmatched) <- !(match.list$data.matched)
                
                enabled(table.nonmatched) <- TRUE
                
                svalue(lbl.unmatchedcount) <- paste("Unmatched Count:", sum(!match.list$data.matched))
                svalue(lbl.matchedcount) <- paste("Matched Count:", sum(match.list$data.matched))

                dev.hold()
                plot(mapData$geometry, col = matchplot.colours[match.list$map.matched + 1])
                legend("topleft", legend = c("Data present for region",
                                                "Data missing for region"),
                       fill = matchplot.colours[2:1])
                dev.flush()
                
                if(any(visible(table.nonmatched))) {
                    visible(lbl.allmatched) <- FALSE
                    visible(lbl.blank) <- TRUE
                } else {
                    visible(lbl.allmatched) <- TRUE
                    visible(lbl.blank) <- FALSE
                }

                visible(sep.multipleobs) <- match.list$multiple.obs
                visible(lbl.multipleobs) <- match.list$multiple.obs
                visible(tbl.sequencevar) <- match.list$multiple.obs
                has.multipleobs <<- match.list$multiple.obs
            }
            
            ## Widget definitions
            
            tbl.variables <- glayout()
            
            combobox.mapvars <- gcombobox(items = c(""))
            combobox.datavars <- gcombobox(items = colnames(activeData))
            
            tbl.variables[1, 1] <- glabel("Data Variable: ")
            tbl.variables[1, 2, expand = TRUE] <- combobox.datavars
            
            tbl.variables[1, 4] <- glabel("Map Variable: ")
            tbl.variables[1, 5, expand = TRUE] <- combobox.mapvars
            
            lbl.nonmatchedtitle <- glabel("Unmatched Data")
            lbl.nonmatchedsubtitle <- glabel("Observations in the dataset with no corresponding region in the map file")
            font(lbl.nonmatchedtitle) <- list(weight = "bold", family = "normal", size = 10)
            table.nonmatched <- gtable(nomatch.df)
            
            lbl.matchedcount <- glabel("Matched Count: 0")
            lbl.unmatchedcount <- glabel("Unmatched Count: 0")
            tbl.matchcounts <- glayout()
            tbl.matchcounts[1, 1, expand = TRUE] <- lbl.unmatchedcount
            tbl.matchcounts[1, 2, expand = TRUE] <- lbl.matchedcount
            
            lbl.allmatched <- glabel("All rows of data matched to a region!")
            lbl.loading <- glabel("Loading map... Please wait...")
            lbl.blank <- glabel("")

            sep.multipleobs <- gseparator()
            lbl.multipleobs <- glabel("Multiple observations for each region were found!")
            lbl.sequencevar <- glabel("Select sequence variable:")
            combobox.sequencevar <- gcombobox(items = colnames(activeData))
            tbl.sequencevar <- glayout()
            tbl.sequencevar[1, 1, expand = TRUE] <- lbl.sequencevar
            tbl.sequencevar[1, 2, expand = TRUE] <- combobox.sequencevar
            
            ## Add to frame
            add(frame.variables, group.variables, expand = TRUE)
            add(group.variables, expand.variables, expand = TRUE)
            addSpace(expand.variables, 15)
            add(expand.variables, tbl.variables)
            addSpace(expand.variables, 15)
            add(expand.variables, lbl.allmatched)
            add(expand.variables, lbl.loading)
            add(expand.variables, lbl.blank)
            add(expand.variables, lbl.nonmatchedtitle)
            add(expand.variables, lbl.nonmatchedsubtitle)
            addSpace(expand.variables, 5)
            add(expand.variables, table.nonmatched, expand = TRUE)
            add(expand.variables, tbl.matchcounts)
            addSpace(expand.variables, 5)
            add(expand.variables, sep.multipleobs)
            add(expand.variables, lbl.multipleobs)
            add(expand.variables, tbl.sequencevar)
            
            visible(table.nonmatched) <- FALSE
            
            visible(lbl.allmatched) <- FALSE
            visible(lbl.loading) <- FALSE
            visible(lbl.blank) <- FALSE

            visible(sep.multipleobs) <- FALSE
            visible(lbl.multipleobs) <- FALSE
            visible(tbl.sequencevar) <- FALSE
            
            ## Event handlers
            addHandlerChanged(combobox.mapvars, handler = cb.change)
            addHandlerChanged(combobox.datavars, handler = cb.change)
            ### Right click menu for nonmatched table to update it.
            
################################################################################
                                        # Finish Importing
            
            addHandlerClicked(btn.finish, function(h, ...) {
                ## Join data to map
                data.var <- svalue(combobox.datavars)
                map.var <- svalue(combobox.mapvars)

                if (is.null(mapType)) {
                    mapType <<- "region"
                }

                ## TODO: Simplification
                combinedData <<- iNZightMaps::iNZightMapPlot(data = activeData,
                                                             map = mapData, 
                                                             type = "region",
                                                             by.data = data.var,
                                                             by.map = map.var,
                                                             simplification.level = 0.01,
                                                             multiple.obs = has.multipleobs,
                                                             sequence.var = svalue(combobox.sequencevar))

                ## If the given file has a name given in the metadata,
                ## use that. Otherwise use the filename.
                chosen.name <- mapdir.contents$tidy_filename[which(mapdir.contents[, 1] == mapFilename)]
                if(length(chosen.name) > 0 && !is.na(chosen.name)) {
                    mapName <<- as.character(chosen.name)
                } else {
                    mapName <<- as.character(sub("^.*/([-\\._A-z0-9]+)\\.[A-z0-9]*$", "\\1", mapFilename))
                }
                
                
                ## TODO: Do this a better way
                combinedData$type <<- mapType
                
                ## dispose(w.match)
                initiateModule()
            })

                                        # Bottom group of buttons
            

            btmGrp <- ggroup(container = gv.match)

            helpButton <- gbutton("Help", expand = TRUE, fill = TRUE,
                                  cont = btmGrp,
                                  handler = function(h, ...) {
                                      browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/add_ons/?topic=maps")
                                  })
            homeButton <- gbutton("Home", expand = TRUE, fill = TRUE,
                                  cont = btmGrp,
                                  handler = function(h, ...) {
                                      ## delete the module window
                                      delete(GUI$leftMain)
                                      delete(GUI$leftMain, GUI$leftMain$children[[2]])
                                      ## display the default view (data, variable, etc.)
                                      GUI$plotToolbar$restore()
                                      visible(GUI$gp1) <<- TRUE
                                  })
            GUI$plotToolbar$update(NULL, refresh = "updatePlot")
        },
        ## Create the map object based on the options given in the importation dialog box
        createMapObject = function() {},
        ## After the map object has been constructed, initialize the interface for the Maps module (sidebar)
        initiateModule = function() {
            updateOptions = function() {
                ## Plot Options
                plotTitle <<- svalue(edit.plottitle)
                plotAxes <<- svalue(checkbox.axislabels)
                plotXLab <<- svalue(edit.xaxis)
                plotYLab <<- svalue(edit.yaxis)
                plotDatumLines <<- svalue(checkbox.datum)            
                plotProjection <<- proj.df[svalue(combobox.mapproj, index = TRUE), "PROJ4"]
                plotTheme <<- svalue(combobox.palette)

                ## Variable Options
                mapVars <<- svalue(table.vars)
                mapSizeVar <<- svalue(combobox.sizeselect)

                if(length(mapVars) == 0) {
                    mapVars <<- NULL
                }

                if(length(mapSizeVar) == 0 || mapSizeVar == "") {
                    mapSizeVar <<- NULL
                }

                updatePlot()
            }
            
            GUI$initializeModuleWindow(.self)
            
            
            mainGrp <<- gvbox(spacing = 5, container = GUI$moduleWindow, expand = TRUE)
            visible(mainGrp) <<- FALSE

            mainGrp$set_borderwidth(5)
            
            addSpace(mainGrp, 10)
            
            lbl.inzightmaps <- glabel("iNZight Maps")
            font(lbl.inzightmaps) <- list(weight = "bold",
                                          family = "normal",
                                          size   = 12)
            add(mainGrp, lbl.inzightmaps, anchor = c(0, 0))
            addSpace(mainGrp, 10)
            
            frame.mapoptions <- gframe(horizontal = FALSE)
            group.mapoptions <- ggroup(spacing = 5)
            group.mapoptions$set_borderwidth(10)
            expand.mapoptions <- gexpandgroup(text = "Advanced Map Options", horizontal = FALSE)
            font(expand.mapoptions) <- list(weight = "bold", family = "normal", size = 10)
            
            add(mainGrp, frame.mapoptions)
            add(frame.mapoptions, group.mapoptions, expand = TRUE)
            add(group.mapoptions, expand.mapoptions, expand = TRUE)
            
            frame.plotoptions <- gframe(horizontal = FALSE)
            group.plotoptions <- ggroup(spacing = 5)
            group.plotoptions$set_borderwidth(10)
            expand.plotoptions <- gexpandgroup(text = "Advanced Plot Options", horizontal = FALSE)
            font(expand.plotoptions) <- list(weight = "bold", family = "normal", size = 10)
            
            add(mainGrp, frame.plotoptions)
            add(frame.plotoptions, group.plotoptions, expand = TRUE)
            add(group.plotoptions, expand.plotoptions, expand = TRUE)
            
            frame.main <- gframe(horizontal = FALSE)
            group.main <- ggroup(spacing = 5, horizontal = FALSE)
            group.main$set_borderwidth(10)
            
            add(mainGrp, group.main, expand = TRUE, fill = TRUE)
            
            visible(expand.mapoptions) <- FALSE
            visible(expand.plotoptions) <- FALSE
            
            ## Map Options
            
            tbl.mapoptions <- glayout()
            
            lbl.currentmap <- glabel("Current Map:")
            lbl.mapname <- glabel(mapName)
            btn.changemap <- gbutton("Change")
            
            tbl.mapoptions[1, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.currentmap
            tbl.mapoptions[1, 2:3, expand = TRUE, anchor = c(-1, 0)] <- lbl.mapname
            tbl.mapoptions[1, 4] <- btn.changemap

#####
            proj.df <- read.csv("H:/echome/inzightwork/package/iNZightMaps/projections.csv",
                                stringsAsFactors = FALSE)
            
            lbl.mapproj <- glabel("Projection:")
            combobox.mapproj <- gcombobox(proj.df$Name)

            if(!is.null(plotProjection)) {
                svalue(combobox.mapproj, index = TRUE) <- which(proj.df$PROJ4 == plotProjection) 
            }
                        
            tbl.mapoptions[2, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.mapproj
            tbl.mapoptions[2, 2] <- gcombobox(c("World", "Continent", "Country"))
            tbl.mapoptions[2, 3:4, expand = TRUE] <- combobox.mapproj
            
            add(expand.mapoptions, tbl.mapoptions, expand = TRUE, fill = TRUE)
            
            ## Plot Options
            
            tbl.plotoptions <- glayout()
            
            lbl.plottitle <- glabel("Plot Title:")
            edit.plottitle <- gedit(plotTitle)
            checkbox.axislabels <- gcheckbox(text = "Axis Labels", checked = plotAxes)
            lbl.xaxis <- glabel("x-axis Label:")
            lbl.yaxis <- glabel("y-axis Label:")
            edit.xaxis <- gedit(plotXLab)
            edit.yaxis <- gedit(plotYLab)
            checkbox.datum <- gcheckbox("Grid Lines", checked = plotDatumLines)
            
            lbl.palette <- glabel("Map Theme:")
            combobox.palette <- gcombobox(c("Default", "Dark"))
            svalue(combobox.palette) <- plotTheme

            addHandlerChanged(combobox.palette, function(h, ...) {
                updateOptions()
            })
            
            
            tbl.xaxisedit <- glayout()
            tbl.xaxisedit[1, 1, expand = TRUE] <- edit.xaxis
            
            tbl.yaxisedit <- glayout()
            tbl.yaxisedit[1, 1, expand = TRUE] <- edit.yaxis
            
            tbl.plotoptions[1, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.plottitle
            tbl.plotoptions[1, 2:4, expand = TRUE] <- edit.plottitle
            
            tbl.plotoptions[2, 1, expand = TRUE,  anchor = c(1, 0)] <- lbl.palette
            tbl.plotoptions[2, 2:4, expand = TRUE] <- combobox.palette
            
            tbl.plotoptions[3, 2:4, expand = TRUE, anchor = c(-1, 0)] <- checkbox.datum
            
            tbl.plotoptions[4, 2:4, expand = TRUE] <- checkbox.axislabels
            tbl.plotoptions[5, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.xaxis
            tbl.plotoptions[5, 2:4, expand = TRUE] <- tbl.xaxisedit
            tbl.plotoptions[6, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.yaxis
            tbl.plotoptions[6, 2:4, expand = TRUE] <- tbl.yaxisedit
            
            add(expand.plotoptions, tbl.plotoptions, expand = TRUE, fill = TRUE)

            visible(lbl.xaxis) <- plotAxes
            visible(tbl.xaxisedit) <- plotAxes
            visible(lbl.yaxis) <- plotAxes
            visible(tbl.yaxisedit) <- plotAxes

            addHandlerChanged(checkbox.axislabels, function (h, ...) {
                if(svalue(checkbox.axislabels)) {
                    visible(lbl.xaxis) <- TRUE
                    visible(tbl.xaxisedit) <- TRUE
                    visible(lbl.yaxis) <- TRUE
                    visible(tbl.yaxisedit) <- TRUE
                } else {
                    visible(lbl.xaxis) <- FALSE
                    visible(tbl.xaxisedit) <- FALSE
                    visible(lbl.yaxis) <- FALSE
                    visible(tbl.yaxisedit) <- FALSE
                }
                updateOptions()
            })

            addHandlerChanged(edit.plottitle, function(h, ...) {
                updateOptions()
            })

            addHandlerChanged(edit.xaxis, function(h, ...) {
                updateOptions()
            })

            addHandlerChanged(edit.yaxis, function(h, ...) {
                updateOptions()
            })
            
            addHandlerChanged(checkbox.datum, function(h, ...) {
                if(!svalue(checkbox.datum) && svalue(checkbox.axislabels)) {
                    svalue(checkbox.axislabels) <- FALSE
                } else {
                    updateOptions()
                }
            })

            addHandlerChanged(combobox.mapproj, function(h, ...) {
                updateOptions()
            })

            addHandlerClicked(btn.changemap, function(h, ...) {
                visible(mainGrp) <<- FALSE
                importDialog()
            })
            
            ## Variable selection
            
            lbl.maintitle <- glabel("Select Variable/s to Display")
            lbl.mainsubtitle <- glabel("(Use Ctrl+Click to select multiple variables)")
            font(lbl.maintitle) <- list(weight = "bold", family = "normal", size = 10)
            
            tbl.main <- glayout()
            
            var.vect <- sort(iNZightMapVars(combinedData))
            table.vars <- gtable(var.vect, multiple = TRUE)

            if (!is.null(mapVars)) {
                svalue(table.vars) <- mapVars
            }

            lbl.maptype <- glabel("Plot as:")
            radio.maptype <- gradio(c("Regions", "Centroids"), horizontal = TRUE,
                                    selected = (mapType == "point") + 1)
            
            
            lbl.sizeselect <- glabel("Size by:")
            combobox.sizeselect <- gcombobox(c("", var.vect))

            if (!is.null(mapSizeVar)) {
                svalue(combobox.sizeselect) <- mapSizeVar
            }

            if (!combinedData$multiple.obs) {
                tbl.main[2, 1,   expand = TRUE, anchor = c(1, 0)] <- lbl.maptype
                tbl.main[2, 2,   expand = TRUE, anchor = c(-1, 0), fill = "x"] <- radio.maptype
                
                tbl.main[3, 1,   expand = TRUE, anchor = c(1, 0)] <- lbl.sizeselect
                tbl.main[3, 2,   expand = TRUE] <- combobox.sizeselect
            } else {
                separator.timevariable <- gseparator()
                lbl.timevariable <- glabel("Dataset has multiple observations for regions:")

                radio.multipleobs <- gradio(c("Single Value", "All Values", "Aggregate"), horizontal = TRUE)

                if (isTRUE(is.null(multipleObsOption))) {
                    multipleObsOption <<- "singleval"
                }
                
                unique.singlevals <- unique(as.data.frame(combinedData[["region.data"]])[, combinedData$sequence.var])
                combobox.singleval <- gcombobox(unique.singlevals[!is.na(unique.singlevals)])
                svalue(combobox.singleval, index = TRUE) <- length(combobox.singleval)

                radio.allvalues <- gradio(c("Sparklines", "Grid (TODO)"), horizontal = TRUE)

                combobox.aggregate <- gcombobox(c("Mean", "Median"))

                tbl.main[2, 1:3] <- lbl.timevariable
                tbl.main[3, 1:3] <- radio.multipleobs
                tbl.main[4, 2:3,   expand = TRUE, anchor = c(-1, 0), fill = "x"] <- combobox.singleval
                tbl.main[4, 2:3,   expand = TRUE, anchor = c(-1, 0), fill = "x"] <- combobox.aggregate
                tbl.main[5, 1:3] <- separator.timevariable
                tbl.main[6, 2,   expand = TRUE, anchor = c(-1, 0), fill = "x"] <- radio.allvalues
                tbl.main[6, 1,   expand = TRUE, anchor = c(1, 0)] <- lbl.maptype
                tbl.main[6, 2,   expand = TRUE, anchor = c(-1, 0), fill = "x"] <- radio.maptype
                
                tbl.main[7, 1,   expand = TRUE, anchor = c(1, 0)] <- lbl.sizeselect
                tbl.main[7, 2,   expand = TRUE] <- combobox.sizeselect

                visible(radio.allvalues) <- FALSE
                visible(combobox.aggregate) <- FALSE

                addHandlerChanged(radio.multipleobs, function(h, ...) {
                    radio.choice <- svalue(radio.multipleobs, index = TRUE)

                    visible(combobox.singleval) <- radio.choice == 1
                    visible(combobox.aggregate) <- radio.choice == 3

                    if (isTRUE(!is.null(mapVars))) {
                        visible(radio.allvalues) <- radio.choice == 2
                        visible(radio.maptype) <- radio.choice != 2
                        visible(lbl.sizeselect) <- radio.choice != 2 && mapType == "point"
                        visible(combobox.sizeselect) <- radio.choice != 2 && mapType == "point"
                    } else {
                        visible(radio.allvalues) <- FALSE
                        visible(radio.maptype) <- FALSE
                        visible(lbl.sizeselect) <- FALSE
                        visible(combobox.sizeselect) <- FALSE
                        visible(lbl.maptype) <- FALSE
                    }

                    if (radio.choice == 1) {
                        multipleObsOption <<- "singleval"
                        combinedData$type <<- mapType
                        combinedData <<- iNZightMapAggregation(combinedData,
                                                               "singlevalue",
                                                               single.value = svalue(combobox.singleval))
                    } else if (radio.choice == 2) {
                        multipleObsOption <<- "allvalues"
                        combinedData$type <<- "sparklines"
                        if (isTRUE(!is.null(mapVars))) {
                            vars.to.keep <- sapply(as.data.frame(combinedData$region.data)[, mapVars, drop = FALSE], is.numeric)
                            print(vars.to.keep)
                            print(mapVars[vars.to.keep])
                            if (sum(vars.to.keep) > 0) {
                                svalue(table.vars) <- mapVars[vars.to.keep]
                            } else {
                                svalue(table.vars, index = TRUE) <- 0
                            }
                        }
                    } else if (radio.choice == 3) {
                        multipleObsOption <<- "aggregate"
                        combinedData$type <<- mapType
                        combinedData <<- iNZightMapAggregation(combinedData,
                                                               tolower(svalue(combobox.aggregate)))
                    }
                    updateOptions()
                })

                addHandlerChanged(combobox.singleval, function(h, ...) {
                    combinedData <<- iNZightMapAggregation(combinedData, "singlevalue",
                                                           single.value = svalue(combobox.singleval))
                    updatePlot()
                })

                addHandlerChanged(radio.allvalues, function(h, ...) {
                    combinedData$type <<- "sparklines"
                })

                addHandlerChanged(combobox.aggregate, function(h, ...) {
                    combinedData <<- iNZightMapAggregation(combinedData,
                                                           tolower(svalue(combobox.aggregate)))
                    updatePlot()
                })
                
                
            }

            tbl.main[1, 1:3, expand = TRUE, fill = "both"] <- table.vars

             
            visible(lbl.maptype) <- !is.null(mapVars)
            visible(radio.maptype) <- !is.null(mapVars)
            visible(lbl.sizeselect) <- mapType == "point"
            visible(combobox.sizeselect) <- mapType == "point"
            
            add(group.main, lbl.maintitle)
            add(group.main, lbl.mainsubtitle)
            add(group.main, tbl.main, expand = TRUE, fill = TRUE)

            addHandlerSelectionChanged(table.vars, function(h, ...) {
                if (isTRUE(length(svalue(table.vars)) > 0)) {
                    visible(lbl.maptype) <- TRUE
                    print(multipleObsOption)
                    if (isTRUE(multipleObsOption != "allvalues")) {
                        visible(radio.maptype) <- TRUE
                        visible(radio.allvalues) <- FALSE
                    } else {
                        visible(radio.maptype) <- FALSE
                        visible(radio.allvalues) <- TRUE
                    }
                } else {
                    visible(radio.allvalues) <- FALSE
                    visible(radio.maptype) <- FALSE
                    visible(lbl.sizeselect) <- FALSE
                    visible(combobox.sizeselect) <- FALSE
                    visible(lbl.maptype) <- FALSE
                }

                if (combinedData$type == "sparklines") {
                    vars.numeric <- combinedData$var.types[svalue(table.vars)] %in% c("numeric", "integer")
                    print(vars.numeric)
                    if (isTRUE(any(!vars.numeric))) {
                        galert("Categorical variables cannot be used with sparklines")
                        if (sum(vars.numeric) > 0) {
                            svalue(table.vars) <- svalue(table.vars)[vars.numeric]
                        } else {
                            svalue(table.vars, index = TRUE) <- 0
                        }
                    }
                }

                ## if(length(svalue(table.vars)) > 1) {
                   ## svalue(edit.plottitle) <- ""
                ## } else {
                    ## svalue(edit.plottitle) <- svalue(table.vars)
                ## }
                
                updateOptions()
            })
            
            addHandlerChanged(radio.maptype, function(h, ...) {
                if (svalue(radio.maptype, index = TRUE) == 1) {
                    visible(lbl.sizeselect) <- FALSE
                    visible(combobox.sizeselect) <- FALSE
                    combinedData$type <<- "region"
                    mapType <<- "region"
                } else if (svalue(radio.maptype, index = TRUE) == 2) {
                    visible(lbl.sizeselect) <- TRUE
                    visible(combobox.sizeselect) <- TRUE
                    combinedData$type <<- "point"
                    mapType <<- "point"
                }
                updateOptions()
            })

            addHandlerChanged(combobox.sizeselect, function(h, ...) {
                updateOptions()
            })
            
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
            ## GUI$plotToolbar$update(NULL, refresh = "updatePlot")

            test.btn <- gbutton("interactivity", cont = mainGrp)
            addHandlerClicked(test.btn, function(h, ...) {
                library(grid)
                library(gridSVG)
                grid.force()
                regions <- grid::grid.grep("pathgrob", grep = TRUE, global = TRUE)

                for (i in 1:length(regions)) {
                    curr.region.tooltip <- paste0(combinedData$data$NAME[i],
                                                  " (", mapVars, ": ", combinedData$data[i, mapVars], ")")
                    curr.region.tooltip <- gsub("'", "\\\\'", curr.region.tooltip)
                    curr.region.tooltip <- gsub('"', '\\\\"', curr.region.tooltip)
                    grid.garnish(regions[[i]],
                                 onmouseover = paste("showTooltip(evt, '", curr.region.tooltip, "')"),
                                 onmouseout = "hideTooltip()")
                }
                
                grid.script(filename = "tooltip.js", inline = TRUE)
                grid.export("testinzight.svg")
            })
            
            visible(mainGrp) <<- TRUE
            updateOptions()
        },
        ## Update and plot the map given
        updatePlot = function() {
            if(length(mapVars) > 1) {
                multiple.vars <- TRUE
            } else {
                multiple.vars <- FALSE
            }

            if (isTRUE(combinedData$multiple.obs && multipleObsOption != "allvalues")) {
                aggregation <- TRUE
            } else {
                aggregation <- FALSE
            }

            print(mapVars)
            
            print("updating plot...")
            print(sys.calls())
            grid::grid.rect(width = 0.25, height = 0.10,
                            gp = grid::gpar(fill = "#FFFFFF80", colour = "#FFFFFF80"))
            grid::grid.text("Please wait... Loading...")

            dev.hold()
            grid::grid.newpage()
            grid::grid.draw(plot(combinedData, main = plotTitle,
                 axis.labels = plotAxes, xlab = plotXLab, ylab = plotYLab,
                 datum.lines = plotDatumLines, projection = plotProjection,
                 multiple.vars = multiple.vars, colour.var = mapVars,
                 size.var = mapSizeVar, aggregate = aggregation,
                 theme = plotTheme))
            dev.flush()

        }
    ))

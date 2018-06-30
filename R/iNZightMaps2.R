##' iNZight Mapping Module 2
##'
##' Opens a UI for visualising geographical data
##'
##' @title iNZight Maps Module 2
##'
##' @author Daniel Barnett
##'
##' @export iNZightMap2Mod
##' @exportClass iNZightMap2Mod

iNZightMap2Mod <- setRefClass(
    "iNZightMap2Mod",

    fields = list(
        GUI = "ANY",
        mainGrp = "ANY",

        activeData = "data.frame",
        mapData = "ANY",
        combinedData = "ANY",
        staleMap = "logical",
        has.multipleobs = "logical",
        mapSequenceVar = "ANY",
        plotObject = "ANY",

        mapName = "character",
        mapType = "ANY",
        mapVars = "ANY",
        mapSizeVar = "ANY",
        mapRegionsPlot = "ANY",
        mapExcludedRegions = "ANY",
        plotMaxSeqInd = "ANY",
        plotCurrSeqInd = "ANY",

        plotTitle = "ANY",
        plotAxes = "logical",
        plotXLab = "ANY",
        plotYLab = "ANY",
        plotDatumLines = "ANY",
        plotProjection = "ANY",
        plotTheme = "ANY",
        plotPalette = "ANY",
        plotConstantAlpha = "ANY",
        plotConstantSize = "ANY",
        plotCurrentSeqVal = "ANY",
        plotSparklinesType = "ANY",
        timer = "ANY",
        plotPlay = "ANY",
        playdelay = "ANY",
        plotLabelVar = "ANY",
        plotScaleLimits = "ANY",
        plotAxisScale = "ANY",
        plotLabelScale = "ANY",

        multipleObsOption = "ANY",

        codeHistory = "ANY",
        shapefileDir = "ANY"
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

            if (packageVersion("iNZightMaps") < "2.0-0") {
                resp <- gconfirm("A later version of the Maps package is required. Do you want to install it now?",
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
            activeData <<- as.data.frame(GUI$getActiveData())

            mapName <<- ""
            mapType <<- NULL
            mapVars <<- NULL
            mapSizeVar <<- NULL
            mapSequenceVar <<- NULL
            mapRegionsPlot <<- NULL
            mapExcludedRegions <<- TRUE

            plotTitle <<- ""
            plotAxes <<- FALSE
            plotXLab <<- "Longitude"
            plotYLab <<- "Latitude"
            plotDatumLines <<- FALSE
            plotProjection <<- NULL
            plotTheme <<- FALSE
            plotPalette <<- "Viridis"
            plotConstantAlpha <<- 1.0
            plotConstantSize <<- 1.0
            plotCurrentSeqVal <<- NULL
            plotSparklinesType <<- "Absolute"
            timer <<- NULL
            plotPlay <<- FALSE
            playdelay <<- 0.1
            plotLabelVar <<- NULL
            plotAxisScale <<- 1
            plotLabelScale <<- 4

            multipleObsOption <<- NULL
            OS <- if (.Platform$OS == "windows") "windows" else if (Sys.info()["sysname"] == "Darwin") "mac" else "linux"
            iNZightDir <- switch(OS,
                       "windows" = {
                           if (file.exists(file.path("~", "iNZightVIT"))) {
                               path <- file.path("~", "iNZightVIT")
                           } else {
                               path <- file.path("~")
                           }

                           path
                       },
                       "mac" = {
                           if (file.exists(file.path("~", "Documents", "iNZightVIT"))) {
                               path <- file.path("~", "Documents", "iNZightVIT")
                           } else {
                               path <- file.path("~")
                           }

                           path
                       },
                       "linux" = {
                           path <- file.path("~")

                           path
                       })
            shapefileDir <<- file.path(iNZightDir, "shapefiles")

            if (!dir.exists(shapefileDir) && !dir.create(shapefileDir))
                gmessage('Cannot create shape directory to load/save shapefiles.',
                         title='Cannot load shapefiles', icon = 'error')

            GUI$rhistory$add(c("SEP", "## New Maps Module"))

            importDialog()
        },

        # mapTypeDialog = function() {
        #     w <- gwindow("Define Geographical Variables",
        #                  width = 400,
        #                  height = 500,
        #                  parent = GUI$win,
        #                  visible = FALSE)
        # 
        #     gv <- gvbox(cont = w, expand = TRUE, fill = TRUE)
        #     gv$set_borderwidth(15)
        # 
        #     lbl <- glabel("Type of Map Data")
        #     font(lbl) <- list(weight = "bold", size = 12, family = "normal")
        #     radioMapType <- gradio(c("Coordinate (latitude, longitude)",
        #                              "Regions (country, state, county, etc.)"))
        #     add(gv, lbl)
        #     add(gv, radioMapType)
        # 
        #     coord.or.region <- any(grepl("(latitude|longitude)", colnames(activeData), TRUE))
        #     svalue(radioMapType, index = TRUE) <- if(coord.or.region) 1 else 2
        # 
        #     addSpace(gv, 10)
        # 
        #     btnFinish <- gbutton("OK")
        #     add(gv, btnFinish)
        # 
        #     addHandlerClicked(btnFinish, function(h, ...) {
        #         if(svalue(radioMapType, index = TRUE) == 1) {
        #             print("Coordinate")
        #         } else {
        #             dispose(w)
        #             importDialog()
        #         }
        #     })
        # 
        #     visible(w) <- TRUE
        # },
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

            btn.finish <- gbutton(action = gaction("Use Map", icon = "apply"))
            enabled(btn.finish) <- FALSE
            font(btn.finish) <- list(weight = "bold")

            btn.back <- gbutton("Cancel Map Change")
            visible(btn.back) <- class(mapData) != "uninitializedField"

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
            stored.shapefiles <- list.files(shapefileDir,
                                            recursive = TRUE,
                                            pattern = ".(shp|rds)$")

            if (length(stored.shapefiles) == 0) {
                shapefileDL <- gconfirm("Download shapefiles?")

                if (shapefileDL) {
                    tryCatch(iNZightMaps::download.shapefiles("https://www.stat.auckland.ac.nz/~wild/data/shapefiles/",
                                                 shapefileDir, shapefileDir),
                             error = function(e) gmessage(paste("Shapefile download failed:", e, sep = "\n"))
                             )
                    stored.shapefiles <- list.files(shapefileDir,
                                                    recursive = TRUE,
                                                    pattern = ".(shp|rds)$")

                }
            }

            metadata <- tryCatch(iNZightMaps::read.mapmetadata(shapefileDir),
                                 error = function(e) {
                                     gmessage("Could not download metadata file")
                                     metadata <- c(NA, NA, NA)
                                     metadata <- matrix(metadata, ncol = 3, byrow = TRUE)
                                     colnames(metadata) <- c("filepath", "tidy_filename", "description")
                                     metadata
                                 })

            mapdir.contents <- merge(stored.shapefiles, metadata,
                                     by.x = 1, by.y = 1, all.x = TRUE)

            mapdir.contents <- iNZightMaps::decodeMapDir(mapdir.contents)
            mapdir.contents <- vapply(mapdir.contents, as.character, character(nrow(mapdir.contents)))

            ## Heading area
            lbl <- glabel("Map Source:")
            mapSource <- gradio(c("Use Inbuilt Map", "Import Shapefile"),
                                horizontal = TRUE)

            ## Inbuilt Map Data
            tblInbuiltfile <- glayout()


            mapInbuiltBrowse <- gtree(offspring = offspring.files,
                                      offspring.data = mapdir.contents[, "tidy_filepath"],
                                      chosen.col = "filename",
                                      offspring.col = "has.children")
            mapInbuiltBrowse$widget$`headers-visible` <- FALSE
            tooltip(mapInbuiltBrowse) <- "Select an inbuilt map using the +/- icons. Double click to preview the map"

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
                    visible(expand.variables) <- FALSE
                    enabled(frame.variables) <- FALSE
                    visible(lbl.allmatched) <- FALSE
                    enabled(btn.finish) <- FALSE
                }

                chosen.filepath <- paste(svalue(mapInbuiltBrowse), collapse = .Platform$file.sep)
                chosen.map <- which(mapdir.contents[, "tidy_filepath"] == chosen.filepath)
                chosen.desc <- mapdir.contents[chosen.map, "description"]

                if(length(chosen.desc) > 0 && !is.na(chosen.desc)) {
                    svalue(lbl.mapdesc) <- paste("Description:", chosen.desc)
                } else {
                    svalue(lbl.mapdesc) <- "Description: No description available."
                }

                font(lbl.mapdesc) <- list(weight = "bold", size = 10, family = "normal")

                inbuilt.path <- mapdir.contents[chosen.map, "x"]
                if (isTRUE(length(inbuilt.path) > 0 && grepl("\\.[A-z0-9]+$", inbuilt.path))) {
                    ## mapFilename <<- inbuilt.path
                    map.filename <- file.path(shapefileDir, inbuilt.path)

                    dev.hold()
                    plot(iNZightMaps::retrieveMap(map.filename)$geometry,
                         col = "#FFFFFF")
                    dev.flush()
                }
            })

### Import the map; update the relevant widgets in the variable
### merging section; hide the map selection section and unhide the
### variable merging section. Intialize the dropboxes with the pair of
### variables with the higest number of matches
            addHandlerClicked(btn.import, handler = function(h, ...) {
                ## Extract the true filename from inputs
                if(svalue(mapSource, index = TRUE) == 1) {
                    chosen.filepath <- paste(svalue(mapInbuiltBrowse), collapse = .Platform$file.sep)
                    chosen.map <- which(mapdir.contents[, "tidy_filepath"] == chosen.filepath)
                    inbuilt.path <- mapdir.contents[chosen.map, "x"]
                    map.filename <- file.path(shapefileDir, inbuilt.path)

                    chosen.name <- mapdir.contents[which(mapdir.contents[, "x"] == inbuilt.path), "tidy_filename"]
                    if(length(chosen.name) > 0 && !is.na(chosen.name)) {
                        mapName <<- as.character(chosen.name)
                    } else {
                        mapName <<- as.character(sub("^.*/([-\\._A-z0-9]+)\\.[A-z0-9]*$", "\\1", map.filename))
                    }
                } else {
                    map.filename <- svalue(mapSourceBrowse)
                    mapName <<- as.character(sub("^.*[/\\\\]([-\\._A-z0-9]+)\\.[A-z0-9]*$", "\\1", map.filename))
                }

                ## If the given file has a name given in the metadata,
                ## use that. Otherwise use the filename.

                ## mapFilename <<- map.filename

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
                map.vars <- as.data.frame(mapData)[, !(colnames(mapData) %in% "geometry"), drop = FALSE]

                ## Only take variables in the shapefile that are unique to one
                ## region in the map file
                combobox.mapvars[] <- colnames(map.vars[, !(apply(map.vars, 2, anyDuplicated, incomparables = c(NA, ""))), drop = FALSE])
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
                ## if (match.list$multiple.obs) {
                    ## cents <- sf::st_coordinates(sf::st_centroid(mapData$geometry))
                         ## text(cents[,"X"], cents[,"Y"], labels = match.list$n.obs.per.region)
                ## }
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
            lbl.datavars <- glabel("Data Variable: ")
            lbl.mapvars <- glabel("Map Variable: ")
            combobox.mapvars <- gcombobox(items = c(""))
            combobox.datavars <- gcombobox(items = colnames(activeData))

            tbl.variables[1, 1] <- lbl.datavars
            tbl.variables[1, 2, expand = TRUE] <- combobox.datavars

            tbl.variables[1, 4] <- lbl.mapvars
            tbl.variables[1, 5, expand = TRUE] <- combobox.mapvars

            tooltip(lbl.datavars) <- "Variable in the dataset used to match each observation to a region in the map"
            tooltip(lbl.mapvars) <- "Variable in the map used to match each region in the map to an observation"

            tooltip(combobox.datavars) <- tooltip(lbl.datavars)
            tooltip(combobox.mapvars) <- tooltip(lbl.mapvars)


            lbl.nonmatchedtitle <- glabel("Unmatched Data")
            lbl.nonmatchedsubtitle <- glabel("Observations in the dataset with no corresponding region in the map")
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
            font(lbl.multipleobs) <- list(weight = "bold", size = 10, family = "normal")
            ## font(lbl.sequencevar) <- list(weight = "bold", size = 10, family = "normal")
            combobox.sequencevar <- gcombobox(items = colnames(activeData))

            if (!isTRUE(is.null(mapSequenceVar))) {
                svalue(combobox.sequencevar) <- mapSequenceVar
            }

            timevar <- grepl("(year|date)", colnames(activeData), ignore.case = TRUE)
            if (any(timevar)) {
                svalue(combobox.sequencevar) <- colnames(activeData)[timevar][1]
            }

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

                if (has.multipleobs) {
                    sequence.var <- svalue(combobox.sequencevar)
                } else {
                    sequence.var <- NULL
                }

                ## TODO: Simplification
                combinedData <<- iNZightMaps::iNZightMapPlot(data = activeData,
                                                             map = mapData,
                                                             type = "region",
                                                             by.data = data.var,
                                                             by.map = map.var,
                                                             simplification.level = 0.01,
                                                             multiple.obs = has.multipleobs,
                                                             sequence.var = sequence.var)

                mapSequenceVar <<- svalue(combobox.sequencevar)



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

        updatePlot = function() {
            gdkWindowSetCursor(getToolkitWidget(GUI$win)$getWindow(), gdkCursorNew("GDK_WATCH"))
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

            if (!plotPlay) {
                grid::grid.rect(width = 0.25, height = 0.10, y = 0.05,
                                gp = grid::gpar(fill = "#FFFFFF80", colour = "#FFFFFF80"))
                grid::grid.text("Please wait... Loading...", y = 0.05)
                axis.limits <- plotScaleLimits
            } else {
                axis.limits <- c(min(as.data.frame(combinedData$region.data)[, mapVars], na.rm = TRUE),
                                 max(as.data.frame(combinedData$region.data)[, mapVars], na.rm = TRUE))
            }

            print(plotProjection)
            map.plot <- plot(combinedData, main = plotTitle,
                 axis.labels = plotAxes, xlab = plotXLab, ylab = plotYLab,
                 datum.lines = plotDatumLines, projection = plotProjection,
                 multiple.vars = multiple.vars, colour.var = mapVars,
                 size.var = mapSizeVar, aggregate = aggregation,
                 darkTheme = plotTheme, alpha.const = plotConstantAlpha, size.const = plotConstantSize,
                 current.seq = plotCurrentSeqVal, palette = plotPalette,
                 sparkline.type = plotSparklinesType,
                 regions.to.plot = mapRegionsPlot, keep.other.regions = mapExcludedRegions,
                 scale.limits = axis.limits, label.var = plotLabelVar,
                 scale.axis = plotAxisScale, scale.label = plotLabelScale)

            GUI$rhistory$add(attr(map.plot, "code"), keep = FALSE)

            dev.hold()
            grid::grid.newpage()
            grid::grid.draw(map.plot)
            dev.flush()
            gdkWindowSetCursor(getToolkitWidget(GUI$win)$getWindow(), NULL)

            invisible(map.plot)
            plotObject <<- map.plot
        },

        ## After the map object has been constructed, initialize the interface for the Maps module (sidebar)
        initiateModule = function() {
            updateOptions = function() {
                ## Plot Options
                print("UpdateOptions")
                plotTitle <<- svalue(edit.plottitle)
                plotAxes <<- svalue(checkbox.axislabels)
                plotXLab <<- svalue(edit.xaxis)
                plotYLab <<- svalue(edit.yaxis)
                plotDatumLines <<- svalue(checkbox.datum)

                plotProjection <<- ifelse(svalue(combobox.mapproj) == "From Shapefile",
                                          "Default",
                                          proj.df[svalue(combobox.mapproj, index = TRUE) - 1, "PROJ4"])

                plotTheme <<- svalue(checkbox.darktheme)
                plotPalette <<- svalue(combobox.palette)
                plotConstantAlpha <<- 1 - svalue(slider.constalpha)
                plotAxisScale <<- svalue(slider.scaleaxis)
                plotLabelScale <<- svalue(slider.scalelabels)

                if (combinedData$type == "sparklines") {
                    plotConstantSize <<- svalue(slider.constsizespark)
                } else {
                    plotConstantSize <<- svalue(slider.constsize)
                }

                if (svalue(checkbox.labels)) {
                    if (svalue(combobox.labels, index = TRUE) == 1) {
                        plotLabelVar <<- "use_colour_var"
                    } else {
                        plotLabelVar <<- svalue(combobox.labels)
                    }
                } else {
                    plotLabelVar <<- NULL
                }

                ## Variable Options
                mapVars <<- svalue(table.vars)
                mapSizeVar <<- svalue(combobox.sizeselect)

                plotScaleLimits <<- switch(svalue(combobox.scale),
                                          "Independent scales" = NULL,
                                          "Same for all plots" = iNZightMaps::getMinMax(combinedData, mapVars),
                                          "Scales fixed at 0-1" = c(0, 1),
                                          "Scales fixed at 0-100" = c(0, 100),
                                          "Scales fixed at custom range" = as.numeric(c(svalue(input.scalemin),
                                                                          svalue(input.scalemax))))


                ## Only assign value if at least one is unchecked. This prevents
                ## the plot function from needing to run filter() for no reason.
                if (length(svalue(checkbox.regions)) != length(checkbox.regions)) {
                    mapRegionsPlot <<- svalue(checkbox.regions)
                    ## mapExcludedRegions <<- svalue(checkbox.showexcluded)
                    mapExcludedRegions <<- TRUE
                } else {
                    mapRegionsPlot <<- NULL
                }

                if(length(mapVars) == 0) {
                    mapVars <<- NULL
                }

                if(length(mapSizeVar) == 0 || mapSizeVar == "") {
                    mapSizeVar <<- NULL
                }

                ## Sparklines Options
                if (combinedData$multiple.obs) {
                    plotSparklinesType <<- svalue(combobox.sparkline)
                }

                updatePlot()
            }

            playPlot <- function(currSeq = 1) {
                if (currSeq < length(combobox.singleval$items)) {
                    svalue(combobox.singleval, index = TRUE) <- currSeq + 1
                } else {
                    plotPlay <<- FALSE
                }
            }


            GUI$initializeModuleWindow(.self)
            GUI$rhistory$add(c(sprintf("## Using the %s map", mapName)), keep = TRUE)
            GUI$rhistory$add(attr(combinedData, "code"), keep = TRUE)

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
            expand.mapoptions <- gexpandgroup(text = "Map Options", horizontal = FALSE)
            font(expand.mapoptions) <- list(weight = "bold", family = "normal", size = 10)

            add(mainGrp, frame.mapoptions)
            add(frame.mapoptions, group.mapoptions, expand = TRUE)
            add(group.mapoptions, expand.mapoptions, expand = TRUE)

            frame.plotoptions <- gframe(horizontal = FALSE)
            group.plotoptions <- ggroup(spacing = 5)
            group.plotoptions$set_borderwidth(10)
            expand.plotoptions <- gexpandgroup(text = "Extra Plot Options", horizontal = FALSE)
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
            proj.df <- iNZightMaps::iNZightMapProjections()

            lbl.mapproj <- glabel("Projection:")
            combobox.mapproj <- gcombobox(c("From Shapefile", proj.df$Name))

            if(!is.null(plotProjection)) {
                if (plotProjection == "Default")
                    svalue(combobox.mapproj, index = TRUE) <- 1
                else
                    svalue(combobox.mapproj) <- proj.df[which(proj.df$PROJ4 == plotProjection), "Name"]
            }

            tbl.mapoptions[4, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.mapproj
            tbl.mapoptions[4, 2:4, expand = TRUE] <- combobox.mapproj


            ######
            group.regions <- ggroup(use.scrollwindow = TRUE)
            checkbox.regions <- gcheckboxgroup(iNZightMaps::iNZightMapRegions(combinedData),
                                               horizontal=FALSE, checked = TRUE)
            checkbox.regionall <- gcheckbox("Select All")
            checkbox.regionnone <- gcheckbox("Select None")
            ## checkbox.showexcluded <- gcheckbox("Plot Excluded Regions", checked = TRUE)
            add(group.regions, checkbox.regions, expand = TRUE, fill = TRUE)

            tbl.mapoptions[2, 1:4, expand = TRUE, fill = TRUE] <- group.regions
            tbl.mapoptions[3, 1, expand = TRUE, fill = TRUE] <- checkbox.regionall
            tbl.mapoptions[3, 2, expand = TRUE, fill = TRUE] <- checkbox.regionnone
            ## tbl.mapoptions[4, 1:4, expand = TRUE, fill = TRUE] <- checkbox.showexcluded

            addHandlerChanged(checkbox.regions, function(h, ...) {
                if (length(svalue(checkbox.regions, index = TRUE)) != length(checkbox.regions)) {
                    svalue(checkbox.regionall) <- FALSE
                }
                if (length(svalue(checkbox.regions, index = TRUE)) > 0) {
                    svalue(checkbox.regionnone) <- FALSE
                }

                updateOptions()
            })

            ## addHandlerChanged(checkbox.showexcluded, function(h, ...) {
                ## updateOptions()
            ## })

            addHandlerChanged(checkbox.regionall, function(h, ...) {
                if (svalue(checkbox.regionall)) {
                    svalue(checkbox.regions, index = TRUE) <- 1:length(checkbox.regions)
                }
            })

            addHandlerChanged(checkbox.regionnone, function(h, ...) {
                if (svalue(checkbox.regionnone)) {
                    svalue(checkbox.regions) <- FALSE
                }
            })
            ######

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

            lbl.palette <- glabel("Map Palette:")
            checkbox.darktheme <- gcheckbox("Dark")
            combobox.palette <- gcombobox(c("Default",
                                            "Viridis", "Magma", "Plasma", "Inferno",
                                            "BrBG", "PiYG", "PRGn",
                                            "Accent", "Dark2", "Paired", "Pastel1", "Set1",
                                            "Blues", "BuGn", "BuPu", "GnBu"))

            svalue(combobox.palette) <- plotPalette
            svalue(checkbox.darktheme) <- plotTheme

            lbl.labels <- glabel("Region Labels:")
            checkbox.labels <- gcheckbox("Region Labels")
            combobox.labels <- gcombobox(c("Current Variable", iNZightMaps::iNZightMapVars(combinedData, map.vars = TRUE)))
            visible(combobox.labels) <- FALSE

            addHandlerChanged(checkbox.labels, function(h, ...) {
                visible(combobox.labels) <- svalue(checkbox.labels)
                visible(lbl.scalelabels) <- svalue(checkbox.labels)
                visible(slider.scalelabels) <- svalue(checkbox.labels)
                updateOptions()
            })

            addHandlerChanged(combobox.labels, function(h, ...) {
                updateOptions()
            })

            addHandlerChanged(checkbox.darktheme, function(h, ...) {
                updateOptions()
            })

            addHandlerChanged(combobox.palette, function(h, ...) {
              updateOptions()
            })

            ## checkbox.scaleprop <- gcheckbox("Fixed Scale for Proportions", visible = TRUE)
            lbl.scale <- glabel("Map Scales:")
            combobox.scale <- gcombobox(c("Independent scales",
                                          "Same for all plots",
                                          "Scales fixed at 0-1",
                                          "Scales fixed at 0-100",
                                          "Scales fixed at custom range"))
            input.scalemin <- gedit(initial.msg = "Min", width = 4)
            input.scalemax <- gedit(initial.msg = "Max", width = 4)
            tbl.scales <- glayout()
            tbl.scales[1, 1] <- input.scalemin
            tbl.scales[1, 2] <- input.scalemax

            visible(tbl.scales) <- FALSE

            lbl.scaleaxis <- glabel("Plot title font size:")
            lbl.scalelabels <- glabel("Label font size:")
            slider.scaleaxis <- gslider(7, 15, value = 11)
            slider.scalelabels <- gslider(2, 6, value = 4, by = 0.5)

            visible(lbl.scalelabels) <- svalue(checkbox.labels)
            visible(slider.scalelabels) <- svalue(checkbox.labels)

            addHandlerChanged(combobox.scale, function(h, ...) {
                visible(tbl.scales) <- svalue(combobox.scale) == "Scales fixed at custom range"
                updateOptions()
            })

            addHandlerChanged(input.scalemin, function(h, ...) {
                updateOptions()
            })

            addHandlerChanged(input.scalemax, function(h, ...) {
                updateOptions()
            })

            addHandlerChanged(slider.scaleaxis, function(h, ...) {
                if (!is.null(timer))
                    if (timer$started) timer$stop_timer()
                timer <<- gtimer(1000, function(...) updateOptions(), one.shot = TRUE)
            })

            addHandlerChanged(slider.scalelabels, function(h, ...) {
                if (!is.null(timer))
                    if (timer$started) timer$stop_timer()
                timer <<- gtimer(1000, function(...) updateOptions(), one.shot = TRUE)
            })

            tbl.xaxisedit <- glayout()
            tbl.xaxisedit[1, 1, expand = TRUE] <- edit.xaxis

            tbl.yaxisedit <- glayout()
            tbl.yaxisedit[1, 1, expand = TRUE] <- edit.yaxis

            tbl.plotoptions[1, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.plottitle
            tbl.plotoptions[1, 2:4, expand = TRUE] <- edit.plottitle

            tbl.plotoptions[2, 1, expand = TRUE,  anchor = c(1, 0)] <- lbl.palette
            tbl.plotoptions[2, 2:3, expand = TRUE] <- combobox.palette
            tbl.plotoptions[2, 4] <- checkbox.darktheme

            tbl.plotoptions[3, 2, expand = TRUE, anchor = c(-1, 0)] <- checkbox.datum

            tbl.plotoptions[3, 4, expand = TRUE] <- checkbox.axislabels
            ## tbl.plotoptions[5, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.xaxis
            ## tbl.plotoptions[5, 2:4, expand = TRUE] <- tbl.xaxisedit
            ## tbl.plotoptions[6, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.yaxis
            ## tbl.plotoptions[6, 2:4, expand = TRUE] <- tbl.yaxisedit

            ## tbl.plotoptions[4, 1, expand = TRUE] <- lbl.labels
            tbl.plotoptions[6, 2] <- checkbox.labels
            tbl.plotoptions[6, 3:4, expand = TRUE] <- combobox.labels

            tbl.plotoptions[4, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.scale
            tbl.plotoptions[4, 2:3, expand = TRUE] <- combobox.scale
            tbl.plotoptions[4, 4] <- tbl.scales

            tbl.plotoptions[5, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.scaleaxis
            tbl.plotoptions[5, 2:4] <- slider.scaleaxis
            tbl.plotoptions[7, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.scalelabels
            tbl.plotoptions[7, 2:4] <- slider.scalelabels

            slider.constalpha     <- gslider(0, 0.9, by = 0.1)
            slider.constsize      <- gslider(1, 10, by = 1, value = 5)
            slider.constsizespark <- gslider(0.5, 2.0, by = 0.25, value = 1.25)

            lbl.constalpha     <- glabel("Transparency of map:")
            lbl.constsize      <- glabel("Overall size:")
            lbl.constsizespark <- glabel("Overall size:")

            visible(slider.constalpha) <- mapType == "point"
            visible(lbl.constalpha)    <- mapType == "point"
            visible(slider.constsize)  <- mapType == "point"
            visible(lbl.constsize)     <- mapType == "point"

            visible(slider.constsizespark) <- FALSE

            addHandlerChanged(slider.constalpha, function(h, ...) {
                if (!is.null(timer))
                    if (timer$started) timer$stop_timer()
                timer <<- gtimer(1000, function(...) updateOptions(), one.shot = TRUE)
            })

            addHandlerChanged(slider.constsize, function(h, ...) {
                if (!is.null(timer))
                    if (timer$started) timer$stop_timer()
                timer <<- gtimer(1000, function(...) updateOptions(), one.shot = TRUE)
            })

            addHandlerChanged(slider.constsizespark, function(h, ...) {
                if (!is.null(timer))
                    if (timer$started) timer$stop_timer()
                timer <<- gtimer(1000, function(...) updateOptions(), one.shot = TRUE)
            })

            add(expand.plotoptions, tbl.plotoptions, expand = TRUE, fill = TRUE)

            visible(lbl.xaxis)     <- plotAxes
            visible(tbl.xaxisedit) <- plotAxes
            visible(lbl.yaxis)     <- plotAxes
            visible(tbl.yaxisedit) <- plotAxes

            addHandlerChanged(checkbox.axislabels, function (h, ...) {
                if(svalue(checkbox.axislabels)) {
                    visible(lbl.xaxis)     <- TRUE
                    visible(tbl.xaxisedit) <- TRUE
                    visible(lbl.yaxis)     <- TRUE
                    visible(tbl.yaxisedit) <- TRUE
                } else {
                    visible(lbl.xaxis)     <- FALSE
                    visible(tbl.xaxisedit) <- FALSE
                    visible(lbl.yaxis)     <- FALSE
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

            var.vect <- iNZightMaps::iNZightMapVars(combinedData)
            table.vars <- gtable(sort(var.vect), multiple = TRUE)
            table.vars$widget$`headers-visible` <- FALSE

            if (!is.null(mapVars)) {
                svalue(table.vars) <- mapVars
            }

            lbl.maptype <- glabel("Plot as:")
            radio.maptype <- gradio(c("Regions", "Centroids"), horizontal = TRUE,
                                    selected = (mapType == "point") + 1)


            lbl.sizeselect <- glabel("Size by:")
            numericvar.vect <- c("", sort(iNZightMaps::iNZightMapVars(combinedData, TRUE)[combinedData$var.types %in% c("numeric", "integer")]))
            combobox.sizeselect <- gcombobox(numericvar.vect)

            if (!is.null(mapSizeVar)) {
                svalue(combobox.sizeselect) <- mapSizeVar
            }

            if (!combinedData$multiple.obs) {
                tbl.main[2, 1,   expand = TRUE, anchor = c(1, 0)] <- lbl.maptype
                tbl.main[2, 2,   expand = TRUE, anchor = c(-1, 0), fill = "x"] <- radio.maptype

                tbl.main[3, 1,   expand = TRUE, anchor = c(1, 0)] <- lbl.sizeselect
                tbl.main[3, 2,   expand = TRUE] <- combobox.sizeselect
                tbl.main[5, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.constalpha
                tbl.main[5, 2, expand = TRUE, anchor = c(-1, 0)] <- slider.constalpha
                tbl.main[4, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.constsize
                tbl.main[4, 2, expand = TRUE, anchor = c(-1, 0)] <- slider.constsize
            } else {
                separator.timevariable <- gseparator()
                lbl.timevariable <- glabel("Dataset has multiple observations for regions:")
                font(lbl.timevariable) <- list(weight = "bold", size = 10, family = "normal")

                radio.multipleobs <- gradio(c("Single Value", "All Values", "Aggregate"), horizontal = TRUE)

                if (isTRUE(is.null(multipleObsOption))) {
                    multipleObsOption <<- "singleval"
                }

                lbl.singleval <- glabel(sprintf("Value of %s variable:", mapSequenceVar))
                unique.singlevals <- unique(as.data.frame(combinedData[["region.data"]])[, combinedData$sequence.var])
                combobox.singleval <- gslider(unique.singlevals[!is.na(unique.singlevals)])
                svalue(combobox.singleval) <- combobox.singleval$items[length(combobox.singleval$items)]

                radio.allvalues <- gradio(c("Sparklines"), horizontal = TRUE)

                # Relative       [ ]
                # Percent change [ ]
                # Starting/Ending positions |----[]-------|
                lbl.aggregate <- glabel("Aggregation type:")
                combobox.aggregate <- gcombobox(c("Mean", "Median"))
                lbl.sparkline <- glabel("Line Chart Type:")
                combobox.sparkline <- gcombobox(c("Absolute", "Relative", "Percent Change"))

                tbl.main[2, 1:3] <- lbl.timevariable

                tbl.main[3, 1:3] <- radio.multipleobs

                tbl.main[4, 1,   expand = TRUE, anchor = c(1, 0)] <- lbl.singleval
                tbl.main[4, 2, expand = TRUE, anchor = c(-1, 0), fill = "x"] <- combobox.singleval
                tbl.main[4, 1,   expand = TRUE, anchor = c(1, 0)] <- lbl.aggregate
                tbl.main[4, 2:3, expand = TRUE, anchor = c(-1, 0), fill = "x"] <- combobox.aggregate

                tbl.main[5, 1:3] <- separator.timevariable

                tbl.main[6, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.maptype
                tbl.main[6, 2, expand = TRUE, anchor = c(-1, 0), fill = "x"] <- radio.allvalues
                tbl.main[6, 2, expand = TRUE, anchor = c(-1, 0), fill = "x"] <- radio.maptype

                tbl.main[7, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.sizeselect
                tbl.main[7, 2, expand = TRUE] <- combobox.sizeselect
                tbl.main[7, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.sparkline
                tbl.main[7, 2, expand = TRUE] <- combobox.sparkline

                tbl.main[9, 1, expand = TRUE, anchor = c(1, 0)]  <- lbl.constalpha
                tbl.main[9, 2, expand = TRUE, anchor = c(-1, 0)] <- slider.constalpha
                tbl.main[8, 1, expand = TRUE, anchor = c(1, 0)]  <- lbl.constsize
                tbl.main[8, 2, expand = TRUE, anchor = c(-1, 0)] <- slider.constsize
                tbl.main[8, 2, expand = TRUE, anchor = c(-1, 0)] <- slider.constsizespark

                img.playicon <- system.file("images/icon-play.png", package = "iNZight")
                img.stopicon <- system.file("images/icon-stop.png", package = "iNZight")

                btn.play <- iNZight:::gimagebutton(filename = img.playicon, size = "button")
                btn.delay <- iNZight:::gimagebutton(filename = system.file("images/icon-clock.png", package = "iNZight"),
                                                    size = "button")

                addHandlerClicked(btn.play, function(h, ...) {
                    if (svalue(combobox.singleval, index = TRUE) < length(combobox.singleval$items)) {
                        plotPlay <<- TRUE
                        btn.play$set_value(img.stopicon)

                        svalue(combobox.singleval, index = TRUE) <- svalue(combobox.singleval, index = TRUE) + 1
                    }
                })
                
                addHandlerClicked(btn.delay, function(h, ...) {
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

                tbl.playcontrol <- glayout()
                tbl.playcontrol[1, 1, anchor = c(-1, 1)] <- btn.play
                tbl.playcontrol[1, 2, anchor = c(-1, 1)] <- btn.delay

                tbl.main[4, 3, anchor = c(-1, 1)] <- tbl.playcontrol

                visible(radio.allvalues)    <- FALSE
                visible(lbl.aggregate)      <- FALSE
                visible(combobox.aggregate) <- FALSE
                visible(combobox.sparkline) <- FALSE

                addHandlerChanged(radio.multipleobs, function(h, ...) {
                    radio.choice <- svalue(radio.multipleobs, index = TRUE)

                    if (isTRUE(!is.null(mapVars))) {
                        visible(lbl.singleval)      <- radio.choice == 1
                        visible(combobox.singleval) <- radio.choice == 1
                        visible(radio.allvalues)    <- radio.choice == 2
                        visible(lbl.aggregate)      <- radio.choice == 3
                        visible(combobox.aggregate) <- radio.choice == 3

                        visible(lbl.maptype)   <- TRUE
                        visible(radio.maptype) <- radio.choice != 2

                        visible(lbl.sizeselect)      <- radio.choice != 2 && mapType == "point"
                        visible(combobox.sizeselect) <- radio.choice != 2 && mapType == "point"

                        visible(lbl.constsize)    <- mapType == "point" || radio.choice == 2
                        visible(slider.constsize) <- mapType == "point" && radio.choice != 2

                        visible(lbl.constalpha)    <- mapType == "point" || radio.choice == 2
                        visible(slider.constalpha) <- mapType == "point" || radio.choice == 2

                        visible(lbl.sparkline)         <- radio.choice == 2
                        visible(combobox.sparkline)    <- radio.choice == 2
                        visible(slider.constsizespark) <- radio.choice == 2

                        visible(btn.play) <- radio.choice == 1
                        visible(btn.delay) <- radio.choice == 1
                    } else {
                        visible(lbl.singleval)       <- FALSE
                        visible(combobox.singleval)  <- FALSE
                        visible(radio.allvalues)     <- FALSE
                        visible(lbl.aggregate)       <- FALSE
                        visible(combobox.aggregate)  <- FALSE
                        visible(lbl.maptype)         <- FALSE
                        visible(radio.maptype)       <- FALSE
                        visible(lbl.sizeselect)      <- FALSE
                        visible(combobox.sizeselect) <- FALSE
                        visible(lbl.constsize)       <- FALSE
                        visible(slider.constsize)    <- FALSE
                        visible(lbl.constalpha)      <- FALSE
                        visible(slider.constalpha)   <- FALSE
                        visible(lbl.sparkline)       <- FALSE
                        visible(combobox.sparkline)  <- FALSE
                        visible(btn.play)            <- FALSE
                        visible(btn.delay)           <- FALSE
                    }

                    if (radio.choice == 1) {
                        multipleObsOption <<- "singleval"
                        combinedData$type <<- mapType
                        plotCurrentSeqVal <<- svalue(combobox.singleval)
                        combinedData <<- iNZightMaps::iNZightMapAggregation(combinedData,
                                                               "singlevalue",
                                                               single.value = svalue(combobox.singleval))
                    } else if (radio.choice == 2) {
                        multipleObsOption <<- "allvalues"
                        combinedData$type <<- "sparklines"
                        plotCurrentSeqVal <<- NULL
                        if (isTRUE(!is.null(mapVars))) {
                            vars.to.keep <- sapply(as.data.frame(combinedData$region.data)[, mapVars, drop = FALSE], is.numeric)
                            if (sum(vars.to.keep) > 0) {
                                svalue(table.vars) <- mapVars[vars.to.keep]
                            } else {
                                svalue(table.vars, index = TRUE) <- 0
                            }
                        }
                    } else if (radio.choice == 3) {
                        multipleObsOption <<- "aggregate"
                        combinedData$type <<- mapType
                        plotCurrentSeqVal <<- svalue(combobox.aggregate)
                        combinedData <<- iNZightMaps::iNZightMapAggregation(combinedData,
                                                               tolower(svalue(combobox.aggregate)))
                    }

                    if (isTRUE(length(svalue(table.vars)) > 1)) {
                        svalue(edit.plottitle) <- ""
                    } else {
                        if (isTRUE(has.multipleobs)) {
                            if (isTRUE(multipleObsOption == "singleval")) {
                                svalue(edit.plottitle) <- paste0(svalue(table.vars), " (", svalue(combobox.singleval), ")")
                            } else if (multipleObsOption == "aggregate") {
                                svalue(edit.plottitle) <- paste0(svalue(table.vars), " (", svalue(combobox.aggregate), ")")
                            } else {
                                svalue(edit.plottitle) <- svalue(table.vars)
                            }
                        } else {
                            svalue(edit.plottitle) <- svalue(table.vars)
                        }
                    }
                })

                addHandlerChanged(combobox.singleval, function(h, ...) {
                    combinedData <<- iNZightMaps::iNZightMapAggregation(combinedData, "singlevalue",
                                                           single.value = svalue(combobox.singleval))
                    plotCurrentSeqVal <<- svalue(combobox.singleval)

                    if (plotPlay) {
                        ## playPlot(svalue(combobox.singleval, index = TRUE))
                        plotTitle <<- sprintf("%s (%s)", mapVars, plotCurrentSeqVal)
                        updatePlot()

                        if (svalue(combobox.singleval, index = TRUE) < length(combobox.singleval$items)) {
                            Sys.sleep(playdelay)
                            svalue(combobox.singleval, index = TRUE) <- svalue(combobox.singleval, index = TRUE) + 1
                        } else {
                            btn.play$set_value(img.playicon)
                            plotPlay <<- FALSE
                        }
                    } else {
                        if (isTRUE(length(svalue(table.vars)) > 1)) {
                            svalue(edit.plottitle) <- ""
                        } else {
                            if (isTRUE(has.multipleobs && multipleObsOption == "singleval")) {
                                svalue(edit.plottitle) <- paste0(svalue(table.vars), " (", svalue(combobox.singleval), ")")
                            } else {
                                svalue(edit.plottitle) <- svalue(table.vars)
                            }
                        }
                    }
                })

                addHandlerChanged(radio.allvalues, function(h, ...) {
                    combinedData$type <<- "sparklines"
                })

                addHandlerChanged(combobox.aggregate, function(h, ...) {
                  combinedData <<- iNZightMaps::iNZightMapAggregation(combinedData,
                                                                      tolower(svalue(combobox.aggregate)))
                  plotCurrentSeqVal <<- svalue(combobox.aggregate)
                  
                  if (isTRUE(length(svalue(table.vars)) > 1)) {
                    svalue(edit.plottitle) <- ""
                  } else {
                    if (isTRUE(has.multipleobs && multipleObsOption == "aggregate")) {
                      svalue(edit.plottitle) <- paste0(svalue(table.vars), " (", svalue(combobox.aggregate), ")")
                    } else {
                      svalue(edit.plottitle) <- svalue(table.vars)
                    }
                  }
                })
                
                addHandlerChanged(combobox.sparkline, function(h, ...) {
                  updateOptions()
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

            addDropSource(table.vars, function(h, ...) {
                varname <- svalue(h$obj)
                varname
            })

            addHandlerSelectionChanged(table.vars, function(h, ...) {
                if (isTRUE(length(svalue(table.vars)) > 0)) {
                    visible(lbl.maptype) <- TRUE
                    if (has.multipleobs) {
                        if (isTRUE(multipleObsOption != "allvalues")) {
                            visible(radio.maptype) <- TRUE
                            visible(radio.allvalues) <- FALSE
                        } else {
                            visible(radio.maptype) <- FALSE
                            visible(radio.allvalues) <- TRUE
                        }
                    } else {
                        visible(radio.maptype) <- TRUE
                    }
                } else {
                    visible(lbl.maptype) <- FALSE
                    visible(radio.maptype) <- FALSE
                    if (has.multipleobs) {
                        visible(radio.allvalues) <- FALSE
                        visible(lbl.sizeselect) <- FALSE
                        visible(combobox.sizeselect) <- FALSE
                    }
                }

                if (combinedData$type == "sparklines") {
                    vars.numeric <- combinedData$var.types[svalue(table.vars)] %in% c("numeric", "integer")
                    if (isTRUE(any(!vars.numeric))) {
                        galert("Categorical variables cannot be used with sparklines")
                        if (sum(vars.numeric) > 0) {
                            svalue(table.vars) <- svalue(table.vars)[vars.numeric]
                        } else {
                            svalue(table.vars, index = TRUE) <- 0
                        }
                    }
                }

                if(length(svalue(table.vars)) > 1) {
                   svalue(edit.plottitle) <- ""
                } else {
                    if (isTRUE(has.multipleobs)) {
                        if (multipleObsOption == "singleval") {
                            svalue(edit.plottitle) <- paste0(svalue(table.vars), " (", svalue(combobox.singleval), ")")
                        } else if (multipleObsOption == "aggregate") {
                            svalue(edit.plottitle) <- paste0(svalue(table.vars), " (", svalue(combobox.aggregate), ")")
                        } else {
                            svalue(edit.plottitle) <- svalue(table.vars)
                        }
                    } else {
                        svalue(edit.plottitle) <- svalue(table.vars)
                    }
                }
            })

            addHandlerChanged(radio.maptype, function(h, ...) {
                if (svalue(radio.maptype, index = TRUE) == 1) {
                    combinedData$type <<- "region"
                    mapType <<- "region"
                } else if (svalue(radio.maptype, index = TRUE) == 2) {
                    combinedData$type <<- "point"
                    mapType <<- "point"
                }
                    visible(lbl.sizeselect) <- svalue(radio.maptype, index = TRUE) == 2
                    visible(combobox.sizeselect) <- svalue(radio.maptype, index = TRUE) == 2
                    visible(lbl.constalpha) <- svalue(radio.maptype, index = TRUE) == 2
                    visible(slider.constalpha) <- svalue(radio.maptype, index = TRUE) == 2
                    visible(lbl.constsize) <- svalue(radio.maptype, index = TRUE) == 2
                    visible(slider.constsize) <- svalue(radio.maptype, index = TRUE) == 2

                updateOptions()
            })

            addHandlerChanged(combobox.sizeselect, function(h, ...) {
                updateOptions()
            })

            addDropTarget(combobox.sizeselect, function(h, ...) {
                svalue(combobox.sizeselect) <- h$dropdata
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

            exportButton <- iNZight:::gimagebutton(stock.id = "zoom-in",
                                         tooltip = "Export interactive map", size = "button")

            addHandlerClicked(exportButton, function(h, ...) {
                browseURL(iNZightPlots::exportHTML(x = plotObject,
                                         mapObj = combinedData,
                                         file = tempfile(fileext = ".html")))
            })

            GUI$plotToolbar$update(NULL, refresh = "updatePlot", extra = list(exportButton))

            visible(mainGrp) <<- TRUE
            updateOptions()
        }
        ## Update and plot the map given
    ))

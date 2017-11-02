library(gWidgets2)
map.data <- NULL
map.vars <- NULL
nomatch.df <- data.frame(var.name = "")

## State Variables
stale.map.data <- TRUE

## Layout Styles
font.header <- list(weight = "bold", size = 12, family = "normal")

## Overall Layout
w.match <- gwindow("Match Variables", width = 700, height = 500,
                   visible = TRUE)

gv.match <- gvbox(container = w.match, expand = TRUE, fill = TRUE)
gv.match$set_borderwidth(15)

## Expandable boxes 
frame.import <- gframe(horizontal =  FALSE)
group.import <- ggroup(spacing = 5)
group.import$set_borderwidth(10)
expand.import    <- gexpandgroup(text = "Select Map", horizontal = FALSE)

frame.variables <- gframe(horizontal =  FALSE)
group.variables <- ggroup(spacing = 5)
group.variables$set_borderwidth(10)
expand.variables <- gexpandgroup(text = "Select Merge Variables", horizontal = FALSE)

frame.merge <- gframe(horizontal =  FALSE)
group.merge <- ggroup(spacing = 5)
group.merge$set_borderwidth(10)
expand.merge     <- gexpandgroup(text = "Merge Results")

font(expand.import)    <- font.header
font(expand.variables) <- font.header
font(expand.merge)     <- font.header

visible(expand.variables) <- FALSE

enabled(frame.variables) <- FALSE

## Add all frames to window
add(gv.match, frame.import, expand = TRUE)
add(gv.match, frame.variables, expand = TRUE)
add(gv.match, frame.merge)

## Map Source Box

lbl <- glabel("Map Source:")
mapSource <- gradio(c("Use Inbuilt Map", "Import Shapefile"),
                    horizontal = TRUE)

### Inbuilt Map Data
tblInbuiltfile <- glayout()
stored.shapefiles <- list.files("H:/Documents/iNZightVIT/shapefiles/",
                                recursive = TRUE,
                                pattern = ".shp$")
# stored.shapefiles <- sub(".shp$", "", stored.shapefiles)
            
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

mapInbuiltBrowse <- gtree(offspring = offspring.files,
                          offspring.data = stored.shapefiles,
                          chosen.col = "filename",
                          offspring.col = "has.children")

tblInbuiltfile[1, 1, expand = TRUE, fill = "both"] <- mapInbuiltBrowse

### User-imported Shapefile
tblShapefile <- glayout()
            
mapSourceBrowse <- gfilebrowse(text = "Open Shapefile...",
                               type = "open",
                               filter = list("All formats" = list(patterns = c("*.shp",
                                                                               "*.json",
                                                                               "*.geojson")),
                                             "Shapefile" = list(patterns = c("*.shp")),
                                             "GeoJSON" = list(patterns = c("*.json",
                                                                           "*.geojson"))))
            
tblShapefile[1, 1, expand = TRUE] <- mapSourceBrowse

btn.import <- gbutton(text = "Import Map")

### Add widgets to layout
add(frame.import, group.import, expand = TRUE)
add(group.import, expand.import, expand = TRUE)
add(expand.import, lbl)
add(expand.import, mapSource)
add(expand.import, tblShapefile, expand = TRUE)
add(expand.import, tblInbuiltfile, expand = TRUE)
addSpace(expand.import, value = 15)
add(expand.import, btn.import)
            
visible(tblInbuiltfile) <- TRUE
visible(tblShapefile) <- FALSE
            
findBestMatch <- function(data, map.data) {
    ## Eliminate columns with duplicates in the shapefile
    map.data <- map.data[, !(apply(map.data, 2, anyDuplicated))]
    ## Eliminate multiple rows per observation
    match.sums <- apply(data, 2, function(x) {
        apply(map.data, 2, function(y) {
            sum(y %in% unique(x))
        })
    })
    ## Find indices of the best matching pair
    best.match <- which(match.sums == max(match.sums), arr.ind = TRUE)[1,]
    ## Extract out the names of the variables (as we have removed map vars with dups)
    best.match.vars <- c(colnames(match.sums)[best.match[2]], rownames(match.sums)[best.match[1]])
    best.match.vars
}

addHandlerChanged(mapSource, function(h, ...) {
    v <- svalue(mapSource, index = TRUE)
    visible(tblShapefile) <- v == 2
    visible(tblInbuiltfile) <- v == 1
})

addHandlerClicked(btn.import, handler = function(h, ...) {
    ## Change which region has focus
    visible(expand.import) <- FALSE
    visible(expand.variables) <- TRUE

    ## Extract the filename from inputs
    if(svalue(mapSource, index = TRUE) == 1) {
        inbuilt.path <- paste(svalue(mapInbuiltBrowse), collapse = "/")
        map.filename <- paste0("H:/Documents/iNZightVIT/shapefiles/", inbuilt.path)
    } else {
        map.filename <- svalue(mapSourceBrowse)
    }
    
    ## Loading bar
    ### TO IMPLEMENT

    ## Import the map data
    ### TO CHANGE: Allow rds files too
    lbl.loading <- glabel("Please wait... Map is loading...")
    add(expand.variables, lbl.loading)
    visible(expand.variables) <- TRUE
    
    map.data <<- sf::st_read(map.filename)
    map.vars <<- as.data.frame(map.data)[, !(colnames(map.data) %in% "geometry")]
    combobox.mapvars[] <- colnames(map.vars)

    best.vars <- findBestMatch(activeData, map.vars)
    best.data.var <- best.vars[1]
    best.map.var <-  best.vars[2]
    
    Sys.sleep(2)
    
    delete(expand.variables, lbl.loading)

    visible(tbl.variables) <- TRUE
    visible(table.nonmatched) <- TRUE
    enabled(frame.variables) <- TRUE

    svalue(combobox.datavars) <- best.data.var
    svalue(combobox.mapvars) <- best.map.var
})

## Variables Box
tbl.variables <- glayout()

combobox.mapvars <- gcombobox(items = c(""))
combobox.datavars <- gcombobox(items = colnames(activeData))

tbl.variables[1, 1] <- glabel("Map Variable: ")
tbl.variables[1, 2, expand = TRUE] <- combobox.mapvars

tbl.variables[1, 4] <- glabel("Data Variable: ")
tbl.variables[1, 5, expand = TRUE] <- combobox.datavars

table.nonmatched <- gtable(nomatch.df)
lbl.allmatched <- glabel("All matched!")

### Add to frame
add(frame.variables, group.variables, expand = TRUE)
add(group.variables, expand.variables, expand = TRUE)
addSpace(expand.variables, 15)
add(expand.variables, tbl.variables)
addSpace(expand.variables, 15)
add(expand.variables, table.nonmatched, expand = TRUE)
addSpace(expand.variables, 15)
add(expand.variables, lbl.allmatched)

visible(tbl.variables) <- FALSE
visible(table.nonmatched) <- FALSE
visible(lbl.allmatched) <- FALSE

cb.change <- function(h, ...) {
    enabled(table.nonmatched) <- FALSE

    data.var <- svalue(combobox.datavars)
    map.var <- svalue(combobox.mapvars)

    data.is.na <- is.na(activeData[, data.var])
    activeData2 <- activeData[!data.is.na, ]

    table.nonmatched[] <- unique(activeData2[!(as.character(activeData2[, data.var]) %in% as.character(map.vars[, map.var])), data.var, drop = FALSE])

    enabled(table.nonmatched) <- TRUE
    ##print(visible(table.nonmatched))

##     if(length(table.nonmatched[]) > 0) {
##         visible(table.nonmatched) <- TRUE
##         visible(lbl.allmatched) <- FALSE
##     } else {
##         visible(table.nonmatched) <- FALSE
##         visible(table.nonmatched) <- FALSE
##         visible(lbl.allmatched) <- TRUE
##     }
    
    }

addHandlerChanged(combobox.mapvars, handler = cb.change)
addHandlerChanged(combobox.datavars, handler = cb.change)



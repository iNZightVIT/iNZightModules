library(gWidgets2)

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
expand.variables <- gexpandgroup(text = "Select Merge Variables")

frame.merge <- gframe(horizontal =  FALSE)
group.merge <- ggroup(spacing = 5)
group.merge$set_borderwidth(10)
expand.merge     <- gexpandgroup(text = "Merge Results")

font(expand.import)    <- font.header
font(expand.variables) <- font.header
font(expand.merge)     <- font.header

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
            
addHandlerChanged(mapSource, function(h, ...) {
    v <- svalue(mapSource, index = TRUE)
    visible(tblShapefile) <- v == 2
    visible(tblInbuiltfile) <- v == 1
})

addHandlerClicked(btn.import, function(h, ...) {
    ## Change which region has focus
    visible(expand.import) <- FALSE
    visible(expand.variables) <- TRUE

    ## Extract the filename from inputs
    if(svalue(mapSource, index = TRUE) == 1) {
        map.filename <- svalue(mapInbuiltBrowse)
    } else {
        map.filename <- svalue(mapSourceBrowse)
    }
    
    ## Loading bar
    ### TO IMPLEMENT

    ## Import the map data
    map.data <- importMapData(map.filename)
})

## Variables Box


            
add(gv.match, frame.import, expand = TRUE)
add(gv.match, frame.variables)
add(gv.match, frame.merge)

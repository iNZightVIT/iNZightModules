library(gWidgets2)
devtools::load_all("h:/echome/inzightwork/package/iNZightMaps")
activeData <- read.csv("h:/echome/inzightwork/2017_10_09-mortalitydata/heart.csv")
activeData <- read.csv("h:/Documents/iNZightVIT/gapminder.csv")

################################################################################
                                        # General variables

## Variables used later on in the merge variable selection section
map.data <- NULL
map.vars <- NULL
nomatch.df <- data.frame(var.name = "")

## Have the above variables been populated with values from the chosen map?
stale.map.data <- TRUE

## Section heading font
font.header <- list(weight = "bold", size = 12, family = "normal")

################################################################################
                                        # Create window, etc.

## Overall Layout
w.match <- gwindow("Import Map File", width = 500, height = 750)
gv.match <- gvbox(container = w.match, expand = TRUE, fill = TRUE)
gv.match$set_borderwidth(15)

## Expandable boxes 
frame.import <- gframe(horizontal = FALSE)
group.import <- ggroup(spacing = 5)
group.import$set_borderwidth(10)
expand.import <- gexpandgroup(text = "Select Map", horizontal = FALSE)
font(expand.import)    <- font.header

frame.variables <- gframe(horizontal = FALSE)
group.variables <- ggroup(spacing = 5)
group.variables$set_borderwidth(10)
expand.variables <- gexpandgroup(text = "Select Merge Variables", horizontal = FALSE)
font(expand.variables) <- font.header

visible(expand.variables) <- FALSE
enabled(frame.variables) <- FALSE

btn.finish <- gbutton("Finish")
enabled(btn.finish) <- FALSE

## Add all frames to window
add(gv.match, frame.import, expand = TRUE)
add(gv.match, frame.variables, expand = TRUE)
add(gv.match, btn.finish, fill = "x")

################################################################################
                                        # Map Source Box
## Function definitions

### Read descriptions from ~/iNZightVIT/shapefiles/metadata
read.mapmetadata <- function() {
    metadata <- scan("h:/Documents/iNZightVIT/shapefiles/metadata",
                            what = rep("character", 3), fill = TRUE,
                            comment.char = ";", sep = "\t")
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

lbl.mapdesc <- gtext("Description: No description available.")

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
    if(!stale.map.data) {
        stale.map.data <- TRUE
        visible(expand.variables) <- FALSE
        enabled(frame.variables) <- FALSE
        enabled(btn.finish) <- FALSE
    }
})

### If user changes the map file, hide the variable merging section
### again to prevent dissonance between the two sections.
addHandlerSelectionChanged(mapInbuiltBrowse, function(h, ...) {
    if(!stale.map.data) {
        stale.map.data <<- TRUE
        visible(expand.variables) <- FALSE
        enabled(frame.variables) <- FALSE
        enabled(btn.finish) <- FALSE
    }
})

### Again, prevent dissonance between sections. Also insert the map
### description if it is present in the metadata
addHandlerSelect(mapInbuiltBrowse, function(h, ...) {
    if(!stale.map.data) {
        stale.map.data <<- TRUE
        visible(expand.variables) <- FALSE
        enabled(frame.variables) <- FALSE
        visible(lbl.allmatched) <- FALSE
        enabled(btn.finish) <- FALSE
    }

    chosen.filename <- paste(svalue(mapInbuiltBrowse), collapse = "/")
    chosen.desc <- mapdir.contents$description[which(mapdir.contents[, 1] == chosen.filename)]

    if(length(chosen.desc) > 0 && !is.na(chosen.desc)) {
        svalue(lbl.mapdesc) <- paste("Description:", chosen.desc)
    } else {
        svalue(lbl.mapdesc) <- "Description: No description available." 
    }
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

    ## Import the map - either a shapefile or rds
    map.data <<- iNZightMaps::retrieveMap(map.filename)
    map.vars <<- as.data.frame(map.data)[, !(colnames(map.data) %in% "geometry")]
    
    ## Only take variables in the shapefile that are unique to one
    ## region in the map file
    combobox.mapvars[] <- colnames(map.vars[, !(apply(map.vars, 2, anyDuplicated))])
    stale.map.data <<- FALSE

    ## Find the pair of variables with the highest number of matches
    best.vars <- findBestMatch(activeData, map.vars)
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

### Find the pair of variables from the map and data that have the
### highest number of matches. This function returns in form: (data
### variable, map variable)
findBestMatch <- function(data, map.data) {
    ## Eliminate variables that are associated with multiple regions
    ## in the map
    map.data <- map.data[, !(apply(map.data, 2, anyDuplicated))]

    ## Only count the number of unique matches (prevents situations
    ## where one particular matched region may have so many matches it
    ## obfuscates the fact that few other regions were matched with
    ## that variable)
    match.sums <- apply(data, 2, function(x) {
        apply(map.data, 2, function(y) {
            sum(y %in% unique(x))
        })
    })

    ## Find indices of the best matching pair and return
    best.match <- which(match.sums == max(match.sums), arr.ind = TRUE)[1,]
    best.match.vars <- c(colnames(match.sums)[best.match[2]], rownames(match.sums)[best.match[1]])
    best.match.vars
}

### Helper function that is called each time either combobox is
### changed. Updates the gtable of nonmatches.
cb.change <- function(h, ...) {
    enabled(table.nonmatched) <- FALSE

    data.var <- svalue(combobox.datavars)
    map.var <- svalue(combobox.mapvars)

    data.is.na <- is.na(activeData[, data.var])
    activeData2 <- activeData[!data.is.na, ]

    table.nonmatched[] <- unique(activeData2[, data.var, drop = FALSE]) 
    visible(table.nonmatched) <- !(unique(as.character(activeData2[, data.var])) %in% as.character(map.vars[, map.var]))

    enabled(table.nonmatched) <- TRUE

    if(any(visible(table.nonmatched))) {
        visible(lbl.allmatched) <- FALSE
        visible(lbl.blank) <- TRUE
    } else {
        visible(lbl.allmatched) <- TRUE
        visible(lbl.blank) <- FALSE
    }
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

lbl.allmatched <- glabel("All rows of data matched to a region!")
lbl.loading <- glabel("Loading map... Please wait...")
lbl.blank <- glabel("")

## Add to frame
add(frame.variables, group.variables, expand = TRUE)
add(group.variables, expand.variables, expand = TRUE)
addSpace(expand.variables, 15)
add(expand.variables, tbl.variables)
addSpace(expand.variables, 15)
add(expand.variables, lbl.allmatched)
add(expand.variables, lbl.loading)
add(expand.variables, lbl.blank)
#addSpace(expand.variables, 5)
add(expand.variables, lbl.nonmatchedtitle)
add(expand.variables, lbl.nonmatchedsubtitle)
addSpace(expand.variables, 5)
add(expand.variables, table.nonmatched, expand = TRUE)

#visible(lbl.nonmatchedtitle) <- FALSE
#visible(lbl.nonmatchedsubtitle) <- FALSE
visible(table.nonmatched) <- FALSE

visible(lbl.allmatched) <- FALSE
visible(lbl.loading) <- FALSE
visible(lbl.blank) <- FALSE

## Event handlers
addHandlerChanged(combobox.mapvars, handler = cb.change)
addHandlerChanged(combobox.datavars, handler = cb.change)


################################################################################
                                        # Finish Importing

addHandlerClicked(btn.finish, function(h, ...) {
    ## Join data to map

    ## Pass through the joined data, map name, etc.

})

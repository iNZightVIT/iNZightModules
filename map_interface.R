library(gWidgets2)
data(mtcars)

map.name <- "New Zealand DHBs (2012)"
if(nchar(map.name) > 50) map.name = paste0(substr(map.name, 1, 47), "...")
activeData <- mtcars

proj.vect <- c("Web Mercator" = 3857,
               "Mercator" = 0000,
               "World Geodetic System (WGS 84)" = 4326,
               "Lambert Azimuthal Equal-Area" = 9820,
               "British National Grid" = 27700,
               "European Terrestrial Reference System (ETRS 89)" = 4258,
               "North American Datum (NAD 83)" = 4269,
               "South American Datum (SIRGAS 2000)" = 4674,
               "Robinson" = 0000,
               "Plate Carree" =  0000,
               "Natural Earth" = 0000)
## http://www.radicalcartography.net/index.html?projectionref

has.multipleobs <- FALSE

################################################################################
                                        # Create window, etc.

## Overall Layout
w.match <- gwindow("Map Interface", width = 500, height = 700)

mainGrp <<- gvbox(spacing = 5, container = w.match, expand = TRUE)
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
expand.plotoptions <- gexpandgroup(text = "Plot Options", horizontal = FALSE)
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
lbl.mapname <- glabel(map.name)
btn.changemap <- gbutton("Change")

tbl.mapoptions[1, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.currentmap
tbl.mapoptions[1, 2:3, expand = TRUE, anchor = c(-1, 0)] <- lbl.mapname
tbl.mapoptions[1, 4] <- btn.changemap

lbl.mapproj <- glabel("Projection:")
combobox.mapproj <- gcombobox(names(proj.vect))

tbl.mapoptions[2, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.mapproj
tbl.mapoptions[2, 2] <- gcombobox(c("World", "Continent", "Country"))
tbl.mapoptions[2, 3:4, expand = TRUE] <- combobox.mapproj

add(expand.mapoptions, tbl.mapoptions, expand = TRUE, fill = TRUE)

## Plot Options

tbl.plotoptions <- glayout()

lbl.plottitle <- glabel("Plot Title:")
edit.plottitle <- gedit("")
checkbox.axislabels <- gcheckbox(text = "Axis Labels", checked = TRUE)
lbl.xaxis <- glabel("x-axis Label:")
lbl.yaxis <- glabel("y-axis Label:")
edit.xaxis <- gedit("Longitude")
edit.yaxis <- gedit("Latitude")
checkbox.datum <- gcheckbox("Grid Lines", checked = TRUE)

lbl.palette <- glabel("Map Theme:")
combobox.palette <- gcombobox(c("Default", "Dark"))

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
})

## Variable selection

lbl.maintitle <- glabel("Select Variable/s to Display")
lbl.mainsubtitle <- glabel("(Use Ctrl to select multiple variables)")
font(lbl.maintitle) <- list(weight = "bold", family = "normal", size = 10)

tbl.main <- glayout()

table.vars <- gtable(colnames(activeData), multiple = TRUE)
lbl.maptype <- glabel("Plot as:")
radio.maptype <- gradio(c("Regions", "Centroids"), horizontal = TRUE)
lbl.sizeselect <- glabel("Size by:")
combobox.sizeselect <- gcombobox(colnames(activeData))

lbl.timevariable <- glabel("Dataset has multiple observations for regions:")
radio.timevariable <- gradio(c("Separate", "Aggregate"), horizontal = TRUE)
lbl.timevariablechoice <- glabel("Plot:")
combobox.aggregation <- gcombobox(c("Average", "First Observation", "Median"))
combobox.separate <- gcombobox(c("Separate Plots", "Sparklines", "Barchart"))
separator.timevariable <- gseparator()

tbl.main[1, 1:3, expand = TRUE, fill = "both"] <- table.vars
tbl.main[2, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.maptype
tbl.main[2, 2, expand = TRUE, anchor = c(-1, 0), fill = "x"] <- radio.maptype
tbl.main[3, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.sizeselect
tbl.main[3, 2, expand = TRUE] <- combobox.sizeselect

tbl.main[4, 1:3, expand = TRUE] <- separator.timevariable
tbl.main[5, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.timevariable
tbl.main[5, 2, expand = TRUE, anchor = c(-1, 0)] <- radio.timevariable
tbl.main[6, 1, expand = TRUE, anchor = c(1, 0)] <- lbl.timevariablechoice
tbl.main[6, 2, expand = TRUE] <- combobox.aggregation
tbl.main[6, 2, expand = TRUE] <- combobox.separate

visible(lbl.maptype) <- FALSE
visible(radio.maptype) <- FALSE
visible(lbl.sizeselect) <- FALSE
visible(combobox.sizeselect) <- FALSE

visible(separator.timevariable) <- FALSE
visible(radio.timevariable) <- FALSE
visible(lbl.timevariable) <- FALSE
visible(lbl.timevariablechoice) <- FALSE
visible(combobox.aggregation) <- FALSE
visible(combobox.separate) <- FALSE

add(group.main, lbl.maintitle)
add(group.main, lbl.mainsubtitle)
add(group.main, tbl.main, expand = TRUE, fill = TRUE)

addHandlerSelectionChanged(table.vars, function(h, ...) {
    visible(lbl.maptype) <- TRUE
    visible(radio.maptype) <- TRUE

    if(has.multipleobs) {
        visible(separator.timevariable) <- TRUE
        visible(lbl.timevariable) <- TRUE
        visible(radio.timevariable) <- TRUE
        visible(lbl.timevariablechoice) <- TRUE
        visible(combobox.separate) <- TRUE
    }
})

addHandlerChanged(radio.maptype, function(h, ...) {
    if(svalue(radio.maptype, index = TRUE) == 1) {
        visible(lbl.sizeselect) <- FALSE
        visible(combobox.sizeselect) <- FALSE
    } else {
        visible(lbl.sizeselect) <- TRUE
        visible(combobox.sizeselect) <- TRUE
    }
})

addHandlerChanged(radio.timevariable, function(h, ...) {
    if(svalue(radio.timevariable, index = TRUE) == 1) {
        svalue(lbl.timevariablechoice) <- "Plot:"
        visible(combobox.aggregation) <- FALSE
        visible(combobox.separate) <- TRUE
    } else {
        svalue(lbl.timevariablechoice) <- "Aggregation:"
        visible(combobox.aggregation) <- TRUE
        visible(combobox.separate) <- FALSE
    }
})

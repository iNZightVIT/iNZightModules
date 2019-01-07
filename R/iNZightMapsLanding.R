##' iNZight Mapping Module Landing Window
##'
##' Opens a UI for visualising geographical data
##'
##' @title iNZight Maps Module Landing
##'
##' @author Daniel Barnett
##'
##' @export iNZightMapLanding
##' @exportClass iNZightMapLanding
iNZightMapLanding <- setRefClass(
  "iNZightMapLanding",
  fields = list(
    GUI         = "ANY",
    activeData = "ANY"
  ),
  methods = list(
    ## Function with all GUI specifications
    initialize = function(GUI) {
      initFields(GUI = GUI, activeData = NULL)
      
      activeData <<- GUI$getActiveData()
      
      is.mac <- Sys.info()["sysname"] == "Darwin"

      w <- gwindow("iNZightMaps", width = 400, height = 300, parent = GUI$win, visible = FALSE)
      gv <- gvbox(cont = w, expand = TRUE, fill = TRUE)
      gv$set_borderwidth(15)
      
      lbl <- glabel("Choose a map type")
      font(lbl) <- list(weight = "bold", size = 12, family = "normal")
      mapType <- gradio(c("Coordinate (latitude, longitude)",
                          "Regions (country, state, county, etc.)"))
      add(gv, lbl)
      add(gv, mapType)
      
      if (is.mac) {
        enabled(mapType) <- FALSE
        mac.message <- glabel("The region maps module is currently unavailable for Mac users.")
        add(gv, mac.message)
      }
      
      addSpace(gv, 10)
      
      lbl.coordmapvars <- glabel("Coordinate Variables")
      font(lbl.coordmapvars) <- list(weight = "bold", size = 12, family = "normal")
      add(gv, lbl.coordmapvars, anchor = c(-1, 0))

      ## latitude and longitude
      tbl <- glayout(homogeneous = FALSE)
      ii <- 1
      
      numVars <- numericVars()
      
      lbl <- "Latitude :"
      latVar <- gcombobox(c("", numVars))
      tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
      tbl[ii, 2:4, anchor = c(-1, 0), expand = TRUE] <- latVar
      ii <- ii + 1
      
      lbl <- "Longitude :"
      lonVar <- gcombobox(c("", numVars))
      tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
      tbl[ii, 2:4, anchor = c(-1, 0), expand = TRUE] <- lonVar
      ii <- ii + 1
      
      ## try find lat/lon columns in data set:
      vars <- numVars
      lat.match <- grep("lat", tolower(vars))
      if (length(lat.match)) svalue(latVar, index = TRUE) <- lat.match[1] + 1
      lon.match <- grep("lon", tolower(vars))
      if (length(lon.match)) svalue(lonVar, index = TRUE) <- lon.match[1] + 1
      
      if (length(lat.match) || length(lon.match) && !is.mac) {
        svalue(mapType, index = TRUE) <- 1
      } else {
        svalue(mapType, index = TRUE) <- 2
        visible(lbl.coordmapvars) <- FALSE
        visible(tbl) <- FALSE
      }

      addSpace(gv, 10)
      add(gv, tbl, expand = TRUE, fill = TRUE)
      addSpring(gv)
      
      ## switch between them using radio buttons
      addHandlerChanged(mapType, function(h, ...) {
        v <- svalue(mapType, index = TRUE)
        visible(tbl) <- v == 1
        visible(lbl.coordmapvars) <- v == 1
      })
      
      ## OK Button
      btnGrp <- ggroup(cont = gv)
      
      addSpring(btnGrp)
      okbtn <- gbutton("OK", expand = TRUE,
                       cont = btnGrp,
                       handler = function(h, ...) {
                         if (svalue(mapType, index = TRUE) == 1) {
                           if (svalue(latVar, TRUE) > 1 && svalue(lonVar, TRUE) > 1) {
                             # setVars(list(latitude = svalue(latVar),
                             #              longitude = svalue(lonVar)),
                             #         type = "points")
                             # initiateModule()
                             iNZightModules::iNZightMapMod$new(GUI, svalue(latVar), svalue(lonVar))
                             dispose(w)
                           } else {
                             gmessage("Please select a variable for latitude and longitude")
                           }
                         } else {
                           # if (svalue(mapLoc, TRUE) > 1 && svalue(locVar, TRUE) > 1) {
                           #   setVars(list(location = svalue(mapLoc),
                           #                location.var = svalue(locVar)),
                           #           type = "shape")
                           #   initiateModule(shape = TRUE)
                           #   dispose(w)
                           # } else {
                           #   gmessage("Please select a map location and variable")
                           # }
                           
                           iNZightModules::iNZightMap2Mod$new(GUI)
                           dispose(w)
                         }
                       })
      cnclBtn <- gbutton("Cancel", expand = TRUE, cont = btnGrp,
                         handler = function(h, ...) {
                           dispose(w)
                         })
      
      
      visible(w) <- TRUE
    },
    numericVars = function() {
      colnames(activeData)[sapply(activeData, is.numeric)]
    },
    characterVars = function() {
      colnames(activeData)[!sapply(activeData, is.numeric)]
    }
    )
)
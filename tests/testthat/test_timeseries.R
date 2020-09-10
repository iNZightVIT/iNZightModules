context("Time Series module")

# library(devtools)
# load_all()
# load_all('../iNZight')
# test(filter='regression')
# devtools::load_all("../iNZightTS"); devtools::load_all("../iNZight")

data(visitorsQ, package = 'iNZightTS')
data(visitorsA2, package = 'iNZightTS')

require(iNZight)
# try(ui$close(), T); load_all()
ui <- iNZGUI$new()
ui$initializeGui(visitorsQ)
on.exit(try(ui$close(), silent = TRUE))

test_that("Module opens and closes nicely", {
    expect_silent(mod <- iNZightTSMod$new(ui))
    expect_is(mod, "iNZightTSMod")
    expect_equal(mod$timeVar, "Date")
    expect_silent(mod$close())
    rm("mod")
})

mod <- iNZightTSMod$new(ui)
test_that("Axis limits sliders show the range of the data", {
    expect_equal(svalue(mod$xlimLower), "1998Q4")
    expect_equal(svalue(mod$xlimUpper), "2012Q1")
})

## Note: necessary to add timeouts because of the slider on timers
test_that("Lower limit is truncated to upper limit", {
    svalue(mod$xlimUpper) <- "2005Q4"
    Sys.sleep(0.3)
    expect_equal(
        tail(mod$xlimLower$get_items(), 1),
        "2003Q4"
    )
    svalue(mod$xlimUpper, index = TRUE) <- 1
    Sys.sleep(0.3)
    expect_false(enabled(mod$xlimLower))
    svalue(mod$xlimUpper) <- "2012Q1"
    Sys.sleep(0.3)
})

test_that("Upper limit is truncated to upper limit", {
    svalue(mod$xlimLower) <- "2005Q1"
    Sys.sleep(0.3)
    expect_equal(
        mod$xlimUpper$get_items()[1],
        "2007Q1"
    )
    svalue(mod$xlimLower, index = TRUE) <- length(mod$xlimLower$get_items())
    Sys.sleep(0.3)
    expect_false(enabled(mod$xlimUpper))
    svalue(mod$xlimLower) <- "1998Q4"
    Sys.sleep(0.3)
})


test_that("Model limit is same as view by default", {
    expect_true(svalue(mod$modLimEqual))
    expect_false(visible(mod$modLimLower))
    expect_false(visible(mod$modLimUpper))
})

test_that("Model limit can be specified manually", {
    svalue(mod$modLimEqual) <- FALSE
    expect_true(visible(mod$modLimLower))
    expect_true(visible(mod$modLimUpper))
})

test_that("Lower modelling limit is truncated to upper modelling limit", {
    svalue(mod$modLimUpper) <- "2005Q4"
    Sys.sleep(0.3)
    expect_equal(
        tail(mod$modLimLower$get_items(), 1),
        "2003Q4"
    )
    svalue(mod$modLimUpper, index = TRUE) <- 1
    Sys.sleep(0.3)
    expect_false(enabled(mod$modLimLower))
    svalue(mod$modLimUpper) <- "2012Q1"
    Sys.sleep(0.3)
})

test_that("Upper modelling limit is truncated to upper modelling limit", {
    svalue(mod$modLimLower) <- "2005Q1"
    Sys.sleep(0.3)
    expect_equal(
        mod$modLimUpper$get_items()[1],
        "2007Q1"
    )
    svalue(mod$modLimLower, index = TRUE) <- length(mod$modLimLower$get_items())
    Sys.sleep(0.3)
    expect_false(enabled(mod$modLimUpper))
    svalue(mod$modLimLower) <- "1998Q4"
    Sys.sleep(0.3)
})

test_that("Smoother display can be controlled", {
    expect_silent(svalue(mod$smootherChk) <- FALSE)
    expect_false(mod$show.smoother)
    expect_false(enabled(mod$smthSlider))
    expect_silent(svalue(mod$smootherChk) <- TRUE)
    expect_true(mod$show.smoother)
    expect_true(enabled(mod$smthSlider))
})

ui$close()


data(visitorsM2, package = 'iNZightTS')
test_that("Other datasets work too", {
    ui$initializeGui(visitorsM2)
    mod <- iNZightTSMod$new(ui)
})

ui$close()
# load_all("../iNZightTS")

test_that("Plots works", {
    ui$initializeGui(visitorsM2)
    expect_silent(mod <- iNZightTSMod$new(ui))
    Sys.sleep(1)
    mod$plottype <- 2
    expect_silent(mod$updatePlot())
    mod$plottype <- 3
    expect_silent(mod$updatePlot())
    mod$plottype <- 4
    expect_silent(mod$updatePlot())

    # change var
    tbl <- mod$mainGrp$children[[3]]$children[[1]]$children[[1]]$children[[2]]
    expect_silent(svalue(tbl, index = TRUE) <- 1:4)
    rdio <- mod$mainGrp$children[[3]]$children[[2]]$children[[2]]$children[[1]]
    expect_silent(svalue(rdio, index = TRUE) <- 2)
})

ui$close()

# try(ui$close(), T); load_all()
ui <- iNZGUI$new()
ui$initializeGui(visitorsA2)
on.exit(try(ui$close(), silent = TRUE))

test_that("Annual data disables unsupported plots", {
    expect_silent(mod <- iNZightTSMod$new(ui))
    on.exit(mod$close())
    expect_equal(
        mod$plotType$get_items(),
        c("Standard", "Decomposition")
    )

    expect_silent(mod$varSelect$set_selected(0:2))
    expect_equal(
        mod$compareChk$get_items(),
        c("Separate graphs")
    )
})

test_that("Annual data can be manually specified", {
    expect_silent(mod <- iNZightTSMod$new(ui))
    expect_silent(mod$timeVarType$set_index(2L))

    expect_silent(mod$timePeriodList$set_value("Year"))
    expect_silent(mod$timeFreqList$set_value("Yearly (1)"))
    expect_silent(mod$timeStartPeriod$set_value("1999"))
})

ui$close()

# devtools::load_all("../iNZightPlots")
# devtools::load_all("../iNZight")
# devtools::load_all()
ui <- iNZGUI$new()
ui$initializeGui(visitorsM2)
mod <- iNZightTSMod$new(ui)
Sys.sleep(1)

test_that("Interact button activated when plot supported", {
    expect_true(enabled(ui$plotToolbar$exportplotBtn))
    mod$plottype <- 2
    expect_silent(mod$updatePlot())
    expect_false(enabled(ui$plotToolbar$exportplotBtn))
    mod$plottype <- 3
    expect_silent(mod$updatePlot())
    expect_false(enabled(ui$plotToolbar$exportplotBtn))
    mod$plottype <- 4
    expect_silent(mod$updatePlot())
    expect_true(enabled(ui$plotToolbar$exportplotBtn))
})


covid <- iNZightTS:::covid
ui$close()

# devtools::load_all("../iNZightPlots")
# devtools::load_all("../iNZight")
# ui$close(); devtools::load_all()
ui <- iNZGUI$new()
ui$initializeGui(iNZightTS:::covid)
Sys.sleep(1)

test_that("User can choose between multiple non-numeric variables as time variable", {
    expect_silent(mod <- iNZightTSMod$new(ui))
    expect_equal(mod$timeVar, "Time")

    expect_silent(mod$varSelect$set_value("Daily_Deaths"))
    expect_silent(mod$varSelect$invoke_change_handler())
})

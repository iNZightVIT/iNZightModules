context("Time Series module")

# library(devtools)
# load_all()
# load_all('../iNZight')
# test(filter='regression')
# devtools::load_all("../iNZightTS"); devtools::load_all("../iNZight")

data(visitorsQ, package = 'iNZightTS')

try(ui$close(), T)
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

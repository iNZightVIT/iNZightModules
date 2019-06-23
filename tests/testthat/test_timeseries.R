context("Time Series module")

require(iNZight)

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
        "2004Q4"
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
        "2006Q1"
    )
    svalue(mod$xlimLower, index = TRUE) <- length(mod$xlimLower$get_items())
    Sys.sleep(0.3)
    expect_false(enabled(mod$xlimUpper))
    svalue(mod$xlimLower) <- "1998Q4"
    Sys.sleep(0.3)
})



context("Time Series module")


require(iNZight)

# library(devtools)
# load_all()
# load_all('../iNZight')
# test(filter='regression')

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
})

test_that("Axis limits sliders show the range of the data", {
    mod <- iNZightTSMod$new(ui)
    xr <- range(time(mod$tsObj$tsObj))
    # expect_equal(svalue(xlimLower), xr[1])
    # expect_equal(svalue(xlimUpper), xr[2])
})


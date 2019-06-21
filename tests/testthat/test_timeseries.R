context("Time Series module")

require(iNZight)
data(visitorsQ, package = 'iNZightTS')

ui <- iNZight:::iNZGUI$new()
ui$initializeGui(visitorsQ)
on.exit(try(ui$close(), silent = TRUE))

test_that("Module opens and closes nicely", {
    expect_silent(mod <- iNZightTSMod$new(ui))
    expect_is(mod, "iNZightTSMod")
    expect_equal(mod$timeVar, "Date")
    expect_silent(mod$close())
})

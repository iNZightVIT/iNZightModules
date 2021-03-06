context("Addon modules")

mod_dir <- file.path(getwd(), "modules")

# load_all("../../../iNZight")
# load_all("../..")

require(iNZight)
ui <- iNZGUI$new()
ui$initializeGui(iris)
on.exit(try(ui$close(), silent = TRUE))
Sys.sleep(2)

test_that("CustomModule super class works", {
    custmod <- CustomModule$new(ui)
    expect_is(custmod, "CustomModule")
    expect_silent(custmod$homeButton$invoke_change_handler())
})

test_that("Module loads", {
    mod <- getmodule(file.path(mod_dir, "DemoModule.R"))
    expect_is(mod, "environment")
    expect_equal(mod$name, "DemoModule")

    expect_output(
        modwin <- mod$module$new(ui),
        "Running new module"
    )
    expect_is(modwin, "Demo Module")
    expect_is(modwin, "CustomModule")

    expect_is(modwin$homeButton, "GButton")
    expect_output(
        modwin$homeButton$invoke_change_handler(),
        "Closing module"
    )
})

test_that("Directory of modules are loaded", {
    mods <- getModules(mod_dir)
    expect_is(mods, "list")
    expect_equal(length(mods), 3)
    expect_equal(
        names(mods),
        c("DemoModule", "DemoModule2", "DemoModule3")
    )
})

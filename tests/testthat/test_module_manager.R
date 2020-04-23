context("Module manager")

skip_if_offline()
td <- tempdir()
mod_dir <- file.path(td, "modules")
dir.create(mod_dir)
on.exit(unlink(mod_dir, TRUE))

# testing mod dir
test_mods <- file.path(getwd(), "modules")

# load_all("../../../iNZight")
# load_all("../..")

require(iNZight)
ui <- iNZGUI$new()
ui$initializeGui(iris)
on.exit(try(ui$close(), silent = TRUE), add = TRUE)
Sys.sleep(2)

test_that("Module list is empty", {
    ui$addonModuleDir <- mod_dir
    ui$menuBarWidget$defaultMenu() # update with modules ... shold be empty
    adv <- ui$menuBarWidget$menubar$menu_list$Advanced
    # should be ... Maps | Install | ...
    expect_equal(
        which(names(adv) == "maps"),
        which(names(adv) == "install") - 2
    )
})

test_that("Addon manager opens", {
    # load_all(); try(dispose(inst$win))
    expect_silent(inst <- ModuleManager$new(ui))

    expect_equal(
        inst$modules,
        data.frame(Name = "Modules will appear here once installed")
    )
    expect_equal(svalue(inst$upd_btn), "Update all")
    expect_false(enabled(inst$upd_btn))
    expect_false(enabled(inst$rmv_btn))

    expect_silent(dispose(inst$win))
})

test_that("Modules can be installed from a file", {
    # load_all(); try(dispose(inst$win))
    inst <- ModuleManager$new(ui)
    on.exit(dispose(inst$win))
    Sys.sleep(1)

    expect_silent(add_win <- InstallModules$new(inst))
    expect_is(add_win, "InstallModules")

    expect_silent(svalue(add_win$from) <- "Local file")
    expect_silent(
        svalue(add_win$filename) <- file.path(test_mods, "DemoModule.R")
    )
    expect_silent(add_win$installBtn$invoke_change_handler())

    expect_equal(inst$modules$Name, "Demo module")
    expect_true(enabled(inst$upd_btn))
})

test_that("Modules can be removed", {

})
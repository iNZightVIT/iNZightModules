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
        which(names(adv) == "manage") - 2
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
    expect_silent(add_win$inst_file_btn$invoke_change_handler())

    expect_equal(inst$modules$Name, "Demo module")
    expect_true(enabled(inst$upd_btn))
})

test_that("Modules can be removed", {
    # load_all(); try(dispose(inst$win))
    inst <- ModuleManager$new(ui, FALSE)
    on.exit(dispose(inst$win))
    Sys.sleep(1)

    expect_false(enabled(inst$rmv_btn))
    expect_silent(inst$m_tbl$set_cell(1, 1, TRUE))
    expect_true(enabled(inst$rmv_btn))
    expect_equal(svalue(inst$upd_btn), "Update selected")

    expect_silent(inst$rmv_btn$invoke_change_handler())
    expect_equal(
        inst$modules,
        data.frame(Name = "Modules will appear here once installed")
    )
    expect_equal(svalue(inst$upd_btn), "Update all")
    expect_false(enabled(inst$upd_btn))
    expect_false(enabled(inst$rmv_btn))
})

the_url <- "https://raw.githubusercontent.com/iNZightVIT/addons/dev/plot3DModule.R"
test_that("Modules can be installed from a URL", {
    # load_all(); try(dispose(inst$win))
    inst <- ModuleManager$new(ui, FALSE)
    on.exit(dispose(inst$win))
    Sys.sleep(1)

    expect_silent(add_win <- InstallModules$new(inst))
    expect_is(add_win, "InstallModules")

    expect_silent(svalue(add_win$from) <- "URL")
    expect_silent(svalue(add_win$url) <- the_url)
    expect_silent(add_win$inst_url_btn$invoke_change_handler())

    expect_equal(inst$modules$Name, "3D Plotting")
    expect_equal(basename(inst$modules$path), "plot3DModule.R")
    expect_true(enabled(inst$upd_btn))
    expect_equal(svalue(inst$upd_btn), "Update all")
    expect_false(enabled(inst$rmv_btn))

    expect_silent(inst$m_tbl$set_cell(1, 1, TRUE))
    expect_true(enabled(inst$rmv_btn))
    expect_equal(svalue(inst$upd_btn), "Update selected")

    expect_silent(inst$rmv_btn$invoke_change_handler())
    expect_equal(
        inst$modules,
        data.frame(Name = "Modules will appear here once installed")
    )
    expect_equal(svalue(inst$upd_btn), "Update all")
    expect_false(enabled(inst$upd_btn))
    expect_false(enabled(inst$rmv_btn))
})

test_that("Modules can be installed from addon repo", {
    # load_all(); try(dispose(add_win$installWin)); try(dispose(inst$win))
    inst <- ModuleManager$new(ui, FALSE)
    on.exit(dispose(inst$win))
    Sys.sleep(1)

    expect_silent(svalue(inst$repo, index = TRUE) <- 2L)
    expect_silent(add_win <- InstallModules$new(inst))
    expect_is(add_win, "InstallModules")
    expect_silent(svalue(add_win$from) <- "Addon repository")

    expect_false(enabled(add_win$inst_repo_btn))
    w <- which(add_win$add_tbl$get_frame()$Name == "Demo Module")
    expect_silent(add_win$add_tbl$set_cell(w, 1, TRUE))
    expect_true(enabled(add_win$inst_repo_btn))

    w <- which(add_win$add_tbl$get_frame()$Name == "3D Plotting")
    expect_silent(add_win$add_tbl$set_cell(w, 1, TRUE))

    expect_silent(add_win$inst_repo_btn$invoke_change_handler())
    expect_true(all(c("Demo Module", "3D Plotting") %in% inst$modules$Name))

    expect_true(enabled(inst$upd_btn))
    expect_equal(svalue(inst$upd_btn), "Update all")
    expect_false(enabled(inst$rmv_btn))
})

test_that("Modules can be updated from the addon repo", {
    ## Manually decrease version of 3d addon
    f3d <- readLines(file.path(mod_dir, "plot3DModule.R"))
    f3d[grep("@version", f3d, fixed = TRUE)] <- "#' @version 0"
    writeLines(f3d, file.path(mod_dir, "plot3DModule.R"))

    inst <- ModuleManager$new(ui, FALSE)
    on.exit(dispose(inst$win))
    Sys.sleep(1)
    expect_silent(svalue(inst$repo, index = TRUE) <- 2L)
    expect_equal(inst$m_tbl[2,5], "0")
    w <- which(inst$repo_addons[, "Name"] == "3D Plotting")
    expect_equivalent(inst$m_tbl[2,7], inst$repo_addons[w, "Version"])

    expect_silent(inst$m_tbl$set_cell(2, 1, TRUE))
    expect_true(enabled(inst$rmv_btn))
    expect_equal(svalue(inst$upd_btn), "Update selected")

    expect_silent(inst$upd_btn$invoke_change_handler())
    expect_equal(inst$m_tbl[2,5], inst$m_tbl[2,7])
})

test_that("Modules can be removed (from addon repo)", {
    # load_all(); try(dispose(add_win$installWin)); try(dispose(inst$win))
    inst <- ModuleManager$new(ui, FALSE)
    on.exit(dispose(inst$win))
    Sys.sleep(1)

    expect_silent(inst$m_tbl$set_cell(1, 1, TRUE))
    expect_silent(inst$m_tbl$set_cell(2, 1, TRUE))
    expect_true(enabled(inst$rmv_btn))
    expect_equal(svalue(inst$upd_btn), "Update selected")

    expect_silent(inst$rmv_btn$invoke_change_handler())
    expect_equal(
        inst$modules,
        data.frame(Name = "Modules will appear here once installed")
    )
    expect_equal(svalue(inst$upd_btn), "Update all")
    expect_false(enabled(inst$upd_btn))
    expect_false(enabled(inst$rmv_btn))
})

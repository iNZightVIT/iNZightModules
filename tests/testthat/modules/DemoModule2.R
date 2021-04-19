DemoModule2 <- setRefClass(
    "DemoModule2",
    contains = "CustomModule",
    fields = list(
        GUI = "ANY"
    ),
    methods = list(
        initialize = function(gui, name) {
            callSuper(gui,
                name = name,
                embedded = TRUE,
                uses_code_panel = TRUE
            )

            ## The main code for your module goes here,
            ## inside a top-level container called "mainGrp"
            label <- glabel("This is a second demo module. It writes code.",
                container = mainGrp)

            cat("Running code-writing module2\n")
        },
        close = function() {
            cat("Closing module 2\n")

            callSuper()
        }
    ),
    where = e
)

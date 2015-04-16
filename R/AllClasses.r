################
## All Classes
################

setClass("plrs",
         representation(coefficients = "numeric",
                        fitted.values = "numeric",
                        residuals = "numeric",
                        X = "matrix",
                        data = "list",
                        mdata = "list",
                        QP = "list",
                        test = "list",
                        cb = "list",
                        selected = "logical",
                        type = "character",
                        call.arg = "list")
)
setClass("plrs.select",
         representation(table = "matrix",
                        model = "plrs",
                        crit = "character")
)
setClass("plrs.series",
         representation(coefficients = "matrix",
                        effects = "list",
                        test = "matrix",
                        general = "matrix",
                        modelsType = "list",
                        call.arg = "list")
)



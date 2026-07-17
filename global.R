cloudcal <- "Loaded"

# Disable client-side error popups while keeping console stack traces
#options(warn = 2, shiny.fullstacktrace = TRUE)
options(shiny.devmode.verbose = FALSE)

get_os <- function(){
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
        os <- sysinf['sysname']
        if (os == 'Darwin')
        os <- "osx"
    } else { ## mystery machine
        os <- .Platform$OS.type
        if (grepl("^darwin", R.version$os))
        os <- "osx"
        if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    tolower(os)
}

tryCatch(options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx81920m")), error=function(e) NULL)

list.of.packages <- c("backports", "mgsub", "pbapply", "reshape2", "TTR", "dplyr", "ggtern",  "shiny", "rhandsontable", "random", "DT", "shinythemes", "broom", "shinyjs", "gridExtra", "dtplyr", "formattable", "XML", "corrplot", "scales", "rmarkdown", "markdown",  "httpuv", "stringi", "reticulate", "devtools", "randomForest", "caret", "data.table", "mvtnorm", "DescTools",  "doSNOW", "doParallel", "baseline",  "pls", "prospectr", "stringi", "ggplot2", "compiler", "itertools", "foreach", "grid", "nnet", "neuralnet", "xgboost", "reshape", "magrittr", "reactlog", "Metrics", "strip", "bartMachine", "arm", "brnn", "kernlab", "rBayesianOptimization", "magrittr", "smooth", "smoother", "ggrepel", "tibble", "purrr", "remotes", "tidyverse", "tools", "shinycssloaders", "openxlsx", "itraxR", "pbmcapply")
# Install a single package, falling back to its CRAN GitHub mirror when it is not
# available from CRAN itself (e.g. archived or platform-specific packages such as
# RDCOMClient). install.packages signals an unavailable package/dependency as a
# warning, so we install each package defensively and only fall back / report a
# failure if the package is still missing afterwards.
install_one <- function(pkg, type) {
    if (pkg %in% rownames(installed.packages())) return(invisible(TRUE))
    tryCatch(
        install.packages(pkg, repos = "http://cran.rstudio.com/", dep = TRUE, ask = FALSE, type = type),
        error = function(e) NULL, warning = function(w) NULL)
    if (!(pkg %in% rownames(installed.packages()))) {
        if (!requireNamespace("remotes", quietly = TRUE))
            try(install.packages("remotes", repos = "http://cran.rstudio.com/"), silent = TRUE)
        tryCatch(
            remotes::install_github(paste0("cran/", pkg)),
            error = function(e) message("Could not install package '", pkg, "': ", conditionMessage(e)))
    }
    invisible(pkg %in% rownames(installed.packages()))
}

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){
    # Don't let a benign install warning (e.g. an unavailable optional dependency)
    # abort setup via warn = 2; install each package defensively, then restore.
    old.warn <- getOption("warn")
    options(warn = 1)
    install.type <- if (get_os() == "linux") "source" else "binary"
    for (x in new.packages) install_one(x, install.type)
    options(warn = old.warn)
}

#if(!"caret" %in% installed.packages()[,"Package"]){
#    if(get_os()=="windows"){
#        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/raw/master/Packages/caret_6.0-93.1.zip", repos=NULL, type="win.binary"), error=function(e) tryCatch(remotes::install_github("leedrake5/caret", subdir="pkg/caret"), error=function(e) NULL))
#        } else if(get_os()!="windows"){
#            tryCatch(install.packages("https://github.com/leedrake5/CloudCal/raw/master/Packages/caret_6.0-93.1.tar.gz", type="source", repos=NULL), error=function(e) NULL)
#        }
#} else {
#    if(packageVersion("caret")!="6.0.93.1"){
#        if(get_os()!="windows"){
#        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/raw/master/Packages/caret_6.0-93.1.zip", repos=NULL, type="win.binary"), error=function(e) NULL)
#        } else if(get_os()!="windows"){
#            tryCatch(install.packages("https://github.com/leedrake5/CloudCal/raw/master/Packages/caret_6.0-93.1.tar.gz", type="source", repos=NULL), error=function(e) NULL)
#        }
#    }
#    }


#if(!"xrftools" %in% installed.packages()[,"Package"]){
#    tryCatch(devtools::install_github("paleolimbot/xrftools"), error=function(e) NULL)
#}



#if(packageVersion("ggplot2")!="2.2.1") devtools::install_version("ggplot2", version = "2.2.1", repos = "http://cran.us.r-project.org", checkBuilt=TRUE)

if("caret" %in% installed.packages()[,"Package"]==FALSE && get_os()=="windows"){
        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/raw/line_calculation/Packages/caret_6.0.93.1.zip", repos=NULL, type="win.binary"), error=function(e) tryCatch(remotes::install_github("leedrake5/caret", subdir="pkg/caret"), error=function(e) NULL))
    } else if ("caret" %in% installed.packages()[,"Package"]==FALSE && get_os()=="osx"){
        if(Sys.info()[["machine"]]=="arm64"){
            tryCatch(install.packages("https://github.com/leedrake5/CloudCal/raw/line_calculation/Packages/caret_6.0.93.1_arm64_macos.tgz", type="binary", repos=NULL), error=function(e) tryCatch(remotes::install_github("leedrake5/caret", subdir="pkg/caret"), error=function(e) NULL))
        } else {
            tryCatch(install.packages("https://github.com/leedrake5/CloudCal/raw/line_calculation/Packages/caret_6.0.93.1_x86_64_macos.tgz", type="binary", repos=NULL), error=function(e)  tryCatch(remotes::install_github("leedrake5/caret", subdir="pkg/caret"), error=function(e) NULL))
            }
    } else if ("caret" %in% installed.packages()[,"Package"]==FALSE && get_os()=="linux"){
        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/raw/line_calculation/Packages/caret_6.0.93.1.tar.gz", type="source", repos=NULL), error=function(e) tryCatch(remotes::install_github("leedrake5/caret", subdir="pkg/caret"), error=function(e) NULL))
    }

#if(packageVersion("caret")!="6.0.93.1" && get_os()=="windows"){
#        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/raw/master/Packages/caret_6.0.93.1.zip", repos=NULL, type="win.binary"), error=function(e) tryCatch(remotes::install_github("leedrake5/caret", subdir="pkg/caret"), error=function(e) NULL))
#    } else if (packageVersion("caret")!="6.0.93.1" && get_os()=="osx"){
#        if(Sys.info()[["machine"]]=="arm64"){
            #tryCatch(install.packages("https://github.com/leedrake5/CloudCal/raw/master/Packages/caret_6.0-93.1_arm64_macos.tgz", type="binary", repos=NULL), error=function(e) tryCatch(remotes::install_github("leedrake5/caret", subdir="pkg/caret"), error=function(e) NULL))
#        } else {
#            tryCatch(install.packages("https://github.com/leedrake5/CloudCal/raw/master/Packages/caret.6.0-93.1_x86_64_macos.tgz", type="binary", repos=NULL), error=function(e)  tryCatch(remotes::install_github("leedrake5/caret", subdir="pkg/caret"), error=function(e) NULL))
#            }
#    } else if (packageVersion("caret")!="6.0.93.1" && get_os()=="linux"){
#        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/raw/master/Packages/caret_6.0.93.1.tar.gz", type="source", repos=NULL), error=function(e) tryCatch(remotes::install_github("leedrake5/caret", subdir="pkg/caret"), error=function(e) NULL))
#    }

# rPDZ package management
# install.packages(url, repos=NULL) signals warnings (not errors) on failure,
# so tryCatch(..., error=...) misses the common case. Verify by checking the
# installed version afterward, and fall back to remotes::install_github if the
# hosted archive didn't land.
required_version <- "2.0.2"

installed_version <- function(pkg) {
    if (pkg %in% rownames(installed.packages())) as.character(packageVersion(pkg)) else NA_character_
}

# Draw a friendly notice in a renderPlot slot when there's nothing meaningful to
# plot (e.g. a covariance matrix with a single observation). Replaces a hard
# error with a clear message in the panel users were expecting.
singleSpectrumNotice <- function(message = "Only one spectrum loaded — variance cannot be calculated.\nAdd additional spectra to compute correlations.") {
    op <- par(mar = c(0, 0, 0, 0))
    on.exit(par(op))
    plot.new()
    text(0.5, 0.5, message, cex = 1.2, col = "#444444")
}

install_rPDZ <- function() {
    is_win <- .Platform$OS.type == "windows"
    url <- if (is_win) {
        paste0("https://github.com/leedrake5/CloudCal/raw/line_calculation/Packages/rPDZ_",
               required_version, ".zip")
    } else {
        paste0("https://github.com/leedrake5/CloudCal/raw/line_calculation/Packages/rPDZ_",
               required_version, ".tar.gz")
    }
    type <- if (is_win) "binary" else "source"
    try(install.packages(url, repos = NULL, type = type), silent = TRUE)

    if (!identical(installed_version("rPDZ"), required_version)) {
        message("Hosted rPDZ install did not yield ", required_version,
                "; falling back to remotes::install_github")
        if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
        remotes::install_github("leedrake5/rPDZ")
    }
}

if (!identical(installed_version("rPDZ"), required_version)) {
    if ("package:rPDZ" %in% search()) {
        try(detach("package:rPDZ", unload = TRUE), silent = TRUE)
    }
    if ("rPDZ" %in% rownames(installed.packages())) {
        try(remove.packages("rPDZ"), silent = TRUE)
    }
    install_rPDZ()
}


if("Peaks" %in% installed.packages()[,"Package"]==FALSE && get_os()=="windows"){
    tryCatch(install.packages("https://github.com/leedrake5/CloudCal/raw/line_calculation/Packages/Peaks_0.3.zip", repos=NULL, type="win.binary"), error=function(e) tryCatch(remotes::install_github("leedrake5/Peaks"), error=function(e) NULL))
} else if ("Peaks" %in% installed.packages()[,"Package"]==FALSE && get_os()!="windows"){
    tryCatch(install.packages("https://github.com/leedrake5/CloudCal/raw/line_calculation/Packages/Peaks_0.3.tar.gz", type="source", repos=NULL), error=function(e) tryCatch(remotes::install_github("leedrake5/Peaks"), error=function(e) NULL))
}

if("xrftools" %in% installed.packages()[,"Package"]==FALSE && get_os()=="windows"){
    tryCatch(install.packages("https://github.com/leedrake5/CloudCal/raw/line_calculation/Packages/xrftools_0.0.3.zip", repos=NULL, type="win.binary"), error=function(e) tryCatch(remotes::install_github("leedrake5/xrftools"), error=function(e) NULL))
} else if ("xrftools" %in% installed.packages()[,"Package"]==FALSE && get_os()!="windows"){
        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/raw/line_calculation/Packages/xrftools_0.0.3.tar.gz", type="binary", repos=NULL), error=function(e) tryCatch(remotes::install_github("leedrake5/xrftools"), error=function(e) NULL))
    }

if(packageVersion("xrftools")!="0.0.3" && get_os()=="windows"){
    tryCatch(install.packages("https://github.com/leedrake5/CloudCal/raw/line_calculation/Packages/xrftools_0.0.3.zip", repos=NULL, type="win.binary"), error=function(e) tryCatch(remotes::install_github("leedrake5/xrftools"), error=function(e) NULL))
} else if(packageVersion("xrftools")!="0.0.3" && get_os()!="windows"){
    tryCatch(install.packages("https://github.com/leedrake5/CloudCal/raw/line_calculation/Packages/xrftools_0.0.3.tar.gz", type="source", repos=NULL), error=function(e) tryCatch(remotes::install_github("leedrake5/xrftools"), error=function(e) NULL))
}

#sourceCpp("pdz.cpp")

library(rPDZ)
library(reactlog)
options(shiny.reactlog = TRUE)
#reactlog_enable()
shiny::devmode(TRUE)
options(shiny.fullstacktrace=TRUE)
###update packages
#update.packages(repos='http://cran.rstudio.com/', ask=FALSE)

###Old ggplot2
#devtools::install_version("ggplot2", version = "2.2.1", repos = "http://cran.us.r-project.org", checkBuilt=TRUE)


#sudo su - -c "R -e \"install.packages(c('shiny', 'pbapply', 'reshape2', 'TTR', 'dplyr', 'ggtern', 'ggplot2', 'shiny', 'rhandsontable', 'random', 'data.table', 'DT', 'shinythemes', 'Cairo', 'broom', 'shinyjs', 'gridExtra', 'dtplyr', 'formattable', 'XML', 'corrplot', 'scales', 'rmarkdown', 'markdown', 'randomForest', 'doMC', 'caret'), repos='http://cran.rstudio.com/')\""
library(tools)
library(grid)
library(shiny)
library(ggplot2)
library(pbapply)
library(reshape)
library(reshape2)
library(dplyr)
library(DT)
library(XML)
#library(gRbase)
library(reticulate)
library(Rcpp)
library(data.table)
library(compiler)
library(itertools)
library(foreach)
require(compiler)
library(doParallel)
library(parallel)
library(randomForest)
library(nnet)
library(neuralnet)
library(xgboost)
library(gridExtra)
library(magrittr)
library(Metrics)
tryCatch(library(taRifx), error=function(e) NULL)
library(strip)
tryCatch(library(mgsub), error=function(e) NULL)
#tryCatch(library(bartMachine), error=function(e) NULL)
tryCatch(library(arm), error=function(e) NULL)
tryCatch(library(brnn), error=function(e) NULL)
library(kernlab)
tryCatch(library(rBayesianOptimization), error=function(e) NULL)
tryCatch(library(xrftools), error=function(e) NULL)
#tryCatch(library(tidyverse))
library(magrittr)
library(Peaks)
enableJIT(3)
library(shinythemes)
library(rhandsontable)
library(broom)
library(shinyjs)
library(formattable)
library(markdown)
library(rmarkdown)
library(corrplot)
library(scales)
library(caret)
library(DescTools)
library(pls)
library(shinycssloaders)
tryCatch(library(pbmcapply), error=function(e) NULL)

#source("xgbTree.R")
#source("xgbDART.R")


options(digits=12)

my.cores <- if(parallel::detectCores()>=3){
    paste0(parallel::detectCores()-2)
} else if(parallel::detectCores()<=2){
    "1"
}

source('file_loading.R')
#tryCatch(source('file_loading.R'), error=function(e) source("https://raw.githubusercontent.com/leedrake5/CloudCal/line_calculation/file_loading.R"))

remove.factors = function(df) {
    for(varnum in 1:length(df)) {
        if("factor" %in% class(df[,varnum])) {
            df[varnum]=as.character(df[,varnum])
        }
    }
    return(df)
}

my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
my.min <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA)

layOut = function(...) {
    
    require(grid)
    
    x <- list(...)
    n <- max(sapply(x, function(x) max(x[[2]])))
    p <- max(sapply(x, function(x) max(x[[3]])))
    pushViewport(viewport(layout = grid.layout(n, p)))
    
    for (i in seq_len(length(x))) {
        print(x[[i]][[1]], vp = viewport(layout.pos.row = x[[i]][[2]],
        layout.pos.col = x[[i]][[3]]))
    }
}


generate_grid_multi <- function(bounds, init_points, init_grid_dt = NULL){
    DT_bounds <- data.table(Parameter = names(bounds), Lower = sapply(bounds,
    magrittr::extract2, 1), Upper = sapply(bounds, magrittr::extract2, 2), Type = sapply(bounds,
        class))
    setDT(init_grid_dt)
    if (nrow(init_grid_dt) != 0) {
        if (identical(names(init_grid_dt), DT_bounds[, Parameter]) ==
            TRUE) {
            init_grid_dt[, `:=`(Value, -Inf)]
        }
        else if (identical(names(init_grid_dt), c(DT_bounds[,
            Parameter], "Value")) == TRUE) {
            paste(nrow(init_grid_dt), "points in hyperparameter space were pre-sampled\n",
                sep = " ") %>% cat(.)
        }
        else {
            stop("bounds and init_grid_dt should be compatible")
        }
    }
    init_points_dt <- Matrix_runif(n = init_points, lower = DT_bounds[,
        Lower], upper = DT_bounds[, Upper]) %>% data.table(.) %T>%
        setnames(., old = names(.), new = DT_bounds[, Parameter]) %T>%
        {
            if (any(DT_bounds[, Type] == "integer")) {
                set(., j = DT_bounds[Type == "integer", Parameter],
                  value = round(magrittr::extract(., j = DT_bounds[Type ==
                    "integer", Parameter], with = FALSE)))
            }
            else {
                .
            }
        } %T>% magrittr::extract(., j = `:=`(Value, -Inf))
        
        result <- as.data.frame(init_points_dt)
        result <- result[,!colnames(result) %in% "Value"]
        return(result)
}

generate_grid_single <- function(bounds){
    as.data.frame(bounds)[1,]
}

generate_grid <- function(bounds, init_points, init_grid_dt = NULL){
    
    tryCatch(generate_grid_multi(bounds=bounds, init_points=init_points, init_grid_dt=init_grid_dt), error=function(e) generate_grid_single(bounds))

}


# Reset foreach's registered parallel backend to sequential and clear its
# accumulated globals. The usual one-liner (foreach:::.foreachGlobals) hard-errors
# if that internal is renamed or if foreach isn't loaded; this version guards
# both (exists() check + tryCatch) and no-ops safely instead of throwing. Defined
# here in global.R so any code that expects unregister_dopar() finds it on the
# search path -- a missing definition is what surfaced under debug = TRUE.
unregister_dopar <- function() {
    ns <- tryCatch(getNamespace("foreach"), error = function(e) NULL)
    if (!is.null(ns) && exists(".foreachGlobals", envir = ns)) {
        env <- get(".foreachGlobals", envir = ns)
        rm(list = ls(name = env), pos = env)
    }
    invisible(NULL)
}


BayesianOptimization <- function(FUN, bounds, init_grid_dt = NULL, init_points = 0,
                                 n_iter, acq = "ei", kappa = 2.576, eps = 0,
                                 kernel = list(type = "exponential", power = 2),
                                 verbose = TRUE, seed = NULL, debug = FALSE) {
  require(data.table)
  # Seed the RNG INSIDE the call so the random initialisation is reproducible
  # even in parallel workers (a set.seed() in the master process does not reach
  # %dopar%/doSNOW workers). Passing the same seed to each group's run makes
  # every group draw the identical init_points, so their Histories align.
  if (!is.null(seed)) set.seed(seed)
  # debug = TRUE is what BayesianOptimizationDebug() sets: errors propagate with
  # a full traceback instead of being caught and logged. debug = FALSE (default)
  # "jumps around" errors, recording them in Error_Log and continuing.
  DT_bounds <- data.table(Parameter = names(bounds),
                          Lower = sapply(bounds, `[[`, 1),
                          Upper = sapply(bounds, `[[`, 2),
                          Type = sapply(bounds, class))
  # NB: select columns with `..param_cols`. The `..` prefix binds only to the
  # token after it, so the earlier `..DT_bounds$Parameter` parsed as
  # `(..DT_bounds)$Parameter` and returned the name vector itself (which then
  # reached FUN as character args). That broke every round.
  param_cols <- DT_bounds$Parameter

  setDT(init_grid_dt)
  
  # Initialize error tracking dataframe
  Error_DT <- data.table()

  # Catch a step's error, log it to Error_DT (when `step` is given), and continue
  # with `fallback`. In debug mode ALSO print the failing step, message, and the
  # live call stack (a traceback) BEFORE recovering -- so you can walk a whole
  # run and see every failure instead of aborting at the first one. Both `expr`
  # and `fallback` are lazy, so `fallback` (e.g. a random Matrix_runif draw) is
  # only evaluated on failure and never disturbs the seeded stream.
  guard <- function(expr, step = NULL, fallback = NULL) {
    logit <- function(e) {
      if (!is.null(step))
        Error_DT <<- rbind(Error_DT, data.table(Round = i, Step = step,
                                                Message = conditionMessage(e)))
    }
    if (!isTRUE(debug))
      return(tryCatch(expr, error = function(e) { logit(e); fallback }))
    # debug: capture the call stack at the point of failure (via
    # withCallingHandlers, stack still intact), but PRINT in the outer handler
    # after the stack has unwound -- otherwise a cat() fired inside FUN's
    # capture.output() sink is swallowed. Then recover and continue.
    tb <- NULL
    tryCatch(
      withCallingHandlers(expr, error = function(e) { tb <<- sys.calls() }),
      error = function(e) {
        logit(e)
        cat(sprintf("\n[debug] Round %d step '%s' FAILED: %s\n",
                    i, if (is.null(step)) "FUN" else step, conditionMessage(e)))
        if (!is.null(tb) && length(tb) > 1) {
          cat("[debug] traceback:\n"); print(utils::head(tb, -1))
        }
        fallback
      }
    )
  }

  if (!is.null(init_grid_dt) && nrow(init_grid_dt) != 0) {
    if (identical(names(init_grid_dt), DT_bounds$Parameter)) {
      init_grid_dt[, Value := NA_real_]
    } else if (identical(names(init_grid_dt), c(DT_bounds$Parameter, "Value"))) {
      if (verbose) cat(nrow(init_grid_dt), "points pre-sampled\n")
    } else {
      stop("bounds and init_grid_dt should be compatible")
    }
  }

  # Initialize points
  init_points_dt <- Matrix_runif(n = init_points, lower = DT_bounds$Lower,
                                 upper = DT_bounds$Upper) %>%
    data.table() %>% setnames(DT_bounds$Parameter)

  # Round integers
  for (param in DT_bounds[Type == "integer", Parameter]) {
    init_points_dt[, (param) := round(get(param))]
  }
  init_points_dt[, Value := NA_real_]

  DT_history <- rbind(init_grid_dt, init_points_dt, fill=TRUE)
  DT_history[, Round := .I]

  Pred_list <- vector(mode = "list", length = nrow(DT_history) + n_iter)

  # Evaluation loop
  for (i in seq_len(nrow(DT_history) + n_iter)) {
    if (i <= nrow(DT_history)) {
      This_Par <- DT_history[i, ..param_cols]
    } else {
      # Fit GP model on good data
      valid_rows <- DT_history[!is.na(Value)]

      if (nrow(valid_rows) < 2) {
        warning("Not enough valid points for GP fitting; skipping iteration.")
        next
      }

      Par_Mat <- Min_Max_Scale_Mat(as.matrix(valid_rows[, ..param_cols]),
                                   DT_bounds$Lower, DT_bounds$Upper)

      GP <- guard(GPfit::GP_fit(X = Par_Mat, Y = valid_rows$Value, corr = kernel),
                  step = "GP_fit", fallback = NULL)

      if (is.null(GP)) next

      Next_Par <- guard(
        Utility_Max(DT_bounds, GP, acq, max(valid_rows$Value), kappa, eps) %>%
          Min_Max_Inverse_Scale_Vec(DT_bounds$Lower, DT_bounds$Upper) %>%
          setNames(DT_bounds$Parameter),
        step = "Utility_Max",
        # Name the fallback draw like the success path (x, y, ...); an unnamed
        # Matrix_runif matrix would otherwise land as V1/V2 columns via
        # rbind(fill=TRUE) and pollute History.
        fallback = setNames(as.numeric(Matrix_runif(1, DT_bounds$Lower, DT_bounds$Upper)),
                            param_cols))

      # Round integers
      for (param in DT_bounds[Type == "integer", Parameter]) {
        Next_Par[param] <- round(Next_Par[param])
      }
      This_Par <- as.data.table(as.list(Next_Par))
      DT_history <- rbind(DT_history, cbind(This_Par, Value=NA_real_, Round=i), fill=TRUE)
    }

    # Evaluate FUN but KEEP everything it emits so a failed round can be
    # explained: the list(Score, Pred) return value, its printed output, and any
    # warnings (muffled from the console on success, surfaced on failure). Only
    # messages stay suppressed (usually progress noise). In debug mode a thrown
    # error still propagates with a traceback via guard().
    .fun_out <- NULL
    .fun_log <- character(0)
    .warnbox <- new.env(parent = emptyenv()); .warnbox$w <- character(0)
    This_Time <- system.time(
      This_Score_Pred <- guard({
        .fun_log <- withCallingHandlers(
          suppressMessages(capture.output(
            .fun_out <- do.call(FUN, as.list(This_Par))
          )),
          warning = function(w) {
            .warnbox$w <- c(.warnbox$w, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        )
        .fun_out
      }, step = NULL, fallback = NULL)
    )
    .fun_warn <- .warnbox$w

    # Human-readable "param=value, ..." for the tested point (compact: integers
    # print whole, continuous to 5 sig figs), so the log shows WHERE we sampled.
    if (verbose) {
      pv <- unlist(This_Par)
      par_str <- paste(names(pv),
                       vapply(pv, function(v) format(v, trim = TRUE, digits = 5),
                              character(1)),
                       sep = "=", collapse = ", ")
    }

    if (is.null(This_Score_Pred) || is.null(This_Score_Pred$Score)) {
      DT_history[i, Value := NA_real_]
      # Explain WHY: NULL result vs a result that lacks a usable $Score, plus
      # what FUN actually returned (so you can see if the shape is wrong).
      reason <- if (is.null(This_Score_Pred)) {
        "FUN returned NULL (it caught/swallowed an error, or produced no value)"
      } else {
        sprintf("FUN returned no $Score (got %s%s)",
                paste(class(This_Score_Pred), collapse = "/"),
                if (!is.null(names(This_Score_Pred)))
                  paste0(" with names: ", paste(names(This_Score_Pred), collapse = ", "))
                else "")
      }
      Error_DT <- rbind(Error_DT, data.table(Round=i, Step="FUN", Message=reason))
      Pred_list[[i]] <- NULL
      if (verbose) {
        cat(sprintf("Round %d FAILED (%.2fs) | %s\n", i, This_Time["elapsed"], par_str))
        cat("   why:", reason, "\n")
        if (length(.fun_warn))
          cat(paste0("   FUN warning: ", utils::tail(.fun_warn, 5)), sep = "\n")
        if (length(.fun_log)) {
          cat("   FUN output (last lines):\n")
          cat(paste0("     ", utils::tail(.fun_log, 15)), sep = "\n"); cat("\n")
        }
      }
    } else {
      DT_history[i, Value := This_Score_Pred$Score]
      Pred_list[[i]] <- This_Score_Pred$Pred
      if (verbose)
        cat(sprintf("Round %d | Value = %0.4f (%.2fs) | %s\n",
                    i, This_Score_Pred$Score, This_Time["elapsed"], par_str))
    }
  }

  # Final best results
  valid_history <- DT_history[!is.na(Value)]
  Best_Par <- as.numeric(valid_history[which.max(Value), ..param_cols]) %>%
    setNames(DT_bounds$Parameter)
  Best_Value <- max(valid_history$Value)

  if (verbose) {
    cat("\nBest Parameters Found:\n")
    cat(paste(names(Best_Par), round(Best_Par,4), sep="=", collapse=", "), "\n")
    cat(sprintf("Best Value: %0.4f\n", Best_Value))
    if (nrow(Error_DT) > 0) {
      cat("\nSome evaluations encountered errors. See 'Error_Log' in results.\n")
    }
  }

  Result <- list(Best_Par = Best_Par,
                 Best_Value = Best_Value,
                 History = DT_history,
                 Pred = Pred_list,
                 Error_Log = Error_DT)

  return(Result)
}


# BayesianOptimizationDebug: identical to BayesianOptimization but with
# debug = TRUE, so evaluation errors propagate with a full traceback instead of
# being caught and logged to Error_Log. Kept as a thin wrapper so the two never
# drift apart -- every feature (seed, Error_Log, History shape) lives in one
# place. Returns the same result structure, so mergeOptRes() accepts either.
BayesianOptimizationDebug <- function(FUN, bounds, init_grid_dt = NULL, init_points = 0,
    n_iter, acq = "ei", kappa = 2.576, eps = 0,
    kernel = list(type = "exponential", power = 2), verbose = TRUE, seed = NULL) {
    BayesianOptimization(FUN = FUN, bounds = bounds, init_grid_dt = init_grid_dt,
        init_points = init_points, n_iter = n_iter, acq = acq, kappa = kappa,
        eps = eps, kernel = kernel, verbose = verbose, seed = seed, debug = TRUE)
}


# ---------------------------------------------------------------------------
# mergeOptRes: combine the History of two or more BayesianOptimization runs
# (or BayesianOptimizationDebug runs) so you can see which hyperparameter
# settings score well ACROSS instruments, not just on a single one.
#
# Each run samples different hyperparameter points, so points are first grouped
# into "similar" bins: integer-valued parameters are matched exactly, and
# continuous parameters are rounded to `digits` significant figures. Within a
# run, points falling in the same bin are collapsed with `agg` (max by default,
# matching BO's maximise-the-metric convention). Runs are then outer-joined on
# the binned hyperparameters, each run's metric column is renamed to its source
# label, and a trailing Mean column averages the metric across runs -- so the
# rows with the highest Mean are the settings that generalise best.
#
# Runs can be supplied via `...` (argument names become source labels) or as a
# single named list, e.g. both of these are equivalent:
#     mergeOptRes(InstrA = resA, InstrB = resB)
#     mergeOptRes(list(InstrA = resA, InstrB = resB))
#
# Works on output from BOTH BayesianOptimization (failed points = NA) and
# BayesianOptimizationDebug (failed points = -Inf): unevaluated points are
# dropped with a finite check either way. By default a malformed run is skipped
# with a warning ("jump around errors"); set debug = TRUE to let it raise so
# you get a traceback.
#
# Args:
#   ...          OPT_Res lists, or one named list of them (see above).
#   .list        Optional extra named list of runs, merged with `...`.
#   digits       Sig figs for binning continuous hyperparameters (default 3).
#   agg          Within-run collapse of a bin's metrics (default max).
#   min_sources  Keep only settings evaluated by at least this many runs
#                (default 1; set to the number of runs for shared-only).
#   value_col    Name of the metric column in History (default "Value").
#   sort         Sort rows by descending Mean (default TRUE).
#   debug        FALSE = skip bad runs with a warning; TRUE = raise (traceback).
#
# Returns a data.frame: [binned hyperparameters..., <one column per source>,
# N_Sources, Mean, SD], with Mean then SD last (SD = spread of the metric across
# sources; NA when only one source contributed).
# ---------------------------------------------------------------------------
mergeOptRes <- function(..., .list = NULL, digits = 3, agg = max,
                        min_sources = 1, value_col = "Value",
                        sort = TRUE, debug = FALSE) {

    runs <- c(list(...), .list)

    # Unwrap a single list-of-runs passed positionally (e.g. a result_list from
    # a parallel sweep). Unwrap when it holds AT LEAST ONE run -- using `all`
    # here meant a single failed/NULL parallel task left the list wrapped, so it
    # looked like "one run" and produced the misleading "needs at least two".
    is_run <- function(x) is.list(x) && !is.null(x[["History"]])
    if (length(runs) == 1 && is.list(runs[[1]]) && !is_run(runs[[1]]) &&
        length(runs[[1]]) > 0 && any(vapply(runs[[1]], is_run, logical(1)))) {
        runs <- runs[[1]]
    }

    n_supplied <- length(runs)
    n_valid    <- sum(vapply(runs, is_run, logical(1)))
    if (n_valid < 2)
        stop(sprintf(paste0("mergeOptRes() needs >= 2 valid OPT_Res runs; found %d valid ",
                            "of %d supplied. A valid run is a list with a $History element -- ",
                            "failed or NULL parallel tasks do not qualify (check which tasks ",
                            "errored in your sweep)."),
                     n_valid, n_supplied))

    # Source labels: use names where given, fill blanks with Source<i>.
    src <- names(runs)
    if (is.null(src)) src <- rep("", length(runs))
    blank <- !nzchar(src)
    src[blank] <- paste0("Source", seq_along(runs))[blank]
    src <- make.unique(src, sep = "_")

    guarded <- function(expr, label) {
        if (debug) return(force(expr))
        tryCatch(force(expr), error = function(e) {
            warning(sprintf("mergeOptRes: skipping run '%s': %s",
                            label, conditionMessage(e)), call. = FALSE)
            NULL
        })
    }

    # Pull one run's History down to param cols + a finite metric column.
    extract_one <- function(run, label) {
        if (!is_run(run))
            stop("not a valid OPT_Res (no $History) -- likely a failed or NULL task")
        hist <- as.data.frame(run[["History"]], stringsAsFactors = FALSE)
        if (!value_col %in% names(hist))
            stop(sprintf("no '%s' column in History", value_col))
        param_cols <- setdiff(names(hist), c(value_col, "Round"))
        if (length(param_cols) == 0)
            stop("no hyperparameter columns in History")
        v <- suppressWarnings(as.numeric(hist[[value_col]]))
        keep <- is.finite(v)
        out <- hist[keep, c(param_cols, value_col), drop = FALSE]
        out[[value_col]] <- v[keep]
        attr(out, "param_cols") <- param_cols
        out
    }

    tabs <- Map(function(run, label) guarded(extract_one(run, label), label),
                runs, src)
    ok <- !vapply(tabs, is.null, logical(1))
    tabs <- tabs[ok]; src <- src[ok]
    if (length(tabs) < 2)
        stop(sprintf(paste0("mergeOptRes(): only %d of %d valid run(s) had a usable History ",
                            "(a '%s' column with finite values). Nothing to merge."),
                     length(tabs), n_valid, value_col))

    # Hyperparameters common to every surviving run.
    param_cols <- Reduce(intersect, lapply(tabs, attr, "param_cols"))
    if (length(param_cols) == 0)
        stop("Runs share no common hyperparameter columns.")

    # Integer-valued params (matched exactly) vs continuous (rounded), decided
    # from the pooled values so binning is consistent across runs.
    pooled <- do.call(rbind, lapply(tabs, function(d) d[, param_cols, drop = FALSE]))
    is_int <- vapply(param_cols, function(p) {
        x <- suppressWarnings(as.numeric(pooled[[p]]))
        x <- x[is.finite(x)]
        length(x) > 0 && all(abs(x - round(x)) < 1e-8)
    }, logical(1))

    bin_key <- function(d) {
        for (p in param_cols) {
            x <- suppressWarnings(as.numeric(d[[p]]))
            d[[p]] <- if (is_int[[p]]) round(x) else signif(x, digits)
        }
        d
    }

    # Bin, then collapse within-run duplicate bins to one metric per bin.
    collapse_one <- function(d, label) {
        d <- bin_key(d[, c(param_cols, value_col), drop = FALSE])
        aggd <- aggregate(d[[value_col]], by = d[param_cols], FUN = agg)
        names(aggd)[ncol(aggd)] <- label
        aggd
    }

    binned <- Map(collapse_one, tabs, src)

    merged <- Reduce(function(a, b) merge(a, b, by = param_cols, all = TRUE), binned)

    val_mat <- as.matrix(merged[, src, drop = FALSE])
    merged[["N_Sources"]] <- rowSums(!is.na(val_mat))
    merged[["Mean"]] <- rowMeans(val_mat, na.rm = TRUE)
    # Spread of the metric across sources (NA where only one source contributed,
    # since SD is undefined for a single value) -- high SD = the setting works on
    # some instruments but not others, low SD = it generalises evenly.
    merged[["SD"]] <- apply(val_mat, 1, function(v) stats::sd(v, na.rm = TRUE))

    merged <- merged[merged[["N_Sources"]] >= min_sources, , drop = FALSE]
    if (sort) merged <- merged[order(-merged[["Mean"]]), , drop = FALSE]
    rownames(merged) <- NULL

    # Hyperparameters, per-source metrics, N_Sources, then Mean, SD last.
    merged[, c(param_cols, src, "N_Sources", "Mean", "SD"), drop = FALSE]
}


fluorescence.lines.directory <- if(file.exists("data/FluorescenceLines.csv")){
    "data/FluorescenceLines.csv"
} else if(!file.exists("data/FluorescenceLines.csv")){
    "https://raw.githubusercontent.com/leedrake5/CloudCal/master/data/FluorescenceLines.csv"
}

RDS_from_web <- function(url) {
  
  tempFile_location<- tempfile()
  download.file(url, tempFile_location)
  b <- readRDS(tempFile_location)
  file.remove(tempFile_location)
  b
}
######Load lines
lineLibrary <- tryCatch(readRDS("data/LineDefinitions.rdata"), error=function(e) RDS_from_web("https://github.com/leedrake5/CloudCal/raw/master/data/LineDefinitions.rdata"))
#temp <- tempfile()
fluorescence.lines <- lineLibrary$FluorescenceeLines
Wide <- lineLibrary$Wide
attach(lineLibrary$Tables)

line_strip <- function(elements){
    gsub("\\.(K\\.(alpha|beta)|L\\.(alpha|beta)|M\\.line|K12|L1)$", "", elements)
}
line_strip <- cmpfun(line_strip)

atomic_order <- function(element){
    subset(fluorescence.lines, Symbol==line_strip(element))$AtomicNumber
}
atomic_order <- cmpfun(atomic_order)


atomic_order_vector <- function(elements){
    # Length-preserving: a non-element (e.g. the new xrftools scatter_compton / scatter_rayleigh or escape
    # templates) yields numeric(0) from atomic_order. Map those to NA instead of letting unlist() silently
    # drop them -- dropping shortened the result and broke `frame$order <- atomic_order_vector(frame$element)`
    # ("replacement has N rows, data has M").
    vapply(elements, function(e){ o <- atomic_order(e); if(length(o) == 0) NA_real_ else as.numeric(o[1]) }, numeric(1))
}
atomic_order_vector <- cmpfun(atomic_order_vector)

order_elements <- function(elements){
    not.elements <- elements[!elements %in% spectralLines]
    elements <- elements[elements %in% spectralLines]
    
    
    elements.simp <- mgsub::mgsub(pattern=c(".K.alpha", ".K.beta", ".L.alpha", ".L.beta", ".M.line"), replacement=c("", "", "", "", ""), string=elements)
    
    element.frame.1 <- data.frame(Line=elements, Symbol=elements.simp)
    element.frame.2 <- merge(element.frame.1, fluorescence.lines[fluorescence.lines$Symbol %in% elements.simp, c("Symbol", "AtomicNumber")], by="Symbol")
    element.frame <- element.frame.2[order(element.frame.2$AtomicNumber),]
    
    
    elements <- as.vector(element.frame$Line)
    
    return(c(elements[complete.cases(elements)], not.elements))
}

order_elements_simple <- function(elements){

    elements.simp <- mgsub::mgsub(pattern=c(".K.alpha", ".K.beta", ".L.alpha", ".L.beta", ".M.line"), replacement=c("", "", "", "", ""), string=elements)
    
    element.frame.1 <- data.frame(Line=elements, Symbol=elements.simp)
    element.frame.2 <- merge(element.frame.1, fluorescence.lines[fluorescence.lines$Symbol %in% elements.simp, c("Symbol", "AtomicNumber")], by="Symbol")
    element.frame <- element.frame.2[order(element.frame.2$AtomicNumber),]
    
    
    elements <- as.vector(element.frame$Line)
    
    return(c(elements[complete.cases(elements)]))
}

element_line_pull <- function(element.line){
    element <- strsplit(x=element.line, split="\\.")[[1]][1]
    destination <- strsplit(x=element.line, split="\\.")[[1]][2]
    distance <- strsplit(x=element.line, split="\\.")[[1]][3]
    data.frame(ElementLine=element.line, Element=element, Orbital=destination, Line=distance, stringsAsFactors=FALSE)
}
element_line_pull <- cmpfun(element_line_pull)


Hodder.v.old <- function(y)
{
    
    n<-length(y)
    
    for(i in 1:(n-1)) {
        y[i] <- y[i+1] - y[i]
        y[1:(n-1)]
        y <- abs(y)
    }
    y <- c(0, y[1:(n-1)])
    
    return(y)
}
Hodder.v.old <- cmpfun(Hodder.v.old)


Hodder.v <- function(y)
{
    
    n<-length(y)
    
    for(i in 1:(n-1)) {
        y[i] <- (y[i+1] - y[i])/211
        y[1:(n-1)]
        
    }
    y <- c(0, y[1:(n-1)])
    
    return(y)
}
Hodder.v <- cmpfun(Hodder.v)


int_to_unit <- function (x, adjustment=2^32) {
    x <- as.numeric(x)
    signs <- sign(x)
    x[signs < 0] <- x[signs < 0] + adjustment
    x
}
int_to_unit <- cmpfun(int_to_unit)



recognize_fold <- function(spectrum){
    index <- which(Hodder.v(spectrum$CPS)<(-0.5))
    index[index %in% seq(41, 2040, 1)]
    
}
recognize_fold <- cmpfun(recognize_fold)


unfold_simple <- function(spectrum){
    
    index.seq <- recognize_fold(spectrum)
    
    spectrum$CPSNew <- ifelse(as.numeric(rownames(spectrum)) %in% index.seq, spectrum$CPS+211, spectrum$CPS)
    
    data.frame(Spectrum=spectrum$Spectrum, Energy=spectrum$Energy, CPS=spectrum$CPSNew, stringsAsFactors=FALSE)
    
}
unfold_simple <- cmpfun(unfold_simple)


unfold <- function(spectrum){
    
    first_unfold <- unfold_simple(spectrum)
    second_unfold <- unfold_simple(first_unfold)
    third_unfold <- unfold_simple(second_unfold)
    fourth_unfold <- unfold_simple(third_unfold)
    fourth_unfold
    
}
unfold <- cmpfun(unfold)


correlationcoeff <- function(x,y){
    n = length(x)
    yl = length(y)
    xy = x * y
    sx = sum(x)
    sy = sum(y)
    sxy = sum(xy)
    x2 = x ^ 2
    y2 = y ^ 2
    sx2 = sum(x2)
    sy2 = sum(y2)
    r = ((n*sxy) - (sx * sy)) / (sqrt((((n*sx2)-(sx^2)) * ((n*sy2)-(sy^2)))))
    return(r)
}

cal.lmsummary <-function(lm.object){
    res<-c(paste(as.character(summary(lm.object)$call),collapse=" "),
    length(lm.object$model),
    summary(lm.object)$r.squared,
    summary(lm.object)$adj.r.squared,
    summary(lm.object)$fstatistic,
    pf(summary(lm.object)$fstatistic[1],summary(lm.object)$fstatistic[2],summary(lm.object)$fstatistic[3],lower.tail=FALSE))
    names(res)<-c("Call","n", "R2","Adj. R2",
    "F-statistic","numdf","dendf","p-value")
    return(res)}
cal.lmsummary <- cmpfun(cal.lmsummary)

cal.lmsummary2 <-function(element.model.list, model.name){
    lm.object <- element.model.list[[2]]
    n <- length(na.omit(element.model.list[[1]]$StandardsUsed))
    res<-c(model.name,
    n,
    round(summary(lm.object)$r.squared, 2),
    round(summary(lm.object)$adj.r.squared, 2),
    round(summary(lm.object)$fstatistic, 2),
    round(pf(summary(lm.object)$fstatistic[1],summary(lm.object)$fstatistic[2],summary(lm.object)$fstatistic[3],lower.tail=FALSE), 2))
    names(res)<-c("Model","n", "R2","Adj. R2",
    "F-statistic","numdf","dendf","p-value")
    return(res)}
cal.lmsummary2 <- cmpfun(cal.lmsummary2)

calEvaluationSummary <- function(calList){
    
    model.list <- pblapply(names(calList), function(x) data.frame(t(cal.lmsummary2(element.model.list=calList[[x]], model.name=x)), stringsAsFactors=FALSE))
    model.frame <- as.data.frame(rbindlist(model.list), stringsAsFactors=FALSE)
}
calEvaluationSummary <- cmpfun(calEvaluationSummary)


val.lmsummary <-function(lm.object){
    res<-c(paste(as.character(summary(lm.object)$call),collapse=" "),
    lm.object$coefficients[1],
    lm.object$coefficients[2],
    length(lm.object$model),
    summary(lm.object)$coefficients[2,2],
    summary(lm.object)$r.squared,
    summary(lm.object)$adj.r.squared,
    summary(lm.object)$fstatistic,
    pf(summary(lm.object)$fstatistic[1],summary(lm.object)$fstatistic[2],summary(lm.object)$fstatistic[3],lower.tail=FALSE))
    names(res)<-c("Call","Intercept","Slope","n","Slope SE","R2","Adj. R2",
    "F-statistic","numdf","dendf","p-value")
    return(res)}
val.lmsummary <- cmpfun(val.lmsummary)





file.0 <- function(file) {
    if (length(file) > 0)
    {
    return(file)
    }else{
        return(levels(file))
    }
}
file.0 <- cmpfun(file.0)


is.0 <- function(cps, file) {
    file.0 <- function(file) {
        if (length(file) > 0)
        {
            return(file)
        }else{
            return(levels(file))
        }
    }
    if (length(cps) > 0)
    {
        hope <-data.frame(cps, file.0(file))
        return(hope)
    } else {
        empty <- rep(0, length(file.0(file)))
        framed <- data.frame(empty, file.0(file))
        return(framed)
    }
}
is.0 <- cmpfun(is.0)


dt_options <- reactive({
    # dynamically create options for `aoColumns` depending on how many columns are selected.
    toggles <- lapply(1:length(input$show_vars), function(x) list(bSearchable = F))
    # for `species` columns
    toggles[[length(toggles) + 1]] <- list(bSearchable = T)
    
    list(
    aoColumns = toggles,
    bFilter = 1, bSortClasses = 1,
    aLengthMenu = list(c(10,25,50, -1), list('10','25', '50', 'Todas')),
    iDisplayLength = 10
    )
})


ifrm <- function(obj, env = globalenv()) {
    obj <- deparse(substitute(obj))
    if(exists(obj, envir = env)) {
        rm(list = obj, envir = env)
    }
}

ifrm <- cmpfun(ifrm)


lmp <- function (modelobject) {
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
}
lmp <- cmpfun(lmp)


lm_eqn.old <- function(df){
    m <- lm(y ~ x, df);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
    list(a = format(coef(m)[1], digits = 2),
    b = format(coef(m)[2], digits = 2),
    r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));
}
lm_eqn.old <- cmpfun(lm_eqn.old)


lm_eqn = function(m) {
    
    l <- list(a = as.numeric(format(coef(m)[1], digits = 2)),
    b = as.numeric(format(abs(coef(m)[2]), digits = 2)),
    r2 = format(summary(m)$r.squared, digits = 3));
    
        eq <- substitute(italic(C)[i] == a + b %.% italic(I)[i]*","~~italic(r)^2~"="~r2,l)
  
    
    as.character(as.expression(eq));
}
lm_eqn <- cmpfun(lm_eqn)

lm_eqn_simple <- function(df){
    m <- lm(y ~ x, df);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
         list(a = format(unname(coef(m)[1]), digits = 2),
              b = format(unname(coef(m)[2]), digits = 2),
             r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));
}

lm_eqn_poly = function(m) {
    
    l <- list(a = as.numeric(format(coef(m)[1], digits = 2)),
    b = as.numeric(format(abs(coef(m)[2]), digits = 2)),
    c = as.numeric(format(abs(coef(m)[3]), digits = 2)),
    r2 = format(summary(m)$r.squared, digits = 3));
    
        eq <- substitute(italic(C)[i] == a + c %.% italic(I)[i]^2 + b %.% italic(I)[i]*","~~italic(r)^2~"="~r2,l)
   
    
    as.character(as.expression(eq));
}
lm_eqn_poly <- cmpfun(lm_eqn_poly)


lm_eqn_val = function(m) {
    
    l <- list(a = as.numeric(format(coef(m)[1], digits = 2)),
    b = as.numeric(format(abs(coef(m)[2]), digits = 2)),
    r2 = format(summary(m)$r.squared, digits = 3));
    
        eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
   
    
    as.character(as.expression(eq));
}
lm_eqn_val <- cmpfun(lm_eqn_val)


numericInput2<-function (inputId, label, value = "",...)
{
    div(style="display:inline-block",
    tags$label(label, `for` = inputId),
    tags$input(id = inputId, type = "text", value = value,...))
}
numericInput2 <- cmpfun(numericInput2)


numericInputRow<-function (inputId, label, min, max,  value = "")
{
    div(style="display:inline-block",
    tags$label(label, `for` = inputId),
    tags$input(id = inputId, type = "text", value = value, class="input-mini", width='20%'))
}
numericInputRow <- cmpfun(numericInputRow)

fitresid_resid <- function(model){
    p1<-ggplot(model, aes(as.vector(.fitted), as.vector(.resid)))+geom_point()
    p1<-p1+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
    p1<-p1+xlab("Fitted values")+ylab("Residuals")
    p1<-p1+ggtitle("Residual vs Fitted Plot")+theme_light()
    return(p1)
}

qq <- function(model){
    p2 <- ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm = TRUE)
    p2 <- p2+geom_abline()+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
    p2 <- p2+ggtitle("Normal Q-Q")+theme_bw()
    return(p2)
}

scale_location <- function(model){
    p3<-ggplot(model, aes(as.vector(.fitted), sqrt(abs(as.vector(.stdresid)))))+geom_point(na.rm=TRUE)
    p3<-p3+stat_smooth(method="loess", na.rm = TRUE)+xlab("Fitted Value")
    p3<-p3+ylab(expression(sqrt("|Standardized residuals|")))
    p3<-p3+ggtitle("Scale-Location")+theme_light()
    return(p3)
}

cooksdist_bar <- function(model){
    p4<-ggplot(model, aes(seq_along(as.vector(.cooksd)), as.vector(.cooksd)))+geom_bar(stat="identity", position="identity")
    p4<-p4+xlab("Obs. Number")+ylab("Cook's distance")
    p4<-p4+ggtitle("Cook's distance")+theme_light()
    return(p4)
}

resid_leverage <- function(model){
    p5<-ggplot(model, aes(as.vector(.hat), as.vector(.stdresid)))+geom_point(aes(size=as.vector(.cooksd)), na.rm=TRUE)
    p5<-p5+stat_smooth(method="loess", na.rm=TRUE)
    p5<-p5+xlab("Leverage")+ylab("Standardized Residuals")
    p5<-p5+ggtitle("Residual vs Leverage Plot")
    p5<-p5+scale_size_continuous("Cook's Distance", range=c(1,5))
    p5<-p5+theme_light()+theme(legend.position="bottom")
    return(p5)
}

cooksdist_leverage <- function(model){
    p6<-ggplot(model, aes(as.vector(.hat), as.vector(.cooksd)))+geom_point(na.rm=TRUE)+stat_smooth(method="loess", na.rm=TRUE)
    p6<-p6+xlab("Leverage")+ylab("Cook's Distance")
    p6<-p6+ggtitle("Cook's dist vs Leverage")
    p6<-p6+geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed")
    p6<-p6+theme_light()
    return(p6)
}

diagPlot<-function(model){
    p1 <- fitresid_resid(model)
    p2 <- qq(model)
    p3 <- scale_location(model)
    p4 <- cooksdist_bar(model)
    p5 <- resid_leverage(model)
    p6 <- cooksdist_leverage(model)
    
    return(list(rvfPlot=p1, qqPlot=p2, sclLocPlot=p3, cdPlot=p4, rvlevPlot=p5, cvlPlot=p6))
}
diagPlot <- cmpfun(diagPlot)


rbind.match.columns <- function(input1, input2) {
    n.input1 <- ncol(input1)
    n.input2 <- ncol(input2)
    
    if (n.input2 < n.input1) {
        TF.names <- which(names(input2) %in% names(input1))
        column.names <- names(input2[, TF.names])
    } else {
        TF.names <- which(names(input1) %in% names(input2))
        column.names <- names(input1[, TF.names])
    }
    
    return(rbind(input1[, column.names], input2[, column.names]))
}
rbind.match.columns <- cmpfun(rbind.match.columns)


strip_glm <- function(cm) {
    cm$y = c()
    cm$model = c()
    
    cm$residuals = c()
    cm$fitted.values = c()
    cm$effects = c()
    cm$qr$qr = c()
    cm$linear.predictors = c()
    cm$weights = c()
    cm$prior.weights = c()
    cm$data = c()
    
    
    cm$family$variance = c()
    cm$family$dev.resids = c()
    cm$family$aic = c()
    cm$family$validmu = c()
    cm$family$simulate = c()
    attr(cm$terms,".Environment") = c()
    attr(cm$formula,".Environment") = c()
    
    cm
}
strip_glm <- cmpfun(strip_glm)


strip_env <- function(cm) {
    attr(cm$terms,".Environment") = c()
    attr(cm$formula,".Environment") = c()
    
    cm
}
strip_env <- cmpfun(strip_env)


merge_Sum <- function(.df1, .df2, .id_Columns, .match_Columns){
    merged_Columns <- unique(c(names(.df1),names(.df2)))
    merged_df1 <- data.frame(matrix(nrow=nrow(.df1), ncol=length(merged_Columns)), stringsAsFactors=FALSE)
    names(merged_df1) <- merged_Columns
    for (column in merged_Columns){
        if(column %in% .id_Columns | !column %in% names(.df2)){
            merged_df1[, column] <- .df1[, column]
        } else if (!column %in% names(.df1)){
            merged_df1[, column] <- .df2[match(.df1[, .match_Columns],.df2[, .match_Columns]), column]
        } else {
            df1_Values=.df1[, column]
            df2_Values=.df2[match(.df1[, .match_Columns],.df2[, .match_Columns]), column]
            df2_Values[is.na(df2_Values)] <- 0
            merged_df1[, column] <- df1_Values + df2_Values
        }
    }
    return(merged_df1)
}
merge_Sum <- cmpfun(merge_Sum)




parallel_prediction_stats <-function(object,newdata, ...)
{
    
    cl <- makePSOCKcluster(as.numeric(my.cores))
    registerDoParallel(cl)
    num_splits<-as.numeric(my.cores)
    split_testing<-sort(rank(1:nrow(newdata))%%num_splits)
    predictions<-foreach(i=unique(split_testing),
    .combine=c,.packages=c("stats")) %dopar% {
        as.numeric(predict(object,newdata=newdata[split_testing==i,], ...))
    }
    stopCluster(cl)
    predictions
}
parallel_prediction_stats <- cmpfun(parallel_prediction_stats)



parallel_prediction_caret <-function(object,newdata, ...)
{
    
    cl <- makePSOCKcluster(as.numeric(my.cores))
    registerDoParallel(cl)
    num_splits<-as.numeric(my.cores)
    split_testing<-sort(rank(1:nrow(newdata))%%num_splits)
    predictions<-foreach(i=unique(split_testing),
    .combine=c,.packages=c("caret")) %dopar% {
        as.numeric(predict(object,newdata=newdata[split_testing==i,], ...))
    }
    stopCluster(cl)
    predictions
}
parallel_prediction_caret <- cmpfun(parallel_prediction_caret)


GG_save_pdf = function(list, filename) {
    #start pdf
    pdf(filename)
    
    #loop
    for (p in list) {
        print(p)
    }
    
    #end pdf
    dev.off()
    
    invisible(NULL)
}
GG_save_pdf <- cmpfun(GG_save_pdf)


###Train Functions

pull_test <- function(a.vector, a.value.position){
    
    scaled <- scale(a.vector)[,1]
    
    value <- scaled[a.value.position]
    scale.vector <- scaled[-a.value.position]
    
    ZScore <- (value-mean(scale.vector))/sd(scale.vector)
    pvalue <- pnorm(-abs(ZScore))
    is.sig <- pvalue < 0.05
    
    data.frame(Value=a.vector[a.value.position], ZScore=ZScore, pvalue=pvalue, Sig=is.sig, stringsAsFactors=FALSE)
}
pull_test <- cmpfun(pull_test)


Z_frame <- function(a.vector){
    as.data.frame(data.table::rbindlist(lapply(seq(1, length(a.vector), 1), function(x) pull_test(a.vector, x)), use.names=TRUE, fill=TRUE))
}
Z_frame <- cmpfun(Z_frame)


Z_choose <- function(a.vector){
    
    full <- Z_frame(a.vector)
    full[full$Sig,]
    
}
Z_choose <- cmpfun(Z_choose)

variable_select_xrf <- function(intensities, values, analyte){
    
    control <- trainControl(method="cv", number=5)
    seed <- 7
    metric <- "RMSE"
    set.seed(seed)
    
    cal.table <- data.frame(intensities, Concentration=values[,analyte], stringsAsFactors=FALSE)
    fit.lm <- train(Concentration~., data=cal.table, method="lm", metric=metric, preProc=c("center", "scale"), trControl=control)
    importance <- varImp(fit.lm, scale=FALSE)
    importance.frame <- as.data.frame(importance$importance, stringsAsFactors=FALSE)
    elements <- rownames(importance$importance)
    elements[as.numeric(rownames(Z_choose(importance.frame$Overall)))]
    
}
variable_select_xrf <- cmpfun(variable_select_xrf)


variable_select_short_xrf <- function(importance){
    importance.frame <- as.data.frame(importance$importance, stringsAsFactors=FALSE)
    elements <- rownames(importance$importance)
    elements[as.numeric(rownames(Z_choose(importance.frame$Overall)))]
}
variable_select_short_xrf <- cmpfun(variable_select_short_xrf)


black.diamond.directory <- if(file.exists("data/blackdiamond.csv")){
    "data/blackdiamond.csv"
} else if(!file.exists("data/blackdiamond.csv")){
    "https://raw.githubusercontent.com/leedrake5/CloudCal/master/data/blackdiamond.csv"
}

black.diamond.melt.directory <- if(file.exists("data/blackdiamondmelt.csv")){
    "data/blackdiamondmelt.csv"
} else if(!file.exists("data/blackdiamondmelt.csv")){
    "https://raw.githubusercontent.com/leedrake5/CloudCal/master/data/blackdiamondmelt.csv"
}

black.diamond <- read.csv(black.diamond.directory, header=FALSE, sep=",")
black.diamond.melt <- read.csv(file=black.diamond.melt.directory, sep=",")






elementGaussianKalpha <- function(element, data, method="sum", buffer=0.02) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy < elementLine[6][1,]-buffer | data$Energy > elementLine[5][1,]+buffer), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, "K-alpha", sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
elementGaussianKalpha <- cmpfun(elementGaussianKalpha)

elementFirstKalpha <- function(element, data, method="sum", buffer=0.02) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy < elementLine[5][1,]-buffer | data$Energy > elementLine[5][1,]+buffer), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, "K-alpha", sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
elementFirstKalpha <- cmpfun(elementFirstKalpha)

elementSecondKalpha <- function(element, data, method="sum", buffer=0.02) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy < elementLine[6][1,]-buffer | data$Energy > elementLine[6][1,]+buffer), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, "K-alpha", sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
elementSecondKalpha <- cmpfun(elementSecondKalpha)

elementSplitKalpha <- function(element, data, method="sum", buffer=0.1) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[((data$Energy >= elementLine[6][1,]-buffer & data$Energy <= elementLine[6][1,]+buffer) | (data$Energy >= elementLine[5][1,]-buffer & data$Energy <= elementLine[5][1,]+buffer)), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, "K-alpha", sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
elementSplitKalpha <- cmpfun(elementSplitKalpha)


elementGrabKalpha <- function(element, data, calculation="gaussian", gaus_buffer=0.02, split_buffer=0.1){
    if(calculation=="gaussian"){
        elementGaussianKalpha(element=element, data=data, method="sum", buffer=gaus_buffer)
    } else if(calculation=="split"){
        elementSplitKalpha(element=element, data=data, method="sum", buffer=split_buffer)
    } else if(calculation=="first"){
        elementFirstKalpha(element=element, data=data, method="sum", buffer=gaus_buffer)
    } else if(calculation=="second"){
        elementSecondKalpha(element=element, data=data, method="sum", buffer=gaus_buffer)
    }
}


## ---------------------------------------------------------------------------
## Limit of Detection (LOD) estimate from the calibration spectra.
##
## Reports a 3-sigma detection limit for one element/line, in the SAME
## concentration units as the calibration's stored Concentration column, given
## the linear sensitivity `slope` (= d Concentration / d Intensity from
## lm(Concentration ~ Intensity)).
##
## The noise term is the crux. The SNIP baseline is a SMOOTH continuum, so its
## own point-to-point scatter has had the counting noise removed - using it to
## set the noise level badly underestimates the LOD. Instead we recover the real
## noise two ways and take the LARGER (conservative):
##
##  1. Shoulder noise (primary): the high-frequency counting noise measured from
##     robust channel-to-channel first differences of the raw spectrum in
##     PEAK-FREE SHOULDER windows just outside the line ROI. Differencing cancels
##     the smooth continuum/peak shape and any baseline/fit mis-modelling, and
##     MAD(diff)/sqrt(2) rejects contaminated channels - so this recovers the
##     counting noise smoothing removed, without needing LiveTime. Scaled to the
##     ROI sum by sqrt(n_ROI).
##  2. Currie/counting-statistics (cross-check): sigma = sqrt(background counts)
##     from the baseline ROI level and LiveTime - the textbook XRF LLD. Only
##     available when LiveTime is known; converges with (1) when present.
##
## LOD_i = 3 * max(sigma_resid, sigma_currie) * slope / F_i, aggregated across
## the kept standards by the median.
##
## Arguments mirror the Cal Curves page controls so the result is responsive to
## the line-definition and normalization choices:
##   element.line     - full line name, e.g. "Fe.K.alpha" (input$calcurveelement)
##   baseline         - Deconvoluted$Baseline frame (Spectrum, Energy, CPS)
##   spectra_raw      - Calibration$Spectra (raw per-channel CPS, same grid)
##   fit              - Deconvoluted$Spectra (fitted peak response, same grid)
##   line.preference  - "Narrow" | "Wide" | "Area"  (input$linepreferenceelement)
##   line.structure   - "gaussian" | "split" | "first" | "second"
##   norm.type        - 1 none, 2 total counts, 3 Compton  (input$normcal)
##   norm.src         - per-channel frame used for normalization divisor
##   norm.min/max     - Compton window (input$comptonmin/comptonmax)
##   metadata         - frame with Spectrum + LiveTime (enables the Currie term)
##   keep.spectra     - character vector of standards kept on the cal curve
##   slope            - linear sensitivity (d Concentration / d Intensity)
##   range.table      - custom line Definitions (Name, EnergyMin, EnergyMax)
## ---------------------------------------------------------------------------
baseline_lod_estimate <- function(element.line, baseline, spectra_raw=NULL, fit=NULL,
                                   line.preference="Narrow",
                                   line.structure="gaussian", gaus.buffer=0.02,
                                   split.buffer=0.1, norm.type=1, norm.src=NULL,
                                   norm.min=0, norm.max=0, metadata=NULL,
                                   keep.spectra=NULL, slope=NA, range.table=NULL){

    if(is.null(baseline) || !all(c("Spectrum", "Energy", "CPS") %in% names(baseline))){
        return(list(note="no_baseline"))
    }
    if(!is.finite(slope) || slope==0){
        return(list(note="bad_slope"))
    }

    strip_ext <- function(x){
        mgsub::mgsub(as.character(x), c(".pdz", ".csv", ".CSV", ".spt", ".mca", ".spx", ".PDZ", ".spe"), rep("", 8))
    }

    ## Window dispatch shared by the ROI sum and the ROI-membership probe, so the
    ## LOD window is always identical to the one the calibration itself uses.
    grab <- function(dat){
        if(line.preference=="Wide"){
            calc <- if(line.structure %in% c("gaussian", "split")) line.structure else "gaussian"
            wideElementGrab(element.line=element.line, data=dat, range.table=range.table, calculation=calc, buffer=split.buffer)
        } else {
            elementGrab(element.line=element.line, data=dat, range.table=range.table, calculation=line.structure, gaus_buffer=gaus.buffer, split_buffer=split.buffer)
        }
    }

    ## Baseline ROI level per standard (the background level under the peak).
    roi <- grab(baseline)
    if(is.null(roi) || ncol(roi) < 2){
        return(list(note="not_estimable"))
    }
    roi <- data.frame(Spectrum=strip_ext(roi$Spectrum), rawBase=as.numeric(roi[[2]]), stringsAsFactors=FALSE)

    ## Discover which energy channels fall in the ROI by probing the SAME window
    ## with a one-channel-per-"spectrum" frame: the returned Spectrum names come
    ## back as the in-window energies. n_ROI scales channel noise to the ROI sum.
    grid <- sort(unique(round(as.numeric(baseline$Energy), 3)))
    roi_energies <- tryCatch({
        probe <- data.frame(Spectrum=sprintf("%.3f", grid), Energy=grid, CPS=1, stringsAsFactors=FALSE)
        hit <- grab(probe)
        e <- suppressWarnings(as.numeric(hit$Spectrum[is.finite(as.numeric(hit[[2]])) & as.numeric(hit[[2]]) > 0]))
        round(e[is.finite(e)], 3)
    }, error=function(e) numeric(0))
    n_roi <- length(roi_energies)

    ## Per-standard COUNTING noise from peak-free shoulder windows flanking the
    ## ROI. We take successive-channel first differences of the raw spectrum
    ## within each shoulder and estimate the per-channel sigma robustly as
    ## MAD(diff)/sqrt(2). Differencing cancels the smooth continuum, peak tails,
    ## and any baseline/fit mis-modelling (all low-frequency), so only the
    ## high-frequency counting noise survives; MAD rejects the odd contaminated
    ## channel (e.g. a neighbouring L-line edge). This isolates exactly the noise
    ## that smoothing removed, needs no LiveTime, and converges to the Currie
    ## counting-statistics value when LiveTime is available.
    sigma_ch <- NULL   # data.frame(Spectrum, sigma) per standard
    if(n_roi > 0 && !is.null(spectra_raw) && all(c("Spectrum","Energy","CPS") %in% names(spectra_raw))){
        lo <- min(roi_energies); hi <- max(roi_energies)
        step <- stats::median(diff(grid)); if(!is.finite(step) || step <= 0) step <- 0.02
        w <- max(2 * (hi - lo), 8 * step)
        roi_set <- round(roi_energies, 3)
        left_e  <- round(grid[grid >= lo - w & grid < lo & !(round(grid,3) %in% roi_set)], 3)
        right_e <- round(grid[grid > hi & grid <= hi + w & !(round(grid,3) %in% roi_set)], 3)
        keep_e <- c(left_e, right_e)
        if(length(keep_e) >= 5){
            sh <- spectra_raw[round(as.numeric(spectra_raw$Energy),3) %in% keep_e, c("Spectrum","Energy","CPS")]
            if(nrow(sh) > 0){
                sh$Spectrum <- strip_ext(sh$Spectrum)
                sh$Energy <- round(as.numeric(sh$Energy), 3)
                sh$side <- ifelse(sh$Energy < lo, "L", "R")
                ## Robust per-channel sigma via MAD of within-shoulder first
                ## differences; sqrt(2) undoes the variance doubling from diffing.
                robust_sigma <- function(d){
                    d <- d[order(d$Energy), ]
                    diffs <- unlist(lapply(split(d$CPS, d$side), function(v) if(length(v) >= 2) diff(v) else numeric(0)))
                    if(length(diffs) < 4) return(NA_real_)
                    s <- stats::mad(diffs) / sqrt(2)
                    if(!is.finite(s) || s == 0) s <- stats::sd(diffs) / sqrt(2)
                    s
                }
                parts <- lapply(split(sh, sh$Spectrum), function(d) data.frame(Spectrum=d$Spectrum[1], sigma=robust_sigma(d), stringsAsFactors=FALSE))
                sigma_ch <- do.call(rbind, parts)
            }
        }
    }

    ## Normalization divisor per standard (mirrors spectra_tc_/spectra_comp_/lucas_* logic).
    if(norm.type==1 || is.null(norm.src)){
        roi$F <- 1
    } else {
        src <- norm.src
        if(norm.type==3){
            src <- subset(src, !(src$Energy < norm.min | src$Energy > norm.max))
        }
        fac <- aggregate(CPS ~ Spectrum, data=src, FUN=sum)
        colnames(fac) <- c("Spectrum", "F")
        fac$Spectrum <- strip_ext(fac$Spectrum)
        fac$F[fac$F==0] <- 1
        roi <- merge(roi, fac, by="Spectrum", all.x=TRUE)
        roi$F[is.na(roi$F) | roi$F==0] <- 1
    }

    ## LiveTime (seconds) for the Currie counting-statistics term, if available.
    livetime_used <- FALSE
    if(!is.null(metadata) && all(c("Spectrum", "LiveTime") %in% names(metadata))){
        lt <- data.frame(Spectrum=strip_ext(metadata$Spectrum), LiveTime=as.numeric(metadata$LiveTime), stringsAsFactors=FALSE)
        lt <- lt[is.finite(lt$LiveTime) & lt$LiveTime > 0, ]
        if(nrow(lt) > 0){
            roi <- merge(roi, lt, by="Spectrum", all.x=TRUE)
            livetime_used <- TRUE
        }
    }

    ## Attach per-standard shoulder sigma.
    if(!is.null(sigma_ch)){
        roi <- merge(roi, sigma_ch, by="Spectrum", all.x=TRUE)
    } else {
        roi$sigma <- NA_real_
    }

    ## Restrict to the standards kept on the cal curve.
    if(!is.null(keep.spectra)){
        roi <- roi[roi$Spectrum %in% strip_ext(keep.spectra), ]
    }
    roi <- roi[is.finite(roi$rawBase), ]
    n <- nrow(roi)
    if(n < 1){
        return(list(note="not_estimable"))
    }

    m <- abs(slope)

    ## Per-standard sigma (normalized CPS units) for each method.
    ## Residual: channel SD scaled to the ROI sum by sqrt(n_ROI).
    sigma_resid_i <- if(!is.null(sigma_ch) && n_roi > 0){
        sqrt(n_roi) * roi$sigma / roi$F
    } else rep(NA_real_, n)
    ## Currie: sqrt(background counts) from baseline level and LiveTime.
    sigma_currie_i <- if(livetime_used && "LiveTime" %in% names(roi)){
        lt_ok <- is.finite(roi$LiveTime) & roi$LiveTime > 0 & is.finite(roi$rawBase) & roi$rawBase > 0
        s <- rep(NA_real_, n)
        s[lt_ok] <- sqrt(roi$rawBase[lt_ok] / roi$LiveTime[lt_ok]) / roi$F[lt_ok]
        s
    } else rep(NA_real_, n)

    ## Conservative per-standard sigma = max of the two available terms.
    sigma_i <- mapply(function(a, b){
        vals <- c(a, b); vals <- vals[is.finite(vals)]
        if(length(vals)==0) NA_real_ else max(vals)
    }, sigma_resid_i, sigma_currie_i)

    lod_i     <- 3 * sigma_i * m
    lod_resid <- if(any(is.finite(sigma_resid_i))) 3 * stats::median(sigma_resid_i[is.finite(sigma_resid_i)]) * m else NA_real_
    lod_currie<- if(any(is.finite(sigma_currie_i))) 3 * stats::median(sigma_currie_i[is.finite(sigma_currie_i)]) * m else NA_real_
    lod       <- if(any(is.finite(lod_i))) stats::median(lod_i[is.finite(lod_i)]) else NA_real_

    if(!is.finite(lod)){
        return(list(note="not_estimable"))
    }

    method <- if(any(is.finite(sigma_resid_i)) && any(is.finite(sigma_currie_i))){
        "residual+currie"
    } else if(any(is.finite(sigma_resid_i))){
        "residual"
    } else {
        "currie"
    }

    list(n=n, n_roi=n_roi, lod=lod, lod_resid=lod_resid, lod_currie=lod_currie,
         method=method, livetime_used=livetime_used, note="ok")
}
baseline_lod_estimate <- cmpfun(baseline_lod_estimate)


elementGaussianKbeta <- function(element, data, method="sum", buffer=0.02) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.cps <- if(elementLine[8][1,]!=0){
        subset(data$CPS, !(data$Energy < elementLine[7][1,]-buffer | data$Energy > elementLine[8][1,]+buffer))
    } else if(elementLine[8][1,]==0){
        subset(data$CPS, !(data$Energy < elementLine[7][1,]-buffer | data$Energy > elementLine[7][1,]+buffer))
    }
    
    
    hold.file <- if(elementLine[8][1,]!=0){
        subset(data$Spectrum, !(data$Energy < elementLine[7][1,]-buffer | data$Energy > elementLine[8][1,]+buffer))
    } else if(elementLine[8][1,]==0){
            subset(data$Spectrum, !(data$Energy < elementLine[7][1,]-buffer | data$Energy > elementLine[7][1,]+buffer))
    }
    hold.frame <- data.frame(is.0(hold.cps, hold.file), stringsAsFactors=FALSE)
    colnames(hold.frame) <- c("CPS", "Spectrum")
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, "K-beta", sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
elementGaussianKbeta <- cmpfun(elementGaussianKbeta)

elementFirstKbeta <- function(element, data, method="sum", buffer=0.02) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.cps <- if(elementLine[8][1,]!=0){
        subset(data$CPS, !(data$Energy < elementLine[7][1,]-buffer | data$Energy > elementLine[7][1,]+buffer))
    } else if(elementLine[8][1,]==0){
        subset(data$CPS, !(data$Energy < elementLine[7][1,]-buffer | data$Energy > elementLine[7][1,]+buffer))
    }
    
    
    hold.file <- if(elementLine[8][1,]!=0){
        subset(data$Spectrum, !(data$Energy < elementLine[7][1,]-buffer | data$Energy > elementLine[7][1,]+buffer))
    } else if(elementLine[8][1,]==0){
            subset(data$Spectrum, !(data$Energy < elementLine[7][1,]-buffer | data$Energy > elementLine[7][1,]+buffer))
    }
    hold.frame <- data.frame(is.0(hold.cps, hold.file), stringsAsFactors=FALSE)
    colnames(hold.frame) <- c("CPS", "Spectrum")
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, "K-beta", sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
elementFirstKbeta <- cmpfun(elementFirstKbeta)

elementSecondKbeta <- function(element, data, method="sum", buffer=0.02) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.cps <- if(elementLine[8][1,]!=0){
        subset(data$CPS, !(data$Energy < elementLine[8][1,]-buffer | data$Energy > elementLine[8][1,]+buffer))
    } else if(elementLine[8][1,]==0){
        subset(data$CPS, !(data$Energy < elementLine[7][1,]-buffer | data$Energy > elementLine[7][1,]+buffer))
    }
    
    
    hold.file <- if(elementLine[8][1,]!=0){
        subset(data$Spectrum, !(data$Energy < elementLine[8][1,]-buffer | data$Energy > elementLine[8][1,]+buffer))
    } else if(elementLine[8][1,]==0){
            subset(data$Spectrum, !(data$Energy < elementLine[7][1,]-buffer | data$Energy > elementLine[7][1,]+buffer))
    }
    hold.frame <- data.frame(is.0(hold.cps, hold.file), stringsAsFactors=FALSE)
    colnames(hold.frame) <- c("CPS", "Spectrum")
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, "K-beta", sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
elementSecondKbeta <- cmpfun(elementSecondKbeta)


elementSplitKbeta <- function(element, data, method="sum", buffer=0.1) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.cps <- if(elementLine[8][1,]!=0){
        subset(data$CPS, !(data$Energy <= elementLine[7][1,]-buffer & data$Energy >= elementLine[7][1,]+buffer | data$Energy >= elementLine[8][1,]+buffer & data$Energy <= elementLine[8][1,]-buffer))
    } else if(elementLine[8][1,]==0){
        subset(data$CPS, !(data$Energy <= elementLine[7][1,]-buffer | data$Energy >= elementLine[7][1,]+buffer | data$Energy >= elementLine[7][1,]+buffer | data$Energy <= elementLine[7][1,]-buffer))
    }
    
    
    hold.file <- if(elementLine[8][1,]!=0){
        subset(data$Spectrum, !(data$Energy <= elementLine[7][1,]-buffer | data$Energy >= elementLine[7][1,]+buffer| data$Energy >= elementLine[8][1,]+buffer | data$Energy <= elementLine[8][1,]-buffer))
    } else if(elementLine[8][1,]==0){
            subset(data$Spectrum, !(data$Energy <= elementLine[7][1,]-buffer | data$Energy >= elementLine[7][1,]+buffer | data$Energy >= elementLine[7][1,]+buffer | data$Energy <= elementLine[7][1,]-buffer))
    }
    hold.frame <- data.frame(is.0(hold.cps, hold.file), stringsAsFactors=FALSE)
    colnames(hold.frame) <- c("CPS", "Spectrum")
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, "K-beta", sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
elementSplitKbeta <- cmpfun(elementSplitKbeta)

elementGrabKbeta <- function(element, data, calculation="gaussian", gaus_buffer=0.02, split_buffer=0.1){
    if(calculation=="gaussian"){
        elementGaussianKbeta(element=element, data=data, method="sum", buffer=gaus_buffer)
    } else if(calculation=="split"){
        elementSplitKbeta(element=element, data=data, method="sum", buffer=split_buffer)
    } else if(calculation=="first"){
        elementFirstKbeta(element=element, data=data, method="sum", buffer=gaus_buffer)
    } else if(calculation=="second"){
        elementSecondKbeta(element=element, data=data, method="sum", buffer=gaus_buffer)
    }
}


elementGaussianLalpha <- function(element, data, method="sum", buffer=0.02) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy < elementLine[11][1,]-buffer | data$Energy > elementLine[10][1,]+buffer), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, "L-alpha", sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
elementGaussianLalpha <- cmpfun(elementGaussianLalpha)

elementFirstLalpha <- function(element, data, method="sum", buffer=0.02) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy < elementLine[10][1,]-buffer | data$Energy > elementLine[10][1,]+buffer), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, "L-alpha", sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
elementFirstLalpha <- cmpfun(elementFirstLalpha)

elementSecondLalpha <- function(element, data, method="sum", buffer=0.02) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy < elementLine[11][1,]-buffer | data$Energy > elementLine[11][1,]+buffer), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, "L-alpha", sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
elementSecondLalpha <- cmpfun(elementSecondLalpha)

elementSplitLalpha <- function(element, data, method="sum", buffer=0.1) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy <= elementLine[11][1,]-buffer & data$Energy >= elementLine[11][1,]+buffer | data$Energy >= elementLine[10][1,]+buffer & data$Energy <= elementLine[10][1,]-buffer), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, "L-alpha", sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
elementSplitLalpha <- cmpfun(elementSplitLalpha)

elementGrabLalpha <- function(element, data, calculation="gaussian", gaus_buffer=0.02, split_buffer=0.1){
    if(calculation=="gaussian"){
        elementGaussianLalpha(element=element, data=data, method="sum", buffer=gaus_buffer)
    } else if(calculation=="split"){
        elementSplitLalpha(element=element, data=data, method="sum", buffer=split_buffer)
    } else if(calculation=="first"){
        elementFirstLalpha(element=element, data=data, method="sum", buffer=gaus_buffer)
    } else if(calculation=="second"){
        elementSecondLalpha(element=element, data=data, method="sum", buffer=gaus_buffer)
    }
}
elementGrabLalpha <- cmpfun(elementGrabLalpha)


elementGaussianLbeta <- function(element, data, method="sum", buffer=0.02) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy < elementLine[12][1,]-buffer | data$Energy > elementLine[14][1,]+buffer), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, "L-beta", sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
elementGaussianLbeta <- cmpfun(elementGaussianLbeta)

elementFirstLbeta <- function(element, data, method="sum", buffer=0.02) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy < elementLine[12][1,]-buffer | data$Energy > elementLine[12][1,]+buffer), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, "L-beta", sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
elementFirstLbeta <- cmpfun(elementFirstLbeta)

elementSecondLbeta <- function(element, data, method="sum", buffer=0.02) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy < elementLine[14][1,]-buffer | data$Energy > elementLine[14][1,]+buffer), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, "L-beta", sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
elementSecondLbeta <- cmpfun(elementSecondLbeta)

elementSplitLbeta <- function(element, data, method="sum", buffer=0.1) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy <= elementLine[12][1,]-buffer & data$Energy >= elementLine[12][1,]+buffer | data$Energy >= elementLine[14][1,]+buffer & data$Energy <= elementLine[14][1,]-buffer), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, "L-beta", sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
elementSplitLbeta <- cmpfun(elementSplitLbeta)

elementGrabLbeta <- function(element, data, calculation="gaussian", gaus_buffer=0.02, split_buffer=0.1){
    if(calculation=="gaussian"){
        elementGaussianLbeta(element=element, data=data, method="sum", buffer=gaus_buffer)
    } else if(calculation=="split"){
        elementSplitLbeta(element=element, data=data, method="sum", buffer=split_buffer)
    } else if(calculation=="first"){
        elementFirstLbeta(element=element, data=data, method="sum", buffer=gaus_buffer)
    } else if(calculation=="second"){
        elementSecondLbeta(element=element, data=data, method="sum", buffer=gaus_buffer)
    }
}
elementGrabLbeta <- cmpfun(elementGrabLbeta)

elementGaussianMalpha <- function(element, data, method="sum", buffer=0.02) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy < elementLine[20][1,]-buffer | data$Energy > elementLine[22][1,]+buffer), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, "M-line", sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
elementGaussianMalpha <- cmpfun(elementGaussianMalpha)

elementFirstMalpha <- function(element, data, method="sum", buffer=0.02) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy < elementLine[20][1,]-buffer | data$Energy > elementLine[20][1,]+buffer), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, "M-line", sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
elementFirstMalpha <- cmpfun(elementFirstMalpha)

elementSecondMalpha <- function(element, data, method="sum", buffer=0.02) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy < elementLine[22][1,]-buffer | data$Energy > elementLine[22][1,]+buffer), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, "M-line", sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
elementSecondMalpha <- cmpfun(elementSecondMalpha)

elementSplitMalpha <- function(element, data, method="sum", buffer=0.1) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy <= elementLine[20][1,]-buffer & data$Energy >= elementLine[20][1,]+buffer | data$Energy >= elementLine[22][1,]+buffer & data$Energy <= elementLine[22][1,]-buffer), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, "M-line", sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
elementSplitMalpha <- cmpfun(elementSplitMalpha)

elementSplitMalpha <- function(element, data, calculation="gaussian", gaus_buffer=0.02, split_buffer=0.1){
    if(calculation=="gaussian"){
        elementGaussianMalpha(element=element, data=data, method="sum", buffer=gaus_buffer)
    } else if(calculation=="split"){
        elementSplitMalpha(element=element, data=data, method="sum", buffer=split_buffer)
    } else if(calculation=="first"){
        elementFirstMalpha(element=element, data=data, method="sum", buffer=gaus_buffer)
    } else if(calculation=="second"){
        elementSecondMalpha(element=element, data=data, method="sum", buffer=gaus_buffer)
    }
}
elementSplitMalpha <- cmpfun(elementSplitMalpha)

elementSplitLbeta <- cmpfun(elementSplitLbeta)

elementGrabMalpha <- function(element, data, calculation="gaussian", gaus_buffer=0.02, split_buffer=0.1){
    if(calculation=="gaussian"){
        elementGaussianMalpha(element=element, data=data, method="sum", buffer=gaus_buffer)
    } else if(calculation=="split"){
        elementSplitMalpha(element=element, data=data, method="sum", buffer=split_buffer)
    } else if(calculation=="first"){
        elementFirstMalpha(element=element, data=data, method="sum", buffer=gaus_buffer)
    } else if(calculation=="second"){
        elementSecondMalpha(element=element, data=data, method="sum", buffer=gaus_buffer)
    }
}
elementGrabMalpha <- cmpfun(elementGrabMalpha)

standardElementGrab <- function(element.line, data, calculation="gaussian", gaus_buffer=0.02, split_buffer=0.1){

    element.line <- make.names(element.line)
    
    element <- strsplit(x=element.line, split="\\.")[[1]][1]
    destination <- strsplit(x=element.line, split="\\.")[[1]][2]
    distance <- strsplit(x=element.line, split="\\.")[[1]][3]
    
    if(destination=="K" && distance=="alpha"){
        elementGrabKalpha(element=element, data=data, calculation=calculation, gaus_buffer=gaus_buffer, split_buffer=split_buffer)
    } else if(destination=="K" && distance=="beta"){
        elementGrabKbeta(element=element, data=data, calculation=calculation, gaus_buffer=gaus_buffer, split_buffer=split_buffer)
    } else if(destination=="L" && distance=="alpha"){
        elementGrabLalpha(element=element, data=data, calculation=calculation, gaus_buffer=gaus_buffer, split_buffer=split_buffer)
    } else if (destination=="L" && distance=="beta"){
        elementGrabLbeta(element=element, data=data, calculation=calculation, gaus_buffer=gaus_buffer, split_buffer=split_buffer)
    } else if (destination=="M" && distance=="line"){
        elementGrabMalpha(element=element, data=data, calculation=calculation, gaus_buffer=gaus_buffer, split_buffer=split_buffer)
    }
        
}
standardElementGrab <- cmpfun(standardElementGrab)




range_gaussian_xrf <- function(range.frame, data, method="mean"){
    
    new.data <- subset(data, Energy >= range.frame$EnergyMin & Energy <= range.frame$EnergyMax, drop=TRUE)[,c("Spectrum", "CPS")]
    newer.data <- aggregate(list(new.data$CPS), by=list(new.data$Spectrum), FUN=method, na.rm=TRUE)
    colnames(newer.data) <- c("Spectrum", as.character(range.frame$Name))
    newer.data
}
range_gaussian_xrf <- cmpfun(range_gaussian_xrf)

range_split_xrf <- function(range.frame, data, method="mean", buffer=0.1){
    
    new.data <- subset(data, Energy >= range.frame$EnergyMin - buffer & Energy <= range.frame$EnergyMin + buffer & Energy <= range.frame$EnergyMax + buffer & Energy >= range.frame$EnergyMax - buffer, drop=TRUE)[,c("Spectrum", "CPS")]
    newer.data <- aggregate(list(new.data$CPS), by=list(new.data$Spectrum), FUN=method, na.rm=TRUE)
    colnames(newer.data) <- c("Spectrum", as.character(range.frame$Name))
    newer.data
}
range_split_xrf <- cmpfun(range_split_xrf)

range_subset_xrf <- function(range.frame, data, calculation="gaussian", buffer=0.1){
    #if(calculation=="gaussian"){
        range_gaussian_xrf(range.frame=range.frame, data=data, method="mean")
    #} else if(calculation=="split"){
        #range_split_xrf(range.frame=range.frame, data=data, method="mean", buffer=buffer)
    #    range_gaussian_xrf(range.frame=range.frame, data=data, method="mean")
    #}
}
range_subset_xrf <- cmpfun(range_subset_xrf)


xrf_parse <- function(range.table, data, calculation="gaussian", buffer=0.1){
    
    choice.lines <- range.table[complete.cases(range.table),]
    
    choice.list <- split(choice.lines, f=choice.lines$Name)
    names(choice.list) <- choice.lines[,"Name"]
    
    index <- choice.lines[,"Name"]
    
    selected.list <- lapply(index, function(x) range_subset_xrf(range.frame=choice.list[[x]], data=data, calculation=calculation, buffer=buffer))
    
    Reduce(function(...) merge(..., all=T), selected.list)
}
xrf_parse <- cmpfun(xrf_parse)

xrf_parse_single <- function(range.table, data, element, calculation="gaussian", buffer=0.1){
    
    choice.lines <- range.table[range.table$Name %in% element,]
    
    choice.list <- split(choice.lines, f=choice.lines$Name)
    names(choice.list) <- choice.lines[,"Name"]
    
    index <- choice.lines[,"Name"]
    
    selected.list <- lapply(index, function(x) range_subset_xrf(range.frame=choice.list[[x]], data=data, calculation=calculation, buffer=buffer))
    
    Reduce(function(...) merge(..., all=T), selected.list)
}
xrf_parse_single <- cmpfun(xrf_parse_single)



elementGrabPre <- function(element.line, data, range.table=NULL, calculation="gaussian", gaus_buffer=0.02, split_buffer=0.1, buffer=0.1){
    
    is.element <- element.line %in% spectralLines
    
    if(is.element==TRUE){
        standardElementGrab(element.line, data, calculation=calculation, gaus_buffer=gaus_buffer, split_buffer=split_buffer)
    } else if(is.element==FALSE){
        xrf_parse_single(range.table, data, element.line, calculation="gaussian", buffer=buffer)
    }

    
}
elementGrabPre <- cmpfun(elementGrabPre)

elementGrab <- function(element.line, data, range.table=NULL, calculation="gaussian", gaus_buffer=0.02, split_buffer=0.1, buffer=0.1){
    
    error_frame <- data.frame(Spectrum=unique(data$Spectrum), Hold=0)
    colnames(error_frame) <- c("Spectrum", element.line)
    
    tryCatch(
        elementGrabPre(element.line=element.line, data=data, range.table=range.table, calculation=calculation, gaus_buffer=gaus_buffer, split_buffer=split_buffer, buffer=buffer)
        , error=function(e) error_frame)
    
    
}

elementGrabError <- function(data, element.line){
    error_frame <- data.frame(Spectrum=unique(data$Spectrum), Hold=0)
    colnames(error_frame) <- c("Spectrum", element.line)
    return(error_frame)
}

add_missing_columns <- function(df, colnames) {
  for (colname in colnames) {
    if (!(colname %in% names(df))) {
      df[[colname]] <- rep(0, nrow(df))
    }
  }
  return(df)
}


elementFrame <- function(data, range.table=NULL, elements, calculation="gaussian", gaus_buffer=0.02, split_buffer=0.1, buffer=0.1, allowParallel=TRUE){
    
    error_frame <- data.frame(Spectrum=unique(data$Spectrum), Hold=0)
    
    spectra.line.list <- if(get_os()=="windows"){
        lapply(elements, function(x) elementGrab(element.line=x, data=data, range.table=range.table, calculation=calculation, gaus_buffer=gaus_buffer, split_buffer=split_buffer, buffer=buffer))
    } else if(get_os()!="windows"){
        core.mod <- if(length(elements)>=as.numeric(my.cores)){
            as.numeric(my.cores)
        } else if(length(elements)<as.numeric(my.cores)){
            length(elements)
        }
        if(allowParallel==TRUE){
            tryCatch(pblapply(cl=core.mod, X=elements, function(x) elementGrab(element.line=x, data=data, range.table=range.table, calculation=calculation, gaus_buffer=gaus_buffer, split_buffer=split_buffer, buffer=buffer)), error=function(e) lapply(elements, function(x) elementGrab(element.line=x, data=data, range.table=range.table, calculation=calculation, gaus_buffer=gaus_buffer, split_buffer=split_buffer, buffer=buffer)))
        } else if(allowParallel==FALSE){
            lapply(elements, function(x) elementGrab(element.line=x, data=data, range.table=range.table, calculation=calculation, gaus_buffer=gaus_buffer, split_buffer=split_buffer, buffer=buffer))
        }
    }
    
    spectra.line.frame <- Reduce(function(x, y) merge(x, y, all=TRUE), spectra.line.list)
    spectra.line.frame <- as.data.frame(spectra.line.frame, stringsAsFactors=FALSE)
    
    good_elements <- make.names(names(spectra.line.frame)[-1])
    missing_elements <- elements[!elements %in% good_elements]

    if(length(missing_elements >= 1)){
        spectra.line.frame <- add_missing_columns(df=spectra.line.frame, colnames=elements)
    }
    colnames(spectra.line.frame)  <- make.names(colnames(spectra.line.frame))

    #colnames(spectra.line.frame) <- c("Spectrum", elements)

    spectra.line.frame <- as.data.frame(spectra.line.frame, stringsAsFactors=FALSE)
    
    spectra.line.frame <- spectra.line.frame[order(as.character(spectra.line.frame$Spectrum)),]

    # Remove file extensions in a single pass
    file_extensions <- c(".pdz", ".csv", ".CSV", ".spt", ".mca", ".spx", ".PDZ", ".spe")
    spectra.line.frame$Spectrum <- mgsub::mgsub(spectra.line.frame$Spectrum, file_extensions, rep("", length(file_extensions)))

    spectra.line.frame

}
elementFrame <- cmpfun(elementFrame)


wideElementGaussianLine <- function(element.line, data, method="sum") {
    
    element <- strsplit(x=element.line, split="\\.")[[1]][1]
    destination <- strsplit(x=element.line, split="\\.")[[1]][2]
    distance <- strsplit(x=element.line, split="\\.")[[1]][3]
    
    line <- paste0(destination, "-", distance)
    
    elementLine <- Wide[[element]]
    
    emission <- if(line=="K-alpha"){
        "Ka1"
    } else if(line=="K-beta"){
        "Kb1"
    } else if(line=="L-alpha"){
        "La1"
    } else if(line=="L-beta"){
        "Lb1"
    } else if(line=="M-line"){
        "Ma1"
    }
    
    #hold.frame <- data[data$Energy < elementLine[2, emission] && data$Energy > elementLine[1, emission], c("CPS", "Spectrum")]
    hold.frame <- data[!(data$Energy < elementLine[1, emission] | data$Energy > elementLine[2, emission]), c("CPS", "Spectrum")]
    
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, line, sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
wideElementGaussianLine <- cmpfun(wideElementGaussianLine)

wideElementGaussianLine <- function(element.line, data, method="sum") {
    
    element <- strsplit(x=element.line, split="\\.")[[1]][1]
    destination <- strsplit(x=element.line, split="\\.")[[1]][2]
    distance <- strsplit(x=element.line, split="\\.")[[1]][3]
    
    line <- paste0(destination, "-", distance)
    
    elementLine <- Wide[[element]]
    
    emission <- if(line=="K-alpha"){
        "Ka1"
    } else if(line=="K-beta"){
        "Kb1"
    } else if(line=="L-alpha"){
        "La1"
    } else if(line=="L-beta"){
        "Lb1"
    } else if(line=="M-line"){
        "Ma1"
    }
    
    #hold.frame <- data[data$Energy < elementLine[2, emission] && data$Energy > elementLine[1, emission], c("CPS", "Spectrum")]
    hold.frame <- data[!(data$Energy < elementLine[1, emission] | data$Energy > elementLine[2, emission]), c("CPS", "Spectrum")]
    
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, line, sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
wideElementGaussianLine <- cmpfun(wideElementGaussianLine)

wideElementSplitLine <- function(element.line, data, method="sum", buffer=0.1) {
    
    element <- strsplit(x=element.line, split="\\.")[[1]][1]
    destination <- strsplit(x=element.line, split="\\.")[[1]][2]
    distance <- strsplit(x=element.line, split="\\.")[[1]][3]
    
    line <- paste0(destination, "-", distance)
    
    elementLine <- Wide[[element]]
    
    emission <- if(line=="K-alpha"){
        "Ka1"
    } else if(line=="K-beta"){
        "Kb1"
    } else if(line=="L-alpha"){
        "La1"
    } else if(line=="L-beta"){
        "Lb1"
    } else if(line=="M-line"){
        "Ma1"
    }
    
    #hold.frame <- data[data$Energy < elementLine[2, emission] && data$Energy > elementLine[1, emission], c("CPS", "Spectrum")]
    hold.frame <- data[!(data$Energy <= elementLine[1, emission]-buffer & data$Energy >= elementLine[1, emission]+buffer | data$Energy >= elementLine[2, emission]+buffer & data$Energy <= elementLine[2, emission]-buffer), c("CPS", "Spectrum")]
    
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN=method)
    colnames(hold.ag) <- c("Spectrum", paste(element, line, sep=" "))
    if(any(is.na(as.numeric(hold.ag[[2]])))){
      # Replace NA values with 0
      as.numeric(hold.ag[[2]])[is.na(as.numeric(hold.ag[[2]]))] <- 0
    }
    hold.ag
    
}
wideElementSplitLine <- cmpfun(wideElementSplitLine)

wideElementGrabLine <- function(element, data, calculation="gaussian", buffer=0.1){
    if(calculation=="gaussian"){
        wideElementGaussianLine(element=element, data=data, method="sum")
    } else if(calculation=="split"){
        wideElementSplitLine(element=element, data=data, method="sum", buffer=buffer)
    }
}
wideElementGrabLine <- cmpfun(wideElementGrabLine)

wideElementGrabPre <- function(element.line, data, range.table=NULL, calculation="gaussian", buffer=0.1){
    
    is.element <- element.line %in% spectralLines
    
    if(is.element==TRUE){
        wideElementGrabLine(element.line, data, calculation=calculation, buffer=buffer)
    } else if(is.element==FALSE){
        xrf_parse_single(range.table, data, element.line, calculation=calculation, buffer=buffer)
    }

    
}
wideElementGrabPre <- cmpfun(wideElementGrabPre)

wideElementGrab <- function(element.line, data, range.table=NULL, calculation="gaussian", buffer=0.1){
    
    error_frame <- data.frame(Spectrum=unique(data$Spectrum), Hold=NA)
    colnames(error_frame) <- c("Spectrum", element.line)
    
    tryCatch(wideElementGrabPre(element.line=element.line, data=data, range.table=range.table, calculation=calculation, buffer=buffer), error=function(e) error_frame)
    
    
}

wideElementFrame <- function(data, elements, range.table=NULL, calculation="gaussian", buffer=0.1, allowParallel=TRUE){
    
    spectra.line.list <- if(get_os()=="windows"){
        lapply(elements, function(x) wideElementGrab(element.line=x, data=data, range.table=range.table, calculation=calculation, buffer=buffer))
    }else if(get_os()!="windows"){
        core.mod <- if(length(elements)>=as.numeric(my.cores)){
            as.numeric(my.cores)
        } else if(length(elements)<as.numeric(my.cores)){
            length(elements)
        }
        #pblapply(cl=core.mod, X=elements, function(x) wideElementGrab(element.line=x, data=data))
        lapply(elements, function(x) wideElementGrab(element.line=x, data=data, range.table=range.table, calculation=calculation, buffer=buffer))
    }
    
    element.count.list <- lapply(spectra.line.list, '[', 2)
    
    spectra.line.frame <- Reduce(function(x, y) merge(x, y, all=TRUE), spectra.line.list)
    spectra.line.frame <- as.data.frame(spectra.line.frame, stringsAsFactors=FALSE)
    
    good_elements <- make.names(names(spectra.line.frame)[-1])
    missing_elements <- elements[!elements %in% good_elements]

    if(length(missing_elements >= 1)){
        spectra.line.frame <- add_missing_columns(df=spectra.line.frame, colnames=elements)
    }
    colnames(spectra.line.frame)  <- make.names(colnames(spectra.line.frame))

    #colnames(spectra.line.frame) <- c("Spectrum", elements)

    spectra.line.frame <- as.data.frame(spectra.line.frame, stringsAsFactors=FALSE)
    
    spectra.line.frame <- spectra.line.frame[order(as.character(spectra.line.frame$Spectrum)),]

    # Remove file extensions in a single pass
    file_extensions <- c(".pdz", ".csv", ".CSV", ".spt", ".mca", ".spx", ".PDZ", ".spe")
    spectra.line.frame$Spectrum <- mgsub::mgsub(spectra.line.frame$Spectrum, file_extensions, rep("", length(file_extensions)))

    spectra.line.frame

}
wideElementFrame <- cmpfun(wideElementFrame)



####Normalize

element_norm <- function(data, element, min, max) {
    
    compton.norm <- subset(data$CPS, !(data$Energy < input$min | data$max > input$comptonmax))
    compton.file <- subset(data$Spectrum, !(data$Energy < input$min | data$Energy > input$max))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    
}
element_norm <- cmpfun(element_norm)


####Cal Models

linear_simp_xrf <- function(concentration.table, spectra.line.table, element.line) {
    
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    
    predict.frame <- data.frame(concentration, intensit, stringsAsFactors=FALSE)
    colnames(predict.frame) <- c("Concentration", "Intensity")
    
    
    
    predict.intensity <- data.frame(predict.frame$Intensity, stringsAsFactors=FALSE)
    colnames(predict.intensity) <- c("Intensity")
    
    cal.lm <- lm(predict.frame$Concentration~predict.frame$Intensity)
    
    cal.lm
    
}
linear_simp_xrf <- cmpfun(linear_simp_xrf)


poly_simp_xrf <- function(concentration.table, spectra.line.table, element.line) {
    
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    
    predict.frame <- data.frame(concentration, intensity, stringsAsFactors=FALSE)
    colnames(predict.frame) <- c("Concentration", "Intensity")
    
    
    
    predict.intensity <- data.frame(predict.frame$Intensity, stringsAsFactors=FALSE)
    colnames(predict.intensity) <- c("Intensity")
    
    cal.lm.poly <- lm(predict.frame$Concentration~poly(predict.frame$Intensity, 2))
    
    cal.lm.poly
    
}
poly_simp_xrf <- cmpfun(poly_simp_xrf)


lucas_simp_xrf <- function(concentration.table, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    lucas.intercept.table <- data.frame(rowSums(lucas.intercept.table.x[intercept.element.lines]), stringsAsFactors=FALSE)
    colnames(lucas.intercept.table) <- c("first")
    
    
    
    lucas.intercept <- lucas.intercept.table$first
    lucas.slope <- data.frame(lucas.slope.table[slope.element.lines], stringsAsFactors=FALSE)
    
    
    
    predict.frame.luk <- data.frame(concentration, ((1+intensity/(intensity+lucas.intercept))-lucas.intercept/(intensity+lucas.intercept)),lucas.slope, stringsAsFactors=FALSE)
    colnames(predict.frame.luk) <- c("Concentration", "Intensity", names(lucas.slope))
    
    
    
    predict.intensity.luk <- data.frame(predict.frame.luk$Intensity, lucas.slope, stringsAsFactors=FALSE)
    colnames(predict.intensity.luk) <- c("Intensity", names(lucas.slope))
    
    lucas.lm <- lm(Concentration~., data=predict.frame.luk)
    
    lucas.lm
    
    
}
lucas_simp_xrf <- cmpfun(lucas_simp_xrf)


linear_tc_xrf <- function(concentration.table, spectra.line.table, element.line) {
    
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    
    total.counts <- aggregate(CPS~Spectrum, data=data, sum)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    
    predict.frame.tc <- data.frame(concentration, intensity/total.counts$CPS, stringsAsFactors=FALSE)
    colnames(predict.frame.tc) <- c("Concentration", "Intensity")
    
    
    
    predict.intensity.tc <- data.frame(predict.frame.tc$Intensity, stringsAsFactors=FALSE)
    colnames(predict.intensity.tc) <- c("Intensity")
    
    cal.lm.tc <- lm(predict.frame.tc$Concentration~predict.frame.tc$Intensity)
    
    cal.lm.tc
    
}
linear_tc_xrf <- cmpfun(linear_tc_xrf)


poly_tc_xrf <- function(concentration.table, spectra.line.table, element.line) {
    
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    
    
    
    total.counts <- aggregate(CPS~Spectrum, data=data, sum)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    
    predict.frame.tc <- data.frame(concentration, intensity/total.counts$CPS, stringsAsFactors=FALSE)
    colnames(predict.frame.tc) <- c("Concentration", "Intensity")
    
    
    
    predict.intensity.tc <- data.frame(predict.frame.tc$Intensity, stringsAsFactors=FALSE)
    colnames(predict.intensity.tc) <- c("Intensity")
    
    cal.lm.poly.tc <- lm(predict.frame.tc$Concentration~poly(predict.frame.tc$Intensity, 2))
    
    cal.lm.poly.tc
    
    
    
}
poly_tc_xrf <- cmpfun(poly_tc_xrf)




lucas_tc_xrf <- function(concentration.table, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    lucas.intercept.table.tc <- data.frame(rowSums(lucas.intercept.table.x[intercept.element.lines]), stringsAsFactors=FALSE)/total.counts$CPS
    colnames(lucas.intercept.table.tc) <- c("first")
    
    
    
    lucas.intercept.tc <- lucas.intercept.table.tc$first
    lucas.slope.tc <- data.frame(lucas.slope.table[slope.element.lines], stringsAsFactors=FALSE)/total.counts$CPS
    
    
    
    predict.frame.luc.tc <- data.frame(concentration, ((intensity/total.counts$CPS-lucas.intercept.tc)/(intensity/total.counts$CPS+lucas.intercept.tc)),lucas.slope.tc, stringsAsFactors=FALSE)
    colnames(predict.frame.luc.tc) <- c("Concentration", "Intensity", names(lucas.slope.tc))
    
    
    
    predict.intensity.luc.tc <- data.frame(predict.frame.luc.tc$Intensity, lucas.slope.tc, stringsAsFactors=FALSE)
    colnames(predict.intensity.luc.tc) <- c("Intensity", names(lucas.slope.tc))
    
    lucas.lm.tc <- lm(Concentration~., data=predict.frame.luc.tc)
    
    lucas.lm.tc
    
    
}
lucas_tc_xrf <- cmpfun(lucas_tc_xrf)

linear_comp_xrf <- function(data, concentration.table, spectra.line.table, element.line) {
    
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    
    compton.norm <- subset(data$CPS, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))
    compton.file <- subset(data$Spectrum, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    predict.frame.comp <- data.frame(concentration, intensity/compton.frame.ag$Compton, stringsAsFactors=FALSE)
    colnames(predict.frame.comp) <- c("Concentration", "Intensity")
    
    
    
    predict.intensity.comp <- data.frame(predict.frame.comp$Intensity, stringsAsFactors=FALSE)
    colnames(predict.intensity.comp) <- c("Intensity")
    
    cal.lm.comp <- lm(predict.frame.comp$Concentration~predict.frame.comp$Intensity)
    
    cal.lm.comp
    
}
linear_comp_xrf <- cmpfun(linear_comp_xrf)


poly_comp_xrf <- function(data, concentration.table, spectra.line.table, element.line) {
    
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    
    compton.norm <- subset(data$CPS, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))
    compton.file <- subset(data$Spectrum, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    predict.frame.comp <- data.frame(concentration, intensity/compton.frame.ag$Compton, stringsAsFactors=FALSE)
    colnames(predict.frame.comp) <- c("Concentration", "Intensity")
    
    
    
    predict.intensity.comp <- data.frame(predict.frame.comp$Intensity, stringsAsFactors=FALSE)
    colnames(predict.intensity.comp) <- c("Intensity")
    
    cal.lm.poly.comp <- lm(predict.frame.comp$Concentration~poly(predict.frame.comp$Intensity, 2))
    
    cal.lm.poly.comp
    
}
poly_comp_xrf <- cmpfun(poly_comp_xrf)


lucas_comp_xrf <- function(data, concentration.table, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    concentration <- na.omit(as.vector(as.numeric(unlist(concentration.table[element.line]))))
    
    
    intensity <- na.omit(as.vector(as.numeric(unlist(spectra.line.table[element.line]))))
    
    
    compton.norm <- subset(data$CPS, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))
    compton.file <- subset(data$Spectrum, !(data$Energy < input$comptonmin | data$Energy > input$comptonmax))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    
    
    lucas.intercept.table.comp <- data.frame(rowSums(lucas.intercept.table.x[intercept.element.lines]), stringsAsFactors=FALSE)/compton.frame.ag$Compton
    colnames(lucas.intercept.table.comp) <- c("first")
    
    
    
    lucas.intercept.comp <- lucas.intercept.table.comp$first
    lucas.slope.comp <- data.frame(lucas.slope.table[slope.element.lines], stringsAsFactors=FALSE)/compton.frame.ag$Compton
    
    
    
    
    predict.frame.luc.comp <- data.frame(concentration, ((1+intensity/compton.frame.ag$Compton)/(intensity/compton.frame.ag$Compton+lucas.intercept.comp)-lucas.intercept.comp/(intensity/compton.frame.ag$Compton+lucas.intercept.comp)),lucas.slope.comp, stringsAsFactors=FALSE)
    colnames(predict.frame.luc.comp) <- c("Concentration", "Intensity", names(lucas.slope.comp))
    
    
    
    predict.intensity.luc.comp <- data.frame(predict.frame.luc.comp$Intensity, lucas.slope.comp, stringsAsFactors=FALSE)
    colnames(predict.intensity.luc.comp) <- c("Intensity", names(lucas.slope.comp))
    
    lucas.lm.comp <- lm(Concentration~., data=predict.frame.luc.comp)
    
    lucas.lm.comp
    
}
lucas_comp_xrf <- cmpfun(lucas_comp_xrf)



###Spectra Manipulaton

james <- function(x) (abs(x)+x)/2
james.cp <- compiler::cmpfun(james)

spectra_summary_general <- function(spectra.frame, norm.type, norm.min, norm.max, compress="100 eV", transformation="None", energy.range=c(0.7, 37)){
    
    if(norm.type==1){
        spectra_simp_trans_xrf(spectra=spectra.frame, compress=compress, transformation=transformation, energy.min=energy.range[1], energy.max=energy.range[2])
    } else if(norm.type==2){
        spectra_tc_trans_xrf(spectra=spectra.frame, compress=compress, transformation=transformation, energy.min=energy.range[1], energy.max=energy.range[2])
    } else if(norm.type==3){
        spectra_comp_trans_xrf(spectra=spectra.frame, norm.min=norm.min, norm.max=norm.max, compress=compress, transformation=transformation, energy.min=energy.range[1], energy.max=energy.range[2])
    }
    
}
spectra_summary_general <- cmpfun(spectra_summary_general)



spectra_stats <- function(spectra.frame, norm.type, norm.min, norm.max, compress="100 eV", transformation="None", energy.range=c(0.7, 37)){
    
    
    data.processed <- spectra_summary_general(spectra.frame=spectra.frame, norm.type=norm.type, norm.min=norm.min, norm.max=norm.max, compress=compress, transformation=transformation, energy.range=energy.range)
    
    
    
    data.sum <- data.frame(
    Energy = data.processed$Energy,
    Min = apply(data.processed[,-1], 1, min),
    Max = apply(data.processed[,-1], 1, max),
    Mean = apply(data.processed[,-1], 1, mean),
    Median = apply(data.processed[,-1], 1, median),
    SD = apply(data.processed[,-1], 1, sd), stringsAsFactors=FALSE)
    
    data.sum$SDMin <- data.sum$Mean - data.sum$SD
    data.sum$SDMax <- data.sum$Mean + data.sum$SD
    data.sum$SD2Min <- data.sum$Mean - data.sum$SD*2
    data.sum$SD2Max <- data.sum$Mean + data.sum$SD*2
    
    
    
    data.sum <- as.data.frame(apply(data.sum, 2, james.cp), stringsAsFactors=FALSE)
    return(data.sum)
    #data.sum[!duplicated(data.sum), ]
    
}
spectra_stats <- cmpfun(spectra_stats)


###############
###Prep Data###
###############


###############
###Full Spectra##
###############


spectra_frame_xrf <- function(spectra){
    
    data <- reshape2::dcast(spectra, Spectrum~Energy, value.var="CPS")
    
    #test <- apply(test, 2, as.numeric)
    colnames(data) <- make.names(colnames(data))
    data[complete.cases(data),]
}
spectra_frame_xrf <- cmpfun(spectra_frame_xrf)



spectra_table_xrf <- function(spectra, concentration){
    
    data <- reshape2::dcast(spectra, Spectrum~Energy, value.var="CPS")
    data$Concentration <- concentration
    
    #test <- apply(test, 2, as.numeric)
    colnames(data) <- make.names(colnames(data))
    data[complete.cases(data),]
}
spectra_table_xrf <- cmpfun(spectra_table_xrf)


spectra_simp_prep_xrf <- function(spectra, energy.min=NULL, energy.max=NULL, compress="100 eV", transformation="None"){
    
    spectra$CPS[spectra$CPS<0] <- 0.0000000000001

    energy.min <- if(is.null(energy.min)){
        0.7
    } else if(!is.null(energy.min)){
        energy.min
    }
    
    energy.max <- if(is.null(energy.max)){
        37
    } else if(!is.null(energy.max)){
        energy.max
    }
    
    compress <- if(is.null(compress)){
        "100 eV"
    } else if(!is.null(compress)){
        compress
    }
    
    transformation <- if(is.null(transformation)){
        "None"
    } else if(!is.null(transformation)){
        transformation
    }
    
    
    spectra <- if(transformation=="None"){
        spectra
    } else if(transformation!="None"){
        transformSpectra(spectra, transformation=transformation)
    }

    spectra$Energy <- if(compress=="100 eV"){spectra$Energy <- round(spectra$Energy, 1)
    } else if(compress=="50 eV"){
        round(spectra$Energy/0.05)*0.05
    } else if(compress=="25 eV"){
        round(spectra$Energy/0.025)*0.025
    }
    
    spectra <- subset(spectra, !(spectra$Energy < energy.min | spectra$Energy > energy.max))

    
    spectra <- data.table(spectra)
    spectra.aggregate <- spectra[, list(CPS=mean(CPS, na.rm = TRUE)), by = list(Spectrum,Energy)]
    
    data <- as.data.frame(dcast.data.table(spectra.aggregate, Spectrum~Energy, value.var="CPS"), stringsAsFactors=FALSE)
    
    #test <- apply(test, 2, as.numeric)
    colnames(data) <- make.names(colnames(data))
    spectra.frame <- do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),0)))
    
    return(spectra.frame)


}
spectra_simp_prep_xrf <- cmpfun(spectra_simp_prep_xrf)


spectra_tc_prep_xrf <- function(spectra, energy.min=NULL, energy.max=NULL, compress="100 eV", transformation="None", compton.type="Raw", deconvolution=NULL){
    
    norm_data <- if(compton.type=="Raw"){
        spectra
    } else if(compton.type=="Baseline"){
        deconvolution$Baseline
    } else if(compton.type=="Net"){
        deconvolution$Spectra
    }
    
    spectra$CPS[spectra$CPS<0] <- 0.0000000000001
    
    energy.min <- if(is.null(energy.min)){
        0.7
    } else if(!is.null(energy.min)){
        energy.min
    }
    
    energy.max <- if(is.null(energy.max)){
        37
    } else if(!is.null(energy.max)){
        energy.max
    }
    
    compress <- if(is.null(compress)){
        "100 eV"
    } else if(!is.null(compress)){
        compress
    }
    
    transformation <- if(is.null(transformation)){
        "None"
    } else if(!is.null(transformation)){
        transformation
    }
    
    
    spectra <- as.data.frame(spectra, stringsAsFactors=FALSE)
    
    spectra <- if(transformation=="None"){
        spectra
    } else if(transformation!="None"){
        transformSpectra(spectra, transformation=transformation)
    }
    
    spectra$Energy <- if(compress=="100 eV"){
        spectra$Energy <- round(spectra$Energy, 1)
    } else if(compress=="50 eV"){
        round(spectra$Energy/0.05)*0.05
    } else if(compress=="25 eV"){
        round(spectra$Energy/0.025)*0.025
    }
    
    norm_data$Energy <- if(compress=="100 eV"){
        norm_data$Energy <- round(spectra$Energy, 1)
    } else if(compress=="50 eV"){
        round(norm_data$Energy/0.05)*0.05
    } else if(compress=="25 eV"){
        round(norm_data$Energy/0.025)*0.025
    }
    
    spectra <- subset(spectra, !(spectra$Energy < energy.min | spectra$Energy > energy.max))
    norm_data <- subset(norm_data, !(norm_data$Energy < energy.min | norm_data$Energy > energy.max))
    
    spectra <- data.table(spectra)
    spectra.aggregate <- spectra[, list(CPS=mean(CPS, na.rm = TRUE)), by = list(Spectrum,Energy)]
    
    norm_data <- data.table(norm_data)
    norm_data.aggregate <- norm_data[, list(CPS=mean(CPS, na.rm = TRUE)), by = list(Spectrum,Energy)]
    
    data <- as.data.frame(dcast.data.table(spectra.aggregate, Spectrum~Energy, value.var="CPS"), stringsAsFactors=FALSE)
    norm_data <- as.data.frame(dcast.data.table(norm_data.aggregate, Spectrum~Energy, value.var="CPS"), stringsAsFactors=FALSE)

    #test <- apply(test, 2, as.numeric)
    colnames(data) <- make.names(colnames(data))
    #data <- data[,complete.cases(data)]
    
    total.counts <- rowSums(norm_data[,-1], na.rm=TRUE)
    
    data <- data.frame(Spectrum=data$Spectrum, data[,-1]/total.counts, stringsAsFactors=FALSE)
    spectra.frame <- do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),0)))
    
    return(spectra.frame)

}
spectra_tc_prep_xrf <- cmpfun(spectra_tc_prep_xrf)


spectra_comp_prep_xrf <- function(spectra, energy.min=NULL, energy.max=NULL, norm.min, norm.max, compress="100 eV", transformation="None", compton.type="Raw", deconvolution=NULL){
    
    spectra$CPS[spectra$CPS<0] <- 0.0000000000001

    norm_data <- if(compton.type=="Raw"){
        spectra
    } else if(compton.type=="Baseline"){
        deconvolution$Baseline
    } else if(compton.type=="Net"){
        deconvolution$Spectra
    }
    
    energy.min <- if(is.null(energy.min)){
        0.7
    } else if(!is.null(energy.min)){
        energy.min
    }
    
    energy.max <- if(is.null(energy.max)){
        37
    } else if(!is.null(energy.max)){
        energy.max
    }
    
    compress <- if(is.null(compress)){
        "100 eV"
    } else if(!is.null(compress)){
        compress
    }
    
    transformation <- if(is.null(transformation)){
        "None"
    } else if(!is.null(transformation)){
        transformation
    }
    
    spectra <- if(transformation=="None"){
        spectra
    } else if(transformation!="None"){
        transformSpectra(spectra, transformation=transformation)
    }

    compton.norm <- subset(norm_data$CPS, !(norm_data$Energy < norm.min | norm_data$Energy > norm.max))
    compton.file <- subset(norm_data$Spectrum, !(norm_data$Energy < norm.min | norm_data$Energy > norm.max))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    
    spectra$Energy <- if(compress=="100 eV"){spectra$Energy <- round(spectra$Energy, 1)
    } else if(compress=="50 eV"){
        round(spectra$Energy/0.05)*0.05
    } else if(compress=="25 eV"){
        round(spectra$Energy/0.025)*0.025
    }
    
    spectra <- subset(spectra, !(spectra$Energy < energy.min | spectra$Energy > energy.max))
    
    spectra <- data.table(spectra)
    spectra.aggregate <- spectra[, list(CPS=mean(CPS, na.rm = TRUE)), by = list(Spectrum,Energy)]
    
    data <- as.data.frame(dcast.data.table(spectra.aggregate, Spectrum~Energy, value.var="CPS"), stringsAsFactors=FALSE)
    #test <- apply(test, 2, as.numeric)
    colnames(data) <- make.names(colnames(data))
    
    data <- data.frame(Spectrum=data$Spectrum, data[,-1]/compton.frame.ag$Compton, stringsAsFactors=FALSE)
    spectra.frame <- do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),0)))
    
    
    return(spectra.frame)
    
}
spectra_comp_prep_xrf <- cmpfun(spectra_comp_prep_xrf)



spectra_simp_trans_xrf <- function(spectra, energy.min=0.2, energy.max=40, compress="100 eV", transformation="None"){
    
    spectra$CPS[spectra$CPS<0] <- 0.0000000000001

    
    spectra <- if(transformation=="None"){
        spectra
    } else if(transformation!="None"){
        transformSpectra(spectra, transformation=transformation)
    }
    
    
    spectra$Energy <- if(compress=="100 eV"){spectra$Energy <- round(spectra$Energy, 1)
    } else if(compress=="50 eV"){
        round(spectra$Energy/0.05)*0.05
    } else if(compress=="25 eV"){
        round(spectra$Energy/0.025)*0.025
    }
    
    spectra <- subset(spectra, !(spectra$Energy < energy.min | spectra$Energy > energy.max))
    spectra <- data.table(spectra)
    spectra.aggregate <- spectra[, list(CPS=mean(CPS, na.rm = TRUE)), by = list(Spectrum,Energy)]
    
    data <- as.data.frame(dcast.data.table(spectra.aggregate, Spectrum~Energy, value.var="CPS"), stringsAsFactors=FALSE)
    
    #test <- apply(test, 2, as.numeric)
    colnames(data) <- make.names(colnames(data))
    first.pass <- do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),0)))
    first.pass <- data.table(first.pass)
    
    
    
    first.pass.t <- as.data.frame(data.table::transpose(first.pass), stringsAsFactors=FALSE)
    names <- as.vector(unlist(first.pass.t[1,]))
    first.pass.t.frame <- first.pass.t[-1,]
    colnames(first.pass.t.frame) <- names
    first.pass.t.frame <- apply(first.pass.t.frame, 2, as.numeric)
    
    
    data.frame(Energy=as.numeric(gsub("X", "", colnames(data)))[-1], first.pass.t.frame, stringsAsFactors=FALSE)
    
}
spectra_simp_trans_xrf <- cmpfun(spectra_simp_trans_xrf)


spectra_tc_trans_xrf <- function(spectra, energy.min=0.7, energy.max=37, compress="100 eV", transformation="None"){
    
    spectra$CPS[spectra$CPS<0] <- 0.0000000000001

    
    spectra <- if(transformation=="None"){
        spectra
    } else if(transformation!="None"){
        transformSpectra(spectra, transformation=transformation)
    }
    
    spectra$Energy <- if(compress=="100 eV"){spectra$Energy <- round(spectra$Energy, 1)
    } else if(compress=="50 eV"){
        round(spectra$Energy/0.05)*0.05
    } else if(compress=="25 eV"){
        round(spectra$Energy/0.025)*0.025
    }
    
    spectra <- subset(spectra, !(spectra$Energy < energy.min | spectra$Energy > energy.max))
    
    spectra <- data.table(spectra)
    spectra.aggregate <- spectra[, list(CPS=mean(CPS, na.rm = TRUE)), by = list(Spectrum,Energy)]
    
    data <- as.data.frame(dcast.data.table(spectra.aggregate, Spectrum~Energy, value.var="CPS"), stringsAsFactors=FALSE)
    
    #test <- apply(test, 2, as.numeric)
    colnames(data) <- make.names(colnames(data))
    data <- data[complete.cases(data),]
    
    total.counts <- rowSums(data[,-1], na.rm=TRUE)
    
    data <- data.frame(Spectrum=data$Spectrum, data[,-1]/total.counts, stringsAsFactors=FALSE)
    first.pass <- do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),0)))
    first.pass <- data.table(first.pass)
    
    
    
    first.pass.t <- as.data.frame(data.table::transpose(first.pass), stringsAsFactors=FALSE)
    names <- as.vector(unlist(first.pass.t[1,]))
    first.pass.t.frame <- first.pass.t[-1,]
    colnames(first.pass.t.frame) <- names
    first.pass.t.frame <- apply(first.pass.t.frame, 2, as.numeric)
    
    
    data.frame(Energy=as.numeric(gsub("X", "", colnames(data)))[-1], first.pass.t.frame, stringsAsFactors=FALSE)
}
spectra_tc_trans_xrf <- cmpfun(spectra_tc_trans_xrf)


spectra_comp_trans_xrf <- function(spectra, energy.min=0.7, energy.max=37, norm.min, norm.max, compress="100 eV", transformation="None"){
    
    spectra$CPS[spectra$CPS<0] <- 0.0000000000001

    
    spectra <- if(transformation=="None"){
        spectra
    } else if(transformation!="None"){
        transformSpectra(spectra, transformation=transformation)
    }
    
    
    compton.norm <- subset(spectra$CPS, !(spectra$Energy < norm.min | spectra$Energy > norm.max))
    compton.file <- subset(spectra$Spectrum, !(spectra$Energy < norm.min | spectra$Energy > norm.max))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    
    
    spectra$Energy <- if(compress=="100 eV"){spectra$Energy <- round(spectra$Energy, 1)
    } else if(compress=="50 eV"){
        round(spectra$Energy/0.05)*0.05
    } else if(compress=="25 eV"){
        round(spectra$Energy/0.025)*0.025
    }
    
    spectra <- subset(spectra, !(spectra$Energy < energy.min | spectra$Energy > energy.max))
    
    spectra <- data.table(spectra)
    spectra.aggregate <- spectra[, list(CPS=mean(CPS, na.rm = TRUE)), by = list(Spectrum,Energy)]
    
    data <- as.data.frame(dcast.data.table(spectra.aggregate, Spectrum~Energy, value.var="CPS"), stringsAsFactors=FALSE)
    #test <- apply(test, 2, as.numeric)
    colnames(data) <- make.names(colnames(data))
    
    data <- data.frame(Spectrum=data$Spectrum, data[,-1]/compton.frame.ag$Compton, stringsAsFactors=FALSE)
    first.pass <- do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),0)))
    first.pass <- data.table(first.pass)
    
    
    
    first.pass.t <- as.data.frame(data.table::transpose(first.pass), stringsAsFactors=FALSE)
    names <- as.vector(unlist(first.pass.t[1,]))
    first.pass.t.frame <- first.pass.t[-1,]
    colnames(first.pass.t.frame) <- names
    first.pass.t.frame <- apply(first.pass.t.frame, 2, as.numeric)
    
    
    data.frame(Energy=as.numeric(gsub("X", "", colnames(data)))[-1], first.pass.t.frame, stringsAsFactors=FALSE)
}
spectra_comp_trans_xrf <- cmpfun(spectra_comp_trans_xrf)


###############
###Prep Data###
###############


###############
###Raw Spectra##
###############


general_prep_xrf <- function(spectra.line.table, element.line) {
    
    intensity <- spectra.line.table[,element.line]
    
    
    data.frame(Intensity=spectra.line.table[,element.line], stringsAsFactors=FALSE)

}
general_prep_xrf <- cmpfun(general_prep_xrf)


simple_tc_prep_xrf <- function(data,spectra.line.table, element.line, deconvolution=NULL, compton.type="Raw") {
    
    data <- if(compton.type=="Raw"){
        data
    } else if(compton.type=="Baseline"){
        deconvolution$Baseline
    } else if(compton.type=="Net"){
        deconvolution$Spectra
    }
    
    intensity <- spectra.line.table[,element.line]
    
    
    total.counts <- aggregate(CPS~Spectrum, data=data, sum)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    
    predict.frame.tc <- data.frame(Intensity=intensity/total.counts$CPS, stringsAsFactors=FALSE)
    
    predict.frame.tc
}
simple_tc_prep_xrf <- cmpfun(simple_tc_prep_xrf)


simple_comp_prep_xrf <- function(data, spectra.line.table, deconvolution=NULL, element.line, norm.min, norm.max, compton.type="Raw") {
    
    data <- if(compton.type=="Raw"){
        data
    } else if(compton.type=="Baseline"){
        deconvolution$Baseline
    } else if(compton.type=="Net"){
        deconvolution$Spectra
    }
    
    
    intensity <- spectra.line.table[,element.line]
    
    
    compton.norm <- subset(data$CPS, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.file <- subset(data$Spectrum, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    
    compton.frame.ag[compton.frame.ag ==0 ] <- 1

    
    predict.frame.comp <- data.frame(Intensity=intensity/compton.frame.ag$Compton, stringsAsFactors=FALSE)
    
    predict.frame.comp
    
}
simple_comp_prep_xrf <- cmpfun(simple_comp_prep_xrf)

just_spectra_summary_apply <- function(spectra.frame, normalization, min=NULL, max=NULL, compress="100 eV", transformation="None", energy.range=c(0.7, 37), compton.type="Raw", deconvolution=NULL){
    
    new.spectrum <- if(normalization==1){
        spectra_simp_prep_xrf(spectra=spectra.frame, compress=compress, transformation=transformation, energy.min=energy.range[1], energy.max=energy.range[2])
    } else if(normalization==2){
        spectra_tc_prep_xrf(spectra=spectra.frame, compress=compress, transformation=transformation, energy.min=energy.range[1], energy.max=energy.range[2], compton.type=compton.type, deconvolution=deconvolution)
    } else if(normalization==3){
        spectra_comp_prep_xrf(spectra=spectra.frame, norm.min=min, norm.max=max, compress=compress, transformation=transformation, energy.min=energy.range[1], energy.max=energy.range[2], compton.type=compton.type, deconvolution=deconvolution)
    }
    
    newer.spectrum <- reshape2::melt(new.spectrum, id.var="Spectrum")
    colnames(newer.spectrum) <- c("Spectrum", "Energy", "CPS")
    newer.spectrum$Energy <- as.numeric(gsub("X", "", newer.spectrum$Energy))
    newer.spectrum
}
just_spectra_summary_apply <- cmpfun(just_spectra_summary_apply)



###Prep Data



lucas_simp_prep_xrf <- function(spectra.line.table, element.line, slope.element.lines, intercept.element.lines=NULL) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    if(!is.null(intercept.element.lines)){
        intercept.none <- rep(0, length(spectra.line.table[,1]))
        lucas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none, stringsAsFactors=FALSE)
        colnames(lucas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
        lucas.intercept.table <- data.frame(first=rowSums(lucas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")]), stringsAsFactors=FALSE)
        lucas.intercept <- lucas.intercept.table$first
    }
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lucas.slope.table <- data.frame(spectra.line.table, slope.none, stringsAsFactors=FALSE)
    colnames(lucas.slope.table) <- c(names(spectra.line.table), "None")
    
    lucas.slope <- data.frame(lucas.slope.table[,slope.element.lines], stringsAsFactors=FALSE)
    colnames(lucas.slope) <- slope.element.lines
    
    predict.frame.luk <- if(is.null(intercept.element.lines)){
        data.frame(Intensity=intensity,lucas.slope, stringsAsFactors=FALSE)
    } else if(!is.null(intercept.element.lines)){
        data.frame(Intensity=((1+intensity/(intensity+lucas.intercept))-lucas.intercept/(intensity+lucas.intercept)),lucas.slope, stringsAsFactors=FALSE)
    }
    
    predict.frame.luk
    
    
}
lucas_simp_prep_xrf <- cmpfun(lucas_simp_prep_xrf)


lucas_tc_prep_xrf <- function(data, spectra.line.table, deconvolution=NULL, element.line, slope.element.lines, intercept.element.lines=NULL, compton.type="Raw") {
    
    data <- if(compton.type=="Raw"){
        data
    } else if(compton.type=="Baseline"){
        deconvolution$Baseline
    } else if(compton.type=="Net"){
        deconvolution$Spectra
    }
    
    intensity <- spectra.line.table[,element.line]
    
    
    total.counts <- aggregate(CPS~Spectrum, data=data, sum)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    if(!is.null(intercept.element.lines)){
        intercept.none <- rep(0, length(spectra.line.table[,1]))
        lucas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none, stringsAsFactors=FALSE)
        colnames(lucas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
        lucas.intercept.table.tc <- data.frame(rowSums(lucas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")]), stringsAsFactors=FALSE)/total.counts$CPS
        colnames(lucas.intercept.table.tc) <- c("first")
        lucas.intercept.tc <- lucas.intercept.table.tc$first
    }
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lucas.slope.table <- data.frame(spectra.line.table, slope.none, stringsAsFactors=FALSE)
    colnames(lucas.slope.table) <- c(names(spectra.line.table), "None")
    
    lucas.slope.tc <- data.frame(lucas.slope.table[,slope.element.lines], stringsAsFactors=FALSE)/total.counts$CPS
    colnames(lucas.slope.tc) <- slope.element.lines
    
    predict.intensity.luc.tc <- if(is.null(intercept.element.lines)){
        data.frame(Intensity=intensity,lucas.slope.tc, stringsAsFactors=FALSE)
    } else if(!is.null(intercept.element.lines)){
        data.frame(Intensity=((1+intensity/(intensity+lucas.intercept.tc))-lucas.intercept.tc/(intensity+lucas.intercept.tc)),lucas.slope.tc, stringsAsFactors=FALSE)
    }
    
    predict.intensity.luc.tc
}
lucas_tc_prep_xrf <- cmpfun(lucas_tc_prep_xrf)





lucas_comp_prep_xrf <- function(data, spectra.line.table, deconvolution=NULL, element.line, slope.element.lines, intercept.element.lines=NULL, norm.min, norm.max, compton.type="Raw") {
    
    data <- if(compton.type=="Raw"){
        data
    } else if(compton.type=="Baseline"){
        deconvolution$Baseline
    } else if(compton.type=="Net"){
        deconvolution$Spectra
    }
    
    intensity <- spectra.line.table[,element.line]
    
    
    
    compton.norm <- subset(data$CPS, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.file <- subset(data$Spectrum, !(data$Energy < norm.min | data$Energy > norm.max))
    compton.frame <- data.frame(is.0(compton.norm, compton.file))
    colnames(compton.frame) <- c("Compton", "Spectrum")
    compton.frame.ag <- aggregate(list(compton.frame$Compton), by=list(compton.frame$Spectrum), FUN="sum")
    colnames(compton.frame.ag) <- c("Spectrum", "Compton")
    compton.frame.ag[compton.frame.ag ==0 ] <- 1
    
    if(!is.null(intercept.element.lines)){
        intercept.none <- rep(0, length(spectra.line.table[,1]))
        lucas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none, stringsAsFactors=FALSE)
        colnames(lucas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
        lucas.intercept.table.comp <- data.frame(rowSums(lucas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")])/compton.frame.ag$Compton, stringsAsFactors=FALSE)
        colnames(lucas.intercept.table.comp) <- c("first")
        lucas.intercept.comp <- lucas.intercept.table.comp$first
    }
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lucas.slope.table <- data.frame(spectra.line.table, slope.none, stringsAsFactors=FALSE)
    colnames(lucas.slope.table) <- c(names(spectra.line.table), "None")
    
    lucas.slope.comp <- data.frame(lucas.slope.table[,slope.element.lines]/compton.frame.ag$Compton, stringsAsFactors=FALSE)
    colnames(lucas.slope.comp) <- slope.element.lines
    
    predict.frame.luc.comp <- if(is.null(intercept.element.lines)){
        data.frame(Intensity=intensity/compton.frame.ag$Compton,lucas.slope.comp, stringsAsFactors=FALSE)
    } else if(!is.null(intercept.element.lines)){
        data.frame(Intensity=((1+intensity/compton.frame.ag$Compton)/(intensity/compton.frame.ag$Compton+lucas.intercept.comp)-lucas.intercept.comp/(intensity/compton.frame.ag$Compton+lucas.intercept.comp)),lucas.slope.comp, stringsAsFactors=FALSE)
    }

    predict.frame.luc.comp
}
lucas_comp_prep_xrf <- cmpfun(lucas_comp_prep_xrf)




###############
###Prep Data###
###############


###############
###Net Counts##
###############


general_prep_xrf_net <- function(spectra.line.table, element.line) {
    
    intensity <- spectra.line.table[,element.line]
    
    
    predict.frame <- data.frame(Intensity=intensity, stringsAsFactors=FALSE)
    
    predict.frame
}
general_prep_xrf_net <- cmpfun(general_prep_xrf_net)


simple_tc_prep_xrf_net <- function(data,spectra.line.table, deconvolution=NULL, element.line, compton.type="Raw") {
    
    data <- if(compton.type=="Raw"){
        data
    } else if(compton.type=="Baseline"){
        deconvolution$Baseline
    } else if(compton.type=="Net"){
        deconvolution$Spectra
    }
    
    intensity <- spectra.line.table[,element.line]
    
    total.counts.net <- rowSums(spectra.line.table[,-1])
    total.counts <- data.frame(data$Spectrum, total.counts.net, stringsAsFactors=FALSE)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    
    predict.frame.tc <- data.frame(Intensity=intensity/total.counts$CPS, stringsAsFactors=FALSE)
    
    predict.frame.tc
}
simple_tc_prep_xrf_net <- cmpfun(simple_tc_prep_xrf_net)


simple_comp_prep_xrf_net <- function(data, spectra.line.table, deconvolution=NULL, element.line, norm.min, norm.max, compton.type="Raw") {
    
    data <- if(compton.type=="Raw"){
        data
    } else if(compton.type=="Baseline"){
        deconvolution$Baseline
    } else if(compton.type=="Net"){
        deconvolution$Spectra
    }
    
    intensity <- spectra.line.table[,element.line]
    
    
    compton.ag.fake.Spectrum <- data$Spectrum
    compton.ag.fake.Compton <- rep(1, length(data$Spectrum))
    compton.ag.fake <- data.frame(compton.ag.fake.Spectrum,compton.ag.fake.Compton, stringsAsFactors=FALSE)
    colnames(compton.ag.fake) <- c("Spectrum", "Compton")
    
    predict.frame.comp <- data.frame(Intensity=intensity/compton.ag.fake$Compton, stringsAsFactors=FALSE)

    predict.frame.comp
    
}
simple_comp_prep_xrf_net <- cmpfun(simple_comp_prep_xrf_net)



###Prep Data



lucas_simp_prep_xrf_net <- function(spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
    intensity <- spectra.line.table[,element.line]
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lucas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none, stringsAsFactors=FALSE)
    colnames(lucas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lucas.slope.table <- data.frame(spectra.line.table, slope.none, stringsAsFactors=FALSE)
    colnames(lucas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    lucas.intercept.table <- data.frame(rowSums(lucas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")]), stringsAsFactors=FALSE)
    colnames(lucas.intercept.table) <- c("first")
    
    
    
    lucas.intercept <- lucas.intercept.table$first
    lucas.slope <- data.frame(lucas.slope.table[,slope.element.lines], stringsAsFactors=FALSE)
    colnames(lucas.slope) <- slope.element.lines
    
    
    
    predict.frame.luk <- data.frame(Intensity=((1+intensity/(intensity+lucas.intercept))-lucas.intercept/(intensity+lucas.intercept)),lucas.slope, stringsAsFactors=FALSE)
    
    
    
    
    predict.frame.luk
    
    
}
lucas_simp_prep_xrf_net <- cmpfun(lucas_simp_prep_xrf_net)



lucas_tc_prep_xrf_net <- function(data, spectra.line.table, deconvolution=NULL, element.line, slope.element.lines, intercept.element.lines, compton.type="Raw") {
    
    data <- if(compton.type=="Raw"){
        data
    } else if(compton.type=="Baseline"){
        deconvolution$Baseline
    } else if(compton.type=="Net"){
        deconvolution$Spectra
    }
    
    intensity <- spectra.line.table[,element.line]
    
    
    total.counts.net <- rowSums(spectra.line.table[,-1])
    total.counts <- data.frame(data$Spectrum, total.counts.net, stringsAsFactors=FALSE)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lucas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none, stringsAsFactors=FALSE)
    colnames(lucas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lucas.slope.table <- data.frame(spectra.line.table, slope.none, stringsAsFactors=FALSE)
    colnames(lucas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    
    lucas.intercept.table.tc <- data.frame(rowSums(lucas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")]), stringsAsFactors=FALSE)/total.counts$CPS
    colnames(lucas.intercept.table.tc) <- c("first")
    
    
    
    
    lucas.intercept.tc <- lucas.intercept.table.tc$first
    lucas.slope.tc <- data.frame(lucas.slope.table[,slope.element.lines], stringsAsFactors=FALSE)/total.counts$CPS
    colnames(lucas.slope.tc) <- slope.element.lines
    
    
    predict.intensity.luc.tc <- data.frame(Intensity=((1+intensity/(intensity+lucas.intercept.tc)-lucas.intercept.tc/(intensity+lucas.intercept.tc))),lucas.slope.tc, stringsAsFactors=FALSE)
    
    predict.intensity.luc.tc
}
lucas_tc_prep_xrf_net <- cmpfun(lucas_tc_prep_xrf_net)


lucas_comp_prep_xrf_net <- function(data, spectra.line.table, deconvolution=NULL, element.line, slope.element.lines, intercept.element.lines, norm.min, norm.max, compton.type="Raw") {
    
    data <- if(compton.type=="Raw"){
        data
    } else if(compton.type=="Baseline"){
        deconvolution$Baseline
    } else if(compton.type=="Net"){
        deconvolution$Spectra
    }
    
    intensity <- spectra.line.table[,element.line]
    
    
    
    
    compton.ag.fake.Spectrum <- data$Spectrum
    compton.ag.fake.Compton <- rep(1, length(data$Spectrum))
    compton.ag.fake <- data.frame(compton.ag.fake.Spectrum,compton.ag.fake.Compton, stringsAsFactors=FALSE)
    colnames(compton.ag.fake) <- c("Spectrum", "Compton")
    
    
    intercept.none <- rep(0, length(spectra.line.table[,1]))
    lucas.intercept.table.x <- data.frame(spectra.line.table, intercept.none, intercept.none, stringsAsFactors=FALSE)
    colnames(lucas.intercept.table.x) <- c(names(spectra.line.table), "None", "NoneNull")
    
    
    
    
    slope.none <- rep(1, length(spectra.line.table[,1]))
    lucas.slope.table <- data.frame(spectra.line.table, slope.none, stringsAsFactors=FALSE)
    colnames(lucas.slope.table) <- c(names(spectra.line.table), "None")
    
    
    
    lucas.intercept.table.comp <- data.frame(rowSums(lucas.intercept.table.x[,c(intercept.element.lines, "None", "NoneNull")]), stringsAsFactors=FALSE)/compton.ag.fake$Compton
    colnames(lucas.intercept.table.comp) <- c("first")
    
    
    
    
    lucas.intercept.comp <- lucas.intercept.table.comp$first
    lucas.slope.comp <- data.frame(lucas.slope.table[,slope.element.lines], stringsAsFactors=FALSE)/compton.ag.fake$Compton
    colnames(lucas.slope.comp) <- slope.element.lines
    
    

    predict.frame.luc.comp <- data.frame(Intensity=((1+intensity/(intensity+lucas.intercept.comp)-lucas.intercept.comp/(intensity+lucas.intercept.comp))),lucas.slope.comp, stringsAsFactors=FALSE)
    
    
    predict.frame.luc.comp
}
lucas_comp_prep_xrf_net <- cmpfun(lucas_comp_prep_xrf_net)



blank.data.frame <- data.frame(rep(0, length(standard)), rep(0, length(standard)), rep(0, length(standard)), rep(0, length(standard)), rep(0, length(standard)), rep(0, length(standard)), rep(0, length(standard)), stringsAsFactors=FALSE)
colnames(blank.data.frame) <- standard


combos.xrf <- function(a.vector){
    
    so <- seq(from=2, to=length(a.vector), by=1)
    
    long <- pblapply(so, function(x) combnPrim(x=a.vector, m=x), cl=6L)
    and <- pblapply(long, function(x) plyr::alply(x, 2), cl=6L)
    thanks.for.all.the.fish <- do.call(list, unlist(and, recursive=FALSE))
    
    thanks.for.all.the.fish
    
}
combos.xrf <- cmpfun(combos.xrf)


create.frame.slopes.xrf <- function(element, slopes, values, intensities){
    values <- values[complete.cases(values[,element]),]
    intensities <- intensities[complete.cases(values[,element]),]
    
    data.frame(Value=values[,element],
    Intensity=intensities[,"Intensity"],
    intensities[,slopes], stringsAsFactors=FALSE)
    
}
create.frame.slopes.xrf <- cmpfun(create.frame.slopes.xrf)


create.frame.intercepts.xrf <- function(element, slopes, values, intensities){
    
    data.frame(Value=values[,element],
    Intensity=intensities[,"Intensity"],
    intensities[,slopes], stringsAsFactors=FALSE)
    
}
create.frame.intercepts.xrf <- cmpfun(create.frame.intercepts.xrf)



optimal_r_chain.xrf <- function(element, intensities, values, possible.slopes, keep){
    
    values <- values[complete.cases(values[,element]),]
    intensities <- intensities[complete.cases(values[,element]),]
    index <- seq(1, length(possible.slopes), 1)
    
    chain.lm <- pbapply::pblapply(possible.slopes, function(x) lm(Value~Intensity+., data=create.frame.slopes(element=element, slopes=x, values=values[keep,], intensities=intensities)[keep,]))
    
    #chain.predict <- pblapply(index, function(x) predict(object=chain.lm[[x]], newdata=create.frame.slopes(element=element, slopes=possible.slopes[[x]], values=values[keep,], intensities=intensities)[keep,], interval='confidence'))
    #chain.fits <- pblapply(chain.predict, function(x) data.frame(x)$fit)
    #val.lm <- pblapply(chain.fits, function(x) lm(values[,element]~x))
    
    aic <- lapply(chain.lm, function(x) extractAIC(x, k=log(length(possible.slopes)))[2])
    best <- chain.lm[[which.min(unlist(aic))]]
    best.aic <- unlist(aic)[which.min(unlist(aic))]
    #r.adj <- lapply(chain.lm, function(x) summary(x)$adj.r.squared)
    #best <- chain.lm[[which.max(unlist(r.adj))]]
    coef <- data.frame(best$coefficients, stringsAsFactors=FALSE)
    best.var <- rownames(coef)[3:length(rownames(coef))]
    
    simple.lm <- lm(Value~Intensity, data=create.frame.slopes(element=element, slopes=element, values=values, intensities=intensities)[keep,])
    #simple.predict <- as.data.frame(predict(simple.lm, newdata=create.frame.slopes(element=element, slopes=element, values=values[keep,], intensities=intensities)[keep,], interval='confidence'), interval='confidence')$fit
    #simple.val <- lm(values[,element]~simple.predict)
    simple.aic <- extractAIC(simple.lm, k=log(length(1)))[2]
    
    if(simple.aic <= best.aic){
           element
        } else if(best.aic < simple.aic){
           best.var
       }
    
    #best.var
}
optimal_r_chain.xrf <- cmpfun(optimal_r_chain.xrf)



optimal_norm_chain_xrf <- function(data, element, spectra.line.table, values, possible.mins, possible.maxs){
    
    index <- seq(1, length(possible.mins), 1)
    
    chain.lm <- pbapply::pblapply(index, function(x) lm(values[,element]~simple_comp_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=element, norm.min=possible.mins[x], norm.max=possible.maxs[x])$Intensity, na.action=na.exclude))
    aic <- lapply(chain.lm, function(x) extractAIC(x, k=log(length(1)))[2])
    best <- index[[which.min(unlist(aic))]]

    
    best
    
}
optimal_norm_chain_xrf <- cmpfun(optimal_norm_chain_xrf)


optimal_intercept_chain_xrf <- function(element, intensities, values, keep){
    
    
    chain.lm <- pbapply::pblapply(intensities, function(x) lm(values[,element]~Intensity, data=x[keep,]))
    aic <- lapply(chain.lm, function(x) extractAIC(x, k=log(1))[2])
    best <- chain.lm[[which.min(unlist(aic))]]
    coef <- data.frame(best$coefficients, stringsAsFactors=FALSE)
    best.var <- rownames(coef)[3:length(rownames(coef))]
    
    best.var
    
}
optimal_intercept_chain_xrf <- cmpfun(optimal_intercept_chain_xrf)


likely_intercepts_xrf <- function(element){
    
    if(element=="Na.K.alpha"){
        c("Cl.K.alpha", "Rh.L.alpha")
    } else if(element=="Mg.K.alpha"){
        c("Rh.L.alpha", "Al.K.alpha", "Cl.K.alpha")
    } else if(element=="Al.K.alpha"){
        c("Mg.K.alpha", "Si.K.alpha", "K.K.alpha")
    } else if(element=="Si.K.alpha"){
        c("Al.K.alpha", "Ca.K.alpha")
    } else if(element=="P.K.alpha"){
        c("Ca.K.alpha", "Si.K.alpha", "S.K.alpha")
    } else if(element=="S.K.alpha"){
        c("Rh.L.alpha", "P.K.alpha", "Cl.K.alpha")
    } else if(element=="Cl.K.alpha"){
        c("Rh.L.alpha", "S.K.alpha")
    } else if(element=="K.K.alpha"){
        c("Rh.L.alpha", "Ag.L.alpha", "Cd.L.alpha")
    } else if(element=="Ca.K.alpha"){
        c("K.K.alpha", "Ag.L.alpha", "Cd.L.alpha")
    } else if(element=="Sc.K.alpha"){
        c("Ca.K.alpha", "Cd.L.alpha")
    } else if(element=="Ti.K.alpha"){
        c("Ba.L.alpha", "Fe.K.alpha")
    } else if(element=="V.K.alpha"){
        c("Ba.L.alpha", "Ti.K.alpha")
    } else if(element=="Cr.K.alpha"){
        c("Ba.L.alpha", "V.K.alpha")
    } else if(element=="Mn.K.alpha"){
        c("Cr.K.alpha", "Ba.L.alpha")
    } else if(element=="Mn.K.alpha"){
        c("Cr.K.alpha", "Ba.L.alpha", "Fe.K.alpha")
    } else if(element=="Fe.K.alpha"){
        c("Mn.K.alpha", "K.K.alpha", "Cu.K.alpha", "Ca.K.alpha")
    } else if(element=="Co.K.alpha"){
        c("Fe.K.alpha", "Ca.K.alpha", "Zn.K.alpha")
    } else if(element=="Ni.K.alpha"){
        c("Co.K.alpha", "Ca.K.alpha", "Zn.K.alpha")
    } else if(element=="Cu.K.alpha"){
        c("Ni.K.alpha", "Zn.K.alpha")
    } else if(element=="Zn.K.alpha"){
        c("Cu.K.alpha", "Pb.L.alpha", "Au.L.alpha")
    } else if(element=="Ga.K.alpha"){
        c("Zn.K.alpha", "Au.L.alpha", "Pb.L.alpha")
    } else if(element=="As.K.alpha"){
        c("Pb.L.beta", "Cr.K.alpha")
    } else if(element=="Rb.K.alpha"){
        c("Th.L.alpha", "Fe.K.alpha")
    } else if(element=="Sr.K.alpha"){
        c("U.L.alpha", "Zr.K.alpha", "Co.K.alpha")
    } else if(element=="Y.K.alpha"){
        c("Rb.K.alpha", "Ni.K.alpha", "Nb.K.alpha")
    } else if(element=="Zr.K.alpha"){
        c("Sr.K.alpha", "Cu.K.alpha", "Mo.K.alpha")
    } else if(element=="Nb.K.alpha"){
        c("Y.K.alpha", "Cu.K.alpha", "Zn.K.alpha", "Rh.K.alpha")
    } else if(element=="Mo.K.alpha"){
        c("Zr.K.alpha", "Rh.K.alpha", "Zn.K.alpha")
    } else if(element=="Ag.K.alpha"){
        c("Rh.K.alpha", "Pd.K.alpha")
    } else if(element=="Cd.K.alpha"){
        c("Rh.K.alpha", "Pd.K.alpha")
    } else if(element=="Sn.K.alpha"){
        c("Rh.K.alpha", "Ag.K.alpha")
    } else if(element=="Sb.K.alpha"){
        c("Sn.K.alpha", "Rh.K.alpha")
    } else if(element=="Ba.L.alpha"){
        c("Ti.K.alpha", "Fe.K.alpha")
    } else if(element=="La.L.alpha"){
        c("Ti.K.alpha", "Fe.K.alpha")
    } else if(element=="Ce.L.alpha"){
        c("Ti.K.alpha", "V.K.alpha", "Ba.L.alpha", "Fe.K.alpha")
    } else if(element=="Nd.L.alpha"){
        c("Ti.K.alpha", "Cr.K.alpha", "V.K.alpha", "Ba.L.alpha", "Fe.K.alpha")
    } else if(element=="W.L.alpha"){
        c("Cu.K.alpha", "Ni.K.alpha")
    } else if(element=="Au.L.alpha"){
        c("Zn.K.alpha", "Ga.K.alpha", "W.L.alpha")
    } else if(element=="Hg.L.alpha"){
        c("Pb.L.alpha", "Au.L.alpha")
    } else if(element=="Pb.L.beta"){
        c("As.K.alpha", "Th.L.alpha")
    } else if(element=="Th.L.alpha"){
        c("Rb.K.alpha", "Pb.L.alpha")
    } else if(element=="U.L.alpha"){
        c("Rb.K.alpha", "Sr.K.alpha")
    }
}
likely_intercepts_xrf <- cmpfun(likely_intercepts_xrf)


peak_threshold_xrf <- function(spectrum){
    
    spectrum$Hodder <- Hodder.v(Hodder.v(spectrum$CPS))*-1
    spectrum$Peaks <- ifelse(spectrum$Hodder > 0, spectrum$Hodder, 0)
    spectrum$isPeak <- ifelse(log(spectrum$Peaks) > 1, TRUE, FALSE)
    ggplot(spectrum) + geom_line(aes(Energy, Peaks)) + theme_light() + scale_y_log10()
    ggplot(spectrum) + geom_line(aes(Energy, CPS)) + theme_light() + geom_point(data=spectrum[spectrum$isPeak,], aes(Energy, CPS), colour="red", alpha=0.5)
    
}
peak_threshold_xrf <- cmpfun(peak_threshold_xrf)



find_peaks_xrf <- function(spectrum){
    
    #spectrum$Hodder <- Hodder.v(spectrum$CPS)
    #spectrum$Peak <- ifelse(spectrum$Hodder < (-200), TRUE, FALSE)
    #ggplot(spectrum) + geom_line(aes(Energy, Hodder)) + theme_light() + geom_point(data=spectrum[spectrum$Peak,], aes(Energy, Hodder), colour="red", alpha=0.5)
    
    #spectrum$Hodder2 <- Hodder.v(spectrum$Hodder)
    #ggplot(spectrum) + geom_line(aes(Energy, Hodder2)) + theme_light()

    #spectrum$Peak <- ifelse(spectrum$Hodder2 < (-1), TRUE, FALSE)
    #ggplot(spectrum) + geom_line(aes(Energy, Hodder2)) + theme_light() + geom_point(data=spectrum[spectrum$Peak,], aes(Energy, Hodder2), colour="red", alpha=0.5)


    spectrum$Hodder <- Hodder.v(Hodder.v(spectrum$CPS))
    spectrum$Peak <- ifelse(spectrum$Hodder < (-1), TRUE, FALSE)
    data.frame(Energy=spectrum[spectrum$Peak,]$Energy, CPS=spectrum[spectrum$Peak,]$CPS, stringsAsFactors=FALSE)

}
find_peaks_xrf <- cmpfun(find_peaks_xrf)



###Unit Transformation

data_summarize <- function(xrf.table) {
    
    xrf.table
    
    xrf.table$Depth <- round(xrf.table$Depth, 1)
    #xrf.table <- subset(xrf.table, !(xrf.table$Depth < 5 | xrf.table$Depth > 37))
    
    xrf.table <- data.table(xrf.table)
    ###Neds work
    xrf.table.aggregate <- xrf.table[, list(CPS=mean(CPS, na.rm = TRUE)), by = list(Depth)]
    
    data <- as.data.frame(dcast.data.table(xrf.table.aggregate, Spectrum~Energy, value.var="CPS"), stringsAsFactors=FALSE)

    data
    
    
}
data_summarize <- cmpfun(data_summarize)


plot.nnet<-function(mod.in,nid=T,all.out=T,all.in=T,bias=T,wts.only=F,rel.rsc=5,
circle.cex=5,node.labs=T,var.labs=T,x.lab=NULL,y.lab=NULL,
line.stag=NULL,struct=NULL,cex.val=1,alpha.val=1,
circle.col='lightblue',pos.col='black',neg.col='grey',
bord.col='lightblue', max.sp = F,...){
    
    require(scales)
    
    #sanity checks
    if('mlp' %in% class(mod.in)) warning('Bias layer not applicable for rsnns object')
    if('numeric' %in% class(mod.in)){
        if(is.null(struct)) stop('Three-element vector required for struct')
        if(length(mod.in) != ((struct[1]*struct[2]+struct[2]*struct[3])+(struct[3]+struct[2])))
        stop('Incorrect length of weight matrix for given network structure')
    }
    if('train' %in% class(mod.in)){
        if('nnet' %in% class(mod.in$finalModel)){
            mod.in<-mod.in$finalModel
            warning('Using best nnet model from train output')
        } else if('nn' %in% class(mod.in$finalModel)){
            mod.o <- mod.in
            mod.in<-mod.in$finalModel
            warning('Using best nn model from train output')
        }
        else stop('Only nnet method can be used with train object')
    }
    
    #gets weights for neural network, output is list
    #if rescaled argument is true, weights are returned but rescaled based on abs value
    nnet.vals <- function(mod.in,nid,rel.rsc,struct.out=struct){
        
        require(scales)
        require(reshape)
        
        if('numeric' %in% class(mod.in)){
            struct.out<-struct
            wts<-mod.in
        }
        
        #neuralnet package
        if('nn' %in% class(mod.in)){
            struct.out<-unlist(lapply(mod.in$weights[[1]],ncol))
            struct.out<-struct.out[-length(struct.out)]
            struct.out<-c(
            length(mod.in$model.list$variables),
            struct.out,
            length(mod.in$model.list$response)
            )
            wts<-unlist(mod.in$weights[[1]])
        }
        
        #nnet package
        if('nnet' %in% class(mod.in)){
            struct.out<-mod.in$n
            wts<-mod.in$wts
        }
        
        #RSNNS package
        if('mlp' %in% class(mod.in)){
            struct.out<-c(mod.in$nInputs,mod.in$archParams$size,mod.in$nOutputs)
            hid.num<-length(struct.out)-2
            wts<-mod.in$snnsObject$getCompleteWeightMatrix()
            
            #get all input-hidden and hidden-hidden wts
            inps<-wts[grep('Input',row.names(wts)),grep('Hidden_2',colnames(wts)),drop=F]
            inps<-reshape2::melt(rbind(rep(NA,ncol(inps)),inps))$value
            uni.hids<-paste0('Hidden_',1+seq(1,hid.num))
            for(i in 1:length(uni.hids)){
                if(is.na(uni.hids[i+1])) break
                tmp<-wts[grep(uni.hids[i],rownames(wts)),grep(uni.hids[i+1],colnames(wts)),drop=F]
                inps<-c(inps,reshape2::melt(rbind(rep(NA,ncol(tmp)),tmp))$value)
            }
            
            #get connections from last hidden to output layers
            outs<-wts[grep(paste0('Hidden_',hid.num+1),row.names(wts)),grep('Output',colnames(wts)),drop=F]
            outs<-rbind(rep(NA,ncol(outs)),outs)
            
            #weight vector for all
            wts<-c(inps,reshape2::melt(outs)$value)
            assign('bias',F,envir=environment(nnet.vals))
        }
        
        if(nid) wts<-rescale(abs(wts),c(1,rel.rsc))
        
        #convert wts to list with appropriate names
        hid.struct<-struct.out[-c(length(struct.out))]
        row.nms<-NULL
        for(i in 1:length(hid.struct)){
            if(is.na(hid.struct[i+1])) break
            row.nms<-c(row.nms,rep(paste('hidden',i,seq(1:hid.struct[i+1])),each=1+hid.struct[i]))
        }
        row.nms<-c(
        row.nms,
        rep(paste('out',seq(1:struct.out[length(struct.out)])),each=1+struct.out[length(struct.out)-1])
        )
        out.ls<-data.frame(wts,row.nms, stringsAsFactors=FALSE)
        out.ls$row.nms<-factor(row.nms,levels=unique(row.nms),labels=unique(row.nms))
        out.ls<-split(out.ls$wts,f=out.ls$row.nms)
        
        assign('struct',struct.out,envir=environment(nnet.vals))
        
        out.ls
        
    }
    
    wts<-nnet.vals(mod.in,nid=F)
    
    if(wts.only) return(wts)
    
    #circle colors for input, if desired, must be two-vector list, first vector is for input layer
    if(is.list(circle.col)){
        circle.col.inp<-circle.col[[1]]
        circle.col<-circle.col[[2]]
    } else circle.col.inp<-circle.col
    
    #initiate plotting
    x.range<-c(0,100)
    y.range<-c(0,100)
    #these are all proportions from 0-1
    if(is.null(line.stag)) line.stag<-0.011*circle.cex/2
    layer.x<-seq(0.17,0.9,length=length(struct))
    bias.x<-layer.x[-length(layer.x)]+diff(layer.x)/2
    bias.y<-0.95
    circle.cex<-circle.cex
    
    #get variable names from mod.in object
    #change to user input if supplied
    if('numeric' %in% class(mod.in)){
        x.names<-paste0(rep('X',struct[1]),seq(1:struct[1]))
        y.names<-paste0(rep('Y',struct[3]),seq(1:struct[3]))
    }
    if('mlp' %in% class(mod.in)){
        all.names<-mod.in$snnsObject$getUnitDefinitions()
        x.names<-all.names[grep('Input',all.names$unitName),'unitName']
        y.names<-all.names[grep('Output',all.names$unitName),'unitName']
    }
    if('nn' %in% class(mod.in)){
        x.names<-mod.in$model.list$variables
        y.names<-mod.in$model.list$respons
    }
    if('xNames' %in% names(mod.in)){
        x.names<-mod.in$xNames
        y.names<-if('nn' %in% class(mod.in)){
            attr(terms(mod.o),'factor')
        } else {
            attr(terms(mod.in),'factor')
        }
        
        y.names<-row.names(y.names)[!row.names(y.names) %in% x.names]
    }
    if(!'xNames' %in% names(mod.in) & 'nnet' %in% class(mod.in)){
        if(is.null(mod.in$call$formula)){
            x.names<-colnames(eval(mod.in$call$x))
            y.names<-colnames(eval(mod.in$call$y))
        }
        else{
            forms<-eval(mod.in$call$formula)
            x.names<-mod.in$coefnames
            facts<-attr(terms(mod.in),'factors')
            y.check<-mod.in$fitted
            if(ncol(y.check)>1) y.names<-colnames(y.check)
            else y.names<-as.character(forms)[2]
        }
    }
    #change variables names to user sub
    if(!is.null(x.lab)){
        if(length(x.names) != length(x.lab)) stop('x.lab length not equal to number of input variables')
        else x.names<-x.lab
    }
    if(!is.null(y.lab)){
        if(length(y.names) != length(y.lab)) stop('y.lab length not equal to number of output variables')
        else y.names<-y.lab
    }
    
    #initiate plot
    plot(x.range,y.range,type='n',axes=F,ylab='',xlab='',...)
    
    #function for getting y locations for input, hidden, output layers
    #input is integer value from 'struct'
    get.ys<-function(lyr, max_space = max.sp){
        if(max_space){
            spacing <- diff(c(0*diff(y.range),0.9*diff(y.range)))/lyr
        } else {
            spacing<-diff(c(0*diff(y.range),0.9*diff(y.range)))/max(struct)
        }
        
        seq(0.5*(diff(y.range)+spacing*(lyr-1)),0.5*(diff(y.range)-spacing*(lyr-1)),
        length=lyr)
    }
    
    #function for plotting nodes
    #'layer' specifies which layer, integer from 'struct'
    #'x.loc' indicates x location for layer, integer from 'layer.x'
    #'layer.name' is string indicating text to put in node
    layer.points<-function(layer,x.loc,layer.name,cex=cex.val){
        x<-rep(x.loc*diff(x.range),layer)
        y<-get.ys(layer)
        points(x,y,pch=21,cex=circle.cex,col=bord.col,bg=in.col)
        if(node.labs) text(x,y,paste(layer.name,1:layer,sep=''),cex=cex.val)
        if(layer.name=='I' & var.labs) text(x-line.stag*diff(x.range),y,x.names,pos=2,cex=cex.val)
        if(layer.name=='O' & var.labs) text(x+line.stag*diff(x.range),y,y.names,pos=4,cex=cex.val)
    }
    
    #function for plotting bias points
    #'bias.x' is vector of values for x locations
    #'bias.y' is vector for y location
    #'layer.name' is  string indicating text to put in node
    bias.points<-function(bias.x,bias.y,layer.name,cex,...){
        for(val in 1:length(bias.x)){
            points(
            diff(x.range)*bias.x[val],
            bias.y*diff(y.range),
            pch=21,col=bord.col,bg=in.col,cex=circle.cex
            )
            if(node.labs)
            text(
            diff(x.range)*bias.x[val],
            bias.y*diff(y.range),
            paste(layer.name,val,sep=''),
            cex=cex.val
            )
        }
    }
    
    #function creates lines colored by direction and width as proportion of magnitude
    #use 'all.in' argument if you want to plot connection lines for only a single input node
    layer.lines<-function(mod.in,h.layer,layer1=1,layer2=2,out.layer=F,nid,rel.rsc,all.in,pos.col,
    neg.col,...){
        
        x0<-rep(layer.x[layer1]*diff(x.range)+line.stag*diff(x.range),struct[layer1])
        x1<-rep(layer.x[layer2]*diff(x.range)-line.stag*diff(x.range),struct[layer1])
        
        if(out.layer==T){
            
            y0<-get.ys(struct[layer1])
            y1<-rep(get.ys(struct[layer2])[h.layer],struct[layer1])
            src.str<-paste('out',h.layer)
            
            wts<-nnet.vals(mod.in,nid=F,rel.rsc)
            wts<-wts[grep(src.str,names(wts))][[1]][-1]
            wts.rs<-nnet.vals(mod.in,nid=T,rel.rsc)
            wts.rs<-wts.rs[grep(src.str,names(wts.rs))][[1]][-1]
            
            cols<-rep(pos.col,struct[layer1])
            cols[wts<0]<-neg.col
            
            if(nid) segments(x0,y0,x1,y1,col=cols,lwd=wts.rs)
            else segments(x0,y0,x1,y1)
            
        }
        
        else{
            
            if(is.logical(all.in)) all.in<-h.layer
            else all.in<-which(x.names==all.in)
            
            y0<-rep(get.ys(struct[layer1])[all.in],struct[2])
            y1<-get.ys(struct[layer2])
            src.str<-paste('hidden',layer1)
            
            wts<-nnet.vals(mod.in,nid=F,rel.rsc)
            wts<-unlist(lapply(wts[grep(src.str,names(wts))],function(x) x[all.in+1]))
            wts.rs<-nnet.vals(mod.in,nid=T,rel.rsc)
            wts.rs<-unlist(lapply(wts.rs[grep(src.str,names(wts.rs))],function(x) x[all.in+1]))
            
            cols<-rep(pos.col,struct[layer2])
            cols[wts<0]<-neg.col
            
            if(nid) segments(x0,y0,x1,y1,col=cols,lwd=wts.rs)
            else segments(x0,y0,x1,y1)
            
        }
        
    }
    
    bias.lines<-function(bias.x,mod.in,nid,rel.rsc,all.out,pos.col,neg.col,...){
        
        if(is.logical(all.out)) all.out<-1:struct[length(struct)]
        else all.out<-which(y.names==all.out)
        
        for(val in 1:length(bias.x)){
            
            wts<-nnet.vals(mod.in,nid=F,rel.rsc)
            wts.rs<-nnet.vals(mod.in,nid=T,rel.rsc)
            
            if(val != length(bias.x)){
                wts<-wts[grep('out',names(wts),invert=T)]
                wts.rs<-wts.rs[grep('out',names(wts.rs),invert=T)]
                sel.val<-grep(val,substr(names(wts.rs),8,8))
                wts<-wts[sel.val]
                wts.rs<-wts.rs[sel.val]
            }
            
            else{
                wts<-wts[grep('out',names(wts))]
                wts.rs<-wts.rs[grep('out',names(wts.rs))]
            }
            
            cols<-rep(pos.col,length(wts))
            cols[unlist(lapply(wts,function(x) x[1]))<0]<-neg.col
            wts.rs<-unlist(lapply(wts.rs,function(x) x[1]))
            
            if(nid==F){
                wts.rs<-rep(1,struct[val+1])
                cols<-rep('black',struct[val+1])
            }
            
            if(val != length(bias.x)){
                segments(
                rep(diff(x.range)*bias.x[val]+diff(x.range)*line.stag,struct[val+1]),
                rep(bias.y*diff(y.range),struct[val+1]),
                rep(diff(x.range)*layer.x[val+1]-diff(x.range)*line.stag,struct[val+1]),
                get.ys(struct[val+1]),
                lwd=wts.rs,
                col=cols
                )
            }
            
            else{
                segments(
                rep(diff(x.range)*bias.x[val]+diff(x.range)*line.stag,struct[val+1]),
                rep(bias.y*diff(y.range),struct[val+1]),
                rep(diff(x.range)*layer.x[val+1]-diff(x.range)*line.stag,struct[val+1]),
                get.ys(struct[val+1])[all.out],
                lwd=wts.rs[all.out],
                col=cols[all.out]
                )
            }
            
        }
    }
    
    #use functions to plot connections between layers
    #bias lines
    if(bias) bias.lines(bias.x,mod.in,nid=nid,rel.rsc=rel.rsc,all.out=all.out,pos.col=alpha(pos.col,alpha.val),
    neg.col=alpha(neg.col,alpha.val))
    
    #layer lines, makes use of arguments to plot all or for individual layers
    #starts with input-hidden
    #uses 'all.in' argument to plot connection lines for all input nodes or a single node
    if(is.logical(all.in)){
        mapply(
        function(x) layer.lines(mod.in,x,layer1=1,layer2=2,nid=nid,rel.rsc=rel.rsc,
        all.in=all.in,pos.col=alpha(pos.col,alpha.val),neg.col=alpha(neg.col,alpha.val)),
        1:struct[1]
        )
    }
    else{
        node.in<-which(x.names==all.in)
        layer.lines(mod.in,node.in,layer1=1,layer2=2,nid=nid,rel.rsc=rel.rsc,all.in=all.in,
        pos.col=alpha(pos.col,alpha.val),neg.col=alpha(neg.col,alpha.val))
    }
    #connections between hidden layers
    lays<-split(c(1,rep(2:(length(struct)-1),each=2),length(struct)),
    f=rep(1:(length(struct)-1),each=2))
    lays<-lays[-c(1,(length(struct)-1))]
    for(lay in lays){
        for(node in 1:struct[lay[1]]){
            layer.lines(mod.in,node,layer1=lay[1],layer2=lay[2],nid=nid,rel.rsc=rel.rsc,all.in=T,
            pos.col=alpha(pos.col,alpha.val),neg.col=alpha(neg.col,alpha.val))
        }
    }
    #lines for hidden-output
    #uses 'all.out' argument to plot connection lines for all output nodes or a single node
    if(is.logical(all.out))
    mapply(
    function(x) layer.lines(mod.in,x,layer1=length(struct)-1,layer2=length(struct),out.layer=T,nid=nid,rel.rsc=rel.rsc,
    all.in=all.in,pos.col=alpha(pos.col,alpha.val),neg.col=alpha(neg.col,alpha.val)),
    1:struct[length(struct)]
    )
    else{
        node.in<-which(y.names==all.out)
        layer.lines(mod.in,node.in,layer1=length(struct)-1,layer2=length(struct),out.layer=T,nid=nid,rel.rsc=rel.rsc,
        pos.col=pos.col,neg.col=neg.col,all.out=all.out)
    }
    
    #use functions to plot nodes
    for(i in 1:length(struct)){
        in.col<-circle.col
        layer.name<-'H'
        if(i==1) { layer.name<-'I'; in.col<-circle.col.inp}
        if(i==length(struct)) layer.name<-'O'
        layer.points(struct[i],layer.x[i],layer.name)
    }
    
    if(bias) bias.points(bias.x,bias.y,'B')
    
}
plot.nnet <- cmpfun(plot.nnet)

###UI Choices

deconvolutionWidthUI <- function(selection=5){
    sliderInput('deconvolutionwidth', "Width of Smoothing Window", min=1, max=23, step=2, value=selection)
}

deconvolutionAlphaUI <- function(selection=2.5){
    sliderInput('deconvolutionalpha', "Smoothing Window Focus on Center", min=1, max=15, step=0.1, value=selection)
}

deconvolutionDefaultSigmaUI <- function(selection=0.07){
    sliderInput('deconvolutiondefaultsigma', "Default Peak Standard Deviation", min=0.01, max=1, step=0.01, value=selection)
}

deconvolutionSmoothIterUI <- function(selection=20){
    sliderInput('deconvolutionsmoothiter', "Smoothing Iterations", min=5, max=100, step=1, value=selection)
}

deconvolutionSnipIterUI <- function(selection=20){
    sliderInput('deconvolutionsnipiter', "Baseline Snip Iterations", min=5, max=100, step=1, value=selection)
}

# --- Deconvolution physics UI (phase b2) --------------------------------------------------------
# "Legacy (current)" reproduces historical behaviour; the other modes opt into the newer xrftools
# physics (non-negative fit, energy-dependent resolution, cross-section/electron excitation, etc.).
deconvolutionModeUI <- function(selection="legacy"){
    selectInput('deconvolutionmode', "Instrument / physics mode",
        choices=c("Legacy (current)"="legacy", "Handheld XRF"="handheld", "SEM-EDS (electron)"="sem",
                  "PIXE"="pixe", "High-energy / HPGe-CdTe"="high_energy"),
        selected=selection)
}

# Beam / accelerating voltage (keV). Blank = auto (max spectrum energy). Only used by non-legacy modes.
deconvolutionBeamEnergyUI <- function(selection=NULL){
    numericInput('deconvolutionbeamenergy', "Beam / tube voltage (keV, blank = auto)",
        value=selection, min=1, max=300, step=1)
}

# Tube anode element (enables Rayleigh/Compton scatter templates). Handheld / high-energy modes.
deconvolutionTubeAnodeUI <- function(selection="None"){
    selectInput('deconvolutiontubeanode', "Tube anode (scatter)",
        choices=c("None", "Rh", "Ag", "W", "Mo", "Au", "Pd", "Cr", "Ta", "Cu"), selected=selection)
}

# Primary-beam filter that hardens the tube spectrum. Free text so a filter stack can be given, e.g.
# "Cu 100; Ti 25; Al 300" (each "Element thickness_um"; ';' or ',' separated; blank = none). Auto-seeded
# from the imported file's filter metadata when available -- REVIEW/EDIT before use, since a handheld may
# report filter-wheel contents rather than the single in-beam filter for a given measurement.
deconvolutionTubeFilterUI <- function(selection=NULL){
    textInput('deconvolutiontubefilter', "Beam filter (e.g. 'Cu 100; Ti 25; Al 300'; blank = none)",
        value=if(is.null(selection)) "" else selection)
}

# Detector type (energy-dependent resolution + efficiency). "Auto" follows the instrument mode
# (SDD for handheld/SEM/PIXE, HPGe for high-energy); the ubiquitous default is a silicon-drift detector.
deconvolutionDetectorUI <- function(selection="Auto"){
    selectInput('deconvolutiondetector', "Detector",
        choices=c("Auto (mode default: SDD)"="Auto", "SDD", "SiLi", "SiPIN", "HPGe", "CdTe"),
        selected=selection)
}

# Detector active-layer thickness (microns) for the efficiency model. Defaults to 450 um, the modern
# silicon-drift-detector standard. Applied to SDD/Si detectors; HPGe/CdTe keep their mm-scale preset
# unless a non-450 value is entered. Only used when a non-legacy mode engages the efficiency model.
deconvolutionThicknessUI <- function(selection=450){
    numericInput('deconvolutionthickness', "Detector active thickness (um, SDD = 450)",
        value=selection, min=1, max=20000, step=10)
}

# Measurement-environment presets: the atmosphere over the sample->detector path plus any polymer snout
# window, which attenuate low-energy lines. There's no reliable way to read the exact path/window from a
# file, so we default to the most common handheld setup (air + a 4 um polypropylene window) and let the
# user revise. Each maps to (atmosphere, air_path_cm, window) consumed by xrf_detector_efficiency.
deconvolution_environment_presets <- function(){
    c("Air + polypropylene window (4 um)"="air_pp",
      "Helium flush + polypropylene (4 um)"="helium",
      "Air + Kapton window (8 um)"="air_kapton",
      "Air only (no window)"="air_only",
      "Vacuum"="vacuum",
      "None (ignore path)"="none")
}
deconvolution_environment_config <- function(preset){
    switch(as.character(preset),
        air_pp     = list(atmosphere="Air",    air_path_cm=0.5, window="polypropylene 4"),
        helium     = list(atmosphere="He",     air_path_cm=0.5, window="polypropylene 4"),
        air_kapton = list(atmosphere="Air",    air_path_cm=0.5, window="Kapton 8"),
        air_only   = list(atmosphere="Air",    air_path_cm=0.5, window=NULL),
        vacuum     = list(atmosphere="vacuum", air_path_cm=0,   window=NULL),
        list(atmosphere=NULL, air_path_cm=NULL, window=NULL))   # "none" / unknown -> no path model
}
# Measurement environment selector. Defaults to the most common handheld case; user revises otherwise.
deconvolutionEnvironmentUI <- function(selection="air_pp"){
    selectInput('deconvolutionenvironment', "Measurement environment (path + window)",
        choices=deconvolution_environment_presets(), selected=selection)
}

# FP mass-estimate control. Off by default. "Relative" divides areas by an FP sensitivity computed once per
# batch (fast; matrix-decoupled A/S). "Full FP" runs a per-spectrum fundamental-parameters solve with
# self-absorption + secondary/tertiary fluorescence (slower, but correct for heavy matrices where secondary
# fluorescence -- e.g. Cu enhancing Fe/Mn/Cr in a bronze -- biases the relative estimate). Adds a $Mass table.
deconvolutionMassUI <- function(selection="off"){
    # accept the legacy logical (TRUE/FALSE) as well as the new mode strings
    sel <- if(isTRUE(selection)) "relative" else if(isFALSE(selection) || is.null(selection)) "off" else as.character(selection)
    selectInput('deconvolutionmass', "Estimate mass (FP)",
        choices=c("Off"="off", "Relative (fast)"="relative",
                  "Full FP (self-absorption + secondary fluor.; slower)"="full"),
        selected=sel)
}



deconvolutionUI <- function(radiocal=3, selection=NULL){
    
    selection <- if(is.null(selection)){
        "None"
    } else if(!is.null(selection)){
        selection
    }
    
    if(radiocal==0){
        selectInput('deconvolution', "Deconvolution",  choices=c("None", "Least Squares"), selected=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        selectInput('deconvolution', "Deconvolution",  choices=c("None", "Least Squares"), selected=selection)
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        selectInput('deconvolution', "Deconvolution",  choices=c("None", "Least Squares"), selected=selection)
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        selectInput('deconvolution', "Deconvolution",  choices=c("None", "Least Squares"), selected=selection)
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        selectInput('deconvolution', "Deconvolution",  choices=c("None", "Least Squares"), selected=selection)
    } else if(radiocal==12){
        NULL
    } else if(radiocal==13){
        selectInput('deconvolution', "Deconvolution",  choices=c("None", "Least Squares"), selected=selection)
    }
    
}

compressUI <- function(radiocal=3, selection=NULL){
    
    selection <- if(is.null(selection)){
        "100 eV"
    } else if(!is.null(selection)){
        selection
    }
    
    if(radiocal==0){
        selectInput('compress', label="Compress", choices=c("100 eV", "50 eV", "25 eV"), selected=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        selectInput('compress', label="Compress", choices=c("100 eV", "50 eV", "25 eV"), selected=selection)
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        selectInput('compress', label="Compress", choices=c("100 eV", "50 eV", "25 eV"), selected=selection)
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        selectInput('compress', label="Compress", choices=c("100 eV", "50 eV", "25 eV"), selected=selection)
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        selectInput('compress', label="Compress", choices=c("100 eV", "50 eV", "25 eV"), selected=selection)
    } else if(radiocal==12){
        NULL
    } else if(radiocal==13){
        selectInput('compress', label="Compress", choices=c("100 eV", "50 eV", "25 eV"), selected=selection)
    }
}

transformationUI <- function(radiocal=3, selection=NULL){
    
    selection <- if(is.null(selection)){
        "None"
    } else if(!is.null(selection)){
        selection
    }
    
    if(radiocal==0){
        selectInput('transformation', label="Spectra Transformation", choices=c("None", "Log", "e", "Velocity"), selected=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        selectInput('transformation', label="Spectra Transformation", choices=c("None", "Log", "e", "Velocity"), selected=selection)
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        selectInput('transformation', label="Spectra Transformation", choices=c("None", "Log", "e", "Velocity"), selected=selection)
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        selectInput('transformation', label="Spectra Transformation", choices=c("None", "Log", "e", "Velocity"), selected=selection)
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        selectInput('transformation', label="Spectra Transformation", choices=c("None", "Log", "e", "Velocity"), selected=selection)
    } else if(radiocal==12){
        NULL
    } else if(radiocal==13){
        selectInput('transformation', label="Spectra Transformation", choices=c("None", "Log", "e", "Velocity"), selected=selection)
    }
}

dependentTransformationUI <- function(radiocal=3, selection=NULL){
    
    selection <- if(is.null(selection)){
        "None"
    } else if(!is.null(selection)){
        selection
    }
    
    if(radiocal==0){
        selectInput('deptransformation', label="Concentration Transformation", choices=c("None", "Log", "e", "Scale"), selected=selection)
    } else if(radiocal==1){
        selectInput('deptransformation', label="Concentration Transformation", choices=c("None", "Log", "e", "Scale"), selected=selection)
    } else if(radiocal==2){
        selectInput('deptransformation', label="Concentration Transformation", choices=c("None", "Log", "e", "Scale"), selected=selection)
    } else if(radiocal==3){
        selectInput('deptransformation', label="Concentration Transformation", choices=c("None", "Log", "e", "Scale"), selected=selection)
    } else if(radiocal==4){
        selectInput('deptransformation', label="Concentration Transformation", choices=c("None", "Log", "e", "Scale"), selected=selection)
    }  else if(radiocal==5){
        selectInput('deptransformation', label="Concentration Transformation", choices=c("None", "Log", "e", "Scale"), selected=selection)
    } else if(radiocal==6){
        selectInput('deptransformation', label="Concentration Transformation", choices=c("None", "Log", "e", "Scale"), selected=selection)
    } else if(radiocal==7){
        selectInput('deptransformation', label="Concentration Transformation", choices=c("None", "Log", "e", "Scale"), selected=selection)
    } else if(radiocal==8){
        selectInput('deptransformation', label="Concentration Transformation", choices=c("None", "Log", "e", "Scale"), selected=selection)
    } else if(radiocal==9){
        selectInput('deptransformation', label="Concentration Transformation", choices=c("None", "Log", "e", "Scale"), selected=selection)
    } else if(radiocal==10){
        selectInput('deptransformation', label="Concentration Transformation", choices=c("None", "Log", "e", "Scale"), selected=selection)
    } else if(radiocal==11){
        selectInput('deptransformation', label="Concentration Transformation", choices=c("None", "Log", "e", "Scale"), selected=selection)
    } else if(radiocal==12){
        selectInput('deptransformation', label="Concentration Transformation", choices=c("None", "Log", "e", "Scale"), selected=selection)
    } else if(radiocal==13){
        selectInput('deptransformation', label="Concentration Transformation", choices=c("None", "Log", "e", "Scale"), selected=selection)
    }
}

energyRangeUI <- function(radiocal=3, selection=NULL, compress="100 eV"){
    
    selection <- if(is.null(selection)){
        c(0.7, 37)
    } else if(!is.null(selection)){
        selection
    }
    
    step <- if(compress=="100 eV"){
        0.1
    } else if(compress=="50 eV"){
        0.05
    } else if(compress=="25 eV"){
        0.025
    }
    
    if(radiocal==0){
        sliderInput('energyrange', label="Energy Range", min=0, max=40, step=step,  value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        sliderInput('energyrange', label="Energy Range", min=0, max=40, step=step,  value=selection)
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        sliderInput('energyrange', label="Energy Range", min=0, max=40, step=step, value=selection)
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        sliderInput('energyrange', label="Energy Range", min=0, max=40, step=step, value=selection)
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        sliderInput('energyrange', label="Energy Range", min=0, max=40, step=step, value=selection)
    } else if(radiocal==12){
        NULL
    } else if(radiocal==13){
        sliderInput('energyrange', label="Energy Range", min=0, max=40, step=step, value=selection)
    }
}

lineTypeUI <- function(radiocal=3, selection="Narrow"){
    
    
    if(radiocal==0){
        selectInput("linetype", "Line Type", choices=c("Narrow", "Wide", "Area"), selected=selection)
    } else if(radiocal==1){
        selectInput("linetype", "Line Type", choices=c("Narrow", "Wide", "Area"), selected=selection)
    } else if(radiocal==2){
        selectInput("linetype", "Line Type", choices=c("Narrow", "Wide", "Area"), selected=selection)
    } else if(radiocal==3){
        selectInput("linetype", "Line Type", choices=c("Narrow", "Wide", "Area"), selected=selection)
    } else if(radiocal==4){
        selectInput("linetype", "Line Type", choices=c("Narrow", "Wide", "Area"), selected=selection)
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        selectInput("linetype", "Line Type", choices=c("Narrow", "Wide", "Area"), selected=selection)
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8){
        selectInput("linetype", "Line Type", choices=c("Narrow", "Wide", "Area"), selected=selection)
    } else if(radiocal==9){
        NULL
    } else if(radiocal==10){
        selectInput("linetype", "Line Type", choices=c("Narrow", "Wide", "Area"), selected=selection)
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
        selectInput("linetype", "Line Type", choices=c("Narrow", "Wide", "Area"), selected=selection)
    } else if(radiocal==13){
        NULL

    }
}

lineStructureUI <- function(radiocal=3, selection="gaussian"){
    
    if(radiocal==0){
        selectInput("linestructure", "Line Calculation", choices=c("gaussian", "split"), selected=selection)
    } else if(radiocal==1){
        selectInput("linestructure", "Line Calculation", choices=c("gaussian", "split"), selected=selection)
    } else if(radiocal==2){
        selectInput("linestructure", "Line Calculation", choices=c("gaussian", "split"), selected=selection)
    } else if(radiocal==3){
        selectInput("linestructure", "Line Calculation", choices=c("gaussian", "split"), selected=selection)
    } else if(radiocal==4){
        selectInput("linestructure", "Line Calculation", choices=c("gaussian", "split"), selected=selection)
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        selectInput("linestructure", "Line Calculation", choices=c("gaussian", "split"), selected=selection)
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8){
        selectInput("linestructure", "Line Calculation", choices=c("gaussian", "split"), selected=selection)
    } else if(radiocal==9){
        NULL
    } else if(radiocal==10){
        selectInput("linestructure", "Line Calculation", choices=c("gaussian", "split"), selected=selection)
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
        selectInput("linestructure", "Line Calculation", choices=c("gaussian", "split"), selected=selection)
    } else if(radiocal==13){
        NULL

    }
    
}

interceptUI <- function(radiocal=3, selection=NULL, elements){
    

    if(radiocal==0){
        selectInput(inputId = "intercept_vars", label = "Intercept", choices=elements, selected=selection, multiple=TRUE)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        selectInput(inputId = "intercept_vars", label = "Intercept", choices=elements, selected=selection, multiple=TRUE)
    } else if(radiocal==4){
        selectInput(inputId = "intercept_vars", label = "Intercept", choices=elements, selected=selection, multiple=TRUE)
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        selectInput(inputId = "intercept_vars", label = "Intercept", choices=elements, selected=selection, multiple=TRUE)
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8){
        selectInput(inputId = "intercept_vars", label = "Intercept", choices=elements, selected=selection, multiple=TRUE)
    } else if(radiocal==9){
        NULL
    } else if(radiocal==10){
        selectInput(inputId = "intercept_vars", label = "Intercept", choices=elements, selected=selection, multiple=TRUE)
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
        selectInput(inputId = "intercept_vars", label = "Intercept", choices=elements, selected=selection, multiple=TRUE)
    } else if(radiocal==13){
        NULL
    }
}

slopeUI <- function(radiocal=3, selection=NULL, elements){
    
    elements.mod <- elements

    if(radiocal==0){
        selectInput(inputId = "slope_vars", label = "Slope", choices=elements.mod, selected=selection, multiple=TRUE)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        selectInput(inputId = "slope_vars", label = "Slope", choices=elements.mod, selected=selection, multiple=TRUE)
    } else if(radiocal==4){
        selectInput(inputId = "slope_vars", label = "Slope", choices=elements.mod, selected=selection, multiple=TRUE)
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        selectInput(inputId = "slope_vars", label = "Slope", choices=elements.mod, selected=selection, multiple=TRUE)
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8){
        selectInput(inputId = "slope_vars", label = "Slope", choices=elements.mod, selected=selection, multiple=TRUE)
    } else if(radiocal==9){
        NULL
    } else if(radiocal==10){
        selectInput(inputId = "slope_vars", label = "Slope", choices=elements.mod, selected=selection, multiple=TRUE)
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
        selectInput(inputId = "slope_vars", label = "Slope", choices=elements.mod, selected=selection, multiple=TRUE)
    } else if(radiocal==13){
        NULL
    }
}

addAllSlopeUI <- function(radiocal=3){
    if(radiocal==0 | radiocal==3 | radiocal==4 | radiocal==6 | radiocal==8 | radiocal==10 | radiocal==12){
        actionButton(inputId = "addallslopes", label = "Add All Slopes")
    } else if(radiocal!=0 | radiocal!=3 | radiocal!=4 | radiocal!=6 | radiocal!=8 | radiocal!=10 | radiocal!=12){
        NULL
    }
}

removeAllSlopeUI <- function(radiocal=3){
    if(radiocal==0 | radiocal==3 | radiocal==4 | radiocal==6 | radiocal==8 | radiocal==10 | radiocal==12){
        actionButton(inputId = "removeallslopes", label = "Remove All Slopes")
        } else if(radiocal!=0 | radiocal!=3 | radiocal!=4 | radiocal!=6 | radiocal!=8 | radiocal!=10 | radiocal!=12){
        NULL
    }
}

   

forestTryUI <- function(radiocal=3, neuralhiddenlayers=NULL, selection=NULL, maxsample=NULL){
    
    neuralhiddenlayers <- if(is.null(neuralhiddenlayers)){
        1
    } else if(!is.null(neuralhiddenlayers)){
        neuralhiddenlayers
    }
    
    selection <- if(is.null(selection)){
        5
    } else if(!is.null(selection)){
        selection
    }
    
    maxsample <- if(is.null(maxsample)){
        15
    } else if(!is.null(maxsample)){
        maxsample
    }
    
    if(radiocal==0){
        sliderInput("foresttry", label="Sampling", min=2, max=maxsample-2, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        sliderInput("foresttry", label="Sampling", min=2, max=maxsample-2, value=selection)
    }  else if(radiocal==5){
        sliderInput("foresttry", label="Sampling", min=2, max=maxsample-2, value=selection)
    } else if(radiocal==6 && neuralhiddenlayers == 1){
        NULL
    } else if(radiocal==6 && neuralhiddenlayers > 1){
        sliderInput("foresttry", label="Sampling", min=2, max=maxsample-2, value=selection)
    } else if(radiocal==7 && neuralhiddenlayers == 1){
        NULL
    } else if(radiocal==7 && neuralhiddenlayers > 1){
        sliderInput("foresttry", label="Sampling", min=2, max=maxsample-2, value=selection)
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        NULL
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
            NULL
    } else if(radiocal==13){
        NULL
    }
}

maeSummary <- function (data,
lev = NULL,
model = NULL) {
    out <- Metrics::mae(data$obs, data$pred)
    names(out) <- "MAE"
    out
}

logmaeSummary <- function (data,
lev = NULL,
model = NULL) {
    out <- Metrics::mae(log10(data$obs), log10(data$pred))
    names(out) <- "logMAE"
    out
}

smapeSummary <- function (data,
lev = NULL,
model = NULL) {
    out <- Metrics::smape(data$obs, data$pred)
    names(out) <- "SMAPE"
    out
}

forestMetricUI <- function(radiocal, selection){
    if(radiocal==0){
        selectInput("forestmetric", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "Mean Absolute Error"="MAE", "Log Absolute Error"="logMAE", "Symmetric Mean Absolute Percentage Error"="SMAPE"), selected=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        selectInput("forestmetric", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "Mean Absolute Error"="MAE", "Log Absolute Error"="logMAE", "Symmetric Mean Absolute Percentage Error"="SMAPE"), selected=selection)
    } else if(radiocal==5){
        selectInput("forestmetric", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "Mean Absolute Error"="MAE", "Log Absolute Error"="logMAE", "Symmetric Mean Absolute Percentage Error"="SMAPE"), selected=selection)
    } else if(radiocal==6){
        selectInput("forestmetric", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "Mean Absolute Error"="MAE", "Log Absolute Error"="logMAE", "Symmetric Mean Absolute Percentage Error"="SMAPE"), selected=selection)
    } else if(radiocal==7){
        selectInput("forestmetric", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "Mean Absolute Error"="MAE", "Log Absolute Error"="logMAE", "Symmetric Mean Absolute Percentage Error"="SMAPE"), selected=selection)
    } else if(radiocal==8){
        selectInput("forestmetric", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "Mean Absolute Error"="MAE", "Log Absolute Error"="logMAE", "Symmetric Mean Absolute Percentage Error"="SMAPE"), selected=selection)
    } else if(radiocal==9){
        selectInput("forestmetric", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "Mean Absolute Error"="MAE", "Log Absolute Error"="logMAE", "Symmetric Mean Absolute Percentage Error"="SMAPE"), selected=selection)
    } else if(radiocal==10){
        selectInput("forestmetric", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "Mean Absolute Error"="MAE", "Log Absolute Error"="logMAE", "Symmetric Mean Absolute Percentage Error"="SMAPE"), selected=selection)
    } else if(radiocal==11){
        selectInput("forestmetric", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "Mean Absolute Error"="MAE", "Log Absolute Error"="logMAE", "Symmetric Mean Absolute Percentage Error"="SMAPE"), selected=selection)
    } else if(radiocal==12){
        selectInput("forestmetric", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "Mean Absolute Error"="MAE", "Log Absolute Error"="logMAE", "Symmetric Mean Absolute Percentage Error"="SMAPE"), selected=selection)
    } else if(radiocal==13){
        selectInput("forestmetric", label="Metric", choices=c("Root Mean Square Error"="RMSE", "R2"="Rsquared", "Mean Absolute Error"="MAE", "Log Absolute Error"="logMAE", "Symmetric Mean Absolute Percentage Error"="SMAPE"), selected=selection)
    }
}

forestTrainUI <- function(radiocal, selection){
    if(radiocal==0){
        selectInput("foresttrain", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV", "Out of Bag Estimation"="oob"), selected=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        selectInput("foresttrain", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV", "Out of Bag Estimation"="oob"), selected=selection)
    }  else if(radiocal==5){
        selectInput("foresttrain", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV", "Out of Bag Estimation"="oob"), selected=selection)
    } else if(radiocal==6){
        selectInput("foresttrain", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV"), selected=selection)
    } else if(radiocal==7){
        selectInput("foresttrain", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV"), selected=selection)
    } else if(radiocal==8){
        selectInput("foresttrain", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV"), selected=selection)
    } else if(radiocal==9){
        selectInput("foresttrain", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV"), selected=selection)
    } else if(radiocal==10){
        selectInput("foresttrain", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV"), selected=selection)
    } else if(radiocal==11){
        selectInput("foresttrain", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV"), selected=selection)
    } else if(radiocal==12){
        selectInput("foresttrain", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV"), selected=selection)
    } else if(radiocal==13){
        selectInput("foresttrain", label="Train Control", choices=c("k-fold Cross Validation"="cv", "Bootstrap"="boot", "0.632 Bootstrap"="boot632", "Optimism Bootstrap"="optimism_boot", "Repeated k-fold Cross Validation"="repeatedcv", "Leave One Out Cross Validation"="LOOCV"), selected=selection)
    }
}

forestNumberUI <- function(radiocal, selection){
    if(radiocal==0){
        sliderInput("forestnumber", label="Iterations", min=5, max=2000, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        sliderInput("forestnumber", label="Iterations", min=1, max=2000, value=selection)
    }  else if(radiocal==5){
        sliderInput("forestnumber", label="Iterations", min=1, max=2000, value=selection)
    } else if(radiocal==6){
        sliderInput("forestnumber", label="Iterations", min=1, max=2000, value=selection)
    } else if(radiocal==7){
        sliderInput("forestnumber", label="Iterations", min=1, max=2000, value=selection)
    } else if(radiocal==8){
        sliderInput("forestnumber", label="Iterations", min=1, max=2000, value=selection)
    } else if(radiocal==9){
        sliderInput("forestnumber", label="Iterations", min=1, max=2000, value=selection)
    } else if(radiocal==10){
        sliderInput("forestnumber", label="Iterations", min=1, max=2000, value=selection)
    } else if(radiocal==11){
        sliderInput("forestnumber", label="Iterations", min=1, max=2000, value=selection)
    } else if(radiocal==12){
        sliderInput("forestnumber", label="Iterations", min=1, max=2000, value=selection)
    } else if(radiocal==13){
        sliderInput("forestnumber", label="Iterations", min=1, max=2000, value=selection)
    }
       
}

cvRepeatsUI <- function(radiocal, foresttrain, selection){
    if(radiocal==0){
        sliderInput("cvrepeats", label="Repeats", min=5, max=500, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4 && foresttrain=="repeatedcv"){
        sliderInput("cvrepeats", label="Repeats", min=5, max=500, value=selection)
    } else if(radiocal==4 && foresttrain!="repeatedcv"){
        NULL
    } else if(radiocal==5 && foresttrain=="repeatedcv"){
        sliderInput("cvrepeats", label="Repeats", min=5, max=500, value=selection)
    } else if(radiocal==5 && foresttrain!="repeatedcv"){
        NULL
    } else if(radiocal==6 && foresttrain=="repeatedcv"){
        sliderInput("cvrepeats", label="Repeats", min=5, max=500, value=selection)
    } else if(radiocal==6 && foresttrain!="repeatedcv"){
        NULL
    } else if(radiocal==7 && foresttrain=="repeatedcv"){
        sliderInput("cvrepeats", label="Repeats", min=5, max=500, value=selection)
    } else if(radiocal==7 && foresttrain!="repeatedcv"){
        NULL
    } else if(radiocal==8 && foresttrain=="repeatedcv"){
        sliderInput("cvrepeats", label="Repeats", min=5, max=500, value=selection)
    } else if(radiocal==8 && foresttrain!="repeatedcv"){
        NULL
    } else if(radiocal==9 && foresttrain=="repeatedcv"){
        sliderInput("cvrepeats", label="Repeats", min=5, max=500, value=selection)
    } else if(radiocal==9 && foresttrain!="repeatedcv"){
        NULL
    }  else if(radiocal==10 && foresttrain=="repeatedcv"){
        sliderInput("cvrepeats", label="Repeats", min=5, max=500, value=selection)
    } else if(radiocal==10 && foresttrain!="repeatedcv"){
        NULL
    } else if(radiocal==11 && foresttrain=="repeatedcv"){
        sliderInput("cvrepeats", label="Repeats", min=5, max=500, value=selection)
    } else if(radiocal==11 && foresttrain!="repeatedcv"){
        NULL
    } else if(radiocal==12 && foresttrain=="repeatedcv"){
        sliderInput("cvrepeats", label="Repeats", min=5, max=500, value=selection)
    } else if(radiocal==12 && foresttrain!="repeatedcv"){
        NULL
    } else if(radiocal==13 && foresttrain=="repeatedcv"){
        sliderInput("cvrepeats", label="Repeats", min=5, max=500, value=selection)
    } else if(radiocal==13 && foresttrain!="repeatedcv"){
        NULL
    }
}

forestTreesUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==0){
        sliderInput("foresttrees", label="Trees", min=50, max=2000, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        sliderInput("foresttrees", label="Trees", min=50, max=2000, value=selection)
    } else if(radiocal==5){
        sliderInput("foresttrees", label="Trees", min=50, max=2000, value=selection)
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8 && xgbtype=="Tree"){
        sliderInput("foresttrees", label="Number of Rounds", min=50, max=2000, value=selection)
    } else if(radiocal==8 && xgbtype=="Dart"){
        sliderInput("foresttrees", label="Number of Rounds", min=50, max=2000, value=selection)
    } else if(radiocal==8 && xgbtype=="Linear"){
        sliderInput("foresttrees", label="Number of Rounds", min=50, max=2000, value=selection)
    } else if(radiocal==9 && xgbtype=="Tree"){
        sliderInput("foresttrees", label="Number of Rounds", min=50, max=2000, value=selection)
    } else if(radiocal==9 && xgbtype=="Dart"){
        sliderInput("foresttrees", label="Number of Rounds", min=50, max=2000, value=selection)
    } else if(radiocal==9 && xgbtype=="Linear"){
        sliderInput("foresttrees", label="Number of Rounds", min=50, max=2000, value=selection)
    }  else if(radiocal==10 && xgbtype=="Tree"){
        sliderInput("foresttrees", label="Trees", min=50, max=2000, value=selection)
    } else if(radiocal==10 && xgbtype!="Tree"){
        NULL
    } else if(radiocal==11 && xgbtype=="Tree"){
        sliderInput("foresttrees", label="Trees", min=50, max=2000, value=selection)
    } else if(radiocal==11 && xgbtype!="Tree"){
        NULL
    } else if(radiocal==12){
        NULL
    } else if(radiocal==13){
        NULL
    }
}

neuralHiddenLayersUI <- function(radiocal, selection){
    if(radiocal==0){
        sliderInput("neuralhiddenlayers", label="Hidden Layers", min=1, max=3, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        sliderInput("neuralhiddenlayers", label="Hidden Layers", min=1, max=3, value=selection)
    } else if(radiocal==7){
        sliderInput("neuralhiddenlayers", label="Hidden Layers", min=1, max=3, value=selection)
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        NULL
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
        NULL
    }  else if(radiocal==13){
        NULL
    }
}

neuralHiddenUnitsUi <- function(radiocal, selection, xgbtype="Neural Net"){
    if(radiocal==0){
        sliderInput("neuralhiddenunits", label="Hidden Units", min=1, max=10, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        sliderInput("neuralhiddenunits", label="Hidden Units", min=1, max=10, value=selection)
    } else if(radiocal==7){
        sliderInput("neuralhiddenunits", label="Hidden Units", min=1, max=10, value=selection)
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        NULL
    } else if(radiocal==10 && xgbtype=="Neural Net"){
        sliderInput("neuralhiddenunits", label="Neurons", min=1, max=10, value=selection)
    } else if(radiocal==10 && xgbtype!="Neural Net"){
        NULL
    } else if(radiocal==11 && xgbtype=="Neural Net"){
        sliderInput("neuralhiddenunits", label="Neurons", min=1, max=10, value=selection)
    } else if(radiocal==11 && xgbtype!="Neural Net"){
        NULL
    } else if(radiocal==12){
        NULL
    }  else if(radiocal==13){
        NULL
    }
}

neuralWeightDecayUI <- function(radiocal, selection, neuralhiddenlayers){
    if(radiocal==0){
        sliderInput("neuralweightdecay", label="Weight Decay", min=0.1, max=0.7, step=0.1, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6 && neuralhiddenlayers == 1){
        sliderInput("neuralweightdecay", label="Weight Decay", min=0.1, max=0.7, step=0.1, value=selection)
    } else if(radiocal==6 && neuralhiddenlayers > 1){
        NULL
    } else if(radiocal==7 && neuralhiddenlayers == 1){
        sliderInput("neuralweightdecay", label="Weight Decay", min=0.1, max=0.7, step=0.1, value=selection)
    } else if(radiocal==7 && neuralhiddenlayers > 1){
        NULL
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        NULL
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
        NULL
    }  else if(radiocal==13){
        NULL
    }
}

neuralMaxIterationsUI <- function(radiocal, selection, neuralhiddenlayers){
    if(radiocal==0){
        sliderInput("neuralmaxiterations", label="Max Iterations", min=50, max=2000, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6 && neuralhiddenlayers == 1){
        sliderInput("neuralmaxiterations", label="Max Iterations", min=50, max=2000, value=selection)
    } else if(radiocal==6 && neuralhiddenlayers > 1){
        NULL
    } else if(radiocal==7 && neuralhiddenlayers == 1){
        sliderInput("neuralmaxiterations", label="Max Iterations", min=50, max=2000, value=selection)
    } else if(radiocal==7 && neuralhiddenlayers > 1){
        NULL
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        NULL
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
        NULL
    }  else if(radiocal==13){
        NULL
    }
}

treeDepthUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==0){
        sliderInput("treedepth", label="Tree Depth", min=2, max=50, step=1, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8 && xgbtype=="Tree"){
        sliderInput("treedepth", label="Tree Depth", min=2, max=50, step=1, value=selection)
    } else if(radiocal==8 && xgbtype=="Dart"){
        sliderInput("treedepth", label="Tree Depth", min=2, max=50, step=1, value=selection)
    } else if(radiocal==8 && xgbtype=="Linear"){
        NULL
    } else if(radiocal==9 && xgbtype=="Tree"){
        sliderInput("treedepth", label="Tree Depth", min=2, max=50, step=1, value=selection)
    } else if(radiocal==9 && xgbtype=="Dart"){
        sliderInput("treedepth", label="Tree Depth", min=2, max=50, step=1, value=selection)
    } else if(radiocal==9 && xgbtype=="Linear"){
        NULL
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
        NULL
    }  else if(radiocal==13){
        NULL
    }
}

treeMethodUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==0){
        selectInput("treemethod", label="Tree Method", choices=c("auto", "exact", "approx", "hist", "gpu_hist"), selected=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8 && xgbtype=="Tree"){
        selectInput("treemethod", label="Tree Method", choices=c("auto", "exact", "approx", "hist", "gpu_hist"), selected=selection)
    } else if(radiocal==8 && xgbtype=="Dart"){
        selectInput("treemethod", label="Tree Method", choices=c("auto", "exact", "approx", "hist", "gpu_hist"), selected=selection)
    } else if(radiocal==8 && xgbtype=="Linear"){
        NULL
    } else if(radiocal==9 && xgbtype=="Tree"){
        selectInput("treemethod", label="Tree Method", choices=c("auto", "exact", "approx", "hist", "gpu_hist"), selected=selection)
    } else if(radiocal==9 && xgbtype=="Dart"){
        selectInput("treemethod", label="Tree Method", choices=c("auto", "exact", "approx", "hist", "gpu_hist"), selected=selection)
    } else if(radiocal==9 && xgbtype=="Linear"){
        NULL
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
        NULL
    }  else if(radiocal==13){
        NULL
    }
}

xgbTypeUI <- function(radiocal, selection){
    if(radiocal==0){
        NULL
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8){
        selectInput("xgbtype", label="XGBoost Type", choices=c("Tree", "Dart", "Linear"), selected="Linear")
    } else if(radiocal==9){
        selectInput("xgbtype", label="XGBoost Type", choices=c("Tree", "Dart", "Linear"), selected="Linear")
    } else if(radiocal==10){
        selectInput("xgbtype", label="Bayesian Model Type", choices=c("Tree", "Linear", "Neural Net"), selected="Linear")
    } else if(radiocal==11){
        selectInput("xgbtype", label="Bayesian Model Type", choices=c("Tree", "Linear", "Neural Net"), selected="Linear")
    } else if(radiocal==12){
        #selectInput("xgbtype", label="Support Vector Machine", choices=c("Linear", "Polynomial", "Exponential", "Radial", "Radial Cost", "Radial Sigma", "Boundrange String", "Spectrum String"), selected="Linear")
        selectInput("xgbtype", label="Support Vector Machine", choices=c("Linear", "Polynomial", "Radial", "Radial Cost", "Radial Sigma"), selected="Linear")
    } else if(radiocal==13){
        selectInput("xgbtype", label="Support Vector Machine", choices=c("Linear", "Polynomial", "Radial", "Radial Cost", "Radial Sigma"), selected="Linear")
    }
}

dropTreeUI <- function(radiocal, selection, xgbtype="Dart"){
    if(radiocal==0){
        sliderInput("droptree", label="Drop Trees", min=0.1, max=0.9, step=0.05, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8 && xgbtype=="Dart"){
        sliderInput("droptree", label="Drop Trees", min=0.1, max=0.9, step=0.05, value=selection)
    } else if(radiocal==8 && xgbtype!="Dart"){
        NULL
    } else if(radiocal==9 && xgbtype=="Dart"){
        sliderInput("droptree", label="Drop Trees", min=0.1, max=0.9, step=0.05, value=selection)
    } else if(radiocal==9 && xgbtype!="Dart"){
        NULL
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        NULL
    }  else if(radiocal==12){
        NULL
    } else if(radiocal==13){
        NULL
    }
}

skipDropUI <- function(radiocal, selection, xgbtype="Dart"){
    if(radiocal==0){
        sliderInput("skipdrop", label="Drop Trees", min=0.1, max=0.9, step=0.05, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8 && xgbtype=="Dart"){
        sliderInput("skipdrop", label="Skip Drop", min=0.1, max=0.9, step=0.05, value=selection)
    } else if(radiocal==8 && xgbtype!="Dart"){
        NULL
    } else if(radiocal==9 && xgbtype=="Dart"){
        sliderInput("skipdrop", label="Skip Drop", min=0.1, max=0.9, step=0.05, value=selection)
    } else if(radiocal==9 && xgbtype!="Dart"){
        NULL
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        NULL
    }  else if(radiocal==12){
        NULL
    } else if(radiocal==13){
        NULL
    }
}

xgbAlphaUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==0){
        sliderInput("xgbalpha", label="Alpha", min=0, max=10, step=0.05, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8){
        sliderInput("xgbalpha", label="Alpha", min=0, max=10, step=0.05, value=selection)
    } else if(radiocal==9){
        sliderInput("xgbalpha", label="Alpha", min=0, max=10, step=0.05, value=selection)
    } else if(radiocal==10 && xgbtype=="Tree"){
        sliderInput("xgbalpha", label="Alpha", min=0, max=10, step=0.05, value=selection)
    } else if(radiocal==10 && xgbtype!="Tree"){
        NULL
    } else if(radiocal==11 && xgbtype=="Tree"){
        sliderInput("xgbalpha", label="Alpha", min=0, max=10, step=0.05, value=selection)
    } else if(radiocal==11 && xgbtype!="Tree"){
        NULL
    } else if(radiocal==12){
        NULL
    } else if(radiocal==13){
        NULL
    }
}

xgbGammaUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==0){
        sliderInput("xgbgamma", label="Gamma", min=0, max=300, step=1, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8 && xgbtype!="Linear"){
        sliderInput("xgbgamma", label="Gamma", min=0, max=300, step=1, value=selection)
    } else if(radiocal==8 && xgbtype=="Linear"){
        NULL
    } else if(radiocal==9 && xgbtype!="Linear"){
        sliderInput("xgbgamma", label="Gamma", min=0, max=300, step=1, value=selection)
    } else if(radiocal==9 && xgbtype=="Linear"){
        NULL
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        NULL
    }  else if(radiocal==12){
        NULL
    } else if(radiocal==13){
        NULL
    }
}

xgbEtaUI <- function(radiocal, selection){
    if(radiocal==0){
        sliderInput("xgbeta", label="Eta", min=0.01, max=0.99, step=0.01, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8){
        sliderInput("xgbeta", label="Eta", min=0.01, max=0.99, step=0.01, value=selection)
    } else if(radiocal==9){
        sliderInput("xgbeta", label="Eta", min=0.01, max=0.99, step=0.01, value=selection)
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
        NULL
    } else if(radiocal==13){
        NULL
    }
}

xgbLambdaUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==0){
        sliderInput("xgblambda", label="Lambda", min=0, max=300, step=1, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8){
        sliderInput("xgblambda", label="Lambda", min=0, max=300, step=1, value=selection)
    } else if(radiocal==9){
        sliderInput("xgblambda", label="Lambda", min=0, max=300, step=1, value=selection)
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
        if(xgbtype=="Exponential" | xgbtype=="Spectrum String"){
            sliderInput("xgblambda", label="Lambda", min=0, max=10, step=0.05, value=selection)
        } else {
            NULL
        }
    } else if(radiocal==13){
        if(xgbtype=="Exponential" | xgbtype=="Spectrum String"){
            sliderInput("xgblambda", label="Lambda", min=0, max=10, step=0.05, value=selection)
        } else {
            NULL
        }
    }
}

xgbSubSampleUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==0){
        sliderInput("xgbsubsample", label="Sub Sample", min=0.05, max=0.95, step=0.05, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8 && xgbtype!="Linear"){
        sliderInput("xgbsubsample", label="Sub Sample", min=0.05, max=0.95, step=0.05, value=selection)
    } else if(radiocal==8 && xgbtype=="Linear"){
        NULL
    } else if(radiocal==9 && xgbtype!="Linear"){
        sliderInput("xgbsubsample", label="Sub Sample", min=0.05, max=0.95, step=0.05, value=selection)
    } else if(radiocal==9 && xgbtype=="Linear"){
        NULL
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
        NULL
    } else if(radiocal==13){
        NULL
    }
}

xgbColSampleUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==0){
        sliderInput("xgbcolsample", label="Col Sample", min=0.05, max=0.95, step=0.05, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8 && xgbtype!="Linear"){
        sliderInput("xgbcolsample", label="Col Sample", min=0.05, max=0.95, step=0.05, value=selection)
    } else if(radiocal==8 && xgbtype=="Linear"){
        NULL
    } else if(radiocal==9 && xgbtype!="Linear"){
        sliderInput("xgbcolsample", label="Col Sample", min=0.05, max=0.95, step=0.05, value=selection)
    } else if(radiocal==9 && xgbtype=="Linear"){
        NULL
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
        NULL
    } else if(radiocal==13){
        NULL
    }
}

xgbMinChildUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==0){
        sliderInput("xgbminchild", label="Min Child", min=0, max=300, step=1, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8 && xgbtype!="Linear"){
        sliderInput("xgbminchild", label="Min Child", min=0, max=300, step=1, value=selection)
    } else if(radiocal==8 && xgbtype=="Linear"){
        NULL
    } else if(radiocal==9 && xgbtype!="Linear"){
        sliderInput("xgbminchild", label="Min Child", min=0, max=300, step=1, value=selection)
    } else if(radiocal==9 && xgbtype=="Linear"){
        NULL
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
        NULL
    } else if(radiocal==13){
        NULL
    }
}

xgbMaxDeltaStepUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==0){
        sliderInput("xgbmaxdeltastep", label="Min Child", min=0, max=15, step=1, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8 && xgbtype!="Linear"){
        sliderInput("xgbmaxdeltastep", label="Max Delta Step", min=0, max=15, step=1, value=selection)
    } else if(radiocal==8 && xgbtype=="Linear"){
        NULL
    } else if(radiocal==9 && xgbtype!="Linear"){
        sliderInput("xgbmaxdeltastep", label="Max Delta Step", min=0, max=15, step=1, value=selection)
    } else if(radiocal==9 && xgbtype=="Linear"){
        NULL
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
        NULL
    } else if(radiocal==13){
        NULL
    }
}

xgbScalePosWeightUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==0){
        sliderInput("xgbscaleposweight", label="Scale Pos Weight", min=0, max=5, step=1, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8 && xgbtype!="Linear"){
        sliderInput("xgbscaleposweight", label="Scale Pos Weight", min=0, max=5, step=1, value=selection)
    } else if(radiocal==8 && xgbtype=="Linear"){
        NULL
    } else if(radiocal==9 && xgbtype!="Linear"){
        sliderInput("xgbscaleposweight", label="Scale Pos Weight", min=0, max=5, step=1, value=selection)
    } else if(radiocal==9 && xgbtype=="Linear"){
        NULL
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
        NULL
    } else if(radiocal==13){
        NULL
    }
}


dnorminv<-function(y) sqrt(-2*log(sqrt(2*pi)*y))


bartKUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==0){
        sliderInput("bartk", label="Prior Probability", min=61, max=99, step=1, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        NULL
    } else if(radiocal==10 && xgbtype=="Tree"){
        sliderInput("bartk", label="Prior Probability", min=61, max=99, step=1, value=selection)
    } else if(radiocal==10 && xgbtype!="Tree"){
        NULL
    } else if(radiocal==11 && xgbtype=="Tree"){
        sliderInput("bartk", label="Prior Probability", min=61, max=99, step=1, value=selection)
    } else if(radiocal==11 && xgbtype!="Tree"){
        NULL
    } else if(radiocal==12){
        NULL
    } else if(radiocal==13){
        NULL
    }
}

bartBetaUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==0){
        sliderInput("bartbeta", label="Beta", min=1, max=2, step=1, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        NULL
    } else if(radiocal==10 && xgbtype=="Tree"){
        sliderInput("bartbeta", label="Beta", min=1, max=3, step=1, value=selection)
    } else if(radiocal==10 && xgbtype!="Tree"){
        NULL
    } else if(radiocal==11 && xgbtype=="Tree"){
        sliderInput("bartbeta", label="Beta", min=1, max=3, step=1, value=selection)
    } else if(radiocal==11 && xgbtype!="Tree"){
        NULL
    } else if(radiocal==12){
        NULL
    } else if(radiocal==13){
        NULL
    }
}

bartNuUI <- function(radiocal, selection, xgbtype="Tree"){
    if(radiocal==0){
        sliderInput("bartnu", label="Degrees of Freedom", min=1, max=2, step=1, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        NULL
    } else if(radiocal==10 && xgbtype=="Tree"){
        sliderInput("bartnu", label="Degrees of Freedom", min=1, max=5, step=1, value=selection)
    } else if(radiocal==10 && xgbtype!="Tree"){
        NULL
    } else if(radiocal==11 && xgbtype=="Tree"){
        sliderInput("bartnu", label="Degrees of Freedom", min=1, max=5, step=1, value=selection)
    } else if(radiocal==11 && xgbtype!="Tree"){
        NULL
    } else if(radiocal==12){
        NULL
    } else if(radiocal==13){
        NULL
    }
}

svmCUI <- function(radiocal, selection){
    if(radiocal==0){
        sliderInput("svmc", label="Cost", min=1, max=5, step=1, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        NULL
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
        sliderInput("svmc", label="Cost", min=1, max=5, step=1, value=selection)
    } else if(radiocal==13){
        sliderInput("svmc", label="Cost", min=1, max=5, step=1, value=selection)
    }
}

svmDegreeUI <- function(radiocal, selection, xgbtype="Linear"){
    if(radiocal==0){
        sliderInput("svmdegree", label="Degree", min=1, max=5, step=1, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        NULL
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
        if(xgbtype=="Polynomial"){
            sliderInput("svmdegree", label="Degree", min=1, max=5, step=1, value=selection)
        } else {
            NULL
        }
    } else if(radiocal==13){
        if(xgbtype=="Polynomial"){
            sliderInput("svmdegree", label="Degree", min=1, max=5, step=1, value=selection)
        } else {
            NULL
        }
    }
}

svmScaleUI <- function(radiocal, selection, xgbtype="Linear"){
    if(radiocal==0){
        sliderInput("svmscale", label="Scale", min=1, max=5, step=1, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        NULL
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
        if(xgbtype=="Polynomial"){
            sliderInput("svmscale", label="Scale", min=1, max=5, step=1, value=selection)
        } else {
            NULL
        }
    } else if(radiocal==13){
        if(xgbtype=="Polynomial"){
            sliderInput("svmscale", label="Scale", min=1, max=5, step=1, value=selection)
        } else {
            NULL
        }
    }
}

svmSigmaUI <- function(radiocal, selection, xgbtype="Linear"){
    if(radiocal==0){
        sliderInput("svmsigma", label="Sigma", min=1, max=5, step=1, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        NULL
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
        if(xgbtype=="Radial" | xgbtype=="Radial Cost" | xgbtype=="Radial Sigma"){
            sliderInput("svmsigma", label="Sigma", min=1, max=5, step=1, value=selection)
        } else {
            NULL
        }
    } else if(radiocal==13){
        if(xgbtype=="Radial" | xgbtype=="Radial Cost" | xgbtype=="Radial Sigma"){
            sliderInput("svmsigma", label="Sigma", min=1, max=5, step=1, value=selection)
        } else {
            NULL
        }
    }
}

svmLengthUI <- function(radiocal, selection, xgbtype="Linear"){
    if(radiocal==0){
        sliderInput("svmlength", label="Length", min=1, max=5, step=1, value=selection)
    } else if(radiocal==1){
        NULL
    } else if(radiocal==2){
        NULL
    } else if(radiocal==3){
        NULL
    } else if(radiocal==4){
        NULL
    }  else if(radiocal==5){
        NULL
    } else if(radiocal==6){
        NULL
    } else if(radiocal==7){
        NULL
    } else if(radiocal==8){
        NULL
    } else if(radiocal==9){
        NULL
    } else if(radiocal==10){
        NULL
    } else if(radiocal==11){
        NULL
    } else if(radiocal==12){
        if(xgbtype=="Boundrange String"){
            sliderInput("svmlength", label="Length", min=1, max=5, step=1, value=selection)
        } else {
            NULL
        }
    } else if(radiocal==13){
        if(xgbtype=="Boundrange String"){
            sliderInput("svmlength", label="Length", min=1, max=5, step=1, value=selection)
        } else {
            NULL
        }
    }
}

nThreads <- function(open_mp=FALSE, nthreads=-1){
    
    if(Sys.info()[["machine"]]=="arm64"){
        nthreads <- 1
    }
    if(open_mp==TRUE){
        sliderInput("open_mp_threads", label="nthreads", min=-1, max=(as.numeric(my.cores)+2), step=1, value=nthreads)
    } else if(open_mp==FALSE){
        NULL
    }
}

lineSubset <- function(spectra, definitions){
    xrf_parse(range.table=definitions, data=spectra)
}

spectraData <- function(spectra, element.lines.to.use, definitions){
    
    line.data <- elementFrame(data=spectra, elements=element.lines.to.use)
    
    table <- definitions
    table <- table[complete.cases(table),]
    
    line.subset <- lineSubset(spectra=spectra, definitions=definitions)
    
    result <- if(length(table[,1])==0){
        line.data
    } else if(length(table[,1])!=0){
        merge(line.data, line.subset, by="Spectrum")
    }
    
    return(result)
}

netData <- function(spectra, element.lines.to.use){
    
    net.data <- spectra
    
    elements <- element.lines.to.use
    
    
    net.data.partial <- net.data[,elements]
    net.data <- data.frame(net.data$Spectrum ,net.data.partial)
    colnames(net.data) <- c("Spectrum", elements)
    net.data <- net.data[order(as.character(net.data$Spectrum)),]
    
    net.data$Spectrum <- gsub(".csv", "", net.data$Spectrum)
    net.data$Spectrum <- gsub(".CSV", "", net.data$Spectrum)
    
    return(net.data)
    
}

holdFrameGen <- function(intensities, values, element){
    spectra.line.table <- intensities
    concentration.table <- values
    spectra.line.table$Spectrum <- concentration.table$Spectrum
    
    concentration.table <- concentration.table[concentration.table$Spectrum %in% spectra.line.table$Spectrum,]
    spectra.line.table <- spectra.line.table[spectra.line.table$Spectrum %in% concentration.table$Spectrum,]
    
    concentration <- as.vector(as.numeric(unlist(concentration.table[,element])))
    
    hold.frame <- data.frame(spectra.line.table, Concentration=concentration)
    
    return(hold.frame[complete.cases(hold.frame),])
}

spectrumSelect <- function(spectra, hold.frame){
    data <- spectra
    return(data[data$Spectrum %in% hold.frame$Spectrum, ])
}

predictIntensitySimpPreGen <- function(spectra, hold.frame, deconvolution = NULL, element, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra", compton.type="Raw"){

    data <- if(compton.type=="Raw"){
        spectra
    } else if(compton.type=="Baseline"){
        deconvolution$Baseline
    } else if(compton.type=="Net"){
        deconvolution$Spectra
    }
    # Keep `data` aligned with the active calibration set. dataNormCal() already
    # does this for Raw upstream; deconvolution$Baseline / $Spectra come from
    # calMemory and aren't refiltered when the user deselects standards, so do
    # it here. Without this, prep helpers that aggregate per Spectrum (e.g.
    # lucas_comp_prep_xrf) build a vector longer than `intensity` and the
    # downstream data.frame() fails with "differing number of rows".
    if(!is.null(data) && "Spectrum" %in% colnames(data)){
        data <- data[data$Spectrum %in% hold.frame$Spectrum, ]
    }
    spectra.line.table <- hold.frame
    
    
    predict.intensity <- if(norm.type==1){
        if(data.type=="Spectra"){
            general_prep_xrf(spectra.line.table=spectra.line.table, element.line=element)
        } else if(data.type=="Net"){
            general_prep_xrf_net(spectra.line.table=spectra.line.table, element.line=element)
        }
    } else if(norm.type==2){
        if(data.type=="Spectra"){
            simple_tc_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=element)
        } else if(data.type=="Net"){
            simple_tc_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=element)
        }
    } else if(norm.type==3){if(data.type=="Spectra"){
            simple_comp_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=element, norm.min=norm.min, norm.max=norm.max)
        } else if(data.type=="Net"){
            simple_comp_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=element, norm.min=norm.min, norm.max=norm.max)
        }
    }
    
    return(predict.intensity)
}

scaleTransform <- function(values, y_min, y_max){
    
    y_min <- my.min(values)
    y_max <- my.max(values)
    y_train_scale <- ((values-y_min)/(y_max-y_min))

    return(y_train_scale)
}

scaleDecode <- function(values, y_min, y_max){
    
    y_train_decoded <- (values*(y_max-y_min)) + y_min

    return(y_train_decoded)
}


predictFrameSimpGen <- function(spectra, hold.frame, deconvolution=NULL, dependent.transformation="None", element, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra", compton.type="Raw", y_min=0, y_max=1, seed=NULL){

    data <- spectra
    spectra.line.table <- hold.frame

    predict.intensity.simp <- predictIntensitySimpPreGen(spectra=spectra, hold.frame=hold.frame, deconvolution=deconvolution, element=element, norm.type=norm.type, norm.min=norm.min, norm.max=norm.max, data.type=data.type, compton.type=compton.type)

    # Include Spectrum for proper data linkage (preserves standard identity)
    predict.frame.simp <- data.frame(
        Spectrum = spectra.line.table[, "Spectrum"],
        predict.intensity.simp,
        Concentration = spectra.line.table[, "Concentration"],
        stringsAsFactors = FALSE
    )
    predict.frame.simp <- predict.frame.simp[complete.cases(predict.frame.simp$Concentration),]

    predict.frame.simp$Concentration <- if(dependent.transformation=="None"){
        predict.frame.simp$Concentration
    } else if(dependent.transformation=="Log"){
        log(predict.frame.simp$Concentration)
    } else if(dependent.transformation=="Scale"){
        scaleTransform(values=predict.frame.simp$Concentration, y_min=y_min, y_max=y_max)
    }

    result <- predictFrameCheck(predict.frame.simp)
    set.seed(seed)
    result$RandXXX <- rnorm(nrow(result), 1, 0.2)
    result <- result[order(result$RandXXX),!colnames(result) %in% "RandXXX"]


    return(result)

}

predictIntensitySimp <- function(predict.frame){
    predict.frame[,!(colnames(predict.frame) %in% "Concentration")]
}

predictIntensityForestPreGen <- function(spectra, hold.frame, deconvolution=NULL, element, intercepts=NULL, slopes=NULL, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra", compton.type="Raw"){

    data <- if(compton.type=="Raw"){
        spectra
    } else if(compton.type=="Baseline"){
        deconvolution$Baseline
    } else if(compton.type=="Net"){
        deconvolution$Spectra
    }
    # See predictIntensitySimpPreGen: deconvolution slots aren't refiltered when
    # the user deselects calibration standards, so align `data` with
    # hold.frame$Spectrum here to match what dataNormCal() does for Raw.
    if(!is.null(data) && "Spectrum" %in% colnames(data)){
        data <- data[data$Spectrum %in% hold.frame$Spectrum, ]
    }

    spectra.line.table <- hold.frame
    element.lines.to.use <- if(is.null(slopes)){
        names(hold.frame)[!names(hold.frame) %in% c("Spectrum", "Concentration")]
    } else if(!is.null(slopes)){
        slopes
    }
    
    
    predict.intensity <- if(norm.type==1){
        if(data.type=="Spectra"){
            lucas_simp_prep_xrf(spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=element.lines.to.use, intercept.element.lines=intercepts)
        } else if(data.type=="Net"){
            lucas_simp_prep_xrf_net(spectra.line.table=spectra.line.table, element.line=input$calcurveelement, slope.element.lines=element.lines.to.use, intercept.element.lines=intercepts)
        }
    } else if(norm.type==2){
        if(data.type=="Spectra"){
            lucas_tc_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=element.lines.to.use, intercept.element.lines=intercepts)
        } else if(data.type=="Net"){
            lucas_tc_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=element.lines.to.use, intercept.element.lines=intercepts)
        }
    } else if(norm.type==3){
        if(data.type=="Spectra"){
            lucas_comp_prep_xrf(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=element.lines.to.use, intercept.element.lines=intercepts, norm.min=norm.min, norm.max=norm.max)
        } else if(data.type=="Net"){
            lucas_comp_prep_xrf_net(data=data, spectra.line.table=spectra.line.table, element.line=element, slope.element.lines=element.lines.to.use, intercept.element.lines=intercepts, norm.min=norm.min, norm.max=norm.max)
        }
    }
    
    return(predict.intensity)
}

predictFrameXGBoostGen <- function(spectra, hold.frame, deconvolution=NULL, slopes=NULL, dependent.transformation="None", element, intercepts=NULL, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra", y_min=0, y_max=1, compton.type="Raw"){

    spectra.line.table <- hold.frame

    predict.intensity.forest <- predictIntensityForestPreGen(spectra=spectra, hold.frame=hold.frame, deconvolution=deconvolution, element=element, slopes=slopes, intercepts=intercepts, norm.type=norm.type, norm.min=norm.min, norm.max=norm.max, data.type=data.type, compton.type="Raw")

    # Include Spectrum for proper data linkage (preserves standard identity)
    predict.frame.forest <- data.frame(
        Spectrum = spectra.line.table[, "Spectrum"],
        predict.intensity.forest,
        Concentration = spectra.line.table[, "Concentration"],
        stringsAsFactors = FALSE
    )
    predict.frame.forest <- predict.frame.forest[complete.cases(predict.frame.forest$Concentration),]

    predict.frame.forest$Concentration <- if(dependent.transformation=="None"){
        predict.frame.forest$Concentration
    } else if(dependent.transformation=="Log"){
        log(predict.frame.forest$Concentration)
    } else if(dependent.transformation=="Scale"){
        scaleTransform(values=predict.frame.forest$Concentration, y_min=y_min, y_max=y_max)
    }

    # Return data frame (matrix conversion done elsewhere for XGBoost when needed)
    return(predictFrameCheck(predict.frame.forest))

}

predictIntensityXGBoost <- function(predict.frame){
    as.matrix(predict.frame[,!(colnames(predict.frame) %in% "Concentration")])
}


predictFrameForestGen <- function(seed=1, spectra, hold.frame, deconvolution=NULL, slopes=NULL, dependent.transformation="None", element, intercepts=NULL, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra", y_min=0, y_max=1, compton.type="Raw"){

    spectra.line.table <- hold.frame

    predict.intensity.forest <- predictIntensityForestPreGen(spectra=spectra, hold.frame=hold.frame, deconvolution=deconvolution, element=element, slopes=slopes, intercepts=intercepts, norm.type=norm.type, norm.min=norm.min, norm.max=norm.max, data.type=data.type, compton.type=compton.type)

    # Include Spectrum for proper data linkage (preserves standard identity)
    predict.frame.forest <- data.frame(
        Spectrum = spectra.line.table[, "Spectrum"],
        predict.intensity.forest,
        Concentration = spectra.line.table[, "Concentration"],
        stringsAsFactors = FALSE
    )
    predict.frame.forest <- predict.frame.forest[complete.cases(predict.frame.forest$Concentration),]
    
    predict.frame.forest$Concentration <- if(dependent.transformation=="None"){
        predict.frame.forest$Concentration
    } else if(dependent.transformation=="Log"){
        log(predict.frame.forest$Concentration)
    } else if(dependent.transformation=="Scale"){
        scaleTransform(values=predict.frame.forest$Concentration, y_min=y_min, y_max=y_max)
    }
    
    result <- predictFrameCheck(predict.frame.forest)
    set.seed(seed)
    result$RandXXX <- rnorm(nrow(result), 1, 0.2)
    result <- result[order(result$RandXXX),!colnames(result) %in% "RandXXX"]

    
    return(result)
        
}

predictIntensityForest <- function(predict.frame){
    predict.frame[,!(colnames(predict.frame) %in% "Concentration")]
}

predictIntensityLucPreGen <- function(spectra, hold.frame, deconvolution = NULL, element, intercepts=NULL, slopes, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra", compton.type="Raw"){
    
    predict.intensity.forest <- predictIntensityForestPreGen(spectra=spectra, hold.frame=hold.frame, deconvolution=deconvolution, element=element, intercepts=intercepts, norm.type=norm.type, norm.min=norm.min, norm.max=norm.max, data.type=data.type, compton.type=compton.type)

    predict.intensity.forest[,c("Intensity", slopes)]
    
}

predictFrameLucGen <- function(seed=1, spectra, hold.frame, element, intercepts=NULL, slopes, dependent.transformation="None", deconvolution = NULL, norm.type, norm.min=NULL, norm.max=NULL, compton.type="Raw", data.type="Spectra", y_min=0, y_max=1){

    spectra.line.table <- hold.frame

    predict.intensity.luc <- predictIntensityLucPreGen(spectra=spectra, hold.frame=hold.frame, deconvolution=deconvolution, element=element, intercepts=intercepts, slopes=slopes, norm.type=norm.type, norm.min=norm.min, norm.max=norm.max, data.type=data.type, compton.type=compton.type)

    # Include Spectrum for proper data linkage (preserves standard identity)
    predict.frame.luc <- data.frame(
        Spectrum = spectra.line.table[, "Spectrum"],
        predict.intensity.luc,
        Concentration = spectra.line.table[, "Concentration"],
        stringsAsFactors = FALSE
    )
    predict.frame.luc <- predict.frame.luc[complete.cases(predict.frame.luc),]
    
    predict.frame.luc$Concentration <- if(dependent.transformation=="None"){
        predict.frame.luc$Concentration
    } else if(dependent.transformation=="Log"){
        log(predict.frame.luc$Concentration)
    } else if(dependent.transformation=="Scale"){
        scaleTransform(values=predict.frame.luc$Concentration, y_min=y_min, y_max=y_max)
    }
    
    result <- predictFrameCheck(predict.frame.luc)
    #set.seed(seed)
    #result$RandXXX <- rnorm(nrow(result), 1, 0.2)
    #result <- result[order(result$RandXXX),!colnames(result) %in% "RandXXX"]

    
    return(result)
}

predictIntensityLuc <- function(predict.frame){
    predict.frame[,!(colnames(predict.frame) %in% "Concentration")]
}

rainforestDataPreGen <- function(spectra, compress="100 eV", transformation="None", energy.range=c(0.7, 37), norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra"){
    
    spectra.data <- if(norm.type==1){
        if(data.type=="Spectra"){
            spectra_simp_prep_xrf(spectra=spectra, compress=compress, energy.min=energy.range[1], energy.max=energy.range[2], transformation=transformation)
        } else if(data.type=="Net"){
            NULL
        }
    } else if(norm.type==2){
        if(data.type=="Spectra"){
            spectra_tc_prep_xrf(spectra=spectra, compress=compress, energy.min=energy.range[1], energy.max=energy.range[2], transformation=transformation)
        } else if(data.type=="Net"){
            NULL
        }
    } else if(norm.type==3){
        if(data.type=="Spectra"){
            spectra_comp_prep_xrf(spectra=spectra, compress=compress, energy.min=energy.range[1], energy.max=energy.range[2], transformation=transformation, norm.min=norm.min, norm.max=norm.max)
        } else if(data.type=="Net"){
            NULL
        }
    }
    
    return(spectra.data)
}


xgboostDataGen <- function(spectra, compress="100 eV", transformation="None", dependent.transformation="None", energy.range=c(0.7, 37), hold.frame, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra", y_min=0, y_max=0){
    
    spectra.line.table <- hold.frame
    
    spectra.data <- rainforestDataPreGen(spectra=spectra, compress=compress, transformation=transformation, energy.range=energy.range, norm.type=norm.type, norm.min=norm.min, norm.max=norm.max, data.type=data.type)
    
    spectra.data <- merge(spectra.data, hold.frame[,c("Spectrum", "Concentration")], by="Spectrum")
    spectra.data <- spectra.data[complete.cases(spectra.data$Concentration),]
    
    spectra.data$Concentration <- if(dependent.transformation=="None"){
        spectra.data$Concentration
    } else if(dependent.transformation=="Log"){
        log(spectra.data$Concentration)
    } else if(dependent.transformation=="Scale"){
        scaleTransform(values=spectra.data$Concentration, y_min=y_min, y_max=y_max)
    }
    
    return(as.matrix(predictFrameCheck(spectra.data)))
}

xgboostIntensity <- function(rainforest.data){
    as.matrix(rainforest.data[,!(colnames(rainforest.data) %in% "Concentration")])
}


rainforestDataGen <- function(seed=1, spectra, compress="100 eV", transformation="None", dependent.transformation="None", energy.range=c(0.7, 37), hold.frame, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra", y_min=0, y_max=1, compton.type="Raw"){
    
    spectra.line.table <- hold.frame
    
    spectra.data <- rainforestDataPreGen(spectra=spectra, compress=compress, transformation=transformation, energy.range=energy.range, norm.type=norm.type, norm.min=norm.min, norm.max=norm.max, data.type=data.type)
    
    spectra.data <- merge(spectra.data, hold.frame[,c("Spectrum", "Concentration")], by="Spectrum")
    spectra.data <- spectra.data[complete.cases(spectra.data$Concentration),]
    
    spectra.data$Concentration <- if(dependent.transformation=="None"){
        spectra.data$Concentration
    } else if(dependent.transformation=="Log"){
        log(spectra.data$Concentration)
    } else if(dependent.transformation=="Scale"){
        scaleTransform(values=spectra.data$Concentration, y_min=y_min, y_max=y_max)
    }
    
    result <- predictFrameCheck(spectra.data)
    set.seed(seed)
    result$RandXXX <- rnorm(nrow(result), 1, 0.2)
    result <- result[order(result$RandXXX),!colnames(result) %in% "RandXXX"]

    
    return(result)
    }

rainforestIntensity <- function(rainforest.data){
    rainforest.data[,!(colnames(rainforest.data) %in% "Concentration")]
}


predictFrame <- function(cal.type, spectra){
    if (input$radiocal==1){
        predictFrameSimp()
    } else if(input$radiocal==2){
        predictFrameSimp()
    } else if(input$radiocal==3){
        predictFrameLuc()
    } else if(input$radiocal==4){
        predictFrameForest()
    } else if(input$radiocal==5){
        rainforestData()
    } else if(input$radiocal==6){
        predictFrameForest()
    } else if(input$radiocal==7){
        rainforestData()
    } else if(input$radiocal==8){
        predictFrameForest()
    } else if(input$radiocal==9){
        rainforestData()
    } else if(input$radiocal==10){
        predictFrameForest()
    } else if(input$radiocal==11){
        rainforestData()
    } else if(input$radiocal==12){
        predictFrameForest()
    } else if(input$radiocal==13){
        rainforestData()
    }
}


valFrame <- function(predict.intensity, predict.frame, element.model.list, cal.type){
    element.model <- element.model.list[[2]]
    
    
    if (cal.type==1){
        cal.est.conc.pred <- predict(object=element.model, newdata=predict.intensity, interval='confidence')
        cal.est.conc.tab <- data.frame(cal.est.conc.pred)
        cal.est.conc <- cal.est.conc.tab$fit
        
        val.frame <- data.frame(na.omit(predict.frame$Concentration), cal.est.conc)
        colnames(val.frame) <- c("Concentration", "Prediction")
    }
    
    if (cal.type==2){
        cal.est.conc.pred <- predict(object=element.model, newdata=predict.intensity, interval='confidence')
        cal.est.conc.tab <- data.frame(cal.est.conc.pred)
        cal.est.conc <- cal.est.conc.tab$fit
        
        val.frame <- data.frame(na.omit(predict.frame$Concentration), cal.est.conc)
        colnames(val.frame) <- c("Concentration", "Prediction")
    }
    
    if (cal.type==3){
        
        
        cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity, interval='confidence')
        cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
        cal.est.conc.luc <- cal.est.conc.tab$fit
        cal.est.conc.luc.up <- cal.est.conc.tab$upr
        cal.est.conc.luc.low <- cal.est.conc.tab$lwr
        
        
        val.frame <- data.frame(predict.frame$Concentration, predict.intensity$Intensity, cal.est.conc.luc, cal.est.conc.luc, cal.est.conc.luc.up, cal.est.conc.luc.low)
        colnames(val.frame) <- c("Concentration", "IntensityOrg", "Intensity", "Prediction", "Upper", "Lower")
    }
    
    if (cal.type==4){
        
        
        
        cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity)
        #cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
        #cal.est.conc.luc <- cal.est.conc.tab$fit
        #cal.est.conc.luc.up <- cal.est.conc.tab$upr
        #cal.est.conc.luc.low <- cal.est.conc.tab$lwr
        
        
        val.frame <- data.frame(predict.frame$Concentration, predict.intensity$Intensity, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
        colnames(val.frame) <- c("Concentration", "IntensityOrg", "Intensity", "Prediction")
    }
    
    
    if (cal.type==5){
        
        
        cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity)
        #cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
        #cal.est.conc.luc <- cal.est.conc.tab$fit
        #cal.est.conc.luc.up <- cal.est.conc.tab$upr
        #cal.est.conc.luc.low <- cal.est.conc.tab$lwr
        
        
        val.frame <- data.frame(predict.frame$Concentration, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
        colnames(val.frame) <- c("Concentration",  "Intensity", "Prediction")
    }
    
    
    if (cal.type==6){
        
        
        cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity)
        #cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
        #cal.est.conc.luc <- cal.est.conc.tab$fit
        #cal.est.conc.luc.up <- cal.est.conc.tab$upr
        #cal.est.conc.luc.low <- cal.est.conc.tab$lwr
        
        
        val.frame <- data.frame(predict.frame$Concentration, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
        colnames(val.frame) <- c("Concentration",  "Intensity", "Prediction")
    }
    
    
    if (cal.type==7){
        
        
        cal.est.conc.pred.luc <- predict(object=element.model , newdata=predict.intensity)
        #cal.est.conc.tab <- data.frame(cal.est.conc.pred.luc)
        #cal.est.conc.luc <- cal.est.conc.tab$fit
        #cal.est.conc.luc.up <- cal.est.conc.tab$upr
        #cal.est.conc.luc.low <- cal.est.conc.tab$lwr
        
        
        val.frame <- data.frame(predict.frame$Concentration, as.vector(cal.est.conc.pred.luc), as.vector(cal.est.conc.pred.luc))
        colnames(val.frame) <- c("Concentration",  "Intensity", "Prediction")
    }
    
    
    
    
    return(val.frame)
}

calCurvePlot <- function(predict.frame, element.model.list, val.frame, element, cal.type, unit="%"){
    
    element.name <- if(element %in% spectralLines){
        gsub("[.]", "", substr(element, 1, 2))
    } else {
        element
    }
    
    intens <- " Counts per Second"
    norma <- " Normalized"
    norma.comp <- " Compton Normalized"
    norma.tc <- " Valid Counts Normalized"
    conen <- paste0(" ", unit)
    predi <- paste0(" Estimate ", unit)
    log <- "Log "
    
    
    intensity.name <- c(element.name, intens)
    concentration.name <- c(element.name, conen)
    prediction.name <- c(element.name, predi)
    
    use.standards <- element.model.list[[1]]$StandardsUsed
    element.model <- element.model.list[[2]]
    
    
    
    
    if(cal.type==1){
        calcurve.plot <- ggplot(data=predict.frame[use.standards, , drop = FALSE], aes(Intensity, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn(lm(Concentration~Intensity, predict.frame[use.standards, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm", fullrange = TRUE) +
        geom_point() +
        geom_point(data = predict.frame[!use.standards, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
        scale_x_continuous(paste(element.name, intens), breaks=scales::pretty_breaks()) +
        scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
        coord_cartesian(expand = TRUE)
        
    }
    
    if(cal.type==2){
        calcurve.plot <- ggplot(data=predict.frame[use.standards, , drop = FALSE], aes(Intensity, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn_poly(lm(Concentration~Intensity + I(Intensity^2), predict.frame[use.standards, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        stat_smooth(method="lm", formula=y~poly(x,2)) +
        geom_point() +
        geom_point(data = predict.frame[!use.standards, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
        scale_x_continuous(paste(element.name, intens), breaks=scales::pretty_breaks()) +
        scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
        coord_cartesian(expand = TRUE)
        
    }
    
    if(cal.type==3){
        calcurve.plot <- ggplot(data=val.frame[use.standards, , drop = FALSE], aes(Intensity, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn(lm(Concentration~., val.frame[use.standards, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        geom_smooth(aes(x=Intensity, y=Concentration, ymin = Lower, ymax = Upper)) +
        geom_point() +
        geom_point(aes(Intensity, Concentration), data = val.frame[!use.standards, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
        scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
        scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
        coord_cartesian(expand = TRUE)
        
    }
    
    if(cal.type==4){
        calcurve.plot <- ggplot(data=val.frame[use.standards, , drop = FALSE], aes(Intensity, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn(lm(Concentration~., val.frame[use.standards, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        geom_smooth() +
        geom_point() +
        geom_point(aes(Intensity, Concentration), data = val.frame[!use.standards, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
        scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
        scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
        coord_cartesian(expand = TRUE)
        
    }
    
    if(cal.type==5){
        calcurve.plot <- ggplot(data=val.frame[use.standards, , drop = FALSE], aes(Intensity, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn(lm(Concentration~., val.frame[use.standards, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        geom_smooth() +
        geom_point() +
        geom_point(aes(Intensity, Concentration), data = val.frame[!use.standards, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
        scale_x_continuous(paste(element.name, norma), breaks=scales::pretty_breaks()) +
        scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
        coord_cartesian(expand = TRUE)
        
    }
    
    if(cal.type==6){
        
        calcurve.plot <- grobTree(plot.nnet(element.model,nid=T))
        
    }
    
    if(cal.type==7){
        
        calcurve.plot <- grobTree(plot.nnet(element.model,nid=T))
        
        
    }
    
    return(calcurve.plot)
}

valCurvePlotGen <- function(element, calibration, unit){
    
    
    
    element.name <- if(element %in% spectralLines){
        gsub("[.]", "", substr(element, 1, 2))
    } else {
        element
    }
    
    intens <- " Counts per Second"
    norma <- " Normalized"
    norma.comp <- " Compton Normalized"
    norma.tc <- " Valid Counts Normalized"
    conen <- paste0(" ", unit)
    predi <- paste0(" Estimate ", unit)
    log <- "Log "
    
    intensity.name <- c(element.name, intens)
    concentration.name <- c(element.name, conen)
    prediction.name <- c(element.name, predi)
    val.frame <- valFrame()
    
    use.standards <- element.model.list[[1]]$StandardsUsed


    valcurve.plot <- ggplot(data=val.frame[use.standards, , drop = FALSE], aes(Prediction, Concentration)) +
    theme_bw() +
    annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame[use.standards, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
    geom_abline(intercept=0, slope=1, lty=2) +
    stat_smooth(method="lm") +
    geom_point() +
    geom_point(aes(Prediction, Concentration),  data = val.frame[!use.standards, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
    scale_x_continuous(paste(element.name, predi), breaks=scales::pretty_breaks()) +
    scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
    coord_cartesian(expand = TRUE)
    

    return(valcurve.plot)
}

modelSummaryPre <- function(element.model, element.name){
    
    model.class <- if(element.model[[1]][["CalTable"]]$CalType[1]==1){
        "Regression"
    } else if(element.model[[1]][["CalTable"]]$CalType[1]==2){
        "Regression"
    } else if(element.model[[1]][["CalTable"]]$CalType[1]==3){
        "Regression"
    } else if(element.model[[1]][["CalTable"]]$CalType[1]==4){
        "Caret"
    } else if(element.model[[1]][["CalTable"]]$CalType[1]==5){
        "Caret"
    } else if(element.model[[1]][["CalTable"]]$CalType[1]==6){
        "Caret"
    } else if(element.model[[1]][["CalTable"]]$CalType[1]==7){
        "Caret"
    } else if(element.model[[1]][["CalTable"]]$CalType[1]==8){
        "Caret"
    } else if(element.model[[1]][["CalTable"]]$CalType[1]==9){
        "Caret"
    } else if(element.model[[1]][["CalTable"]]$CalType[1]==10){
        "Caret"
    } else if(element.model[[1]][["CalTable"]]$CalType[1]==11){
        "Caret"
    } else if(element.model[[1]][["CalTable"]]$CalType[1]==12){
        "Caret"
    } else if(element.model[[1]][["CalTable"]]$CalType[1]==13){
        "Caret"
    }
    
    r2 <- if(model.class=="Regression"){
        tryCatch(summary(element.model[[2]])$r.squared, error=function(e) 0)
    } else if(model.class=="Caret"){
        tryCatch(max(element.model[[2]][["results"]]$Rsquared), error=function(e) 0)
    }
    
    data.frame(Element=element.name, R2=tryCatch(round(r2, 2), error=function(e) NULL), stringsAsFactors=FALSE)
}

modelSummary <- function(element.model, element.name){
    if(is.null(element.model)){
        data.frame(Element=element.name, R2=NA, stringsAsFactors=FALSE)
    } else if(!is.null(element.model)){
        modelSummaryPre(element.model=element.model, element.name=element.name)
    }
}

calProgressSummary <- function(calList){
    element.names <- names(calList)
    
    cal.results.list <- lapply(element.names, function(x) modelSummary(element.model=calList[[x]], element.name=x))
    
    rbindlist(cal.results.list)
}



###Spectra Manipulations
logspec <- function(x) ifelse(x!=0, log10(x), 0)
exspec <- function(x) ifelse(x!=0, exp(x), 0)


spectraBackgroundSubtract <- function(spectra){
    spectra$CPS <- Hodder.v(spectra$CPS)
    return(spectra)
}
spectraBackgroundSubtract <- cmpfun(spectraBackgroundSubtract)

dataLog <- function(spectra){
    spectra$CPS <- logspec(spectra$CPS)
    return(spectra)
}
dataLog <- cmpfun(dataLog)

dataExp <- function(spectra){
    spectra$CPS <- exspec(spectra$CPS)
    return(spectra)
}
dataExp <- cmpfun(dataExp)


transformSpectrum <- function(spectra, transformation){
    spectra.transformed <- if(transformation=="None"){
        spectra
    } else if(transformation=="Velocity"){
        spectraBackgroundSubtract(spectra)
    } else if(transformation=="Log"){
        dataLog(spectra)
    } else if(transformation=="e"){
        dataExp(spectra)
    }
    
    return(spectra.transformed)
}
transformSpectrum <- cmpfun(transformSpectrum)

transformSpectra <- function(spectra.frame, transformation){
    spectra.list <- split(spectra.frame , f = spectra.frame$Spectrum )
    names(spectra.list) <- unique(spectra.frame$Spectrum)
    
    spectra.transformed <- lapply(spectra.list, function(x) transformSpectrum(spectra=x, transformation=transformation))
    names(spectra.transformed) <- names(spectra.list)
    return(rbindlist(spectra.transformed))
}

chooseTransformation <- function(spectra=NULL, cal){
    
    spectra <- if(is.null(spectra)){
        cal[["Spectra"]]
    } else if(!is.null(spectra)){
        spectra
    }
    
    spectra.transformed <- if(is.null(cal$Transformation)){
        transformSpectra(cal[["Spectra"]], transformation="None")
    } else if(!is.null(cal$Transformation)){
        transformSpectra(cal[["Spectra"]], transformation=cal$Transformation)
    }
    
    return(spectra.transformed)
}
chooseTransformation <- cmpfun(chooseTransformation)

calBundle <- function(filetype, units, spectra, intensities, wide.intensities, definitions, values, notes, calList, compress=FALSE){
    
    list(FileType=filetype, Units=units, Spectra=spectra, Intensities=intensities, WideIntensities=wide.intensities, Definitions=definitions, Values=values, Notes=notes, calList=calListCompress(calList))
    
}


cloudCalPredict <- function(Calibration, elements.cal, elements, variables, valdata, deconvoluted_valdata=NULL, count.list=NULL, rounding=4, multiplier=1, confidence=FALSE, cores=NULL){
    
    if(is.null(cores)){
        cores = parallel::detectCores()-2
    }
    
    if(is.null(deconvoluted_valdata)){
        deconvolution_parameters <- Calibration$Deconvoluted$Parameters
        deconvoluted_data <-spectra_gls_deconvolute(valdata, width=deconvolution_parameters$SmoothWidth, alpha=deconvolution_parameters$SmoothAlpha, default_sigma=deconvolution_parameters$DefaultSigma, smooth_iter=deconvolution_parameters$SmoothIter, snip_iter=deconvolution_parameters$SnipIter, cores=1, physics=deconvolution_physics_from_params(deconvolution_parameters), mass=TRUE)
        deconvoluted_valdata <- deconvoluted_data
    }

    other_spectra_stuff <- totalCountsGen(valdata)
    val_extra_cols <- deconvolution_extra_cols(deconvoluted_valdata$Areas)   # Baseline + Compton/Rayleigh if present
    if(length(val_extra_cols) > 0){
        other_spectra_stuff <- merge(other_spectra_stuff, deconvoluted_valdata$Areas[,c("Spectrum", val_extra_cols), drop=FALSE], all=T, sort=T)
    }
        
    
    if(is.null(count.list)){
        count.list <- list(
            Narrow_gaussian=merge(narrowLineTable(spectra=valdata, definition.table=Calibration$Definitions, elements=variables), other_spectra_stuff, by="Spectrum", all=T, sort=T),
            Narrow_first=merge(narrowLineTableFirst(spectra=valdata, definition.table=Calibration$Definitions, elements=variables), other_spectra_stuff, by="Spectrum", all=T, sort=T),
            Narrow_second=merge(narrowLineTableSecond(spectra=valdata, definition.table=Calibration$Definitions, elements=variables), other_spectra_stuff, by="Spectrum", all=T, sort=T),
            Wide_gaussian=merge(wideLineTable(spectra=valdata, definition.table=Calibration$Definitions, elements=variables), other_spectra_stuff, by="Spectrum", all=T, sort=T),
            Narrow_split=merge(narrowLineTableSplit(spectra=valdata, definition.table=Calibration$Definitions, elements=variables), other_spectra_stuff, by="Spectrum", all=T, sort=T),
            Wide_split=merge(wideLineTableSplit(spectra=valdata, definition.table=Calibration$Definitions, elements=variables), other_spectra_stuff, by="Spectrum", all=T, sort=T)
            )
        count.list$Area_gaussian <- merge(deconvolutionIntensityFrame(deconvoluted_valdata$Areas, count.list$Narrow_gaussian), other_spectra_stuff, by="Spectrum", all=T, sort=T)
        count.list$Area_split <- count.list$Area_gaussian
        count.list$Area_first <- count.list$Area_gaussian
        count.list$Area_second <- count.list$Area_gaussian
        count.list$Wide_first <- count.list$Wide_gaussian
        count.list$Wide_second <- count.list$Wide_gaussian
    }
    

    
    
    #count.table <- data.frame(fullInputValCounts())
    the.cal <- Calibration[["calList"]]
    #elements.cal <- calValElements()
    elements <- elements.cal[!is.na(match(elements.cal, names(count.list[["Narrow_gaussian"]])))]
    #elements <- names(Calibration$calList)
    #variables <- calVariableElements()
    #valdata <- myValData()
        #elements <- fluorescence.lines$Symbol[sort(order(fluorescence.lines$Symbol)[elements])]

        cal_type <- function(element){
    
    
            if(the.cal[[element]][[1]]$CalTable$CalType[1]==1){
                    1
                } else if(the.cal[[element]][[1]]$CalTable$CalType[1]==2){
                    1
                } else if(the.cal[[element]][[1]]$CalTable$CalType[1]==3){
                    3
                } else if(the.cal[[element]][[1]]$CalTable$CalType[1]==4){
                    4
                } else if(the.cal[[element]][[1]]$CalTable$CalType[1]==5){
                    5
                } else if(the.cal[[element]][[1]]$CalTable$CalType[1]==6){
                    6
                }  else if(the.cal[[element]][[1]]$CalTable$CalType[1]==7){
                    7
                } else if(the.cal[[element]][[1]]$CalTable$CalType[1]==8){
                    8
                }  else if(the.cal[[element]][[1]]$CalTable$CalType[1]==9){
                    9
                } else if(the.cal[[element]][[1]]$CalTable$CalType[1]==10){
                    10
                }  else if(the.cal[[element]][[1]]$CalTable$CalType[1]==11){
                    11
                } else if(the.cal[[element]][[1]]$CalTable$CalType[1]==12){
                    12
                }  else if(the.cal[[element]][[1]]$CalTable$CalType[1]==13){
                    13
                }
    
        }
        cal_type <- cmpfun(cal_type)

        val.data.type <- if(Calibration[["FileType"]]=="Spectra"){
                "Spectra"
            } else if(Calibration[["FileType"]]=="CSV"){
                "Spectra"
            } else if(Calibration[["FileType"]]=="Aggregate CSV File"){
                "Spectra"
            } else if(Calibration[["FileType"]]=="TXT"){
                "Spectra"
            } else if(Calibration[["FileType"]]=="Net"){
                "Net"
            } else if(Calibration[["FileType"]]=="Elio"){
                "Spectra"
            } else if(Calibration[["FileType"]]=="SPX"){
                "Spectra"
            } else if(Calibration[["FileType"]]=="MCA"){
                "Spectra"
            } else if(Calibration[["FileType"]]=="PDZ"){
                "Spectra"
            } else if(is.null(Calibration[["FileType"]])){
                "Spectra"
            }
            
            count.table <- count.list$Narrow_gaussian
        
        predicted.frame <- data.frame(Spectrum=count.table$Spectrum, stringsAsFactors=FALSE)
            
            #pblapply(elements, function(x) tryCatch(predicted.frame[,x] <-
        
        for(x in elements){
            values <-
            if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==1 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=general_prep_xrf(
                        spectra.line.table=as.data.frame(
                            count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                            ),
                            element.line=x),
                            dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                            ymin=the.cal[[x]][[1]][1]$Scale$Min,
                            ymax=the.cal[[x]][[1]][1]$Scale$Max,
                            confidence=confidence
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==1 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2) {
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=simple_tc_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                            ),
                        deconvolution = deconvoluted_valdata,
                        compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                        element.line=x
                        ),
                        dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                        ymin=the.cal[[x]][[1]][1]$Scale$Min,
                        ymax=the.cal[[x]][[1]][1]$Scale$Max,
                        confidence=confidence
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==1 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3) {
                mclPred(
                    object=the.cal[[x]][["Model"]],
                        newdata=simple_comp_prep_xrf(
                            data=valdata,
                            spectra.line.table=as.data.frame(
                            count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                                ),
                            deconvolution = deconvoluted_valdata,
                            compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                            element.line=x,
                            norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                            norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                            ),
                            dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                            ymin=the.cal[[x]][[1]][1]$Scale$Min,
                            ymax=the.cal[[x]][[1]][1]$Scale$Max,
                            confidence=confidence
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==3 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                 mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_simp_prep_xrf(
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                            ),
                        element.line=x,
                        slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                        ),
                        dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                        ymin=the.cal[[x]][[1]][1]$Scale$Min,
                        ymax=the.cal[[x]][[1]][1]$Scale$Max,
                        confidence=confidence
                 )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==3 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_tc_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                            ),
                        deconvolution = deconvoluted_valdata,
                        compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                        element.line=x,
                        slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                        ),
                        dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                        ymin=the.cal[[x]][[1]][1]$Scale$Min,
                        ymax=the.cal[[x]][[1]][1]$Scale$Max,
                        confidence=confidence
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==3 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_comp_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                            ),
                            deconvolution = deconvoluted_valdata,
                            compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                            element.line=x,
                            slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                            norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                            norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                            ),
                        dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                        ymin=the.cal[[x]][[1]][1]$Scale$Min,
                        ymax=the.cal[[x]][[1]][1]$Scale$Max,
                        confidence=confidence
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None"  && cal_type(x)==4 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_simp_prep_xrf(
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                        ),
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==4 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_tc_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                        ),
                        deconvolution = deconvoluted_valdata,
                        compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==4 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][["Model"]],
                    newdata=lucas_comp_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                        ),
                        deconvolution = deconvoluted_valdata,
                        compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                    ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==5 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=spectra_simp_prep_xrf(
                        spectra=valdata,
                        energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                        energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                        compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                        transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1]
                        )[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==5 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_tc_prep_xrf(spectra=valdata,
                        energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                        energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                        compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                        transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1]
                        )[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==5 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_comp_prep_xrf(spectra=valdata,
                        energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                        energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                        compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                        transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1],
                            norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                            norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1])[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==6 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_simp_prep_xrf(
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                        ),
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==6 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_tc_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                        ),
                        deconvolution = deconvoluted_valdata,
                        compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==6 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][["Model"]],
                    newdata=lucas_comp_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]][,variables]
                        ),
                        deconvolution = deconvoluted_valdata,
                        compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                    ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==7 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=spectra_simp_prep_xrf(spectra=valdata,
                    energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                    energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                    compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                    transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1]
                    )[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==7 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_tc_prep_xrf(spectra=valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1]
                )[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==7 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_comp_prep_xrf(spectra=valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1],
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1])[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==8 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_simp_prep_xrf(
                    spectra.line.table=as.data.frame(
                    count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                ),
                element.line=x,
                slope.element.lines=variables,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                ),
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==8 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                if(confidence==FALSE){
                    mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_tc_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                        ),
                        deconvolution = deconvoluted_valdata,
                        compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                        element.line=x,
                        slope.element.lines=variables,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=FALSE
                    )
                } else if(confidence==TRUE){
                    mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=xgb.DMatrix(as.matrix(lucas_tc_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1],the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]] [,colnames(the.cal[[x]][["Model"]][["trainingData"]][,c(-1, -2)])]
                        ),
                        deconvolution = deconvoluted_valdata,
                        compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                        element.line=x,
                        slope.element.lines=colnames(the.cal[[x]][["Model"]][["trainingData"]][,c(-1, -2)]),
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                        ))),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                    )
                }
                
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==8 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_comp_prep_xrf(
                    data=valdata,
                    spectra.line.table=as.data.frame(
                    count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                    ),
                    deconvolution = deconvoluted_valdata,
                    compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                    ),
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==9 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_simp_prep_xrf(spectra=valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1]
                )[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==9 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_tc_prep_xrf(spectra=valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1]
                )[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==9 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_comp_prep_xrf(spectra=valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1],
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1])[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==10 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_simp_prep_xrf(
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                        ),
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==10 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_tc_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                        ),
                        deconvolution = deconvoluted_valdata,
                        compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                        element.line=x,
                        slope.element.lines=variables,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                        ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==10 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][["Model"]],
                    newdata=lucas_comp_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                        ),
                        deconvolution = deconvoluted_valdata,
                        compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                        element.line=x,
                        slope.element.lines=variables,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                        norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                        norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                        ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==11 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=spectra_simp_prep_xrf(spectra=valdata,
                    energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                    energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                    compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                    transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1]
                    )[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==11 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_tc_prep_xrf(spectra=valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1]
                )[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==11 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_comp_prep_xrf(spectra=valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1],
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1])[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==12 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_simp_prep_xrf(
                    spectra.line.table=as.data.frame(
                    count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                ),
                element.line=x,
                slope.element.lines=variables,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                ),
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==12 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_tc_prep_xrf(
                    data=valdata,
                    spectra.line.table=as.data.frame(
                    count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                    ),
                    deconvolution = deconvoluted_valdata,
                    compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                ),
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==12 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_comp_prep_xrf(
                    data=valdata,
                    spectra.line.table=as.data.frame(
                    count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                    ),
                    deconvolution = deconvoluted_valdata,
                    compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                    ),
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==13 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_simp_prep_xrf(spectra=valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1]
                )[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==13 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_tc_prep_xrf(spectra=valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1]
                )[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="None" && cal_type(x)==13 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_comp_prep_xrf(spectra=valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1],
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1])[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==1 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=general_prep_xrf(
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                            ),
                            element.line=x),
                            dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                            ymin=the.cal[[x]][[1]][1]$Scale$Min,
                            ymax=the.cal[[x]][[1]][1]$Scale$Max,
                            confidence=confidence
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==1 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2) {
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=simple_tc_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                            ),
                            deconvolution = deconvoluted_valdata,
                            compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                            element.line=x
                            ),
                        dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                        ymin=the.cal[[x]][[1]][1]$Scale$Min,
                        ymax=the.cal[[x]][[1]][1]$Scale$Max,
                        confidence=confidence
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==1 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3) {
                mclPred(
                    object=the.cal[[x]][["Model"]],
                        newdata=simple_comp_prep_xrf(
                            data=deconvoluted_valdata,
                            spectra.line.table=as.data.frame(
                            count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                                ),
                                deconvolution = deconvoluted_valdata,
                                compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                                element.line=x,
                                norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                                norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                                ),
                            dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                            ymin=the.cal[[x]][[1]][1]$Scale$Min,
                            ymax=the.cal[[x]][[1]][1]$Scale$Max,
                            confidence=confidence
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==3 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                 mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_simp_prep_xrf(
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                            ),
                        element.line=x,
                        slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                        ),
                        dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                        ymin=the.cal[[x]][[1]][1]$Scale$Min,
                        ymax=the.cal[[x]][[1]][1]$Scale$Max,
                        confidence=confidence
                 )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==3 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_tc_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]][,variables]
                            ),
                            deconvolution = deconvoluted_valdata,
                            compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                            element.line=x,
                            slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                            ),
                        dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                        ymin=the.cal[[x]][[1]][1]$Scale$Min,
                        ymax=the.cal[[x]][[1]][1]$Scale$Max,
                        confidence=confidence
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==3 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_comp_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                            ),
                            deconvolution = deconvoluted_valdata,
                            compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                            element.line=x,
                            slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                            norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                            norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                            ),
                        dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                        ymin=the.cal[[x]][[1]][1]$Scale$Min,
                        ymax=the.cal[[x]][[1]][1]$Scale$Max,
                        confidence=confidence
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares"  && cal_type(x)==4 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_simp_prep_xrf(
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                        ),
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==4 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_tc_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                        ),
                        deconvolution = deconvoluted_valdata,
                        compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                        element.line=x,
                        slope.element.lines=variables,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                        ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==4 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][["Model"]],
                    newdata=lucas_comp_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                        ),
                        deconvolution = deconvoluted_valdata,
                        compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                        element.line=x,
                        slope.element.lines=variables,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                        norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                        norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                        ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==5 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=spectra_simp_prep_xrf(
                        spectra=deconvoluted_valdata,
                        energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                        energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                        compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                        transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1]
                        )[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==5 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_tc_prep_xrf(spectra=deconvoluted_valdata,
                        energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                        energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                        compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                        transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1]
                        )[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==5 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_comp_prep_xrf(spectra=deconvoluted_valdata,
                        energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                        energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                        compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                        transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1],
                            norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                            norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1])[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==6 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_simp_prep_xrf(
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                        ),
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==6 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_tc_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                        ),
                        deconvolution = deconvoluted_valdata,
                        compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                        element.line=x,
                        slope.element.lines=variables,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                        ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==6 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][["Model"]],
                    newdata=lucas_comp_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                        ),
                        deconvolution = deconvoluted_valdata,
                        compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                        element.line=x,
                        slope.element.lines=variables,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                        norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                        norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                        ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==7 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=spectra_simp_prep_xrf(spectra=deconvoluted_valdata,
                    energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                    energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                    compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                    transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1]
                    )[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==7 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_tc_prep_xrf(spectra=deconvoluted_valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1]
                )[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==7 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_comp_prep_xrf(spectra=deconvoluted_valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1],
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1])[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==8 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_simp_prep_xrf(
                    spectra.line.table=as.data.frame(
                    count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                ),
                element.line=x,
                slope.element.lines=variables,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                ),
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==8 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                if(confidence==FALSE){
                    mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_tc_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                        ),
                        deconvolution = deconvoluted_valdata,
                        compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                        element.line=x,
                        slope.element.lines=variables,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                        ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=FALSE
                    )
                } else if(confidence==TRUE){
                    mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=xgb.DMatrix(as.matrix(lucas_tc_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,colnames(the.cal[[x]][["Model"]][["trainingData"]][,c(-1, -2)])]
                        ),
                        deconvolution = deconvoluted_valdata,
                        compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                        element.line=x,
                        slope.element.lines=colnames(the.cal[[x]][["Model"]][["trainingData"]][,c(-1, -2)]),
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                        ))),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                    )
                }
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==8 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_comp_prep_xrf(
                    data=deconvoluted_valdata,
                    spectra.line.table=as.data.frame(
                    count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                    ),
                    deconvolution = deconvoluted_valdata,
                    compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                    ),
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==9 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_simp_prep_xrf(spectra=deconvoluted_valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1]
                )[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==9 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_tc_prep_xrf(spectra=deconvoluted_valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1]
                )[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==9 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_comp_prep_xrf(spectra=deconvoluted_valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1],
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1])[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==10 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_simp_prep_xrf(
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                        ),
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==10 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_tc_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                        ),
                        deconvolution = deconvoluted_valdata,
                        compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                        element.line=x,
                        slope.element.lines=variables,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                        ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==10 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][["Model"]],
                    newdata=lucas_comp_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                        ),
                        deconvolution = deconvoluted_valdata,
                        compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                        element.line=x,
                        slope.element.lines=variables,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                        norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                        norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                        ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==11 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=spectra_simp_prep_xrf(spectra=deconvoluted_valdata,
                    energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                    energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                    compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                    transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1]
                    )[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==11 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_tc_prep_xrf(spectra=deconvoluted_valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1]
                )[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==11 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_comp_prep_xrf(spectra=deconvoluted_valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1],
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1])[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==12 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_simp_prep_xrf(
                    spectra.line.table=as.data.frame(
                    count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                    ),
                    deconvolution = deconvoluted_valdata,
                    compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    ),
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==12 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_tc_prep_xrf(
                    data=deconvoluted_valdata,
                    spectra.line.table=as.data.frame(
                    count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                    ),
                    deconvolution = deconvoluted_valdata,
                    compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    ),
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==12 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_comp_prep_xrf(
                    data=deconvoluted_valdata,
                    spectra.line.table=as.data.frame(
                    count.list[[paste0(the.cal[[x]][["Parameters"]]$CalTable$LineType[1], "_", the.cal[[x]][["Parameters"]]$CalTable$LineStructure[1])]][,variables]
                    ),
                    deconvolution = deconvoluted_valdata,
                    compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                    ),
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==13 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_simp_prep_xrf(spectra=deconvoluted_valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1]
                )[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==13 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_tc_prep_xrf(spectra=deconvoluted_valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1]
                )[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][["Parameters"]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==13 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=spectra_comp_prep_xrf(spectra=deconvoluted_valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][["Parameters"]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][["Parameters"]]$CalTable$Compress[1],
                transformation=the.cal[[x]][["Parameters"]]$CalTable$Transformation[1],
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1])[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Net" && cal_type(x)==1 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=general_prep_xrf_net(
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                            element.line=x),
                            dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                            confidence=confidence
                )
            } else if(val.data.type=="Net" && cal_type(x)==1 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2) {
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=simple_tc_prep_xrf_net(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]]
                            ),
                            deconvolution = deconvoluted_valdata,
                            compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                            element.line=x
                            ),
                            dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                            ymin=the.cal[[x]][[1]][1]$Scale$Min,
                            ymax=the.cal[[x]][[1]][1]$Scale$Max,
                            confidence=confidence
                )
            } else if(val.data.type=="Net" && cal_type(x)==1 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3) {
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=simple_comp_prep_xrf_net(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]]
                            ),
                            deconvolution = deconvoluted_valdata,
                            compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                            element.line=x,
                            norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                            norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                            ),
                        dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                        ymin=the.cal[[x]][[1]][1]$Scale$Min,
                        ymax=the.cal[[x]][[1]][1]$Scale$Max,
                        confidence=confidence
                )
            } else if(val.data.type=="Net" && cal_type(x)==3 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_simp_prep_xrf_net(
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]]
                            ),
                        element.line=x,
                        slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                        ),
                        dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                        ymin=the.cal[[x]][[1]][1]$Scale$Min,
                        ymax=the.cal[[x]][[1]][1]$Scale$Max,
                        confidence=confidence
                )
            } else if(val.data.type=="Net" && cal_type(x)==3 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_tc_prep_xrf_net(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]]
                            ),
                            deconvolution = deconvoluted_valdata,
                            compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                            element.line=x,
                            slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                            ),
                        dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                        ymin=the.cal[[x]][[1]][1]$Scale$Min,
                        ymax=the.cal[[x]][[1]][1]$Scale$Max,
                        confidence=confidence
                )
            } else if(val.data.type=="Net" && cal_type(x)==3 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
                mclPred(
                    object=the.cal[[x]][["Model"]],
                    newdata=lucas_comp_prep_xrf_net(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]]
                            ),
                            deconvolution = deconvoluted_valdata,
                            compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                            element.line=x,
                            slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                            norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                            norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                            ),
                        dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                        ymin=the.cal[[x]][[1]][1]$Scale$Min,
                        ymax=the.cal[[x]][[1]][1]$Scale$Max,
                        confidence=confidence
                )
        } else if(val.data.type=="Net" && cal_type(x)==4 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
            mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_simp_prep_xrf_net(
                    spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]]
                        ),
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
            )
        } else if(val.data.type=="Net" && cal_type(x)==4 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
            mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_tc_prep_xrf_net(
                    data=valdata,
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]]
                            ),
                            deconvolution = deconvoluted_valdata,
                            compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                            element.line=x,
                            slope.element.lines=variables,
                            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                            ),
                        dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                        confidence=confidence,
                        finalModel=TRUE
            )
        } else if(val.data.type=="Net" && cal_type(x)==4 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
            mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_comp_prep_xrf_net(
                data=valdata,
                    spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]]
                        ),
                        deconvolution = deconvoluted_valdata,
                        compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                        element.line=x,
                        slope.element.lines=variables,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                        norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                        norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                        ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
            )
        }  else if(val.data.type=="Net" && cal_type(x)==6 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
            mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_simp_prep_xrf_net(
                    spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]]
                        ),
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
            )
        } else if(val.data.type=="Net" && cal_type(x)==6 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
            mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_tc_prep_xrf_net(
                    data=valdata,
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]]
                            ),
                            deconvolution = deconvoluted_valdata,
                            compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                            element.line=x,
                            slope.element.lines=variables,
                            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                            ),
                        dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                        ymin=the.cal[[x]][[1]][1]$Scale$Min,
                        ymax=the.cal[[x]][[1]][1]$Scale$Max,
                        confidence=confidence,
                        finalModel=TRUE
            )
        } else if(val.data.type=="Net" && cal_type(x)==6 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
            mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_comp_prep_xrf_net(
                data=valdata,
                    spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]]
                        ),
                        deconvolution = deconvoluted_valdata,
                        compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                        element.line=x,
                        slope.element.lines=variables,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                        norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                        norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                        ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
            )
        }  else if(val.data.type=="Net" && cal_type(x)==8 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
            mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_simp_prep_xrf_net(
                    spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]]
                        ),
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
            )
        } else if(val.data.type=="Net" && cal_type(x)==8 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
            mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_tc_prep_xrf_net(
                    data=valdata,
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]]
                            ),
                            deconvolution = deconvoluted_valdata,
                            compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                            element.line=x,
                            slope.element.lines=variables,
                            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                            ),
                        dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                        ymin=the.cal[[x]][[1]][1]$Scale$Min,
                        ymax=the.cal[[x]][[1]][1]$Scale$Max,
                        confidence=confidence,
                        finalModel=TRUE
            )
        } else if(val.data.type=="Net" && cal_type(x)==8 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
            mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_comp_prep_xrf_net(
                data=valdata,
                    spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]]
                        ),
                        deconvolution = deconvoluted_valdata,
                        compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                        element.line=x,
                        slope.element.lines=variables,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                        norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                        norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                        ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
            )
        }  else if(val.data.type=="Net" && cal_type(x)==10 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
            mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_simp_prep_xrf_net(
                    spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]]
                        ),
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
            )
        } else if(val.data.type=="Net" && cal_type(x)==10 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
            mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_tc_prep_xrf_net(
                    data=valdata,
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]]
                            ),
                            deconvolution = deconvoluted_valdata,
                            compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                            element.line=x,
                            slope.element.lines=variables,
                            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                            ),
                        dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                        ymin=the.cal[[x]][[1]][1]$Scale$Min,
                        ymax=the.cal[[x]][[1]][1]$Scale$Max,
                        confidence=confidence,
                        finalModel=TRUE
            )
        } else if(val.data.type=="Net" && cal_type(x)==10 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
            mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_comp_prep_xrf_net(
                data=valdata,
                    spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]]
                            ),
                            deconvolution = deconvoluted_valdata,
                            compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                        element.line=x,
                        slope.element.lines=variables,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                        norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                        norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                        ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
            )
        }  else if(val.data.type=="Net" && cal_type(x)==12 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==1){
            mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_simp_prep_xrf_net(
                    spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]]
                        ),
                    element.line=x,
                    slope.element.lines=variables,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
            )
        } else if(val.data.type=="Net" && cal_type(x)==12 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==2){
            mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_tc_prep_xrf_net(
                    data=valdata,
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]]
                            ),
                            deconvolution = deconvoluted_valdata,
                            compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                            element.line=x,
                            slope.element.lines=variables,
                            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                            ),
                        dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                        ymin=the.cal[[x]][[1]][1]$Scale$Min,
                        ymax=the.cal[[x]][[1]][1]$Scale$Max,
                        confidence=confidence,
                        finalModel=TRUE
            )
        } else if(val.data.type=="Net" && cal_type(x)==12 && the.cal[[x]][["Parameters"]]$CalTable$NormType[1]==3){
            mclPred(
                object=the.cal[[x]][["Model"]],
                newdata=lucas_comp_prep_xrf_net(
                data=valdata,
                    spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][["Parameters"]]$CalTable$LineType[1]]]
                            ),
                            deconvolution = deconvoluted_valdata,
                            compton.type=the.cal[[x]][["Parameters"]]$CalTable$ComptonType[1],
                        element.line=x,
                        slope.element.lines=variables,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                        norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                        norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                        ),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
            )
        }        #, error=function(e) NULL)
        if(!is.null(values)){
            predicted.frame$hold <- values
            colnames(predicted.frame)[which(names(predicted.frame) == "hold")] <- x
        }
        }
            
            #predicted.vector <- unlist(predicted.list)
        
        #predicted.vector <- predicted.vector*multiplier
        
        #predicted.vector <- round(predicted.vector, rounding)

        
        #dim(predicted.vector) <- c(length(count.table$Spectrum), length(elements))
        
        #predicted.frame <- data.frame(count.table$Spectrum, predicted.vector)
        
        #colnames(predicted.frame) <- c("Spectrum", elements)
        #elements <- elements[order(match(fluorescence.lines$Symbol, elements))]

        

        predicted.data.table <- round(predicted.frame[,-1]*multiplier, rounding)

        #predicted.values <- t(predicted.values)
        data.frame(Spectrum=predicted.frame$Spectrum, predicted.data.table, stringsAsFactors=FALSE)
        
        
}


mclValGen <- function(model, data, predict.frame, dependent.transformation, y_min=0, y_max=1){
    # Guard against a missing/NA/unrecognized transformation (e.g. older or large
    # cals whose CalTable$DepTrans is NA). Without this the if-chain below either
    # errors on `if(NA)` or leaves the prediction NULL, which makes the val.frame
    # collapse to all-zero predictions.
    if(is.null(dependent.transformation) || length(dependent.transformation) != 1 ||
       is.na(dependent.transformation) || !dependent.transformation %in% c("None", "Log", "e", "Scale")){
        dependent.transformation <- "None"
    }
    cal.est.conc.pred.luc <- if(dependent.transformation=="None"){
        predict(object=model, newdata=data)
    } else if(dependent.transformation=="Log"){
        exp(predict(object=model, newdata=data))
    } else if(dependent.transformation=="e"){
        log(predict(object=model, newdata=data))
    } else if(dependent.transformation=="Scale"){
        scaleDecode(values=predict(object=model, newdata=data), y_min=y_min, y_max=y_max)
    }
    
    concentration <- if(dependent.transformation=="None"){
        predict.frame$Concentration
    } else if(dependent.transformation=="Log"){
        exp(predict.frame$Concentration)
    } else if(dependent.transformation=="e"){
        log(predict.frame$Concentration)
    } else if(dependent.transformation=="Scale"){
        scaleDecode(values=predict.frame$Concentration, y_min=y_min, y_max=y_max)
    }
        
    # Include Spectrum for proper data linkage (preserves standard identity)
    val.frame <- data.frame(
        Spectrum = predict.frame$Spectrum,
        Concentration = concentration,
        Intensity = as.vector(cal.est.conc.pred.luc),
        Prediction = as.vector(cal.est.conc.pred.luc),
        stringsAsFactors = FALSE
    )

    return(val.frame)
}

xgbValGen <- function(model, data, predict.frame, dependent.transformation, y_min=0, y_max=1){
    if(is.null(dependent.transformation) || length(dependent.transformation) != 1 ||
       is.na(dependent.transformation) || !dependent.transformation %in% c("None", "Log", "e", "Scale")){
        dependent.transformation <- "None"
    }
    cal.est.conc.pred.luc <- if(dependent.transformation=="None"){
        predict(object=model, newdata=xgb.DMatrix(as.matrix(data)))
    } else if(dependent.transformation=="Log"){
        exp(predict(object=model, newdata=xgb.DMatrix(as.matrix(data))))
    } else if(dependent.transformation=="e"){
        log(predict(object=model, newdata=xgb.DMatrix(as.matrix(data))))
    } else if(dependent.transformation=="Scale"){
        scaleDecode(values=predict(object=model, newdata=xgb.DMatrix(as.matrix(data))), y_min=y_min, y_max=y_max)
    }
    
    concentration <- if(dependent.transformation=="None"){
        predict.frame$Concentration
    } else if(dependent.transformation=="Log"){
        exp(predict.frame$Concentration)
    } else if(dependent.transformation=="e"){
        log(predict.frame$Concentration)
    } else if(dependent.transformation=="Scale"){
        scaleDecode(values=predict.frame$Concentration, y_min=y_min, y_max=y_max)
    }
    
    # Include Spectrum for proper data linkage (preserves standard identity)
    val.frame <- data.frame(
        Spectrum = predict.frame$Spectrum,
        Concentration = concentration,
        Intensity = as.vector(cal.est.conc.pred.luc),
        Prediction = as.vector(cal.est.conc.pred.luc),
        stringsAsFactors = FALSE
    )

    return(val.frame)
}

mclPred <- function(object, newdata, dependent.transformation, ymin=0, ymax=1, confidence=TRUE, finalModel=TRUE, y_min=0, y_max=1){
    if(confidence==FALSE){
        if(dependent.transformation=="None"){
            tryCatch(predict(object=object, newdata=newdata,
            na.action=na.pass), error=function(e) NA)
        } else if(dependent.transformation=="Log"){
            tryCatch(exp(predict(object=object, newdata=newdata,
            na.action=na.pass)), error=function(e) NA)
        } else if(dependent.transformation=="e"){
            tryCatch(log(predict(object=object, newdata=newdata,
            na.action=na.pass)), error=function(e) NA)
        } else if(dependent.transformation=="Scale"){
            tryCatch(scaleDecode(predict(object=object, newdata=newdata,
            na.action=na.pass), y_min=y_min, y_max=y_max), error=function(e) NA)
        }
    } else if(confidence==TRUE){
        if(finalModel==TRUE){
            if(dependent.transformation=="None"){
                tryCatch(predict(object=object$finalModel, newdata=newdata,
                na.action=na.pass, interval="confidence"), error=function(e) NA)
            } else if(dependent.transformation=="Log"){
                tryCatch(exp(predict(object=object$finalModel, newdata=newdata,
                na.action=na.pass, interval="confidence")), error=function(e) NA)
            } else if(dependent.transformation=="e"){
                tryCatch(log(predict(object=object$finalModel, newdata=newdata,
                na.action=na.pass, interval="confidence")), error=function(e) NA)
            } else if(dependent.transformation=="Scale"){
                tryCatch(scaleDecode(predict(object=object$finalModel, newdata=newdata,
                na.action=na.pass, interval="confidence"), y_min=y_min, y_max=y_max), error=function(e) NA)
            }
        } else if(finalModel==FALSE){
            if(dependent.transformation=="None"){
                tryCatch(predict(object=object, newdata=newdata,
                na.action=na.pass, interval="confidence"), error=function(e) NA)
            } else if(dependent.transformation=="Log"){
                tryCatch(exp(predict(object=object, newdata=newdata,
                na.action=na.pass, interval="confidence")), error=function(e) NA)
            } else if(dependent.transformation=="e"){
                tryCatch(log(predict(object=object, newdata=newdata,
                na.action=na.pass, interval="confidence")), error=function(e) NA)
            } else if(dependent.transformation=="Scale"){
                tryCatch(scaleDecode(predict(object=object, newdata=newdata,
                na.action=na.pass, interval="confidence"), y_min=y_min, y_max=y_max), error=function(e) NA)
            }
        }
    }
    
}

valFrameCheck <- function(val.frame){
    if("Include" %in% colnames(val.frame)){
        test <- remove.factors(val.frame)
        # Handle case where there are no element columns (only Include + Spectrum)
        if(ncol(test) > 2){
            test2 <- as.data.frame(lapply(test[, 3:ncol(test), drop=FALSE], as.numeric), stringsAsFactors=FALSE)
            new.frame <- data.frame(Include=test$Include, Spectrum=test$Spectrum, test2, stringsAsFactors=FALSE)
        } else {
            new.frame <- data.frame(Include=test$Include, Spectrum=test$Spectrum, stringsAsFactors=FALSE)
        }
    } else if(!"Include" %in% colnames(val.frame)){
        test <- remove.factors(val.frame)
        # Handle case where there are no element columns (only Spectrum)
        if(ncol(test) > 1){
            test2 <- as.data.frame(lapply(test[, -1, drop=FALSE], as.numeric), stringsAsFactors=FALSE)
            new.frame <- data.frame(Spectrum=test$Spectrum, test2, stringsAsFactors=FALSE)
        } else {
            new.frame <- data.frame(Spectrum=test$Spectrum, stringsAsFactors=FALSE)
        }
    }

    return(new.frame)
}

intensityFrameCheck <- function(intensity.table){
    if("Spectrum" %in% colnames(intensity.table)){
        test <- remove.factors(intensity.table)
        test2 <- as.data.frame(lapply(test[,-1], as.numeric), stringsAsFactors=FALSE)
        new.frame <- data.frame(Spectrum=test$Spectrum, test2, stringsAsFactors=FALSE)
    } else if(!"Spectrum" %in% colnames(intensity.table)){
        test <- remove.factors(intensity.table)
        test2 <- as.data.frame(lapply(test, as.numeric), stringsAsFactors=FALSE)
        new.frame <- data.frame(test2, stringsAsFactors=FALSE)
    }
    
    return(new.frame)
}

spectraCheck <- function(spectra){
    test <- remove.factors(spectra)
    test2 <- as.data.frame(lapply(test[,c("Energy", "CPS")], as.numeric), stringsAsFactors=FALSE)
    new.frame <- data.frame(Spectrum=test$Spectrum, test2, stringsAsFactors=FALSE)
    return(new.frame)
}

predictFrameCheck <- function(predict.frame){
    if("Spectrum" %in% colnames(predict.frame)){
        test <- remove.factors(predict.frame)
        test2 <- as.data.frame(lapply(test[,!colnames(test) %in% "Spectrum"], as.numeric), stringsAsFactors=FALSE)
        new.frame <- data.frame(Spectrum=test$Spectrum, test2, stringsAsFactors=FALSE)
    } else if(!"Spectrum" %in% colnames(predict.frame)){
        test <- remove.factors(predict.frame)
        test2 <- as.data.frame(lapply(test, as.numeric), stringsAsFactors=FALSE)
        new.frame <- data.frame(test2, stringsAsFactors=FALSE)
    } else if(length(colnames(predict.frame))==1){
        test <- remove.factors(predict.frame)
        test[,1] <- as.numeric(test[,1])
        new.frame <- test
    }
    
    return(new.frame)
}


calConvert <- function(calibration, null.strip=TRUE, temp=FALSE, extensions=FALSE){
    Calibration <- calibration
    
    
        tryCatch(if(Calibration$FileType=="Spectra"){Calibration$FileType <- "CSV"}, error=function(e) NULL)
        
        Calibration$Notes <- if(!is.null(Calibration[["Notes"]])){
            paste0(Calibration[["Notes"]], " Updated on ", Sys.time())
        } else if(is.null(Calibration[["Notes"]])){
            paste0("Updated on ", Sys.time())
        }
        
        
        if(extensions==TRUE){
            extensions <- c(".spx", ".PDZ", ".pdz", ".CSV", ".csv", ".spt", ".mca")
            Calibration[["Spectra"]]$Spectrum <- mgsub::mgsub(pattern=extensions, replacement=rep("", length(extensions)), string=as.character(Calibration[["Spectra"]]$Spectrum))
            Calibration[["Values"]]$Spectrum <- mgsub::mgsub(pattern=extensions, replacement=rep("", length(extensions)), string=as.character(Calibration[["Values"]]$Spectrum))
        }
        

        
        Calibration$Values <- valFrameCheck(Calibration$Values)
        Calibration$Intensities <- intensityFrameCheck(Calibration$Intensities)
        Calibration$Spectra <- spectraCheck(Calibration$Spectra)
        #Calibration$LinePreference <- if(is.null(Calibration$LinePreference)){
        #    "Narrow"
        #} else if(!is.null(Calibration$LinePreference)){
        #    Calibration$LinePreference
        #}
        
        if(null.strip==TRUE){
            null.list <- sapply(Calibration$calList, function(x) is.null(x[[2]]))
            tryCatch(for(i in names(Calibration$calList)){
                if(null.list[i]==TRUE){
                    Calibration$calList[[i]] <- NULL
                }
            }, error=function(e) NULL)

        }
        
        if(null.strip==TRUE){
            null.list <- sapply(Calibration$calList, function(x) is.null(x[[1]]$CalTable))
            tryCatch(for(i in names(Calibration$calList)){
                if(null.list[i]==TRUE){
                    Calibration$calList[[i]] <- NULL
                }
            }, error=function(e) NULL)

        }
        
        calpre <- pblapply(order_elements(names(Calibration[["calList"]])), function(x) tryCatch(calPre(element=x, element.model.list=Calibration[["calList"]][[x]], temp=temp), error=function(e) NULL))
        names(calpre) <- order_elements(names(Calibration[["calList"]]))
        
        Calibration$calList <- calpre
        
        if(is.null(Calibration$Definitions)){
            Calibration$Definitions <- data.frame(
            Name=as.vector(as.character(rep("", 75))),
            EnergyMin=as.numeric(rep("", 75)),
            EnergyMax=as.numeric(rep("", 75)),
            stringsAsFactors = FALSE
            )
        }
        
        return(Calibration)
}

modelPackPre <- function(parameters, model, table, compress=TRUE){
    
    if(parameters$CalTable$CalType==8 | parameters$CalTable$CalType==9){
        model.raw <-
        tryCatch(
            xgb.save.raw(
                tryCatch(
                    xgb.Booster.complete(model$finalModel)
                    , error=function(e) model$finalModel))
                , error=function(e) NULL)
    } else {
        model.raw <- NULL
    }
    
    model <- if(compress==TRUE){
        if(parameters$CalTable$CalType==1){
            strip(model, keep=c("predict", "summary"))
        } else if(parameters$CalTable$CalType==2){
            strip(model, keep=c("predict", "summary"))
        } else if(parameters$CalTable$CalType==3){
            strip(model, keep=c("predict", "summary"))
        } else if(parameters$CalTable$CalType==4){
            strip_glm(model)
        } else if(parameters$CalTable$CalType==5){
            strip_glm(model)
        } else if(parameters$CalTable$CalType==6){
            model
        } else if(parameters$CalTable$CalType==7){
            model
        } else if(parameters$CalTable$CalType==8){
            strip_glm(model)
        } else if(parameters$CalTable$CalType==9){
            strip_glm(model)
        } else if(parameters$CalTable$CalType==10){
            strip_glm(model)
        } else if(parameters$CalTable$CalType==11){
            strip_glm(model)
        } else if(parameters$CalTable$CalType==12){
            strip_glm(model)
        } else if(parameters$CalTable$CalType==13){
            strip_glm(model)
        }
    } else if(compress==FALSE){
        model
    }
    
    result.list <- if(parameters$CalTable$CalType!=8 | parameters$CalTable$CalType!=9){
        list(Parameters=parameters, Model=model, rawModel=model.raw, Table=table)
    } else if(parameters$CalTable$CalType==8 | parameters$CalTable$CalType==9){
        list(Parameters=parameters, Model=model, rawModel=model.raw, Table=table)
    }
    
    if(is.null(result.list$rawModel)){
        result.list$rawModel <- NULL
    }
    
    return(result.list)
    
}

modelPack <- function(parameters, model, table, compress=TRUE){
    modelPackPre(parameters=parameters, model=model, table=table, compress=compress)
}

calListCompress <- function(calList){
    calListNames <- names(calList)[as.vector(sapply(calList, function(x) !"Delete" %in% colnames(x[[1]]$CalTable)))]
    newcalList <- list()
    for(i in calListNames){
        newcalList[[i]] <- modelPack(parameters=calList[[i]][[1]], model=calList[[i]][[2]], table=calList[[i]][["Table"]], compress=TRUE)
    }
        return(newcalList)
}

calibrationElements <- function(calibration){
    variables <- colnames(calibration$Values)[as.vector(sapply(calibration$Values, is.numeric))]
    variables <- variables[as.vector(sapply(variables, function(x) length(unique(calibration$Values[,x]))>1))]
    return(variables)
}

defaultCalList <- function(calibration, temp=FALSE){
    variables <- calibrationElements(calibration)

    skipped <- character(0)
    candidates <- character(0)

    for(i in variables){
        if(i %in% colnames(calibration$Intensities)){
            if(i %in% names(calibration$calList)){
                calibration$calList[[i]] <- calibration$calList[[i]]
            } else if(!i %in% names(calibration$calList)){
                candidates <- c(candidates, i)
                cal.df <- merge(
                    calibration$Values[, c("Spectrum", i)],
                    calibration$Intensities[, c("Spectrum", i)],
                    by = "Spectrum", suffixes = c(".val", ".int")
                )
                val.col <- paste0(i, ".val")
                int.col <- paste0(i, ".int")
                cal.df <- cal.df[complete.cases(cal.df[, c(val.col, int.col)]), ]
                # Skip elements with too few overlapping standards to fit a line;
                # otherwise lm() aborts with "0 (non-NA) cases" and kills the whole load.
                if(nrow(cal.df) >= 2){
                    calibration$calList[[i]] <- list(Parameters=deleteCalConditions(element=i), Model=lm(cal.df[, val.col]~cal.df[, int.col]), na.action=na.omit)
                } else {
                    skipped <- c(skipped, i)
                }
            }
        }

    }

    if(length(skipped) > 0){
        if(length(skipped) == length(candidates)){
            warning("defaultCalList: every element was skipped (no Spectrum overlap between Values and Intensities had >=2 complete cases). Likely a Spectrum-key mismatch between the two tables, not a sparse calibration.")
        } else {
            print(paste0("defaultCalList: skipped ", length(skipped), "/", length(candidates), " elements with <2 co-measured standards: ", paste(skipped, collapse=", ")))
        }
    }

    if(temp==TRUE){
        for(i in variables){
            if(i %in% names(calibration$calList)){
                calibration$calList[[i]][[1]]$CalTable$Delete <- TRUE
            }
        }
    }

    
    return(calibration)
}

valCurve <- function(element, unit="%", loglinear="Linear", val.frame, rangesvalcurve, usestandards=NULL){
    element.name <- if(element %in% spectralLines){
        gsub("[.]", "", substr(element, 1, 2))
    } else {
        element
    }
    intens <- " Counts per Second"
    norma <- " Normalized"
    norma.comp <- " Compton Normalized"
    norma.tc <- " Valid Counts Normalized"
    conen <- paste0(" ", unit)
    predi <- paste0(" Estimate ", unit)
    log <- "Log "
    
    intensity.name <- c(element.name, intens)
    concentration.name <- c(element.name, conen)
    prediction.name <- c(element.name, predi)
    
    usestandards <- if(is.null(usestandards)){
        rep(TRUE, nrow(val.frame))
    } else if(!is.null(usestandards)){
        usestandards
    }
    
    if(unit=="ppm"){
        val.frame$Prediction <- val.frame$Prediction*10000
        val.frame$Concentration <- val.frame$Concentration*10000
    }
    
    valcurve.plot <- if(loglinear=="Linear"){
        #tryCatch(
        ggplot(data=val.frame[usestandards, , drop = FALSE], aes(Prediction, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame[usestandards, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        geom_abline(intercept=0, slope=1, lty=2) +
        stat_smooth(method="lm") +
        geom_point() +
        geom_point(aes(Prediction, Concentration),  data = val.frame[!usestandards, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
        scale_x_continuous(paste(element.name, predi), breaks=scales::pretty_breaks()) +
        scale_y_continuous(paste(element.name, conen), breaks=scales::pretty_breaks()) +
        coord_cartesian(xlim = rangesvalcurve$x, ylim = rangesvalcurve$y, expand = TRUE)
        #, error=function(e) NULL)
    } else if(loglinear=="Log"){
        #tryCatch(
        ggplot(data=val.frame[usestandards, , drop = FALSE], aes(Prediction, Concentration)) +
        theme_light() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction, val.frame[usestandards, , drop = FALSE])), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE)+
        geom_abline(intercept=0, slope=1, lty=2) +
        stat_smooth(method="lm") +
        geom_point() +
        geom_point(aes(Prediction, Concentration),  data = val.frame[!usestandards, , drop = FALSE], shape = 21, fill = "red", color = "black", alpha = 0.25) +
        scale_x_log10(paste("Log ", element.name, predi), breaks=scales::pretty_breaks()) +
        scale_y_log10(paste("Log ", element.name, conen), breaks=scales::pretty_breaks()) +
        coord_cartesian(xlim = rangesvalcurve$x, ylim = rangesvalcurve$y, expand = TRUE)
        #, error=function(e) NULL)
    }
    
    return(valcurve.plot)
}

is_lm <- function(element_model){
    if(element_model$Parameters$CalTable$CalType <= 3){
        TRUE
    } else {
        FALSE
    }
}

r2_gather <- function(calibration, lm.list, element){
    if(calibration[["calList"]][[element]][["Parameters"]][["CalTable"]][["CalType"]][[1]]==1 | calibration[["calList"]][[element]][["Parameters"]][["CalTable"]][["CalType"]][[element]]==2 | calibration[["calList"]][[element]][["Parameters"]][["CalTable"]][["CalType"]][[element]]==3){
        tryCatch(summary(lm.list[[element]])$r.squared, error=function(e) NULL)
    } else {
        tryCatch(calibration[["calList"]][[element]][["Model"]]$results[which.min(calibration[["calList"]][[element]][["Model"]]$results[, "Rsquared"]), ]$Rsquared, error=function(e) tryCatch(summary(lm.list[[element]])$r.squared, error=function(e) NULL), error=function(e) NULL)
    }
}

mse_calc <- function(res){
    RSS <- c(crossprod(res$residuals))
    MSE <- RSS / length(res$residuals)
    return(MSE)
}

rmse_calc <- function(res){
    MSE <- mse_calc(res)
    RMSE <- sqrt(MSE)
    return(RMSE)
}

rmse_gather <- function(calibration, lm.list, element){
    if(calibration[["calList"]][[element]][["Parameters"]][["CalTable"]][["CalType"]][[1]]==1 | calibration[["calList"]][[element]][["Parameters"]][["CalTable"]][["CalType"]][[1]]==2 | calibration[["calList"]][[element]][["Parameters"]][["CalTable"]][["CalType"]][[1]]==3){
        tryCatch(rmse_calc(lm.list[[element]]), error=function(e) NULL)
    } else {
        if("result" %in% names(calibration[["calList"]][[element]][["Model"]])){
            tryCatch(calibration[["calList"]][[element]][["Model"]]$results[which.min(calibration[["calList"]][[element]][["Model"]]$results[, "RMSE"]), ]$RMSE, error=function(e) tryCatch(rmse_calc(lm.list[[element]]), error=function(e) NULL), error=function(e) NULL)
        } else if(!"result" %in% names(calibration[["calList"]][[element]][["Model"]])){
            tryCatch(rmse_calc(lm.list[[element]]), error=function(e) NULL)
        }
    }
}

rmspe_gather <- function(calibration, element, predictions){
    tryCatch(MLmetrics::RMSPE(y_pred = predictions[complete.cases(calibration[["Values"]][element]),element], y_true = calibration[["Values"]][complete.cases(calibration[["Values"]][,element]),element]), error=function(e) NULL)
}

mae_gather <- function(calibration, lm.list, element, predictions){
        if(calibration[["calList"]][[element]][["Parameters"]][["CalTable"]][["CalType"]][[1]]==1 | calibration[["calList"]][[element]][["Parameters"]][["CalTable"]][["CalType"]][[1]]==2 | calibration[["calList"]][[element]][["Parameters"]][["CalTable"]][["CalType"]][[1]]==3){
            tryCatch(MLmetrics::MAE(y_pred = predictions[complete.cases(calibration[["Values"]][element]),element], y_true = calibration[["Values"]][complete.cases(calibration[["Values"]][,element]),element]), error=function(e) NULL)
    } else {
        tryCatch(calibration[["calList"]][[element]][["Model"]]$results[which.min(calibration[["calList"]][[element]][["Model"]]$results[, "MAE"]), ]$MAE, error=function(e) tryCatch(MLmetrics::MAE(y_pred = predictions[complete.cases(calibration[["Values"]][element]),element], y_true = calibration[["Values"]][complete.cases(calibration[["Values"]][,element]),element]), error=function(e) NULL), error=function(e) NULL)
    }
}

mape_gather <- function(calibration, lm.list, element, predictions){
       if(calibration[["calList"]][[element]][["Parameters"]][["CalTable"]][["CalType"]][[1]]==1 | calibration[["calList"]][[element]][["Parameters"]][["CalTable"]][["CalType"]][[1]]==2 | calibration[["calList"]][[element]][["Parameters"]][["CalTable"]][["CalType"]][[1]]==3){
            tryCatch(MLmetrics::MAPE(y_pred = predictions[complete.cases(calibration[["Values"]][element]),element], y_true = calibration[["Values"]][complete.cases(calibration[["Values"]][,element]),element]), error=function(e) NULL)
    } else {
        tryCatch(calibration[["calList"]][[element]][["Model"]]$results[which.min(calibration[["calList"]][[element]][["Model"]]$results[, "MAE"]), ]$MAPE, error=function(e) tryCatch(MLmetrics::MAPE(y_pred = predictions[complete.cases(calibration[["Values"]][element]),element], y_true = calibration[["Values"]][complete.cases(calibration[["Values"]][,element]),element]), error=function(e) NULL), error=function(e) NULL)
    }
}

lmSEapprox <- function(calibration, use_predictions=TRUE, predictions=NULL, parallel=FALSE, cores=2){
    elements <- names(calibration$calList)
    
    if(use_predictions==TRUE){
        
        lm_check <- sapply(calibration$calList, is_lm)
        lm_model_names <- names(lm_check[lm_check==TRUE])
        
        #if(parallel==FALSE){
        if(is.null(predictions)){
            predictions <-  cloudCalPredict(Calibration=calibration, elements.cal=names(calibration$calList), variables=names(calibration$Intensities)[!names(calibration$Intensities) %in% "Spectrum"], valdata=calibration$Spectra, rounding=10, multiplier=1, confidence=FALSE)
        }
        #} else if(parallel==TRUE){
            #prediction_list <- pblapply(lm_model_names, function(x) cloudCalPredict(Calibration=calibration, elements.cal=x, variables=names(calibration$Intensities)[!names(calibration$Intensities) %in% "Spectrum"], valdata=calibration$Spectra, rounding=10, multiplier=1, confidence=FALSE), cl=as.numeric(my.cores))
            #predictions <- Reduce(function(...) merge(..., by="Spectrum", all=F), prediction_list)
        #}
            
        if(parallel==FALSE){
            lm.list <- list()
            for(i in names(calibration$calList)){
                lm.list[[i]] <- lm(calibration[["Values"]][complete.cases(calibration[["Values"]][i]),i]~predictions[complete.cases(calibration[["Values"]][i]),i])
            }
        } else if(parallel==TRUE){
            lm.list <- pblapply(names(calibration$calList), function(i) lm(calibration[["Values"]][complete.cases(calibration[["Values"]][i]),i]~predictions[complete.cases(calibration[["Values"]][i]),i]), cl=as.numeric(cores))
            names(lm.list) <- names(calibration$calList)
        }
    }
    
       if(parallel==FALSE){
           rmse.list <- list()
           for(i in elements){
               rmse.list[[i]] <- rmse_gather(calibration=calibration, lm.list=lm.list, element=i)
           }
       } else if(parallel==TRUE){
           rmse.list <- pblapply(elements, function(i) rmse_gather(calibration=calibration, lm.list=lm.list, element=i), cl=as.numeric(cores))
           names(rmse.list) <- elements
       }
       
       if(parallel==FALSE){
           rmspe.list <- list()
           for(i in elements){
               rmspe.list[[i]] <- rmspe_gather(calibration=calibration, element=i)
           }
       } else if(parallel==TRUE){
           rmspe.list <- pblapply(elements, function(i) rmspe_gather(calibration=calibration, element=i), cl=as.numeric(cores))
           names(rmspe.list) <- elements
       }
       
       if(parallel==FALSE){
           mae.list <- list()
           for(i in elements){
               mae.list[[i]] <- mae_gather(calibration=calibration, lm.list=lm.list, element=i)
           }
       } else if(parallel==TRUE){
           mae.list <- pblapply(elements, function(i) mae_gather(calibration=calibration, lm.list=lm.list, element=i), cl=as.numeric(cores))
           names(mae.list) <- elements
       }
       
       if(parallel==FALSE){
           mape.list <- list()
           for(i in elements){
               mape.list[[i]] <- mape_gather(calibration=calibration, lm.list=lm.list, element=i)
           }
       } else if(parallel==TRUE){
           mape.list <- pblapply(elements, function(i) mape_gather(calibration=calibration, lm.list=lm.list, element=i), cl=as.numeric(cores))
           names(mape.list) <- elements
       }
          
       if(use_predictions==TRUE){
           return(list(Models=lm.list, Predictions=predictions, RMSE=rmse.list, RMSPE=rmspe.list, MAE=mae.list, MAPE=mape.list))
        } else if(use_predictions==FALSE){
            return(list(RMSE=rmse.list, RMSPE=rmspe.list, MAE=mae.list, MAPE=mape.list))
        }
}

fanoFactor <- function(data, energy.min=0.7, energy.max=0.9){
    data_window <- data[data$Energy > energy.min & data$Energy < energy.max,]
    data_window <- data.table::data.table(data_window)
    data_sd <- data_window[, list(CPS=sd(CPS, na.rm = TRUE)), by = list(Spectrum)]
    colnames(data_sd) <- c("Spectrum", "SD")
    data_mean <- data_window[, list(CPS=mean(CPS, na.rm = TRUE)), by = list(Spectrum)]
    colnames(data_mean) <- c("Spectrum", "Mean")
    
    data_aggregate <- merge(data_mean, data_sd, by="Spectrum")
    data_aggregate$Fano <- (data_aggregate$SD^2)/data_aggregate$Mean
    
    return(data_aggregate)
}

cloudCalPredictErrorEQM <- function(Calibration, predictions=NULL, elements.cal, elements, variables, valdata, deconvoluted_valdata=NULL, count.list=NULL, rounding=4, multiplier=1, energy.min=NULL, energy.max=NULL, se=FALSE){
    
    if(se==FALSE){
        se_val <- 1
    } else if(se==TRUE){
        se_val <- 1.96
    }
    
    energy.min <- if(is.null(energy.min)){
        0.7
    } else if(!is.null(energy.min)){
        energy.min
    }
    
    energy.max <- if(is.null(energy.max)){
        0.9
    } else if(!is.null(energy.max)){
        energy.max
    }
    
    error_list <- lmSEapprox(calibration=Calibration, use_predictions=TRUE, parallel=FALSE)
    
    if(is.null(predictions)){
        predictions <- cloudCalPredict(Calibration=Calibration, elements.cal=elements.cal, elements=elements, variables=variables, valdata=valdata, deconvoluted_valdata=deconvoluted_valdata, count.list=count.list, rounding=rounding, multiplier=multiplier)
    }
    data_fano <- fanoFactor(data=valdata, energy.min=energy.min, energy.max=energy.max)
    
    ####possibly 1.96*rmse
    prediction_list <- lapply(names(Calibration$calList), function(x) data.frame(Spectrum=predictions$Spectrum, Element=predictions[,x], Error=error_list$RMSE[[x]]))
    names(prediction_list) <- names(Calibration$calList)
    prediction_list_short <- list()
    for(i in names(prediction_list)){
        colnames(prediction_list[[i]]) <- c("Spectrum", i, paste0(i, " Error"))
        prediction_list[[i]][,paste0(i, " Error")] <- round((prediction_list[[i]][,paste0(i, " Error")]*data_fano$Fano) + (prediction_list[[i]][,paste0(i, " Error")]), rounding)*se_val
        prediction_list_short[[i]] <- prediction_list[[i]][,-1]
    }
    
    results <- Reduce(function(...) merge(..., by="Spectrum", all=F), prediction_list)
    #results <- prediction_list %>% reduce(left_join, by="Spectrum")
    #results <- join_all(prediction_list, by="Spectrum")
    #results <- prediction_list %>% reduce(inner_join, by="Spectrum")
    #results <- prediction_list %>% bind_cols
    #results <- data.frame(Spectrum=prediction_list[[1]]$Spectrum, as.data.frame(prediction_list_short))
    return(results)
}

y_hat_value <- function(prediction){
    sqrt(abs(prediction)*(1-abs(prediction)))
}

y_hat <- function(prediction.string){
    as.numeric(sapply(prediction.string, y_hat_value))
}

cloudCalPredictErrorYHat <- function(Calibration, predictions=NULL, elements.cal, elements, variables, valdata, deconvoluted_valdata=NULL, count.list=NULL, rounding=4, multiplier=1, energy.min=NULL, energy.max=NULL, se=FALSE){
    
    if(se==FALSE){
        se_val <- 1
    } else if(se==TRUE){
        se_val <- 1.96
    }
    
    energy.min <- if(is.null(energy.min)){
        0.7
    } else if(!is.null(energy.min)){
        energy.min
    }
    
    energy.max <- if(is.null(energy.max)){
        0.9
    } else if(!is.null(energy.max)){
        energy.max
    }
    
    error_list <- lmSEapprox(calibration=Calibration, use_predictions=TRUE, parallel=FALSE)
    
    if(is.null(predictions)){
        predictions <- cloudCalPredict(Calibration=Calibration, elements.cal=elements.cal, elements=elements, variables=variables, valdata=valdata, deconvoluted_valdata=deconvoluted_valdata, count.list=count.list, rounding=rounding, multiplier=multiplier)
    }
    yhat_est <- list()
    for(i in names(Calibration$calList)){
        yhat_est[[i]] <- y_hat(predictions[,i])
    }
    
    data_fano <- fanoFactor(data=valdata, energy.min=energy.min, energy.max=energy.max)
    
    ####possibly 1.96*rmse
    prediction_list <- lapply(names(Calibration$calList), function(x) data.frame(Spectrum=predictions$Spectrum, Element=predictions[,x], Error=yhat_est[[x]]))
    names(prediction_list) <- names(Calibration$calList)
    prediction_list_short <- list()
    for(i in names(prediction_list)){
        colnames(prediction_list[[i]]) <- c("Spectrum", i, paste0(i, " Error"))
        prediction_list[[i]][,paste0(i, " Error")] <- round(sqrt((prediction_list[[i]][,paste0(i, " Error")])^2*data_fano$Fano + (prediction_list[[i]][,paste0(i, " Error")])^2), rounding)*se_val
        prediction_list_short[[i]] <- prediction_list[[i]][,-1]
    }
    
    results <- Reduce(function(...) merge(..., by="Spectrum", all=F), prediction_list)
    #results <- prediction_list %>% reduce(left_join, by="Spectrum")
    #results <- join_all(prediction_list, by="Spectrum")
    #results <- prediction_list %>% reduce(inner_join, by="Spectrum")
    #results <- prediction_list %>% bind_cols
    #results <- data.frame(Spectrum=prediction_list[[1]]$Spectrum, as.data.frame(prediction_list_short))
    return(results)
}

#error_estimation <- function(element_model_list){
    #model <- element_model_list$Model
    #cal.type <- element_model_list$CalTable$CalType[1]
    
    #if(cal.type==1){
        
    #}
    
#}


###From https://stackoverflow.com/questions/58015605/getting-confidence-intervals-on-prediction-from-carettrain

caretTrainNewdata <- function(object, newdata, na.action = na.omit){
    if (!is.null(object$modelInfo$library))
        for (i in object$modelInfo$library) do.call("requireNamespaceQuietStop",
                                                    list(package = i))
    if (!is.null(newdata)) {
        if (inherits(object, "train.formula")) {
            newdata <- as.data.frame(newdata)
            rn <- row.names(newdata)
            Terms <- delete.response(object$terms)
            m <- model.frame(Terms, newdata, na.action = na.action,
                             xlev = object$xlevels)
            if (!is.null(cl <- attr(Terms, "dataClasses")))
                .checkMFClasses(cl, m)
            keep <- match(row.names(m), rn)
            newdata <- model.matrix(Terms, m, contrasts = object$contrasts)
            xint <- match("(Intercept)", colnames(newdata),
                          nomatch = 0)
            if (xint > 0)
                newdata <- newdata[, -xint, drop = FALSE]
        }
    }
    else if (object$control$method != "oob") {
        if (!is.null(object$trainingData)) {
            if (object$method == "pam") {
                newdata <- object$finalModel$xData
            }
            else {
                newdata <- object$trainingData
                newdata$.outcome <- NULL
                if ("train.formula" %in% class(object) &&
                    any(unlist(lapply(newdata, is.factor)))) {
                    newdata <- model.matrix(~., data = newdata)[,
                                                                -1]
                    newdata <- as.data.frame(newdata)
                }
            }
        }
        else stop("please specify data via newdata")
    } else
        stop("please specify data data via newdata")
    if ("xNames" %in% names(object$finalModel) & is.null(object$preProcess$method$pca) &
        is.null(object$preProcess$method$ica))
        newdata <- newdata[, colnames(newdata) %in% object$finalModel$xNames,
                           drop = FALSE]
    if(!is.null(object$preProcess))
       newdata <- predict(object$preProcess, newdata)
    if(!is.data.frame(newdata) &&
      !is.null(object$modelInfo$predict) &&
      any(grepl("as.data.frame", as.character(body(object$modelInfo$predict)))))
           newdata <- as.data.frame(newdata)
    newdata
}

background_error <- function(data, element.line, values=NULL, background, slope=NULL, intercept=NULL, norm.type=1, norm.min=9, norm.max=9.2, compress="100 eV", conversion=1){
    
    data <- just_spectra_summary_apply(spectra.frame=data, normalization=norm.type, min=norm.min, max=norm.max, compress=compress, deconvolution=NULL)

    
    element_symbol <- strsplit(x=element.line, split="\\.")[[1]][1]
    destination <- strsplit(x=element.line, split="\\.")[[1]][2]
    distance <- strsplit(x=element.line, split="\\.")[[1]][3]
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element_symbol)
    
    
    if(destination=="K" && distance=="alpha"){
        element_line_boundary <- c(elementLine[6][1,]-0.02, elementLine[5][1,]+0.02)
    } else if(destination=="K" && distance=="beta"){
        element_line_boundary <- c(elementLine[7][1,]-0.02, elementLine[8][1,]+0.02)
    } else if(destination=="L" && distance=="alpha"){
        element_line_boundary <- c(elementLine[11][1,]-0.02, elementLine[10][1,]+0.02)
    } else if (destination=="L" && distance=="beta"){
        element_line_boundary <- c(elementLine[12][1,]-0.02, elementLine[14][1,]+0.02)
    } else if (destination=="M" && distance=="line"){
        element_line_boundary <- c(elementLine[20][1,]-0.02, elementLine[22][1,]+0.02)
    }
    
    increment <- if(compress=="100 eV"){
        0.1
    } else if(compress=="50 eV"){
        0.05
    } else if(compress=="25 eV"){
        0.025
    }
    
    background_width <- length(seq(background[1], background[2], increment))
    element_width <- length(seq(element_line_boundary[1], element_line_boundary[2], increment))
    
    
    range.table <- data.frame(Name="Background", EnergyMin=background[1], EnergyMax=background[2])
    
    element_results <- elementGrab(element.line=element.line, data=data, range.table=range.table)
    colnames(element_results) <- make.names(colnames(element_results))
    element_results[,element.line] <- element_results[,element.line]
    
    background_results <- elementGrab(element.line="Background", data=data, range.table=range.table)
    background_results$Background <- background_results$Background*(element_width/background_width)
    
    #window_adjust <- (element_line_boundary[2]-element_line_boundary[1])/(background[2]-background[1])
    #background_results$Background <- background_results$Background*window_adjust
    
    merged_table <- merge(values[,c("Spectrum", element.line)], element_results, by="Spectrum")
    colnames(merged_table) <- c("Spectrum", "Concentration", "Intensity")
    
    slope_used <- if(!is.null(slope)){
        slope
    } else if(is.null(slope)){
        lm(Concentration~Intensity, data=merged_table)$coef[2]
    }
    
    intercept_used <- if(!is.null(intercept)){
        intercept
    } else if(is.null(intercept)){
        lm(Concentration~Intensity, data=merged_table)$coef[1]
    }
    
    
    lld <- (((2*sqrt(2)))/slope_used) * sqrt(mean(background_results$Background))
    
    ild <- (4.65/slope_used) * sqrt(mean(background_results$Background))
    
    mvr <- slope_used*(mean(background_results$Background)+sd(background_results$Background)*3) + intercept_used
    
    results_table <- data.frame(LLD=lld*conversion, ILD=ild*conversion, MVR=mvr*conversion)
    return(results_table)
}

ldm_calc <- function(prediction.vector, as_percent=FALSE){
    
    prediction.vector <- na.omit(prediction.vector)
    
    c_bar <- mean(prediction.vector)
    
    if(as_percent==FALSE){
        2 * sqrt((sum((prediction.vector-c_bar)^2))/(length(prediction.vector)-1))
    } else if(as_percent==TRUE){
        (2 * sqrt((sum((prediction.vector-c_bar)^2))/(length(prediction.vector)-1)))/c_bar
    }
    
}

ldm_sequence <- function(predictions, conversion=1, as_percent=FALSE){
    
    predictions$Sample <- sapply(predictions$Spectrum, function(x) strsplit(x, "_")[[1]][1])
    predictions$Sample <- sapply(predictions$Sample, function(x) strsplit(x, "-")[[1]][1])
    
    predictions_list <- list()
    for(i in unique(predictions$Sample)){
        predictions_list[[i]] <- predictions[predictions$Sample %in% i,]
        if(nrow(predictions_list[[i]]) <=1){
            predictions_list[[i]] <- NULL
        }
    }
    
    elements <- colnames(predictions)[!colnames(predictions) %in% c("Sample", "Spectrum", "X")]
    
    ldm_list <- list()
    for(i in names(predictions_list)){
        ldm_temp_list <- list()
        predictions_frame_temp <- predictions_list[[i]]
        for(x in elements){
            ldm_temp_list[[x]] <- if(as_percent==FALSE){
                ldm_calc(predictions_frame_temp[,x])*conversion
            } else if(as_percent==TRUE){
                ldm_calc(predictions_frame_temp[,x], as_percent=TRUE)
            }
        }
        ldm_list[[i]] <- data.frame(Sample=i, ldm_temp_list)
    }
    
    results <- as.data.frame(rbindlist(ldm_list), use_names=TRUE, fill=TRUE)
    
    return(results)
}

sd_sequence <- function(predictions, conversion=1){
    
    predictions$Sample <- sapply(predictions$Spectrum, function(x) strsplit(x, "_")[[1]][1])
    predictions$Sample <- sapply(predictions$Sample, function(x) strsplit(x, "-")[[1]][1])
    
    predictions_list <- list()
    for(i in unique(predictions$Sample)){
        predictions_list[[i]] <- predictions[predictions$Sample %in% i,]
        if(nrow(predictions_list[[i]]) <=1){
            predictions_list[[i]] <- NULL
        }
    }
    
    elements <- colnames(predictions)[!colnames(predictions) %in% c("Sample", "Spectrum", "X")]
    
    ldm_list <- list()
    for(i in names(predictions_list)){
        ldm_temp_list <- list()
        predictions_frame_temp <- predictions_list[[i]]
        for(x in elements){
            ldm_temp_list[[x]] <- sd(predictions_frame_temp[,x])*conversion
        }
        ldm_list[[i]] <- data.frame(Sample=i, ldm_temp_list)
    }
    
    results <- as.data.frame(rbindlist(ldm_list), use_names=TRUE, fill=TRUE)
    
    return(results)
}

tibble_convert <- function(spectra_frame){

    new_frame <- data.frame(energy_kev=spectra_frame$Energy, counts=spectra_frame$CPS, background=0, fit=0, cps=spectra_frame$CPS, baseline=0, smooth=0)
    new_tibble <- tibble::as_tibble(new_frame)
    new_tibble_list <- list(.path=unique(spectra_frame$Spectrum), .position = 1, .spectra=list(new_tibble))
    return(new_tibble_list)
}

spectra_frame_deconvolution_convert <- function(a_tibble){
    
    spectra_frame <- data.frame(Spectrum=a_tibble$.path, Energy=a_tibble$.deconvolution_response[[1]]$energy_kev, CPS=a_tibble$.deconvolution_response[[1]]$response_fit)
    return(spectra_frame)
}

spectra_frame_baseline_convert <- function(a_tibble){
    
    spectra_frame <- data.frame(Spectrum=a_tibble$.path, Energy=a_tibble$.spectra[[1]]$energy_kev, CPS=a_tibble$.spectra[[1]]$baseline)
    return(spectra_frame)
}

intensity_frame_deconvolution_convert <- function(deconvolution_tibble, name){
    
    deconvolution_frame <- as.data.frame(deconvolution_tibble)
    deconvolution_frame$order <- atomic_order_vector(deconvolution_frame$element)

    # Real elements: keep, ordered by atomic number.
    element_frame <- deconvolution_frame[!is.na(deconvolution_frame$order), , drop=FALSE]
    element_frame <- element_frame[order(element_frame$order),]

    # Non-element fit components. Keep the tube-scatter channels as named columns (Compton / Rayleigh) so
    # they are available as slope/intercept correction covariates in calibration -- alongside Baseline /
    # Total via otherSpectraStuff -- and drop the rest (e.g. escape peaks). Scatter sharpens the fit either
    # way (its intensity is no longer misattributed to elements); this just also exports the two areas.
    scatter_map <- c(scatter_compton = "Compton", scatter_rayleigh = "Rayleigh")
    scatter_frame <- deconvolution_frame[deconvolution_frame$element %in% names(scatter_map), , drop=FALSE]
    scatter_frame <- scatter_frame[order(match(scatter_frame$element, names(scatter_map))), , drop=FALSE]

    keep_names <- c(element_frame$element, unname(scatter_map[scatter_frame$element]))
    keep_areas <- c(element_frame$peak_area, scatter_frame$peak_area)

    deconvolution_t_frame <- t(keep_areas)
    result_frame <- data.frame(Spectrum=name, deconvolution_t_frame)
    colnames(result_frame) <- c("Spectrum", keep_names)
    return(result_frame)
}

# --- Deconvolution physics helpers (phase b2) ---------------------------------------------------
# Map an instrument mode to the optional xrftools deconvolution arguments (the `physics=` bundle for
# spectra_gls_deconvolute / deconvolute_complete). `kv` = tube voltage or accelerating voltage;
# `anode` = tube anode element (handheld / high-energy). "legacy" reproduces historical behaviour.
instrument_deconv_defaults <- function(mode="legacy", kv=NULL, anode=NULL, detector_type=NULL, active_thickness_um=NULL, filter=NULL, environment="air_pp"){
    mode <- match.arg(as.character(mode), c("legacy","handheld","sem","pixe","high_energy"))
    dt <- function(d) if(is.null(detector_type) || is.na(detector_type) || detector_type=="") d else detector_type
    base <- switch(mode,
        legacy      = list(),   # unconstrained OLS + jump-ratio intensities (no new physics)
        handheld    = list(excitation="photon",   detector_type=dt("SDD"),  tube_anode=anode, tube_kv=kv,
                           efficiency=TRUE, nonneg=TRUE, excitation_weighting="cross_section", coster_kronig=TRUE),
        sem         = list(excitation="electron", detector_type=dt("SDD"),  overvoltage_min=1.2,
                           efficiency=TRUE, nonneg=TRUE, excitation_weighting="cross_section", coster_kronig=TRUE),
        pixe        = list(excitation="photon",   detector_type=dt("SDD"),
                           efficiency=TRUE, nonneg=TRUE, excitation_weighting="cross_section", coster_kronig=TRUE),
        high_energy = list(excitation="photon",   detector_type=dt("HPGe"), tube_anode=anode, tube_kv=kv,
                           efficiency=TRUE, escape=TRUE, nonneg=TRUE,
                           excitation_weighting="cross_section", coster_kronig=TRUE))
    # Active-layer thickness for the efficiency model. The ubiquitous modern SDD is 450 um (the xrftools
    # SDD preset), so a 450 default on a silicon SDD is a no-op that simply makes the assumption explicit
    # in the persisted metadata. Apply the UI value to a true SDD/Si detector; for HPGe/CdTe/Si(Li) --
    # whose active layers are mm-scale -- keep the detector's own preset unless the user deliberately
    # typed a non-default (non-450) thickness. A blank field (NULL/NA) always defers to the preset.
    if(mode != "legacy" && !is.null(active_thickness_um) && !is.na(active_thickness_um)){
        eff_det <- toupper(if(is.null(base$detector_type)) "" else base$detector_type)
        if(eff_det %in% c("SDD","SI","") || active_thickness_um != 450){
            base$active_thickness_um <- active_thickness_um
        }
    }
    # Beam energy also only applies to non-legacy modes: legacy must stay an EMPTY bundle (apart from the
    # stripped .mode tag) so it reproduces the historical result byte-for-byte even if the always-visible
    # beam-energy field happens to hold a value from a mode the user was previously exploring.
    if(mode != "legacy" && !is.null(kv) && !is.na(kv)) base$beam_energy_kev <- kv
    # Primary-beam filter (xrftools "Sym um" string, e.g. the PDZ-inferred "Cu 100"). Non-legacy modes only.
    if(mode != "legacy" && !is.null(filter) && !is.na(filter) && nzchar(trimws(as.character(filter)))){
        base$tube_filter <- as.character(filter)
    }
    # Measurement environment (atmosphere + air path + polymer snout window) for the efficiency model.
    # Non-legacy modes only; defaults to the most common handheld setup (air + 4 um polypropylene).
    if(mode != "legacy"){
        env <- deconvolution_environment_config(environment)
        if(!is.null(env$atmosphere))  base$atmosphere  <- env$atmosphere
        if(!is.null(env$air_path_cm)) base$air_path_cm <- env$air_path_cm
        if(!is.null(env$window))      base$window      <- env$window
        base$.environment <- as.character(environment)   # UI-only tag to restore the selector
    }
    base$.mode <- mode   # UI-only metadata (stripped before the deconvolution call; used to restore the selector)
    base
}

# Extract the persisted physics bundle from a calibration's Deconvoluted$Parameters (old calibrations
# have no Physics field -> empty list -> historical behaviour reproduced).
deconvolution_physics_from_params <- function(params){
    if(is.list(params) && !is.null(params$Physics) && is.list(params$Physics)) params$Physics else list()
}

# Auto-inference (Phase 0): pull representative instrument settings out of an imported SpectraMetadata frame
# so the physics controls can be pre-seeded from the file. Returns NULLs for anything not present, so callers
# fall through to UI / preset defaults. Extended in later phases (geometry, air path, detector cal, ...).
deconvolution_infer_from_metadata <- function(md){
    out <- list(kv=NULL, filter=NULL, filter_stack=NULL, anode=NULL)
    if(is.null(md) || !is.data.frame(md) || nrow(md) == 0) return(out)
    num1 <- function(col){ if(!col %in% names(md)) return(NULL)
        v <- suppressWarnings(as.numeric(md[[col]])); v <- v[is.finite(v) & v > 0]
        if(length(v)) as.numeric(stats::median(v)) else NULL }
    chr1 <- function(col){ if(!col %in% names(md)) return(NULL)
        v <- unique(as.character(md[[col]])); v <- v[!is.na(v) & nzchar(trimws(v))]
        if(length(v) == 1) v else NULL }   # only infer when unambiguous across the imported set
    out$kv           <- num1("TubeVoltage")
    out$filter       <- chr1("TubeFilter")        # primary filter only
    out$filter_stack <- chr1("TubeFilterStack")   # full "Cu 100; Ti 25; Al 300" stack (modelled by xrf_tube)
    out$anode        <- chr1("TubeAnode")          # not carried by PDZ yet; harmless when absent
    out
}

# Special (non-element) deconvolution channels usable as calibration slope/intercept covariates: the SNIP
# Baseline area plus the tube-scatter Compton / Rayleigh peak areas (present only when a tube is modelled).
# Defensive: returns just the columns actually present, so old calibrations (Baseline only, or none) work.
deconvolution_extra_cols <- function(areas){
    if(is.null(areas) || !is.data.frame(areas) || !("Spectrum" %in% names(areas))) return(character(0))
    intersect(c("Baseline", "Compton", "Rayleigh"), names(areas))
}

# Fundamental-parameters mass estimate for a whole deconvolution batch. Observed areal mass per element =
# peak_area / FP sensitivity (matrix-decoupled, un-normalized: A_i/S_i). The sensitivity S_i depends only on
# the element set + excitation/detector physics -- NOT on the spectrum -- so it is computed ONCE via
# xrf_fp_sensitivity and the entire Areas table is divided by it (one FP call per batch, not one per
# spectrum). Returns a frame organised like $Areas (Spectrum + element columns), or NULL on any failure
# (e.g. an older xrftools without xrf_fp_sensitivity) so the deconvolution never fails just to build $Mass.
# Crustal abundance of the elements (ppm by mass, upper continental crust; ~0 for synthetic / short-lived
# nuclides). Used as a Bayesian prior in the full-FP phantom filter: a rarer element must clear a higher
# detection bar than a common one, and physically-impossible elements (Tc, Pm, Po, At, Rn, Ra, Ac, Fr, Pa,
# Np, Pu) get an essentially infinite bar. Standard crustal-abundance values (CRC / USGS).
.xrf_crustal_abundance_ppm <- c(
  O=461000, Si=282000, Al=82300, Fe=56300, Ca=41500, Na=23600, Mg=23300, K=20900, Ti=5650, H=1400,
  P=1050, Mn=950, F=585, Ba=425, Sr=370, S=350, C=200, Zr=165, Cl=145, V=120, Cr=102, Ni=84, Zn=70,
  Cu=60, Ce=66.5, Nd=41.5, La=39, Y=33, Co=25, Sc=22, Li=20, Nb=20, Ga=19, Pb=14, B=10, Th=9.6, Pr=9.2,
  Sm=7.05, Gd=6.2, Dy=5.2, Er=3.5, Yb=3.2, Hf=3, Cs=3, Be=2.8, Sn=2.3, U=2.7, Br=2.4, Ta=2, Eu=2,
  As=1.8, Ge=1.5, Ho=1.3, W=1.25, Mo=1.2, Tb=1.2, Tl=0.85, Lu=0.8, Tm=0.52, I=0.45, In=0.25, Sb=0.2,
  Cd=0.15, Hg=0.085, Ag=0.075, Se=0.05, Ar=1.2, Pd=0.015, Bi=0.009, Os=0.0015, Pt=0.005, Au=0.004,
  Te=0.001, Ru=0.001, Rh=0.001, Ir=0.001, Re=0.0007, Kr=1e-4, Xe=3e-5,
  Tc=1e-9, Pm=1e-9, Po=2e-10, At=1e-12, Rn=4e-13, Ra=9e-7, Ac=5e-10, Pa=1.4e-6, Fr=1e-18, Np=1e-12, Pu=1e-12)

# Full-FP phantom filter. The "fit everything" deconvolution invents dozens of spurious elements at
# baseline-noise level (and worse, at degenerate overlaps: Tb Lalpha+Lbeta land on Fe Kalpha+Kbeta, actinide
# L-lines land on the real Th/U/Pb/Rb lines) which poison the per-spectrum self-absorption matrix. This gates
# each candidate element, per spectrum, through four stacked Bayesian checks -- each catches what the previous
# cannot -- and returns which (spectrum, element) cells are real plus a per-cell area cap:
#   (1) LOD          -- net counts AND fitted-area both >= lod_sigma over the primary line's Poisson background.
#   (2) line-ratio   -- the element's supportable amplitude is bounded by its LEAST-supported *detectable* line
#                       (a = min over lines of (net + cushion)/p, p = rel_intensity x detector_efficiency, so
#                       filtered-out low-E lines don't falsely veto). A phantom can borrow a neighbour's strong
#                       line but not reproduce its own distinguishing lines -> capped near zero (Tb 370->5 sigma).
#   (3) attribution  -- the element's fitted share of its own window vs all competing fitted components,
#                       INCLUDING the Compton/Rayleigh scatter at the KNOWN tube-anode energy. Kills single-line
#                       parasites sitting under a dominant feature (Co under Fe Kbeta; Rh == the anode line).
#   (4) abundance    -- the (2) significance must clear a crustal-abundance-scaled bar: 3 + 1.5*log10(400/ppm),
#                       clamped [3,40]. Fe -> 3 sigma; Tb -> 6.8; Po -> 21. Removes the impossible elements.
# `cap` (<=1) scales a kept element's fitted area down to what its own line ratios actually support, so a
# barely-surviving degenerate (Tb) contributes bounded mass instead of poisoning the matrix at its stolen area.
deconvolution_fp_phantom_gate <- function(area_frame, keep, baseline_frame, spectra_raw, livetime,
                                          physics, beam, lod_sigma=3){
  det <- if(!is.null(physics$detector_type)) physics$detector_type else "SDD"
  sig_kev <- function(E){ s <- tryCatch(xrf_detector_sigma_kev(E, det), error=function(e) NA_real_)
      if(!is.finite(s) || s<=0) s <- 0.05 + 0.0025*E; s }
  eff_at <- function(E){ e <- tryCatch(xrf_detector_efficiency(E, det, active_thickness_um=physics$active_thickness_um,
        air_path_cm=physics$air_path_cm, atmosphere=if(!is.null(physics$atmosphere)) physics$atmosphere else "Air",
        window=physics$window), error=function(err) rep(1, length(E)))
      e[!is.finite(e) | e<0] <- 0; e }
  # per-element detectable line clusters (merge lines within 0.18 keV; keep those expected >= 15% of the primary)
  cl_of <- function(el){
    en <- tryCatch(xrf_energies(el, beam_energy_kev=beam), error=function(e) NULL)
    if(!is.data.frame(en) || !nrow(en)) return(NULL)
    en <- en[is.finite(en$energy_kev) & en$energy_kev>1.5 & en$energy_kev<(beam-1) & en$relative_peak_intensity>=0.02, , drop=FALSE]
    if(!nrow(en)) return(NULL)
    o <- order(en$energy_kev); E <- en$energy_kev[o]; r <- en$relative_peak_intensity[o]
    grp <- cumsum(c(1, diff(E) > 0.18)); Ec <- as.numeric(tapply(E*r, grp, sum)/tapply(r, grp, sum)); rc <- as.numeric(tapply(r, grp, sum))
    p <- rc * eff_at(Ec); o2 <- order(-p); Ec <- Ec[o2]; p <- p[o2]
    if(!length(p) || !is.finite(p[1]) || p[1]<=0) return(NULL)
    # keep lines expected >= 5% of the primary: the DISTINGUISHING lines (an element's weak minor lines a
    # phantom can't fake -- Tb Lbeta2/Lgamma) are the ones refutation needs, so the cutoff must stay low.
    p <- p/p[1]; ok <- p >= 0.05
    list(E=Ec[ok], p=p[ok], Eprim=Ec[1], sigprim=sig_kev(Ec[1]))
  }
  clusters <- lapply(keep, cl_of); names(clusters) <- keep
  valid <- vapply(clusters, Negate(is.null), logical(1))
  ab_ppm <- function(el){ v <- as.numeric(.xrf_crustal_abundance_ppm[el]); if(length(v)!=1 || is.na(v)) 1e-6 else max(v, 1e-18) }
  thr_ab <- setNames(vapply(keep, function(el) max(3, min(3 + 1.5*log10(400/ab_ppm(el)), 40)), numeric(1)), keep)
  abE <- vapply(keep, ab_ppm, numeric(1))
  compE <- vapply(keep, function(el) if(valid[[el]]) clusters[[el]]$Eprim   else NA_real_, numeric(1))
  compS <- vapply(keep, function(el) if(valid[[el]]) clusters[[el]]$sigprim else NA_real_, numeric(1))
  # known-anode scatter competitors: Rayleigh at the (intensity-weighted) anode line, Compton shifted by angle
  scat <- NULL
  if(!is.null(physics$tube_anode) && all(c("Compton","Rayleigh") %in% names(area_frame))){
    aen <- tryCatch(xrf_energies(physics$tube_anode, beam_energy_kev=beam), error=function(e) NULL)
    if(is.data.frame(aen) && nrow(aen)){
      aen <- aen[aen$relative_peak_intensity >= 0.1, , drop=FALSE]
      Eray <- sum(aen$energy_kev*aen$relative_peak_intensity)/sum(aen$relative_peak_intensity)
      th <- if(!is.null(physics$scatter_angle_deg)) physics$scatter_angle_deg else 135
      Ecom <- Eray/(1 + (Eray/511)*(1 - cos(th*pi/180)))
      br <- if(!is.null(physics$compton_broadening)) physics$compton_broadening else 2
      scat <- list(E=c(Eray, Ecom), S=c(sig_kev(Eray), sig_kev(Ecom)*br))
    }
  }
  lt_num <- suppressWarnings(as.numeric(livetime))
  lt_named <- if(!is.null(names(livetime))) setNames(lt_num, names(livetime)) else NULL
  lt_med <- suppressWarnings(stats::median(lt_num[is.finite(lt_num)]))
  lt_lookup <- function(nm){ v <- if(!is.null(lt_named) && nm %in% names(lt_named)) lt_named[[nm]] else NA_real_
      if(!is.finite(v)) v <- lt_med; v }
  bl_by  <- split(baseline_frame[, c("Energy","CPS")], as.character(baseline_frame$Spectrum))
  raw_by <- split(spectra_raw[, c("Energy","CPS")],    as.character(spectra_raw$Spectrum))
  specnames <- as.character(area_frame$Spectrum); ns <- nrow(area_frame); nk <- length(keep)
  pass <- matrix(FALSE, ns, nk, dimnames=list(NULL, keep)); cap <- matrix(1, ns, nk, dimnames=list(NULL, keep))
  winsum <- function(v, en, E, s){ sum(v[en>=E-1.5*s & en<=E+1.5*s], na.rm=TRUE) }
  for(i in seq_len(ns)){
    bl <- bl_by[[specnames[i]]]; rw <- raw_by[[specnames[i]]]
    if(is.null(bl) || !nrow(bl) || is.null(rw) || !nrow(rw)) next
    lt_i <- lt_lookup(specnames[i]); if(!is.finite(lt_i) || lt_i<=0) next
    dE <- suppressWarnings(stats::median(diff(sort(unique(bl$Energy))))); if(!is.finite(dE) || dE<=0) next
    ben <- bl$Energy; bcps <- bl$CPS; ren <- rw$Energy; rcps <- rw$CPS
    areas_i <- suppressWarnings(as.numeric(area_frame[i, keep]))
    sc_area <- if(!is.null(scat)) c(suppressWarnings(as.numeric(area_frame[i,"Rayleigh"])), suppressWarnings(as.numeric(area_frame[i,"Compton"]))) else NULL
    for(kk in seq_len(nk)){
      el <- keep[kk]; if(!valid[[el]]) next
      A <- areas_i[kk]; if(!is.finite(A) || A<=0) next
      cz <- clusters[[el]]; Ep <- cz$Eprim; sp <- cz$sigprim
      bgp <- winsum(bcps, ben, Ep, sp) * lt_i; if(!is.finite(bgp) || bgp<=0) next
      # (1) LOD: net and fitted-area both above the primary line's Poisson noise
      grossp <- winsum(rcps, ren, Ep, sp) * lt_i; a_prim <- grossp - bgp
      net_sig <- a_prim/sqrt(bgp); fit_sig <- (A*lt_i/dE)/sqrt(bgp)
      if(!(is.finite(net_sig) && is.finite(fit_sig) && net_sig>=lod_sigma && fit_sig>=lod_sigma)) next
      # (2) line-ratio refutation: if a strong detectable line predicts far more counts than observed, the
      #     element's pattern is refuted (it borrowed a neighbour's line but can't produce its own). A weak
      #     line lost in noise predicts little, so it cannot refute -- this protects real minor elements.
      #     a_sup tracks the amplitude the lines jointly support, for the mass cap.
      refuted <- FALSE; a_sup <- a_prim
      for(j in seq_along(cz$E)){
        s_j <- sig_kev(cz$E[j]); bg_j <- winsum(bcps, ben, cz$E[j], s_j)*lt_i
        net_j <- winsum(rcps, ren, cz$E[j], s_j)*lt_i - bg_j; pred_j <- a_prim*cz$p[j]
        if(cz$p[j] >= 0.05 && is.finite(pred_j) && pred_j > 0 &&
           (pred_j - net_j)/sqrt(bg_j + pred_j + 1) >= lod_sigma){ refuted <- TRUE; break }
        a_sup <- min(a_sup, (net_j + sqrt(max(bg_j,1)))/cz$p[j])
      }
      if(refuted) next
      # (4) abundance-scaled evidence bar: rarer element -> higher sigma required (impossibles need ~infinite)
      if(!(fit_sig >= thr_ab[[el]])) next
      # (3) attribution: fitted share of the primary window vs competitors incl. the KNOWN-anode scatter.
      #     Each element competitor's leak is downweighted by min(1, abundance_D/abundance_E) -- a rarer
      #     competitor makes a weaker claim on the window -- so the more abundant element wins an overlap.
      lo <- Ep-1.5*sp; hi <- Ep+1.5*sp
      own <- A*(pnorm(hi, Ep, sp) - pnorm(lo, Ep, sp)); leak <- 0
      othr <- which(is.finite(compE) & is.finite(compS)); othr <- othr[othr != kk]
      if(length(othr)) leak <- leak + sum(pmin(1, abE[othr]/abE[kk]) * areas_i[othr] *
          (pnorm(hi, compE[othr], compS[othr]) - pnorm(lo, compE[othr], compS[othr])), na.rm=TRUE)
      if(!is.null(scat) && !is.null(sc_area)) leak <- leak + sum(sc_area*(pnorm(hi, scat$E, scat$S) - pnorm(lo, scat$E, scat$S)), na.rm=TRUE)
      attrib <- own/(own + leak)
      if(!(is.finite(attrib) && attrib >= 0.5)) next
      pass[i,kk] <- TRUE
      fitted_counts <- A*lt_i/dE
      cap[i,kk] <- if(is.finite(a_sup) && fitted_counts>0) max(0, min(1, a_sup/fitted_counts)) else 1
    }
  }
  list(pass=pass, cap=cap)
}

# Build a per-spectrum LiveTime lookup (named numeric, seconds) from an imported metadata frame, for the
# full-FP $Mass count-space LOD. Names are cleaned to match dataHold()'s Spectrum ids (extension stripped).
# Returns NULL when no usable LiveTime column is present (the LOD then falls back to the col-max signal filter).
deconvolution_livetime_lookup <- function(md){
    if(!is.data.frame(md) || !("LiveTime" %in% names(md))) return(NULL)
    lt <- suppressWarnings(as.numeric(md$LiveTime))
    if(!any(is.finite(lt))) return(NULL)
    if("Spectrum" %in% names(md)){
        nm <- gsub("\\.(pdz|csv|CSV|spt|mca|spx|spe)$", "", as.character(md$Spectrum))
        if(length(nm) == length(lt)) return(setNames(lt, nm))
    }
    lt                                               # unnamed -> deconvolution_mass_frame uses the batch median
}

deconvolution_mass_frame <- function(area_frame, physics=list(), energy_max=NULL, fallback_energy=NULL,
                                     mass_min_sensitivity=1e-2, fidelity="relative", mass_full_min_signal=2e-3,
                                     baseline_frame=NULL, spectra_raw=NULL, livetime=NULL, lod_sigma=3){
    tryCatch({
        element_cols <- setdiff(names(area_frame), c("Spectrum", "Baseline", "Compton", "Rayleigh"))
        if(length(element_cols) == 0) return(NULL)
        pget <- function(k, default){ v <- physics[[k]]; if(!is.null(v) && !is.na(v[1])) v else default }
        beam <- pget("beam_energy_kev", if(!is.null(energy_max)) energy_max else fallback_energy)
        if(is.null(beam) || !is.finite(beam)) return(NULL)
        tube_obj <- tryCatch(
            if(!is.null(physics$tube_anode) && !is.null(physics$tube_kv)) xrf_tube(physics$tube_anode, kv=physics$tube_kv, filter=physics$tube_filter) else NULL,
            error=function(e) NULL)
        # Sensitivity uses the run's excitation/detector physics, defaulting to a full-physics estimate
        # (efficiency on, cross-section weighting) so the mass is meaningful even from a legacy fit.
        sens_args <- list(elements = element_cols, beam_energy_kev = beam,
                 detector_type = physics$detector_type,
                 be_window_um = physics$be_window_um, dead_layer_um = physics$dead_layer_um,
                 efficiency = pget("efficiency", TRUE),
                 excitation = pget("excitation", "photon"),
                 excitation_weighting = pget("excitation_weighting", "cross_section"),
                 coster_kronig = pget("coster_kronig", TRUE),
                 tube = tube_obj)
        # active_thickness_um only exists in newer xrftools; pass it only when the installed function accepts
        # it, so an older install still builds $Mass (just without the thickness-consistency refinement).
        if("active_thickness_um" %in% names(formals(xrf_fp_sensitivity)))
            sens_args$active_thickness_um <- physics$active_thickness_um
        # measurement environment (air path / atmosphere / snout window) -- newer xrftools only
        if("air_path_cm" %in% names(formals(xrf_fp_sensitivity))){
            sens_args$air_path_cm <- physics$air_path_cm
            sens_args$atmosphere  <- if(!is.null(physics$atmosphere)) physics$atmosphere else "Air"
            sens_args$window      <- physics$window
        }
        S <- do.call(xrf_fp_sensitivity, sens_args)
        sens <- setNames(S$sensitivity, S$element)[element_cols]           # NA for unexcited elements
        # Detectability floor. An element whose emission line the detector effectively cannot see (below the
        # window cutoff -- e.g. C at 0.28 keV or O at 0.53 keV on an 8um-Be SDD) or that is barely excited has
        # a near-zero FP sensitivity. Its fitted peak_area is then just baseline noise, and peak_area /
        # sensitivity amplifies that into absurd masses (obsidian carbon came out ~1e13). Drop any element
        # whose sensitivity is below `mass_min_sensitivity` x the strongest element's sensitivity -- these are
        # not measurable by this instrument, so a mass estimate for them is meaningless rather than merely noisy.
        smax <- suppressWarnings(max(sens, na.rm=TRUE))
        keep <- element_cols[is.finite(sens) & sens > 0 & is.finite(smax) & sens >= mass_min_sensitivity * smax]
        if(length(keep) == 0) return(NULL)

        if(identical(as.character(fidelity), "full")){
            # Full fidelity: per-spectrum fundamental-parameters solve (self-absorption + secondary/tertiary
            # fluorescence) via xrf_quantify's un-normalized observed_mass. Slower -- one FP solve per spectrum,
            # and the matrix couples elements -- but correct for heavy matrices where secondary fluorescence
            # (e.g. Cu enhancing Fe/Mn/Cr in a bronze) badly biases the relative A/S estimate.
            #
            qargs <- list(beam_energy_kev = beam, detector_type = physics$detector_type,
                be_window_um = physics$be_window_um, dead_layer_um = physics$dead_layer_um,
                efficiency = pget("efficiency", TRUE), excitation = pget("excitation", "photon"),
                excitation_weighting = pget("excitation_weighting", "cross_section"),
                coster_kronig = pget("coster_kronig", TRUE), tube = tube_obj,
                self_absorption = TRUE, secondary_fluorescence = TRUE, tertiary_fluorescence = TRUE)
            qf <- names(formals(xrf_quantify))
            if("active_thickness_um" %in% qf) qargs$active_thickness_um <- physics$active_thickness_um
            if("air_path_cm" %in% qf){ qargs$air_path_cm <- physics$air_path_cm
                qargs$atmosphere <- if(!is.null(physics$atmosphere)) physics$atmosphere else "Air"
                qargs$window <- physics$window }
            if("incidence_deg" %in% qf && !is.null(physics$incidence_deg)) qargs$incidence_deg <- physics$incidence_deg
            if("takeoff_deg"  %in% qf && !is.null(physics$takeoff_deg))  qargs$takeoff_deg  <- physics$takeoff_deg

            # Restrict the FP element set to real signal, per spectrum, via the four-gate Bayesian phantom
            # filter (LOD + line-ratio refutation + abundance-weighted attribution + crustal-abundance prior;
            # see deconvolution_fp_phantom_gate). The "fit everything" deconvolution invents dozens of spurious
            # elements that would otherwise poison the self-absorption matrix. `pass` keeps only elements
            # detected in a given spectrum; `cap` (<=1) scales a barely-surviving degenerate's stolen area down
            # to what its own line ratios support. Needs per-channel raw + baseline + LiveTime; when any is
            # missing (metadata-less import, baseline off, older caller) it falls back to the col-max filter.
            lt_num <- if(!is.null(livetime)) suppressWarnings(as.numeric(livetime)) else numeric(0)
            use_gate <- is.data.frame(baseline_frame) && all(c("Spectrum","Energy","CPS") %in% names(baseline_frame)) &&
                        is.data.frame(spectra_raw)  && all(c("Spectrum","Energy","CPS") %in% names(spectra_raw)) &&
                        any(is.finite(lt_num))
            pass_mat <- NULL; cap_mat <- NULL
            if(use_gate){
                gate <- tryCatch(deconvolution_fp_phantom_gate(area_frame, keep, baseline_frame, spectra_raw,
                                     livetime, physics, beam, lod_sigma), error=function(e) NULL)
                if(is.null(gate)) use_gate <- FALSE else {
                    pass_mat <- gate$pass; cap_mat <- gate$cap
                    ever <- colSums(pass_mat) > 0
                    keep <- keep[ever]; pass_mat <- pass_mat[, ever, drop=FALSE]; cap_mat <- cap_mat[, ever, drop=FALSE]
                    if(length(keep) == 0) return(NULL)
                }
            }
            if(!use_gate){
                col_max <- vapply(keep, function(el) suppressWarnings(max(as.numeric(area_frame[[el]]), na.rm=TRUE)), numeric(1))
                gmax <- suppressWarnings(max(col_max, na.rm=TRUE))
                keep <- keep[is.finite(col_max) & is.finite(gmax) & gmax > 0 & col_max >= mass_full_min_signal * gmax]
                if(length(keep) == 0) return(NULL)
            }
            rows <- lapply(seq_len(nrow(area_frame)), function(i){
                # per-spectrum element set: only lines detected in THIS spectrum enter the FP solve, so
                # phantom/absent lines cannot poison this spectrum's self-absorption matrix.
                els_i <- if(!is.null(pass_mat)) keep[pass_mat[i, ]] else keep
                if(length(els_i) == 0) return(setNames(rep(NA_real_, length(keep)), keep))
                ar <- as.numeric(area_frame[i, els_i])
                if(!is.null(cap_mat)) ar <- ar * as.numeric(cap_mat[i, els_i])   # cap degenerate stolen area
                pk <- data.frame(element = els_i, peak_area = ar, stringsAsFactors=FALSE)
                pk <- pk[is.finite(pk$peak_area) & pk$peak_area > 0, , drop=FALSE]
                if(!nrow(pk)) return(setNames(rep(NA_real_, length(keep)), keep))
                q <- tryCatch(do.call(xrf_quantify, c(list(object = pk), qargs)), error = function(e) NULL)
                if(is.null(q) || !("observed_mass" %in% names(q))) return(setNames(rep(NA_real_, length(keep)), keep))
                setNames(q$observed_mass[match(keep, q$element)], keep)   # NA for below-LOD / absent this spectrum
            })
            mass_mat <- do.call(rbind, rows)
        } else {
            # Relative (fast): the sensitivity is spectrum-independent, so divide the whole Areas table once.
            mass_mat <- sweep(as.matrix(area_frame[, keep, drop=FALSE]), 2, sens[keep], "/")
        }
        data.frame(Spectrum = area_frame$Spectrum, mass_mat, check.names=FALSE, stringsAsFactors=FALSE)
    }, error = function(e){ warning("Could not compute FP $Mass table: ", conditionMessage(e)); NULL })
}

deconvolute_complete <- function(spectra_frame, energy_max=NULL, width=5, alpha=2.5, default_sigma=0.07,
    smooth_iter=20, snip_iter=20, use_qr=TRUE,
    # --- estimator: backward-compat defaults reproduce the historical unconstrained OLS ---
    nonneg=FALSE, weighting="none",
    # --- excitation / line model: backward-compat defaults reproduce the historical jump-ratio model
    #     (identical for single-family K-only / L-only elements; the emission-probability omega fix is
    #     the only unrevertable change and cancels under per-element normalization there) ---
    beam_energy_kev=NULL, excitation="photon", overvoltage_min=1,
    excitation_weighting="jump", coster_kronig=FALSE,
    # --- detector resolution: overrides default_sigma with sigma(E) when detector_type is set ---
    detector_type=NULL, fano=NULL, epsilon_ev=NULL, noise_fwhm_ev=NULL,
    # --- detector response (all off by default) ---
    efficiency=FALSE, escape=FALSE, be_window_um=NULL, dead_layer_um=NULL, active_thickness_um=NULL,
    air_path_cm=NULL, atmosphere="Air", window=NULL,
    # --- scatter (needs a tube) ---
    tube_anode=NULL, tube_kv=NULL, tube_filter=NULL, scatter=NULL, scatter_angle_deg=135, compton_broadening=2,
    # --- line shape / engine ---
    tail=0, step=0, beta=NULL, refine_calibration=FALSE, sum_peaks=FALSE, pileup_tau=NULL,
    cache_templates=TRUE){
    if(is.null(energy_max)){
        energy_max <- max(spectra_frame$Energy)
    }
    # Physics excitation energy is decoupled from the fit window (energy_max). Defaults to energy_max.
    if(is.null(beam_energy_kev)){
        beam_energy_kev <- energy_max
    }
    # The Gaussian smoothing filter needs an ODD, integer number of taps, and the SNIP/smooth iteration
    # counts must be whole numbers. A caller optimizing these over a continuous range (e.g. width = 20.327)
    # would otherwise build an even-length filter that crashes the smoother. Snap them to valid values.
    if(is.numeric(width) && length(width) == 1 && is.finite(width)){
        width <- max(1, 2 * round((width - 1) / 2) + 1)   # nearest odd integer >= 1
    }
    if(is.numeric(smooth_iter) && is.finite(smooth_iter)) smooth_iter <- max(0L, as.integer(round(smooth_iter)))
    if(is.numeric(snip_iter)   && is.finite(snip_iter))   snip_iter   <- max(0L, as.integer(round(snip_iter)))
    if(is.data.frame(spectra_frame)){
        spectrum_name <- unique(spectra_frame$Spectrum)
        spectra_tibble <- tibble_convert(spectra_frame)

        # Peak template list. Defensive: fall back to the minimal xrf_energies() on older xrftools.
        peaks <- tryCatch(
            xrf_energies("everything", beam_energy_kev=beam_energy_kev, excitation=excitation,
                overvoltage_min=overvoltage_min, excitation_weighting=excitation_weighting,
                coster_kronig=coster_kronig),
            error=function(e) xrf_energies("everything", beam_energy_kev=beam_energy_kev))

        # Tube / geometry for optional Rayleigh-Compton scatter templates. A primary-beam filter (e.g. the
        # PDZ-inferred "Cu 100") hardens the excitation spectrum; passed through to xrf_tube when present.
        tube <- tryCatch(
            if(!is.null(tube_anode) && !is.null(tube_kv)) xrf_tube(tube_anode, kv=tube_kv, filter=tube_filter) else NULL,
            error=function(e) NULL)
        geometry <- tryCatch(xrf_geometry(scatter_angle_deg=scatter_angle_deg), error=function(e) NULL)

        # Apply smoothing and baseline
        smoothed_tibble <- spectra_tibble %>%
            xrf_add_smooth_filter(filter = xrf_filter_gaussian(width = width, alpha = alpha), .iter = smooth_iter) %>%
            xrf_add_baseline_snip(.values = .spectra$smooth, iterations = snip_iter)

        # Full (new-xrftools) call; fall back to the historical minimal signature on any older
        # xrftools that lacks the added arguments ("unused argument").
        deconvoluted_spectra_tibble <- tryCatch({
            smoothed_tibble %>%
                xrf_add_deconvolution_gls(.spectra$energy_kev, .spectra$smooth - .spectra$baseline,
                    energy_max_kev = energy_max, peaks = peaks, default_sigma = default_sigma,
                    detector_type = detector_type, fano = fano, epsilon_ev = epsilon_ev,
                    noise_fwhm_ev = noise_fwhm_ev, nonneg = nonneg, weighting = weighting,
                    efficiency = efficiency, escape = escape, be_window_um = be_window_um,
                    dead_layer_um = dead_layer_um, active_thickness_um = active_thickness_um,
                    air_path_cm = air_path_cm, atmosphere = atmosphere, window = window,
                    tube = tube, geometry = geometry, scatter = scatter,
                    compton_broadening = compton_broadening, tail = tail, step = step, beta = beta,
                    refine_calibration = refine_calibration, sum_peaks = sum_peaks,
                    pileup_tau = pileup_tau, cache_templates = cache_templates, use_qr = use_qr)
        }, error = function(e) {
            if (grepl("unused argument", e$message)) {
                # Older xrftools: original behaviour (unconstrained OLS, jump-ratio + double-omega).
                smoothed_tibble %>%
                    xrf_add_deconvolution_gls(.spectra$energy_kev, .spectra$smooth - .spectra$baseline,
                        energy_max_kev = energy_max,
                        peaks = xrf_energies("everything", beam_energy_kev=beam_energy_kev),
                        default_sigma = default_sigma)
            } else {
                stop(e)
            }
        })

        baseline_spectra <- spectra_frame_baseline_convert(deconvoluted_spectra_tibble)
        deconvoluted_spectra <- spectra_frame_deconvolution_convert(deconvoluted_spectra_tibble)
        deconvoluted_peaks <- intensity_frame_deconvolution_convert(deconvoluted_spectra_tibble$.deconvolution_peaks[[1]], name=spectrum_name)
        return(list(Spectra=deconvoluted_spectra, Areas=deconvoluted_peaks, Baseline=baseline_spectra))
    } else if(!is.data.frame(spectra_frame)){
        NULL
    }

}

spectra_gls_deconvolute <- function(spectra_frame, baseline=TRUE, energy_max=NULL, width=5, alpha=2.5, default_sigma=0.07, smooth_iter=20, snip_iter=20, cores=1, physics=list(), mass=FALSE, livetime=NULL){
    spectra_frame$Spectrum <- as.character(spectra_frame$Spectrum)
    spectra_frame$Energy <- as.numeric(spectra_frame$Energy)
    spectra_frame$CPS <- as.numeric(spectra_frame$CPS)
    spectra_frame <- spectra_frame[complete.cases(spectra_frame),]

    cores <- as.integer(cores)
    # Use fast QR for single-core, fork-safe lm() for multicore
    use_qr <- (cores == 1)

    # `physics` bundles the optional new xrftools deconvolution arguments (detector_type, excitation,
    # nonneg, tube_anode/tube_kv, efficiency, escape, tail, ...). Empty list = historical behaviour.
    if(is.null(physics)) physics <- list()
    # dot-prefixed keys (e.g. .mode) are UI-only metadata: keep them in the persisted Parameters for
    # restoring the UI, but strip them from the arguments passed to deconvolute_complete.
    # NB: names() is NULL for an empty/unnamed physics (the default list()), and startsWith(NULL, ".")
    # errors ("non-character object(s)") -- guard it, since an unnamed bundle has no dot-keys to strip.
    physics_names <- names(physics)
    physics_call <- if (is.null(physics_names)) physics else physics[!startsWith(physics_names, ".")]

    spectra_list <- split(spectra_frame, spectra_frame$Spectrum)

    safe_deconvolute <- function(x){
        tryCatch(
            do.call(deconvolute_complete, c(list(
                spectra_frame=x,
                energy_max=energy_max,
                width=width,
                alpha=alpha,
                default_sigma=default_sigma,
                smooth_iter=smooth_iter,
                snip_iter=snip_iter,
                use_qr=use_qr), physics_call)),
            error = function(e){
                warning("Skipping spectrum '", unique(x$Spectrum), "': ", e$message)
                NULL
            }
        )
    }

    if(cores==1){
        new_spectra_list <- pblapply(spectra_list, safe_deconvolute)
    } else if(cores >= 2){
        new_spectra_list <- pbmclapply(spectra_list, safe_deconvolute, mc.cores = cores)
    }

    # Remove failed (NULL) entries
    failed <- sapply(new_spectra_list, is.null)
    if(any(failed)){
        warning(sum(failed), " of ", length(new_spectra_list), " spectra failed deconvolution and were skipped: ",
                paste(names(new_spectra_list)[failed], collapse=", "))
    }
    new_spectra_list <- new_spectra_list[!failed]

    if(length(new_spectra_list) == 0){
        stop("No spectra could be deconvoluted. Check that Spectra data frame has valid numeric Energy and CPS columns.")
    }

    only_spectra_list <- list()
    only_areas_list <- list()
    only_background_list <- list()
    for(i in seq_along(new_spectra_list)){
        only_spectra_list[[i]] <- new_spectra_list[[i]]$Spectra
        only_areas_list[[i]] <- new_spectra_list[[i]]$Areas
        if(baseline==TRUE){only_background_list[[i]] <- new_spectra_list[[i]]$Baseline}
    }
    new_spectra_frame <- as.data.frame(rbindlist(only_spectra_list))
    new_area_frame <- as.data.frame(rbindlist(only_areas_list))
    new_baseline_frame <- NULL                       # per-channel SNIP baseline (Spectrum/Energy/CPS); feeds the LOD filter
    if(baseline==TRUE){
        new_baseline_frame <- as.data.frame(rbindlist(only_background_list))
        new_area_frame$Baseline <- aggregate(CPS ~ Spectrum, data = new_baseline_frame[,c("Spectrum", "CPS")], FUN = sum)$CPS
    }
    # FP mass estimate ($Mass). `mass` selects fidelity: FALSE/"off" = none; TRUE/"relative" = fast A/S
    # (sensitivity once per batch); "full" = per-spectrum fundamental parameters (self-absorption +
    # secondary/tertiary fluorescence), correct for heavy matrices but slower. Organised like $Areas.
    mass_mode <- if(isTRUE(mass)) "relative" else if(is.character(mass) && length(mass)==1) tolower(mass) else "off"
    new_mass_frame <- if(mass_mode %in% c("relative","full")){
        deconvolution_mass_frame(new_area_frame, physics=physics, energy_max=energy_max,
            fallback_energy=suppressWarnings(max(as.numeric(spectra_frame$Energy), na.rm=TRUE)),
            fidelity=mass_mode, baseline_frame=new_baseline_frame, spectra_raw=spectra_frame, livetime=livetime)
    } else NULL

    if(baseline==FALSE){
        return(list(Spectra=new_spectra_frame, Areas=new_area_frame, Mass=new_mass_frame))
    } else if(baseline==TRUE){
        return(list(Spectra=new_spectra_frame, Areas=new_area_frame, Mass=new_mass_frame, Baseline=new_baseline_frame, Parameters=list(SmoothWidth=width, SmoothAlpha=alpha, DefaultSigma=default_sigma, SmoothIter=smooth_iter, SnipIter=snip_iter, ParamVersion=2, Physics=physics, MassFidelity=mass_mode)))
    }
    
}

deconvolutionIntensityFrame <- function(deconvolution_areas, intensity_frame){
    
    k_alpha <- deconvolution_areas
    colnames(k_alpha) <- paste0(colnames(k_alpha), ".K.alpha")
    colnames(k_alpha)[1] <- gsub(".K.alpha", "", colnames(k_alpha)[1])
    k_beta <- deconvolution_areas
    colnames(k_beta) <- paste0(colnames(k_beta), ".K.beta")
    colnames(k_beta)[1] <- gsub(".K.beta", "", colnames(k_beta)[1])
    l_alpha <- deconvolution_areas
    colnames(l_alpha) <- paste0(colnames(l_alpha), ".L.alpha")
    colnames(l_alpha)[1] <- gsub(".L.alpha", "", colnames(l_alpha)[1])
    l_beta <- deconvolution_areas
    colnames(l_beta) <- paste0(colnames(l_beta), ".L.beta")
    colnames(l_beta)[1] <- gsub(".L.beta", "", colnames(l_beta)[1])
    m_lines <- deconvolution_areas
    colnames(m_lines) <- paste0(colnames(m_lines), ".M.line")
    colnames(m_lines)[1] <- gsub(".M.line", "", colnames(m_lines)[1])
    
    all_intensities <- Reduce(function(...) merge(..., all=T, by="Spectrum"), list(k_alpha, k_beta, l_alpha, l_beta, m_lines))
    elements <- colnames(intensity_frame)[!colnames(intensity_frame) %in% "Spectrum"]
    not.elements <- elements[!elements %in% spectralLines]
    elements <- elements[elements %in% spectralLines]
    
    reduced_intensities <- all_intensities[,c("Spectrum", elements)]
    
    intensity_frame_reduced <- intensity_frame[, !colnames(intensity_frame) %in% elements]
    if(is.data.frame(intensity_frame_reduced)==FALSE){
        intensity_frame_reduced = data.frame(Spectrum = intensity_frame_reduced)
    }
    
    deconvoluted_intensities <- merge(reduced_intensities, intensity_frame_reduced, by="Spectrum")
    
    return(deconvoluted_intensities)
}

totalCountsGen <- function(spectra_frame){
    spectra_summary <- aggregate(CPS ~ Spectrum, data = spectra_frame[,c("Spectrum", "CPS")], FUN = sum)
    colnames(spectra_summary) <- c("Spectrum", "Total")
    return(spectra_summary)
}

roundNumericColumns <- function(df, digits=1, multiplier=1) {
  # Apply the round function to each column of the dataframe
  df[] <- lapply(df, function(x) {
    if (is.numeric(x)) {
      return(round(x*multiplier, digits = digits))
    } else {
      return(x)
    }
  })
  
  # Return the modified dataframe
  return(df)
}

simpleValPlot <- function(cal_table, unit="%", element, scale="Linear"){
    
    if(unit!="%"){
        cal_table$Prediction <- cal_table$Prediction*10000
        cal_table$Concentration <- cal_table$Concentration*10000
    }
    
   plot <-  if(scale=="Linear"){
       ggplot(cal_table, aes(Prediction, Concentration)) +
        geom_point() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction,  cal_table)), x=-Inf, y=Inf, hjust=0, vjust=1, parse=TRUE) +
        scale_x_continuous(paste0("XRF Estimate (", unit, ")"),  labels=scales::comma) +
        scale_y_continuous(paste0("Given Value (", unit, ")"),  labels=scales::comma) +
        geom_abline(aes(intercept=0, slope=1), lty=2) +
        stat_smooth(method="lm") +
        #geom_label_repel(aes(label = Standard), size = 3) +
        #facet_wrap(.~Set) +
        ggtitle(element) +
        theme_light()
   } else if(scale=="Log"){
       ggplot(cal_table, aes(Prediction, Concentration)) +
        geom_point() +
        annotate("text", label=lm_eqn_val(lm(Concentration~Prediction,  cal_table)), x=-Inf, y=Inf, hjust=0, vjust=1, parse=TRUE) +
        scale_x_log10(paste0("XRF Estimate (", unit, ")"),  labels=scales::comma) +
        scale_y_log10(paste0("Given Value (", unit, ")"),  labels=scales::comma) +
        geom_abline(aes(intercept=0, slope=1), lty=2) +
        stat_smooth(method="lm") +
        #geom_label_repel(aes(label = Standard), size = 3) +
        #facet_wrap(.~Set) +
        ggtitle(element) +
        theme_light()
   }
    
    return(plot)
}

spectra_select <- function(spectra, pattern="name"){
    sub_spectra <- spectra[!grepl(pattern, spectra$Spectrum),]
    return(sub_spectra)
}

spectra_stack <- function(spectra, rep_delim="_"){
    spectra_names <- unique(spectra$Spectrum)
    channel_length <- nrow(spectra[spectra$Spectrum == spectra_names[1],])
    spectra$Channel <- seq(1, channel_length, 1)
    spectra_reps <- as.numeric(sapply(spectra_names, function(x) strsplit(x=x, split=rep_delim)[[1]][length(strsplit(x=x, split=rep_delim)[[1]])]))
    for(i in unique(spectra_reps)){
        spectra$Spectrum <- gsub(paste0(rep_delim, i), "", spectra$Spectrum)
    }
    spectra_list <- split(spectra, f=spectra$Spectrum)
    aggregate_spectra_list <- list()
    for(i in names(spectra_list)){
        spectra_list[[i]] <- as.data.table(spectra_list[[i]][!colnames(spectra_list[[i]]) %in% "Spectrum"])
        aggregate_spectra_list[[i]] <- as.data.frame(spectra_list[[i]][, lapply(.SD, mean, na.rm=TRUE), by="Channel" ])
        aggregate_spectra_list[[i]] <- data.frame(Spectrum=i, Energy=aggregate_spectra_list[[i]]$Energy, CPS=aggregate_spectra_list[[i]]$CPS)
    }
    aggregate_spectra <- as.data.frame(data.table::rbindlist(aggregate_spectra_list))
    return(aggregate_spectra)
}

values_stack <- function(values, rep_delim="_"){
    value_names <- unique(values$Spectrum)
    value_reps <- as.numeric(sapply(value_names, function(x) strsplit(x=x, split=rep_delim)[[1]][length(strsplit(x=x, split=rep_delim)[[1]])]))
    for(i in unique(value_reps)){
        values$Spectrum <- gsub(paste0(rep_delim, i), "", values$Spectrum)
    }
    values_frame <- data.frame(Include=TRUE, as.data.table(values[,-1])[, lapply(.SD, mean, na.rm=TRUE), by="Spectrum"])
    return(values_frame)
}

calibration_stack <- function(calibration, rep_delim="_"){
    calibration$Spectra <- spectra_stack(calibration$Spectra, rep_delim=rep_delim)
    calibration$Values <- values_stack(calibration$Values, rep_delim=rep_delim)
    calibration$OtherSpectraStuff <- NULL
    calibration$Deconvoluted <- NULL
    new_calibration <- calRDS(Calibration=calibration, rebuild=TRUE, sort=TRUE)
    return(new_calibration)
}

cloudcal <- "Loaded"

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
#options(repos = BiocInstaller::biocinstallRepos())
#getOption("repos")
#options(download.file.method="libcurl", url.method="libcurl")
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
list.of.bioconductor <- c("graph", "RBGL", "Rgraphviz")
new.bioconductor <- list.of.bioconductor[!(list.of.bioconductor %in% installed.packages()[,"Package"])]
#if(length(new.bioconductor)) source("https://www.bioconductor.org/biocLite.R")
if(length(new.bioconductor)) BiocManager::install(new.bioconductor)



list.of.packages <- c("backports", "mgsub", "pbapply", "reshape2", "TTR", "dplyr", "ggtern",  "shiny", "rhandsontable", "random", "DT", "shinythemes", "broom", "shinyjs", "gridExtra", "dtplyr", "formattable", "XML", "corrplot", "scales", "rmarkdown", "markdown",  "httpuv", "stringi", "reticulate", "devtools", "randomForest", "caret", "data.table", "mvtnorm", "DescTools",  "doSNOW", "doParallel", "baseline",  "pls", "prospectr", "stringi", "ggplot2", "compiler", "itertools", "foreach", "grid", "nnet", "neuralnet", "xgboost", "reshape", "magrittr", "reactlog", "Metrics", "strip", "bartMachine", "arm", "brnn", "kernlab", "rBayesianOptimization", "magrittr", "smooth", "smoother", "ggrepel", "tibble", "purrr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(get_os()!="linux"){
    if(length(new.packages)) lapply(new.packages, function(x) install.packages(x, repos="http://cran.rstudio.com/", dep = TRUE, ask=FALSE, type="binary"))
} else if(get_os()=="linux"){
    if(length(new.packages)) lapply(new.packages, function(x) install.packages(x, repos="http://cran.rstudio.com/", dep = TRUE, ask=FALSE, type="source"))
}

#if(!"caret" %in% installed.packages()[,"Package"]){
#    if(get_os()=="windows"){
#        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/caret_6.0-93.1.zip?raw=true", repos=NULL, type="win.binary"), error=function(e) tryCatch(remotes::install_github("leedrake5/caret", subdir="pkg/caret"), error=function(e) NULL))
#        } else if(get_os()!="windows"){
#            tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/caret_6.0-93.1.tar.gz?raw=true", type="source", repos=NULL), error=function(e) NULL)
#        }
#} else {
#    if(packageVersion("caret")!="6.0.93.1"){
#        if(get_os()!="windows"){
#        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/caret_6.0-93.1.zip?raw=true", repos=NULL, type="win.binary"), error=function(e) NULL)
#        } else if(get_os()!="windows"){
#            tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/caret_6.0-93.1.tar.gz?raw=true", type="source", repos=NULL), error=function(e) NULL)
#        }
#    }
#    }


#if(!"xrftools" %in% installed.packages()[,"Package"]){
#    tryCatch(devtools::install_github("paleolimbot/xrftools"), error=function(e) NULL)
#}



#if(packageVersion("ggplot2")!="2.2.1") devtools::install_version("ggplot2", version = "2.2.1", repos = "http://cran.us.r-project.org", checkBuilt=TRUE)

if("caret" %in% installed.packages()[,"Package"]==FALSE && get_os()=="windows"){
        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/caret_6.0-93.1_x86_64_win.zip?raw=true", repos=NULL, type="win.binary"), error=function(e) tryCatch(remotes::install_github("leedrake5/caret", subdir="pkg/caret"), error=function(e) NULL))
    } else if ("caret" %in% installed.packages()[,"Package"]==FALSE && get_os()=="osx"){
        if(Sys.info()[["machine"]]=="arm64"){
            tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/caret_6.0-93.1_arm64_macos.tgz?raw=true", type="binary", repos=NULL), error=function(e) tryCatch(remotes::install_github("leedrake5/caret", subdir="pkg/caret"), error=function(e) NULL))
        } else {
            tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/caret_6.0-93.1_x86_64_macos.tgz?raw=true", type="binary", repos=NULL), error=function(e)  tryCatch(remotes::install_github("leedrake5/caret", subdir="pkg/caret"), error=function(e) NULL))
            }
    } else if ("caret" %in% installed.packages()[,"Package"]==FALSE && get_os()=="linux"){
        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/caret_6.0-93.1.tar.gz?raw=true", type="source", repos=NULL), error=function(e) tryCatch(remotes::install_github("leedrake5/caret", subdir="pkg/caret"), error=function(e) NULL))
    }

if(packageVersion("caret")!="6.0.93.1" && get_os()=="windows"){
        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/caret_6.0-93.1_x86_64_win.zip?raw=true", repos=NULL, type="win.binary"), error=function(e) tryCatch(remotes::install_github("leedrake5/caret", subdir="pkg/caret"), error=function(e) NULL))
    } else if (packageVersion("caret")!="6.0.93.1" && get_os()=="osx"){
        if(Sys.info()[["machine"]]=="arm64"){
            tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/caret_6.0-93.1_arm64_macos.tgz?raw=true", type="binary", repos=NULL), error=function(e) tryCatch(remotes::install_github("leedrake5/caret", subdir="pkg/caret"), error=function(e) NULL))
        } else {
            tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/caret_6.0-93.1_x86_64_macos.tgz?raw=true", type="binary", repos=NULL), error=function(e)  tryCatch(remotes::install_github("leedrake5/caret", subdir="pkg/caret"), error=function(e) NULL))
            }
    } else if (packageVersion("caret")!="6.0.93.1" && get_os()=="linux"){
        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/caret_6.0-93.1.tar.gz?raw=true", type="source", repos=NULL), error=function(e) tryCatch(remotes::install_github("leedrake5/caret", subdir="pkg/caret"), error=function(e) NULL))
    }

if("rPDZ" %in% installed.packages()[,"Package"]==FALSE && get_os()=="windows"){
        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/rPDZ_1.2_x86_64_win.zip?raw=true", repos=NULL, type="win.binary"), error=function(e) tryCatch(remotes::install_github("leedrake5/rPDZ", subdir="rPDZ"), error=function(e) NULL))
    } else if ("rPDZ" %in% installed.packages()[,"Package"]==FALSE && get_os()=="osx"){
        if(Sys.info()[["machine"]]=="arm64"){
            tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/rPDZ_1.2_arm64_macos.tgz?raw=true", type="binary", repos=NULL), error=function(e) tryCatch(remotes::install_github("leedrake5/rPDZ", subdir="rPDZ"), error=function(e) NULL))
        } else {
            tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/rPDZ_1.2_x86_64_macos.tgz?raw=true", type="binary", repos=NULL), error=function(e)  tryCatch(remotes::install_github("leedrake5/rPDZ", subdir="rPDZ"), error=function(e) NULL))
            }
    } else if ("rPDZ" %in% installed.packages()[,"Package"]==FALSE && get_os()=="linux"){
        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/rPDZ_1.2.tar.gz?raw=true", type="source", repos=NULL), error=function(e) tryCatch(remotes::install_github("leedrake5/rPDZ", subdir="rPDZ"), error=function(e) NULL))
    }


if("Peaks" %in% installed.packages()[,"Package"]==FALSE && get_os()=="windows"){
    tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/Peaks_0.2.zip?raw=true", repos=NULL, type="win.binary"), error=function(e) tryCatch(remotes::install_github("cran/Peaks"), error=function(e) NULL))
} else if ("Peaks" %in% installed.packages()[,"Package"]==FALSE && get_os()=="osx"){
    tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/Peaks_0.2.tgz?raw=true", type="binary", repos=NULL), error=function(e) tryCatch(remotes::install_github("cran/Peaks"), error=function(e) NULL))
} else if ("Peaks" %in% installed.packages()[,"Package"]==FALSE && get_os()=="linux"){
    tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/Peaks_0.2.tar.gz?raw=true", type="source", repos=NULL), error=function(e) tryCatch(remotes::install_github("cran/Peaks"), error=function(e) NULL))
}

if("xrftools" %in% installed.packages()[,"Package"]==FALSE && get_os()=="windows"){
    tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/xrftools_0.0.1.9000.zip", repos=NULL, type="win.binary"), error=function(e) tryCatch(remotes::install_github("paleolimbot/xrftools"), error=function(e) NULL))
} else if ("xrftools" %in% installed.packages()[,"Package"]==FALSE && get_os()=="osx"){
    if(Sys.info()[["machine"]]=="arm64"){
        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/xrftools_0.0.1.9000_arm64.tar.gz?raw=true", type="source", repos=NULL), error=function(e) tryCatch(remotes::install_github("paleolimbot/xrftools"), error=function(e) NULL))
    } else {
        tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/xrftools_0.0.1.9000.tgz?raw=true", type="binary", repos=NULL), error=function(e) tryCatch(remotes::install_github("paleolimbot/xrftools"), error=function(e) NULL))
    }
} else if ("xrftools" %in% installed.packages()[,"Package"]==FALSE && get_os()=="linux"){
    tryCatch(install.packages("https://github.com/leedrake5/CloudCal/blob/master/Packages/xrftools_0.0.1.9000.tar.gz?raw=true", type="source", repos=NULL), error=function(e) tryCatch(remotes::install_github("paleolimbot/xrftools"), error=function(e) NULL))
}



#sourceCpp("pdz.cpp")

tryCatch(library(rPDZ), error=function(e) NULL)
library(reactlog)
options(shiny.reactlog = TRUE)
shiny::devmode(TRUE)
options(shiny.fullstacktrace=TRUE)
###update packages
#update.packages(repos='http://cran.rstudio.com/', ask=FALSE)

###Old ggplot2
#devtools::install_version("ggplot2", version = "2.2.1", repos = "http://cran.us.r-project.org", checkBuilt=TRUE)


#sudo su - -c "R -e \"install.packages(c('shiny', 'pbapply', 'reshape2', 'TTR', 'dplyr', 'ggtern', 'ggplot2', 'shiny', 'rhandsontable', 'random', 'data.table', 'DT', 'shinythemes', 'Cairo', 'broom', 'shinyjs', 'gridExtra', 'dtplyr', 'formattable', 'XML', 'corrplot', 'scales', 'rmarkdown', 'markdown', 'randomForest', 'doMC', 'caret'), repos='http://cran.rstudio.com/')\""

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

#source("xgbTree.R")
#source("xgbDART.R")


options(digits=12)

my.cores <- if(parallel::detectCores()>=3){
    paste0(parallel::detectCores()-2)
} else if(parallel::detectCores()<=2){
    "1"
}

tryCatch(source("https://raw.githubusercontent.com/leedrake5/CloudCal/master/file_loading.R"), error=function(e) source('file_loading.R'))

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
                  value = round(extract(., j = DT_bounds[Type ==
                    "integer", Parameter], with = FALSE)))
            }
            else {
                .
            }
        } %T>% extract(., j = `:=`(Value, -Inf))
        
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


BayesianOptimization <- function(FUN, bounds, init_grid_dt = NULL, init_points = 0,
    n_iter, acq = "ei", kappa = 2.576, eps = 0, kernel = list(type = "exponential",
        power = 2), verbose = TRUE)
{
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
                  value = round(extract(., j = DT_bounds[Type ==
                    "integer", Parameter], with = FALSE)))
            }
            else {
                .
            }
        } %T>% extract(., j = `:=`(Value, -Inf))
    iter_points_dt_backup <- Matrix_runif(n = init_points+n_iter, lower = DT_bounds[,
            Lower], upper = DT_bounds[, Upper]) %>% data.table(.) %T>%
            setnames(., old = names(.), new = DT_bounds[, Parameter]) %T>%
            {
                if (any(DT_bounds[, Type] == "integer")) {
                    set(., j = DT_bounds[Type == "integer", Parameter],
                      value = round(extract(., j = DT_bounds[Type ==
                        "integer", Parameter], with = FALSE)))
                }
                else {
                    .
                }
            }
    iter_points_dt <- data.table(matrix(-Inf, nrow = n_iter,
        ncol = nrow(DT_bounds) + 1)) %>% setnames(., old = names(.),
        new = c(DT_bounds[, Parameter], "Value"))
    DT_history <- rbind(init_grid_dt, init_points_dt, iter_points_dt) %>%
        cbind(data.table(Round = 1:nrow(.)), .)
    Pred_list <- vector(mode = "list", length = nrow(DT_history))
    for (i in 1:(nrow(init_grid_dt) + nrow(init_points_dt))) {
        if (is.infinite(DT_history[i, Value]) == TRUE) {
            This_Par <- DT_history[i, DT_bounds[, Parameter],
                with = FALSE]
        }
        else {
            next
        }
        This_Log <- utils::capture.output({
            This_Time <- system.time({
                This_Score_Pred <- tryCatch(do.call(what = FUN, args = as.list(This_Par)), error=function(e) list(Score=sample(-150:-100, 1)))
                if(is.na(This_Score_Pred)){This_Score_Pred <- list(Score=sample(-250:-200, 1))}
            })
        })
        data.table::set(DT_history, i = as.integer(i), j = "Value",
            value = as.list(c(This_Score_Pred$Score)))
        Pred_list[[i]] <- This_Score_Pred$Pred
        if (verbose == TRUE) {
            paste(c("elapsed", names(DT_history)), c(format(This_Time["elapsed"],
                trim = FALSE, digits = NULL, nsmall = 2), format(DT_history[i,
                "Round", with = FALSE], trim = FALSE, digits = NULL,
                nsmall = 0), format(DT_history[i, -"Round", with = FALSE],
                trim = FALSE, digits = NULL, nsmall = 4)), sep = " = ",
                collapse = "\t") %>% cat(., "\n")
        }
    }
   for (j in (nrow(init_grid_dt) + nrow(init_points_dt) + 1):nrow(DT_history)) {
       if (nrow(iter_points_dt) == 0) {
            next
        }
        Par_Mat <- Min_Max_Scale_Mat(as.matrix(DT_history[1:(j -
            1), DT_bounds[, Parameter], with = FALSE]), lower = DT_bounds[,
            Lower], upper = DT_bounds[, Upper])
        Rounds_Unique <- setdiff(1:(j - 1), which(duplicated(Par_Mat) ==
            TRUE))
        Value_Vec <- DT_history[1:(j - 1), Value]
        GP_Log <- utils::capture.output({
            GP <- GPfit::GP_fit(X = Par_Mat[Rounds_Unique, ],
                Y = Value_Vec[Rounds_Unique], corr = kernel)
        })
        Next_Par <- tryCatch(Utility_Max(DT_bounds, GP, acq = acq, y_max = max(DT_history[,
            Value]), kappa = kappa, eps = eps) %>% Min_Max_Inverse_Scale_Vec(.,
            lower = DT_bounds[, Lower], upper = DT_bounds[, Upper]) %>%
            magrittr::set_names(., DT_bounds[, Parameter]) %>%
            inset(., DT_bounds[Type == "integer", Parameter],
                round(extract(., DT_bounds[Type == "integer",
                  Parameter]))), error=function(e) unlist(iter_points_dt_backup[j,]))
        Next_Log <- tryCatch(utils::capture.output({
            Next_Time <- system.time({
                Next_Score_Pred <- tryCatch(do.call(what = FUN, args = as.list(Next_Par)), error=function(e) list(Score=sample(-200:-150, 1)))
                if(is.na(Next_Score_Pred)){Next_Score_Pred <- list(Score=sample(-250:-200, 1))}
            })
        }), error=function(e) NULL)
        tryCatch(data.table::set(DT_history, i = as.integer(j), j = c(DT_bounds[,
            Parameter], "Value"), value = as.list(c(Next_Par,
            Value = Next_Score_Pred$Score))), error=function(e) NULL)
        tryCatch(Pred_list[[j]] <- Next_Score_Pred$Pred, error=function(e) NULL)
        if (verbose == TRUE) {
            tryCatch(paste(c("elapsed", names(DT_history)), c(format(Next_Time["elapsed"],
                trim = FALSE, digits = NULL, nsmall = 2), format(DT_history[j,
                "Round", with = FALSE], trim = FALSE, digits = NULL,
                nsmall = 0), format(DT_history[j, -"Round", with = FALSE],
                trim = FALSE, digits = NULL, nsmall = 4)), sep = " = ",
                collapse = "\t") %>% cat(., "\n"), error=function(e) NULL)
        }#, error=function(e) NULL})
    }
    Best_Par <- as.numeric(DT_history[which.max(Value), DT_bounds[,
        Parameter], with = FALSE]) %>% magrittr::set_names(.,
        DT_bounds[, Parameter])
    Best_Value <- max(DT_history[, Value], na.rm = TRUE)
    Pred_DT <- data.table::as.data.table(Pred_list)
    Result <- list(Best_Par = Best_Par, Best_Value = Best_Value,
        History = DT_history, Pred = Pred_DT)
    cat("\n Best Parameters Found: \n")
    paste(names(DT_history), c(format(DT_history[which.max(Value),
        "Round", with = FALSE], trim = FALSE, digits = NULL,
        nsmall = 0), format(DT_history[which.max(Value), -"Round",
        with = FALSE], trim = FALSE, digits = NULL, nsmall = 4)),
        sep = " = ", collapse = "\t") %>% cat(., "\n")
    return(Result)
}


fluorescence.lines.directory <- if(file.exists("data/FluorescenceLines.csv")){
    "data/FluorescenceLines.csv"
} else if(!file.exists("data/FluorescenceLines.csv")){
    "https://raw.githubusercontent.com/leedrake5/CloudCal/master/data/FluorescenceLines.csv"
}

######Load lines
lineLibrary <- readRDS("data/LineDefinitions.rdata")
#temp <- tempfile()
fluorescence.lines <- lineLibrary$FluorescenceeLines
Wide <- lineLibrary$Wide
attach(lineLibrary$Tables)

line_strip <- function(elements){
    elements <- gsub(".K.alpha", "", elements)
    elements <- gsub(".K.beta", "", elements)
    elements <- gsub(".L.alpha", "", elements)
    elements <- gsub(".L.beta", "", elements)
    elements <- gsub(".M.line", "", elements)
    elements <- gsub(".K12", "", elements)
    elements <- gsub(".L1", "", elements)
    elements
}
line_strip <- cmpfun(line_strip)

atomic_order <- function(element){
    subset(fluorescence.lines, Symbol==line_strip(element))$AtomicNumber
}
atomic_order <- cmpfun(atomic_order)


atomic_order_vector <- function(elements){
    unlist(lapply(elements, atomic_order))
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


uniqueBeamsDetailed <- function(csv_import){

    csv_import <- csv_import %>% select_if(not_all_na)
    csv_import <- csv_import[-1,]
    beams <- as.vector((unlist(csv_import[csv_import$V1=="Exposure Number",-1])))
    unique_beams <- unique(beams)
    return(unique_beams)
}
uniqueBeamsDetailed <- cmpfun(uniqueBeamsDetailed)

uniqueBeams <- function(filepath){
    csv_import <- read.csv("/Users/lee/Google Drive/Reply to Frahm 2019/Export Results from Vanta/beamspectra-804734-2019-09-28-15-29-14.csv", header=F, stringsAsFactors=FALSE)
    
    if(csv_import[1, "V1"]=="Std#"){
        "1"
    } else if(csv_import[1, "V1"]=="sep="){
        uniqueBeamsDetailed(csv_import)
    }
}





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
    
    do.call("rbind", lapply(seq(1, length(a.vector), 1), function(x) pull_test(a.vector, x)))
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






elementGrabKalpha <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy < elementLine[6][1,]-0.02 | data$Energy > elementLine[5][1,]+0.02), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, "K-alpha", sep=" "))
    
    hold.ag
    
}
elementGrabKalpha <- cmpfun(elementGrabKalpha)


elementGrabKbeta <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.cps <- if(elementLine[8][1,]!=0){
        subset(data$CPS, !(data$Energy < elementLine[7][1,]-0.02 | data$Energy > elementLine[8][1,]+0.02))
    } else if(elementLine[8][1,]==0){
        subset(data$CPS, !(data$Energy < elementLine[7][1,]-0.02 | data$Energy > elementLine[7][1,]+0.02))
    }
    
    
    hold.file <- if(elementLine[8][1,]!=0){
        subset(data$Spectrum, !(data$Energy < elementLine[7][1,]-0.02 | data$Energy > elementLine[8][1,]+0.02))
    } else if(elementLine[8][1,]==0){
            subset(data$Spectrum, !(data$Energy < elementLine[7][1,]-0.02 | data$Energy > elementLine[7][1,]+0.02))
    }
    hold.frame <- data.frame(is.0(hold.cps, hold.file), stringsAsFactors=FALSE)
    colnames(hold.frame) <- c("CPS", "Spectrum")
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, "K-beta", sep=" "))
    
    hold.ag
    
}
elementGrabKbeta <- cmpfun(elementGrabKbeta)


elementGrabLalpha <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy < elementLine[11][1,]-0.02 | data$Energy > elementLine[10][1,]+0.02), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, "L-alpha", sep=" "))
    
    hold.ag
    
}
elementGrabLalpha <- cmpfun(elementGrabLalpha)


elementGrabLbeta <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy < elementLine[12][1,]-0.02 | data$Energy > elementLine[14][1,]+0.02), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, "L-beta", sep=" "))
    
    hold.ag
    
}
elementGrabLbeta <- cmpfun(elementGrabLbeta)

elementGrabMalpha <- function(element, data) {
    
    elementLine <- subset(fluorescence.lines, fluorescence.lines$Symbol==element)
    
    hold.frame <- data[!(data$Energy < elementLine[20][1,]-0.02 | data$Energy > elementLine[22][1,]+0.02), c("CPS", "Spectrum")]
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, "M-line", sep=" "))
    
    hold.ag
    
}
elementGrabMalpha <- cmpfun(elementGrabMalpha)


elementGrabpre <- function(element.line, data) {
    
    element.line <- make.names(element.line)
    
    element <- strsplit(x=element.line, split="\\.")[[1]][1]
    destination <- strsplit(x=element.line, split="\\.")[[1]][2]
    distance <- strsplit(x=element.line, split="\\.")[[1]][3]
    
    if(destination=="K" && distance=="alpha"){
        elementGrabKalpha(element=element, data=data)
    } else if(destination=="K" && distance=="beta"){
        elementGrabKbeta(element=element, data=data)
    } else if(destination=="L" && distance=="alpha"){
        elementGrabLalpha(element=element, data=data)
    } else if (destination=="L" && distance=="beta"){
        elementGrabLbeta(element=element, data=data)
    } else if (destination=="M" && distance=="line"){
        elementGrabMalpha(element=element, data=data)
    }
        
}
elementGrabpre <- cmpfun(elementGrabpre)




range_subset_xrf <- function(range.frame, data){
    
    new.data <- subset(data, Energy >= range.frame$EnergyMin & Energy <= range.frame$EnergyMax, drop=TRUE)
    newer.data <- aggregate(new.data, by=list(new.data$Spectrum), FUN=mean, na.rm=TRUE)[,c("Group.1", "CPS")]
    colnames(newer.data) <- c("Spectrum", as.character(range.frame$Name))
    newer.data
}
range_subset_xrf <- cmpfun(range_subset_xrf)


xrf_parse <- function(range.table, data){
    
    choice.lines <- range.table[complete.cases(range.table),]
    
    choice.list <- split(choice.lines, f=choice.lines$Name)
    names(choice.list) <- choice.lines[,"Name"]
    
    index <- choice.lines[,"Name"]
    
    selected.list <- lapply(index, function(x) range_subset_xrf(range.frame=choice.list[[x]], data=data))
    
    Reduce(function(...) merge(..., all=T), selected.list)
}
xrf_parse <- cmpfun(xrf_parse)

xrf_parse_single <- function(range.table, data, element){
    
    choice.lines <- range.table[range.table$Name %in% element,]
    
    choice.list <- split(choice.lines, f=choice.lines$Name)
    names(choice.list) <- choice.lines[,"Name"]
    
    index <- choice.lines[,"Name"]
    
    selected.list <- lapply(index, function(x) range_subset_xrf(range.frame=choice.list[[x]], data=data))
    
    Reduce(function(...) merge(..., all=T), selected.list)
}
xrf_parse_single <- cmpfun(xrf_parse_single)



elementGrabPre <- function(element.line, data, range.table=NULL){
    
    is.element <- element.line %in% spectralLines
    
    if(is.element==TRUE){
        elementGrabpre(element.line, data)
    } else if(is.element==FALSE){
        xrf_parse_single(range.table, data, element.line)
    }

    
}
elementGrabPre <- cmpfun(elementGrabPre)

elementGrab <- function(element.line, data, range.table=NULL){
    
    error_frame <- data.frame(Spectrum=unique(data$Spectrum), Hold=NA)
    colnames(error_frame) <- c("Spectrum", element.line)
    
    tryCatch(elementGrabPre(element.line=element.line, data=data, range.table=range.table), error=function(e) error_frame)
    
    
}


elementFrame <- function(data, range.table=NULL, elements){
    
    spectra.line.list <- if(get_os()=="windows"){
        lapply(elements, function(x) elementGrab(element.line=x, data=data, range.table=range.table))
    } else if(get_os()!="windows"){
        core.mod <- if(length(elements)>=as.numeric(my.cores)){
            as.numeric(my.cores)
        } else if(length(elements)<as.numeric(my.cores)){
            length(elements)
        }
        tryCatch(pblapply(cl=core.mod, X=elements, function(x) elementGrab(element.line=x, data=data, range.table=range.table)), error=function(e) lapply(elements, function(x) elementGrab(element.line=x, data=data, range.table=range.table)))
    }
    
    element.count.list <- lapply(spectra.line.list, '[', 2)
    
    #spectra.line.vector <- as.numeric(unlist(element.count.list))
    
    #dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Spectrum), length(elements))
    
    #spectra.line.frame <- data.frame(spectra.line.list[[1]]$Spectrum, spectra.line.vector, stringsAsFactors=FALSE)
    
    #spectra.line.frame <- spectra.line.list %>% purrr::reduce(full_join, by='Spectrum')
    
    spectra.line.frame <- Reduce(function(x, y) merge(x, y, all=TRUE), spectra.line.list)
    
    colnames(spectra.line.frame) <- c("Spectrum", elements)
    
    spectra.line.frame <- as.data.frame(spectra.line.frame, stringsAsFactors=FALSE)
    
    spectra.line.frame <- spectra.line.frame[order(as.character(spectra.line.frame$Spectrum)),]
    
    spectra.line.frame$Spectrum <- gsub(".pdz", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".csv", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".CSV", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".spt", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".mca", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".spx", "", spectra.line.frame$Spectrum)
    
    
    spectra.line.frame
    
}
elementFrame <- cmpfun(elementFrame)


wideElementGrabLine <- function(element.line, data) {
    
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
    
    hold.ag <- aggregate(list(hold.frame$CPS), by=list(hold.frame$Spectrum), FUN="sum")
    colnames(hold.ag) <- c("Spectrum", paste(element, line, sep=" "))
    
    hold.ag
    
}
wideElementGrabLine <- cmpfun(wideElementGrabLine)

wideElementGrabPre <- function(element.line, data, range.table=NULL){
    
    is.element <- element.line %in% spectralLines
    
    if(is.element==TRUE){
        wideElementGrabLine(element.line, data)
    } else if(is.element==FALSE){
        xrf_parse_single(range.table, data, element.line)
    }

    
}
wideElementGrabPre <- cmpfun(wideElementGrabPre)

wideElementGrab <- function(element.line, data, range.table=NULL){
    
    error_frame <- data.frame(Spectrum=unique(data$Spectrum), Hold=NA)
    colnames(error_frame) <- c("Spectrum", element.line)
    
    tryCatch(wideElementGrabPre(element.line=element.line, data=data, range.table=range.table), error=function(e) error_frame)
    
    
}

wideElementFrame <- function(data, elements, range.table=NULL){
    
    spectra.line.list <- if(get_os()=="windows"){
        lapply(elements, function(x) wideElementGrab(element.line=x, data=data, range.table=range.table))
    }else if(get_os()!="windows"){
        core.mod <- if(length(elements)>=as.numeric(my.cores)){
            as.numeric(my.cores)
        } else if(length(elements)<as.numeric(my.cores)){
            length(elements)
        }
        #pblapply(cl=core.mod, X=elements, function(x) wideElementGrab(element.line=x, data=data))
        lapply(elements, function(x) wideElementGrab(element.line=x, data=data, range.table=range.table))
    }
    
    element.count.list <- lapply(spectra.line.list, '[', 2)
    
    spectra.line.vector <- as.numeric(unlist(element.count.list))
    
    dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Spectrum), length(elements))
    
    spectra.line.frame <- data.frame(spectra.line.list[[1]]$Spectrum, spectra.line.vector, stringsAsFactors=FALSE)
    
    colnames(spectra.line.frame) <- c("Spectrum", elements)
    
    spectra.line.frame <- as.data.frame(spectra.line.frame, stringsAsFactors=FALSE)
    
    spectra.line.frame <- spectra.line.frame[order(as.character(spectra.line.frame$Spectrum)),]
    
    spectra.line.frame$Spectrum <- gsub(".pdz", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".csv", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".CSV", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".spt", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".mca", "", spectra.line.frame$Spectrum)
    spectra.line.frame$Spectrum <- gsub(".spx", "", spectra.line.frame$Spectrum)
    
    
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
    
    data.sum
    
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


spectra_tc_prep_xrf <- function(spectra, energy.min=NULL, energy.max=NULL, compress="100 eV", transformation="None"){
    
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
    #data <- data[,complete.cases(data)]
    
    total.counts <- rowSums(data[,-1], na.rm=TRUE)
    
    data <- data.frame(Spectrum=data$Spectrum, data[,-1]/total.counts, stringsAsFactors=FALSE)
    spectra.frame <- do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),0)))
    
    return(spectra.frame)

}
spectra_tc_prep_xrf <- cmpfun(spectra_tc_prep_xrf)


spectra_comp_prep_xrf <- function(spectra, energy.min=NULL, energy.max=NULL, norm.min, norm.max, compress="100 eV", transformation="None"){
    
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


simple_tc_prep_xrf <- function(data,spectra.line.table, element.line) {
    
    intensity <- spectra.line.table[,element.line]
    
    
    total.counts <- aggregate(CPS~Spectrum, data=data, sum)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    
    predict.frame.tc <- data.frame(Intensity=intensity/total.counts$CPS, stringsAsFactors=FALSE)
    
    predict.frame.tc
}
simple_tc_prep_xrf <- cmpfun(simple_tc_prep_xrf)


simple_comp_prep_xrf <- function(data, spectra.line.table, element.line, norm.min, norm.max) {
    
    
    
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

just_spectra_summary_apply <- function(spectra.frame, normalization, min=NULL, max=NULL, compress="100 eV", transformation="None", energy.range=c(0.7, 37)){
    
    new.spectrum <- if(normalization==1){
        spectra_simp_prep_xrf(spectra=spectra.frame, compress=compress, transformation=transformation, energy.min=energy.range[1], energy.max=energy.range[2])
    } else if(normalization==2){
        spectra_tc_prep_xrf(spectra=spectra.frame, compress=compress, transformation=transformation, energy.min=energy.range[1], energy.max=energy.range[2])
    } else if(normalization==3){
        spectra_comp_prep_xrf(spectra=spectra.frame, norm.min=min, norm.max=max, compress=compress, transformation=transformation, energy.min=energy.range[1], energy.max=energy.range[2])
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


lucas_tc_prep_xrf <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines=NULL) {
    
    
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





lucas_comp_prep_xrf <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines=NULL, norm.min, norm.max) {
    
    
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


simple_tc_prep_xrf_net <- function(data,spectra.line.table, element.line) {
    
    intensity <- spectra.line.table[,element.line]
    
    total.counts.net <- rowSums(spectra.line.table[,-1])
    total.counts <- data.frame(data$Spectrum, total.counts.net, stringsAsFactors=FALSE)
    colnames(total.counts) <- c("Spectrum", "CPS")
    
    
    
    predict.frame.tc <- data.frame(Intensity=intensity/total.counts$CPS, stringsAsFactors=FALSE)
    
    predict.frame.tc
}
simple_tc_prep_xrf_net <- cmpfun(simple_tc_prep_xrf_net)


simple_comp_prep_xrf_net <- function(data, spectra.line.table, element.line, norm.min, norm.max) {
    
    
    
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



lucas_tc_prep_xrf_net <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines) {
    
    
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


lucas_comp_prep_xrf_net <- function(data, spectra.line.table, element.line, slope.element.lines, intercept.element.lines, norm.min, norm.max) {
    
    
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



####Custom Lines


range_subset <- function(range.frame, data){
    
    new.data <- subset(data, Energy >= range.frame$EnergyMin & Energy <= range.frame$EnergyMax, drop=TRUE)
    newer.data <- aggregate(new.data, by=list(new.data$Spectrum), FUN=mean, na.rm=TRUE)[,c("Group.1", "CPS")]
    colnames(newer.data) <- c("Spectrum", as.character(range.frame$Name))
    newer.data
}
range_subset <- cmpfun(range_subset)


xrf_parse <- function(range.table, data){
    
    choice.lines <- range.table[complete.cases(range.table),]
    
    choice.list <- split(choice.lines, f=choice.lines$Name)
    names(choice.list) <- choice.lines[,"Name"]
    
    index <- choice.lines[,"Name"]
    
    selected.list <- lapply(index, function(x) range_subset(range.frame=choice.list[[x]], data=data))
    
    Reduce(function(...) merge(..., all=T), selected.list)
}
xrf_parse <- cmpfun(xrf_parse)


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
        sliderInput("forestnumber", label="Iterations", min=5, max=2000, value=selection)
    }  else if(radiocal==5){
        sliderInput("forestnumber", label="Iterations", min=5, max=2000, value=selection)
    } else if(radiocal==6){
        sliderInput("forestnumber", label="Iterations", min=5, max=2000, value=selection)
    } else if(radiocal==7){
        sliderInput("forestnumber", label="Iterations", min=5, max=2000, value=selection)
    } else if(radiocal==8){
        sliderInput("forestnumber", label="Iterations", min=5, max=2000, value=selection)
    } else if(radiocal==9){
        sliderInput("forestnumber", label="Iterations", min=5, max=2000, value=selection)
    } else if(radiocal==10){
        sliderInput("forestnumber", label="Iterations", min=5, max=2000, value=selection)
    } else if(radiocal==11){
        sliderInput("forestnumber", label="Iterations", min=5, max=2000, value=selection)
    } else if(radiocal==12){
        sliderInput("forestnumber", label="Iterations", min=5, max=2000, value=selection)
    } else if(radiocal==13){
        sliderInput("forestnumber", label="Iterations", min=5, max=2000, value=selection)
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
    } else if(radiocal==8 && xgbtype=="Linear"){
        sliderInput("foresttrees", label="Number of Rounds", min=50, max=2000, value=selection)
    } else if(radiocal==9 && xgbtype=="Tree"){
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

predictIntensitySimpPreGen <- function(spectra, hold.frame, element, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra"){
    
    data <- spectra
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


predictFrameSimpGen <- function(spectra, hold.frame, dependent.transformation="None", element, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra", y_min=0, y_max=1, seed=NULL){
    
    data <- spectra
    spectra.line.table <- hold.frame
    
    predict.intensity.simp <- predictIntensitySimpPreGen(spectra=spectra, hold.frame=hold.frame, element=element, norm.type=norm.type, norm.min=norm.min, norm.max=norm.max, data.type=data.type)
    
    predict.frame.simp <- data.frame(predict.intensity.simp, spectra.line.table[,"Concentration"])
    colnames(predict.frame.simp) <- c(names(predict.intensity.simp), "Concentration")
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

predictIntensityForestPreGen <- function(spectra, hold.frame, element, intercepts=NULL, slopes=NULL, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra"){
    
    data <- spectra
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

predictFrameXGBoostGen <- function(spectra, hold.frame, slopes=NULL, dependent.transformation="None", element, intercepts=NULL, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra", y_min=0, y_max=1){
    
    spectra.line.table <- hold.frame
    
    predict.intensity.forest <- predictIntensityForestPreGen(spectra=spectra, hold.frame=hold.frame, element=element, slopes=slopes, intercepts=intercepts, norm.type=norm.type, norm.min=norm.min, norm.max=norm.max, data.type=data.type)

    
    
    
    predict.frame.forest <- data.frame(predict.intensity.forest, Concentration=spectra.line.table[,"Concentration"])
    predict.frame.forest <- predict.frame.forest[complete.cases(predict.frame.forest$Concentration),]
    
    predict.frame.forest$Concentration <- if(dependent.transformation=="None"){
        predict.frame.forest$Concentration
    } else if(dependent.transformation=="Log"){
        log(predict.frame.forest$Concentration)
    } else if(dependent.transformation=="Scale"){
        scaleTransform(values=predict.frame.forest$Concentration, y_min=y_min, y_max=y_max)
    }
    
    return(as.matrix(predictFrameCheck(predict.frame.forest)))
    
}

predictIntensityXGBoost <- function(predict.frame){
    as.matrix(predict.frame[,!(colnames(predict.frame) %in% "Concentration")])
}


predictFrameForestGen <- function(seed=1, spectra, hold.frame, slopes=NULL, dependent.transformation="None", element, intercepts=NULL, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra", y_min=0, y_max=1){
    
    spectra.line.table <- hold.frame
    
    predict.intensity.forest <- predictIntensityForestPreGen(spectra=spectra, hold.frame=hold.frame, element=element, slopes=slopes, intercepts=intercepts, norm.type=norm.type, norm.min=norm.min, norm.max=norm.max, data.type=data.type)

    
    
    
    predict.frame.forest <- data.frame(predict.intensity.forest, Concentration=spectra.line.table[,"Concentration"])
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

predictIntensityLucPreGen <- function(spectra, hold.frame, element, intercepts=NULL, slopes, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra"){
    
    predict.intensity.forest <- predictIntensityForestPreGen(spectra=spectra, hold.frame=hold.frame, element=element, intercepts=intercepts, norm.type=norm.type, norm.min=norm.min, norm.max=norm.max, data.type=data.type)

    predict.intensity.forest[,c("Intensity", slopes)]
    
}

predictFrameLucGen <- function(seed=1, spectra, hold.frame, element, intercepts=NULL, slopes, dependent.transformation="None", norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra", y_min=0, y_max=1){
    
    data <- spectra
    spectra.line.table <- hold.frame

    predict.intensity.luc <- predictIntensityLucPreGen(spectra=spectra, hold.frame=hold.frame, element=element, intercepts=intercepts, slopes=slopes, norm.type=norm.type, norm.min=norm.min, norm.max=norm.max, data.type=data.type)
    
    predict.frame.luc <- data.frame(predict.intensity.luc, spectra.line.table[,"Concentration"])
    predict.frame.luc <- predict.frame.luc[complete.cases(predict.frame.luc),]
    colnames(predict.frame.luc) <- c(names(predict.intensity.luc), "Concentration")
    predict.frame.luc <- predict.frame.luc[complete.cases(predict.frame.luc$Concentration),]
    
    predict.frame.luc$Concentration <- if(dependent.transformation=="None"){
        predict.frame.luc$Concentration
    } else if(dependent.transformation=="Log"){
        log(predict.frame.luc$Concentration)
    } else if(dependent.transformation=="Scale"){
        scaleTransform(values=predict.frame.luc$Concentration, y_min=y_min, y_max=y_max)
    }
    
    result <- predictFrameCheck(predict.frame.luc)
    set.seed(seed)
    result$RandXXX <- rnorm(nrow(result), 1, 0.2)
    result <- result[order(result$RandXXX),!colnames(result) %in% "RandXXX"]

    
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


rainforestDataGen <- function(seed=1, spectra, compress="100 eV", transformation="None", dependent.transformation="None", energy.range=c(0.7, 37), hold.frame, norm.type, norm.min=NULL, norm.max=NULL, data.type="Spectra", y_min=0, y_max=1){
    
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

calBundle <- function(filetype, units, spectra, intensities, definitions, values, notes, calList, compress=FALSE){
    
    list(FileType=filetype, Units=units, Spectra=spectra, Intensities=intensities, Definitions=definitions, Values=values, Notes=notes, calList=calListCompress(calList))
    
}


cloudCalPredict <- function(Calibration, elements.cal, elements, variables, valdata, deconvoluted_valdata=NULL, count.list=NULL, rounding=4, multiplier=1, confidence=FALSE, cores=NULL){
    
    if(is.null(cores)){
        cores = parallel::detectCores()-2
    }
    
    deconvoluted_data <-spectra_gls_deconvolute(valdata, cores=cores)
    deconvoluted_valdata <- deconvoluted_data
    
    if(any(unlist(sapply(Calibration$calList, function(x) x[[1]][["CalTable"]][["Deconvolution"]]!="None")))){
        if(is.null(deconvoluted_valdata)){

            
        }
        }
        
    
    if(is.null(count.list)){
        count.list <- list(
            Narrow=narrowLineTable(spectra=valdata, definition.table=Calibration$Definitions, elements=variables),
            Wide=wideLineTable(spectra=valdata, definition.table=Calibration$Definitions, elements=variables)
            )
        count.list$Deconvoluted <- deconvolutionIntensityFrame(deconvoluted_data$Areas, count.list$Narrow)
    }
    

    
    
    #count.table <- data.frame(fullInputValCounts())
    the.cal <- Calibration[["calList"]]
    #elements.cal <- calValElements()
    elements <- elements.cal[!is.na(match(elements.cal, names(count.list[["Narrow"]])))]
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
            
            count.table <- count.list$Narrow
        
        predicted.frame <- data.frame(Spectrum=count.table$Spectrum, stringsAsFactors=FALSE)
            
            #pblapply(elements, function(x) tryCatch(predicted.frame[,x] <-
        
        for(x in elements){
            values <-
            if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==1 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=general_prep_xrf(
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                            ),
                            element.line=x),
                            dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                            ymin=the.cal[[x]][[1]][1]$Scale$Min,
                            ymax=the.cal[[x]][[1]][1]$Scale$Max,
                            confidence=confidence
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==1 && the.cal[[x]][[1]]$CalTable$NormType[1]==2) {
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=simple_tc_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                            ),
                        element.line=x
                        ),
                        dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                        ymin=the.cal[[x]][[1]][1]$Scale$Min,
                        ymax=the.cal[[x]][[1]][1]$Scale$Max,
                        confidence=confidence
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==1 && the.cal[[x]][[1]]$CalTable$NormType[1]==3) {
                mclPred(
                    object=the.cal[[x]][[2]],
                        newdata=simple_comp_prep_xrf(
                            data=valdata,
                            spectra.line.table=as.data.frame(
                                count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                                ),
                            element.line=x,
                            norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                            norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                            ),
                            dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                            ymin=the.cal[[x]][[1]][1]$Scale$Min,
                            ymax=the.cal[[x]][[1]][1]$Scale$Max,
                            confidence=confidence
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==3 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                 mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_simp_prep_xrf(
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==3 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_tc_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==3 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_comp_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                            ),
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None"  && cal_type(x)==4 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_simp_prep_xrf(
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==4 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_tc_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==4 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][[2]],
                    newdata=lucas_comp_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                        ),
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==5 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=spectra_simp_prep_xrf(
                        spectra=valdata,
                        energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                        energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                        compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                        transformation=the.cal[[x]][[1]]$CalTable$Transformation[1]
                        )[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==5 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_tc_prep_xrf(spectra=valdata,
                        energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                        energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                        compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                        transformation=the.cal[[x]][[1]]$CalTable$Transformation[1]
                        )[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==5 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_comp_prep_xrf(spectra=valdata,
                        energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                        energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                        compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                        transformation=the.cal[[x]][[1]]$CalTable$Transformation[1],
                            norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                            norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1])[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==6 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_simp_prep_xrf(
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==6 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_tc_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==6 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][[2]],
                    newdata=lucas_comp_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                        ),
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==7 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=spectra_simp_prep_xrf(spectra=valdata,
                    energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                    energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                    compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                    transformation=the.cal[[x]][[1]]$CalTable$Transformation[1]
                    )[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==7 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_tc_prep_xrf(spectra=valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                transformation=the.cal[[x]][[1]]$CalTable$Transformation[1]
                )[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==7 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_comp_prep_xrf(spectra=valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                transformation=the.cal[[x]][[1]]$CalTable$Transformation[1],
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1])[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==8 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_simp_prep_xrf(
                    spectra.line.table=as.data.frame(
                    count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==8 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                if(confidence==FALSE){
                    mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_tc_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                    ),
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
                    object=the.cal[[x]][[2]],
                    newdata=xgb.DMatrix(as.matrix(lucas_tc_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,colnames(the.cal[[x]][[2]][["trainingData"]][,c(-1, -2)])]
                    ),
                    element.line=x,
                    slope.element.lines=colnames(the.cal[[x]][[2]][["trainingData"]][,c(-1, -2)]),
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    ))),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                    )
                }
                
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==8 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_comp_prep_xrf(
                    data=valdata,
                    spectra.line.table=as.data.frame(
                    count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                ),
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==9 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_simp_prep_xrf(spectra=valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                transformation=the.cal[[x]][[1]]$CalTable$Transformation[1]
                )[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==9 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_tc_prep_xrf(spectra=valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                transformation=the.cal[[x]][[1]]$CalTable$Transformation[1]
                )[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==9 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_comp_prep_xrf(spectra=valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                transformation=the.cal[[x]][[1]]$CalTable$Transformation[1],
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1])[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==10 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_simp_prep_xrf(
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==10 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_tc_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==10 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][[2]],
                    newdata=lucas_comp_prep_xrf(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                        ),
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==11 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=spectra_simp_prep_xrf(spectra=valdata,
                    energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                    energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                    compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                    transformation=the.cal[[x]][[1]]$CalTable$Transformation[1]
                    )[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==11 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_tc_prep_xrf(spectra=valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                transformation=the.cal[[x]][[1]]$CalTable$Transformation[1]
                )[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==11 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_comp_prep_xrf(spectra=valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                transformation=the.cal[[x]][[1]]$CalTable$Transformation[1],
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1])[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==12 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_simp_prep_xrf(
                    spectra.line.table=as.data.frame(
                    count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==12 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_tc_prep_xrf(
                    data=valdata,
                    spectra.line.table=as.data.frame(
                    count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                ),
                element.line=x,
                slope.element.lines=variables,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                ),
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==12 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_comp_prep_xrf(
                    data=valdata,
                    spectra.line.table=as.data.frame(
                    count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                ),
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==13 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_simp_prep_xrf(spectra=valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                transformation=the.cal[[x]][[1]]$CalTable$Transformation[1]
                )[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==13 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_tc_prep_xrf(spectra=valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                transformation=the.cal[[x]][[1]]$CalTable$Transformation[1]
                )[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="None" && cal_type(x)==13 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_comp_prep_xrf(spectra=valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                transformation=the.cal[[x]][[1]]$CalTable$Transformation[1],
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1])[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==1 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=general_prep_xrf(
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                            ),
                            element.line=x),
                            dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                            ymin=the.cal[[x]][[1]][1]$Scale$Min,
                            ymax=the.cal[[x]][[1]][1]$Scale$Max,
                            confidence=confidence
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==1 && the.cal[[x]][[1]]$CalTable$NormType[1]==2) {
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=simple_tc_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                            ),
                        element.line=x
                        ),
                        dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                        ymin=the.cal[[x]][[1]][1]$Scale$Min,
                        ymax=the.cal[[x]][[1]][1]$Scale$Max,
                        confidence=confidence
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==1 && the.cal[[x]][[1]]$CalTable$NormType[1]==3) {
                mclPred(
                    object=the.cal[[x]][[2]],
                        newdata=simple_comp_prep_xrf(
                            data=deconvoluted_valdata,
                            spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                                ),
                            element.line=x,
                            norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                            norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                            ),
                            dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                            ymin=the.cal[[x]][[1]][1]$Scale$Min,
                            ymax=the.cal[[x]][[1]][1]$Scale$Max,
                            confidence=confidence
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==3 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                 mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_simp_prep_xrf(
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==3 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_tc_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==3 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_comp_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                            ),
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares"  && cal_type(x)==4 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_simp_prep_xrf(
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==4 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_tc_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==4 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][[2]],
                    newdata=lucas_comp_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                        ),
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==5 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=spectra_simp_prep_xrf(
                        spectra=deconvoluted_valdata,
                        energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                        energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                        compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                        transformation=the.cal[[x]][[1]]$CalTable$Transformation[1]
                        )[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==5 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_tc_prep_xrf(spectra=deconvoluted_valdata,
                        energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                        energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                        compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                        transformation=the.cal[[x]][[1]]$CalTable$Transformation[1]
                        )[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==5 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_comp_prep_xrf(spectra=deconvoluted_valdata,
                        energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                        energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                        compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                        transformation=the.cal[[x]][[1]]$CalTable$Transformation[1],
                            norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                            norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1])[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==6 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_simp_prep_xrf(
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==6 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_tc_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==6 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][[2]],
                    newdata=lucas_comp_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                        ),
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==7 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=spectra_simp_prep_xrf(spectra=deconvoluted_valdata,
                    energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                    energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                    compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                    transformation=the.cal[[x]][[1]]$CalTable$Transformation[1]
                    )[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==7 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_tc_prep_xrf(spectra=deconvoluted_valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                transformation=the.cal[[x]][[1]]$CalTable$Transformation[1]
                )[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==7 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_comp_prep_xrf(spectra=deconvoluted_valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                transformation=the.cal[[x]][[1]]$CalTable$Transformation[1],
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1])[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==8 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_simp_prep_xrf(
                    spectra.line.table=as.data.frame(
                    count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==8 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                if(confidence==FALSE){
                    mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_tc_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                    ),
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
                    object=the.cal[[x]][[2]],
                    newdata=xgb.DMatrix(as.matrix(lucas_tc_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,colnames(the.cal[[x]][[2]][["trainingData"]][,c(-1, -2)])]
                    ),
                    element.line=x,
                    slope.element.lines=colnames(the.cal[[x]][[2]][["trainingData"]][,c(-1, -2)]),
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    ))),
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                    )
                }
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==8 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_comp_prep_xrf(
                    data=deconvoluted_valdata,
                    spectra.line.table=as.data.frame(
                    count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                ),
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==9 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_simp_prep_xrf(spectra=deconvoluted_valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                transformation=the.cal[[x]][[1]]$CalTable$Transformation[1]
                )[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==9 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_tc_prep_xrf(spectra=deconvoluted_valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                transformation=the.cal[[x]][[1]]$CalTable$Transformation[1]
                )[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==9 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_comp_prep_xrf(spectra=deconvoluted_valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                transformation=the.cal[[x]][[1]]$CalTable$Transformation[1],
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1])[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==10 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_simp_prep_xrf(
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==10 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_tc_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==10 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][[2]],
                    newdata=lucas_comp_prep_xrf(
                        data=deconvoluted_valdata,
                        spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                        ),
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==11 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=spectra_simp_prep_xrf(spectra=deconvoluted_valdata,
                    energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                    energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                    compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                    transformation=the.cal[[x]][[1]]$CalTable$Transformation[1]
                    )[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==11 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_tc_prep_xrf(spectra=deconvoluted_valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                transformation=the.cal[[x]][[1]]$CalTable$Transformation[1]
                )[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==11 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_comp_prep_xrf(spectra=deconvoluted_valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                transformation=the.cal[[x]][[1]]$CalTable$Transformation[1],
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1])[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==12 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_simp_prep_xrf(
                    spectra.line.table=as.data.frame(
                    count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==12 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_tc_prep_xrf(
                    data=deconvoluted_valdata,
                    spectra.line.table=as.data.frame(
                    count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                ),
                element.line=x,
                slope.element.lines=variables,
                intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                ),
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==12 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_comp_prep_xrf(
                    data=deconvoluted_valdata,
                    spectra.line.table=as.data.frame(
                    count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]][,variables]
                ),
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
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==13 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_simp_prep_xrf(spectra=deconvoluted_valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                transformation=the.cal[[x]][[1]]$CalTable$Transformation[1]
                )[,-1],
                    dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                    ymin=the.cal[[x]][[1]][1]$Scale$Min,
                    ymax=the.cal[[x]][[1]][1]$Scale$Max,
                    confidence=confidence,
                    finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==13 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_tc_prep_xrf(spectra=deconvoluted_valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                transformation=the.cal[[x]][[1]]$CalTable$Transformation[1]
                )[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Spectra" && the.cal[[x]][[1]]$CalTable$Deconvolution=="Least Squares" && cal_type(x)==13 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                object=the.cal[[x]][[2]],
                newdata=spectra_comp_prep_xrf(spectra=deconvoluted_valdata,
                energy.min=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[1],
                energy.max=as.numeric(unlist(strsplit(as.character(the.cal[[x]][[1]]$CalTable$EnergyRange[1]), "-")))[2],
                compress=the.cal[[x]][[1]]$CalTable$Compress[1],
                transformation=the.cal[[x]][[1]]$CalTable$Transformation[1],
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1])[,-1],
                dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                ymin=the.cal[[x]][[1]][1]$Scale$Min,
                ymax=the.cal[[x]][[1]][1]$Scale$Max,
                confidence=confidence,
                finalModel=TRUE
                )
            } else if(val.data.type=="Net" && cal_type(x)==1 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=general_prep_xrf_net(
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                            element.line=x),
                            dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                            confidence=confidence
                )
            } else if(val.data.type=="Net" && cal_type(x)==1 && the.cal[[x]][[1]]$CalTable$NormType[1]==2) {
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=simple_tc_prep_xrf_net(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]]
                            ),
                            element.line=x
                            ),
                            dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                            ymin=the.cal[[x]][[1]][1]$Scale$Min,
                            ymax=the.cal[[x]][[1]][1]$Scale$Max,
                            confidence=confidence
                )
            } else if(val.data.type=="Net" && cal_type(x)==1 && the.cal[[x]][[1]]$CalTable$NormType[1]==3) {
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=simple_comp_prep_xrf_net(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]]
                            ),
                        element.line=x,
                        norm.min=the.cal[[x]][[1]][1]$CalTable$Min[1],
                        norm.max=the.cal[[x]][[1]][1]$CalTable$Max[1]
                        ),
                        dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                        ymin=the.cal[[x]][[1]][1]$Scale$Min,
                        ymax=the.cal[[x]][[1]][1]$Scale$Max,
                        confidence=confidence
                )
            } else if(val.data.type=="Net" && cal_type(x)==3 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_simp_prep_xrf_net(
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]]
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
            } else if(val.data.type=="Net" && cal_type(x)==3 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_tc_prep_xrf_net(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]]
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
            } else if(val.data.type=="Net" && cal_type(x)==3 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
                mclPred(
                    object=the.cal[[x]][[2]],
                    newdata=lucas_comp_prep_xrf_net(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]]
                            ),
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
        } else if(val.data.type=="Net" && cal_type(x)==4 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
            mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_simp_prep_xrf_net(
                    spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]]
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
        } else if(val.data.type=="Net" && cal_type(x)==4 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
            mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_tc_prep_xrf_net(
                    data=valdata,
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]]
                            ),
                        element.line=x,
                        slope.element.lines=variables,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                        ),
                        dependent.transformation=the.cal[[x]][[1]][1]$CalTable$DepTrans,
                        confidence=confidence,
                        finalModel=TRUE
            )
        } else if(val.data.type=="Net" && cal_type(x)==4 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
            mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_comp_prep_xrf_net(
                data=valdata,
                    spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]]
                        ),
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
        }  else if(val.data.type=="Net" && cal_type(x)==6 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
            mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_simp_prep_xrf_net(
                    spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]]
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
        } else if(val.data.type=="Net" && cal_type(x)==6 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
            mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_tc_prep_xrf_net(
                    data=valdata,
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]]
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
        } else if(val.data.type=="Net" && cal_type(x)==6 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
            mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_comp_prep_xrf_net(
                data=valdata,
                    spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]]
                        ),
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
        }  else if(val.data.type=="Net" && cal_type(x)==8 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
            mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_simp_prep_xrf_net(
                    spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]]
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
        } else if(val.data.type=="Net" && cal_type(x)==8 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
            mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_tc_prep_xrf_net(
                    data=valdata,
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]]
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
        } else if(val.data.type=="Net" && cal_type(x)==8 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
            mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_comp_prep_xrf_net(
                data=valdata,
                    spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]]
                        ),
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
        }  else if(val.data.type=="Net" && cal_type(x)==10 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
            mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_simp_prep_xrf_net(
                    spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]]
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
        } else if(val.data.type=="Net" && cal_type(x)==10 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
            mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_tc_prep_xrf_net(
                    data=valdata,
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]]
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
        } else if(val.data.type=="Net" && cal_type(x)==10 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
            mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_comp_prep_xrf_net(
                data=valdata,
                    spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]]
                            ),
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
        }  else if(val.data.type=="Net" && cal_type(x)==12 && the.cal[[x]][[1]]$CalTable$NormType[1]==1){
            mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_simp_prep_xrf_net(
                    spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]]
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
        } else if(val.data.type=="Net" && cal_type(x)==12 && the.cal[[x]][[1]]$CalTable$NormType[1]==2){
            mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_tc_prep_xrf_net(
                    data=valdata,
                        spectra.line.table=as.data.frame(
                            count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]]
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
        } else if(val.data.type=="Net" && cal_type(x)==12 && the.cal[[x]][[1]]$CalTable$NormType[1]==3){
            mclPred(
                object=the.cal[[x]][[2]],
                newdata=lucas_comp_prep_xrf_net(
                data=valdata,
                    spectra.line.table=as.data.frame(
                        count.list[[the.cal[[x]][[1]]$CalTable$LineType[1]]]
                            ),
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
    
    val.frame <- data.frame(Concentration=concentration, Intensity=as.vector(cal.est.conc.pred.luc), Prediction=as.vector(cal.est.conc.pred.luc))
    
    return(val.frame)
}

xgbValGen <- function(model, data, predict.frame, dependent.transformation, y_min=0, y_max=1){
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
    
    val.frame <- data.frame(Concentration=concentration, Intensity=as.vector(cal.est.conc.pred.luc), Prediction=as.vector(cal.est.conc.pred.luc))
    
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
        test2 <- as.data.frame(lapply(test[,3:length(test)], as.numeric), stringsAsFactors=FALSE)
        new.frame <- data.frame(Include=test$Include, Spectrum=test$Spectrum, test2, stringsAsFactors=FALSE)
    } else if(!"Include" %in% colnames(val.frame)){
        test <- remove.factors(val.frame)
        test2 <- as.data.frame(lapply(test[,-1], as.numeric), stringsAsFactors=FALSE)
        new.frame <- data.frame(Spectrum=test$Spectrum, test2, stringsAsFactors=FALSE)
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

modelPackPre <- function(parameters, model, compress=TRUE){
    
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
        list(Parameters=parameters, Model=model, rawModel=model.raw)
    } else if(parameters$CalTable$CalType==8 | parameters$CalTable$CalType==9){
        list(Parameters=parameters, Model=model, rawModel=model.raw)
    }
    
    if(is.null(result.list$rawModel)){
        result.list$rawModel <- NULL
    }
    
    return(result.list)
    
}

modelPack <- function(parameters, model, compress=TRUE){
    modelPackPre(parameters=parameters, model=model, compress=compress)
}

calListCompress <- function(calList){
    calListNames <- names(calList)[as.vector(sapply(calList, function(x) !"Delete" %in% colnames(x[[1]]$CalTable)))]
    newcalList <- list()
    for(i in calListNames){
        newcalList[[i]] <- modelPack(parameters=calList[[i]][[1]], model=calList[[i]][[2]], compress=TRUE)
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
    
    for(i in variables){
        if(i %in% colnames(calibration$Intensities)){
            if(i %in% names(calibration$calList)){
                calibration$calList[[i]] <- calibration$calList[[i]]
            } else if(!i %in% names(calibration$calList)){
                calibration$calList[[i]] <- list(Parameters=deleteCalConditions(element=i), Model=lm(calibration$Values[,i]~calibration$Intensities[,i]), na.action=na.omit)
            }
        }
        
    }
    
    if(temp==TRUE){
        for(i in variables){
            calibration$calList[[i]][[1]]$CalTable$Delete <- TRUE
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

rmspe_gather <- function(calibration, element){
    tryCatch(MLmetrics::RMSPE(y_pred = predictions[complete.cases(calibration[["Values"]][element]),element], y_true = calibration[["Values"]][complete.cases(calibration[["Values"]][,element]),element]), error=function(e) NULL)
}

mae_gather <- function(calibration, lm.list, element){
        if(calibration[["calList"]][[element]][["Parameters"]][["CalTable"]][["CalType"]][[1]]==1 | calibration[["calList"]][[element]][["Parameters"]][["CalTable"]][["CalType"]][[1]]==2 | calibration[["calList"]][[element]][["Parameters"]][["CalTable"]][["CalType"]][[1]]==3){
            tryCatch(MLmetrics::MAE(y_pred = predictions[complete.cases(calibration[["Values"]][element]),element], y_true = calibration[["Values"]][complete.cases(calibration[["Values"]][,element]),element]), error=function(e) NULL)
    } else {
        tryCatch(calibration[["calList"]][[i]][["Model"]]$results[which.min(calibration[["calList"]][[element]][["Model"]]$results[, "MAE"]), ]$MAE, error=function(e) tryCatch(MLmetrics::MAE(y_pred = predictions[complete.cases(calibration[["Values"]][element]),element], y_true = calibration[["Values"]][complete.cases(calibration[["Values"]][,element]),element]), error=function(e) NULL), error=function(e) NULL)
    }
}

mape_gather <- function(calibration, lm.list, element){
       if(calibration[["calList"]][[element]][["Parameters"]][["CalTable"]][["CalType"]][[1]]==1 | calibration[["calList"]][[element]][["Parameters"]][["CalTable"]][["CalType"]][[1]]==2 | calibration[["calList"]][[element]][["Parameters"]][["CalTable"]][["CalType"]][[1]]==3){
            tryCatch(MLmetrics::MAPE(y_pred = predictions[complete.cases(calibration[["Values"]][element]),element], y_true = calibration[["Values"]][complete.cases(calibration[["Values"]][,element]),element]), error=function(e) NULL)
    } else {
        tryCatch(calibration[["calList"]][[i]][["Model"]]$results[which.min(calibration[["calList"]][[element]][["Model"]]$results[, "MAE"]), ]$MAPE, error=function(e) tryCatch(MLmetrics::MAPE(y_pred = predictions[complete.cases(calibration[["Values"]][element]),element], y_true = calibration[["Values"]][complete.cases(calibration[["Values"]][,element]),element]), error=function(e) NULL), error=function(e) NULL)
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
       newdata <- predict(preProc, newdata)
    if(!is.data.frame(newdata) &&
      !is.null(object$modelInfo$predict) &&
      any(grepl("as.data.frame", as.character(body(object$modelInfo$predict)))))
           newdata <- as.data.frame(newdata)
    newdata
}

background_error <- function(data, element.line, values=NULL, background, slope=NULL, intercept=NULL, norm.type=1, norm.min=9, norm.max=9.2, compress="100 eV", conversion=1){
    
    data <- just_spectra_summary_apply(spectra.frame=data, normalization=norm.type, min=norm.min, max=norm.max, compress=compress)

    
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
    new_tibble_list <- list(.path=unique(spectra_frame$Spectrum), .position <- 1, .spectra=list(new_tibble))
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
    deconvolution_frame <- deconvolution_frame[order(deconvolution_frame$order),]
    #elements <- deconvolution_frame$element
    #elements[1:51] <- paste0(elements[1:51], ".K.alpha")
    #elements[52:length(elements)] <- paste0(elements[52:length(elements)], ".L.alpha")
    deconvolution_t_frame <- t(deconvolution_frame[,"peak_area"])
    result_frame <- data.frame(Spectrum=name, deconvolution_t_frame)
    colnames(result_frame) <- c("Spectrum", deconvolution_frame$element)
    return(result_frame)
}

deconvolute_complete <- function(spectra_frame){
    if(is.data.frame(spectra_frame)){
        spectrum_name <- unique(spectra_frame$Spectrum)
        spectra_tibble <- tibble_convert(spectra_frame)
        deconvoluted_spectra_tibble <-spectra_tibble %>%
            xrf_add_smooth_filter(filter = xrf_filter_gaussian(width = 5), .iter = 20) %>%
            xrf_add_baseline_snip(.values = .spectra$smooth, iterations = 20) %>%
            xrf_add_deconvolution_gls(.spectra$energy_kev, .spectra$smooth - .spectra$baseline, energy_max_kev = 40, peaks = xrf_energies("everything"))
        baseline_spectra <- spectra_frame_baseline_convert(deconvoluted_spectra_tibble)
        deconvoluted_spectra <- spectra_frame_deconvolution_convert(deconvoluted_spectra_tibble)
        deconvoluted_peaks <- intensity_frame_deconvolution_convert(deconvoluted_spectra_tibble$.deconvolution_peaks[[1]], name=spectrum_name)
        return(list(Spectra=deconvoluted_spectra, Areas=deconvoluted_peaks, Baseline=baseline_spectra))
    } else if(!is.data.frame(spectra_frame)){
        NULL
    }
    
}

spectra_gls_deconvolute <- function(spectra_frame, baseline=TRUE, cores=1){
    
    spectra_list <- split(spectra_frame, spectra_frame$Spectrum)
    if(cores==1){
        new_spectra_list <- pblapply(spectra_list, deconvolute_complete)
    } else if(cores > 1){
        if(get_os()=="windows"){
            my.cluster <- parallel::makeCluster(
              cores,
              type = "PSOCK"
              )
            doParallel::registerDoParallel(cl = my.cluster)
            
            ## Pull libraries
            clusterEvalQ(cl= my.cluster, {library(tidyverse)
              library(xrftools)
            })
            
            ## Export all necessary functions to the instances
            clusterExport(my.cluster, 
                          list("as_tibble"
                               , "tibble_convert"
                               , "spectra_frame_deconvolution_convert"
                               , "deconvolute_complete"
                               , "xrf_add_deconvolution_gls"
                               , "spectra_frame_baseline_convert"
                               , "intensity_frame_deconvolution_convert"
                               , "atomic_order_vector"
                               , "atomic_order"
                               , 'fluorescence.lines'
                               , "line_strip"
                          )
            )
        } else if(get_os()!="windows"){
            my.cluster <- parallel::makeCluster(
              cores,
              type = "FORK"
              )
              doParallel::registerDoParallel(cl = my.cluster)
        }
        #new_spectra_frame <- foreach(i=1:length(spectra_list), .combine="rbind") %dopar% {
            #deconvolute_complete(spectra_list[[i]])
        #}
        new_spectra_list <- pblapply(spectra_list,  deconvolute_complete, cl=my.cluster)
        parallel::stopCluster(cl = my.cluster)
    }
    only_spectra_list <- list()
    only_areas_list <- list()
    only_background_list <- list()
    for(i in 1:length(new_spectra_list)){
        only_spectra_list[[i]] <- new_spectra_list[[i]]$Spectra
        only_areas_list[[i]] <- new_spectra_list[[i]]$Areas
        if(baseline==TRUE){only_background_list[[i]] <- new_spectra_list[[i]]$Baseline}
    }
    new_spectra_frame <- as.data.frame(rbindlist(only_spectra_list))
    new_area_frame <- as.data.frame(rbindlist(only_areas_list))
    if(baseline==TRUE){
        new_baseline_frame <- as.data.frame(rbindlist(only_background_list))
        new_area_frame$Baseline <- aggregate(CPS ~ Spectrum, data = new_baseline_frame[,c("Spectrum", "CPS")], FUN = sum)$CPS
    }
    if(baseline==FALSE){
        return(list(Spectra=new_spectra_frame, Areas=new_area_frame))
    } else if(baseline==TRUE){
        return(list(Spectra=new_spectra_frame, Areas=new_area_frame, Baseline=new_baseline_frame))
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



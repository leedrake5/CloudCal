library(shiny)
library(DT)
library(dplyr)
library(shinythemes)
library(dtplyr)
library(rhandsontable)
library(Cairo)

#Jonathan Crouch

options(warn=-1)
assign("last.warning", NULL, envir = baseenv())

ui=list(
tagList(
header=tags$head(tags$style(".table .alignRight {color: black; text-align:right;}"))),


shinyUI(navbarPage("CloudCal", id="nav", theme = shinytheme("yeti"),

tabPanel("Spectrum",
div(class="outer",
headerPanel("X-Ray Fluorescence Calibration"),
sidebarLayout(
sidebarPanel(width=3,


textInput("calname", label = "Calibration Name", value="myCalibration"),

checkboxInput('advanced', "Advanced", value=FALSE),
uiOutput('gainshiftui'),
uiOutput('binaryui'),
             
             tags$hr(),

actionButton("actionprocess", label = "Process Data"),
actionButton("actionplot", label = "Plot Spectrum"),
downloadButton('downloadPlot', "Plot"),


tags$hr(),

uiOutput('filegrab'),

selectInput("filetype", label="Filetype", c("CSV", "TXT", "Net", "Elio", "MCA", "SPX", "PDZ"), selected="Spectra"),


tags$hr(),

element <- selectInput(
"element", "Element:",
c("(Ne) Neon" = "Ne.table",
"(Na) Sodium" = "Na.table",
"(Mg) Magnesium" = "Mg.table",
"(Al) Aluminum" = "Al.table",
"(Si) Silicon" = "Si.table",
"(P)  Phosphorous" = "P.table",
"(S)  Sulfur" = "S.table",
"(Cl) Chlorine" = "Cl.table",
"(Ar) Argon" = "Ar.table",
"(K)  Potassium" = "K.table",
"(Ca) Calcium" = "Ca.table",
"(Sc) Scandium" = "Sc.table",
"(Ti) Titanium" = "Ti.table",
"(V)  Vanadium" = "V.table",
"(Cr) Chromium" = "Cr.table",
"(Mn) Manganese" = "Mn.table",
"(Fe) Iron" = "Fe.table",
"(Co) Cobalt" = "Co.table",
"(Ni) Nickel" = "Ni.table",
"(Cu) Copper" = "Cu.table",
"(Zn) Zinc"= "Zn.table",
"(Ga) Gallium" = "Ga.table",
"(Ge) Germanium" = "Ge.table",
"(As) Arsenic" = "As.table",
"(Se) Selenium" = "Se.table",
"(Br) Bromium" = "Br.table",
"(Kr) Krypton" = "Kr.table",
"(Rb) Rubidium" = "Rb.table",
"(Sr) Strontium" = "Sr.table",
"(Y)  Yttrium" = "Y.table",
"(Zr) Zirconium" = "Zr.table",
"(Nb) Niobium" = "Nb.table",
"(Mo) Molybdenum" = "Mo.table",
"(Tc) Technicium" = "Tc.table",
"(Ru) Ruthenium" = "Ru.table",
"(Rh) Rhodium" = "Rh.table",
"(Pd) Paladium" = "Pd.table",
"(Ag) Silver" = "Ag.table",
"(Cd) Cadmium" = "Cd.table",
"(In) Indium" = "In.table",
"(Sn) Tin" = "Sn.table",
"(Sb) Antimony" = "Sb.table",
"(Te) Tellerium" = "Te.table",
"(I) Iodine" = "I.table",
"(Xe) Xenon" = "Xe.table",
"(Cs) Cesium" = "Cs.table",
"(Ba) Barium" = "Ba.table",
"(Ce) Cerium" = "Ce.table",
"(Pr) Praeseodymeum" = "Pr.table",
"(Nd) Neodymeum" = "Nd.table",
"(Pr) Promethium" = "Pr.table",
"(Sm) Samarium" = "Sm.table",
"(Eu) Europium" = "Eu.table",
"(Gd) Gadolinium" = "Gd.table",
"(Tb) Terbium" = "Tb.table",
"(Dy) Dysprosium" = "Dy.table",
"(Ho) Holmium" = "Ho.table",
"(Er) Erbium" = "Er.table",
"(Tm) Thullium" = "Tm.table",
"(Yb) Ytterbium" = "Yb.table",
"(Lu) Lutetium" = "Lu.table",
"(Hf) Halfnium" = "Hf.table",
"(Ta) Tantalum" = "Ta.table",
"(W)  Tungsten" = "W.table",
"(Re) Rhenium" = "Re.table",
"(Os) Osmium" = "Os.table",
"(Ir) Irridium" = "Ir.table",
"(Pt) Platinum" = "Pt.table",
"(Au) Gold" = "Au.table",
"(Hg) Mercury" = "Hg.table",
"(Tl) Thallium" = "Tl.table",
"(Pb) Lead" = "Pb.table",
"(Bi) Bismuth" = "Bi.table",
"(Po) Polonium" = "Po.table",
"(At) Astatine" = "At.table",
"(Rn) Radon" = "Rn.table",
"(Fr) Francium" = "Fr.table",
"(Ra) Radium" = "Ra.table",
"(Ac) Actinum" = "Ac.table",
"(Th) Thorium" = "Th.table",
"(Pa) Proactinum" = "Pa.table",
"(U)  Uranium" = "U.table"),
selected="Fe.table"),

tags$hr(),
tags$hr(),
tags$hr(),


fileInput('calfileinput', 'Load Cal File', accept=".quant", multiple=FALSE),
checkboxInput('usecalfile', "Use Cal File")


),



mainPanel(
plotOutput("distPlot", height = 685,
dblclick = "plot1_dblclick",
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
)),
actionButton("cropspectra", "Zoom")
),
))
),



tabPanel("Counts",
div(class="outer",

fluidRow(
sidebarLayout(
sidebarPanel(width=3,

actionButton('linecommit', "Confirm Elements"),
downloadButton('downloadData', "Table"),

tags$hr(),

checkboxInput('usespectravalues', "Use Imported Spectra", value=FALSE),


tags$hr(),

conditionalPanel(
condition='input.dataset === dataHold()',
uiOutput('checkboxElementsKalpha'),
uiOutput('checkboxElementsKbeta'),
uiOutput('checkboxElementsLalpha'),
uiOutput('checkboxElementsLbeta'),
uiOutput('checkboxElementsM')


)),




mainPanel(
tabsetPanel(
id = 'dataset',
tabPanel('Custom Lines', rHandsontableOutput('hotline')),
tabPanel('Spectral Lines', dataTableOutput('mytable1')),
tabPanel('Covariance', plotOutput('covarianceplot')),
tabPanel("test", dataTableOutput('LineValues'))
#tabPanel('All Element Lines', uiOutput('checkboxElements'))

)

)
)

)



)),

tabPanel("Add Concentrations",
div(class="outer",

fluidRow(
sidebarLayout(
sidebarPanel(width=3,

actionButton('resethotable', "Reset"),
actionButton('hotableprocess2', "Enter Values"),
tags$hr(),
textInput("calunits", label = "Units", value="Weight %")


),


mainPanel(
tabsetPanel(
id = 'dataset',
tabPanel('Enter Concentrations', rHandsontableOutput('hot')),
tabPanel('Covariance', plotOutput('covarianceplotvalues'))

))
))
)),



tabPanel("Cal Curves",
div(class="outer",

fluidRow(
sidebarLayout(
sidebarPanel(width=3,


actionButton('createcalelement', "Update"),
actionButton('createcal', "Save"),

tags$hr(),

downloadButton('downloadModel', "Model"),
downloadButton('downloadReport', "Report"),


tags$hr(),

actionButton('trainslopes', "Train"),

tags$hr(),

#uiOutput('testing'),


uiOutput('inVar2'),

uiOutput('calTypeInput'),

uiOutput('normTypeInput'),

uiOutput('comptonMinInput'),

uiOutput('comptonMaxInput'),

uiOutput('inVar3'),
uiOutput('inVar4')
#sliderInput("nvariables", label = "# Elements", min=1, max=7, value=2)

),

mainPanel(
tabsetPanel(
tabPanel("Cal Curves",
    splitLayout(cellWidths = c("50%", "50%"),
        column(width=12,
        div(
        style = "position:relative",
        plotOutput("calcurveplots", height = 455, click = "plot_cal_click",
            dblclick = dblclickOpts(id="plot_cal_dblclick"),
            brush = brushOpts(id = "plot_cal_brush", resetOnNew = TRUE),
            hover = hoverOpts("plot_hovercal", delay = 100, delayType = "debounce")),
            uiOutput("hover_infocal")),
        actionButton("cropcal", "Zoom")),
        column(width=12,
        div(
        style = "position:relative",
        plotOutput("valcurveplots", height = 455, click = "plot_val_click",
            dblclick = "plot_val_dblclick",
            brush = brushOpts(id = "plot_val_brush", resetOnNew = TRUE),
            hover = hoverOpts("plot_hoverval", delay = 100, delayType = "debounce")),
            uiOutput("hover_infoval")),
        actionButton("cropval", "Zoom")
        )
        ),
        tags$hr(),
        actionButton("exclude_toggle", "Toggle points"),
        actionButton("exclude_reset", "Reset"),
        downloadButton('downloadcloudplot', "Plot")

),

tabPanel("Cross Validation",
    splitLayout(cellWidths = c("50%", "50%"),
        column(width=12,
        div(
        style = "position:relative",
        plotOutput("calcurveplotsrandom", height = 455,  click = "plot_cal_click_random",
            dblclick = "plot_cal_dblclick_random",
            brush = brushOpts(id = "plot_cal_brush_random", resetOnNew = TRUE),
            hover = hoverOpts("plot_hovercal_random", delay = 100, delayType = "debounce")),
            uiOutput("hover_infocal_random")),
        actionButton("cropcalrandom", "Zoom")
        ),
        column(width=12,
        div(
        style = "position:relative",
        plotOutput("valcurveplotsrandom", height = 455, click = "plot_val_click_random",
            dblclick = "plot_val_dblclick_random",
            brush = brushOpts(id = "plot_val_brush_random", resetOnNew = TRUE),
            hover = hoverOpts("plot_hoverval_random", delay = 100, delayType = "debounce")),
            uiOutput("hover_infoval_random")),
        actionButton("cropvalrandom", "Zoom")
)),
        tags$hr(),
        sliderInput('percentrandom', "Randomize", min=.01, max=.99, value=.20)

),

tabPanel("Diagnostics",
    splitLayout(cellWidths = c("50%", "50%"),
        div(
        style = "position:relative",
        plotOutput("residualsfitted", height=250, click="plot_residualsfitted_click", brush=brushOpts(id="plot_residualsfitted_brush", resetOnNew = TRUE),
            hover = hoverOpts("plot_hoverresidualsfitted", delay = 100, delayType = "debounce")),
        uiOutput("hover_inforesidualsfitted")),
        div(
        style = "position:relative",
        plotOutput("qq", height=250, click="plot_qq_click",
            brush=brushOpts(id="plot_qq_brush", resetOnNew = TRUE),
            hover = hoverOpts("plot_hoverqq", delay = 100, delayType = "debounce")),
        uiOutput("hover_infoqq"))
),
    splitLayout(cellWidths = c("50%", "50%"),
        div(
        style = "position:relative",
        plotOutput("scalelocation", height=250, click="plot_scalelocation_click", brush=brushOpts(id="plot_scalelocation_brush", resetOnNew = TRUE),
            hover = hoverOpts("plot_hoverscalelocation", delay = 100, delayType = "debounce")),
        uiOutput("hover_infoscalelocation")),
        plotOutput("cooksdistance", height=250, click="plot_cooksdistance_click", brush=brushOpts(id="plot_cooksdistance_brush", resetOnNew = TRUE))
),
    splitLayout(cellWidths = c("50%", "50%"),
        div(
        style = "position:relative",
        plotOutput("residualleverage", height=250, click="plot_residualleverage_click", brush=brushOpts(id="plot_residualleverage_brush", resetOnNew = TRUE),
            hover = hoverOpts("plot_hoverresidualleverage", delay = 100, delayType = "debounce")),
        uiOutput("hover_inforesidualleverage")),
        div(
        style = "position:relative",
        plotOutput("cooksleverage", height=250, click="plot_cooksleverage_click", brush=brushOpts(id="plot_cooksleverage_brush", resetOnNew = TRUE),
            hover = hoverOpts("plot_hovercooksleverage", delay = 100, delayType = "debounce")),
        uiOutput("hover_infocooksleverage"))
),
tags$hr(),
actionButton("exclude_toggle_diag", "Toggle points"),
actionButton("exclude_reset_diag", "Reset"),
downloadButton('diagplots', "Plot")


),

tabPanel("Variables",
    div(
    style = "position:relative",
    plotOutput('importanceplot', hover = hoverOpts('plot_hover_variable', delay = 100, delayType = "debounce")),
    uiOutput('hover_info_variable'))),
#tabPanel("test", dataTableOutput('testingagain')),

#tabPanel("Testing", dataTableOutput('testtable')),
#tabPanel("Testing2", dataTableOutput('testtable2')),



tabPanel("Standards",
tabsetPanel(
tabPanel("Validation", dataTableOutput("standardsperformance")),
tabPanel("Used", rHandsontableOutput("whichrowstokeep"))))

))


))

)),


tabPanel("Multiple Instruments",
div(class="outer",

fluidRow(
sidebarLayout(
sidebarPanel(width=3,

actionButton('actionprocess_multi', "Load Cals"),
actionButton('actionprocess2_multi', "Process Cals"),

tags$hr(),

actionButton('createcalelement_multi', "Update"),
actionButton('createcal_multi', "Save"),

tags$hr(),

downloadButton('downloadModel_multi', "Model"),
downloadButton('downloadReport_multi', "Report"),


tags$hr(),

fileInput('calfileinput_multi', 'Load Cal File', accept=".quant", multiple=TRUE),

tags$hr(),


uiOutput('defaultcalui'),

uiOutput('inVar2_multi'),

uiOutput('calTypeInput_multi'),

uiOutput('normTypeInput_multi'),

uiOutput('comptonMinInput_multi'),

uiOutput('comptonMaxInput_multi'),

uiOutput('inVar3_multi'),
uiOutput('inVar4_multi')

),

mainPanel(
tabsetPanel(
#tabPanel("Testing", dataTableOutput('tabletest')),
tabPanel("Cal Curves",
    splitLayout(cellWidths = c("50%", "50%"),
        column(width=12,
        div(
        style = "position:relative",
        plotOutput("calcurveplots_multi", height = 455, click = "plot_cal_click_multi",
            dblclick = "plot_cal_dblclick_multi",
            brush = brushOpts(id = "plot_cal_brush_multi", resetOnNew = TRUE),
            hover = hoverOpts("plot_hovercal_multi", delay = 100, delayType = "debounce")),
        uiOutput("hover_infocal_multi")),
        actionButton("cropcalmulti", "Zoom")
    ),
        column(width=12,
        div(
        style = "position:relative",
        plotOutput("valcurveplots_multi", height = 455, click = "plot_val_click_multi",
            dblclick = "plot_val_dblclick_multi",
            brush = brushOpts(id = "plot_val_brush_multi", resetOnNew = TRUE),
            hover = hoverOpts("plot_hoverval_multi", delay = 100, delayType = "debounce")),
        uiOutput("hover_infoval_multi")),
        actionButton("cropvalmulti", "Zoom")
    )),
    tags$hr(),
        actionButton("exclude_toggle_multi", "Toggle points"),
        actionButton("exclude_reset_multi", "Reset"),
        downloadButton("downloadcloudplot_multi", "Plot")
    ),
tabPanel("Cross Validation",
    splitLayout(cellWidths = c("50%", "50%"),
        column(width=12,
        div(
        style = "position:relative",
        plotOutput("calcurveplotsrandom_multi", height = 455,  click = "plot_cal_click_random_multi",
            dblclick = "plot_cal_dblclick_random_multi",
            brush = brushOpts(id = "plot_cal_brush_random_multi", resetOnNew = TRUE),
            hover = hoverOpts("plot_hovercal_random_multi", delay = 100, delayType = "debounce")),
        uiOutput("hover_infocal_random_multi")),
        actionButton("cropcalmultirandom", "Zoom")),
        column(width=12,
        div(
        style = "position:relative",
        plotOutput("valcurveplotsrandom_multi", height = 455, click = "plot_val_click_random_multi",
            dblclick = "plot_val_dblclick_random_multi",
            brush = brushOpts(id = "plot_val_brush_random_multi", resetOnNew = TRUE),
            hover = hoverOpts("plot_hoverval_random_multi", delay = 100, delayType = "debounce")),
        uiOutput("hover_infoval_random_multi")),
        actionButton("cropvalmultirandom", "Zoom"))
    ),
    tags$hr(),
        sliderInput('percentrandom_multi', "Randomize", min=.01, max=.99, value=.20),
        checkboxInput('switchmulti', "Use Cross-Validation for Report", value=FALSE),
        checkboxInput('switchrand', "Randomize by Spectrum", value=FALSE)

),

#stabPanel("testing", dataTableOutput("moretesting")),


tabPanel("Standards", dataTableOutput("standardsperformance_multi"))

))


))

)),

tabPanel("Apply Calibration",
div(class="outer",

fluidRow(
sidebarLayout(
sidebarPanel(width=3,

actionButton('processvalspectra', "Quantify"),


tags$hr(),

uiOutput('filevalgrab'),

selectInput("valfiletype", label="Filetype", c("CSV", "TXT", "Net", "Elio", "MCA", "SPX", "PDZ"), selected="Spectra"),


tags$hr(),
tags$hr(),
tags$hr(),

fileInput('calfileinput2', 'Load Cal File', accept=".quant", multiple=FALSE),
sliderInput('resultrounding', "Round Results", min=0, max=10, value=4),

downloadButton('downloadValData', "Results")

),


mainPanel(
tabsetPanel(
id = 'dataset2',
tabPanel('Validation', dataTableOutput('myvaltable2')),
tabPanel('Counts', dataTableOutput('myvaltable1'))

))
))
))

))

)









ui=list(
#useShinythemes(theme = "hover_text"),
tagList(
header=tags$head(tags$style(".table .alignRight {color: black; text-align:right;}"))),


shinyUI(navbarPage("CloudCal", id="nav", theme = shinytheme("yeti"),

tabPanel("Spectrum",
div(class="outer",
headerPanel("X-Ray Fluorescence Calibration"),
sidebarLayout(
sidebarPanel(width=3,


textInput("calname", label = "Calibration Name", value="myCalibration"),

checkboxInput('energycal', label = "Automatic Energy Calibration", value=TRUE),
#checkboxInput('advanced', "Advanced", value=FALSE),
#uiOutput('gainshiftui'),
#uiOutput('binaryui'),
             
             tags$hr(),

#actionButton("actionprocess", label = "Process Data"),
#actionButton("actionplot", label = "Plot Spectrum"),



#tags$hr(),

uiOutput('filegrab'),

uiOutput("filetypeui"),
uiOutput("pdzprepui"),
uiOutput("beamnoui"),

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
"(Pm) Promethium" = "Pm.table",
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
uiOutput("anomscatterokui"),
uiOutput("anomscatterui"),

tags$hr(),



fileInput('calfileinput', 'Load Cal File', accept=".quant", multiple=FALSE)


),



mainPanel(
tabsetPanel(
tabPanel("Spectrum",
plotOutput("distPlot", height = 500,
dblclick = "plot1_dblclick",
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
)),
tags$hr(),

splitLayout(cellWidths = c("50%", "50%"),
    column(width=12,
        div(
        style = "position:relative",
        uiOutput("first_channel"),
        uiOutput("second_channel")
        )),
    column(width=12,
        div(
        style = "position:relative",
        uiOutput("first_energy"),
        uiOutput("second_energy")
    )),
column(width=12,
    div(
    style = "position:relative",
    uiOutput("zero_energy"),
    uiOutput("max_energy")
))
),
uiOutput("en_cal_model_type_ui"),
actionButton("cropspectra", "Zoom"),
downloadButton('downloadPlot', "Plot"),
checkboxInput('showlegend', "Show Legend", value=FALSE),
uiOutput('variancespectrumui'),
tags$hr(),
selectInput('deconvolutespectra', "Deconvolution", choices=c("None", "Least Squares"), selected="None"),
selectInput("normspectra", label = "Normalization",
choices = list("Time" = 1, "Total Counts" = 2, "Compton" = 3)),
numericInput('comptonminspectra', label=h6("Min"), step=0.001, value=10, min=0, max=50, width='30%'),
numericInput('comptonmaxspectra', label=h6("Max"), step=0.001, value=10.2, min=0, max=50, width='30%'),

tags$hr(),
numericInput('plotwidth', "Download Width", step=1, value=5, min=1, max=20),
numericInput('plotheight', "Download Height", step=1, value=5, min=1, max=20)
),

tabPanel("Notes",
uiOutput('notesui')),
#tabPanel("Debug", dataTableOutput('anomtest'))
)


))
)),



tabPanel("Counts",
div(class="outer",

fluidRow(
tags$script(
'function checkifrunning() {
var is_running = $("html").attr("class").includes("shiny-busy");
if (is_running){
    $("#loading").show()
} else {
    $("#loading").hide()
}
};
setInterval(checkifrunning, 50)'
),
tags$style(
" body { text-align:left; }
#loading {
display: inline-block;
border: 3px solid #f3f3f3;
border-top: 3px solid #3498db;
border-radius: 50%;
width: 50px;
height: 50px;
animation: spin 1s ease-in-out infinite;
}
@keyframes spin {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(360deg); }
}"
),

sidebarLayout(
sidebarPanel(width=3,

actionButton('linecommit', "Confirm Elements"),
#downloadButton('downloadData', "Table"),
#downloadButton('downloadDataDeconvoluted', "Deconvoluted Table"),


tags$hr(),
uiOutput('linetypeui'),
uiOutput('linestructureui'),
uiOutput('gausbufferui'),
uiOutput('splitbufferui'),
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
tabPanel('Narrow Lines',
    dataTableOutput('mytable1'),
    tags$hr(),
    downloadButton('downloadData', "Table")),
tabPanel('Wide Lines', 
    dataTableOutput('mytable2'),
    tags$hr(),
    downloadButton('downloadWideData', "Table")),
tabPanel('Deconvoluted', 
    dataTableOutput('mytable3'),
    tags$hr(),
    downloadButton('downloadDataDeconvoluted', "Table")),
#tabPanel('Wide Deconvoluted', dataTableOutput('mytable4')),
tabPanel('Covariance',
tabsetPanel(
    tabPanel('Narrow', plotOutput('covarianceplot', height=800)),
    tabPanel('Wide', plotOutput('widecovarianceplot', height=800)),
    tabPanel('Deconvoluted', plotOutput('covarianceplotdeconvoluted', height=800))
    #tabPanel('Wide Deconvoluted', plotOutput('widecovarianceplotdeconvoluted', height=800))
),

tags$hr(),
checkboxInput('linecovarnumber', "Use Numbers", value=FALSE))
#tabPanel("test", dataTableOutput('LineValues'))
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
#actionButton('hotableprocess2', "Enter Values"),
tags$hr(),
textInput("calunits", label = "Units", value="Weight %")


),


mainPanel(
tabsetPanel(
id = 'dataset',
tabPanel('Enter Concentrations', rHandsontableOutput('hot')),
tabPanel('Covariance', plotOutput('covarianceplotvalues', height=800),
tags$hr(),
checkboxInput('conccovarnumber', "Use Numbers", value=FALSE))

))
))
)),



tabPanel("Cal Curves",
div(class="outer",

fluidRow(
sidebarLayout(
sidebarPanel(width=3,
#HTML("&nbsp;"),
#column(12, tags$div(id = "loading",
#tags$script('$("#loading").hide()'))),
tags$hr(),
#uiOutput('mclrunui'),
actionButton('createcalelement', "Update"),
#checkboxInput('modelcompression', "Shrink File Size", value=TRUE),
#actionButton('createcal', "Save"),

tags$hr(),

downloadButton('downloadModel', "Quant File"),
checkboxInput('modelcompress', label="Reduce File Size", value=TRUE),
tags$hr(),
downloadButton('downloadReport', "PDF"),
downloadButton('downloadTableReport', "Worksheet"),
#uiOutput('usecalsep'),
#uiOutput('usecalui'),

tags$hr(),

#actionButton('trainslopes', "Train"),

tags$hr(),
sliderInput("randomize", "Set Seed", min=1, max=10000, value=1, step=1),

#uiOutput('testing'),

uiOutput('inVar2'),

uiOutput('linepreferenceelementui'),

uiOutput('linestructureelementui'),

uiOutput('calTypeInput'),

uiOutput('xgbtypeui'),

uiOutput('bayesparameterui'),

uiOutput('forestmetricui'),

uiOutput('foresttrainui'),

uiOutput('cvrepeatsui'),

uiOutput('forestnumberui'),

uiOutput('foresttryui'),

uiOutput('foresttreesui'),

uiOutput('neuralhiddenlayersui'),

uiOutput('neuralhiddenunitsui'),

uiOutput('neuralweightdecayui'),

uiOutput('neuralmaxiterationsui'),

uiOutput('treemethodui'),

uiOutput('treedepthui'),

uiOutput('droptreeui'),

uiOutput('skipdropui'),

uiOutput('xgbalphaui'),

uiOutput('xgbgammaui'),

uiOutput('xgbetaui'),

uiOutput('xgblambdaui'),

uiOutput('xgbsubsampleui'),

uiOutput('xgbcolsampleui'),

uiOutput('xgbminchildui'),

uiOutput('xgbmaxdeltastepui'),

uiOutput('bartkui'),

uiOutput('bartbetaui'),

uiOutput('bartnuui'),

uiOutput('svmcui'),

uiOutput('svmdegreeui'),

uiOutput('svmscaleui'),

uiOutput('svmsigmaui'),

uiOutput('svmlengthui'),

uiOutput('normTypeInput'),

uiOutput('comptonMinInput'),

uiOutput('comptonMaxInput'),

uiOutput('dependenttransformationui'),

uiOutput('deconvolutionui'),
uiOutput('transformationui'),
uiOutput('compressui'),
uiOutput('energyrangeui'),
uiOutput('inVar3'),
uiOutput('inVar4'),
uiOutput('addallslopesui'),
uiOutput('removeallslopesui'),
uiOutput('multicore_behavior_ui'),
uiOutput('open_mp_threads_ui')

#sliderInput("nvariables", label = "# Elements", min=1, max=7, value=2)

),

mainPanel(
tabsetPanel(
#tabPanel("Testing", dataTableOutput('testingagain')),
#tabPanel("Testing2", dataTableOutput('weird')),
#tabPanel("Test", dataTableOutput("holdframetest")),
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
        actionButton("cropcal", "Zoom"),
        actionButton("zerocal", "Zero")
    ),
        column(width=12,
        div(
        style = "position:relative",
        plotOutput("valcurveplots", height = 455, click = "plot_val_click",
            dblclick = "plot_val_dblclick",
            brush = brushOpts(id = "plot_val_brush", resetOnNew = TRUE),
            hover = hoverOpts("plot_hoverval", delay = 100, delayType = "debounce")),
            uiOutput("hover_infoval")),

        actionButton("cropval", "Zoom"),
        actionButton("zeroval", "Zero")

        )
        ),
        tags$hr(),
        actionButton("exclude_toggle", "Toggle points"),
        actionButton("exclude_reset", "Reset"),
        downloadButton('downloadcloudplot', "Plot"),
        selectInput('imagesize', "Image Size", choices=c("Small", "Large"), selected="Small"),
        selectInput('plotunit', "Unit Display", choices=c("%", "ppm", "ppmv"), selected="%"),
        selectInput('loglinear', "Scale", choices=c("Linear", "Log"), selected="Linear")


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
        actionButton("cropcalrandom", "Zoom"),
        actionButton("zerocalrandom", "Zero")

        ),
        column(width=12,
        div(
        style = "position:relative",
        plotOutput("valcurveplotsrandom", height = 455, click = "plot_val_click_random",
            dblclick = "plot_val_dblclick_random",
            brush = brushOpts(id = "plot_val_brush_random", resetOnNew = TRUE),
            hover = hoverOpts("plot_hoverval_random", delay = 100, delayType = "debounce")),
            uiOutput("hover_infoval_random")),
        actionButton("cropvalrandom", "Zoom"),
        actionButton("zerovalrandom", "Zero")

)),
        tags$hr(),
        sliderInput('percentrandom', "Randomize", min=.01, max=.99, value=0.2), checkboxInput('userandom', "Use Cross-Validated Model", value=FALSE),        tags$hr(),
        downloadButton('downloadcloudplotrandom', "Plot"),
        selectInput('imagesizerandom', "Image Size", choices=c("Small", "Large"), selected="Large")

),

tabPanel("Models", dataTableOutput("models")),


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
    plotOutput('importanceplot',
        hover = hoverOpts('plot_hover_variable', delay = 100, delayType = "debounce"),
        brush = brushOpts(id = 'plot_var_brush', resetOnNew = TRUE), height=500),
    uiOutput('hover_info_variable')),
    tags$hr(),
    actionButton("cropvar", "Zoom"),
    downloadButton("variablePlot", "Plot"),
    uiOutput('varelementui')),

#tabPanel("test", dataTableOutput('holdframetest')),

#tabPanel("Testing", dataTableOutput('testtable')),
#tabPanel("Testing2", dataTableOutput('testtable2')),



tabPanel("Standards",
tabsetPanel(
tabPanel("Validation", dataTableOutput("standardsperformance"), tags$hr(), downloadButton("downloadStandards")),
tabPanel("Used", rHandsontableOutput("whichrowstokeep")))),

tabPanel("Calibration Progress",
dataTableOutput('caliibrationprogresstable'))

))


))

)),


tabPanel("Apply Calibration",
div(class="outer",

fluidRow(
sidebarLayout(
sidebarPanel(width=3,

actionButton('processvalspectra', "Quantify"),
numericInput("multiplier", "Multiply Values By", min=1, max=10000, value=1),
#checkboxInput("error", "Include Error", value=TRUE),
selectInput("error", "Uncertainty", choices=c("None", "eqm", "y hat"), selected="None"),
checkboxInput("se_error", "Use 95% bounds", value=FALSE),
uiOutput('fanowindowui'),

tags$hr(),

uiOutput('filevalgrab'),
uiOutput("pdzprepvalui"),

uiOutput('valfiletypeui'),



tags$hr(),

fileInput('calfileinput2', 'Load Cal File', accept=".quant", multiple=FALSE),
uiOutput('roundingui')



),


mainPanel(
tabsetPanel(
id = 'dataset2',
#tabPanel("Testing", dataTableOutput('testingvalstuff'), downloadButton('downloadTestStuff', "Test")),
tabPanel('Quantification Results', dataTableOutput('myvaltable2'),
tags$hr(),


textInput('quantifiedname', label=NULL, placeholder="Data Label"),
downloadButton('downloadValData', "Results")
),
tabPanel('Counts',
tabsetPanel(
tabPanel('Narrow', dataTableOutput('myvaltable1'), downloadButton('downloadValDataNarrow', "Download")),
tabPanel('Wide', dataTableOutput('myvaltablewide'), downloadButton('downloadValDataWide', "Download")),
tabPanel('Deconvoluted', dataTableOutput('myvaltabledeconvoluted'), downloadButton('downloadValDataDeconvoluted', "Download"))
#tabPanel('Wide Deconvoluted', dataTableOutput('myvaltablewidedeconvoluted'))
),uiOutput('rounding2ui')
)

))
))
))

))

)

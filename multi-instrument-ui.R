
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

uiOutput('forestmetricui_multi'),

uiOutput('foresttrainui_multi'),

uiOutput('forestnumberui_multi'),

uiOutput('foresttreesui_multi'),

uiOutput('normTypeInput_multi'),

uiOutput('comptonMinInput_multi'),

uiOutput('comptonMaxInput_multi'),

uiOutput('inVar3_multi'),
uiOutput('inVar4_multi'),
uiOutput('parallelmethodui')

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
        actionButton("cropcalmulti", "Zoom"),
        actionButton("zerocalmulti", "Zero")

    ),
        column(width=12,
        div(
        style = "position:relative",
        plotOutput("valcurveplots_multi", height = 455, click = "plot_val_click_multi",
            dblclick = "plot_val_dblclick_multi",
            brush = brushOpts(id = "plot_val_brush_multi", resetOnNew = TRUE),
            hover = hoverOpts("plot_hoverval_multi", delay = 100, delayType = "debounce")),
        uiOutput("hover_infoval_multi")),
        actionButton("cropvalmulti", "Zoom"),
        actionButton("zerovalmulti", "Zero")

    )),
    tags$hr(),
        actionButton("exclude_toggle_multi", "Toggle points"),
        actionButton("exclude_reset_multi", "Reset"),
        downloadButton("downloadcloudplot_multi", "Plot"),
        selectInput('imagesize_multi', "Image Size", choices=c("Small", "Large"), selected="Large")

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
        actionButton("cropcalmultirandom", "Zoom"),
        actionButton("zerocalmultirandom", "Zero")),
        column(width=12,
        div(
        style = "position:relative",
        plotOutput("valcurveplotsrandom_multi", height = 455, click = "plot_val_click_random_multi",
            dblclick = "plot_val_dblclick_random_multi",
            brush = brushOpts(id = "plot_val_brush_random_multi", resetOnNew = TRUE),
            hover = hoverOpts("plot_hoverval_random_multi", delay = 100, delayType = "debounce")),
        uiOutput("hover_infoval_random_multi")),
        actionButton("cropvalmultirandom", "Zoom"),
        actionButton("zerovalmultirandom", "Zero"))
    ),
    tags$hr(),
        sliderInput('percentrandom_multi', "Randomize", min=.01, max=.99, value=.33),
        checkboxInput('switchmulti', "Use Cross-Validation for Report", value=FALSE),
        checkboxInput('switchrand', "Randomize by Spectrum", value=FALSE),
        downloadButton("downloadcloudplot_multi_val", "Plot"),
        selectInput('plotunitmulti', "Unit Display", choices=c("%", "ppm", "ppmv"), selected="%")



),

tabPanel("Variables",
div(
style = "position:relative",
plotOutput('importanceplot_multi',
hover = hoverOpts('plot_hover_variable_multi', delay = 100, delayType = "debounce"),
brush = brushOpts(id = 'plot_var_brush_multi', resetOnNew = TRUE), height=500),
uiOutput('hover_info_variable_multi')),
tags$hr(),
actionButton("cropvar_multi", "Zoom"),
downloadButton("variablePlot_multi", "Plot"),
uiOutput('varelementui_multi')),

#stabPanel("testing", dataTableOutput("moretesting")),


tabPanel("Standards", dataTableOutput("standardsperformance_multi"))

))


))

)),

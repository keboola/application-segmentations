
library("keboola.shiny.lib")

# UI.R -- shiny frontend
shinyUI(
    keboolaPage(
        fluidPage(
            column(9,
               tabsetPanel(type = "tabs", id = 'tabsPanel', 
                   tabPanel(
                       "Description",
                       numericInput("minSplit", "Minimum items in node", 0),
                       p("When you increase the minimum number of items in node, you
                         force the tree to create leaves with representing more items (table rows).
                         This leads to more concise and less precise tree."),
                       uiOutput("description"),
                       h2("List of all rules"),
                       p("The table lists all rules used in the decision tree. Rules are sorted from top to bottom, from left to right."),
                       uiOutput("rules")
                   ),
                   tabPanel(
                       "Segmented data",
                       uiOutput('segmentsTable')
                   )
               )
            )
        ), appTitle = "Segmentation"
    )
)
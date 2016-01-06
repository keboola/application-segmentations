# Load libraries
library(shiny)
library(rpart)
library(rpart.plot)

# Load keboola library
library(keboola.shiny.lib)

shinyServer(function(input, output, session) {
    # instantiate our keboola library
    klib <- KeboolaShiny$new()

    keboola <- reactive({
        # The tables this application needs from it's SAPI bucket
        tables <- list(
            cleanData = list(name="SEG__1"),
            rules = list(name="RUL__1"),
            columnTypes = list(name="VAI__1", reducible=FALSE),
            finalResults = list(name="finalResults", reducible=FALSE)
        )
        
        # Start it up
        ret <- klib$startup(list(appTitle = "Segmentation",
                                 tables = tables,
                                 cleanData = TRUE,
                                 dataToSave = rulesData,
                                 configCallback = configCallback,
                                 description = TRUE,
                                 customElements = customElements))
        
        # return our retrieved sourceData
        klib$sourceData()()
        
    })
    
    # here we can do any pre-processing if the data isn't in the shape we need
    sourceData <- reactive({
        dataSet <- keboola()
        if (length(dataSet) == 0) {
            # the startup hasn't completed or we're not authenticated
            NULL
        } else {
            # change cleaned data table name to 'segments'
            dataSet$segments <- dataSet$cleanData
            dataSet$cleanData <- NULL
            
            # remove the run_id from the table if it is there
            dataSet$segments <- dataSet$segments[,!names(dataSet$segments) %in% c("run_id")]
            # Segment column is factor (and is not included in getCleanData() because it is computed)
            dataSet$segments$class <- as.factor(dataSet$segments$class)
            excludedColumns <- dataSet$finalResults[which(dataSet$finalResults$item == 'excludedColumns'), 'value']
            dataSet$excludedColumns <- unlist(strsplit(excludedColumns[1], ','))
            # data set prepared, return it
            dataSet
        }
    })    
    
    # we observe for when sourceData() is altered to update any inputs accordingly
    observe({
        dataSet <- sourceData()
        if (is.null(dataSet$segments)) {
            NULL
        } else {
            # update input
            updateNumericInput(session, 'minSplit', value = round(nrow(dataSet$segments) / 100))
            TRUE
        }
    })
        
    # this method is called when a user loads a previously saved app configuration.
    # update the inputs with the saved values
    configCallback <- function(session, config) {
        updateNumericInput(session, 'minSplit', value = config$minSplit)
    }
    
    #   Render custom elements in description. Use this method to handle
    #   rendering of plots and other custom elements in descriptor. This method
    #   is used as a callback in getDescription(), it is not called directly.
    customElements <- function(elementId, content) {
        print(paste0("custom elements", elementId))
        if (elementId == 'decisionTree') {
            ret <- treePlot(elementId)
        }
        return(ret)
    }

    # Render table with actual segmented data
    output$segmentsTable <- renderUI({
        print("segmentsTable")        
        return(list(DT::datatable(sourceData()$segments)))
    })
    
    # perform some minor re-formatting of the rules table
    rulesData <- reactive({
        rules <- sourceData()$rules
        rules$row_num <- as.integer(rules$row_num)
        rules <- rules[order(rules$row_num), ]
        rules <- rules[, c("var", "splitoperation", "splitvalue", "splitvalueopposite", "yval", "n")]
        names(rules) <- c("column", "operation", "value", "oppositeValue", "class", "numberOfItems")
        rownames(rules) <- 1:nrow(rules)
        rules
    })
    
    # create a datatable of our rules data
    output$rules <- renderUI({
        return(list(DT::datatable(rulesData())))
    })
    
    # Helper function to print lables of a decission tree split
    split.fun <- function(x, labs, digits, varlen, faclen)
    {
        ds <- sourceData()$rules
        for(i in 1:length(labs)) {
            # split labs[i] into original values
            values <- strsplit(labs[i], split = ",")[[1]]
            if (length(values) > 3) {
                # if there are more values, print first two and then the number of values    
                labs[i] <- paste(substr(values[1], 0, 30), ",\n", substr(values[2], 0, 30), " + ", length(values) - 2, " values.")
            } else { 
                # otherwise print the oringal string (croped in case the values are too long)
                labs[i] <- paste(substr(labs[i], 0, 30))
            }
        }
        labs
    }

    
    # Helper function to print node lables of a decission tree
    node.fun <- function(x, labs, digits, varlen)
    {
        cntClasses <- length(unique(x$frame$yval))
        prob <- vector(length = nrow(x$frame))
        for (i in 1:nrow(x$frame$yval2)) {
            prob[i] <- x$frame$yval2[i, 1 + cntClasses + x$frame$yval[i]]
        }
        paste("class: ", x$frame$yval, "\nitems: ", x$frame$n, "\nprob: ", round(prob, 2))
    }
    
    descendants <- function(nodes, include = TRUE)
    {
        n <- length(nodes)
        if (n == 1L) return(matrix(TRUE, 1L, 1L))
        ind <- 1:n
        desc <- matrix(FALSE, n, n)
        if (include) diag(desc) <- TRUE
        parents <- match((nodes %/% 2L), nodes)
        lev <- floor(log(nodes, base = 2))
        desc[1L, 2L:n] <- TRUE
        for (i in max(lev):2L) {
            desc[cbind(ind[parents[lev == i]], ind[lev == i])] <- TRUE
            parents[lev == i] <- parents[parents[lev == i]]
            lev[lev == i] <- i - 1L
        }
        desc
    }

    # Render decission tree 
    treePlot <- function(elementId) {
        print("treePlot")
        data <- sourceData()$segments
        excludedColumns <- sourceData()$excludedColumns
        # remove excluded columns
        data <- data[ , -which(names(data) %in% excludedColumns)]
        # make a classfification tree to determine the class
        treedata <- rpart(class ~ ., data = data, method = "class", minsplit = input$minSplit)
        outfile <- tempfile(fileext = ".png")
        png(outfile, width = 1100, height = 800)
        prp(treedata, type = 3, extra = 1, split.fun = split.fun, node.fun = node.fun, cex = 1.0)
        dev.off()
        on.exit(unlink(outfile))

        img(src = session$fileUrl(elementId, outfile, contentType='image/png'))
    }
})

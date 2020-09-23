#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)
library(bsplus)


logisPlot <- function( glm_obj, which_var, wh_range = c(-3,3), others = NULL, 
                       std_obj = NULL, standardize = FALSE, res = 100, se = TRUE ){
  require( ggplot2 )
    # Create a plot that shows the response on the probability scale.
    # `which_var` should be the name of the covariate to vary
    # `wh_range` is the range over which to vary the covariate
    # `others` is the values at which to set the remaining covariates,
    #    as a single-row data.frame.
    #    If `others` is not specified, remaining covariates will be
    #    set to their mean values.
    # std_obj is a data.frame with columns "covariate, "mean" and "sd", 
    #   and row.names matching the covariate names. If provided,
    #   this will be used to transform the plotting axis back
    #   to the original scale of the data.
    # if standardize is TRUE, then backtransform the predictors
    #   prior to calculating predictions.

    if( is.null( others ) ){
        # Determine mean values
        mf <- model.frame( glm_obj )
        
        others <- as.data.frame(
            lapply( model.frame( glm_obj )[,-1], function(x){
            if( class(x) == "numeric" ) mean(x, na.rm = TRUE)
            else levels(x)[1]
            }) )
    }

    # Create new.data object
    new_data <- as.data.frame( lapply( others, rep, length.out = res ) )
    new_data[[which_var]] <- seq( wh_range[1], wh_range[2], length.out = res )
    
    # Back-transform if necessary
    if( standardize ){
      if( is.null( std_obj ) ) stop( "With standardize, std_obj is required!" )
      for( nm in names(new_data) ){
        if( is.numeric( new_data[,nm] ) ){
          new_data[,nm] <- (new_data[,nm] - std_obj[ std_obj$covariate == nm, "mean" ])/
                              std_obj[ std_obj$covariate == nm, "sd" ]
        }
      }
    }
    
    # Get predicted values
    Xb <- predict( glm_obj, newdata = new_data, se.fit = TRUE )
    new_data$pp <- plogis( Xb$fit )
    new_data$pp_l <- plogis( Xb$fit - 2*Xb$se.fit )
    new_data$pp_u <- plogis( Xb$fit + 2*Xb$se.fit )
    # Get transformed x-values
    if( !is.null( std_obj ) ){
      mu <- std_obj[ std_obj$covariate == which_var, "mean" ]
      sig <- std_obj[ std_obj$covariate == which_var, "sd" ]
      new_data[,which_var] <- new_data[,which_var]*sig + mu
    }
    
out <- ggplot( new_data, aes_( x = as.name(which_var), y = ~pp ) )
if( se ) out <- out + geom_ribbon( aes( ymin = pp_l, ymax = pp_u ), alpha = 0.5 )
out <- out + geom_line() + ylab( "probability" ) + ylim( c(0,1) )
        
out    
}



header <- dashboardHeader(title = "Logistic Explorer")

sidebar <- dashboardSidebar(
    # First, the user must select a logistic regression fit
    #  that has been saved to a .rds file
    # Then, the user can optionally provide mean and sd for
    #  each covariate stored in another .rds file, which will
    #  transform displays back to the original scale.
    h3( "Start here!" ),
    fileInput("file1", "Choose RDS file with glm logistic fit", accept = ".rds") %>%
      bs_embed_tooltip(title = "After you fit a logistic regression with glm(), 
              save it to disk using saveRDS() and select that file here.",
              placement = "bottom" ),
    fileInput("file2", "Optional: Choose RDS file with mean, sd of covariates", accept = ".rds") %>%
      bs_embed_tooltip(title = "If you have standardized the inputs to the logistic regression by
              subtracting the mean and dividing by the standard deviation, create a
              data.frame with three columns: `covariate` with the covariate names
              (must match the names in the glm object exactly);
              `mean` with the mean of each covariate; 
              and `sd` with the standard deviation of each covariate.
              Save the data.frame to disk using saveRDS(), and 
              select that file here. Covariate values will then be
              back-transformed to their original scale.",
              placement = "bottom" )
)

body <- dashboardBody(
  use_bs_tooltip(),
  fluidRow(
    box(
        h4("Covariate of interest (x-axis)") %>%
          bs_embed_tooltip( title = "This covariate will appear as the x-axis on the plot.",
                            placement = "bottom" ),
        div(style = 'overflow:auto;height:600px;',
            uiOutput( "radios" )
        ),
        width = 3
      ),
    box(
        h4("Values of other Covariates") %>%
          bs_embed_tooltip( title = "Changing the values of these covariates will
                                affect the probability on the plot.
                                By default, sliders are set to the mean value of each
                                covariate, and radio buttons are set to the baseline
                                value of each factor.",
                            placement = "bottom" ),
        div(style = 'overflow:auto;height:600px;',
            uiOutput( "sliders" ) 
        ),
        width = 3
      ),
    box(
      plotlyOutput( "plot1" ),
      width = 6,
      height = "600px"
    )
  )
)


ui <- dashboardPage( header, sidebar, body )

# Define server logic required to draw a histogram
server <- function(input, output) {
    # When everything has been loaded, sliders for each continuous
    #  covariate will be displayed, and radio buttons for each
    #  discrete covariate will be displayed
    # Another set of radio buttons will select the x-axis for the
    #  plot, with the y-axis being Pr(y=1)
    
    glm_obj <- reactive( readRDS( input$file1$datapath ) )
    mf <- reactive( model.frame( glm_obj() )[,-1] )
    glm_vars <- reactive( names( mf() ) )
    glm_vars_range <- reactive({
      if( !is.null( input$file2 ) ){
        mf_this <- mf_d()
      }else{
        mf_this <- mf()
      }
      lapply( mf_this, function(x){
        if( is.factor(x) ){
          out <- levels(x)
        }else{
          out <- c( range(x), mean(x) )
        }
        out
      })
    })
    
    var_focal <- reactive( input$whatvar )
    
    std_obj <- reactive({
        if( !is.null( input$file2 ) ){
          readRDS( input$file2$datapath )
        }else{
          NULL
        }
      })
    
    # Create a de-standardized version of the mf
    mf_d <- reactive({
      xx <- lapply( glm_vars(), function(nm, std, mfs){
                  # de-standardize columns in mfs using std: mean, sd
                  if( is.numeric( mfs[[nm]] ) ){
                    mfs[[nm]]*std[std$covariate == nm, "sd"] + std[std$covariate == nm, "mean"]
                  }else{
                    mfs[[nm]]
                  }
                }, std = std_obj(), mfs = mf() )
      xx <- as.data.frame(xx)
      names(xx) <- glm_vars()
      xx
    })
    
    # Render the radio buttons
    output$radios <- renderUI({
      validate(
        need( input$file1, "Please load an RDS file containing the glm fit" )
      )
      tagList(
        radioButtons( "whatvar", "",
          choices = glm_vars()[ which( !sapply( glm_vars_range(), is.character ) ) ], 
          selected = glm_vars()[ min( which( !sapply( glm_vars_range(), is.character ) ) ) ] )
      )
    })
    
    # Render the sliders
    output$sliders <- renderUI({
      validate(
        need( input$file1, "Please load an RDS file containing the glm fit" )
      )
      # Create a list of sliders
      # Omit the x-axis variable from the slider list
      glm_vars_no_focal <- glm_vars()[ glm_vars() != var_focal() ]
      glm_vars_range_no_focal <- glm_vars_range()[ glm_vars_no_focal ]
      # Create the slider list, with radio buttons for factors
      sliders <- lapply(1:length(glm_vars_no_focal), function(i, glm_vars, glm_vars_range) {
        inputName <- glm_vars[i]
        if( is.character( glm_vars_range[[i]] ) ){
          # This is radio buttons for factors
          return( radioButtons( inputName, inputName, choices = glm_vars_range[[i]] ) )
        }
        sliderInput( inputName, inputName, min=signif(glm_vars_range[[i]][1],3), 
                     max=signif(glm_vars_range[[i]][2],3), value=glm_vars_range[[i]][3] )
      }, glm_vars = glm_vars_no_focal, glm_vars_range = glm_vars_range_no_focal )
      do.call(tagList, sliders)
    })
    

    # Create plot from inputs...

    output$plot1 <- renderPlotly({
      validate(
        need( input$file1, "Please load an RDS file containing the glm fit" ),
        need( input$whatvar, label = "whatvar" )
      )
      nm <- glm_vars()
      validate(
        need( input[[nm[1]]], label = "covariates" )
      )
      nm <- nm[ nm != input$whatvar ]
      vals <- lapply( nm, function(x) input[[x]] )
      vals <- as.data.frame( vals )
      names(vals) <- nm
      
      # mean value of the variable of interest
      mean_x <- glm_vars_range()[[var_focal()]][3]
      
      logisPlot( glm_obj = glm_obj(), 
                 which_var = input$whatvar, 
                 wh_range = glm_vars_range()[[ input$whatvar ]][1:2], 
                 others = vals,
                 std_obj = std_obj(),
                 standardize = !is.null( std_obj() ) ) + 
        geom_vline( xintercept = mean_x, linetype = "dashed" ) +
        theme_bw()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

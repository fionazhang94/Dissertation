library(shinyjs)
library(shinyWidgets)
library(visreg)
library(tidyverse)
library(magrittr)
library(bootstrap)
library(foreach)
library(doParallel)

# Prepare Data
air <- as_tibble(airquality[, 1:4]) %>% 
  na.omit()

# Define UI for application
ui <- fluidPage(
  titlePanel("Uncertainty of Regression Model"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "reg_type", "Regression Type",
        choices = c("simple", "multiple")
      ),
      sliderInput(
        "num_boot", "Number of bootstrapped lines",
        min = 1, max = 200, value = 50
      )
    ),
    mainPanel(
      uiOutput("plot_ui")
    )
  )
)


# Define server logic required to draw a histogram
server <- # server function
  function(input, output, session) {
    # split the output for two parts:
    # 1. simple variable
    # 2. multiple variable
    
    plot_result <- reactive({
      if (input$reg_type == "simple") {
        base_plot <- air %>% 
          gather(key = "variable", value = "value", -Ozone) %>% 
          dplyr::filter(variable == "Solar.R") %>% 
          ggplot(aes(x = value, y = Ozone)) +
          facet_wrap(~variable, scales = "free") +
          geom_point()
        
        f2 <- function(selected){
          tb <- tibble()
          for (the_name in names(air)[2]) {
            lm_fit <- lm(Ozone ~ ., data = air[selected, c("Ozone", the_name)])
            tmp <- lm_fit$coef
            tmp_tb <- tibble(intercept = tmp[1], slope = tmp[2], variable = the_name)
            tb <- bind_rows(tb, tmp_tb)
          }
          tb
        }
        
        # boostrap
        n_draws <- input$num_boot
        bootvals <- bootstrap(1:nrow(air), n_draws, f2)
        
        fits <- bootvals$thetastar %>% reduce(bind_rows)
        
        return_plot <- base_plot +
          geom_abline(aes(intercept = intercept, slope = slope),
                      data = fits, color = "grey60", alpha = .15) +
          geom_smooth(method = "lm", se = F) +
          labs(x = "Scatter Plot")
        
        return(return_plot)
      }
      
      if (input$reg_type == "multiple") {
        # --- Base model
        fit2 <- lm(Ozone ~ Solar.R + Wind + Temp, data = air)
        
        # base model result
        a <- visreg(fit2, plot = F)
        # check the help page of visreg function. Here a is list of length 3, corresponds to Solar.R, Wind, and Temp variable
        
        # assign names for later usage
        names_a <- a %>% map(~ .$meta$x) %>% unlist()
        names(a) <- names_a
        
        # fitted values
        a1 <- a %>% 
          map(~ .$fit) %>% 
          map(as_tibble)
        
        dat1_base <- tibble()
        for (i in seq_along(names_a)) {
          tmp <- a1[[names_a[i]]] %>% 
            select(value = names_a[i], Ozone = visregFit) %>% 
            mutate(variable = names_a[i])
          dat1_base <- bind_rows(dat1_base, tmp)
        }
        
        # partial residuals
        a2 <- a %>% 
          map(~ .$res) %>% 
          map(as_tibble)
        
        dat2_base <- tibble()
        for (i in seq_along(names_a)) {
          tmp <- a2[[names_a[i]]] %>% 
            select(value = names_a[i], Ozone = visregRes) %>% 
            mutate(variable = names_a[i])
          dat2_base <- bind_rows(dat2_base, tmp)
        }
        
        # partial residual plot as base plot, 
        # and add bootstraped fitted lines to it.
        gg_base <- dat2_base %>%
          ggplot(aes(x = value, y = Ozone)) +
          geom_point() +
          facet_wrap(~variable, scales = "free")
        # gg_base
        
        ####  bootstrap step
        # function used in bootstrap 
        f2 <- function(selected){
          dat <- air[selected, ]
          lm(Ozone ~ ., data = dat)
          # returns a lm object
        }
        
        # boostrap
        n_draws <- input$num_boot
        bootvals_2 <- bootstrap(1:nrow(air), n_draws, f2)
        
        # f3 takes a lm object and return the intercept and slopes for partial residual plots
        f3 <- function(fit) {
          dat <- fit$model
          
          a <- visreg(fit, plot = F)
          names_a <- unlist(map(a, function(y) y$meta$x))
          names(a) <- names_a
          a1 <- map(a, function(x) x$fit)
          
          # fitted values
          dat1 <- NULL
          for (i in seq_along(names_a)) {
            tmp <- select(a1[[names_a[i]]], value = names_a[i], Ozone = visregFit)
            # calculate intercept
            slope <- diff(tmp$Ozone[1:2])/diff(tmp$value[1:2])
            intercept <- tmp$Ozone[1] - tmp$value[1]*slope
            
            tmp2 <- data.frame(a = intercept, b = slope, variable = names_a[i])
            dat1 <- rbind(dat1, tmp2)
          }
          # only return intercept and slopes
          dat1
        }
        
        # iterate f3 over bootstrapped values to gather fitted lines: 
        # Use parallel to faster the running time
        registerDoParallel(detectCores())#detectcores
        result <- foreach(i = 1:length(bootvals_2[[1]]), .combine = rbind, .packages = c("visreg", "magrittr", "tidyverse")) %dopar% {
          fit <- bootvals_2$thetastar[[i]]
          f3(fit)
        }
        stopImplicitCluster()
        
        return_plot <- gg_base +
          geom_abline(aes(intercept = a, slope = b), data = result, color = "grey60", alpha = 0.15) +
          geom_line(aes(x = value, y = Ozone), data = dat1_base, color = "blue", lwd = 1) +
          labs(x = "Partial Residuals Plot")
        
        return(return_plot)
      }
    })
    
    output$plot <- renderPlot({
      plot_result()
    })
    
    output$plot_ui <- renderUI({
      if (input$reg_type == "simple") {
        plot_width = "500px"
      }
      if (input$reg_type == "multiple") {
        plot_width = "auto"
      }
      plotOutput("plot", width = plot_width)
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)


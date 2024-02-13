library(shiny)
library(tidyverse)
library(data.table)
library(ggpubr)
library(bslib)
library(RColorBrewer)

light <- bs_theme(bootswatch = "minty")
dark <- bslib::bs_theme(
  bg = "#101010",
  fg = "#FDF7F7",
  primary = "#ED79F9",
  secondary = "#1e90ff"
)


##########################################################################################
##########################################################################################
# UI

ui <- fluidPage(
  theme = light,
  titlePanel(
    h1("Cross-sectional topological dynamics across the lifespan"),
    windowTitle = "Probabilistic analysis - Shiny App"
  ),
  p(""),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      checkboxInput("dark_mode", "Dark mode", value = FALSE),
      p("Check", em(a("github.com/lpnc-lang/SENECA", href = "https://github.com/lpnc-lang/SENECA"))),
      checkboxGroupInput("subsystem_choice",
                         h4("Pick at least one subsystem"),
                         choices = list(
                           "Associative" = "1",
                           "Sensorimotor" = "2",
                           "Bottom-up attentional" = "3",
                           "Top-down control-executive" = "4"
                         )
      ),
      checkboxGroupInput("RSN_choice",
                         h4("Pick at least one resting-state network"),
                         choices = list(
                          "Auditory" = "Auditory",
                          "CON" = "CON",
                          "DAN" = "DAN",
                          "DMN" = "DMN",
                          "FPN" = "FPN",
                          "Language" = "Language",
                          "SMN" = "SMN",
                          "PMM" = "PMM",
                          "VMM" = "VMM",
                          "Visual_1" = "Visual_1",
                          "Visual_2" = "Visual_2"
                        )
      ),
      # Input: Decimal interval with step value ----
      sliderInput("decimal", "Neuro-variability (e.g., 0.05 will also plot the trajectories which are within a 5% range of the most probable trajectory for each region)",
        min = 0, max = 0.25,
        value = 0.05, step = 0.01
      ),
      actionButton("button", "Plot trajectory", width = 450, style = "height:50px; border-radius:40px; font-weight:600; font-size: 20px"),
    ),
    mainPanel(
      h3(textOutput("select_var"), align = "center"),
      shinycssloaders::withSpinner(
        plotOutput("plot", height = 650),
        hide.ui = FALSE
      )
    )
  )
)

##########################################################################################
##########################################################################################
# SERVER

# Define server logic ----
server <- function(input, output, session) {
  
  # Even reactive buttons ----
  observe(session$setCurrentTheme(
    if (isTRUE(input$dark_mode)) dark else light
  ))
  

  plot_button_subsystem <- eventReactive(input$button, {
    input$subsystem_choice
  })
  
  plot_button_RSN <- eventReactive(input$button, {
    input$RSN_choice
  })

  sliderValue <- eventReactive(input$button, {
    input$decimal
  })

  
  
  
  # Text output above plot ----
  output$select_var <- renderText({
    # When selecting multiple subsystem
    if (length(plot_button_subsystem()) > 1) {
      list_input_subsystem <- list()
      for (i in 1:length(plot_button_subsystem())) {
        list_input_subsystem[[i]] <- plot_button_subsystem()[i]
        subsystem_label_df <- rbindlist(lapply(list_input_subsystem, as.data.table))
        subsystem_label_df <- subsystem_label_df %>% as.data.frame() %>% 
          mutate(V1 = ifelse(V1 == 1, "Associative", ifelse(V1 == 3, "Bottom-up attentional", 
                                                            ifelse(V1 == 2, "Sensorimotor",
                                                            "Top-down control-executive"))))
        subsystem_label <- capture.output(cat(subsystem_label_df %>% as.matrix(), sep = ", "))
      }
      # When selecting multiple RSN
      if (length(plot_button_RSN()) > 1) {
        list_input_RSN <- list()
        for (i in 1:length(plot_button_RSN())) {
          list_input_RSN[[i]] <- plot_button_RSN()[i]
          RSN_label_df <- rbindlist(lapply(list_input_RSN, as.data.table))
          RSN_label <- capture.output(cat(RSN_label_df %>% as.matrix(), sep = "|"))
        }
        paste("This is the most probable trajectory for", RSN_label, "regions of the", subsystem_label, "subsystems")
        
        # When selecting one RSN
      } else if (length(plot_button_RSN()) == 1) {
        list_input_RSN <- list()
        for (i in 1:length(plot_button_RSN())) {
          list_input_RSN[[i]] <- plot_button_RSN()[i]
          RSN_label_df <- rbindlist(lapply(list_input_RSN, as.data.table))
          RSN_label <- capture.output(cat(RSN_label_df %>% as.matrix(), sep = "|"))
        }
        paste("This is the most probable trajectory for", RSN_label, "regions of the", subsystem_label, "subsystems")
      }
      
      
      # When selecting one subsystem
    } else if (length(plot_button_subsystem()) == 1) {
      list_input_subsystem <- list()
      for (i in 1:length(plot_button_subsystem())) {
        list_input_subsystem[[i]] <- plot_button_subsystem()[i]
        subsystem_label_df <- rbindlist(lapply(list_input_subsystem, as.data.table))
        subsystem_label_df <- subsystem_label_df %>% as.data.frame() %>% 
          mutate(V1 = ifelse(V1 == 1, "Associative", ifelse(V1 == 3, "Bottom-up attentional", 
                                                            ifelse(V1 == 2, "Sensorimotor",
                                                                   "Top-down control-executive"))))
        subsystem_label <- capture.output(cat(subsystem_label_df %>% as.matrix(), sep = ", "))
      }
      # When selecting multiple RSN
      if (length(plot_button_RSN()) > 1) {
        list_input_RSN <- list()
        for (i in 1:length(plot_button_RSN())) {
          list_input_RSN[[i]] <- plot_button_RSN()[i]
          RSN_label_df <- rbindlist(lapply(list_input_RSN, as.data.table))
          RSN_label <- capture.output(cat(RSN_label_df %>% as.matrix(), sep = "|"))
        }
        paste("This is the most probable trajectory for", RSN_label, "regions of the", subsystem_label, "subsystem")
        
        # When selecting one RSN
      } else if (length(plot_button_RSN()) == 1) {
        list_input_RSN <- list()
        for (i in 1:length(plot_button_RSN())) {
          list_input_RSN[[i]] <- plot_button_RSN()[i]
          RSN_label_df <- rbindlist(lapply(list_input_RSN, as.data.table))
          RSN_label <- capture.output(cat(RSN_label_df %>% as.matrix(), sep = "|"))
        }
        paste("This is the most probable trajectory for", RSN_label, "regions of the", subsystem_label, "subsystem")
      }
    }
  })

  # Load datasets ----
  output$plot <- renderPlot({
    dataset <- read.csv("probabilistic_analysis.csv") %>%
      dplyr::select(-X) %>%
      plyr::rename(c("X1st_network" = "1st_network"))

    ############################################################################
    ############################################################################
    ############################################################################
    
    # FUNCTION for plotting the trajectory ---- 
    trajectory <- function(list_community, list_RSN, threshold, AGE_MIDDLE, AGE_OLD) {
      # Get the associated Resting-state networks for all age groups
      # Hub region specific to each subject yielded by hub detection procedure
      ##############################################################################
      modular <<- dataset
      modular$Age_group <- factor(modular$Age_group, levels = c("Young", "Middle", "Old"))
      
      if (list_community != "All" & list_RSN != "All") {
        Cond_PMF <- modular %>%
          filter(grepl(list_community, Consensus_vector_0.15)) %>%
          filter(grepl(list_RSN, `1st_network`)) %>%
          dplyr::select(-`1st_network`) %>% 
          spread(Age_group, freq) %>%
          arrange(MODULAR) %>%
          group_by(Region) %>%
          group_split()
      } else if (list_community != "All" & list_RSN == "All") {
        Cond_PMF <- modular %>%
          filter(grepl(list_community, Consensus_vector_0.15)) %>%
          dplyr::select(-`1st_network`) %>%
          spread(Age_group, freq) %>%
          arrange(MODULAR) %>% 
          group_by(Region) %>%
          group_split()
      } else {
        Cond_PMF <- modular %>%
          dplyr::select(-`1st_network`) %>%
          spread(Age_group, freq) %>%
          arrange(MODULAR) %>% 
          group_by(Region) %>%
          group_split()
      }
      
      ##############################################################################
      ##############################################################################
      # Converts an adjacency matrix to a 2-column dataframe
      ##############################################################################
      ##############################################################################
      adjacency_to_2col <- function(data) {
        crossdata <- lapply(rownames(data), function(x) sapply(colnames(data), function(y) list(x, y, data[x, y])))
        crossdatatmp <- matrix(unlist(crossdata), nrow = 3)
        crossdatamat <- t(crossdatatmp)
        if (colnames(data)[5] == "YM") {
          colnames(crossdatamat) <- c("Young", "Middle", "Value")
        } else {
          colnames(crossdatamat) <- c("Middle", "Old", "Value")
        }
        crossdatadf <- as.data.frame(crossdatamat, stringsAsFactors = F)
        crossdatadf[, 3] <- as.numeric(crossdatadf[, 3] %>% na.omit())
        return(crossdatadf %>% na.omit())
      }
      
      ##############################################################################
      ##############################################################################
      # For each region, finds the most probable trajectory
      ##############################################################################
      ##############################################################################
      
      Cond_PMF_list <- list()
      for (i in 1:length(Cond_PMF)) {
        tmp <<- rbindlist(Cond_PMF[i]) 
        
        ############################################################################
        # First segment - Young to Middle
        ############################################################################
        
        # Beware of the order here, first young then old, make sure of the factor levels
        # Outer compute all pairwise probabilities between each of the 4 functional roles, element-wise matrix multiplication
        outer_young_to_middle <- outer(tmp[,4] %>% as.matrix(), tmp[, 5] %>% as.matrix()) %>% as.data.frame()
        rownames(outer_young_to_middle) <- unlist(tmp$MODULAR)
        colnames(outer_young_to_middle) <- unlist(tmp$MODULAR)
        
        
        outer_young_to_middle <- outer_young_to_middle %>% mutate(YM = "YM")
        # Returning a 4 by 4 matrix which is then converted to a dataframe
        crossdatadf <- adjacency_to_2col(outer_young_to_middle)
        outer_young_to_middle_2 <- crossdatadf
        
        # Filtering out the null probabilities for each region
        tmp_young_to_middle <<- cbind(tmp %>% slice(1:nrow(outer_young_to_middle_2)) %>%
                                        dplyr::select(Consensus_vector_0.15, Region), outer_young_to_middle_2) %>%
          plyr::rename(c("Value" = "Value_YM")) %>% 
          filter(Value_YM > 0)
        
        ############################################################################
        # Second segment - Middle to Old
        ############################################################################
        
        outer_middle_to_old <- outer(tmp_young_to_middle[, 5] %>% as.matrix(), tmp[, 6] %>% as.matrix()) %>% as.data.frame()
        # Indexes a number to each outer product/trajectory to bypass 'duplicate rows not allowed'
        # This essentially keeps trajectories if they are tied, e.g., Provincial-Connector & Provincial-Provincial if tied
        bypass_duplicate_row <- tmp_young_to_middle %>%
          mutate(helper_vector = rep(seq(1:nrow(.)))) %>%
          unite(., Young, "Young", "helper_vector", remove = FALSE) %>%
          unite(., Middle, "Middle", "helper_vector", remove = FALSE) %>%
          dplyr::select(-helper_vector)
        
        rownames(outer_middle_to_old) <- unlist(bypass_duplicate_row$Middle)
        colnames(outer_middle_to_old) <- unlist(tmp$MODULAR)
        
        
        outer_middle_to_old <- outer_middle_to_old %>% mutate(MO = "MO")
        crossdatadf <- adjacency_to_2col(outer_middle_to_old)
        
        # Finds the maximum probability
        max <- crossdatadf %>% dplyr::slice_max(Value, n = 1)
        
        # Keeps the probabilities within a user-defined range, 5% or 0.05 in the present study
        outer_middle_to_old_2 <- crossdatadf %>% filter(Value >= (max$Value[1] - (max$Value*threshold)))
        
        
        tmp_middle_to_old <- cbind(tmp %>% slice(1:nrow(outer_middle_to_old_2)) %>%
                                     dplyr::select(Consensus_vector_0.15, Region), outer_middle_to_old_2) %>%
          plyr::rename(c("Value" = "Value_MO"))
        
        full_trajectory <- tmp_middle_to_old %>%
          merge(., bypass_duplicate_row %>% dplyr::select(c("Young", "Middle", "Value_YM")), by = c("Middle"))
        
        # Aggregate for all regions
        Cond_PMF_list[[i]] <- full_trajectory
      }
      
      
      Cond_PMF_final <<- rbindlist(Cond_PMF_list) %>%
        # Giving the original labels back, because they had an index number from the helper vector
        mutate(Young = ifelse(base::startsWith(Young, "Connector"), "Connector",
                              ifelse(base::startsWith(Young, "Provincial"), "Provincial",
                                     ifelse(base::startsWith(Young, "Peripheral"), "Peripheral",
                                            ifelse(base::startsWith(Young, "Satellite"), "Satellite", Young)
                                     )
                              )
        )) %>%
        mutate(Middle = ifelse(base::startsWith(Middle, "Connector"), "Connector",
                               ifelse(base::startsWith(Middle, "Provincial"), "Provincial",
                                      ifelse(base::startsWith(Middle, "Peripheral"), "Peripheral",
                                             ifelse(base::startsWith(Middle, "Satellite"), "Satellite", Middle)
                                      )
                               )
        )) %>%
        # Identify each region with a unique label
        mutate(helper_vector = rep(seq(nrow(.)))) %>%
        unite(., Region, c("Region", "helper_vector"), remove = FALSE) %>%
        # Transform  back to an alluvial data format
        pivot_longer(
          cols = c("Young", "Middle", "Old"),
          names_to = "Age_group",
          values_to = "MODULAR"
        )
      
      # Makes sure the factor order is right
      Cond_PMF_final$Age_group <- factor(Cond_PMF_final$Age_group, levels = c("Young", "Middle", "Old"))
      
      ############################################################################
      # ALLUVIAL PLOT
      ############################################################################
      library(ggalluvial)
      
      display_cluster <- Cond_PMF_final %>%
        group_by(Age_group, MODULAR) %>%
        summarize(s = n()) %>%
        arrange(Age_group, desc(MODULAR)) %>%
        .$MODULAR
      
      # display_percentage <- Cond_PMF_final %>%
      #   group_by(Age_group, MODULAR) %>%
      #   summarize(s = n()) %>%
      #   group_by(Age_group) %>%
      #   mutate(s = scales::percent(s / sum(s), accuracy = 0.1)) %>%
      #   arrange(Age_group, desc(MODULAR)) %>%
      #   .$s
      
      alluvial_cluster <- ggplot(
        Cond_PMF_final,
        aes(x = Age_group, stratum = MODULAR, alluvium = Region, fill = MODULAR)
      ) +
        geom_flow(alpha = .7, curve_type = "arctangent", width = .2, na.rm = TRUE) +
        geom_stratum(alpha = .8) +
        scale_x_discrete(expand = c(.1, .1)) +
        # geom_text(stat = "stratum", label = display_percentage, nudge_x = -0.05) +
        geom_text(stat = "stratum", label = display_cluster) +
        scale_fill_brewer(palette = "PuOr", direction = 1) +
        # labs(title = paste0("Most probable topological reconfiguration trajectory (z-scored probability threshold: ", threshold, ")")) +
        labs(x = "Age group") +
        theme_pubr(legend = "none",
                   base_size = 18) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank())
      
      alluvial_cluster
    }
    
    ############################################################################
    ############################################################################
    ############################################################################

    # INPUT of the function ----
    if (length(plot_button_subsystem()) == 1) {
      trajectory_subsystem_plot <<- input$subsystem_choice
    } else if (length(plot_button_subsystem()) > 1) {
      trajectory_subsystem <- list()
      for (i in 1:length(plot_button_subsystem())) {
        trajectory_subsystem[[i]] <- plot_button_subsystem()[i]
        trajectory_subsystem_bis <- rbindlist(lapply(trajectory_subsystem, as.data.table))
        trajectory_subsystem_plot <<- capture.output(cat(trajectory_subsystem_bis %>% as.matrix(), sep = "|"))
      }
    } 
    
    if (length(plot_button_RSN()) == 1) {
      trajectory_RSN_plot <<- input$RSN_choice
    } else if (length(plot_button_RSN()) > 1) {
      trajectory_RSN <- list()
      for (i in 1:length(plot_button_RSN())) {
        trajectory_RSN[[i]] <- plot_button_RSN()[i]
        trajectory_RSN_bis <- rbindlist(lapply(trajectory_RSN, as.data.table))
        trajectory_RSN_plot <<- capture.output(cat(trajectory_RSN_bis %>% as.matrix(), sep = "|"))
      }
    } 
    
    threshold_plot <- sliderValue()

    trajectory(list_community = trajectory_subsystem_plot,
               list_RSN = trajectory_RSN_plot, 
               threshold = threshold_plot)
  })
}


##########################################################################################
##########################################################################################
# Deploy the app ----
shinyApp(ui = ui, server = server)
# rsconnect::deployApp("SENECA", account = "lpnc-lang")

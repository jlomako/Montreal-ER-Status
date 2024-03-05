library(dplyr)
library(ggplot2)
library(shiny)

# to do: select whole days 

# get data - start with max
days <- 30
getrows <- 24*days
file_path <- "https://github.com/jlomako/hospital-occupancy-tracker/raw/main/tables/"
occupancy <- tail(vroom::vroom(paste0(file_path, "occupancy.csv"), show_col_types = FALSE), getrows)
patients_total <- tail(vroom::vroom(paste0(file_path, "patients_total.csv"), show_col_types = FALSE), getrows)
patients_waiting <- tail(vroom::vroom(paste0(file_path, "patients_waiting.csv"), show_col_types = FALSE), getrows)

# convert data
occupancy <- as.data.frame(occupancy)
occupancy$Date <- as.POSIXct(occupancy$Date, format = "%Y-%m-%dT%H:%M")

# convert data
patients_total <- as.data.frame(patients_total)
patients_total$Date <- as.POSIXct(patients_total$Date, format = "%Y-%m-%dT%H:%M")

# convert data
patients_waiting <- as.data.frame(patients_waiting)
patients_waiting$Date <- as.POSIXct(patients_waiting$Date, format = "%Y-%m-%dT%H:%M")

# get names
hospitals <- names(occupancy[2:22])


# get max
occupancy_max_value <- max(occupancy[,2:22], na.rm=T)
patients_total_max_value <- max(patients_total[,2:22], na.rm=T)

# get the most recent occupancy rate
most_recent_time <- max(occupancy$Date, na.rm = TRUE)
current_occupancy <- occupancy %>%
  filter(Date == most_recent_time) %>%
  tidyr::gather(key = "Hospital", value = "OccupancyRate", -Date) %>%
  group_by(Hospital) %>%
  #summarize(max_rate = max(OccupancyRate, na.rm = TRUE)) %>%
  summarize(max_rate = ifelse(all(is.na(OccupancyRate)), NA, max(OccupancyRate, na.rm = TRUE))) %>%
  arrange(desc(max_rate))


ui <- bootstrapPage(
  
  # uses bootstrap 5
  theme = bslib::bs_theme(version = 5, bootswatch = "sandstone"),
  
  tags$head(HTML("<title>Montréal Emergency Room Tracker II</title>")),
  tags$script(HTML('https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js')),
  
  div(
    class = "container-sm px-0",
    h1("Montréal Emergency Room Tracker", class = "text-center pt-3"),
    
    div(
      class = "row",
      
      div(
        class = "col-md-12 py-2",
        
        div(
          class = "card h-100 text-center",
          
          div(
            class = "card-header bg-secondary",
            h5("Select a hospital to see current status:", class = "card-title")
          ),
          
          div(
            selectInput(
              inputId = "hospital",
              label = NULL,
              choices = hospitals,
              width = "100%"
            )
          ),
          
          div(HTML("Some text about the current status of the selected hotel, current patient counts and the occupancy rate."), class="text-start"),
          
          div(
            class = "card-body",
            div(class = "row",
                div(class = "col-sm-6 text-start",
                    div(h5("Occupancy Rate", class = "text-start pt-0 pb-2")),
                ),
                div(class="col-sm-6 text-end pb-0",
                    actionButton(
                      inputId = "decrease_days",
                      label = NULL,
                      icon = icon("minus"),
                      class = "button btn btn-secondary btn-sm"
                    ),
                    div(
                      class = "button btn btn-outline-secondary btn-sm",
                      span(textOutput('selected_days_display'))
                    ),
                    actionButton(
                      inputId = "increase_days",
                      label = NULL,
                      icon = icon("plus"),
                      class = "button btn btn-secondary btn-sm"
                    )
                ), # end buttons
            ), # end row
            
            div(
              plotOutput('plot_occupancy')
            ),

            div(
              class = "text-start py-3",
              textOutput('selected_hospital_display')
            ),            
            
            div(
              h5("Patient Counts", class = "text-start py-3")
            ),

            div(
              plotOutput('plot_patients')
            ),
            div(
              h5("See more (will be linked)", class = "text-start py-2")
            ),
            div(
              tableOutput('table_occupancy')
            )
          )  # card body end
        )  # card end
      )  # col end
    )  # row end
  )  # container end
  

) # bootstrapPage end

server <- function(input, output, session) {
  
  # selected hospital
  selected_hospital <- reactiveVal(hospitals[1])
  output$selected_hospital_display <- renderText({ selected_hospital() })
  
  # observe selected hospital
  observe({ selected_hospital(input$hospital) })
  
  # number of days
  selected_days <- reactiveVal(7)
  
  # display selected number of days
  output$selected_days_display <- renderText({ selected_days() })
  
  # increase number of days
  observeEvent(input$increase_days, {
    new_days <- min(selected_days() + 1, 30)
    selected_days(new_days)
  })
  
  # decrease number of days
  observeEvent(input$decrease_days, {
    selected_days(max(selected_days() - 1, 1))
  })
  

  output$plot_occupancy <- renderPlot({
    selected_occupancy <- tail(occupancy, selected_days()*24)
    
    ggplot(selected_occupancy, aes(x = Date, y = !!sym(input$hospital))) +
        geom_line(linewidth = 0.5, show.legend = F, na.rm = T) +
        labs(x = NULL, y = NULL, title = NULL) +
        scale_x_datetime(expand = c(0,0), date_labels = "%a, %b %d %H:%M", date_breaks = "1 day") +
        scale_y_continuous(expand = c(0,0), limits = c(0,occupancy_max_value), labels = scales::percent_format(scale = 1)) +
        theme_minimal() +
        geom_hline(yintercept = 100, linetype="dashed", col = "red") +
        theme(axis.text.x = element_text(angle=90, hjust=0.5, vjust=0.5))
  }, res = 96)
  
  output$plot_patients <- renderPlot({
    selected_patients_total <- tail(patients_total, selected_days()*24)
    selected_patients_waiting <- tail(patients_waiting, selected_days()*24)
    
    ggplot() +
      geom_line(data = selected_patients_total, aes(x = Date, y = !!sym(input$hospital), color = "Total"), linewidth = 0.5, show.legend = F, na.rm = T) +
      geom_line(data = selected_patients_waiting, aes(x = Date, y = !!sym(input$hospital), color = "Waiting"), linewidth = 0.5, show.legend = F, na.rm = T) +
      labs(x = NULL, y = NULL, title = NULL) +
      scale_x_datetime(expand = c(0, 0), date_labels = "%a, %b %d %H:%M", date_breaks = "1 day") +
      scale_y_continuous(expand = c(0, 0), limits = c(0, patients_total_max_value)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)) +
      scale_color_manual(values = c("Total" = "blue", "Waiting" = "red"), name = "Dataset")
  }, res = 96)
  
  output$table_occupancy <- renderTable(current_occupancy)
  
}

shinyApp(ui, server)

#   
# 
# # plot
# ggplot(occupancy, aes(x = Date, y = occupancy[, 3])) +
#   geom_line(size = 0.5, show.legend = F, na.rm = T) +
#   labs(x = "Time", y = NULL, title = paste("Occupancy Rate: ",colnames(occupancy)[3])) +
#   scale_x_datetime(expand = c(0,0), date_labels = "%a, %b %d %H:%M", date_breaks = "1 hour") +
#   scale_y_continuous(expand = c(0,0), limits = c(0,occupancy_max_value), labels = scales::percent_format(scale = 1)) +
#   theme_minimal() +
#   geom_hline(yintercept = 100, linetype="dashed", col = "red") +
#   theme(axis.text.x = element_text(angle=90, hjust=0.5, vjust=0.5))
# 
# # patients total and waiting
# ggplot() +
#   geom_line(data = patients_total, aes(x = Date, y = patients_total[, 3], color = "Total"), size = 0.5, show.legend = F, na.rm = T) +
#   geom_line(data = patients_waiting, aes(x = Date, y = patients_waiting[, 3], color = "Waiting"), size = 0.5, show.legend = F, na.rm = T) +
#   labs(x = "Time", y = NULL, title = paste0("Patients Waiting + Total: ",colnames(occupancy)[3])) +
#   scale_x_datetime(expand = c(0, 0), date_labels = "%a, %b %d %H:%M", date_breaks = "1 hour") +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, patients_total_max_value)) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)) +
#   scale_color_manual(values = c("Total" = "blue", "Waiting" = "red"), name = "Dataset")
# 
# 


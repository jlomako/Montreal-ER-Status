library(dplyr)
library(ggplot2)
library(shiny)

# get data
days <- 30
getrows <- 24*days

col_names <- c("Date", "Centre Hospitalier de l'Université de Montréal", "Centre Hospitalier de St. Mary", "CHU Sainte-Justine", "Hôpital de Lachine", "Hôpital de Lasalle", "Hôpital de Soins Psychiatriques de l'Est-de-Montréal", "Hôpital de Verdun", "Hôpital Douglas", "Hôpital du Sacré-Cœur de Montréal", "Hôpital en Santé Mentale Albert-Prévost", "Hôpital Fleury", "Hôpital Général de Montréal", "Hôpital Général du Lakeshore", "Hôpital Général Juif", "Hôpital Jean-Talon", "Hôpital Maisonneuve-Rosemont", "Hôpital Notre-Dame", "Hôpital Royal Victoria", "Hôpital Santa Cabrini", "Institut de Cardiologie de Montréal", "Hôpital de Montréal pour Enfants", "Total Montréal")

file_path <- "https://github.com/jlomako/hospital-occupancy-tracker/raw/main/tables/"

occupancy <- tail(vroom::vroom(paste0(file_path, "occupancy.csv"), show_col_types = F), getrows)
patients_total <- tail(vroom::vroom(paste0(file_path, "patients_total.csv"), show_col_types = F), getrows)
patients_waiting <- tail(vroom::vroom(paste0(file_path, "patients_waiting.csv"), show_col_types = F), getrows)

# add column names
colnames(occupancy) <- col_names
colnames(patients_total) <- col_names
colnames(patients_waiting) <- col_names

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
hospitals <- col_names[2:22]


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
  theme = bslib::bs_theme(version = 5, bootswatch = "spacelab"),
  
  tags$head(HTML("<title>Montréal Emergency Room Status</title>")),
  tags$script(HTML('https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js')),
  
  div(
    class = "container-sm px-0",
    h1("Montréal Emergency Room Status", class = "text-center pt-5"),
    
    div(
      class = "row",
      
      div(
        class = "col-md-12 py-2",
        
        div(
          class = "card h-100 text-center",
          
          div(
            class = "card-header bg-primary",
            h5("Select a hospital to see current status:", class = "card-title")
          ),
          
          div(class="card-body py-2",
            selectInput(
              inputId = "hospital",
              label = NULL,
              choices = hospitals,
              width = "100%"
            ),

            div(
              class="px-0 text-start",
              span(textOutput('selected_hospital_text'), class="pb-3 strong"),
              div(
                paste0("This information is updated every hour. The last update was on ", 
                       format(most_recent_time, "%a, %b %e, %Y at %l %p"),
                       ". For a detailed chronological representation of patient numbers and occupancy rates over time, please see the charts below."),
                  class="py-3")
                ),
            
            div(
              h5(textOutput('current_hospital'), class = "py-2 border-top")
            ),
            
            div(
              class = "clearfix",
                div(
                  class = "float-start text-start",
                    div(h5("Hourly Occupancy Rate", class = "text-start pt-0 pb-2")),
                ),
                div(
                  class="float-end text-end pb-0",
                    actionButton(
                      inputId = "decrease_days",
                      label = NULL,
                      icon = icon("minus"),
                      class = "button btn btn-primary btn-sm"
                    ),
                    div(
                      class = "button btn btn-outline-secondary btn-sm",
                      span(textOutput('selected_days_display'))
                    ),
                    actionButton(
                      inputId = "increase_days",
                      label = NULL,
                      icon = icon("plus"),
                      class = "button btn btn-primary btn-sm"
                    ),
                 ), # end buttons
             ), # end clearfix
            
            div(
              plotOutput('plot_occupancy')
            ),
            
          ),  # card body end
            
            div(class="card-footer text-start border-bottom", 
                h5("Occupancy Rate: The occupancy rate refers to the percentage of stretchers that are occupied by patients. An occupancy rate of over 100% indicates that the emergency room is over capacity, typically meaning that there are more patients than there are stretchers.", class="small"),
            ),
            

          div(
            class = "card-body",

            div(
              class = "clearfix",
              h5("Hourly Patient Counts", class = "text-start float-start"), 
              div(
                class = "small float-end",
                span("Patients Total", style = "color: #121eff;"), 
                span(" - "), 
                span("Patients waiting", style = "color: #ff0000;")
              )
            ),

            div(
              plotOutput('plot_patients')
            ),
          ),  # card body end

          div(class="card-footer text-start", 
              h5("Patients Waiting: The number of patients in the emergency room who are waiting to be seen by a physician.", class="small "),
              h5("Patients Total: The total number of patients in the emergency room, including those who are currently waiting to be seen by a physician.", class="small")
          )
        )  # card end
      )  # col end
    ),  # row end
    
    div(
      class = "row",
      
      div(
        class = "col-md-12 py-2",
        
        div(
          class = "card h-100 text-start",
          
          div(
            class = "card-header bg-secondary",
            h5("Compare Emergency room status", class = "card-title")
          ),
          
          div(
            class = "card-body",
            div(
              h5("Current Occupancy Rates", class = "text-start py-2")
            ),
            div(
              tableOutput('table_occupancy')
            )
          ),
          
          div(class="card-footer", h5('This website is for informational purposes only. If you are in need of urgent medical treatment, visit your nearest ER or call 9-1-1.
                                               In case of a non-urgent health issue call 8-1-1', 
                                      tags$a(href="https://www.quebec.ca/en/health/finding-a-resource/info-sante-811/", "(Info Santé)"),
                                      class="small")),
        ) # card end
      )  # col end 
    ),  # row 
    
    # source & disclaimer
    div(class="row",
        div(class="col-sm-12 text-center py-3",
            div(HTML("Data source: Ministère de la Santé et des Services sociaux du Québec<br>© Copyright 2022-2024,"),
                tags$a(href="https://github.com/jlomako", "jlomako")
            ),
        ),
    ),
    
    
  )  # container end
  

) # bootstrapPage end

server <- function(input, output, session) {
  
  # text for selected hospital
  selected_hospital <- reactiveVal(hospitals[1])
  # get current values for selected hospital and render text
  output$selected_hospital_text <- renderText({
    current_occupancy <- tail(occupancy[[selected_hospital()]], 1)
    current_patients_waiting <- tail(patients_waiting[[selected_hospital()]], 1)
    current_patients_total <- tail(patients_total[[selected_hospital()]], 1)
    
    # Check for NAs and show different text if any of them is.na
    if (any(is.na(c(current_occupancy, current_patients_waiting, current_patients_total)))) {
      "The data for the selected hospital is currently not available. Please check back later."
    } else {
      paste0(selected_hospital(), ": ",
             "There are currently ", current_patients_total, " patients in the ER, with ",
             current_patients_waiting, " of these patients waiting to be seen by a health professional. ",
             "The current occupancy rate is ", current_occupancy, "%.")
    }
  })
  
  # name of selected hospital
  output$current_hospital <- renderText({ selected_hospital() })
  
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
        scale_x_datetime(expand = c(0,0), date_labels = "%a, %b %e, %l%p", date_breaks = "1 day") +
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
      scale_x_datetime(expand = c(0, 0), date_labels = "%a, %b %e, %l%p", date_breaks = "1 day") +
      scale_y_continuous(expand = c(0, 0), limits = c(0, patients_total_max_value)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)) +
      scale_color_manual(values = c("Total" = "blue", "Waiting" = "red"), name = "Dataset")
  }, res = 96)
  
  output$table_occupancy <- renderTable(current_occupancy)
  
}

shinyApp(ui, server)

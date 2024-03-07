library(dplyr)
library(ggplot2)
library(shiny)

#occupancy <- tail(vroom::vroom(paste0(file_path, "occupancy.csv"), show_col_types = F, col_names = F), getrows) => error when col_names = F

# get data - start with max
days <- 30
getrows <- 24*days
col_names <- c("Date", "Centre Hospitalier de l'Université de Montréal", "Centre Hospitalier de St. Mary", "CHU Sainte-Justine", "Hôpital de Lachine", "Hôpital de Lasalle",  "Hôpital de Verdun", "Hôpital Douglas", "Hôpital Maisonneuve-Rosemont", "Hôpital du Sacré-Cœur de Montréal", "Hôpital en Santé Mentale Albert-Prévost", "Hôpital Fleury", "Hôpital Général de Montréal", "Hôpital Général du Lakeshore", "Hôpital Général Juif", "Hôpital Jean-Talon", "Hôpital Notre-Dame", "Hôpital Royal Victoria", "Hôpital Santa Cabrini", "Institut de Cardiologie de Montréal", "Institut Universitaire en Santé Mentale de Montréal", "Hôpital de Montréal pour Enfants", "Total Montréal")
file_path <- "https://github.com/jlomako/hospital-occupancy-tracker/raw/main/tables/"

occupancy <- tail(vroom::vroom(paste0(file_path, "occupancy.csv"), show_col_types = F), getrows)
patients_total <- tail(vroom::vroom(paste0(file_path, "patients_total.csv"), show_col_types = F), getrows)
patients_waiting <- tail(vroom::vroom(paste0(file_path, "patients_waiting.csv"), show_col_types = F), getrows)
# get last entry of wait times
wait_hours <- tail(vroom::vroom(paste0(file_path, "wait_hours.csv"), col_names = F, show_col_types = F), 1)
wait_hours_stretcher <- tail(vroom::vroom(paste0(file_path, "wait_hours_stretcher.csv"), col_names = F, show_col_types = F), 1)

# function that adds column names and converts Date column
convert_data <- function(data) {
  # add column names
  colnames(data) <- col_names
  # convert data
  data <- as.data.frame(data)
  data$Date <- as.POSIXct(data$Date, format = "%Y-%m-%dT%H:%M")
  return(data)
}

occupancy <- convert_data(occupancy)
patients_total <- convert_data(patients_total)
patients_waiting <- convert_data(patients_waiting)
wait_hours <- convert_data(wait_hours)
wait_hours_stretcher <- convert_data(wait_hours_stretcher)


# get names
hospitals <- col_names[2:22] # names(occupancy[2:22])

# get max
occupancy_max_value <- max(occupancy[,2:22], na.rm=T)
patients_total_max_value <- max(patients_total[,2:22], na.rm=T)

# get update time
most_recent_time <- max(occupancy$Date, na.rm = T)

# function get current occupancy rates, patients counts for all hospitals
get_data <- function(data, column_name) {
  data %>%
    filter(Date == most_recent_time) %>%
    select(-Date) %>%
    tidyr::gather(key = "Hospital", value = !!sym(column_name)) 
}

current_occupancy <- get_data(occupancy, "OccupancyRate")
current_patients_total <- get_data(patients_total, "PatientsTotal")
current_patients_waiting <- get_data(patients_waiting, "PatientsWaiting")
# current_wait_hours <- get_data(wait_hours, "WaitHours")


# combine current data
current_data <- current_occupancy %>% 
  left_join(current_patients_total, by = c("Hospital")) %>%
  left_join(current_patients_waiting, by = c("Hospital")) %>%
  filter(Hospital != "Total Montréal") %>%
  arrange(desc(OccupancyRate))


colnames(current_data) <- c("Hospital", "Occupancy Rate (%)", "Patients Total", "Patients Waiting")



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
              class="alert alert-light text-start",
              span(htmlOutput('selected_hospital_text'), class="pb-3 strong"),
              div(
                paste0("The last update was on ", 
                       format(most_recent_time, "%a, %b %e, %Y, %l:%M %p"),
                       ". This information is updated every hour. For a detailed chronological representation of patient numbers and occupancy rates over time, please see the charts below."),
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
            h5("Compare Emergency room status on", format(most_recent_time, "%a, %b %e, %Y, %l:%M %p"), class = "card-title")
          ),
          
          div(
            class = "card-body px-0",
            div(
              class="table table-hover",
              tableOutput('table_data')
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
    current_occupancy <- current_data[which(current_data$Hospital == selected_hospital()), "Occupancy Rate (%)"]
    current_patients_waiting <- current_data[which(current_data$Hospital == selected_hospital()), "Patients Waiting"]
    current_patients_total <- current_data[which(current_data$Hospital == selected_hospital()), "Patients Total"]
    current_wait_hours <- wait_hours[[selected_hospital()]]
    current_wait_hours_stretcher <- wait_hours_stretcher[[selected_hospital()]]

    # Check for NAs and show different text if any of them is.na
    if (any(is.na(c(current_occupancy, current_patients_waiting, current_patients_total)))) {
      "The data for the selected hospital is currently not available. Please check back later."
    } else {
      paste(
        "Emergency Room Status for ", selected_hospital(), ":",
        "<br>&#128101; Current Patient Count: ", "<strong>", current_patients_total, "</strong>",
        "<br>&#8987; Waiting to be Seen: ", "<strong>", current_patients_waiting, "</strong>",
        "<br>&#128200; Occupancy Rate: ", "<strong>", current_occupancy, "%", "</strong>",
        "<br>&#9201; Average Stay: ", "<strong>", current_wait_hours, " hours" , "</strong>", " (previous day)",
        "<br>&#128719; Average Stay on stretcher: ", "<strong>", current_wait_hours_stretcher, " hours" , "</strong>", " (previous day)",
        "<br><small>Please note that the Average Stay includes patients who left the ER before seeing a health professional.")
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
  
  output$table_data <- renderTable(current_data, digits = 0, spacing = "xs", hover=TRUE)
  
}

shinyApp(ui, server)



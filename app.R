library(dplyr)
library(ggplot2)
library(shiny)

# suppress messages
options(dplyr.summarise.inform = FALSE)

# Notes:
# occupancy <- tail(vroom::vroom(paste0(file_path, "occupancy.csv"), show_col_types = F, col_names = F), getrows) => error when col_names = F
# excluded wait hours in ER waiting area (DMS_ambulatoire) due to doubts about the measurement method

# get data - start with max
days <- 30
getrows <- 24*days

col_names <- c("Date", "Centre Hospitalier de l'Université de Montréal", "Centre Hospitalier de St. Mary", "CHU Sainte-Justine", "Hôpital de Lachine", "Hôpital de Lasalle",  "Hôpital de Verdun", "Hôpital Douglas", "Hôpital Maisonneuve-Rosemont", "Hôpital du Sacré-Cœur de Montréal", "Hôpital en Santé Mentale Albert-Prévost", "Hôpital Fleury", "Hôpital Général de Montréal", "Hôpital Général du Lakeshore", "Hôpital Général Juif", "Hôpital Jean-Talon", "Hôpital Notre-Dame", "Hôpital Royal Victoria", "Hôpital Santa Cabrini", "Institut de Cardiologie de Montréal", "Institut Universitaire en Santé Mentale de Montréal", "Hôpital de Montréal pour Enfants", "Total Montréal")
file_path <- "https://github.com/jlomako/hospital-occupancy-tracker/raw/main/tables/"

occupancy <- tail(vroom::vroom(paste0(file_path, "occupancy.csv"), show_col_types = F), getrows)
patients_total <- tail(vroom::vroom(paste0(file_path, "patients_total.csv"), show_col_types = F), getrows)
patients_waiting <- tail(vroom::vroom(paste0(file_path, "patients_waiting.csv"), show_col_types = F), getrows)
# get last entry of wait times
# wait_hours <- tail(vroom::vroom(paste0(file_path, "wait_hours.csv"), col_names = F, show_col_types = F), 1)
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
# wait_hours <- convert_data(wait_hours)
wait_hours_stretcher <- convert_data(wait_hours_stretcher)


# get names
hospitals <- col_names[2:22]

# get max
occupancy_max_value <- max(occupancy[,2:22], na.rm=T)
patients_total_max_value <- max(patients_total[,2:22], na.rm=T)

# get update time
most_recent_time <- max(occupancy$Date, na.rm = T)
most_recent_hour <- as.POSIXct(paste("2000-01-01", format(most_recent_time, "%H:00:00")))

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

# rename cols for table
colnames(current_data) <- c("Hospital", "Occupancy Rate (%)", "Patients Total", "Patients Waiting")



# calculate median occupancy
occupancy_median <- occupancy %>%
   mutate(hour = lubridate::hour(Date)) %>%
   mutate(day_number = as.POSIXlt(Date)$wday+1) %>% # Sun = 1, Mon = 2 etc
   tidyr::gather(key = "Hospital", value = "Occupancy", -c(Date, hour, day_number)) %>%
#  group_by(Hospital, day_number, hour) %>% # group_by(Hospital, hour) # median for each hospital + day + hour
   group_by(Hospital, hour) %>% # median for each hospital and hour
   summarize(median_occupancy = round(median(Occupancy, na.rm = T)))




# set colors:
primary_color = "#004950"
secondary_color = "#198c7e"
warning_color = "#f8c037"
light_color = "#e4e6e9"


ui <- bootstrapPage(
  
  # uses bootstrap 5
  theme = bslib::bs_theme(
    version = 5, 
    bootswatch = "sandstone",
    primary = primary_color,
    secondary = secondary_color,
    warning = warning_color,
    light = light_color),
  
  tags$head(
    HTML("<title>Montréal Emergency Room Status</title>"),
    tags$script(
      HTML('https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js')),
  
    tags$style(
      HTML('
       .has-items {
         border-color: #004950 !important; # 446e9b 
       }
       .spacing-s, .table- {
         table-layout: fixed;
         width: 100% !important;
       }
       .table.table- > thead > tr > th,
       .table.table- > tbody > tr > td,
       .table.table- > tfoot > tr > td {
         padding-left: 0 !important;
         padding-right: 0 !important;
       }
       .table- th:first-child,
       .table- td:first-child {
         width: 30%; 
       }
       h5 > .shiny-plot-output {
         height: 80px !important;
       }'
      )
    ),
  ),
  
  
  div(
    class = "container-md px-0",
    
    h1("Montréal Emergency Room Status", class = "text-center pt-5"),
    
    div(
      class = "row",
      
      div(
        class = "col-md-6",
        
        div(
          class = "card",
          
          div(
            class = "card-header bg-primary text-center",
            h5("Select a hospital to see current status:")
          ),
          
          div(class="card-body py-2 text-center",
              
              selectInput(
                inputId = "hospital",
                label = NULL,
                choices = hospitals,
                width = "100%"
              ),
              
              div(
                class="text-start",
                span(htmlOutput('selected_hospital_text'), class="pb-3 strong")
              ),
              
              div(
                class="text-start",
                paste0("This information is updated every hour. The last update was on ", 
                       format(most_recent_time, "%a, %b %e, %Y, %l:%M %p"),
                       ". For a detailed chronological representation of patient numbers and occupancy rates over time, please see the charts below."),
                class="py-3"),
              
          ),# end card body

          div(
            class="card-text p-2 pt-3 border-top border-bottom bg-secondary text-center",
              h5(textOutput('current_hospital'))
              ),
          
          # AVERAGE OCCUPANCY MINI PLOT
          div(
             class="p-2 small",
             span("24-Hour Average Occupancy", class="text-start ps-2"),
             br(),
             h5(plotOutput("plot_median_occupancy"), class="text-center"),
            ),
          hr(),
              
            div(
              class="card-text py-2 text-end",
            
                div(
                  class="pe-2",
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


                tabsetPanel(id = "tabs", type = "tabs",
                        tabPanel(value = "tab1", "Occupancy Rate",
                                 
                                 div(
                                   h5("Hourly Occupancy Rate (%)", class = "text-start p-2")
                                   # plotOutput("plot_occupancy")
                                   ),
                                 
                                 div(
                                   class="pe-2",
                                   plotOutput("plot_occupancy")
                                 ),
                                 
                                 div(class="card-footer text-start", 
                                     h5("Occupancy Rate: The occupancy rate refers to the percentage of stretchers that are occupied by patients. An occupancy rate of over 100% indicates that the emergency room is over capacity, typically meaning that there are more patients than there are stretchers.", class="small"),
                                 ),
                        ), # tabpanel1 end
                        
                        tabPanel(value = "tab2", "Patient Counts", 
                                 div(
                                   class = "clearfix",
                                   h5("Hourly Number of Patients", class = "text-start float-start p-2"), 
                                   div(
                                    class = "small float-end pe-2",
                                     br(), 
                                     span("Patients Total", style = "color: #004950;"), 
                                     br(), 
                                     span("Patients Waiting", style = "color: #f8c037;")
                                      )
                                   ),
                                 
                                 div(
                                   class="pe-2",
                                   plotOutput("plot_patients")
                                   ),
                                 
                                 div(class="card-footer text-start", 
                                     h5("Patients Waiting: The number of patients in the emergency room who are waiting to be seen by a physician.", class="small "),
                                     h5("Patients Total: The total number of patients in the emergency room, including those who are currently waiting to be seen by a physician.", class="small")
                                 ),
                        ), # tabpanel2 end
                  ), # tabsetpanel end

          ),  # card body end
        )  # card end
      ),  # col end

      # right card ------------------------->
      
      div(
        class = "col-md-6",
        
        div(
          class = "card text-start",
          
          div(
            class = "card-header text-center bg-primary p-2",
            h5("Compare Emergency room status on", format(most_recent_time, "%a, %b %e, %Y, %l:%M %p"))
          ),
          
          
          div(
            class="card-body px-0",
            div(
              class = "container",
              div(
                class="table px-0",
                tableOutput('table_data')
              )
            ),
          ),
          
          div(class="card-footer", 
          h5('This website is for informational purposes only. If you are in need of urgent medical treatment, visit your nearest ER or call 9-1-1.
              In case of a non-urgent health issue call 8-1-1',
             tags$a(href="https://www.quebec.ca/en/health/finding-a-resource/info-sante-811/", "(Info Santé)", class="text-primary"),
             class="small")),
        ) # card end
      )  # col end 
    ),  # row 
    
    # source & disclaimer
    div(class="row",
        div(class="col-sm-12 text-center py-3",
            div(HTML("Data source: Ministère de la Santé et des Services sociaux du Québec<br>© Copyright 2022-2024,"),
                tags$a(href="https://github.com/jlomako", "jlomako", class="text-primary")
            ),
        ),
    ),
    
    
  )  # container end
  

) # bootstrapPage end

server <- function(input, output, session) {
  
  # first selected hospital
  selected_hospital <- reactiveVal(hospitals[1])
  

  # observe selected hospital
  observeEvent(selected_hospital(input$hospital), {
    # render name of selected hospital
    output$current_hospital <- renderText({ selected_hospital() })
    
    # get current values for selected hospital and render text for selected hospital
    output$selected_hospital_text <- renderText({
      current_occupancy <- current_data[which(current_data$Hospital == selected_hospital()), "Occupancy Rate (%)"]
      current_patients_waiting <- current_data[which(current_data$Hospital == selected_hospital()), "Patients Waiting"]
      current_patients_total <- current_data[which(current_data$Hospital == selected_hospital()), "Patients Total"]
      # current_wait_hours <- wait_hours[[selected_hospital()]]
      current_wait_hours_stretcher <- wait_hours_stretcher[[selected_hospital()]]
      
      # Check for NAs and show different text if any of them is.na
      if (any(is.na(c(current_occupancy, current_patients_waiting, current_patients_total)))) {
        '<div class="container h-100 bg-warning px-1 py-3">
      The data for the selected hospital is currently not available. Please check back later.
      </div>'
      } else {
        paste0(
          '<div class="text-center p-2">Emergency Room Status for ', selected_hospital(), ':</div>',
          '<div class="row text-center row-cols-1 py-3 row-gap-3">',
          '<div class="col-md-4"><div class="container h-100 bg-warning py-2">',
          "Occupancy Rate: ", "<br><strong>", current_occupancy, "%", "</strong>",
          "</div></div>",        
          '<div class="col-md-4"><div class="container h-100 bg-warning py-2">',
          "Current Patient Count: ", '<br><strong>', current_patients_total, "</strong>",
          "</div></div>",
          '<div class="col-md-4"><div class="container h-100 bg-warning py-2">',
          "Waiting to be Seen: ", "<br><strong>", current_patients_waiting, "</strong>",
          '</div></div></div>',
          "Average Stay on stretcher: ", "<strong>", current_wait_hours_stretcher, " hours" , "</strong>", " (previous day)"
        )
      }
    })
    
  })

  
  
  # plot for median occupancy
  output$plot_median_occupancy <- renderPlot({ 
    # get value from 2nd col (occupancy)
    selected_occupancy <- current_data[which(current_data$Hospital == input$hospital), 2] 
    
    if (is.na(selected_occupancy)) {
      p <- NULL
    } else {
      p <- geom_col(aes(x = most_recent_hour, y = selected_occupancy), # colour = "#004950", 
                    fill = "#004950", alpha = 0.4, width=2800, position = "identity", show.legend = F)
    }
    
    occupancy_median %>%
      mutate(hour = as.POSIXct(sprintf("2000-01-01 %02d:00:00", hour))) %>% # dummy date
      filter(Hospital == input$hospital) %>%
        ggplot(aes(x = hour, y = median_occupancy)) +
          geom_col(fill="#004950", alpha = 0.2, position = "identity", show.legend=F, na.rm=T) +
          geom_hline(yintercept=100, linetype="dashed", color = "#004950") +
          scale_x_datetime(expand = c(0,0)) +
          p +
          theme_void() # remove everything around the plot
    
  }, res = 96, height=80)
  
  
  
  
  
    
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
  
  
  # render plots for tabs
  observeEvent(input$tabs,{
    if(input$tabs == "tab1")
      
      # plot occupancy for selected hospital
      output$plot_occupancy <- renderPlot({
        selected_occupancy <- tail(occupancy, selected_days()*24)
        
        ggplot(selected_occupancy, aes(x = Date, y = !!sym(input$hospital))) +
          geom_line(linewidth = 0.5, show.legend = F, na.rm = T) +
          labs(x = NULL, y = NULL, title = NULL) +
          scale_x_datetime(expand = c(0,0), date_labels = "%a\n%b %e\n%l%p", date_breaks = "1 day") +
          scale_y_continuous(expand = c(0,0), limits = c(0,occupancy_max_value), labels = scales::percent_format(scale = 1)) +
          theme_minimal() +
          geom_hline(yintercept = 100, linetype="dashed", col = "red")
         # + theme(axis.text.x = element_text(angle=90, hjust=0.5, vjust=0.5))
      }, res = 96)
      
  }) # end tab1 
  

  observeEvent(input$tabs,{
    if(input$tabs == "tab2")
      
      # plot patient counts for selected hospital
      output$plot_patients <- renderPlot({
        selected_patients_total <- tail(patients_total, selected_days()*24)
        selected_patients_waiting <- tail(patients_waiting, selected_days()*24)
        
        ggplot() +
          geom_line(data = selected_patients_total, aes(x = Date, y = !!sym(input$hospital), color = "Total"), linewidth = 0.5, show.legend = F, na.rm = T) +
          geom_line(data = selected_patients_waiting, aes(x = Date, y = !!sym(input$hospital), color = "Waiting"), linewidth = 0.5, show.legend = F, na.rm = T) +
          labs(x = NULL, y = NULL, title = NULL) +
          scale_x_datetime(expand = c(0,0), date_labels = "%a\n%b %e\n%l%p", date_breaks = "1 day") +
          scale_y_continuous(expand = c(0, 0), limits = c(0, patients_total_max_value)) +
          theme_minimal() +
          # theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)) +
          scale_color_manual(values = c("Total" = primary_color, "Waiting" = warning_color), name = "Dataset")
      }, res = 96)      
      
  }) # end tab2  

  
  # table with current data
  output$table_data <- renderTable(current_data, spacing = "xs", digits = 0) # spacing = "xs", hover=F
  
}

shinyApp(ui, server)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny) 
library(httr)
library(stringr)
library(PetfindeR)



# Load key values from environmental variables


PETFINDER_KEY <- Sys.getenv("LQVyTyilHIq4nzat6EKeukQJTRDsCjgjtAhXeefwB3ZX9U9nCV")
PETFINDER_SECRET_KEY <- Sys.getenv("3tVtCUZVqK3SUgGspYRtkgKPog8N6ybxIPnHK7GI")
PETFINDER_TOKEN <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9.eyJhdWQiOiJMUVZ5VHlpbEhJcTRuemF0NkVLZXVrUUpUUkRzQ2pnanRBaFhlZWZ3QjNaWDlVOW5DViIsImp0aSI6IjE5ZDA1ZjZiNDQ5OTAwZmFhNGVjMmExMTg3ZmYyZjVhN2YyOTNkMzkxNTQ1ZWRhMDVmOTkwYzc1MWUyZDY2Nzc4ZmEwZTBkYzdlYzc2NTdjIiwiaWF0IjoxNjgxNDU0MDMwLCJuYmYiOjE2ODE0NTQwMzAsImV4cCI6MTY4MTQ1NzYzMCwic3ViIjoiIiwic2NvcGVzIjpbXX0.wMlnECsCrUU95u_4oWxUlCfx3OH1G78slO6oREtNLYjApp3Yl_vkewzCnxm9XsfVlG6mQtljtD3ks0XN_YXUS0oyq8elCNAjXnaTbcIwrCEVJXzuVI1DqFkjPjPQjn3d4fn2TCKMbh-XDxqJvJ-czPv2bOE2M8PSv9MEsA8SfT74TBAAYbJBCoD_72CnSAs2dAgIVVYvGDmUqNvTNvkr0Rq7wqgRIpfEf98twKAru91F326Y3OjSe34OgZ0YIfCBYFMQRvSybwV8tL2EmA4XvkKvw0yqgEKIagoJ-SUTyJI6ihAVFXeM-ZYaBbTi1IoUg8t_ZZqL28sgKKM6Q9q-dw"
# Authenticate connection to Petfinder database
pf = Petfinder(key = "LQVyTyilHIq4nzat6EKeukQJTRDsCjgjtAhXeefwB3ZX9U9nCV", secret = "3tVtCUZVqK3SUgGspYRtkgKPog8N6ybxIPnHK7GI")


# Define UI
ui <- fluidPage(
  # Application title
  titlePanel("Animal Table"),
  
  # Sidebar with a select input for animal species
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "species",
                  label = "What animal are you looking for?",
                  choices = sort(unique(names(pf$animal_types()))))
    ),

    # Show a table with animal information for selected species
    mainPanel(
      tableOutput("animalTable")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Filter animal data based on selected species
  
  # Generate animal information table
  output$animalTable  <- renderTable({
    speciesLower<- str_to_lower(input$species)
    filteredData <- pf$animals(animal_type = speciesLower,
                               location = "Tucson, AZ",
                               status = "adoptable",
                               results_per_page = 100,
                               pages = NULL)
    
  
    # Setup a data frame that will print results
    printData <- NULL
    # Loop over how ever many pages are returned
    for (page in 1:length(filteredData)) {
      # Extract information for just one page
      pageData <- filteredData[[page]]
      # Filter to species of interest
      # pageData <- pageData[pageData$type == "Dog", ]
      pageData<-pageData[pageData$type == input$species, ]
      # Pull out columns of interest
      pageData <- pageData[, sort(c("type","name", "breeds.primary", "age", "gender", "contact.address.city"))]
      # Rename columns
      colnames(pageData)[colnames(pageData)=="contact.address.city"] <- "City"
      colnames(pageData)[colnames(pageData)=="type"] <- "Type of Animal"
      colnames(pageData)[colnames(pageData)=="name"] <- "Name"
      colnames(pageData)[colnames(pageData)=="breeds.primary"] <- "Breed"
      colnames(pageData)[colnames(pageData)=="gender"] <- "Gender"
      colnames(pageData)[colnames(pageData)=="age"] <- "Age"
      # Filter by city
      pageData <- pageData[pageData$City == "Tucson", ]
      
      # Update printData
      #   + if this is the first page of results, just assign to printData
      #   + if this is not first page, add to existing printData
      if (is.null(printData)) {
        printData <- pageData
      } else {
        printData <- rbind(printData, pageData)
      }
    }
    printData
  })}

# Run the app
shinyApp(ui, server)
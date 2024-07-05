library(shiny)
library(dplyr)
library(sortable)
setwd("F:/dropbox/teaching/bdma")
# data
site.df <- read.csv("bdma.data/site.selection.csv")
site.selection.df=site.df[,c(1,2,3,4,7,8,10,11)]

# Rank_sites_impt function
rank_sites_impt <- function(data, importance) {
  if (!all(names(importance) %in% colnames(data))) {
    stop("importance must match the column names of the data")
  }
  
  data_no_site <- data[, -1]
  ranks <- data.frame(Site = data$Site)
  
  for (col in colnames(data_no_site)) {
    ranks[[col]] <- rank(-data_no_site[[col]], ties.method = "first")
    ranks[[col]] <- 11 - ranks[[col]]
  }
  
  ranks$mean_score <- rowSums(ranks[, -1] * unlist(importance) / sum(unlist(importance)))
  ranks <- ranks %>% arrange(-mean_score) %>% select(Site, mean_score, everything())
  return(ranks)
}

# Shiny UI
ui <- fluidPage(
  titlePanel("Site Ranking Based on Feature Importance"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Enter the Importance of Each Feature (1-10)"),
      lapply(colnames(site.selection.df)[-1], function(col) {
        numericInput(inputId = col, label = col, value = 5, min = 1, max = 10)
      }),
      actionButton("update", "Update Ranking")
    ),
    
    mainPanel(
      tableOutput("ranking")
    )
  )
)

# Shiny Server
server <- function(input, output) {
  observeEvent(input$update, {
    importance <- sapply(colnames(site.selection.df)[-1], function(col) {
      input[[col]]
    })
    names(importance) <- colnames(site.selection.df)[-1]
    
    output$ranking <- renderTable({
      rank_sites_impt(site.selection.df, importance)
    })
  })
}

shinyApp(ui = ui, server = server)

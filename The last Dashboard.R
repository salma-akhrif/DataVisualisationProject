library(dplyr)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)
library(scales)
library(RColorBrewer)
library(corrplot)
library(readr)
library(stringr)

#Importation du dataset:
df<- read.csv("C:\\Users\\HP\\Desktop\\CourseraDataset-Clean.csv")

# Define pastel colors
pastel_colors <- c("#87C", "#FF9999", "#FF9999", "#FF9999", "#FF9999", "#FF9999", "#FF9999")

# creation d'un Shiny Dashboard:
ui<-shinyUI(dashboardPage(dashboardHeader(title = "Analyse des Cours Coursera",titleWidth =300),
                      dashboardSidebar(
                        sidebarMenu(
                          menuItem("Dataset", tabName = "data", icon = icon("database")),
                          menuItem("Tableau de bord", tabName = "dashboard", icon = icon("dashboard"))
                        )
                      ),
                      dashboardBody(tabItems(
                        tabItem(tabName = "data",
                                # Boîte d'onglets
                                tabBox(id = "t1", width = 12,
                                       tabPanel("À propos des données", icon = icon("address-card"),
                                                fluidPage(
                                                  h4("Coursera Course Analytics: Comprehensive Dataset"),
                                                p("Le jeu de données Coursera semble être une collection d'informations sur divers cours disponibles sur la plateforme Coursera Chaque ligne représente Chaque ligne représente probablement un cours différent, et le jeu de données contient plusieurs attributs décrivant les caractéristiques de chaque cours, tels que le titre, la note, le niveau, l'horaire, les objectifs d'apprentissage, les détails de l'instructeuret l'institution proposant le cours, les mots-clés, l'URL du cours, la durée pour le compléter, et le nombre d'avis.")
                                       )),
                                       tabPanel(title = "Données", icon = icon("address-card"), dataTableOutput("dataT")),
                                       tabPanel(title = "Structure", icon = icon("address-card"), verbatimTextOutput("structure"))
                                )
                        ),
                        tabItem(tabName = "dashboard" ,
                                fluidRow(
                                  valueBoxOutput("total_courses"),
                                  valueBoxOutput("avg_duration_valuebox"),
                                  valueBoxOutput("avgRatingBox")
                                ),
                        fluidRow(
                          box(title="La durée moyenne passée dans les cours par niveau",status = "primary",solidHeader=T,plotlyOutput("durationPlot")),
                          tabBox(tabPanel(title="Fréquence des mots-clés",status = "primary",solidHeader=T,plotlyOutput("keywordPlot")),
                                 tabPanel(title="Compétences les Plus Enseignées sur Coursera",status = "primary",solidHeader=T,plotlyOutput("barPlot"))),
                          box(title="Comparaison de la Popularité",status = "primary",solidHeader=T,plotlyOutput("popularityPlot")),
                          box(title ="Durée vs Note",status = "primary",solidHeader=T,plotlyOutput("durationRatingPlot"))),
                        fluidRow(box(title = "Diagramme de dispersion : Nombre d'avis vs Durée pour terminer", status = "primary",
                                     solidHeader = TRUE,
                                     width = 12,
                                     height = 500,plotlyOutput("plot5")),
                        box(title = "Le nombre totale des cours", status = "primary", solidHeader = T,
                                        selectInput("variable_x", "Filtrer par", choices = c("Level", "Category")),
                                        selectInput("rating_filter", "Filtrer par Rating:",
                                                    choices = c("Tous", "1", "2", "3", "4", "5")),
                                        plotlyOutput("distribution_plot")
                        ))
                      )))))
skills <- ''
for (skill in df$Skill.gain) {
  skills <- paste0(skills, tolower(skill), ', ')
}

skills <- strsplit(skills, ', ')[[1]]

counts <- table(skills)
common_skills <- head(sort(counts, decreasing = TRUE), 16)

server<-shinyServer(function (input,output){
  counts <- table(df$Level)
  data <- data.frame(Level = names(counts), Frequency = as.numeric(counts))
  # DataTable
  output$dataT <- renderDataTable({
    df
  })
  # Structure
  output$structure <- renderPrint({
    print(str(df))
  })
  
  # Afficher le nombre total de cours
  output$total_courses <- renderValueBox({
    num_courses <- nrow(df)
    valueBox(
      num_courses,
      "Total Courses",
      icon = icon("book")
    )
  })
  
  # Afficher la moyenne dans le valueBox
  output$avg_duration_valuebox <- renderValueBox({
    avg_duration <- mean(df$Duration.to.complete..Approx., na.rm = TRUE)
    valueBox(
      round(avg_duration, digits = 2),
      "Durée moyenne pour passer les cours",
      icon("calendar"),
      color = "purple" 
    )
  })
  output$avgRatingBox <- renderValueBox({
    avg_rating <- mean(df$Rating)
    valueBox(
      round(avg_rating, digits = 2),
      "Moyenne des évaluations",
      icon = icon("star"),
      color ="fuchsia"
    )
  })
  # Créer le graphique interactif de la durée moyenne passée dans les cours par niveau :
  output$durationPlot <- renderPlotly({ 
    df_summary <- df %>%
      group_by(Level) %>%
      summarize(Avg_Duration = mean(`Duration.to.complete..Approx..`))
    plot_ly(df_summary, x = ~reorder(Level, -Avg_Duration), y = ~Avg_Duration, type = 'bar', 
            color = ~Level) %>%
      layout(
             xaxis = list(title = 'Niveau du cour', categoryorder = 'total descending'),
             yaxis = list(title = 'Durée moyenne pour terminer par heure (Approx.)'),
             showlegend = FALSE)
  })
  # Créer le graphique interactif de la fréquence des mots-clés
  output$keywordPlot <- renderPlotly({
    # Créer un graphique à barres horizontales
    counts <- table(df$Keyword)
    df_counts <- data.frame(Keyword = names(counts), Frequency = as.numeric(counts))
    
    # Trier les données par fréquence
    df_counts <- df_counts[order(df_counts$Frequency), ]
    
    # Créer un graphique Plotly
    p <- plot_ly(df_counts, x = ~Frequency, y = ~reorder(Keyword, Frequency),
                 type = 'bar', color= ~Keyword) %>%
      layout(
             xaxis = list(title = 'Fréquence'),
             showlegend = FALSE)
    
    # Convertir le graphique Plotly en graphique ggplot
    ggplotly(p)
  })
  output$popularityPlot <- renderPlotly({
    counts_dict <- table(df$Schedule)
    
    # Explode the slices for better visual clarity
    explode <- rep(0.05, length(counts_dict))  # 'explode' all slices equally a little bit
    
    # Prepare legend labels with counts
    legend_labels <- paste(names(counts_dict), ": ", counts_dict, sep = "")
    
    # Create plotly pie chart
    plot_ly(labels = names(counts_dict), values = counts_dict,
            type = 'pie', 
            marker = list(colors = brewer.pal(length(counts_dict), 'Pastel1')),
            textinfo = 'percent+label',
            hole = 0.4,
            domain = list(x = c(0, 0.5)),
            hoverinfo = 'label+percent') %>%
      layout(
             legend = list(title = "Horaires",
                           x = 1, y = 0.5,
                           traceorder = "normal",
                           orientation = "v"),
             annotations = list(text = "Total",
                                showarrow = FALSE,
                                x = 0.22, y = 0.5)
      )
  })
  # Most Taught Skills on Coursera
  output$barPlot <- renderPlotly({
    data1 <- data.frame(skill = names(common_skills), frequency = as.numeric(common_skills))
    data1<- data1[-1,]
    
    # Create a new variable for the ordered factor
    data1$ordered_skill <- reorder(data1$skill, data1$frequency)
    
    plot_ly(data1, x = ~frequency, y = ~ordered_skill,
            type = 'bar', orientation = 'h', color = ~ordered_skill,showlegend=F) %>%
      layout(
             xaxis = list(title = "Fréquence")
             )
  })
  output$distribution_plot <- renderPlotly({
    # Sélectionner la variable en fonction de l'entrée de l'utilisateur
    variable_selected <- switch(input$variable_x,
                                "Level" = df$Level,
                                "Category" = df$Keyword)
    
    # Filtrer les données en fonction du rating sélectionné
    if (input$rating_filter != "Tous") {
      df_filtered <- df[df$Rating == as.numeric(input$rating_filter), ]
      variable_selected <- variable_selected[df$Rating == as.numeric(input$rating_filter)]
    } else {
      df_filtered <- df
    }
    
    # Calculer la distribution de la variable sélectionnée
    variable_counts <- table(variable_selected)
    
    # Créer le graphique circulaire
    plot_ly(labels = names(variable_counts),
            values = variable_counts,
            type = "pie",
            name = paste("Distribution of", input$variable_x),
            showlegend = TRUE,
            marker = list(colors = RColorBrewer::brewer.pal(length(variable_counts), "Set3")))
  })
  # Créer le graphique interactif de la durée vs Note
  output$durationRatingPlot <- renderPlotly({
    # Calculer la moyenne de la durée groupée par la note
    avg_duration <- df %>%
      group_by(Rating) %>%
      summarize(Avg_Duration = mean(Duration.to.complete..Approx.., na.rm = TRUE))
    # Créer le graphique
    plot_ly(data = avg_duration, x = ~Rating, y = ~Avg_Duration, type = 'bar') %>%
      layout(
        title = "Durée moyenne vs Note",
        xaxis = list(title = 'Note'),
        yaxis = list(title = 'Durée moyenne pour terminer (Approx.)')
      )
  })
  output$plot5 <- renderPlotly({
    # Créer le nuage de points avec plotly
    fig <- plot_ly(data = df, x = ~Number.of.Review, y = ~Duration.to.complete..Approx.., color = ~Rating, type = "scatter", mode = "markers") %>%
      layout(
        xaxis = list(title = "Number of Reviews"),
        yaxis = list(title = "Duration to complete (Approx.)")
      )
    
    # Afficher le nuage de points
    fig
  })
  
  
   
})
# executer l'app Shiny
shinyApp(ui = ui, server = server)


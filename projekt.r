library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(viridis)
library(reshape2)

Sys.setlocale(locale = "Polish")

load("przejazdy.RData")

stacje <- unique(przejazdy$Stacja)

przejazdy <- przejazdy %>% mutate(Miesiac = as.factor(month(Data, label = TRUE, abbr = FALSE)), 
                                  Dzien = ordered(weekdays(as.Date(przejazdy$Data)), levels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota","niedziela")), 
                                  Typ = ifelse(Dzien == "sobota" | Dzien == "niedziela", "weekend", "dzień powszedni"))
pogoda <- colnames(przejazdy)[4:12]

ui <- navbarPage("Nawigacja",
                 
                 tabPanel("Zadanie 1",
                          mainPanel(
                            plotOutput("zad1", height = "800px"), width = 12)
                 ),
                 
                 
                 tabPanel("Zadanie 2",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "Stacja", 
                                          label = "Stacja",
                                          choices = stacje)),
                            
                            mainPanel(plotOutput("zad2", height = "800px"))),
                 ),
                 
                 
                 tabPanel("Zadanie 3",
                          sidebarLayout(
                            sidebarPanel(
                              selectizeInput(inputId = "Stacje3", 
                                             label = "Wybierz maksymalnie 4 stacje",
                                             choices = stacje,
                                             multiple = TRUE,
                                             options = list(maxItems = 4),
                                             selected = "Pas Nadmorski")),
                            
                            mainPanel(
                              plotOutput("zad3", height = "800px"), width = 12)),
                 ),
                 
                 
                 tabPanel("Zadanie 4",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         
                                         radioButtons("choice",
                                                      "Wybierz typ podziału danych",
                                                      c("Miesiąc", "Dzień tygodnia", "Typ dnia miesiąca"),
                                                      selected = "Miesiąc"),
                                         
                                         selectInput(inputId = "Stacja2", 
                                                     label = "Stacja",
                                                     choices = stacje)),
                            
                            mainPanel(plotOutput("zad4", height = "600px"), width = 12))
                 ),
                 
                 
                 tabPanel("Zadanie 5",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         
                                         radioButtons("choice_5",
                                                      "Wybierz typ podziału danych",
                                                      c("Miesiąc", "Dzień tygodnia", "Typ dnia miesiąca"),
                                                      selected = "Miesiąc"),
                                         
                                         selectizeInput(inputId = "Stacje_5", 
                                                        label = "Wybierz maksymalnie 4 stacje",
                                                        choices = stacje,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 4),
                                                        selected = "Pas Nadmorski")),
                            
                            mainPanel(plotOutput("zad5", height = "600px"), width = 12))
                 ),
                 
                 
                 
                 
                 
                 
                 tabPanel("Zadanie 6",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         
                                         selectInput(inputId = "choice_6_pogoda",
                                                     "Wybierz warunki pogodowe",
                                                     pogoda,
                                                     selected = "Wiatr"),
                                         
                                         selectInput(inputId = "Stacja6", 
                                                     label = "Stacja",
                                                     choices = stacje)),
                            
                            mainPanel(plotOutput("zad6", height = "600px"), width = 12))
                 ),
                 
                 
                 tabPanel("Zadanie 7",
                          sidebarLayout(
                            sidebarPanel(width = 3,
                                         
                                         selectInput(inputId = "choice_7_pogoda",
                                                     "Wybierz warunki pogodowe",
                                                     pogoda,
                                                     selected = "Wiatr"),
                                         
                                         selectizeInput(inputId = "Stacje_7", 
                                                        label = "Wybierz maksymalnie 4 stacje",
                                                        choices = stacje,
                                                        multiple = TRUE,
                                                        options = list(maxItems = 4),
                                                        selected = "Pas Nadmorski")),
                            
                            mainPanel(plotOutput("zad7", height = "600px"), width = 12))
                 ),
                 
                 
                 
                 
                 
                 
)

# Define server logic
server <- function(input, output) {
  
  output$zad1 <- renderPlot({    
    ggplot(przejazdy, aes(x = Stacja, fill = Stacja)) +
      geom_bar() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 18), 
                         axis.text.y = element_text(size = 18), axis.title=element_text(size=18, face="bold"),
                         plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none") +
      labs(title="Rozkład liczby dni pomiarowych w poszczególnych punktach", y = "Liczba dni pomiarowych")
  })
  
  
  output$zad2 <- renderPlot({
    df.pkt2 <- przejazdy %>% filter(Stacja == input$Stacja) %>% select(Licznik)
    ggplot(df.pkt2, aes(x = Licznik)) +
      geom_histogram(fill="blue") + theme(axis.text.x = element_text(size = 18), 
                                          axis.text.y = element_text(size = 18), axis.title=element_text(size=18, face="bold"),
                                          plot.title = element_text(size=22, face="bold", hjust = 0.5)) +
      labs(title=paste("Rozkład liczby przejazdów dla stacji",input$Stacja), x = "Liczba przejazdów", y = "Częstość")
  })
  
  
  output$zad3 <- renderPlot({
    st3 <- c(strsplit(paste(input$Stacje3,collapse = ","), ",")[[1]])
    df.pkt3 <- melt(przejazdy %>% filter(Stacja == st3) %>% select(Stacja, Licznik))
    ggplot(df.pkt3, aes(x = value, fill = Stacja)) +                       
      geom_histogram(position = "identity", alpha = 0.2, bins = 50) + theme(axis.text.x = element_text(size = 18), 
                                                                            axis.text.y = element_text(size = 18), axis.title=element_text(size=18, face="bold"),
                                                                            plot.title = element_text(size=22, face="bold", hjust = 0.5),
                                                                            legend.text = element_text(size=14),
                                                                            legend.title = element_text(size=18),
                                                                            legend.key.size = unit(1, 'cm')) +
      labs(title=paste("Rozkład liczby przejazdów dla",length(st3),"stacji"), x = "Liczba przejazdów", y = "Częstość")
  })
  
  
  output$zad4 <- renderPlot({
    if(input$choice == 'Miesiąc'){
      df.pkt4 <- przejazdy %>% filter(Stacja == input$Stacja2) %>% select(Licznik, Miesiac)
      ggplot(df.pkt4, aes(x = Miesiac, y = Licznik, fill = Miesiac)) +
        geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 18), 
                                          axis.text.y = element_text(size = 18), axis.title=element_text(size=18, face="bold"),
                                          plot.title = element_text(size=22, face="bold", hjust = 0.5),
                                          legend.position = "none") +
        labs(title=paste("Rozkład liczby przejazdów na przestrzeni miesięcy dla stacji",input$Stacja2), x = "Miesiąc", y = "Liczba przejazdów") + scale_color_viridis(discrete = TRUE)}
    else if(input$choice == 'Dzień tygodnia'){
      df.pkt4 <- przejazdy %>% filter(Stacja == input$Stacja2) %>% select(Licznik, Dzien)
      ggplot(df.pkt4, aes(x = Dzien, y = Licznik, fill = Dzien)) +
        geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 18), 
                                          axis.text.y = element_text(size = 18), axis.title=element_text(size=18, face="bold"),
                                          plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none") +
        labs(title=paste("Rozkład liczby przejazdów na przestrzeni dni tygodnia dla stacji",input$Stacja2), x = "Dzień tygodnia", y = "Liczba przejazdów")}
    else {      df.pkt4 <- przejazdy %>% filter(Stacja == input$Stacja2) %>% select(Licznik, Typ)
    ggplot(df.pkt4, aes(x = Typ, y = Licznik, fill = Typ)) +
      geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 18), 
                                        axis.text.y = element_text(size = 18), axis.title=element_text(size=18, face="bold"),
                                        plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none") +
      labs(title=paste("Rozkład liczby przejazdów dla typów dnia tygodnia dla stacji",input$Stacja2), x = "Typ dnia tygodnia", y = "Liczba przejazdów")}
  })
  
  
  output$zad5 <- renderPlot({
    if(input$choice_5 == 'Miesiąc'){
      
      st5 <- c(strsplit(paste(input$Stacje_5, collapse = ","), ",")[[1]])
      df.pkt5 <- melt(przejazdy %>% filter(Stacja == st5) %>% select(Stacja, Licznik, Miesiac))
      
      ggplot(df.pkt5, aes(x = Miesiac, y = value, fill = Stacja)) +
        geom_bar(stat="identity", position='dodge') +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 18), 
              axis.text.y = element_text(size = 18), axis.title=element_text(size=18, face="bold"),
              plot.title = element_text(size=22, face="bold", hjust = 0.5)) +
        labs(title=paste("Rozkład liczby przejazdów na przestrzeni miesięcy dla", length(st5), "stacji"), x = "Miesiąc", y = "Liczba przejazdów")}
    
    
    
    else if(input$choice_5 == 'Dzień tygodnia'){
      
      st5 <- c(strsplit(paste(input$Stacje_5, collapse = ","), ",")[[1]])
      df.pkt5 <- melt(przejazdy %>% filter(Stacja == st5) %>% select(Stacja, Licznik, Dzien))
      
      ggplot(df.pkt5, aes(x = Dzien, y = value, fill = Stacja)) +
        geom_bar(stat="identity", position='dodge') +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 18), 
              axis.text.y = element_text(size = 18), axis.title=element_text(size=18, face="bold"),
              plot.title = element_text(size=22, face="bold", hjust = 0.5)) +
        labs(title=paste("Rozkład liczby przejazdów dla dnia tygodnia dla", length(st5), "stacji"), x = "Dzień Tygodnia", y = "Liczba przejazdów")}
    
    
    else {
      st5 <- c(strsplit(paste(input$Stacje_5, collapse = ","), ",")[[1]])
      df.pkt5 <- melt(przejazdy %>% filter(Stacja == st5) %>% select(Stacja, Licznik, Typ))
      
      ggplot(df.pkt5, aes(x = Typ, y = value, fill = Stacja)) +
        geom_bar(stat="identity", position='dodge') +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 18), 
              axis.text.y = element_text(size = 18), axis.title=element_text(size=18, face="bold"),
              plot.title = element_text(size=22, face="bold", hjust = 0.5)) +
        labs(title=paste("Rozkład liczby przejazdów dla typu dnia miesiąca dla", length(st5), "stacji"), x = "Dzień Tygodnia", y = "Liczba przejazdów")}
    
    
  })
  
  output$zad6 <- renderPlot({
    
    df.pkt6 <- przejazdy %>% filter(Stacja == input$Stacja6) %>% select(Licznik, pogoda = !!input$choice_6_pogoda)
    
    ggplot(df.pkt6, aes(x=pogoda, y=Licznik)) + 
      geom_point(size=2)+
      theme(axis.text.x = element_text(size = 18), 
            axis.text.y = element_text(size = 18), axis.title=element_text(size=18, face="bold"),
            plot.title = element_text(size=22, face="bold", hjust = 0.5)) +
      labs(title=paste("Zależność liczby przejazdów od zmiennej", input$choice_6_pogoda, "dla stacji",input$Stacja6), x = input$choice_6_pogoda, y = "Liczba przejazdów")
    
    
  })
  
  output$zad7 <- renderPlot({
    
    st7 <- c(strsplit(paste(input$Stacje_7, collapse = ","), ",")[[1]])
    df.pkt7 <- przejazdy %>% filter(Stacja == st7) %>% select(Licznik, Stacja, pogoda = !!input$choice_7_pogoda)
    
    ggplot(df.pkt7, aes(x=pogoda, y=Licznik, color=Stacja)) + 
      geom_point(size=2)+
      theme(axis.text.x = element_text(size = 18), 
            axis.text.y = element_text(size = 18), axis.title=element_text(size=18, face="bold"),
            plot.title = element_text(size=22, face="bold", hjust = 0.5)) +
      labs(title=paste("Zależność liczby przejazdów dla",length(st7),"stacji od zmiennej", input$choice_7_pogoda), x = input$choice_7_pogoda, y = "Liczba przejazdów")
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

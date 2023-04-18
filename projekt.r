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

ui <- navbarPage("Navbar!",
                 tabPanel("Zadanie 1",
                          mainPanel(
                            plotOutput("zad1", height = "800px"), width = 12
                          )),
                  tabPanel("Zadanie 2",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput(inputId = "Stacja", 
                                    label = "Stacja",
                                     choices = stacje)
                                     ),
                         mainPanel(plotOutput("zad2", height = "800px"))
                      ),
                  ),
                 
                 tabPanel("Zadanie 3",
                          sidebarLayout(
                            sidebarPanel(
                              selectizeInput(inputId = "Stacje3", 
                                          label = "Wybierz maksymalnie 4 stacje",
                                          choices = stacje,
                                          multiple = TRUE,
                                          options = list(maxItems = 4),
                                          selected = "Pas Nadmorski")
                            ),
                            mainPanel(
                              plotOutput("zad3", height = "800px"), width = 12
                              )
                          ),
                          ),
                 
                 tabPanel("Zadanie 4",
                          sidebarLayout(
                            sidebarPanel(width = 1,    
                              radioButtons("choice", "Wybierz typ podziału danych",
                                           c("Miesiąc", "Dzień miesiąca", "Typ dnia miesiąca"), selected = "Miesiąc"),
                              selectInput(inputId = "Stacja2", 
                                          label = "Stacja",
                                          choices = stacje)),
                            
                            mainPanel(plotOutput("zad4", height = "600px"), width = 2))),
                 
                 tabPanel("Zadanie 5",
                          mainPanel(
                            plotOutput("zad5", height = "800px"), width = 12
                          )),
                 
                 tabPanel("Zadanie 6",
                          mainPanel(
                            plotOutput("zad6", height = "800px"), width = 12
                          )),
                 
                 tabPanel("Zadanie 7",
                          mainPanel(
                            plotOutput("zad7", height = "800px"), width = 12
                          )),
                 
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
        labs(title=paste("Rozkład liczby przejazdów na przestrzeni miesięcy dla stacji",input$Stacja), x = "Miesiąc", y = "Liczba przejazdów") + scale_color_viridis(discrete = TRUE)}
    else if(input$choice == 'Dzień miesiąca'){
      df.pkt4 <- przejazdy %>% filter(Stacja == input$Stacja2) %>% select(Licznik, Dzien)
      ggplot(df.pkt4, aes(x = Dzien, y = Licznik, fill = Dzien)) +
        geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 18), 
                                 axis.text.y = element_text(size = 18), axis.title=element_text(size=18, face="bold"),
                                 plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none") +
        labs(title=paste("Rozkład liczby przejazdów na przestrzeni dni tygodnia dla stacji",input$Stacja), x = "Dzień tygodnia", y = "Liczba przejazdów")}
    else {      df.pkt4 <- przejazdy %>% filter(Stacja == input$Stacja2) %>% select(Licznik, Typ)
    ggplot(df.pkt4, aes(x = Typ, y = Licznik, fill = Typ)) +
      geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 18), 
                               axis.text.y = element_text(size = 18), axis.title=element_text(size=18, face="bold"),
                               plot.title = element_text(size=22, face="bold", hjust = 0.5), legend.position = "none") +
      labs(title=paste("Rozkład liczby przejazdów dla typów dnia tygodnia dla stacji",input$Stacja), x = "Typ dnia tygodnia", y = "Liczba przejazdów")}
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)

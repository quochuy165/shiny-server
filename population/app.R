# Library ------------------
library(XML)
library(reshape2)
library(tidyverse)
library(highcharter)
library(readxl)
library(haven)

# Read data ---------------
dieutradanso1999 <- read_excel("dieutrads1999.xlsx", sheet = 2,
                               col_names = TRUE)

dieutradanso2009 <- read_excel("dieutrads2009.xlsx",
                               col_names = TRUE)

dieutradanso2016 <- read_dta("vhlss_2016.dta")

# Data --------------

dieutradanso1999$Male <- dieutradanso1999$Male *(-1)
dieutradanso2009$Male <- dieutradanso2009$Male *(-1)

dieutradanso2016$age <- cut(dieutradanso2016$m1ac5 , breaks = seq(0, 100, 5), right = FALSE) 
dieutradanso2016 <- dieutradanso2016 %>% drop_na()
dieutradanso2016 <- dieutradanso2016 %>% separate(age, into = c("x","y","z","t","m"))

dieutradanso2016 <- dieutradanso2016[order(as.numeric(dieutradanso2016$y)),]
dieutradanso2016 <- dieutradanso2016[order(as.numeric(dieutradanso2016$z)),]

dieutradanso2016$tuoi <- paste(dieutradanso2016$y, dieutradanso2016$z, sep = "-")
dieutradanso2016 <- dieutradanso2016 %>% select(m1ac2, tuoi)
dieutradanso2016$Population <- 1
dieutradanso2016$m1ac2 <- ifelse(dieutradanso2016$m1ac2 == 1, "Male", "Female")
dieutradanso2016 <- dieutradanso2016 %>% group_by(m1ac2, tuoi) %>% summarise(n = n()) %>% ungroup()
dieutradanso2016 <- dieutradanso2016 %>% spread(m1ac2,n)
dieutradanso2016$Male <- dieutradanso2016$Male*(-1)

# UI -------------------

ui <- fluidPage(
  # head --------------
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                    
                    h1 {
                    font-family: 'Lobster', cursive;
                    font-weight: 500;
                    line-height: 1.1;
                    color: green;
                    } 
                    body {
                    background-color: #fff;
                    }
                    ")),
    tags$link(rel = "stylesheet", type = "text/css", href = "dygraph.css")
  ),
  headerPanel("Population pyramid for VietNam between 1999 and 2016"),
  sidebarLayout(
    sidebarPanel(
      selectInput("anno", "Year:", choices = c("1999", "2009", "2016")
                  )
      ), 
    mainPanel(highchartOutput("pyramid")
              )
  )
)

server <- function(input, output) {
  output$pyramid <- renderHighchart({
    hc <- highchart() %>% 
      hc_chart(type = "bar") %>% 
      hc_subtitle(text = "Source: GSO.com") %>% 
      hc_yAxis(title = list(text = "Population"),lineWidth = 0, labels = list(
        formatter = JS("function () {return Math.abs(this.value);}"))) %>% 
      hc_plotOptions(series = list(stacking = "normal")) %>% 
      hc_tooltip(formatter = JS("function () {
        return '<b>' + this.series.name + ', age ' + this.point.category + '</b><br/>' +
          'Population: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);}")) %>% 
      hc_add_theme(hc_theme_sandsignika())
    if (input$anno == "1999"){
      hc %>% 
        hc_title(text = "Population pyramid for VietNam, 1999") %>% 
        hc_xAxis(categories = dieutradanso1999$tuoi,reversed = FALSE) %>% 
        hc_add_series(name = "Male", data = dieutradanso1999$Male) %>% 
        hc_add_series(name = "Female", data = dieutradanso1999$Female)
      } else if (input$anno == "2009"){
        hc %>% 
          hc_title(text = "Population pyramid for VietNam, 2009") %>% 
          hc_xAxis(categories = dieutradanso2009$tuoi,reversed = FALSE) %>% 
          hc_add_series(name = "Male", data = dieutradanso2009$Male) %>% 
          hc_add_series(name = "Female", data = dieutradanso2009$Female)
      } else {
        hc %>% 
          hc_title(text = "Population pyramid for VietNam, 2016") %>% 
          hc_subtitle(text = "Source: VHLSS") %>%
          hc_xAxis(categories = dieutradanso2016$tuoi,reversed = FALSE) %>% 
          hc_add_series(name = "Male", data = dieutradanso2016$Male) %>% 
          hc_add_series(name = "Female", data = dieutradanso2016$Female)
      }
  })
}

shinyApp(ui,server)


library(shiny)
library(highcharter)
library(tidyverse)
library(raster)
library(leaflet)
library(readxl)
library(colourpicker)
library(shinydashboardPlus)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)

# Call data ---------------------------------------------------------------
gvpt <- read_excel("data.xlsx", sheet = "gvpt_30-9")
th_lh_phothong <- read_excel("data.xlsx", sheet = "th_lh_phothong_30-9")
nugv_nuhs <- read_excel("data.xlsx", sheet = "nugv_nuhs_30-9")
hv_saudh <- read_excel("data.xlsx", sheet = "hv_saudh")
hsbq <- read_excel("data.xlsx", sheet = "hsbq")
gv_theotrinhdo <- read_excel("data.xlsx", sheet = "gv_theotrinhdo")
GD_maugiao <- read_excel("data.xlsx", sheet = "GD_maugiao_30-9")
gv_hspt <- read_excel("data.xlsx", sheet = "gv_hspt_30-9")
gddh_cd <- read_excel("data.xlsx", sheet = "gddh_cd")


# map data Vietnam --------------------------------------------------------

vn<-getData(name="GADM",country="Vietnam",level=1)
vn <- vn[order(vn$VARNAME_1),]

#iOrder data
gvpt <- gvpt[order(gvpt$Province),]
vn$Province <- gvpt$Province
vn1 <- merge(vn, gvpt, by = "Province")

gv_sv_dhcd <- read_excel("data.xlsx", sheet = "gv_sv_dhcd")
gv_sv_dhcd <- gv_sv_dhcd[order(gv_sv_dhcd$Province),]

vn2 <- merge(vn, gv_sv_dhcd)


gv_sv_dhcd <-  gv_sv_dhcd %>% gather(key = "group", value = "value", -c("province", "Province")) %>%
  separate(col = group, into = c("group", "year"), sep = " - ")

gvpt <-  gvpt %>% gather(key = "group", value = "value", -c("province", "Province")) %>%
  separate(col = group, into = c("group", "year"), sep = " - ")



# Colors palette ----------------------------------------------------------

palette <- c(rownames(RColorBrewer::brewer.pal.info), "viridis", "magma", "inferno", "plasma")

# index Selection ---------------------------------------------------------


index <- c("Uiversity and college education" = "unc", "Number of teachers and pupils of general education" = "tnp",
           "Kindergarten education" = "ke", "Number of teachers in universities and colleges by professional qualification" = "teacher_qual",
           "Average number of pupils per class and average number of pupils per teacher of general education" = "avg_",
           "Number of students participated in post-graduate and medical specialized training" = "pg_ms",
           "Number of woman teachers and schoolgirls of general schools" = "woman",
           "Number of schools and classes of general education" = "snc")
index_1 <- c("Number of direct teaching teachers of general education by province" = "direct_teaching",
             "Number of teachers, students in universities and colleges by province" = "tns_p")


# UI header ---------------------------------------------------------------

header <- dashboardHeaderPlus(title = tagList(
  tags$head(
    includeScript(
      "www/rightsidebar.js"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  span(class = "logo-lg", "Education in VN"),
  img(src = 'https://image.flaticon.com/icons/svg/182/182321.svg', width = "30", height = "30")),
  enable_rightsidebar = TRUE,
  rightSidebarIcon = "sliders-h",
  left_menu = tagList(
    dropdownBlock(
      id = "dropdowm",
      title = "Pick up color",
      conditionalPanel(
        condition = "input.menu == 'graphs'",
        column(3, colourInput("a", NULL, "#ff9a00","background")),
        column(3, colourInput("b", NULL, "#85ef47","background")),
        column(3,conditionalPanel(condition = "input.data != 'Tỷ lệ' && input.index != 'ke' &&
                              input.index != 'avg_'" ,
                                  colourInput("c", NULL, "#14ffec","background"))),
        column(3,conditionalPanel(condition = "(input.index == 'tnp' && !input.data1) ||
                              (input.index == 'teacher_qual' && !input.data3) ||
                              (input.index == 'woman' && !input.data6) ||
                              (input.index == 'snc' && !(input.type7 == 'Lớp' && input.data7))",
                                  colourInput("d", NULL, "#163172", "background"))),
        column(3,conditionalPanel(condition = "input.index == 'snc' && !(input.data7)",
                                  colourInput("e",NULL, "#ff165d", "background")))
      ),
      conditionalPanel(
        condition = "input.menu == 'maps'",
        selectInput("pal", "Map Palette:",choices = palette, selected = "viridis"),
        h4(strong("Graph Color:")),
          column(3, colourInput("a_1", NULL, "#2f89fc","background")),
        conditionalPanel(
          condition = "input.index_1 == 'tns_p'",
          column(3, colourInput("a_2", NULL, "#ffd700","background"))
        )
      )
    )
  )
)


# UI sidebar --------------------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(id = "menu",
              menuItem("Graphs", tabName = "graphs", icon = icon("chart-line ")),
              menuItem("Maps", tabName = "maps", icon = icon("map")),
              menuItem("Data", tabName = "data", icon = icon("database ")),
              menuItem("About", tabName = "about", icon = icon("info" ))
  )
)


# UI body -----------------------------------------------------------------

body <- dashboardBody(
  shinyDashboardThemes(
    theme = "blue_gradient"
  ),
  chooseSliderSkin("Modern"),
  tabItems(
    #Graphs tab
    tabItem(tabName = "graphs",
            highchartOutput("hc",width = "100%", height = "600px")
    ),
    #Maps tab
    tabItem(tabName = "maps",
            column(4,plotOutput("plot")),
            column(8,leafletOutput("map", width = "100%", height= "600px"))
    ),
    #Data tab
    tabItem(tabName = "data",
            h2(HTML("Data source from <a href='https://www.gso.gov.vn/Default_en.aspx?tabid=491'>GSO Vietnam</a>")), br(),
            h3(HTML("Script and data can be downloaded <a href='https://google.com'>here</a>"))
    ),
    tabItem(tabName = "about",
            h4("Some information")
    )
  )
)


# UI rightsidebar ---------------------------------------------------------

rightsidebar <- rightSidebar(
  background = "light",
  conditionalPanel(
    condition = "input.menu == 'graphs'",
    strong(h4(selectInput("index", "Pick an index", choices = index, selectize = FALSE))),
    conditionalPanel(
      condition = "input.index == 'unc'",
      selectInput(inputId = "type", label = strong("Select a plot"),
                  choices = c("Trường học", "Giáo viên", "Sinh viên",
                              "Sinh viên tốt nghiệp"), selectize = FALSE),
      conditionalPanel(condition = "input.type == 'Giáo viên' || input.type == 'Sinh viên'",
                       checkboxInput("kind", "By Gender")),
      prettyRadioButtons(inputId = "data",
                         label = "Pick plot", icon = icon("check"),
                         choices = c("Absolute", "Proportion"),
                         animation = "tada", status = "warning"),

      sliderInput("nam", "Select range year",
                  min = min(gddh_cd$year), max = max(gddh_cd$year), value = c(2005,2010), step = 1, sep = "")
    ),
    conditionalPanel(
      condition = "input.index == 'tnp'",
      selectInput(inputId = "type1", label = strong("Select a plot"),
                  choices = c("Giáo viên" = "Giáo viên",
                              "Học sinh" = "Học sinh"), selectize = FALSE),
      prettyToggle(
        inputId = "data1",
        label_on = "Proportion",
        label_off = "Absolute",
        icon_on = icon("percent"),
        icon_off = icon("draft2digital")),

      sliderInput("nam1", "Select range year",
                  min = min(gv_hspt$year), max = max(gv_hspt$year), value = c(2004,2012), step = 1, sep = "")
    ),
    conditionalPanel(
      condition = "input.index == 'ke'",
      selectInput(inputId = "type2", label = strong("Select a plot"),
                  choices = colnames(GD_maugiao[,c(2:7)]), selectize = FALSE),
      sliderInput("nam2", "Select range year",
                  min = min(GD_maugiao$year), max = max(GD_maugiao$year), value = c(2004,2012), step = 1, sep = "")
    ),
    conditionalPanel(
      condition = "input.index == 'teacher_qual'",
      selectInput(inputId = "type3", label = strong("Select a plot"),
                  choices = c(
                    "tổng", "công lập", "ngoài công lập"
                  ), selectize = FALSE),
      prettyToggle(
        inputId = "data3",
        label_on = "Proportion",
        label_off = "Absolute",
        icon_on = icon("percent"),
        icon_off = icon("draft2digital")),
      sliderInput("nam3", "Select range year",
                  min = min(gv_theotrinhdo$year), max = max(gv_theotrinhdo$year), value = c(2004,2012), step = 1, sep = "")
    ),
    conditionalPanel(
      condition = "input.index == 'avg_'",
      selectInput("type4", "Select plot",
                  choices = c("Tổng số",
                              "Tiểu học" = "tiểu học",
                              "Trung học cơ sở" = "trung học cơ sở",
                              "Trung học phổ thông" = "trung học phổ thông"), selectize = FALSE),
      sliderInput("nam4", "Select range year",
                  min = min(hsbq$year), max = max(hsbq$year), value = c(2011,2018), step = 1, sep = "")
    ),
    conditionalPanel(
      condition = "input.index == 'pg_ms'",
      selectInput("type5", "select plot:",
                  choices = c(
                    "Số học viên được đào tạo sau đại học",
                    "Số học viên tốt nghiệp sau đại học",
                    "Số học viên được đào tạo chuyên khoa y",
                    "Số học viên tốt nghiệp chuyên khoa Y"
                  ), selectize = FALSE),
      sliderInput("nam5", "Select range year",
                  min = min(hv_saudh$year), max = max(hv_saudh$year), value = c(2011,2018), step = 1, sep = "")
    ),
    conditionalPanel(
      condition = "input.index == 'woman'",
      selectInput(inputId = "type6", label = strong("Select a plot"),
                  choices = c("Nữ giáo viên" = "nữ giáo viên",
                              "Nữ học sinh" = "nữ học sinh",
                              "Nữ học sinh bình quân một giáo viên nữ"="nữ học sinh/nữ giáo viên"), selectize = FALSE),
      sliderInput("nam6", "Select range year",
                  min = min(nugv_nuhs$year), max = max(nugv_nuhs$year), value = c(2004,2012), step = 1, sep = ""),
      conditionalPanel(condition = "input.type6 != 'nữ học sinh/nữ giáo viên'",
                       prettyToggle(
                         inputId = "data6",
                         label_on = "Proportion",
                         label_off = "Absolute",
                         icon_on = icon("percent"),
                         icon_off = icon("draft2digital"))
      )),
    conditionalPanel(
      condition = "input.index == 'snc'",
      selectInput(inputId = "type7", label = strong("Select a plot"),
                  choices = c("Theo trường" = "Trường",
                              "Theo lớp" = "Lớp"), selectize = FALSE),
      sliderInput("nam7", "Select range year",
                  min = min(th_lh_phothong$year), max = max(th_lh_phothong$year), value = c(2004,2012), step = 1, sep = ""),
      column(6,checkboxInput("y7", "Index")),
      column(6,prettyToggle(
        inputId = "data7",
        label_on = "Proportion",
        label_off = "Absolute",
        icon_on = icon("percent"),
        icon_off = icon("draft2digital")))
    )
  ),
  conditionalPanel(
    condition = "input.menu == 'maps'",
    strong(h4(selectInput("index_1", "Pick an index", choices = index_1, selectize = FALSE))),
    conditionalPanel(
      condition = "input.index_1 == 'direct_teaching'",
      selectInput("level", "Select level:",
                  choices = c("All" = "Total",
                              "Primary school" = "Primary",
                              "Junior high school" = "Junior",
                              "High school" = "High school"), selectize = FALSE),
      sliderInput("year", "Select year:",
                  value = 2002, 2002, 2017, step = 1,
                  sep = "",
                  animate = animationOptions(interval = 2000,
                                             loop = T
                  ))
    ),
    conditionalPanel(
      condition = "input.index_1 == 'tns_p'",
      selectInput("map", "Select map:", choices = c("Giáo viên", "Sinh viên"), selectize = FALSE),
      numericInput("namm","Select year to plot to:", 2010, min = 2001, max = 2017, step = 1)
    )
  )
    )


# ui ----------------------------------------------------------------------

ui <- dashboardPagePlus(header, sidebar, body,
                        rightsidebar = rightsidebar
)

# Server ------------------------------------------------------------------

server <- function(input, output) {

  output$hc <- renderHighchart({
    if (input$index == "tnp") {
      data <- gv_hspt %>% filter(year >= min(input$nam1) & year <= max(input$nam1))
      hc <-  highchart() %>%
        hc_exporting(enabled = TRUE) %>%
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   borderWidth = 2, shared = TRUE) %>%
        hc_xAxis(categories = data$year) %>%
        hc_title(text = paste0(input$type1, " qua các cấp học"), align = "center") %>%
        hc_add_theme(hc_theme_economist())
      if (input$data1) {
        hc %>% hc_yAxis(title = list(text = "Tỷ lệ (%)")) %>%
          hc_add_series(type = "area",color = input$a, data = unlist(data[,paste0("Tỷ lệ ", input$type1, " tiểu học")], use.names = F), name = "Tiểu học") %>%
          hc_add_series(type = "area",color = input$b, data = unlist(data[,paste0("Tỷ lệ ", input$type1, " trung học cơ sở")], use.names = F), name = "Trung học cơ sở") %>%
          hc_add_series(type = "area",color = input$c, data = unlist(data[,paste0("Tỷ lệ ", input$type1, " trung học phổ thông")], use.names = F), name = "Trung học phổ thông") %>%
          hc_plotOptions(area = list(
            dataLabels = list(enabled = TRUE),
            stacking = "percent"
          ))
      }
      else {
        if (input$type1 == "Giáo viên") {
          hc %>%       hc_yAxis_multiples(list(title = list(text = "Quantity (nghìn người"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Grow"),labels = list(format = "{value}%"), opposite = TRUE)) %>%
            hc_add_series(type ="column",color = input$a, data = data$"Tổng số Giáo viên", name = "Tổng", yAxis = 0) %>%
            hc_add_series(type = "column",color = input$b, data = data$"Giáo viên tiểu học", name = "Tiểu học", yAxis = 0) %>%
            hc_add_series(type = "column",color = input$c,  data = data$"Giáo viên trung học cơ sở", name = "Trung học cơ sở", yAxis = 0) %>%
            hc_add_series(type = "column",color = input$d,  data = data$"Giáo viên trung học phổ thông", name = "Trung học phổ thông", yAxis = 0) %>%
            hc_add_series(type ="line",color = input$a, data = data$"Tăng giáo viên", name = "Tổng", yAxis = 1) %>%
            hc_add_series(type = "line",color = input$b, data = data$"Tăng giáo viên tiểu học", name = "Tiểu học", yAxis = 1) %>%
            hc_add_series(type = "line",color = input$c,  data = data$"Tăng giáo viên trung học cơ sở", name = "Trung học cơ sở", yAxis = 1) %>%
            hc_add_series(type = "line",color = input$d,  data = data$"Tăng giáo viên trung học phổ thông", name = "Trung học phổ thông", yAxis = 1)

        }
        else
        {
          hc %>%
            hc_yAxis_multiples(list(title = list(text = "Quantity (nghìn người"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Grow"),labels = list(format = "{value}%"), opposite = TRUE)) %>%
            hc_add_series(type ="column",color = input$a, data = data$"Tổng số Học sinh", name = "Tổng", yAxis = 0) %>%
            hc_add_series(type = "column",color = input$b, data = data$"Học sinh tiểu học", name = "Tiểu học", yAxis = 0) %>%
            hc_add_series(type = "column",color = input$c,  data = data$"Học sinh trung học cơ sở", name = "Trung học cơ sở", yAxis = 0) %>%
            hc_add_series(type = "column",color = input$d,  data = data$"Học sinh trung học phổ thông", name = "Trung học phổ thông", yAxis = 0) %>%
            hc_add_series(type ="line",color = input$a, data = data$"Tăng học sinh", name = "Tổng", yAxis = 1) %>%
            hc_add_series(type = "line",color = input$b, data = data$"Tăng học sinh tiểu học", name = "Tiểu học", yAxis = 1) %>%
            hc_add_series(type = "line",color = input$c,  data = data$"Tăng học sinh trung học cơ sở", name = "Trung học cơ sở", yAxis = 1) %>%
            hc_add_series(type = "line",color = input$d,  data = data$"Tăng học sinh trung học phổ thông", name = "Trung học phổ thông", yAxis = 1)
        }
      }
    }
    else if (input$index == "unc") {
      gddh_cd <- filter(gddh_cd, year != 2016)
      gddh_cd <- filter(gddh_cd, year != 2017)
      df <-  gddh_cd %>% filter(year >= min(input$nam) & year <= max(input$nam))
      h <-  highchart() %>%
        hc_xAxis(categories = df$year) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 2) %>%
        hc_title(text="Giáo dục bậc đại học, cao đẳng",align="center") %>%
        hc_add_theme(hc_theme_economist())
      if (input$data == "Absolute") {
        if (input$kind) {
          h %>% hc_yAxis_multiples(list(title = list(text = "Quantity (nghìn người)"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Grow"),labels = list(format = "{value}%"), opposite = TRUE)) %>%
            hc_add_series(type = "column",color = input$a, data = unlist(df[,input$type], use.names = F), dataLabels = list(enabled = T), name = input$type, yAxis = 0) %>%
            hc_add_series(type = "line",color = input$a, data = unlist(df[,"Giáo viên"], use.names = F), dataLabels = list(enabled = T), name = "Chỉ số tăng", yAxis = 1) %>%
            hc_add_series(type = "column",color = input$b, data = unlist(df[,"Giáo viên nam"],use.names = F), dataLabels = list(enabled = T), name = "Nam", yAxis = 0) %>%
            hc_add_series(type = "line",color = input$b, data = unlist(df[,"Giáo viên nam tăng"],use.names = F),dataLabels = list(enabled = T), name = "Chỉ số tăng của nam", yAxis = 1) %>%
            hc_add_series(type = "column",color = input$c, data = unlist(df[,"Giáo viên nữ"],use.names = F),dataLabels = list(enabled = T), name = "Nữ", yAxis = 0, color = "orange") %>%
            hc_add_series(type = "line",color = input$c, data = unlist(df[,"Giáo viên nữ tăng"],use.names = F),dataLabels = list(enabled = T), name = "Chỉ số tăng của nữ", yAxis = 1)
        }
        else {
          h %>%  hc_yAxis_multiples(list(title = list(text = "Quantity (nghìn người)"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Grow"),labels = list(format = "{value}%"), opposite = TRUE)) %>%
            hc_add_series(type = "column",color = input$a, data = unlist(df[,input$type], use.names = F), dataLabels = list(enabled = T), name = input$type, yAxis = 0) %>%
            hc_add_series(type = "line",color = input$a, data = unlist(df[,paste0(input$type," tăng")], use.names = F), dataLabels = list(enabled = T), name = paste0("Chỉ số tăng ", input$type), yAxis = 1) %>%
            hc_add_series(type = "column",color = input$b, data = unlist(df[,paste0(input$type, " công lập")],use.names = F), dataLabels = list(enabled = T), name = paste0(input$type, " công lập"), yAxis = 0) %>%
            hc_add_series(type = "line",color = input$b, data = unlist(df[,paste0(input$type, " công lập", " tăng")],use.names = F),dataLabels = list(enabled = T), name = paste0("Chỉ số tăng ", input$type, " công lập"), yAxis = 1) %>%
            hc_add_series(type = "column",color = input$c, data = unlist(df[,paste0(input$type, " ngoài công lập")],use.names = F),dataLabels = list(enabled = T), name = paste0(input$type, " ngoài công lập"), yAxis = 0, color = "orange") %>%
            hc_add_series(type = "line",color = input$c, data = unlist(df[,paste0(input$type, " ngoài công lập", " tăng")],use.names = F),dataLabels = list(enabled = T), name = paste0("Chỉ số tăng ", input$type, " ngoài công lập"), yAxis = 1)
        }
      }
      else {
        if (input$kind) {
          h %>% hc_yAxis(title = list(text = "Tỷ lệ (%)")) %>%
            hc_add_series(type = "column",color = input$a, data = unlist(df[, paste0("Tỷ lệ ",input$type, " nam")],use.names = F), name = "Nam") %>%
            hc_add_series(type = "column",color = input$b, data = unlist(df[, paste0("Tỷ lệ ",input$type, " nữ")],use.names = F), name = "Nữ") %>%
            hc_plotOptions(
              column = list(
                dataLabels = list(enabled = TRUE),
                stacking = "normal")
            )
        }
        else {
          h %>% hc_yAxis(title = list(text = "Tỷ lệ (%)")) %>%
            hc_add_series(type = "column",color = input$a, data = unlist(df[, paste0("Tỷ lệ ",input$type, " công lập")],use.names = F), name = "Công lập") %>%
            hc_add_series(type = "column",color = input$b, data = unlist(df[, paste0("Tỷ lệ ",input$type, " ngoài công lập")],use.names = F), name = "Ngoài công lập") %>%
            hc_plotOptions(
              column = list(
                dataLabels = list(enabled = TRUE),
                stacking = "normal")
            )
        }
      }
    }
    else if (input$index == "ke") {
      data <- GD_maugiao %>% filter(year >= min(input$nam2) & year <= max(input$nam2))
      highchart() %>%
        hc_exporting(enabled = TRUE) %>%
        hc_tooltip(shared = TRUE, crosshairs = TRUE) %>%
        hc_xAxis(categories = data$year) %>%
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Grow"),labels = list(format = "{value}%"), opposite = TRUE)) %>%
        hc_title(text = "Giáo dục mầm non", align = "center") %>%
        hc_subtitle(text = "") %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_add_series(type ="column",color = input$a, data = unlist(data[,input$type2],use.names = F), name = input$type2, yAxis = 0, dataLabels = list(enabled = TRUE)) %>%
        hc_add_series(type ="line",color = input$b, data = unlist(data[,paste0("Tăng ",input$type2)],use.names = F), name = "Tăng trưởng", yAxis = 1,dataLabels = list(enabled = TRUE))
    }
    else if (input$index == "teacher_qual") {
      data <- gv_theotrinhdo %>% filter(year >= min(input$nam3) & year <= max(input$nam3))
      if (input$data3) {
        highchart() %>%
          hc_exporting(enabled = TRUE) %>%
          hc_add_theme(hc_theme_economist()) %>%
          hc_xAxis(categories = data$year) %>%
          hc_yAxis(title = list(text = "Tỷ lệ (%)")) %>%
          hc_title(text = "Tỷ lệ giáo viên theo bằng cấp", align = "center") %>%
          hc_plotOptions(
            column = list(
              dataLabels = list(enabled = TRUE),
              stacking = "normal")) %>%
          hc_tooltip(shared = TRUE, crosshairs = TRUE) %>%
          hc_add_series(type = "column",color = input$a, data = unlist(data[,paste0("Tỷ lệ trên đại học_",input$type3)], use.names = F), name = "Trình độ trên đại học") %>%
          hc_add_series(type = "column",color = input$b, data = unlist(data[,paste0("Tỷ lệ đại học cao đẳng_",input$type3)], use.names = F), name = "Trình độ đại học cao đẳng") %>%
          hc_add_series(type = "column",color = input$c, data = unlist(data[,paste0("Tỷ lệ trình độ khác_",input$type3)], use.names = F), name = "Trình độ khác")

      }
      else {
        highchart() %>%
          hc_add_theme(hc_theme_economist()) %>%
          hc_exporting(enabled = TRUE) %>%
          hc_tooltip(shared = TRUE, crosshairs = TRUE) %>%
          hc_xAxis(categories = data$year) %>%
          hc_add_theme(hc_theme_economist()) %>%
          hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Grow"),labels = list(format = "{value}%"), opposite = TRUE)) %>%
          hc_title(text = "Số lượng giáo viên theo bằng cấp", align = "center") %>%
          hc_add_series(type ="column",color = input$a, data = unlist(data[,paste0("Tổng số_",input$type3)], use.names = F), name = "Tổng", yAxis = 0) %>%
          hc_add_series(type ="column",color = input$b, data = unlist(data[,paste0("Trên đại học_",input$type3)], use.names = F), name = "Trên đại học", yAxis = 0) %>%
          hc_add_series(type ="column",color = input$c, data = unlist(data[,paste0("Đại học. cao đẳng_",input$type3)], use.names = F), name = "Đại học cao đẳng", yAxis = 0) %>%
          hc_add_series(type ="column",color = input$d, data = unlist(data[,paste0("Trình độ khác_",input$type3)], use.names = F), name = "Trình độ khác", yAxis = 0) %>%
          hc_add_series(type ="line",color = input$a, data = unlist(data[,paste0("Tổng số_",input$type3, " tăng")], use.names = F), name = "Tổng", yAxis = 1) %>%
          hc_add_series(type ="line",color = input$b, data = unlist(data[,paste0("Trên đại học_",input$type3, " tăng")], use.names = F), name = "Trên đại học", yAxis = 1) %>%
          hc_add_series(type ="line",color = input$c, data = unlist(data[,paste0("Đại học. cao đẳng_",input$type3, " tăng")], use.names = F), name = "Đại học cao đẳng", yAxis = 1) %>%
          hc_add_series(type ="line",color = input$d, data = unlist(data[,paste0("Trình độ khác_",input$type3, " tăng")], use.names = F), name = "Trình độ khác", yAxis = 1)
      }
    }
    else if (input$index == "avg_") {
      df <-  hsbq %>% filter(year >= min(input$nam4) & year <= max(input$nam4))
      h <-  highchart() %>%
        hc_xAxis(categories = df$year) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 2) %>%
        hc_title(text="Số lượng học sinh trung bình",align="center") %>%
        hc_add_theme(hc_theme_elementary()) %>%
        hc_yAxis(title = list(text = "Số lượng")) %>%
        hc_add_theme(hc_theme_economist())
      if (input$type4 == "Tổng số") {
        h %>% hc_add_series(type = "column",color = input$a, data = df$"Tổng số Bình quân một lớp học",dataLabels = list(enabled = T), name = "Số học sinh bình quân một lớp học") %>%
          hc_add_series(type = "column",color = input$b, data = df$"Tổng số Bình quân một giáo viên",dataLabels = list(enabled = T), name = "Số học sinh bình quân một giáo viên")
      }
      else {
        h %>% hc_add_series(type = "column",color = input$a, data = unlist(df[,paste0("Bình quân một lớp học ", input$type4)], use.names = FALSE),dataLabels = list(enabled = T), name = "Số học sinh bình quân một lớp học") %>%
          hc_add_series(type = "column",color = input$b, data = unlist(df[,paste0("Bình quân một giáo viên ", input$type4)], use.names = FALSE),dataLabels = list(enabled = T), name = "Số học sinh bình quân một giáo viên")
      }
    }
    else if (input$index == "pg_ms") {
      df <-  hv_saudh %>% filter(year >= min(input$nam5) & year <= max(input$nam5))
      h <-  highchart() %>%
        hc_xAxis(categories = df$year) %>%
        hc_exporting(enabled = TRUE) %>%
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 2) %>%
        hc_title(text="Học viên sau đại học",align="center") %>%
        hc_add_theme(hc_theme_economist()) %>%
        hc_yAxis(title = list(text = "Số lượng")) %>%
        hc_add_series(type = "column",color = input$a, dataLabels = list(enabled = T), data = unlist(df[,paste0(input$type5," - Tổng số")],use.names = F), name = "Tổng số")
      if (input$type5 == "Số học viên được đào tạo sau đại học" |
          input$type5 == "Số học viên tốt nghiệp sau đại học" )  {
        h %>% hc_add_series(type = "column",color = input$b, dataLabels = list(enabled = T), data = unlist(df[,paste0(input$type5," - Nghiên cứu sinh")],use.names = F), name = "Nghiên cứu sinh") %>%
          hc_add_series(type = "column",color = input$c, dataLabels = list(enabled = T), data = unlist(df[,paste0(input$type5," - Cao học")],use.names = F), name = "Cao học")
      }
      else {
        h %>% hc_add_series(type = "column",color = input$b, dataLabels = list(enabled = T), data = unlist(df[,paste0(input$type5," - Cấp 1")],use.names = F), name = "Cấp 1") %>%
          hc_add_series(type = "column",color = input$c, dataLabels = list(enabled = T), data = unlist(df[,paste0(input$type5," - Cấp 2")],use.names = F), name = "Cấp 2")
      }
    }
    else if (input$index == "woman") {
      data <- nugv_nuhs %>% filter(year >= min(input$nam6) & year <= max(input$nam6))
      hc <-  highchart() %>%
        hc_exporting(enabled = TRUE) %>%
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5", shared = TRUE,
                   borderWidth = 2) %>%
        hc_xAxis(categories = data$year) %>%
        hc_title(text = paste0(input$type6, " theo cấp học"), align = "center") %>%
        hc_add_theme(hc_theme_economist())
      if (input$type6 != "nữ học sinh/nữ giáo viên") {
        if (input$data6) {
          hc %>% hc_yAxis(title = list(text = "Tỷ lệ (%)")) %>%
            hc_add_series(type = "column",color = input$a, data = unlist(data[,paste0("Tỷ lệ ", input$type6, " tiểu học")], use.names = F), name = "Tiểu học") %>%
            hc_add_series(type = "column",color = input$b, data = unlist(data[,paste0("Tỷ lệ ", input$type6, " trung học cơ sở")], use.names = F), name = "Trung học cơ sở") %>%
            hc_add_series(type = "column",color = input$c, data = unlist(data[,paste0("Tỷ lệ ", input$type6, " trung học phổ thông")], use.names = F), name = "Trung học phổ thông") %>%
            hc_plotOptions(column = list(
              dataLabels = list(enabled = TRUE),
              stacking = "normal"
            ))
        }
        else {
          if (input$type6 == "nữ giáo viên") {
            hc %>%
              hc_yAxis_multiples(list(title = list(text = "Quantity (nghìn người)"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Grow"),labels = list(format = "{value}%"), opposite = TRUE)) %>%
              hc_add_series(type ="column",color = input$a, data = data$"Tổng số nữ giáo viên", name = "Tổng", yAxis = 0) %>%
              hc_add_series(type = "column",color = input$b, data = data$"nữ giáo viên tiểu học", name = "Tiểu học", yAxis = 0) %>%
              hc_add_series(type = "column", color = input$c, data = data$"nữ giáo viên trung học cơ sở", name = "Trung học cơ sở", yAxis = 0) %>%
              hc_add_series(type = "column",color = input$d,  data = data$"nữ giáo viên trung học phổ thông", name = "Trung học phổ thông", yAxis = 0) %>%
              hc_add_series(type ="line",color = input$a, data = data$"Tăng số nữ giáo viên", name = "Tổng", yAxis = 1) %>%
              hc_add_series(type = "line", color = input$b,data = data$"Tăng số nữ giáo viên tiểu học", name = "Tiểu học", yAxis = 1) %>%
              hc_add_series(type = "line", color = input$c, data = data$"Tăng số nữ giáo viên trung học cơ sở", name = "Trung học cơ sở", yAxis = 1) %>%
              hc_add_series(type = "line", color = input$d, data = data$"Tăng số nữ giáo viên trung học phổ thông", name = "Trung học phổ thông", yAxis = 1)

          }
          else
          {
            hc %>%
              hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Grow"),labels = list(format = "{value}%"), opposite = TRUE)) %>%
              hc_add_series(type ="column",color = input$a, data = data$"Tổng số nữ học sinh", name = "Tổng", yAxis = 0) %>%
              hc_add_series(type = "column",color = input$b, data = data$"nữ học sinh tiểu học", name = "Tiểu học", yAxis = 0) %>%
              hc_add_series(type = "column",color = input$c,  data = data$"nữ học sinh trung học cơ sở", name = "Trung học cơ sở", yAxis = 0) %>%
              hc_add_series(type = "column",color = input$d,  data = data$"nữ học sinh trung học phổ thông", name = "Trung học phổ thông", yAxis = 0) %>%
              hc_add_series(type ="line",color = input$a, data = data$"Tăng số nữ học sinh", name = "Tổng", yAxis = 1) %>%
              hc_add_series(type = "line",color = input$b, data = data$"Tăng số nữ học sinh tiểu học", name = "Tiểu học", yAxis = 1) %>%
              hc_add_series(type = "line", color = input$c, data = data$"Tăng số nữ học sinh trung học cơ sở", name = "Trung học cơ sở", yAxis = 1) %>%
              hc_add_series(type = "line", color = input$d, data = data$"Tăng số nữ học sinh trung học phổ thông", name = "Trung học phổ thông", yAxis = 1)
          }
        }
      }
      else {
        hc %>% hc_add_series(type = "line",color = input$a, data = unlist(data[,paste0(input$type6, " tiểu học")], use.names = F), name = "Tiểu học") %>%
          hc_add_series(type = "line",color = input$b, data = unlist(data[,paste0(input$type6, " trung học cơ sở")], use.names = F), name = "Trung học cơ sở") %>%
          hc_add_series(type = "line",color = input$c, data = unlist(data[,paste0(input$type6, " trung học phổ thông")], use.names = F), name = "Trung học phổ thông") %>%
          hc_add_series(type = "line", color = input$d,data = unlist(data[,input$type6], use.names = F), name = "Tất cả") %>%
          hc_plotOptions(line = list(
            dataLabels = list(enabled = TRUE)
          ))
      }
    }
    else if (input$index == "snc") {
      data <- th_lh_phothong %>% filter(year >= min(input$nam7) & year <= max(input$nam7))
      h <-  highchart() %>%
        hc_exporting(enabled = TRUE) %>%
        hc_tooltip(shared = TRUE, crosshairs = TRUE) %>%
        hc_xAxis(categories = data$year) %>%
        hc_add_theme(hc_theme_economist())
      if (input$data7) {
        hc <- h %>%
          hc_plotOptions(areaspline = list(
            dataLabels = list(enabled = TRUE),
            stacking = "percentage"
          )) %>%
          hc_yAxis(title = list(text = "%")) %>%
          hc_title(text = "Số lượng trường học và lớp học")
        if (input$type7 == "Trường") {
          hc %>% hc_add_series(type = "areaspline",color = input$a, data = unlist(data[,"Tỷ lệ Trường tiểu học"],use.names = F), name = "Tiểu học") %>%
            hc_add_series(type = "areaspline",color = input$b, data = unlist(data[,"Tỷ lệ Trường trung học cơ sở"],use.names = F), name = "Trung học cơ sở") %>%
            hc_add_series(type = "areaspline",color = input$c, data = unlist(data[,"Tỷ lệ Trường trung học phổ thông"],use.names = F), name = "Trung học phổ thông") %>%
            hc_add_series(type = "areaspline",color = input$d, data = unlist(data[,"Tỷ lệ Trường trung học"],use.names = F), name = "Trung học")
        }
        else{
          hc %>% hc_add_series(type = "areaspline",color = input$a, data = unlist(data[,"Tỷ lệ Lớp tiểu học"],use.names = F), name = "Tiểu học") %>%
            hc_add_series(type = "areaspline",color = input$b, data = unlist(data[,"Tỷ lệ Lớp trung học cơ sở"],use.names = F), name = "Trung học cơ sở") %>%
            hc_add_series(type = "areaspline", color = input$c,data = unlist(data[,"Tỷ lệ Lớp trung học phổ thông"],use.names = F), name = "Trung học phổ thông")
        }
      }
      else {
        if (input$type7 == "Trường") {
          hc <-  h %>% hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Grow"),labels = list(format = "{value}%"), opposite = TRUE)) %>%
            hc_add_series(type ="column",color = input$a, data = unlist(data[,paste0(input$type7, " học")],use.names = F), name = "Tổng trường", yAxis = 0, color = "green") %>%
            hc_add_series(type ="column",color = input$b, data = unlist(data[,paste0(input$type7," tiểu học")],use.names = F), name = "Trường tiểu học", yAxis = 0) %>%
            hc_add_series(type ="column",color = input$c, data = unlist(data[,paste0(input$type7," trung học cơ sở")],use.names = F), name = "Trường trung học cơ sở", yAxis = 0) %>%
            hc_add_series(type ="column",color = input$d, data = unlist(data[,paste0(input$type7," trung học phổ thông")],use.names = F), name = "Trường trung học phổ thông", yAxis = 0) %>%
            hc_add_series(type ="column",color = input$e, data = unlist(data[,paste0(input$type7," trung học")],use.names = F), name = "Trường trung học", yAxis = 0)

          if (input$y7) {
            hc %>%
              hc_add_series(type ="line",color = input$a, data = unlist(data[,paste0("Tăng ",input$type7, " học")],use.names = F), name = "Tăng tổng trường", yAxis = 1, color = "green") %>%
              hc_add_series(type ="line",color = input$b, data = unlist(data[,paste0("Tăng ",input$type7," tiểu học")],use.names = F), name = "Tăng trường tiểu học", yAxis = 1) %>%
              hc_add_series(type ="line",color = input$c, data = unlist(data[,paste0("Tăng ",input$type7," trung học cơ sở")],use.names = F), name = "Tăng trường trung học cơ sở", yAxis = 1) %>%
              hc_add_series(type ="line",color = input$d, data = unlist(data[,paste0("Tăng ",input$type7," trung học phổ thông")],use.names = F), name = "Tăng trường trung học phổ thông", yAxis = 1) %>%
              hc_add_series(type ="line",color = input$e, data = unlist(data[,paste0("Tăng ",input$type7," trung học")],use.names = F), name = "Tăng trường trung học", yAxis = 1)
          }
          else {
            hc
          }
        }
        else {
          hc <-  h %>% hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Grow"),labels = list(format = "{value}%"), opposite = TRUE)) %>%
            hc_add_series(type ="column",color = input$a, data = unlist(data[,paste0(input$type7, " học")],use.names = F), name = "Tổng lớp", yAxis = 0, color = "green") %>%
            hc_add_series(type ="column",color = input$b, data = unlist(data[,paste0(input$type7," tiểu học")],use.names = F), name = "Lớp tiểu học", yAxis = 0) %>%
            hc_add_series(type ="column",color = input$c, data = unlist(data[,paste0(input$type7," trung học cơ sở")],use.names = F), name = "Lớp trung học cơ sở", yAxis = 0) %>%
            hc_add_series(type ="column",color = input$d, data = unlist(data[,paste0(input$type7," trung học phổ thông")],use.names = F), name = "Lớp trung học phổ thông", yAxis = 0)
          if (input$y7) {
            hc %>%
              hc_add_series(type ="line",color = input$a, data = unlist(data[,paste0("Tăng ",input$type7, " học")],use.names = F), name = "Tăng tổng lớp", yAxis = 1, color = "green") %>%
              hc_add_series(type ="line",color = input$b, data = unlist(data[,paste0("Tăng ",input$type7," tiểu học")],use.names = F), name = "Tăng lớp tiểu học", yAxis = 1) %>%
              hc_add_series(type ="line",color = input$c, data = unlist(data[,paste0("Tăng ",input$type7," trung học cơ sở")],use.names = F), name = "Tăng lớp trung học cơ sở", yAxis = 1) %>%
              hc_add_series(type ="line",color = input$d, data = unlist(data[,paste0("Tăng ",input$type7," trung học phổ thông")],use.names = F), name = "Tăng lớp trung học phổ thông", yAxis = 1)
          }
          else {hc
          }
        }
      }
    }
  })

  #reactive data
  data_vn <- reactive({
    if (input$index_1 == "tns_p") {
      merge(vn, gv_sv_dhcd)
    }
    else {
      merge(vn, gvpt, by = "Province")
    }
  })

  data_of_click <- reactiveValues(Clicks=NULL)

  output$map <- renderLeaflet({
    if (input$index_1 == "tns_p") {
      a <- unlist(gv_sv_dhcd[str_detect(gv_sv_dhcd$group, input$map), "value"], use.names = F)
      bins <- quantile(na.omit(a), seq(0,1,0.2))
      pal <- colorBin(input$pal, bins = bins, na.color = "#444444")

      leaflet() %>% addTiles(options = providerTileOptions(minZoom = 6, maxZoom = 10)) %>%
        addPolygons(data = vn2, layerId = vn2$VARNAME_1, fillColor = ~pal(as.data.frame(vn2)[,paste0(input$map," - ",input$namm)]),fillOpacity = 0.9,color="red",weight=0.5,
                    label = ~province) %>%
        addLegend("bottomright",opacity=1,pal=pal,values=as.data.frame(vn2)[,paste0(input$map," - ",input$namm)],title=paste0("Số lượng ",input$map, " phân chia theo tỉnh"), na.label = "No data available")
    }
    else if (input$index_1 == "direct_teaching") {
      index1 <- paste0(input$level, " - ", input$year)
      pal <- colorNumeric(input$pal, NULL, na.color = "grey")
      leaflet() %>% addTiles(options = providerTileOptions(minZoom = 6, maxZoom = 10)) %>% addPolygons(data = vn1) %>%
        addPolygons(data=vn1,layerId = vn1$VARNAME_1, fillColor = ~pal(as.data.frame(vn1)[,index1]),fillOpacity = 0.9,color="red",weight=0.5,popup=~as.character(as.data.frame(vn1)[,index1]),label=~paste0(province," Province",": ",as.data.frame(vn1)[,index1])) %>%
        addLegend("topright",opacity=1,pal=pal,values=as.data.frame(vn1)[,index1],title="Number of teacher by Province", na.label = "No data available")
    }
  })
  # store the click
  observeEvent(input$map_shape_click,{
    data_of_click$Clicks <- input$map_shape_click
  })
  output$plot <- renderPlot({
    my_place=data_of_click$Clicks$id
    if (input$index_1 == "tns_p") {
      if(is.null(my_place)) {my_place="Ha Noi"}
      data <- gv_sv_dhcd %>% filter(year <= input$namm) %>%
        filter(Province == my_place) %>%
        filter(str_detect(group, input$map))
      ggplot(data, aes(fill = group, x = year, y = value)) +
        geom_bar(position = "dodge", stat = "identity") +
        ggtitle(paste0(input$map," tỉnh ", data$province, " tính đến năm ",input$namm), subtitle = "Đơn vị: nghìn người") +
        labs(y = "Số lượng", x = "Năm") +
        geom_text(aes(label = value), position = position_dodge(0.9),
                  hjust = 1) + coord_flip() +
        scale_fill_manual("Nhóm", values = c(input$a_1, input$a_2)) +
        theme(legend.position = "bottom")
    }
    else {
      if(is.null(my_place)) {my_place="Ha Noi"}
      data <- gvpt %>% filter(year <= input$year) %>%
        filter(Province == my_place) %>%
        filter(str_detect(group, input$level))
      ggplot(data, aes(fill = group, x = year, y = value)) +
        geom_bar(position = "dodge", stat = "identity") +
        ggtitle(paste0("Giáo viên tỉnh ", data$province, " tính đến năm ",input$year), subtitle = "Đơn vị: người") +
        labs(y = "Số lượng", x = "Năm") +
        geom_text(aes(label = value), position = position_dodge(0.9),
                  hjust = 1) + coord_flip() +
        scale_fill_manual("Nhóm", values = input$a_1) +
        theme(legend.position = "bottom")
    }
  })
}

shinyApp(ui = ui, server = server)

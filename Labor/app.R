library(ggplot2)
library(tidyverse)
library(readxl)
library(highcharter)
library(shiny)
library(shinydashboard)
library(raster)
library(leaflet)
library(DT)
library(shinyWidgets)
library(shinythemes)
library(viridisLite)
library(dashboardthemes)
library(ggExtra)
library(colourpicker)
library(memoise)

# Import data #########################################################

laodong_gt_khuvuc <- read_excel("laodong_gt_khuvuc.xlsx",sheet = 3,col_names = TRUE)
laodong_gt_khuvuc_tyle <- read_excel("laodong_gt_khuvuc.xlsx",sheet = 4,col_names = TRUE)
data_ldgtkv <- read_excel("laodong_gt_khuvuc.xlsx",sheet = 2,col_names = TRUE)
lucluong_gt_khuvuc <- read_excel("lucluong_gt_khuvuc.xlsx",sheet = 2,col_names = TRUE)
lucluong_gt_khuvuc_cocau <- read_excel("lucluong_gt_khuvuc.xlsx",sheet = 3,col_names = TRUE)
laodong_nganhnghe <- read_excel("laodong_nganhnghe.xlsx",sheet = 2,col_names = TRUE)
data_laodong_nganhnghe <- read_excel("laodong_nganhnghe.xlsx",sheet = 1,col_names = TRUE)
tyleld_nganhkt <- read_excel("tyle_ld_nganhkt.xlsx",sheet = 2,col_names = TRUE)
laodong_tuoi <- read_excel("laodong_tuoi.xlsx",sheet = 2,col_names = TRUE)
data_laodong_tuoi <- read_excel("laodong_tuoi.xlsx",sheet = 4,col_names = TRUE)
lucluong_tuoi <- read_excel("lucluong_tuoi.xlsx",sheet = 2,col_names = TRUE)
lucluong_tuoi_cocau <- read_excel("lucluong_tuoi.xlsx",sheet = 3,col_names = TRUE)
laodong_tpkt <- read_excel("laodong_tpkt.xlsx",sheet = 2,col_names = TRUE)
laodong_tpkt_cocau <- read_excel("laodong_tpkt.xlsx",sheet = 3,col_names = TRUE)
data_laodong_vithevieclam <- read_excel("laodong_vithevieclam.xlsx",sheet = 1,col_names = TRUE)
laodong_vithevieclam <- read_excel("laodong_vithevieclam.xlsx",sheet = 2,col_names = TRUE)
laodong_vithevieclam_tyle <- read_excel("laodong_vithevieclam.xlsx", sheet = 3,col_names = TRUE)
laodong_nganhkt <- read_excel("ld_nganhkt.xlsx",sheet = 2,col_names = TRUE)
data_laodong_nganhkt <- read_excel("laodong_nganhkt.xlsx",sheet = 3,col_names = TRUE)
laodong_diaphuong <- read_excel("laodong_diaphuong.xlsx",sheet = 2,col_names = TRUE)
laodong_diaphuong2 <- read_excel("laodong_diaphuong.xlsx",sheet = 3,col_names = TRUE)
data_laodong_daotao <- read_excel("laodong_daotao.xlsx",sheet = 1, col_names = TRUE)
laodong_daotao <- read_excel("laodong_daotao.xlsx",sheet = 3,col_names = TRUE)
laodong_trongtuoilaodong <- read_excel("laodong_daotao.xlsx",sheet = 2,col_names = TRUE)
laodong_trinhdo <- read_excel("laodong_trinhdo.xlsx",sheet = 2,col_names = TRUE)
data_laodong_trinhdo <- read_excel("laodong_trinhdo.xlsx",sheet = 1,col_names = TRUE)
thatnghiep <- read_excel("Tylethatnghiep-thieuviec.xlsx",sheet = 3,col_names = TRUE)
data_tntv <- read_excel("Tylethatnghiep-thieuviec.xlsx",sheet = 1,col_names = TRUE)
Thieuvieclam <- read_excel("Tylethatnghiep-thieuviec.xlsx",sheet = 2,col_names = TRUE)
thatnghiep_gtinh <- read_excel("thatnghiep_gioitinh.xlsx")
thieuvl_gtinh <- read_excel("thieuvl_gioitinh.xlsx")
nangsuatlaodong <- read_excel("nangsuatlaodong.xlsx",sheet = 2,col_names = TRUE)
data_nangsuatlaodong <- read_excel("nangsuatlaodong.xlsx",sheet = 1,col_names = TRUE)
laodong_daotao_tuoi <- read_excel("laodong_daotao_tuoi.xlsx",sheet = 2,col_names = TRUE)
tyle_ld_diaphuong <- read_excel("tyle_ld_diaphuong.xlsx",sheet = 2,col_names = TRUE)
laodong_diaphuong <- laodong_diaphuong[order(laodong_diaphuong$Province),]
tyle_ld_diaphuong <- tyle_ld_diaphuong[order(tyle_ld_diaphuong$Province),]

# data VietNam -----------------
vn<-readRDS("gadm36_VNM_1_sp.rds") # Lấy dữ liệu địa lý
vn1<-vn[order(vn$VARNAME_1),]

data <- laodong_diaphuong %>% gather(`2005`,`2007`,`2008`,`2009`,`2010`,
                                     `2011`,`2012`,`2013`,`2014`,`2015`,`2016`,
                                     `2017`,key = "Year", value = "value")
data2 <- tyle_ld_diaphuong %>% gather(`2005`,`2007`,`2008`,`2009`,`2010`,
                                      `2011`,`2012`,`2013`,`2014`,`2015`,`2016`,
                                      `2017`,key = "Year", value = "value")

vn1$Province<-laodong_diaphuong$Province
vn2 <- merge(vn1, laodong_diaphuong, by = "Province")
vn3 <- merge(vn1, tyle_ld_diaphuong, by = "Province")

# Color ----------
pal <- colorBin(c("#004529", "#006837","#238443",
                  "#41ab5d","#78c679","#addd8e", "#d9f0a3","#f7fcb9", "#ffffe5"),
                c(168.4:4415.9) ,bins = c(0,500,1000,1500,2000,2500,3000,3500,4000,4500))

pal1 <- colorBin(c("#ffffe5","#f7fcb9", "#d9f0a3","#addd8e","#78c679","#41ab5d",
                   "#238443","#006837","#004529"),
                 c(43.5:71.3) ,bins = c(40,44,48,52,56,60,62,66,72))
cols <- viridis(3)

cols <- substr(cols, 0, 7)

# select ------------
index <- c("1.Labour force at 15 years of age and above by sex and by residence" = "h1",
           "2.Labour force at 15 years of age and above by age group" = "h2" ,
           "3.Annual employed population at 15 years of age and above by types of ownership" = "h3",
           "4.Annual employed population at 15 years of age and above by sex and by residence" = "h4",
           "5.Annual employed population at 15 years of age and above by age group" = "h5",
           "6.Annual employed population and annual employed population at 15 years of age and above by kinds of economic activity" = "h6",
           "7.Annual employed population at 15 years of age and above by occupation" = "h7",
           "8.Annual employed population at 15 years of age and above by status in employment"= "h8",
           "9.Percentage of trained employed workers by sex and by residence" = "h9",
           "10.Percentage of trained employed workers at 15 years of age and above by age group" = "h10",
           "11.Percentage of trained employed workers at 15 years of age and above by qualification" = "h11",
           "12.Productivity of employed population by industry" = "h12",
           "13.Unemployment and underemployment rate of labour force at working age by region and by residence" = "h13")

###################################################################

# UI ----------------------

header <- dashboardHeader(title = "EMPLOYMENT")
# SideBar:
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              menuItem("Graphs", tabName = "graphs", icon = icon("chart-line "),
                       menuSubItem("1.Labour force at 15 years of age and above by sex and by residence." ,tabName= "hue1"),
                       menuSubItem("2.Labour force at 15 years of age and above by age group" ,tabName= "hue2"),
                       menuSubItem("3.Annual employed population at 15 years of age and above by types of ownership" ,tabName= "hue3"),
                       menuSubItem("4.Annual employed population at 15 years of age and above by sex and by residence" ,tabName= "hue4"),
                       menuSubItem("5.Annual employed population at 15 years of age and above by age group",tabName= "hue5"),
                       menuSubItem("6.Annual employed population and annual employed population at 15 years of age and above by kinds of economic activity" ,tabName= "hue6"),
                       menuSubItem("7.Annual employed population at 15 years of age and above by occupation" ,tabName= "hue7"),
                       menuSubItem("8.Annual employed population at 15 years of age and above by status in employment" ,tabName= "hue8"),
                       menuSubItem("9.Percentage of trained employed workers by sex and by residence" ,tabName= "hue9"),
                       menuSubItem("10.Percentage of trained employed workers at 15 years of age and above by age group" ,tabName= "hue10"),
                       menuSubItem("11.Percentage of trained employed workers at 15 years of age and above by qualification" ,tabName= "hue11"),
                       menuSubItem("12.Productivity of employed population by industry" ,tabName= "hue12"),
                       menuSubItem("13.Unemployment and underemployment rate of labour force at working age by region and by residence" ,tabName= "hue13")
                       
              ),
              menuItem("Maps", tabName = "maps", icon = icon("map")),
              menuItem("Retreive Data", tabName = "data", icon = icon("database ")),
              menuItem("About", tabName = "about", icon = icon("info" ))
  )
)
# ui body
body <- dashboardBody(
  shinyDashboardThemes(theme = 'blue_gradient'),
  ### GRAPHS TAB
  tabItems(
    # graph 1
    tabItem( 
      tabName = "hue1",
      dropdown(tags$h3("List of Inputs"),
               
               pickerInput("select1", "Select :", choices = c("Phân theo thành thị, nông thôn", "Phân theo giới tính"),
                           options = list(`style` = "btn-info")),
               
               pickerInput("select2","Select :" , choices = c("Tổng số (nghìn người)","Cơ cấu (%)"),options = list(`style` = "btn-warning")),
               
               sliderInput("year1_1", label = "Select range year:", min = 2000, max = 2017,value = c(2000,2017), step = 1, sep = ""),
               
               circle = TRUE, # Use a circle button
               status = "primary",
               icon = icon("angle-double-right"), width = "300px",
               tooltip = tooltipOptions(title = "Click to see inputs !"),animate = animateOptions(
                 enter = animations$fading_entrances$fadeInLeftBig, exit = animations$fading_exits$fadeOutRightBig),
               
               colourInput("col1_1", NULL, "#3fc1c9"), 
               colourInput("col1_2", NULL, "#364f6b")),
      
      highchartOutput("chart1")
      ),
    #graph 2
    tabItem(
      tabName = "hue2",
      dropdown(tags$h3("List of Inputs"), 
               
               pickerInput("choi1_1", "Seclect:", choices = c("Tổng số (Nghìn người)","Cơ cấu (%)"),
                           options = list(`style` = "btn-info")),
               
               sliderInput("year3_1", label = "Select range year:", min = 2000, max = 2017, value = c(2000,2017), step = 1, sep = ""),
               
               circle = TRUE,status = "primary",icon = icon("angle-double-right"), width = "300px",
               tooltip = tooltipOptions(title = "Click to see inputs !"), animate = animateOptions(
                 enter = animations$fading_entrances$fadeInLeftBig, exit = animations$fading_exits$fadeOutRightBig),
               
               colourInput("col2_1", NULL, "#2C3E50"),
               colourInput("col2_2", NULL, "#049372"),
               colourInput("col2_3", NULL, "#F7CA18")),
      
      highchartOutput("chart2")
    ),
    # graph 3
    tabItem(
      tabName = "hue3",
      dropdown(tags$h3("List of Inputs"),
               pickerInput("choi", "Seclect:", choices = c("Tổng số (Nghìn người)", "Cơ cấu (%)"),
                           options = list(`style` = "btn-info")),
               
               sliderInput("year3", label = "Select range year:",min = 2000, max = 2017, value = c(2000,2017), step = 1, sep = ""),
               
               circle = TRUE,status = "primary", icon = icon("angle-double-right"), width = "300px", 
               tooltip = tooltipOptions(title = "Click to see inputs !"),
               animate = animateOptions( enter = animations$fading_entrances$fadeInLeftBig, exit = animations$fading_exits$fadeOutRightBig),
               
               colourInput("col3_1", NULL, "#3B0440"),
               colourInput("col3_2", NULL, "#049372"),
               colourInput("col3_3", NULL, "#F7CA18")),
      
      highchartOutput("chart3")
      ),
    # graph 4
    tabItem(
      tabName = "hue4",
      dropdown(tags$h3("List of Inputs"),
               
               pickerInput("select", "Select 1:", choices = c("Phân theo thành thị, nông thôn","Phân theo giới tính"),
                           options = list(`style` = "btn-info")),
               
               sliderInput("year1", label = "Select range year:",
                          min = 2005, max = 2017, value = c(2005,2017), step = 1, sep = ""),
               
               circle = TRUE,status = "primary", icon = icon("angle-double-right"), width = "300px",
               tooltip = tooltipOptions(title = "Click to see inputs !"), animate = animateOptions( 
                 enter = animations$fading_entrances$fadeInLeftBig,
                 exit = animations$fading_exits$fadeOutRightBig),
               
               colourInput("col4_1", NULL, "#364f6b"),
               colourInput("col4_2", NULL, "#3fc1c9"),
               colourInput("col4_3", NULL, "#ec610a"),
               colourInput("col4_4", NULL, "#c61951")),
      
      highchartOutput("chart4")
    ),
    # graph 5
    tabItem(
      tabName = "hue5",
      dropdown( tags$h3("List of Inputs"),
                
                pickerInput("age", "Select age group :",
                            choices = c("Nhóm tuổi từ 15-19" = "15-19",
                                        "Nhóm tuổi từ 20-24" = "20-24",
                                        "Nhóm tuổi từ 25-29" = "25-29",
                                        "Nhóm tuổi từ 30-34" = "30-34",
                                        "Nhóm tuổi từ 35-39" = "35-39",
                                        "Nhóm tuổi từ 40-44" = "40-44",
                                        "Nhóm tuổi từ 45-49" = "45-49",
                                        "Nhóm tuổi từ 50+" = "50+",
                                        "Tổng số các nhóm tuổi",
                                        "Tỷ lệ so với tổng dân số"),
                            options = list(`style` = "btn-info")),
                
                sliderInput("nam", label = "Select range year:", min = 2009, max = 2017, 
                            value = c(2009,2017), step = 1, sep = ""),
                
                circle = TRUE,status = "primary",icon = icon("angle-double-right"), width = "300px",
                tooltip = tooltipOptions(title = "Click to see inputs !"),
                animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                         exit = animations$fading_exits$fadeOutRightBig),
                
                colourInput("col5_1", NULL, "#2FACD6"),
                colourInput("col5_2", NULL, "#81180A")),
      
      highchartOutput("chart5")),
    # graph 6
    tabItem(
      tabName = "hue6",
      dropdown(tags$h3("List of Inputs"),
               
               pickerInput("eco", label = "Select an industry:",
                           choices = c("Nông nghiệp, lâm nghiệp và thủy sản" = "Nonglamnghiep" ,
                                      "Khai khoáng" = "Khaikhoang",
                                      "Công nghiệp chế biến, chế tạo" = "Cnchebienchetao",
                                      "Sản xuất và phân phối điện, khí đốt, nước nóng, hơi nước và điều hòa không khí" = "sanxuatvaphanphoi",
                                      "Cung cấp nước; hoạt động quản lý và xử lý rác thải, nước thải" ="Cungcapnuoc",
                                      "Xây dựng" = "Xaydung",
                                      "Bán buôn và bán lẻ; sửa chữa ô tô, mô tô, xe máy và xe có động cơ khác" = "Banbuonbanle",
                                      "Vận tải, kho bãi" = "Vantai",
                                      "Dịch vụ lưu trú và ăn uống" ="Dichvu",
                                      "Thông tin và truyền thông" = "Thongtinvatruyenthong",
                                      "Hoạt động tài chính, ngân hàng và bảo hiểm" = "hdtaichinh",
                                      "Hoạt động kinh doanh bất động sản" = "hdkinhdoanh",
                                      "Hoạt động chuyên môn, khoa học và công nghệ" = "hdchuyenmon",
                                      "Hoạt động hành chính và dịch vụ hỗ trợ" ="hdhanhchinh",
                                      "Hoạt động của Đảng Cộng sản, tổ chức chính trị - xã hội; quản lý Nhà nước, an ninh quốc phòng; đảm bảo xã hội bắt buộc"= "hddang",
                                      "Giáo dục và đào tạo" = "giaoduc",
                                      "Y tế và hoạt động trợ giúp xã hội" = "yte",
                                      "Nghệ thuật, vui chơi và giải trí" ="nghethuat",
                                      "Hoạt động dịch vụ khác" ="hddichvu",
                                      "Hoạt động làm thuê các công việc trong các hộ gia đình, sản xuất sản phẩm, vật chất và dịch vụ tiêu dùng của hộ gia đình" ="hdlamthue",
                                      "Hoạt động của các tổ chức và cơ quan quốc tế" = "hdtochuc",
                                      "Tổng tất cả các ngành",
                                      "Cơ cấu tất cả các ngành"),
                          options = list(`style` = "btn-info")),
               
               sliderInput("year8", label = "Select range year:",min = 2005, max = 2017, 
                           value = c(2005,2017), step = 1, sep = ""),
               
               circle = TRUE,status = "primary",icon = icon("angle-double-right"), width = "300px",
               tooltip = tooltipOptions(title = "Click to see inputs !"),
               animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                        exit = animations$fading_exits$fadeOutRightBig),
               
               colourInput("col6_1", NULL, "#2FACD6"),
               colourInput("col6_2", NULL, "#81180A")),
      
      highchartOutput("chart6")),
    # graph 7
    tabItem(
      tabName = "hue7",
      dropdown(tags$h3("List of Inputs"),
               
               pickerInput("career", "Occupation:",
                           choices = c("Nhà lãnh đạo" = "Nhalanhdao",
                                       "Chuyên môn kỹ thuật bậc cao" = "Chuyenmonkythuatbaccao",
                                       "Chuyên môn kỹ thuật bậc trung" = "Chuyenmonkythuatbactrung",
                                       "Nhân viên" = "Nhanvien",
                                       "Dịch vụ cá nhân, bảo vệ bán hàng" = "Dichvucanhan&baovebanhang",
                                       "Nghề trong nông, lâm, ngư nghiệp" = "nonglamngunghiep",
                                       "Thợ thủ công và các thợ khác có liên quan" = "Thothhucong&cacthocolienquan",
                                       "Thợ lắp ráp và vận hành máy móc, thiết bị" = "Tholaprap&vanhanhmaymocthietbi",
                                       "Nghề giản đơn" = "Nghegiandon",
                                       "Khác" = "Khac",
                                       "Tổng tất cả các ngành",
                                       "Tỷ lệ so với tổng dân số của tất cả các ngành"),
                           options = list(`style` = "btn-info")),
               
               sliderInput("ye1", label = "Select range year:",min = 2009, max = 2017, 
                           value = c(2009,2017), step = 1, sep = ""),
               
               circle = TRUE,status = "primary",icon = icon("angle-double-right"), width = "300px",
               tooltip = tooltipOptions(title = "Click to see inputs !"),
               animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                        exit = animations$fading_exits$fadeOutRightBig
               ),
               colourInput("col7_1", NULL, "#2FACD6"),
               colourInput("col7_2", NULL, "#81180A")
               ),
             highchartOutput("chart7")),
    # Graph 8
    tabItem(
      tabName = "hue8",
      dropdown(tags$h3("List of Inputs"),
               
               pickerInput("labour3", "Select:",choices = c("Tổng số - nghìn người", "Cơ cấu - %"),
                          options = list(`style` = "btn-info")),
              
              sliderInput("year4", label = "Select range year:",
                          min = 2009, max = 2017, 
                          value = c(2009,2017), step = 1, sep = ""),
              
              circle = TRUE,status = "primary",icon = icon("angle-double-right"), width = "300px",
              tooltip = tooltipOptions(title = "Click to see inputs !"),
              animate = animateOptions(
                enter = animations$fading_entrances$fadeInLeftBig,
                exit = animations$fading_exits$fadeOutRightBig
              ),
              colourInput("col8_1",NULL,"#2574A9"), 
              colourInput("col8_2",NULL,"#22A7F0"), 
              colourInput("col8_3",NULL,"#03A678"), 
              colourInput("col8_4",NULL,"#EB974E"),
              colourInput("col8_5",NULL,"#913D88")
              ),
      highchartOutput("chart8")),
    # Graph 9
    tabItem(
      tabName = "hue9",
      dropdown(tags$h3("List of Inputs"),
               
               pickerInput("ind", label = "Select:",
                           choices = c("Tỷ lệ lao động từ 15 tuổi trở lên" = "above"
                                      ,"Tỷ lệ lao động trong độ tuổi lao động" = "in"),
                          options = list(`style` = "btn-info")),
               
               conditionalPanel(condition = "input.ind == 'above'",
                                pickerInput("cho", "Select :",
                                            choices = c("Phân theo giới tính",
                                        "Phân theo thành thị, nông thôn"),
                                        options = list(`style` = "btn-warning")),
                                
                                sliderInput("year5", label = "Select range year:",min = 2000, max = 2017, 
                                            value = c(2000,2017), step = 1, sep = "")),
               
               conditionalPanel(condition = "input.ind == 'in'",
                                pickerInput("cho1", "Select :",
                                            choices = c("Phân theo giới tính", "Phân theo thành thị, nông thôn"),
                                            options = list(`style` = "btn-warning")),
                                
                                sliderInput("year5_1", label = "Select range year:", min = 2007, max = 2017, 
                                            value = c(2007,2017), step = 1, sep = "")),
               
               circle = TRUE,status = "primary",icon = icon("angle-double-right"), width = "300px",
               tooltip = tooltipOptions(title = "Click to see inputs !"),
               animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                        exit = animations$fading_exits$fadeOutRightBig
                                        ),
               colourInput("col10_1", NULL, "#0A2081"),
               colourInput("col10_2", NULL, "#147C1F")
               ),
      conditionalPanel(condition = "input.ind == 'above'",
                       highchartOutput("chart10_1")),
      conditionalPanel(condition = "input.ind == 'in'", 
                       highchartOutput("chart10_2"))
      ),
    # Graph 10
    tabItem(
      tabName = "hue10",
      dropdown(tags$h3("List of Inputs"),
               pickerInput("tuoi", "Age group:",
                           choices = c("nhóm tuổi từ 15-19" = "15-19",
                                      "nhóm tuổi từ 20-24" = "20-24",
                                      "nhóm tuổi từ 25-29" = "25-29",
                                      "nhóm tuổi từ 30-34" = "30-34",
                                      "nhóm tuổi từ 35-39" = "35-39",
                                      "nhóm tuổi từ 40-44" = "40-44",
                                      "nhóm tuổi từ 45-49" = "45-49",
                                      "nhóm tuổi từ 50+" = "50+",
                                      "All"),
                          options = list(`style` = "btn-info")),
               
               sliderInput("year6", label = "Select range year:",min = 2009, max = 2017, 
                           value = c(2009,2017), step = 1, sep = ""),
               
               circle = TRUE,status = "primary",icon = icon("angle-double-right"), width = "300px",
               tooltip = tooltipOptions(title = "Click to see inputs !"),
               animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                        exit = animations$fading_exits$fadeOutRightBig
                                        ),
               colourInput("col11", NULL, "#59ABE3")
               ),
      highchartOutput("chart11")),
    # Graph11
    tabItem(tabName = "hue11",
            highchartOutput("chart12")),
    # Graph 12
    tabItem(
      tabName = "hue12",
      dropdown(tags$h3("List of Inputs"),
               pickerInput("industry", label = "Select an industry:",
                           choices = c("Nông nghiệp, lâm nghiệp và thủy sản" = "nln" ,
                                      "Khai khoáng" = "kk",
                                      "Công nghiệp chế biến, chế tạo" = "cncbct",
                                      "Sản xuất và phân phối điện, khí đốt, nước nóng, hơi nước và điều hòa không khí" = "sxpp",
                                      "Cung cấp nước; hoạt động quản lý và xử lý rác thải, nước thải" ="ccn",
                                      "Xây dựng" = "xd",
                                      "Bán buôn và bán lẻ; sửa chữa ô tô, mô tô, xe máy và xe có động cơ khác" = "bbbl",
                                      "Vận tải, kho bãi" = "vt",
                                      "Dịch vụ lưu trú và ăn uống" = "dv",
                                      "Thông tin và truyền thông" = "tttt",
                                      "Hoạt động tài chính, ngân hàng và bảo hiểm" = "hdtc",
                                      "Hoạt động kinh doanh bất động sản" = "hdkd",
                                      "Hoạt động chuyên môn, khoa học và công nghệ" = "hdcm",
                                      "Hoạt động hành chính và dịch vụ hỗ trợ" ="hdhc",
                                      "Hoạt động của Đảng Cộng sản, tổ chức chính trị - xã hội; quản lý Nhà nước, an ninh quốc phòng; đảm bảo xã hội bắt buộc"= "hdd",
                                      "Giáo dục và đào tạo" = "gd",
                                      "Y tế và hoạt động trợ giúp xã hội" = "yt",
                                      "Nghệ thuật, vui chơi và giải trí" ="nghet",
                                      "Hoạt động dịch vụ khác" ="hddv",
                                      "Hoạt động làm thuê các công việc trong các hộ gia đình, sản xuất sản phẩm, vật chất và dịch vụ tiêu dùng của hộ gia đình" ="hdlt",
                                      "Các ngành"),
                          options = list(`style` = "btn-info")),
               
               sliderInput("rangeyear", label = "Select range year:",min = 2005, max = 2017, 
                           value = c(2005,2017), step = 1, sep = ""),
               
               circle = TRUE,status = "primary",icon = icon("angle-double-right"), width = "300px",
               tooltip = tooltipOptions(title = "Click to see inputs !"),
               animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                        exit = animations$fading_exits$fadeOutRightBig
                                        ),
               colourInput("col13", NULL, "#094F10")
               ),
      highchartOutput("chart13")),
    # Graph 13
    tabItem(
      tabName = "hue13",
      dropdown(tags$h3("List of Inputs"),
               pickerInput("ind2", label = "Select:",
                           choices = c("Tỷ lệ thất nghiệp chung" = "tnc",
                                       "Tỷ lệ thiếu việc làm chung" = "tvc",
                                       "Tỷ lệ thất nghiệp theo giới tính" = "tngt",
                                       "Tỷ lệ thiếu việc làm giới tính" = "tvgt"),
                           options = list(`style` = "btn-info")),
               
               conditionalPanel(condition = "input.ind2 == 'tnc'",
                                pickerInput("area", "Region: ",
                                            choices = c("Cả Nước" = "CaNuoc1",
                                                        "Trung du và miền núi phía Bắc" = "Trungdumiennui1",
                                                        "Bắc Trung Bộ và duyên hải miền Trung" = "Bactrungbo1",
                                                        "Tây Nguyên" = "TayNguyen1",
                                                        "Đồng bằng sông Hồng" = "DBSH1",
                                                        "Đông Nam Bộ" = "Dongnambo1",
                                                        "Đồng bằng sông Cửu Long" = "DHSCL1",
                                                        "All"),
                                            options = list(`style` = "btn-warning")),
                                
                                sliderInput("year7", label = "Select range year:",min = 2008, max = 2017, 
                                            value = c(2008,2017), step = 1, sep = "")),
               
               conditionalPanel(condition = "input.ind2 == 'tvc'",
                                pickerInput("area1", "Region: ",
                                            choices = c("Cả Nước" = "CaNuoc2",
                                                        "Trung du và miền núi phía Bắc" = "Trungdumiennui2",
                                                        "Bắc Trung Bộ và duyên hải miền Trung" = "Bactrungbo2",
                                                        "Tây Nguyên" = "TayNguyen2",
                                                        "Đồng bằng sông Hồng" = "DBSH2",
                                                        "Đông Nam Bộ" = "Dongnambo2",
                                                        "Đồng bằng sông Cửu Long" = "DHSCL2",
                                                        "All"),
                                            options = list(`style` = "btn-warning")),
                                
                                sliderInput("year7_1", label = "Select range year:",min = 2008, max = 2017, 
                                            value = c(2008,2017), step = 1, sep = "")),
               
               conditionalPanel(condition = "input.ind2 == 'tngt'",
                                pickerInput("area2", "Region: ",
                                            choices = c("Cả Nước" = "CaNuoc",
                                                        "Trung du và miền núi phía Bắc" = "Trungdumiennui",
                                                        "Bắc Trung Bộ và duyên hải miền Trung" = "Bactrungbo",
                                                        "Tây Nguyên" = "TayNguyen",
                                                        "Đồng bằng sông Hồng" = "DBSH",
                                                        "Đông Nam Bộ" = "Dongnambo",
                                                        "Đồng bằng sông Cửu Long" = "DHSCL"),
                                            options = list(`style` = "btn-warning")),
                                
                                sliderInput("year7_2", label = "Select range year:",min = 2008, max = 2017, 
                                            value = c(2008,2017), step = 1, sep = "")),
               
               conditionalPanel(condition = "input.ind2 == 'tvgt'",
                                pickerInput("area3", "Region: ",
                                            choices = c("Cả Nước" = "CaNuoc",
                                                        "Trung du và miền núi phía Bắc" = "Trungdumiennui",
                                                        "Bắc Trung Bộ và duyên hải miền Trung" = "Bactrungbo",
                                                        "Tây Nguyên" = "TayNguyen",
                                                        "Đồng bằng sông Hồng" = "DBSH",
                                                        "Đông Nam Bộ" = "Dongnambo",
                                                        "Đồng bằng sông Cửu Long" = "DHSCL"),
                                            options = list(`style` = "btn-warning")),
                                
                                sliderInput("year7_3", label = "Select range year:",min = 2008, max = 2017, 
                                            value = c(2008,2017), step = 1, sep = "")),
               
               circle = TRUE,status = "primary",icon = icon("angle-double-right"), width = "300px",
               tooltip = tooltipOptions(title = "Click to see inputs !"),
               animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                        exit = animations$fading_exits$fadeOutRightBig
               ),
               colourInput("col14_1", NULL, "#2F9E3A"),
               colourInput("col14_2", NULL, "#100457")
               ),
      conditionalPanel(condition = "input.ind2 == 'tnc'",
                       highchartOutput("chart14_1")
                       ), 
      conditionalPanel(condition = "input.ind2 == 'tvc'", 
                       highchartOutput("chart14_2")
                       ),
      conditionalPanel(condition = "input.ind2 == 'tngt'",
                       highchartOutput("chart14_3")
                       ), 
      conditionalPanel(condition = "input.ind2 == 'tvgt'", 
                       highchartOutput("chart14_4")
                       )
      ),
    
    ### MAP tab
    tabItem(
      tabName = "maps",
      br(),
      column(8, leafletOutput("map", height = "600px")),
      column(4, 
             selectInput("choice", "Select:", 
                         choices = c("Lực lượng lao động", "Tỷ lệ lao động đang làm việc so với tổng dân số")), 
             selectInput("year", label = "Select year",
                         choices = c("2005" = "2005","2007" = "2007","2008" = "2008","2009" = "2009",
                                     "2010" = "2010","2011" = "2011","2012" = "2012","2013" = "2013",
                                     "2014" = "2014","2015" = "2015","2016" = "2016","2017" = "2017"),
                         selected = "2010"),
             br(), br(),
             plotOutput("plot")), 
      br()
      ),
    ### DATA tab
    tabItem(
      tabName = "data",
      sidebarLayout(
        sidebarPanel = sidebarPanel(uiOutput('select_input_table1')),
        mainPanel = mainPanel(DTOutput('sql_data'))
        )
      )
    ),
  tags$head(tags$style(HTML('
                            .main-header .logo {
                            font-family: "Georgia", Times, "Times New Roman", serif;
                            font-weight: bold;
                            font-size: 24px;
                            }
                            ')
                       )
            )
  )

ui <- dashboardPage(header, sidebar, body)


# SERVER --------------
server <- function(input, output){
  output$chart1 <- renderHighchart({
    hc <- highchart() %>% 
      hc_subtitle(text = "Source: GSO.com",style = list(color = "#0A27F9")) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5", shared = TRUE, borderWidth = 3, split = TRUE) %>% 
      hc_exporting(enabled = TRUE) %>% 
      hc_add_theme(hc_theme_ffx())
    if(input$select1 == "Phân theo thành thị, nông thôn" & input$select2 == "Tổng số (nghìn người)") {
      lucluong_gt_khuvuc <- lucluong_gt_khuvuc %>% filter(Year >= min(input$year1_1) & Year <= max(input$year1_1))
      hc %>% 
        hc_chart(type = "column") %>% 
        hc_title(text = "Labour by residence",
                 style = list(color = "#0A27F9", useHTML = TRUE, fontSize= '16px',fontWeight= 'bold')) %>%
        hc_xAxis(categories = lucluong_gt_khuvuc$Year) %>% 
        hc_yAxis(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")) %>% 
        hc_add_series(name = "Urban",color = input$col1_1 ,data = lucluong_gt_khuvuc$Urban) %>% 
        hc_add_series(name = "Rural", color = input$col1_2 ,data = lucluong_gt_khuvuc$Rural)
      } 
    else if (input$select1 == "Phân theo thành thị, nông thôn" & input$select2 == "Cơ cấu (%)"){
      lucluong_gt_khuvuc_cocau <- lucluong_gt_khuvuc_cocau %>% filter(Year >= min(input$year1_1) & Year <= max(input$year1_1))
      hc %>%  
        hc_chart(type = "column") %>% 
        hc_title(text = "Labour by residence",
                 style = list(color = "#0A27F9", useHTML = TRUE, fontSize= '16px',fontWeight= 'bold')) %>%
        hc_xAxis(categories = lucluong_gt_khuvuc_cocau$Year) %>% 
        hc_yAxis(title = list(text = "Percentage"),lineWidth = 0, labels = list(format = "{value}%")) %>% 
        hc_tooltip(split= TRUE, valueSuffix = "%") %>% 
        hc_add_series(name = "Urban", color = input$col1_1 ,data = lucluong_gt_khuvuc_cocau$Urban,dataLabels = list(enabled = T)) %>% 
        hc_add_series(name = "Rural", color = input$col1_2 ,data = lucluong_gt_khuvuc_cocau$Rural,dataLabels = list(enabled = T)) %>% 
        hc_plotOptions(series = list(stacking = "percent"))
      } 
    else if (input$select1 == "Phân theo giới tính" & input$select2 == "Tổng số (nghìn người)"){
      lucluong_gt_khuvuc <- lucluong_gt_khuvuc %>% filter(Year >= min(input$year1_1) & Year <= max(input$year1_1))
      hc %>% hc_colors(c("#3fc1c9", "#364f6b")) %>%  
        hc_chart(type = "column") %>% 
        hc_title(text = "Labour by sex",
                 style = list(color = "#0A27F9", useHTML = TRUE, fontSize= '16px',fontWeight= 'bold')) %>%
        hc_xAxis(categories = lucluong_gt_khuvuc$Year) %>%
        hc_yAxis(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")) %>% 
        hc_add_series(name = "Male",color = input$col1_1 , data = lucluong_gt_khuvuc$Male) %>% 
        hc_add_series(name = "Female", color = input$col1_2 ,data = lucluong_gt_khuvuc$Female)
      } 
    else {
      lucluong_gt_khuvuc_cocau <- lucluong_gt_khuvuc_cocau %>% filter(Year >= min(input$year1_1) & Year <= max(input$year1_1))
      hc %>% hc_colors(c("#3fc1c9", "#364f6b")) %>% 
        hc_chart(type = "column") %>% 
        hc_title(text = "Labour by sex",
                 style = list(color = "#0A27F9", useHTML = TRUE, fontSize= '16px',fontWeight= 'bold')) %>%
        hc_xAxis(categories = lucluong_gt_khuvuc_cocau$Year) %>%
        hc_yAxis(title = list(text = "Percentage"),lineWidth = 0, labels = list(format = "{value}%")) %>% 
        hc_plotOptions(series = list(stacking = "percent")) %>% 
        hc_tooltip(split= TRUE, valueSuffix = "%") %>% 
        hc_add_series(name = "Male", color = input$col1_1 ,data = lucluong_gt_khuvuc_cocau$Male,dataLabels = list(enabled = T)) %>% 
        hc_add_series(name = "Female", color = input$col1_2 ,data = lucluong_gt_khuvuc_cocau$Female,dataLabels = list(enabled = T))
    }
  })
  
  output$chart2 <- renderHighchart({
    hc <- highchart() %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",shared = TRUE, borderWidth = 3) %>% 
      hc_colors(cols) %>% 
      hc_title(text = "Labour by age group",style = list(color = "#0A27F9", useHTML = TRUE, fontSize= '16px',fontWeight= 'bold')) %>% 
      hc_exporting(enabled = TRUE) %>% 
      hc_subtitle(text = "Source: GSO.com",style = list(color = "#0A27F9")) %>% 
      hc_add_theme(hc_theme_ffx())
    if (input$choi1_1 == "Tổng số (Nghìn người)"){
      lucluong_tuoi <- lucluong_tuoi  %>% filter(Year >= min(input$year3_1) & Year <= max(input$year3_1))
      hc %>% hc_xAxis(categories = lucluong_tuoi$Year) %>% 
        hc_yAxis(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")) %>%
        hc_add_series(name = "Nhóm tuổi từ 15-24",color = input$col2_1, data = lucluong_tuoi$`15-24`) %>% 
        hc_add_series(name = "Nhóm tuổi từ 25-49", color = input$col2_2,data = lucluong_tuoi$`25-49`) %>% 
        hc_add_series(name = "Nhóm tuổi lớn hơn 50", color = input$col2_3, data = lucluong_tuoi$`50+`) %>% 
        hc_chart(type = "column")
      } 
    else {
      lucluong_tuoi_cocau <- lucluong_tuoi_cocau  %>% filter(Year >= min(input$year3_1) & Year <= max(input$year3_1))
      hc %>% hc_xAxis(categories = lucluong_tuoi_cocau$Year) %>% 
        hc_yAxis(title = list(text = "Percentage"),lineWidth = 0, labels = list(format = "{value}%")) %>%
        hc_add_series(name = "Nhóm tuổi từ 15-24", data = lucluong_tuoi_cocau$`15-24_Structure`,color = input$col2_1,
                      dataLabels = list(enabled = T)) %>% 
        hc_add_series(name = "Nhóm tuổi từ 25-49", data = lucluong_tuoi_cocau$`25-49_Structure`,color = input$col2_2,
                      dataLabels = list(enabled = T)) %>% 
        hc_add_series(name = "Nhóm tuổi lớn hơn 50", data = lucluong_tuoi_cocau$`50+_Structure`,color = input$col2_3,
                      dataLabels = list(enabled = T)) %>% 
        hc_chart(type = "column") %>% 
        hc_tooltip(split= TRUE, valueSuffix = "%") %>% 
        hc_plotOptions(series = list(stacking = "percent"))
      }
    })
  
  output$chart3 <- renderHighchart({
    hc <- highchart() %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 3) %>% 
      hc_colors(cols) %>% 
      hc_title(text = "Labour by types of ownership",
               style = list(color = "#0A27F9", useHTML = TRUE, fontSize= '16px',fontWeight= 'bold')) %>% 
      hc_exporting(enabled = TRUE) %>% 
      hc_subtitle(text = "Source: GSO.com",
                  style = list(color = "#0A27F9")) %>% 
      hc_add_theme(hc_theme_ffx())
    if (input$choi == "Tổng số (Nghìn người)"){
      laodong_tpkt <- laodong_tpkt  %>% filter(Year >= min(input$year3) & Year <= max(input$year3))
      hc %>% hc_xAxis(categories = laodong_tpkt$Year) %>% 
        hc_yAxis(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")) %>%
        hc_add_series(name = "Kinh tế Nhà nước",color = input$col3_1, data = laodong_tpkt$KinhteNhanuoc) %>% 
        hc_add_series(name = "Kinh tế ngoài Nhà nước",color = input$col3_2, data = laodong_tpkt$KinhtengoaiNhanuoc) %>% 
        hc_add_series(name = "Nước ngoài",color = input$col3_3, data = laodong_tpkt$Nuocngoai) %>% 
        hc_chart(type = "column")
      } 
    else {
      laodong_tpkt_cocau <- laodong_tpkt_cocau  %>% filter(Year >= min(input$year3) & Year <= max(input$year3))
      hc %>% hc_xAxis(categories = laodong_tpkt_cocau$Year) %>% 
        hc_yAxis(title = list(text = "Percentage"),lineWidth = 0, labels = list(format = "{value}%")) %>%
        hc_add_series(name = "Kinh tế Nhà nước", data = laodong_tpkt_cocau$KinhteNhanuoc_cc,color = input$col3_1,
                      dataLabels = list(enabled = T)) %>% 
        hc_add_series(name = "Kinh tế ngoài Nhà nước", data = laodong_tpkt_cocau$KinhtengoaiNhanuoc_cc,color = input$col3_2,
                      dataLabels = list(enabled = T)) %>% 
        hc_add_series(name = "Nước ngoài", data = laodong_tpkt_cocau$Nuocngoai_cc,color = input$col3_3,
                      dataLabels = list(enabled = T)) %>% 
        hc_chart(type = "column") %>% 
        hc_tooltip(split= TRUE, valueSuffix = "%") %>% 
        hc_plotOptions(series = list(stacking = "percent"))
    }
  })
  
  output$chart4 <- renderHighchart({
    laodong_gt_khuvuc <- laodong_gt_khuvuc %>% filter(Year >= min(input$year1) & Year <= max(input$year1))
    hc <- highchart() %>% 
      hc_xAxis(categories = laodong_gt_khuvuc$Year) %>%
      hc_subtitle(text = "Source: GSO.com",
                  style = list(color = "#0A27F9")) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 3, split = TRUE) %>% 
      hc_exporting(enabled = TRUE) %>% 
      hc_add_theme(hc_theme_ffx())
    if(input$select == "Phân theo thành thị, nông thôn"){
      hc %>% 
        hc_title(text = "Labour by residence",
                 style = list(color = "#0A27F9", useHTML = TRUE, fontSize= '16px',fontWeight= 'bold')) %>%
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng số-khu vực thành thị",
                      color = input$col4_1, data = laodong_gt_khuvuc$Urban, yAxis = 0) %>% 
        hc_add_series(type = "column",name = "Tổng số-khu vực nông thôn", 
                      color = input$col4_2, data = laodong_gt_khuvuc$Rural, yAxis = 0) %>%
        hc_add_series(type = "line",name = "Tỷ lệ so với tổng dân số-khu vực thành thị",
                      color = input$col4_3, data = laodong_gt_khuvuc$Urban_, yAxis = 1) %>% 
        hc_add_series(type = "line",name = "Tỷ lệ so với tổng dân số-khu vực nông thôn",
                      color = input$col4_4, data = laodong_gt_khuvuc$Rural_, yAxis = 1)
      
    } else {
      hc %>%
        hc_title(text = "Labour by residence",
                 style = list(color = "#0A27F9", useHTML = TRUE, fontSize= '16px',fontWeight= 'bold')) %>%
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng số-nam", data = laodong_gt_khuvuc$Male, yAxis = 0) %>% 
        hc_add_series(type = "column",name = "Tổng số-nữ", data = laodong_gt_khuvuc$Female, yAxis = 0) %>%
        hc_add_series(type = "line",name = "Tỷ lệ so với tổng dân số-nam", data = laodong_gt_khuvuc$Male_, yAxis = 1) %>% 
        hc_add_series(type = "line",name = "Tỷ lệ so với tổng dân số-nữ", data = laodong_gt_khuvuc$Female_, yAxis = 1)
    }
  })
  
  output$chart5 <- renderHighchart({
    laodong_tuoi <- laodong_tuoi %>% filter(Year >= min(input$nam) & Year <= max(input$nam))
    hc <- highchart() %>% 
      hc_xAxis(categories = laodong_tuoi$Year) %>% 
      hc_title(text = "Labour by age group",
               style = list(color = "#0A27F9", useHTML = TRUE, 
                            fontSize= '16px',fontWeight= 'bold')) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 3) %>% 
      hc_exporting(enabled = TRUE) %>% 
      hc_subtitle(text = "Source: GSO.com",
                  style = list(color = "#0A27F9")) %>% 
      hc_add_theme(hc_theme_ffx())
    if (input$age == "15-19") {
      hc %>%  
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>%
        hc_add_series(type = "column",name = "Tổng số-nhóm tuổi từ 15-19", 
                      color = input$col5_1, data = laodong_tuoi$`15-19`, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Tỷ lệ so với tổng dân số-nhóm tuổi từ 15-19",
                      color = input$col5_2,data = laodong_tuoi$`15-19_`, yAxis = 1)
    } else if (input$age == "20-24"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>%
        hc_add_series(type = "column",name = "Tổng số-nhóm tuổi từ 20-24",
                      color = input$col5_1,data = laodong_tuoi$`20-24`, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Tỷ lệ so với tổng dân số-nhóm tuổi từ 20-24",
                      color = input$col5_2,data = laodong_tuoi$`20-24_`, yAxis = 1)
    } else if (input$age == "25-29") {
      hc %>%  
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>%
        hc_add_series(type = "column",name = "Tổng số-nhóm tuổi từ 25-29", 
                      color = input$col5_1,data = laodong_tuoi$`25-29`, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Tỷ lệ so với tổng dân số-nhóm tuổi từ 25-29",
                      color = input$col5_2,data = laodong_tuoi$`25-29_` , yAxis = 1)
    } else if (input$age == "30-34"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>%
        hc_add_series(type = "column",name = "Tổng số-nhóm tuổi từ 30-34", 
                      color = input$col5_1,data = laodong_tuoi$`30-34`, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Tỷ lệ so với tổng dân số-nhóm tuổi từ 30-34",
                      color = input$col5_2,data = laodong_tuoi$`30-34_` , yAxis = 1)
    } else if (input$age == "35-39"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>%
        hc_add_series(type = "column",name = "Tổng số-nhóm tuổi từ 35-39", 
                      color = input$col5_1,data = laodong_tuoi$`35-39`, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Tỷ lệ so với tổng dân số-nhóm tuổi từ 35-39"
                      ,color = input$col5_2, data = laodong_tuoi$`35-39_` , yAxis = 1)
    } else if (input$age == "40-44"){
      hc %>%
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>%
        hc_add_series(type = "column",name = "Tổng số-nhóm tuổi từ 40-44", 
                      color = input$col5_1,data = laodong_tuoi$`40-44`, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Tỷ lệ so với tổng dân số-nhóm tuổi từ 40-44",
                      color = input$col5_2,data = laodong_tuoi$`40-44_` , yAxis = 1) 
    } else if (input$age == "45-49"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>%
        hc_add_series(type = "column",name = "Tổng số-nhóm tuổi từ 45-49",
                      color = input$col5_1,data = laodong_tuoi$`45-49`, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Tỷ lệ so với tổng dân số-nhóm tuổi từ 45-49",
                      color = input$col5_2,data = laodong_tuoi$`45-49_` , yAxis = 1) 
    } else if (input$age == "50+"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>%
        hc_add_series(type = "column",name = "Tổng số-nhóm tuổi từ 50+",
                      color = input$col5_1,data = laodong_tuoi$`50+`, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Tỷ lệ so với tổng dân số-nhóm tuổi từ 50+",
                      color = input$col5_2,data = laodong_tuoi$`50+_` , yAxis = 1) 
    } else if (input$age == "Tổng số các nhóm tuổi"){
      hc %>%
        hc_yAxis(title = list(text = "Quantity")) %>%
        hc_chart(type = "line") %>% 
        hc_add_series(name = "Tổng số-nhóm tuổi từ 15-19", data = laodong_tuoi$`15-19`) %>% 
        hc_add_series(name = "Tổng số-nhóm tuổi từ 20-24", data = laodong_tuoi$`20-24`) %>% 
        hc_add_series(name = "Tổng số-nhóm tuổi từ 25-29", data = laodong_tuoi$`25-29`) %>% 
        hc_add_series(name = "Tổng số-nhóm tuổi từ 30-34", data = laodong_tuoi$`30-34`) %>% 
        hc_add_series(name = "Tổng số-nhóm tuổi từ 35-39", data = laodong_tuoi$`35-39`) %>% 
        hc_add_series(name = "Tổng số-nhóm tuổi từ 40-44", data = laodong_tuoi$`40-44`) %>% 
        hc_add_series(name = "Tổng số-nhóm tuổi từ 45-49", data = laodong_tuoi$`45-49`) %>% 
        hc_add_series(name = "Tổng số-nhóm tuổi từ 50+", data = laodong_tuoi$`50+`) %>% 
        hc_tooltip(split= TRUE)
    } else {
      hc %>% 
        hc_yAxis(title = list(text = "Percentage")) %>%
        hc_chart(type = "line") %>% 
        hc_add_series(name = "Tỷ lệ so với tổng dân số-nhóm tuổi từ 15-19", data = laodong_tuoi$`15-19_`) %>% 
        hc_add_series(name = "Tỷ lệ so với tổng dân số-nhóm tuổi từ 20-24", data = laodong_tuoi$`20-24_`) %>% 
        hc_add_series(name = "Tỷ lệ so với tổng dân số-nhóm tuổi từ 25-29", data = laodong_tuoi$`25-29_`) %>% 
        hc_add_series(name = "Tỷ lệ so với tổng dân số-nhóm tuổi từ 30-34", data = laodong_tuoi$`30-34_`) %>% 
        hc_add_series(name = "Tỷ lệ so với tổng dân số-nhóm tuổi từ 35-39", data = laodong_tuoi$`35-39_`) %>% 
        hc_add_series(name = "Tỷ lệ so với tổng dân số-nhóm tuổi từ 40-44", data = laodong_tuoi$`40-44_`) %>% 
        hc_add_series(name = "Tỷ lệ so với tổng dân số-nhóm tuổi từ 45-49", data = laodong_tuoi$`45-49_`) %>% 
        hc_add_series(name = "Tỷ lệ so với tổng dân số-nhóm tuổi từ 50+", data = laodong_tuoi$`50+_`) %>% 
        hc_tooltip(split= TRUE, valueSuffix = "%")
    }
  })
  
  
  output$chart6 <- renderHighchart({
    laodong_nganhkt <- laodong_nganhkt %>% filter(Year >= min(input$year8) & Year <= max(input$year8))
    hc <- highchart() %>% 
      hc_xAxis(categories = laodong_nganhkt$Year) %>% 
      hc_title(text = "Labour by kinds of economic activity",
               style = list(color = "#0A27F9", useHTML = TRUE, fontSize= '16px',fontWeight= 'bold')) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_subtitle(text = "Source: GSO.com", style = list(color = "#0A27F9")) %>% 
      hc_add_theme(hc_theme_ffx())
    
    if (input$eco == "Nonglamnghiep") {
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng ngành nông nghiệp, lâm nghiệp và thủy sản" , 
                      color = input$col6_1, data = laodong_nganhkt$Nonglamnghiep, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Cơ cấu ngành nông nghiệp, lâm nghiệp và thủy sản" ,
                      color = input$col6_2,data = laodong_nganhkt$Nonglamnghiep_, yAxis = 1)
      
    } else if (input$eco == "Khaikhoang") {
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng ngành khai khoáng" ,
                      color = input$col6_1,data = laodong_nganhkt$Khaikhoang, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Cơ cấu ngành khai khoáng" , 
                      color = input$col6_2,data = laodong_nganhkt$Khaikhoang_, yAxis = 1)
      
    } else if (input$eco == "Cnchebienchetao") {
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng ngành công nghiệp chế biến, chế tạo" ,
                      color = input$col6_1,data = laodong_nganhkt$Cnchebienchetao, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Cơ cấu ngành công nghiệp chế biến chế tạo" ,
                      color = input$col6_2,data = laodong_nganhkt$Cnchebienchetao_, yAxis = 1)
      
    } else if (input$eco == "sanxuatvaphanphoi"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng ngành sản xuất và phân phối điện, khí đốt, nước nóng, hơi nước và điều hòa không khí" ,
                      color = input$col6_1, data = laodong_nganhkt$sanxuatvaphanphoi, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Cơ cấu ngành sản xuất và phân phối điện, khí đốt, nước nóng, hơi nước và điều hòa không khí" ,
                      color = input$col6_2,data = laodong_nganhkt$sanxuatvaphanphoi_, yAxis = 1)
      
    } else if(input$eco == "Cungcapnuoc"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng ngành cung cấp nước; hoạt động quản lý và xử lý rác thải, nước thải" ,
                      color = input$col6_1,data = laodong_nganhkt$Cungcapnuoc, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Cơ cấu ngành cung cấp nước; hoạt động quản lý và xử lý rác thải, nước thải" ,
                      color = input$col6_2,data = laodong_nganhkt$Cungcapnuoc_, yAxis = 1)
      
    } else if(input$eco == "Xaydung"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng ngành xây dựng" , 
                      color = input$col6_1,data = laodong_nganhkt$Xaydung, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Cơ cấu ngành xây dựng" , 
                      color = input$col6_2,data = laodong_nganhkt$Xaydung_, yAxis = 1)
      
    } else if(input$eco == "Banbuonbanle"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng ngành bán buôn và bán lẻ; sửa chữa ô tô, mô tô, xe máy và xe có động cơ khác" , 
                      color = input$col6_1,data = laodong_nganhkt$Banbuonbanle, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Cơ cấu ngành bán buôn bán lẻ; sửa chữa ô tô, mô tô, xe máy và xe có động cơ khác" , 
                      color = input$col6_2,data = laodong_nganhkt$Banbuonbanle_, yAxis = 1)
      
    } else if(input$eco == "Vantai"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng ngành vận tải, kho bãi" , 
                      color = input$col6_1,data = laodong_nganhkt$Vantai, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Cơ cấu ngành vận tải, kho bãi" , 
                      color = input$col6_2,data = laodong_nganhkt$Vantai_, yAxis = 1)
      
    } else if(input$eco == "Dichvu"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng ngành dịch vụ lưu trữ và ăn uống" ,
                      color = input$col6_1,data = laodong_nganhkt$Dichvu, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Cơ cấu ngành dịch vụ lưu trữ và ăn uống" ,
                      color = input$col6_2,data = laodong_nganhkt$Dichvu_, yAxis = 1)
      
    } else if(input$eco == "Thongtinvatruyenthong"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng ngành thông tin và truyền thông" , 
                      color = input$col6_1, data = laodong_nganhkt$Thongtinvatruyenthong, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Cơ cấu ngành thông tin và truyền thông" , 
                      color = input$col6_2, data = laodong_nganhkt$Thongtinvatruyenthong_, yAxis = 1)
      
    } else if(input$eco == "hdtaichinh"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng ngành hoạt động tài chính, ngân hàng và bảo hiểm" ,
                      color = input$col6_1,data = laodong_nganhkt$hdtaichinh, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Cơ cấu ngành hoạt động tài chính, ngân hàng và bảo hiểm" ,
                      color = input$col6_2,data = laodong_nganhkt$hdtaichinh_, yAxis = 1)
      
    } else if(input$eco == "hdkinhdoanh"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng ngành hoạt động kinh doanh bất động sản" ,
                      color = input$col6_1, data = laodong_nganhkt$hdkinhdoanh, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Cơ cấu ngành hoạt động kinh doanh bất động sản" ,
                      color = input$col6_2, data = laodong_nganhkt$hdkinhdoanh_, yAxis = 1)
      
    } else if(input$eco == "hdchuyenmon"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng ngành hoạt động chuyên môn, khoa học và công nghệ" , 
                      color = input$col6_1,data = laodong_nganhkt$hdchuyenmon, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Cơ cấu ngành hoạt động chuyên môn, khoa học và công nghệ" ,
                      color = input$col6_2,data = laodong_nganhkt$hdchuyenmon_, yAxis = 1)
      
    } else if(input$eco == "hdhanhchinh"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng ngành hành chính và dịch vụ hỗ trợ" ,
                      color = input$col6_1,data = laodong_nganhkt$hdhanhchinh, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Cơ cấu ngành hành chính và dịch vụ hỗ trợ" ,
                      color = input$col6_2, data = laodong_nganhkt$hdhanhchinh_, yAxis = 1)
      
    } else if(input$eco == "hddang"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng ngành hoạt động của Đảng Cộng sản, tổ chức chính trị-xã hội; quản lý Nhà nươc, an ninh quốc phòng;đảm bảo xã hội bắt buộc"
                      ,color = input$col6_1, data = laodong_nganhkt$hddang, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Cơ cấu ngành hoạt động của Đảng Cộng sản, tổ chức chính trị-xã hội; quản lý Nhà nươc, an ninh quốc phòng; đảm bảo xã hội bắt buộc"
                      ,color = input$col6_2, data = laodong_nganhkt$hddang_, yAxis = 1)
      
    } else if(input$eco == "giaoduc"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng ngành giáo dục và đào tạo" , 
                      color = input$col6_1,data = laodong_nganhkt$giaoduc, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Cơ cấu ngành giáo dục và đào tạo" , 
                      color = input$col6_2,data = laodong_nganhkt$giaoduc_, yAxis = 1)
      
    } else if(input$eco == "yte"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng ngành y tế và hoạt động trợ giúp xã hội" , 
                      color = input$col6_1,data = laodong_nganhkt$yte, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Cơ cấu ngành y tế và hoạt động trợ giúp xã hội" , 
                      color = input$col6_2,data = laodong_nganhkt$yte_, yAxis = 1)
      
    } else if(input$eco == "nghethuat"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng ngành nghệ thuật, vui chơi và giải trí" ,
                      color = input$col6_1, data = laodong_nganhkt$nghethuat, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Cơ cấu ngành nghệ thuật, vui chơi và giải trí" ,
                      color = input$col6_2, data = laodong_nganhkt$nghethuat_, yAxis = 1)
      
    } else if(input$eco == "hddichvu"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng ngành hoạt động dịch vụ khác" ,
                      color = input$col6_1, data = laodong_nganhkt$hddichvu, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Cơ cấu ngành hoạt động dịch vụ khác" ,
                      color = input$col6_2, data = laodong_nganhkt$hddichvu_, yAxis = 1)
      
    } else if(input$eco == "hdlamthue"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng ngành hoạt động làm thuê các công việc trong các hộ gia đình, sản xuất sản phẩm, vật chất và dịch vụ tiêu dùng của hộ gia đình"
                      ,color = input$col6_1, data = laodong_nganhkt$hdlamthue, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Cơ cấu ngành hoạt động làm thuê các công việc trong các hộ gia đình, sản xuất sản phẩm, vật chất và dịch vụ tiêu dùng của hộ gia đình" 
                      ,color = input$col6_2, data = laodong_nganhkt$hdlamthue_, yAxis = 1)
      
    }else if(input$eco == "hdtochuc"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column",name = "Tổng ngành hoạt động của các tổ chức và cơ quan quốc tế" ,
                      color = input$col6_1,data = laodong_nganhkt$hdtochuc, yAxis = 0) %>% 
        hc_add_series(type = "line",name = "Cơ cấu ngành hoạt động của các tổ chức và cơ quan quốc tế" ,
                      color = input$col6_2,data = laodong_nganhkt$hdtochuc_, yAxis = 1)
      
    } else if (input$eco == "Tổng tất cả các ngành"){
      hc %>% hc_chart(type = "line") %>% 
        hc_yAxis(title = list(text = "Quantity")) %>%
        hc_add_series(name = "Nông nghiệp, lâm nghiệp và thủy sản" , data =  laodong_nganhkt$Nonglamnghiep) %>%
        hc_add_series(name = "Khai khoáng" , data =  laodong_nganhkt$Khaikhoang) %>% 
        hc_add_series(name = "Công nghiệp chế biến, chế tạo" , data =  laodong_nganhkt$Cnchebienchetao) %>%
        hc_add_series(name = "Sản xuất và phân phối điện, khí đốt, nước nóng, hơi nước và điều hòa không khí" , data =  laodong_nganhkt$sanxuatvaphanphoi) %>% 
        hc_add_series(name = "Cung cấp nước; hoạt động quản lý và xử lý rác thải, nước thải" , data =  laodong_nganhkt$Cungcapnuoc) %>% 
        hc_add_series(name = "Xây dựng" , data =  laodong_nganhkt$Xaydung) %>% 
        hc_add_series(name = "Bán buôn và bán lẻ; sửa chữa ô tô, mô tô, xe máy và xe có động cơ khác" , data =  laodong_nganhkt$Banbuonbanle) %>% 
        hc_add_series(name = "Vận tải, kho bãi" , data =laodong_nganhkt$Vantai) %>% 
        hc_add_series(name = "Dịch vụ lưu trú và ăn uống" , data = laodong_nganhkt$Dichvu) %>% 
        
        hc_add_series(name = "Thông tin và truyền thông" , data =  laodong_nganhkt$Thongtinvatruyenthong) %>% 
        hc_add_series(name = "Hoạt động tài chính, ngân hàng và bảo hiểm" , data =  laodong_nganhkt$hdtaichinh) %>% 
        hc_add_series(name = "Hoạt động kinh doanh bất động sản" , data =  laodong_nganhkt$hdkinhdoanh) %>% 
        hc_add_series(name = "Hoạt động chuyên môn, khoa học và công nghệ" , data =  laodong_nganhkt$hdchuyenmon) %>% 
        hc_add_series(name = "Hoạt động hành chính và dịch vụ hỗ trợ" , data =  laodong_nganhkt$hdhanhchinh) %>% 
        hc_add_series(name = "Hoạt động của Đảng Cộng sản, tổ chức chính trị - xã hội; quản lý Nhà nước, an ninh quốc phòng; đảm bảo xã hội bắt buộc" , data = laodong_nganhkt$hddang) %>% 
        hc_add_series(name = "Giáo dục và đào tạo" , data = laodong_nganhkt$giaoduc) %>% 
        hc_add_series(name = "Y tế và hoạt động trợ giúp xã hội" , data = laodong_nganhkt$yte) %>% 
        hc_add_series(name = "Nghệ thuật, vui chơi và giải trí" , data = laodong_nganhkt$nghethuat) %>% 
        hc_add_series(name = "Hoạt động dịch vụ khác" , data = laodong_nganhkt$hddichvu) %>% 
        hc_add_series(name = "Hoạt động làm thuê các công việc trong các hộ gia đình, sản xuất sản phẩm, vật chất và dịch vụ tiêu dùng của hộ gia đình" , data = laodong_nganhkt$hdlamthue) %>% 
        hc_add_series(name = "Hoạt động của các tổ chức và cơ quan quốc tế", data = laodong_nganhkt$hdtochuc) %>% 
        hc_legend(enabled = F) %>% 
        hc_tooltip(split = TRUE)
      
    } else {
      hc %>% hc_chart(type = "line") %>% 
        hc_yAxis(title = list(text = "Percentage")) %>%
        hc_add_series(name = "Nông nghiệp, lâm nghiệp và thủy sản" , data =  laodong_nganhkt$Nonglamnghiep_) %>%
        hc_add_series(name = "Khai khoáng" , data =  laodong_nganhkt$Khaikhoang_) %>% 
        hc_add_series(name = "Công nghiệp chế biến, chế tạo" , data =  laodong_nganhkt$Cnchebienchetao_) %>%
        hc_add_series(name = "Sản xuất và phân phối điện, khí đốt, nước nóng, hơi nước và điều hòa không khí" , data =  laodong_nganhkt$sanxuatvaphanphoi_) %>% 
        hc_add_series(name = "Cung cấp nước; hoạt động quản lý và xử lý rác thải, nước thải" , data =  laodong_nganhkt$Cungcapnuoc_) %>% 
        hc_add_series(name = "Xây dựng" , data =  laodong_nganhkt$Xaydung_) %>% 
        hc_add_series(name = "Bán buôn và bán lẻ; sửa chữa ô tô, mô tô, xe máy và xe có động cơ khác" , data =  laodong_nganhkt$Banbuonbanle_) %>% 
        hc_add_series(name = "Vận tải, kho bãi" , data =laodong_nganhkt$Vantai_) %>% 
        hc_add_series(name = "Dịch vụ lưu trú và ăn uống" , data = laodong_nganhkt$Dichvu_) %>% 
        
        hc_add_series(name = "Thông tin và truyền thông" , data =  laodong_nganhkt$Thongtinvatruyenthong_) %>% 
        hc_add_series(name = "Hoạt động tài chính, ngân hàng và bảo hiểm" , data =  laodong_nganhkt$hdtaichinh_) %>% 
        hc_add_series(name = "Hoạt động kinh doanh bất động sản" , data =  laodong_nganhkt$hdkinhdoanh_) %>% 
        hc_add_series(name = "Hoạt động chuyên môn, khoa học và công nghệ" , data =  laodong_nganhkt$hdchuyenmon_) %>% 
        hc_add_series(name = "Hoạt động hành chính và dịch vụ hỗ trợ" , data =  laodong_nganhkt$hdhanhchinh_) %>% 
        hc_add_series(name = "Hoạt động của Đảng Cộng sản, tổ chức chính trị - xã hội; quản lý Nhà nước, an ninh quốc phòng; đảm bảo xã hội bắt buộc" , data = laodong_nganhkt$hddang_) %>% 
        hc_add_series(name = "Giáo dục và đào tạo" , data = laodong_nganhkt$giaoduc_) %>% 
        hc_add_series(name = "Y tế và hoạt động trợ giúp xã hội" , data = laodong_nganhkt$yte_) %>% 
        hc_add_series(name = "Nghệ thuật, vui chơi và giải trí" , data = laodong_nganhkt$nghethuat_) %>% 
        hc_add_series(name = "Hoạt động dịch vụ khác" , data = laodong_nganhkt$hddichvu_) %>% 
        hc_add_series(name = "Hoạt động làm thuê các công việc trong các hộ gia đình, sản xuất sản phẩm, vật chất và dịch vụ tiêu dùng của hộ gia đình" , data = laodong_nganhkt$hdlamthue_) %>% 
        hc_add_series(name = "Hoạt động của các tổ chức và cơ quan quốc tế", data = laodong_nganhkt$hdtochuc_) %>% 
        hc_legend(enabled = F) %>% 
        hc_tooltip(split = TRUE, valueSuffix = "%")
    }
  })
  
  
  output$chart7 <- renderHighchart({
    laodong_nganhnghe <- laodong_nganhnghe %>% filter(Year >= min(input$ye1) & Year <= max(input$ye1))
    hc <- highchart() %>% 
      hc_xAxis(categories = laodong_nganhnghe$Year) %>% 
      hc_title(text = "Labour by occupation",
               style = list(color = "#0A27F9", useHTML = TRUE, fontSize= '16px',fontWeight= 'bold')) %>% 
      hc_subtitle(text = "Source: GSO.com",
                  style = list(color = "#0A27F9")) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 3) %>% 
      hc_exporting(enabled = TRUE) %>% 
      hc_add_theme(hc_theme_ffx())
    
    if (input$career == "Nhalanhdao"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column", name = "Tổng số nhà lãnh đạo", 
                      color = input$col7_1, data = laodong_nganhnghe$Nhalanhdao, yAxis = 0) %>% 
        hc_add_series(type = "line", name = "Tỷ lệ so với tổng dân số nhà lãnh đạo",
                      color = input$col7_2, data = laodong_nganhnghe$Nhalanhdao_, yAxis = 1)
      
    } else if (input$career == "Chuyenmonkythuatbaccao") {
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column", name = "Tổng số-chuyên môn kỹ thuật bậc cao",
                      color = input$col7_1,data = laodong_nganhnghe$Chuyenmonkythuatbaccao, yAxis = 0) %>% 
        hc_add_series(type = "line", name = "Tỷ lệ so với tổng dân số-chuyên môn kỹ thuật bậc cao",
                      color = input$col7_2, data = laodong_nganhnghe$Chuyenmonkythuatbaccao_, yAxis = 1)
      
    } else if (input$career == "Chuyenmonkythuatbactrung") {
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column", name = "Tổng số-chuyên môn kỹ thuật bậc trung",
                      color = input$col7_1, data = laodong_nganhnghe$Chuyenmonkythuatbactrung, yAxis = 0) %>% 
        hc_add_series(type = "line", name = "Tỷ lệ so với tổng dân số-chuyên môn kỹ thuật bậc trung",
                      color = input$col7_2, data = laodong_nganhnghe$Chuyenmonkythuatbactrung_, yAxis = 1)
      
    } else if (input$career == "Nhanvien") {
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column", name = "Tổng số-nhân viên",
                      color = input$col7_1, data = laodong_nganhnghe$Nhanvien, yAxis = 0) %>% 
        hc_add_series(type = "line", name = "Tỷ lệ so với tổng dân số-nhân viên",
                      color = input$col7_2, data = laodong_nganhnghe$Nhanvien_, yAxis = 1)
    } else if (input$career == "Dichvucanhan&baovebanhang"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column", name = "Tổng số-dịch vụ cá nhân, bảo vệ bán hàng",
                      color = input$col7_1, data = laodong_nganhnghe$`Dichvucanhan&baovebanhang`, yAxis = 0) %>% 
        hc_add_series(type = "line", name = "Tỷ lệ so với tổng dân số-dịch vụ cá nhân, bảo vệ bán hàng"
                      ,color = input$col7_2, data = laodong_nganhnghe$`Dichvucanhan&baovebanhang_`, yAxis = 1)
      
    } else if (input$career == "nonglamngunghiep"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column", name = "Tổng số-nghề trong nông, lâm, ngư nghiệp",
                      color = input$col7_1,data = laodong_nganhnghe$nonglamngunghiep, yAxis = 0) %>% 
        hc_add_series(type = "line", name = "Tỷ lệ so với tổng dân số-nghề trong nông, lâm, ngư nghiệp",
                      color = input$col7_2,data = laodong_nganhnghe$nonglamngunghiep_, yAxis = 1)
      
    } else if (input$career == "Thothhucong&cacthocolienquan"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column", name = "Tổng số-thợ thủ công và các thợ khác có liên quan",
                      color = input$col7_1, data = laodong_nganhnghe$`Thothhucong&cacthocolienquan`, yAxis = 0) %>% 
        hc_add_series(type = "line", name = "Tỷ lệ so với tổng dân số-thợ thủ công và các thợ khác có liên quan",
                      color = input$col7_2, data = laodong_nganhnghe$`Thothhucong&cacthocolienquan_`, yAxis = 1)
      
    } else if (input$career == "Tholaprap&vanhanhmaymocthietbi"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column", name = "Tổng số-thợ lắp ráp và vận hành máy móc thiết bị",
                      color = input$col7_1, data = laodong_nganhnghe$`Tholaprap&vanhanhmaymocthietbi`, yAxis = 0) %>% 
        hc_add_series(type = "line", name = "Tỷ lệ so với tổng dân số-thợ lắp ráp và vận hành máy móc thiết bị"
                      ,color = input$col7_2, data = laodong_nganhnghe$`Tholaprap&vanhanhmaymocthietbi_`, yAxis = 1)
      
    } else if (input$career == "Nghegiandon"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column", name = "Tổng số-nghề giản đơn", 
                      color = input$col7_1,data = laodong_nganhnghe$Nghegiandon, yAxis = 0) %>% 
        hc_add_series(type = "line", name = "Tỷ lệ so với tổng dân số-nghề giản đơn",
                      color = input$col7_2,data = laodong_nganhnghe$Nghegiandon_, yAxis = 1)
      
    } else if (input$career == "Khac"){
      hc %>% 
        hc_yAxis_multiples(list(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")), list(title = list(text = "Percentage"),labels = list(format = "{value}%"), opposite = TRUE)) %>% 
        hc_add_series(type = "column", name = "Tổng số - khác", 
                      color = input$col7_1, data = laodong_nganhnghe$Khac, yAxis = 0) %>% 
        hc_add_series(type = "line", name = "Tỷ lệ so với tổng dân số - khác",
                      color = input$col7_2,data = laodong_nganhnghe$Khac_, yAxis = 1)
      
    } else if (input$career == "Tổng tất cả các ngành"){
      hc %>% hc_chart(type = "line") %>% 
        hc_yAxis(title = list(text = "Quantity")) %>% 
        hc_add_series(name = "Nhà lãnh đạo", data = laodong_nganhnghe$Nhalanhdao) %>% 
        hc_add_series(name = "Chuyên môn kỹ thuật bậc cao", data = laodong_nganhnghe$Chuyenmonkythuatbaccao) %>% 
        hc_add_series(name = "Chuyên môn kỹ thuật bậc trung", data = laodong_nganhnghe$Chuyenmonkythuatbactrung) %>% 
        hc_add_series(name = "Nhân viên", data = laodong_nganhnghe$Nhanvien) %>% 
        hc_add_series(name = "Dịch vụ cá nhân, bảo vệ bán hàng", data = laodong_nganhnghe$"Dichvucanhan&baovebanhang") %>% 
        hc_add_series(name = "Nghề trong nông, lâm, ngư nghiệp", data = laodong_nganhnghe$nonglamngunghiep) %>% 
        hc_add_series(name = "Thợ thủ công và các thợ khác có liên quan", data = laodong_nganhnghe$"Thothhucong&cacthocolienquan") %>% 
        hc_add_series(name = "Thợ lắp ráp và vận hành máy móc, thiết bị", data = laodong_nganhnghe$"Tholaprap&vanhanhmaymocthietbi") %>% 
        hc_add_series(name = "Nghề giản đơn", data = laodong_nganhnghe$Nghegiandon) %>% 
        hc_add_series(name = "Khác", data = laodong_nganhnghe$Khac) %>% 
        hc_tooltip(split = TRUE)
    } else {
      hc %>% 
        hc_yAxis(title = list(text = "Percentage")) %>% 
        hc_tooltip(split= TRUE, valueSuffix = "%") %>% 
        hc_add_series(type = "line", name = "Nhà lãnh đạo", data = laodong_nganhnghe$Nhalanhdao_) %>% 
        hc_add_series(type = "line",name = "Chuyên môn kỹ thuật bậc cao", data = laodong_nganhnghe$Chuyenmonkythuatbaccao_) %>% 
        hc_add_series(type = "line",name = "Chuyên môn kỹ thuật bậc trung", data = laodong_nganhnghe$Chuyenmonkythuatbactrung_) %>% 
        hc_add_series(type = "line",name = "Nhân viên", data = laodong_nganhnghe$Nhanvien_) %>% 
        hc_add_series(type = "line",name = "Dịch vụ cá nhân, bảo vệ bán hàng", data = laodong_nganhnghe$`Dichvucanhan&baovebanhang_`) %>% 
        hc_add_series(type = "line",name = "Nghề trong nông, lâm, ngư nghiệp", data = laodong_nganhnghe$nonglamngunghiep_) %>% 
        hc_add_series(type = "line",name = "Thợ thủ công và các thợ khác có liên quan", data = laodong_nganhnghe$`Thothhucong&cacthocolienquan_`) %>% 
        hc_add_series(type = "line",name = "Thợ lắp ráp và vận hành máy móc, thiết bị", data = laodong_nganhnghe$`Tholaprap&vanhanhmaymocthietbi_`) %>% 
        hc_add_series(type = "line",name = "Nghề giản đơn", data = laodong_nganhnghe$Nghegiandon_) %>% 
        hc_add_series(type = "line",name = "Khác", data = laodong_nganhnghe$Khac_)
    } 
  })
  
  
  output$chart8 <- renderHighchart({
    hc <- highchart() %>% 
      hc_colors(c("#2574A9", "#22A7F0", "#03A678", "#EB974E", "#913D88")) %>% 
      hc_title(text = "Labor by status in employment",
               style = list(color = "#0A27F9", useHTML = TRUE, fontSize= '16px',fontWeight= 'bold')) %>% 
      hc_exporting(enabled = TRUE) %>% 
      hc_subtitle(text = "Source: GSO.com",
                  style = list(color = "#0A27F9")) %>% 
      hc_add_theme(hc_theme_ffx())
    if (input$labour3 == "Tổng số - nghìn người"){
      laodong_vithevieclam <- laodong_vithevieclam %>% filter(Year >= min(input$year4) & Year <= max(input$year4))
      hc %>% hc_xAxis(categories = laodong_vithevieclam$Year) %>% 
        hc_yAxis(title = list(text = "Quantity"),lineWidth = 0, labels = list(format = "{value}")) %>%
        hc_add_series(name = "Làm công ăn lương", 
                      color = input$col8_1, data = laodong_vithevieclam$Lamconganluong) %>% 
        hc_add_series(name = "Chủ cơ sở sản xuất kinh doan",
                      color = input$col8_2, data = laodong_vithevieclam$Chucososanxuatkinhdoanh) %>% 
        hc_add_series(name = "Tự làm", 
                      color = input$col8_3, data = laodong_vithevieclam$Tulam) %>% 
        hc_add_series(name = "Lao động gia đình", 
                      color = input$col8_4, data = laodong_vithevieclam$Laodonggiadinh) %>% 
        hc_add_series(name = "Xã viên hợp tác xã", 
                      color = input$col8_5, data = laodong_vithevieclam$Xavienhoptacxa) %>% 
        hc_tooltip(split= TRUE)
    } else {
      laodong_vithevieclam_tyle <- laodong_vithevieclam_tyle %>% filter(Year >= min(input$year4) & Year <= max(input$year4))
      hc %>% hc_xAxis(categories = laodong_vithevieclam_tyle$Year) %>%
        hc_yAxis(title = list(text = "Percentage"),lineWidth = 0, labels = list(format = "{value}%")) %>%
        hc_add_series(name = "Làm công ăn lương", 
                      color = input$col8_1, data = laodong_vithevieclam_tyle$Lamconganluong) %>% 
        hc_add_series(name = "Chủ cơ sở sản xuất kinh doan",
                      color = input$col8_2, data = laodong_vithevieclam_tyle$Chucososanxuatkinhdoanh) %>% 
        hc_add_series(name = "Tự làm", 
                      color = input$col8_3, data = laodong_vithevieclam_tyle$Tulam) %>% 
        hc_add_series(name = "Lao động gia đình", 
                      color = input$col8_4, data = laodong_vithevieclam_tyle$Laodonggiadinh) %>% 
        hc_add_series(name = "Xã viên hợp tác xã", 
                      color = input$col8_5, data = laodong_vithevieclam_tyle$Xavienhoptacxa) %>% 
        hc_tooltip(split= TRUE, valueSuffix = "%")
    }
  })
  
  
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(Clicks=NULL)
  
  # Leaflet map with 2 markers
  output$map <- renderLeaflet({
    index <- paste0(input$year)
    if (input$choice == "Lực lượng lao động"){
      leaflet() %>% addTiles() %>% 
        addWMSTiles("http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
                    layers = "nexrad-n0r-900913",
                    options = WMSTileOptions(format = "image/png", transparent = TRUE)) %>% 
        addMiniMap(toggleDisplay = TRUE) %>% 
        addPolygons(data=vn2,fillColor = ~pal(as.data.frame(vn2)[,index]), layerId = vn2$VARNAME_1,
                    fillOpacity = 0.9,color="red",weight=0.5,popup=~as.character(as.data.frame(vn2)[,index]),
                    label=~paste0(Province," Province",": ",as.data.frame(vn2)[,index])) %>%
        addLegend("bottomright",opacity=1,pal=pal,values=as.data.frame(vn2)[,index],title="Labour by province")
    } else {
      leaflet() %>% addTiles() %>% 
        addWMSTiles("http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
                    layers = "nexrad-n0r-900913",
                    options = WMSTileOptions(format = "image/png", transparent = TRUE)) %>% 
        addMiniMap(toggleDisplay = TRUE) %>% 
        addPolygons(data=vn3,fillColor = ~pal1(as.data.frame(vn3)[,index]), layerId = vn3$VARNAME_1,
                    fillOpacity = 0.9,color="red",weight=0.5,popup=~as.character(as.data.frame(vn3)[,index]),
                    label=~paste0(Province," Province",": ",as.data.frame(vn3)[,index])) %>%
        addLegend("bottomright",opacity=1,pal=pal1,values=as.data.frame(vn3)[,index],title="Labour by province")
      }
    })
  # store the click
  observeEvent(input$map_shape_click,{
    data_of_click$Clicks <- input$map_shape_click
    })
  
  # Make a barplot or scatterplot depending of the selected point
  output$plot <-  renderPlot({
    my_place <- data_of_click$Clicks$id
    if (input$choice == "Lực lượng lao động"){
      if(is.null(my_place)){ my_place = "Ha Noi"}
      data <- data %>% filter(Year <= input$year) %>%  filter(Province == my_place)
      ggplot(data, aes(x = Year, y = value)) +
        geom_bar(stat="identity", width=.5, fill = "blue") +
        geom_text(aes(label = value))+
        coord_flip()+
        labs(title="Bar Chart", subtitle= "Source: GSO.com", y = "Quantity") 
    } else {
      if(is.null(my_place)){my_place = "Ha Noi"} 
      data2 <- data2 %>% filter(Year <= input$year) %>% filter(Province == my_place)
      ggplot(data2, aes(x = Year, y = value)) +
        geom_bar(stat="identity", width=.5, fill= "#412AC7") +
        geom_text(aes(label = value)) +
        coord_flip()+
        labs(title="Bar Chart", subtitle= "Source: GSO.com", y = "Percentage")
    }
  })
  
  output$chart10_1 <- renderHighchart({
    hc <- highchart() %>% 
      hc_xAxis(categories = laodong_daotao$Year) %>% 
      hc_yAxis(title = list(text = "Percentage"),lineWidth = 0, labels = list(format = "{value}%")) %>%
      hc_chart(type = "column") %>% 
      hc_tooltip(split= TRUE, valueSuffix = "%") %>% 
      hc_subtitle(text = "Source: GSO.com",
                  style = list(color = "#0A27F9")) %>% 
      hc_add_theme(hc_theme_ffx())
    if (input$cho == "Phân theo giới tính"){
      laodong_daotao <- laodong_daotao %>% filter(Year >= min(input$year5) & Year <= max(input$year5))
      hc %>% 
        hc_add_series(type = "column",name = "Male", 
                      color = input$col10_1, data = laodong_daotao$Male) %>% 
        hc_add_series(type = "column", name = "Female", 
                      color = input$col10_2, data = laodong_daotao$Female) %>% 
        hc_title(text = "Percentage of trained employed workers by sex",
                 style = list(color = "#0A27F9", useHTML = TRUE, fontSize= '16px',fontWeight= 'bold'))
    } else {
      laodong_daotao <- laodong_daotao %>% filter(Year >= min(input$year5) & Year <= max(input$year5))
      hc %>% 
        hc_add_series(type = "column",name = "Urban", 
                      color = input$col10_1, data = laodong_daotao$Urban) %>% 
        hc_add_series(type = "column",name = "Rural", 
                      color = input$col10_2, data = laodong_daotao$Rural) %>% 
        hc_title(text = "Percentage of trained employed workers by residence",
                 style = list(color = "#0A27F9", useHTML = TRUE, fontSize= '16px',fontWeight= 'bold'))
    }
  })
  
  output$chart10_2 <- renderHighchart({
    laodong_trongtuoilaodong <- laodong_trongtuoilaodong %>% filter(Year >= min(input$year5_1) & Year <= max(input$year5_1))
    hc <- highchart() %>% 
      hc_xAxis(categories = laodong_trongtuoilaodong$Year) %>% 
      hc_yAxis(title = list(text = "Percentage"),lineWidth = 0, labels = list(format = "{value}%")) %>%
      hc_chart(type = "column") %>% 
      hc_tooltip(split= TRUE, valueSuffix = "%") %>% 
      hc_subtitle(text = "Source: GSO.com",
                  style = list(color = "#0A27F9")) %>% 
      hc_add_theme(hc_theme_ffx())
    if (input$cho1 == "Phân theo giới tính"){
      hc %>% 
        hc_add_series(type = "column",name = "Male", 
                      color = input$col10_1,data = laodong_trongtuoilaodong$Male) %>% 
        hc_add_series(type = "column", name = "Female", 
                      color = input$col10_2,data = laodong_trongtuoilaodong$Female) %>% 
        hc_title(text = "Percentage of trained employed workers by sex",
                 style = list(color = "#0A27F9", useHTML = TRUE, fontSize= '16px',fontWeight= 'bold'))
    } else {
      hc %>% hc_add_series(type = "column",name = "Urban", 
                           color = input$col10_1,data = laodong_trongtuoilaodong$Urban) %>% 
        hc_add_series(type = "column",name = "Rural", 
                      color = input$col10_2,data = laodong_trongtuoilaodong$Rural) %>% 
        hc_title(text = "Percentage of trained employed workers by residence",
                 style = list(color = "#0A27F9", useHTML = TRUE, fontSize= '16px',fontWeight= 'bold'))
    }
  })
  
  
  output$chart11 <- renderHighchart({
    laodong_daotao_tuoi <- laodong_daotao_tuoi %>% filter(Year >= min(input$year6) & Year <= max(input$year6))
    hc <- highchart() %>% 
      hc_xAxis(categories = laodong_daotao_tuoi$Year) %>% 
      hc_yAxis(title = list(text = "Percentage"),lineWidth = 0, labels = list(format = "{value}%")) %>%
      hc_chart(type = "column") %>% 
      hc_title(text = "Percentage of trained employed workers by age group",
               style = list(color = "#0A27F9", useHTML = TRUE, fontSize= '16px',fontWeight= 'bold')) %>% 
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 3,
                 split= TRUE, valueSuffix = "%") %>% 
      hc_exporting(enabled = TRUE) %>% 
      hc_subtitle(text = "Source: GSO.com",
                  style = list(color = "#0A27F9")) %>% 
      hc_add_theme(hc_theme_ffx())
    if (input$tuoi == "15-19") {
      hc %>% hc_add_series(name = "nhóm tuổi từ 15-19", color = input$col11, data = laodong_daotao_tuoi$`15-19`) 
    } else if (input$tuoi == "20-24"){
      hc %>% hc_add_series(name = "nhóm tuổi từ 20-24", color = input$col11, data = laodong_daotao_tuoi$`20-24`)
    } else if (input$tuoi == "25-29") {
      hc %>% hc_add_series(name = "nhóm tuổi từ 25-29", color = input$col11, data = laodong_daotao_tuoi$`25-29`)
    } else if (input$tuoi == "30-34"){
      hc %>% hc_add_series(name = "nhóm tuổi từ 30-34", color = input$col11, data = laodong_daotao_tuoi$`30-34`) 
    } else if (input$tuoi == "35-39"){
      hc %>% hc_add_series(name = "nhóm tuổi từ 35-39", color = input$col11, data = laodong_daotao_tuoi$`35-39`) 
    } else if (input$tuoi == "40-44"){
      hc %>% hc_add_series(name = "nhóm tuổi từ 40-44", color = input$col11, data = laodong_daotao_tuoi$`40-44`) 
    } else if (input$tuoi == "45-49"){
      hc %>% hc_add_series(name = "nhóm tuổi từ 45-49", color = input$col11, data = laodong_daotao_tuoi$`45-49`) 
    } else if (input$tuoi == "50+"){
      hc %>% hc_add_series(name = "nhóm tuổi từ 50+", color = input$col11, data = laodong_daotao_tuoi$`50+`) 
    } else {
      hc %>% hc_chart(type = "line") %>% 
        hc_colors(c("#52B3D9","#00B16A","#36D7B7", "#95A5A6","#F9BF3B", "#F2784B","#96281B","#F22613")) %>% 
        hc_add_series(name = "nhóm tuổi từ 15-19", data = laodong_daotao_tuoi$`15-19`) %>% 
        hc_add_series(name = "nhóm tuổi từ 20-24", data = laodong_daotao_tuoi$`20-24`) %>% 
        hc_add_series(name = "nhóm tuổi từ 25-29", data = laodong_daotao_tuoi$`25-29`) %>% 
        hc_add_series(name = "nhóm tuổi từ 30-34", data = laodong_daotao_tuoi$`30-34`) %>% 
        hc_add_series(name = "nhóm tuổi từ 35-39", data = laodong_daotao_tuoi$`35-39`) %>% 
        hc_add_series(name = "nhóm tuổi từ 40-44", data = laodong_daotao_tuoi$`40-44`) %>% 
        hc_add_series(name = "nhóm tuổi từ 45-49", data = laodong_daotao_tuoi$`45-49`) %>% 
        hc_add_series(name = "nhóm tuổi từ 50+", data = laodong_daotao_tuoi$`50+`)
    }
  })
  
  output$chart12 <- renderHighchart({
    hc <- highchart() %>% 
      hc_xAxis(categories = laodong_trinhdo$Year) %>% 
      hc_yAxis(title = list(text = "Percentage"),lineWidth = 0, labels = list(format = "{value}%")) %>%
      hc_add_series(name = "Dạy nghề", data = laodong_trinhdo$Daynghe,
                    dataLabels = list(enabled = T, color = "black")) %>% 
      hc_add_series(name = "Trung cấp chuyên nghiệp", data = laodong_trinhdo$Trungcapchuyennghiep,
                    dataLabels = list(enabled = T, color = "black")) %>% 
      hc_add_series(name = "Cao đẳng", data = laodong_trinhdo$Caodang,
                    dataLabels = list(enabled = T, color = "black")) %>% 
      hc_add_series(name = "Đại học trở lên", data = laodong_trinhdo$Daihoctrolen,
                    dataLabels = list(enabled = T, color = "black")) %>% 
      hc_chart(type = "area") %>% 
      hc_tooltip(split= TRUE, valueSuffix = "%") %>% 
      hc_plotOptions(series = list(stacking = "normal")) %>% 
      hc_colors(colors = c("#5C97BF","#26C281", "#65C6BB", "#F2784B")) %>% 
      hc_title(text = "Percentage of trained employed workers at 15 years of age and above by qualification",
               style = list(color = "#0A27F9", useHTML = TRUE, fontSize= '16px',fontWeight= 'bold')) %>% 
      hc_subtitle(text = "Source: GSO.com",
                  style = list(color = "#0A27F9")) %>% 
      hc_add_theme(hc_theme_ffx())
  })
  
  output$chart13 <- renderHighchart({
    nangsuatlaodong <- nangsuatlaodong %>% 
      filter(Year >= min(input$rangeyear) & Year <= max(input$rangeyear))
    hc <- highchart() %>% 
      hc_xAxis(categories = nangsuatlaodong$Year) %>% 
      hc_yAxis(title = list(text = "Trieu dong/nguoi"),lineWidth = 0, labels = list(format = "{value}")) %>% 
      hc_chart(type = "line") %>% 
      hc_title(text = "Productivity of employed population by industry",
               style = list(color = "#0A27F9", useHTML = TRUE, fontSize= '16px',fontWeight= 'bold')) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_subtitle(text = "Source: GSO.com",
                  style = list(color = "#0A27F9")) %>% 
      hc_add_theme(hc_theme_ffx())
    
    if (input$industry == "nln"){
      hc %>% 
        hc_add_series(name = "Nông nghiệp, lâm nghiệp và thủy sản", color = input$col13, data = nangsuatlaodong$Nonglamnghiep)
      
    } else if (input$industry == "kk"){
      hc %>% 
        hc_add_series(name = "Khai khoáng" ,  color = input$col13,data = nangsuatlaodong$Khaikhoang)
      
    } else if (input$industry == "cncbct"){
      hc %>% 
        hc_add_series(name = "Công nghiệp chế biến, chế tạo" , color = input$col13, data = nangsuatlaodong$Cnchebienchetao)
      
    } else if (input$industry == "sxpp"){
      hc %>% 
        hc_add_series(name = "Sản xuất và phân phối điện, khí đốt, nước nóng, hơi nước và điều hòa không khí" 
                      , color = input$col13, data = nangsuatlaodong$sanxuatvaphanphoi)
      
    } else if(input$industry == "ccn"){
      hc %>% 
        hc_add_series(name = "Cung cấp nước; hoạt động quản lý và xử lý rác thải, nước thải" ,
                      color = input$col13,data = nangsuatlaodong$Cungcapnuoc) 
      
    } else if(input$industry == "xd"){
      hc %>% 
        hc_add_series(name = "Xây dựng" , 
                      color = input$col13,data = nangsuatlaodong$Xaydung)
      
    } else if(input$industry == "bbbl"){
      hc %>% 
        hc_add_series(name = "Bán buôn và bán lẻ; sửa chữa ô tô, mô tô, xe máy và xe có động cơ khác" , 
                      color = input$col13,data = nangsuatlaodong$Banbuonbanle)
      
    } else if(input$industry == "vt"){
      hc %>% 
        hc_add_series(name = "Vận tải, kho bãi" , 
                      color = input$col13, data = nangsuatlaodong$Vantai)
      
    } else if(input$industry == "dv"){
      hc %>% 
        hc_add_series(name = "Dịch vụ lưu trú và ăn uống" ,
                      color = input$col13,data = nangsuatlaodong$Dichvu)
      
    } else if(input$industry == "tttt"){
      hc %>% 
        hc_add_series(name = "Thông tin và truyền thông" , 
                      color = input$col13,data = nangsuatlaodong$Thongtinvatruyenthong)
      
    } else if(input$industry == "hdtc"){
      hc %>%
        hc_add_series(name = "Hoạt động tài chính, ngân hàng và bảo hiểm" , 
                      color = input$col13,data = nangsuatlaodong$hdtaichinh)
      
    } else if(input$industry == "hdkd"){
      hc %>% 
        hc_add_series(name = "Hoạt động kinh doanh bất động sản" ,
                      color = input$col13, data = nangsuatlaodong$hdkinhdoanh)
      
    } else if(input$industry == "hdcm"){
      hc %>% 
        hc_add_series(name = "Hoạt động chuyên môn, khoa học và công nghệ" , 
                      color = input$col13,data = nangsuatlaodong$hdchuyenmon)
      
    } else if(input$industry == "hdhc"){
      hc %>% 
        hc_add_series(name = "Hoạt động hành chính và dịch vụ hỗ trợ" ,
                      color = input$col13,data = nangsuatlaodong$hdhanhchinh)
      
    } else if(input$industry == "hdd"){
      hc %>% 
        hc_add_series(name = "Hoạt động của Đảng Cộng sản, tổ chức chính trị - xã hội; quản lý Nhà nước, an ninh quốc phòng; đảm bảo xã hội bắt buộc" 
                      , color = input$col13, data = nangsuatlaodong$hddang)
      
    } else if(input$industry == "gd"){
      hc %>% 
        hc_add_series(name = "Giáo dục và đào tạo" , 
                      color = input$col13,data = nangsuatlaodong$giaoduc)
      
    } else if(input$industry == "yt"){
      hc %>% 
        hc_add_series(name = "Y tế và hoạt động trợ giúp xã hội" , 
                      color = input$col13, data = nangsuatlaodong$yte)
      
    } else if(input$industry == "nghet"){
      hc %>% 
        hc_add_series(name = "Nghệ thuật, vui chơi và giải trí" , 
                      color = input$col13,data = nangsuatlaodong$nghethuat)
      
    } else if(input$industry == "hddv"){
      hc %>% 
        hc_add_series(name = "Hoạt động dịch vụ khác" ,
                      color = input$col13,data = nangsuatlaodong$hddichvu)
      
    } else if(input$industry == "hdlt"){
      hc %>% 
        hc_add_series(name = "Hoạt động làm thuê các công việc trong các hộ gia đình, sản xuất sản phẩm, vật chất và dịch vụ tiêu dùng của hộ gia đình"
                      , color = input$col13, data = nangsuatlaodong$hdlamthue)
      
    } else{
      hc %>% hc_add_series(name = "Nông nghiệp, lâm nghiệp và thủy sản" , data = nangsuatlaodong$Nonglamnghiep) %>% 
        hc_add_series(name = "Khai khoáng" , data = nangsuatlaodong$Khaikhoang) %>% 
        hc_add_series(name = "Công nghiệp chế biến, chế tạo" , data = nangsuatlaodong$Cnchebienchetao) %>% 
        hc_add_series(name = "Sản xuất và phân phối điện, khí đốt, nước nóng, hơi nước và điều hòa không khí" , data = nangsuatlaodong$sanxuatvaphanphoi) %>% 
        hc_add_series(name = "Cung cấp nước; hoạt động quản lý và xử lý rác thải, nước thải" , data = nangsuatlaodong$Cungcapnuoc) %>% 
        hc_add_series(name = "Xây dựng" , data = nangsuatlaodong$Xaydung) %>% 
        hc_add_series(name = "Bán buôn và bán lẻ; sửa chữa ô tô, mô tô, xe máy và xe có động cơ khác" , data = nangsuatlaodong$Banbuonbanle) %>% 
        hc_add_series(name = "Vận tải, kho bãi" , data = nangsuatlaodong$Vantai) %>% 
        hc_add_series(name = "Dịch vụ lưu trú và ăn uống" , data = nangsuatlaodong$Dichvu) %>% 
        hc_add_series(name = "Thông tin và truyền thông" , data = nangsuatlaodong$Thongtinvatruyenthong) %>% 
        hc_add_series(name = "Hoạt động tài chính, ngân hàng và bảo hiểm" , data = nangsuatlaodong$hdtaichinh) %>% 
        hc_add_series(name = "Hoạt động kinh doanh bất động sản" , data = nangsuatlaodong$hdkinhdoanh) %>% 
        hc_add_series(name = "Hoạt động chuyên môn, khoa học và công nghệ" , data = nangsuatlaodong$hdchuyenmon) %>% 
        hc_add_series(name = "Hoạt động hành chính và dịch vụ hỗ trợ" , data = nangsuatlaodong$hdhanhchinh) %>% 
        hc_add_series(name = "Hoạt động của Đảng Cộng sản, tổ chức chính trị - xã hội; quản lý Nhà nước, an ninh quốc phòng; đảm bảo xã hội bắt buộc" , data = nangsuatlaodong$hddang) %>% 
        hc_add_series(name = "Giáo dục và đào tạo" , data = nangsuatlaodong$giaoduc) %>% 
        hc_add_series(name = "Y tế và hoạt động trợ giúp xã hội" , data = nangsuatlaodong$yte) %>% 
        hc_add_series(name = "Nghệ thuật, vui chơi và giải trí" , data = nangsuatlaodong$nghethuat) %>% 
        hc_add_series(name = "Hoạt động dịch vụ khác" , data = nangsuatlaodong$hddichvu) %>% 
        hc_add_series(name = "Hoạt động làm thuê các công việc trong các hộ gia đình, sản xuất sản phẩm, vật chất và dịch vụ tiêu dùng của hộ gia đình" , data = nangsuatlaodong$hdlamthue) %>% 
        hc_legend(enabled = F) %>%
        hc_tooltip(split = TRUE)
    }
  })
  
  output$chart14_1 <- renderHighchart({
    thatnghiep <- thatnghiep %>% filter(Year >= min(input$year7) & Year <= max(input$year7))
    hc <- highchart() %>% 
      hc_xAxis(categories = thatnghiep$Year) %>% 
      hc_yAxis(titile = list(text = "Percentage"),lineWidth = 0, labels = list(format = "{value}%")) %>% 
      hc_chart(type = "column") %>% 
      hc_tooltip(split= TRUE, valueSuffix = "%") %>% 
      hc_title(text = "Unemployment rate of labour force at working age",
               style = list(color = "#0A27F9", useHTML = TRUE, fontSize= '16px',fontWeight= 'bold')) %>% 
      hc_subtitle(text = "Source: GSO.com",
                  style = list(color = "#0A27F9")) %>% 
      hc_add_theme(hc_theme_ffx())
    if (input$area == "CaNuoc1"){
      hc %>%
        hc_add_series(name = "Cả Nước", 
                      color = input$col14_1, data = thatnghiep$CaNuoc)
    } else if (input$area == "Trungdumiennui1"){
      hc %>% 
        hc_add_series(name = "Trung du và miền núi phía Bắc",
                      color = input$col14_1, data = thatnghiep$Trungdumiennui) 
    } else if (input$area == "Bactrungbo1"){
      hc %>% 
        hc_add_series(name = "Bắc Trung Bộ và duyên hải miền Trung",
                      color = input$col14_1,data = thatnghiep$Bactrungbo)
    } else if (input$area == "TayNguyen1"){
      hc %>% 
        hc_add_series(name = "Tây Nguyên", color = input$col14_1,data = thatnghiep$TayNguyen) 
    } else if (input$area == "DBSH1"){
      hc %>% 
        hc_add_series(name = "Đồng bằng sông Hồng", color = input$col14_1, data = thatnghiep$DBSH)
    } else if (input$area == "Dongnambo1"){
      hc %>% 
        hc_add_series(name = "Đông Nam Bộ", color = input$col14_1, data = thatnghiep$Dongnambo)
    } else if (input$area == "DHSCL1"){
      hc %>% 
        hc_add_series(name = "Đồng bằng sông Cửu Long",color = input$col14_1, data = thatnghiep$DHSCL)
    } else {
      hc %>% hc_add_series(name = "Cả Nước", data = thatnghiep$CaNuoc) %>% 
        hc_add_series(name = "Trung du và miền núi phía Bắc", data = thatnghiep$Trungdumiennui) %>% 
        hc_add_series(name = "Bắc Trung Bộ và duyên hải miền Trung", data = thatnghiep$Bactrungbo) %>% 
        hc_add_series(name = "Tây Nguyên", data = thatnghiep$TayNguyen) %>% 
        hc_add_series(name = "Đồng bằng sông Hồng", data = thatnghiep$DBSH) %>% 
        hc_add_series(name = "Đông Nam Bộ", data = thatnghiep$Dongnambo) %>% 
        hc_add_series(name = "Đồng bằng sông Cửu Long", data = thatnghiep$DHSCL) %>% 
        hc_chart(type = "line")
    }
  })
  
  output$chart14_2 <- renderHighchart({
    Thieuvieclam <- Thieuvieclam %>% filter(Year >= min(input$year7_1) & Year <= max(input$year7_1))
    hc <- highchart() %>% 
      hc_xAxis(categories = Thieuvieclam$Year) %>% 
      hc_yAxis(title = list(text = "Percentage"),lineWidth = 0, labels = list(format = "{value}%")) %>% 
      hc_chart(type = "column") %>% 
      hc_tooltip(split= TRUE, valueSuffix = "%") %>% 
      hc_title(text = "Underemployment rate of labour force at working age",
               style = list(color = "#0A27F9", useHTML = TRUE, fontSize= '16px',fontWeight= 'bold')) %>% 
      hc_subtitle(text = "Source: GSO.com",
                  style = list(color = "#0A27F9")) %>% 
      hc_add_theme(hc_theme_ffx())
    if (input$area1 == "CaNuoc2"){
      hc %>% 
        hc_add_series(name = "Cả Nước",color = input$col14_1, data = Thieuvieclam$CaNuoc)
    } else if (input$area1 == "Trungdumiennui2"){
      hc %>% 
        hc_add_series(name = "Trung du và miền núi phía Bắc",color = input$col14_1, data = Thieuvieclam$Trungdumiennui) 
    } else if (input$area1 == "Bactrungbo2"){
      hc %>% 
        hc_add_series(name = "Bắc Trung Bộ và duyên hải miền Trung",color = input$col14_1, data = Thieuvieclam$Bactrungbo)
    } else if (input$area1 == "TayNguyen2"){
      hc %>% 
        hc_add_series(name = "Tây Nguyên", color = input$col14_1,data = Thieuvieclam$TayNguyen) 
    } else if (input$area1 == "DBSH2"){
      hc %>%
        hc_add_series(name = "Đồng bằng sông Hồng",color = input$col14_1, data =Thieuvieclam$DBSH)
    } else if (input$area1 == "Dongnambo2"){
      hc %>%
        hc_add_series(name = "Đông Nam Bộ", color = input$col14_1,data = Thieuvieclam$Dongnambo)
    } else if (input$area1 == "DHSCL2"){
      hc %>%
        hc_add_series(name = "Đồng bằng sông Cửu Long", color = input$col14_1, data = Thieuvieclam$DHSCL)
    } else {
      hc %>% hc_add_series(name = "Cả Nước", data = Thieuvieclam$CaNuoc) %>% 
        hc_add_series(name = "Trung du và miền núi phía Bắc", data = Thieuvieclam$Trungdumiennui) %>% 
        hc_add_series(name = "Bắc Trung Bộ và duyên hải miền Trung", data =Thieuvieclam$Bactrungbo) %>% 
        hc_add_series(name = "Tây Nguyên", data = Thieuvieclam$TayNguyen) %>% 
        hc_add_series(name = "Đồng bằng sông Hồng", data = Thieuvieclam$DBSH) %>% 
        hc_add_series(name = "Đông Nam Bộ", data = Thieuvieclam$Dongnambo) %>% 
        hc_add_series(name = "Đồng bằng sông Cửu Long", data = Thieuvieclam$DHSCL) %>% 
        hc_chart(type = "line")
    }
  })
  
  output$chart14_3 <- renderHighchart({
    thatnghiep_gtinh <- thatnghiep_gtinh %>%  filter(Year >= min(input$year7_2) & Year <= max(input$year7_2))
    thatnghiep_gtinh <- thatnghiep_gtinh %>% filter(Area == input$area2)
    hc <- highchart() %>% 
      hc_xAxis(categories = thatnghiep_gtinh$Year) %>% 
      hc_yAxis(title = list(text = "Percentage"),lineWidth = 0, labels = list(format = "{value}%")) %>%
      hc_add_series(name = "Male", 
                    color = input$col14_1, data = thatnghiep_gtinh$"Nam") %>% 
      hc_add_series(name = "Female", 
                    color = input$col14_2, data = thatnghiep_gtinh$"Nữ") %>% 
      hc_title(text = "Unemployment rate of labour force at working age by sex",
               style = list(color = "#0A27F9", useHTML = TRUE, 
                            fontSize= '16px',fontWeight= 'bold')) %>% 
      hc_chart(type = "column") %>% 
      hc_tooltip(split= TRUE, valueSuffix = "%") %>% 
      hc_subtitle(text = "Source: GSO.com",
                  style = list(color = "#0A27F9")) %>% 
      hc_add_theme(hc_theme_ffx())
  })
  
  
  output$chart14_4 <- renderHighchart({
    thieuvl_gtinh <- thieuvl_gtinh %>%  filter(Year >= min(input$year7_3) & Year <= max(input$year7_3))
    thieuvl_gtinh <- thieuvl_gtinh %>% filter(Area == input$area3)
    hc <- highchart() %>% 
      hc_xAxis(categories = thieuvl_gtinh$Year) %>% 
      hc_yAxis(title = list(text = "Percentage"),lineWidth = 0, labels = list(format = "{value}%")) %>%
      hc_add_series(name = "Male", 
                    color = input$col14_1, data = thieuvl_gtinh$"Nam") %>% 
      hc_add_series(name = "Female", 
                    color = input$col14_2, data = thieuvl_gtinh$"Nữ") %>% 
      hc_title(text = "Underemployment rate of labour force at working age by sex",
               style = list(color = "#0A27F9", useHTML = TRUE, fontSize= '16px',fontWeight= 'bold')) %>% 
      hc_chart(type = "column") %>% 
      hc_tooltip(split= TRUE, valueSuffix = "%") %>% 
      hc_subtitle(text = "Source: GSO.com",
                  style = list(color = "#0A27F9")) %>% 
      hc_add_theme(hc_theme_ffx())
    
  })
  
  output$select_input_table1 <- renderUI({
    selectInput('sql_table',
                label = 'Please select the Table',
                choices = index)
    })
  
  output$sql_data <- renderDT(
    if (input$sql_table == "h1"){
      datatable(lucluong_gt_khuvuc, extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               buttons =list('colvis','copy','print',
                                             list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),
                                                  text = 'Download')
                                             )
                               )
                )
    } else if (input$sql_table == "h2"){
      datatable(lucluong_tuoi,extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               buttons =list('colvis','copy','print',
                                             list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),
                                                  text = 'Download')
                                             )
                               )
                )
    }else if (input$sql_table == "h3"){
      datatable(laodong_tpkt, 
                colnames = c("Năm", "Kinh tế ngoài nhà nước",
                             "Kinh tế Nhà nước", "Nước ngoài",
                             "Cơ cấu Kinh tế ngoài nhà nước",
                             "Cơ cấu Kinh tế Nhà nước",
                             "Cơ cấu Nước ngoài"),
                extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               buttons =list('colvis','copy','print',
                                             list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),
                                                  text = 'Download')
                                             )
                               )
                )
    }else if (input$sql_table == "h4"){
      datatable(data_ldgtkv,extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               buttons =list('colvis','copy','print',
                                             list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),
                                                  text = 'Download')
                                             )
                               )
                )
    }else if (input$sql_table == "h5"){
      datatable(data_laodong_tuoi,extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               buttons =list('colvis','copy','print',
                                             list(extend = 'collection', buttons = c('csv', 'excel', 'pdf'),
                                                  text = 'Download')
                                             )
                               )
                )
    }else if (input$sql_table == "h6"){
      datatable(data_laodong_nganhkt,extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               buttons =list('colvis','copy','print',
                                             list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),
                                                  text = 'Download'
                                                  )
                                             )
                               )
                )
    }else if (input$sql_table == "h7"){
      datatable(data_laodong_nganhnghe,extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               buttons =list('colvis','copy','print',
                                             list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),
                                                  text = 'Download')
                                             )
                               )
                )
    }else if (input$sql_table == "h8"){
      datatable(data_laodong_vithevieclam,extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               buttons =list('colvis','copy','print',
                                             list(extend = 'collection', buttons = c('csv', 'excel', 'pdf'),
                                                  text = 'Download'
                        
                      )
                    ))
      )
    }else if (input$sql_table == "h9"){
      datatable(data_laodong_daotao,extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               buttons =list('colvis','copy','print',
                                             list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),
                                                  text = 'Download')
                                             )
                               )
                )
    }else if (input$sql_table == "h10"){
      datatable(laodong_daotao_tuoi,extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               buttons = list('colvis','copy','print',
                                              list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),
                                                   text = 'Download')
                                              )
                               )
                )
    }else if (input$sql_table == "h11"){
      datatable(data_laodong_trinhdo,extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               buttons =list('colvis','copy','print',
                                             list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),
                                                  text = 'Download')
                                             )
                               )
                )
    }else if (input$sql_table == "h12"){
      datatable(data_nangsuatlaodong,extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               buttons =list('colvis','copy','print',
                                             list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),
                                                  text = 'Download')
                                             )
                               )
                )
    }else if (input$sql_table == "h13"){
      datatable(data_tntv,extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               buttons =list('colvis','copy','print',
                                             list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),
                                                  text = 'Download')
                                             )
                               )
      )}
  )}

shinyApp(ui = ui, server = server)

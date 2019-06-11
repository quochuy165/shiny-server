library(readxl)
library(tidyverse)
library(dygraphs)
library(xts)
library(shiny)
library(shinyWidgets)
library(janitor)
library(DT)


#function call excel ------------------------------------------------------

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# Function convert_date ----------------------------------------------------

convert_date <- function(df) {
  x <- as.numeric(names(df)[12:ncol(df)]) - 2
  y <- as.Date(x, format = "%Y-%m-%d",origin = "1900-01-01")
  y
}

# call data ---------------------------------------------------------------

macro <- read_excel_allsheets("Vietnam data.xlsx")

#Set first name
for (i in 1:length(macro)) {
  names(macro[[i]])[1] <- c("Series")
}

#Maniplulate
for (i in c(1:36)[-c(5,7)]) {
  #Convert date
  colnames(macro[[i]])[12:ncol(macro[[i]])] <- as.character(convert_date(macro[[i]]))
  #Filter active statsu, Vietnam data, gather data
  macro[[i]] <- macro[[i]] %>%  filter(Status == "Active") %>%
    filter(Country == "Vietnam") %>%
    gather(key = "Time", value = "Value", -c(1:11)) %>%
    select(Series, Time, Value) %>%
    group_by_at(vars(-Value)) %>%
    mutate(row_id=1:n()) %>% ungroup() %>%
    spread("Series", "Value") %>%
    filter(row_id == 1) %>%
    select(-row_id)
  macro[[i]]$Time <- as.Date(macro[[i]]$Time, "%Y-%m-%d")
  #Assign dataframe in list
  assign(paste0("index_",i), macro[[i]])
}

#Daily data
daily <- macro$`Daily data`
daily_data <-  daily[11:nrow(daily),]
daily_data$Series <- as.Date(as.numeric(daily_data$Series) - 2,format = "%Y-%m-%d",origin = "1900-01-01")

#Global.R
macro_choice <- c("Core Consumer Price" = "2", "Gross Domestic Saving" = "3", "Real GDP" = "4",
                  "Trade Balance" = "5")
macro_choice_unit <- c("%" = "2", "%" = "3", "%" = "4", "USD" = "5")

pop_unit <- c("Person th" = "aa", "Person/sq km" = "den", "Person th" = "me", "Person" = "Total", "%" = "Growth")

##Bop data
bop_imf <- index_3[,-c(2:56)] %>% filter(Time < '2013-03-01')

###Financial Account choice
FA_oi_choice <- c(22:46)
names(FA_oi_choice) <- names(index_2[,FA_oi_choice])
###Productivity choice
p_choice <- c(16:29,31:36)
names(p_choice) <- names(index_4[,p_choice])
###Anually gdp choice
a_gdp1 <- c(102,106, 13:20)
names(a_gdp1) <- names(index_6[,a_gdp1])
###Anually gdp index choice
a_gdp2 <- c(3:12)
names(a_gdp2) <- names(index_6[,a_gdp2])
###quarterly gdp choice
q_gdp1 <- c(103,49:74)
names(q_gdp1) <- names(index_6[,q_gdp1])

q_gdp2 <- c(105,75:101)
names(q_gdp2) <- names(index_6[,q_gdp2])

q_gdp3 <- c(2,21:48)
names(q_gdp3) <- names(index_6[,q_gdp3])
###Inflation choices
arp <- names(index_9[,c(2:28)])
cpi <- names(index_9[,c(30,35:47, 64:66)])
cpi_m <- names(index_9[,c(31,48:63)])
cpi_y <- names(index_9[,c(32,83:98)])
cpi_w2014 <- names(index_9[,c(29,67:82)])
expi <- names(index_9[,c(99:105)])
impi <- names(index_9[,c(107:113)])
ppi <- names(index_9[,c(114:118)])
# Money_supply --------------------------------------------------

Money_supply <- read_excel("Vietnam data.xlsx", sheet = "Money supply", col_names = TRUE)
Money_supply <- Money_supply[Money_supply$Status == "Active",]
Money_supply <- Money_supply[-c(2:11)]
x <- excel_numeric_to_date(as.numeric(names(Money_supply[2:104])))
names(Money_supply)[2:104] <- paste(x)
names(Money_supply)[1] <- paste("index")
Money_supply <- Money_supply %>% gather("Date", "Value", -c("index"))
Money_supply <- spread(Money_supply, "index", "Value")

Money_supply_1 <- select(Money_supply, Date,`Money Supply M1: Annual`, `Money Supply M2: Annual`)
Money_supply_1 <- Money_supply_1 %>% drop_na()
Money_supply_1 <- xts(Money_supply_1, order.by = as.Date(Money_supply_1$Date, "%Y-%m-%d"))
Money_supply_1$Date <- NULL

Money_supply_2 <- select(Money_supply, Date, `Monetary Survey: Net Foreign Assets`,`Money Supply M1`,
                         `Money Supply M2`)

Money_supply_2 <- xts(Money_supply_2, order.by = as.Date(Money_supply_2$Date, "%Y-%m-%d"))
Money_supply_2$Date <- NULL

# GOV ---------------
Gov_debt <- read_excel("Vietnam data.xlsx", sheet = "Gov(debt)")
Gov_final <- read_excel("Vietnam data.xlsx", sheet = "Gov(final)")
Gov_plan <- read_excel("Vietnam data.xlsx", sheet = "Gov (plan)")

# Gov debt-----------
Gov_debt <- Gov_debt[Gov_debt$Status == "Active",]
Gov_debt <- Gov_debt[-c(2:11)]
y <- excel_numeric_to_date(as.numeric(names(Gov_debt[2:17])))
names(Gov_debt)[2:17] <- paste(y)
names(Gov_debt)[1] <- paste("index")
Gov_debt <- Gov_debt %>% gather("Date","Value", -c("index"))
Gov_debt <- spread(Gov_debt, "index", "Value")
Gov_debt_1 <- select(Gov_debt, "Date",
                     "Government Debt (GD): Outstanding: VND: Total",
                     "GD: Disbursemnent: VND: Total",
                     "GD: Debt Service (DS): VND: Total",
                     "GD: DS: Principle Payment: VND: Total",
                     "GD: DS: Interest Payment: VND: Total",
                     "Government Guaranteed Debt (GGD): Outstanding: VND: Total",
                     "GGD: Disbursemnent: VND: Total",
                     "GGD: Debt Service (DS): VND: Total",
                     "GGD: DS: Principle Payment: VND: Total",
                     "GGD: DS: Interest Payment: VND: Total",
                     "Public Debt: VND",
                     "Public Debt: Government Debt: VND",
                     "Public Debt: Government Guaranteed Debt: VND",
                     "Public Debt: Local Government Debt: VND")
Gov_debt_1 <- Gov_debt_1 %>% drop_na()

Gov_debt_2 <- select(Gov_debt, "Date",
                     "Public Debt: % of GDP",
                     "Government Debt: % of GDP")
Gov_debt_2 <- Gov_debt_2 %>% drop_na()

Gov_debt_1 <- xts(Gov_debt_1, order.by = as.Date(Gov_debt_1$Date, "%Y-%m-%d"))
Gov_debt_1$Date <- NULL

Gov_debt_2 <- xts(Gov_debt_2, order.by = as.Date(Gov_debt_2$Date, "%Y-%m-%d"))
Gov_debt_2$Date <- NULL

# Gov final--------
Gov_final <- Gov_final[Gov_final$Status == "Active",]
Gov_final <- Gov_final[-c(2:11)]
z <- excel_numeric_to_date(as.numeric(names(Gov_final[2:28])))
names(Gov_final)[2:28] <- paste(z)
names(Gov_final)[1] <- paste("index")
Gov_final <- subset(Gov_final,
                    index == "State Budget: Final: Revenue (RE)"| 
                      index == "State Budget: Final: Expenditure (EP)"|
                      index == "State Budget: Final: Balance")

Gov_final <- Gov_final %>% gather("Date","Value", -c("index"))
Gov_final <- spread(Gov_final, "index", "Value")
Gov_final_xts <- xts(Gov_final, order.by = as.Date(Gov_final$Date, "%Y-%m-%d"))
Gov_final_xts$Date <- NULL

# Gov_plan----------
Gov_plan <- Gov_plan[Gov_plan$Status == "Active",]
Gov_plan <- Gov_plan[-c(2:11)]
names(Gov_plan)[1] <- paste("index")
Gov_plan <- subset(Gov_plan,
                   index == "State Budget: Plan: Revenue"|
                     index == "State Budget: Plan: Expenditure"|
                     index == "State Budget: Plan: Balance: Classified by VN"|
                     index == "State Budget: Plan: Total Financing: Classified by VN")
Gov_plan <- Gov_plan[c(2:5),]
Gov_plan <- Gov_plan %>% gather("Date","Value", -c("index"))
Gov_plan <- spread(Gov_plan, "index", "Value")
Gov_plan$Date <- as.numeric(Gov_plan$Date)



# Retail-------
Retail <- read_excel("Vietnam data.xlsx", sheet = "Retail")
Retail <- Retail[Retail$Status == "Active",]
Retail <- Retail[-c(2,3,5:11)]
n <- excel_numeric_to_date(as.numeric(names(Retail[3:215])))
names(Retail)[3:215] <- paste(n)
names(Retail)[1] <- paste("index")
Retail_pe <- Retail[c(1:5),c(1,3:215)]
Retail_vnd <- Retail[c(6:11),c(1,3:215)]

Retail_pe <- Retail_pe %>% gather("Date","Value", -c("index"))
Retail_pe <- spread(Retail_pe, "index", "Value")
Retail_pe <- Retail_pe %>% drop_na(`Retail Sales: YoY: ytd: Accomodation, Food & Beverage Service`,`Retail Sales: YoY: ytd: Tourism`)

Retail_vnd <- Retail_vnd %>% gather("Date", "Value", -c("index"))
Retail_vnd <- spread(Retail_vnd, "index", "Value")

Retail_pe_xts <- xts(Retail_pe, order.by = as.Date(Retail_pe$Date, "%Y-%m-%d"))
Retail_pe_xts$Date <- NULL

Retail_vnd_xts <- xts(Retail_vnd, order.by = as.Date(Retail_vnd$Date, "%Y-%m-%d"))
Retail_vnd_xts$Date <- NULL

# FDI ----------------
FDI_monthly <- read_excel("Vietnam data.xlsx", sheet = "FDI (monthly)")
FDI_quarterly <- read_excel("Vietnam data.xlsx", sheet = "FDI (quarterly)")
FDI_country <- read_excel("Vietnam data.xlsx", sheet = "FDI (by country)")

FDI_monthly <- FDI_monthly[FDI_monthly$Status == "Active" & FDI_monthly$Unit == "USD mn",]
FDI_monthly <- FDI_monthly[-c(2:11)]
fdi_1 <- excel_numeric_to_date(as.numeric(names(FDI_monthly[2:204])))
names(FDI_monthly)[2:204] <- paste(fdi_1)
names(FDI_monthly)[1] <- paste("index")
FDI_monthly <- FDI_monthly %>% gather("Date","Value", -c("index"))
FDI_monthly <- FDI_monthly %>% spread("index","Value")
FDI_monthly_xts <- xts(FDI_monthly, order.by = as.Date(FDI_monthly$Date, "%Y-%m-%d"))
FDI_monthly_xts$Date <- NULL

FDI_country <- FDI_country[FDI_country$Status == "Active",]
FDI_country <- FDI_country[-c(2:11)]
fdi_2 <- excel_numeric_to_date(as.numeric(names(FDI_country[2:201])))
names(FDI_country)[2:201] <- paste(fdi_2)
names(FDI_country)[1] <- paste("index")
FDI_country <- FDI_country %>% gather("Date","Value", -c("index"))
FDI_country <- FDI_country %>% spread("index","Value")
FDI_country_xts <- xts(FDI_country, order.by = as.Date(FDI_country$Date, "%Y-%m-%d"))
FDI_country_xts$Date <- NULL

FDI_quarterly <- FDI_quarterly[FDI_quarterly$Status == "Active",]
FDI_quarterly <- FDI_quarterly[-c(2:11)]
fdi_3 <- excel_numeric_to_date(as.numeric(names(FDI_quarterly[2:73])))
names(FDI_quarterly)[2:73] <- paste(fdi_3)
names(FDI_quarterly)[1] <- paste("index")
FDI_quarterly <- FDI_quarterly %>% gather("Date","Value", -c("index"))
FDI_quarterly <- FDI_quarterly %>% spread("index","Value")
FDI_quarterly_xts <- xts(FDI_quarterly, order.by = as.Date(FDI_quarterly$Date, "%Y-%m-%d"))
FDI_quarterly_xts$Date <- NULL

# Investment --------------
Investment_2010p <- read_excel("Vietnam data.xlsx", sheet = "Investment (2010p)")
Investment_current <- read_excel("Vietnam data.xlsx", sheet = "Investment (current)")

Investment_2010p <- Investment_2010p[-c(2:11)]
inv1 <- excel_numeric_to_date(as.numeric(names(Investment_2010p[2:14])))
names(Investment_2010p)[2:14] <- paste(inv1)
names(Investment_2010p)[1] <- paste("index")
Investment_2010p <- Investment_2010p %>% gather("Date", "Value", -c("index"))
Investment_2010p <- Investment_2010p %>% spread("index", "Value")
Investment_2010p_xts <- xts(Investment_2010p, order.by = as.Date(Investment_2010p$Date, "%Y-%m-%d"))
Investment_2010p_xts$Date <- NULL

Investment_current <- Investment_current[-c(2:11)]
inv2 <- excel_numeric_to_date(as.numeric(names(Investment_current[2:43])))
names(Investment_current)[2:43] <- paste(inv2)
names(Investment_current)[1] <- paste("index")
Investment_current <- Investment_current %>% gather("Date", "Value", -c("index"))
Investment_current <- Investment_current %>% spread("index", "Value")
Investment_current_xts <- xts(Investment_current, order.by = as.Date(Investment_current$Date, "%Y-%m-%d"))
Investment_current_xts$Date <- NULL

# GDP -----------------------
GDP <- read_excel("Vietnam data.xlsx", sheet = "GDP")
Shape_of_GDP <- read_excel("Vietnam data.xlsx", sheet = "Share of GDP")

GDP_vnd <- GDP[GDP$Unit == "VND bn",]
GDP_2010 <- GDP_vnd[c(1,2,6,13,28),]
GDP_2010 <- GDP_2010[-c(2:109)]
gdp_2010 <- excel_numeric_to_date(as.numeric(names(GDP_2010[2:26])))
names(GDP_2010)[2:26] <- paste(gdp_2010)
names(GDP_2010)[1] <- paste("index")
GDP_2010 <- GDP_2010 %>% gather("Date", "Value", -c("index"))
GDP_2010 <- spread(GDP_2010, "index", "Value")
GDP_2010_xts <- xts(GDP_2010, order.by = as.Date(GDP_2010$Date, "%Y-%m-%d"))
GDP_2010_xts$Date <- NULL

GDP_current <- GDP_vnd[c(29,30,34,41,56),]
GDP_current <- GDP_current[-c(2:53)]
gdp_cu <- excel_numeric_to_date(as.numeric(names(GDP_current[2:82])))
names(GDP_current)[2:82] <- paste(gdp_cu)
names(GDP_current)[1] <- paste("index")
GDP_current <- GDP_current %>% gather("Date", "Value", -c("index"))
GDP_current <- spread(GDP_current, "index", "Value")
GDP_current_xts <- xts(GDP_current, order.by = as.Date(GDP_current$Date, "%Y-%m-%d"))
GDP_current_xts$Date <- NULL

# Exchange rate------------------
ER_monthly <- read_excel("Vietnam data.xlsx", sheet = "Exchange rate(monthly)")
ER_monthly <- ER_monthly[-c(2:192)]

ER_buy <- ER_monthly[c(3:12),]
er1 <- excel_numeric_to_date(as.numeric(names(ER_buy[2:244])))
names(ER_buy)[2:244] <- paste(er1)
names(ER_buy)[1] <- paste("index")
ER_buy <- ER_buy %>% gather("Date","Value", -c("index"))
ER_buy <- spread(ER_buy, "index", "Value")
ER_buy_xts <- xts(ER_buy, order.by = as.Date(ER_buy$Date, "%Y-%m-%d"))
ER_buy_xts$Date <- NULL

ER_transfer <- ER_monthly[c(13:22),]
er2 <- excel_numeric_to_date(as.numeric(names(ER_transfer[2:244])))
names(ER_transfer)[2:244] <- paste(er2)
names(ER_transfer)[1] <- paste("index")
ER_transfer <- ER_transfer %>% gather("Date","Value", -c("index"))
ER_transfer <- spread(ER_transfer, "index", "Value")
ER_transfer_xts <- xts(ER_transfer, order.by = as.Date(ER_transfer$Date, "%Y-%m-%d"))
ER_transfer_xts$Date <- NULL

ER_sell <- ER_monthly[c(23:32),]
er3 <- excel_numeric_to_date(as.numeric(names(ER_sell[2:244])))
names(ER_sell)[2:244] <- paste(er3)
names(ER_sell)[1] <- paste("index")
ER_sell <- ER_sell %>% gather("Date","Value", -c("index"))
ER_sell <- spread(ER_sell, "index", "Value")
ER_sell_xts <- xts(ER_sell, order.by = as.Date(ER_sell$Date, "%Y-%m-%d"))
ER_sell_xts$Date <- NULL


unit <- c("VND/AUD" = "1", "VND/GBP" = "2", "VND/CAD"= "3", "VND/EUR" = "4", "VND/HKD" = "5", 
          "VND/JPY" = "6", "VND/SGD" = "7", "VND/CHF" = "8", "VND/THB" = "9", "VND/USD" = "10")

#ui

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .dygraph-title {
          color: navy;
          font-weight: bold;
        }
      .dygraph-axis-label {
          font-size: 11px;
      }
        @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                    
                    h1 {
                    font-family: 'Lobster', cursive;
                    font-weight: 500;
                    line-height: 1.1;
                    color: #ad1d28;
                    } 
                    body {
                    background-color: #fff;
                    }

    "))
  ),
  headerPanel("Viet Nam Data Series"),
  sidebarLayout(
    sidebarPanel(
      pickerInput("index", label = NULL, 
                  choices = c("General", "Balance of Payments" = "bop",
                              "Demographic" = "demo",
                              "Gross Domestic Products" = "gdp", "Gold",
                              "Inflation" = "inf"
                    ,"Monetary Survey" = "m1", "Government Debt" = "m2",
                              "State Budget" = "m3", "Retail Sales" = "m4",  "Foreign Direct Investment" = "m5",
                              "Investment" = "m6", "Gross Domestic Product" = "m7",
                              "Exchange rate" = "m8"),
                  options = list(
                    title = "Choose value, e.g., GDP")
      ),
      conditionalPanel(
        condition = "input.index == 'inf'",
        selectInput("inf", "Choose indicators", choices = c("Average Retail Price" = "arp",
                                                            "Consumer Price Index: MOM" = "cpi_m",
                                                            "CPI: Weight: 2014 = 100" = "cpi_w2014",
                                                            "Consumer Price Index" = "cpi",
                                                            "Consumer Price Index: YOY" = "cpi_y",
                                                            "ExPI: YOY", "ImPI: YOY", "PPI: YOY",
                                                            "Core Inflation" = "core")),
        conditionalPanel(
          condition = "input.inf == 'arp'",
          selectizeInput("arp", NULL, choices = arp, multiple = TRUE,
                         options = list(placeholder = 'pick series'))
        ),
        conditionalPanel(
          condition = "input.inf == 'cpi'",
          selectizeInput("cpi", NULL, choices = cpi, multiple = TRUE,
                         options = list(placeholder = 'pickseries'))
        ),
        conditionalPanel(
          condition = "input.inf == 'cpi_m'",
          selectizeInput("cpi_m", NULL, choices = cpi_m, multiple = TRUE,
                         options = list(placeholder = 'pickseries'))
        ),
        conditionalPanel(
          condition = "input.inf == 'cpi_w2014'",
          selectizeInput("cpi_w2014", NULL, choices = cpi_w2014, multiple = TRUE,
                         options = list(placeholder = 'pickseries'))
        ),
        conditionalPanel(
          condition = "input.inf == 'cpi_y'",
          selectizeInput("cpi_y", NULL, choices = cpi_y, multiple = TRUE,
                         options = list(placeholder = 'pickseries'))
        ),
        conditionalPanel(
          condition = "input.inf == 'ExPI: YOY'",
          selectizeInput("expi", NULL, choices = expi, multiple = TRUE,
                         options = list(placeholder = 'pickseries'))
        ),
        conditionalPanel(
          condition = "input.inf == 'ImPI: YOY'",
          selectizeInput("impi", NULL, choices = impi, multiple = TRUE,
                         options = list(placeholder = 'pickseries'))
        ),
        conditionalPanel(
          condition = "input.inf == 'PPI: YOY'",
          selectizeInput("ppi", NULL, choices = ppi, multiple = TRUE,
                         options = list(placeholder = 'pickseries'))
        ),
        conditionalPanel(
          condition = "input.inf == 'core'",
          selectInput("core", NULL, choices = c("Core Inflation: MoM",
                                                "Core Inflation: YoY"))
        )
      ),
      conditionalPanel(
        condition = "input.index == 'Gold'",
        selectizeInput("gold", label = NULL, choices = c("Buy" = 2, "Sell" = 3), multiple = TRUE,
                       options = list(placeholder = 'pick series'))
      ),
      conditionalPanel(
        condition = "input.index == 'General'",
        selectInput("macro", "Select plot", choices = macro_choice)
      ),
      conditionalPanel(
        condition = "input.index == 'bop'",
        selectInput("bop", "Select BOP index", choices = c("Current Account" = "CA",
                                                      "Capital Account" = "cA",
                                                      "Financial Account" = "FA",
                                                      "Net error & Omissions" = "NE",
                                                      "Overall Balance" = "OB",
                                                      "Reserve/Related Item" = "R")),
        conditionalPanel(
          condition = "input.bop == 'CA'",
          selectInput("bop.ca", "Select CA index", choices = c("Current Account" = "CA",
                                                               "Goods", "Services", "Primary Income: Investment" = "pi",
                                                               "Secondary Income: Transfer" = "si"))
        ),
        conditionalPanel(
          condition = "input.bop == 'FA'",
          selectInput("bop_fa", "Select FA index", choices = c("Financial Account" = "FA",
                                                               "Direct Investment" = "di",
                                                               "Portfolio Investment" = "pi",
                                                               "Other investment" = "oi")),
          conditionalPanel(
            condition = "input.bop_fa == 'oi'",
            selectizeInput("fa_oi", label = NULL, choices = FA_oi_choice, multiple = TRUE,
                           options = list(placeholder = 'pick series'))
          )
        )
      ),
      conditionalPanel(
        condition = "input.index == 'demo'",
        selectInput("dg", "Select demographic series", c("Population" = "pop",
                                                         "Employment" = "epm",
                                                         "Labour Force Participation Rate" = "Labour",
                                                         "Unemployment Rate" = "unem",
                                                         "Average Monthly Earning" = "earn",
                                                         "Productivity" = "ns"
                                                         )),
        conditionalPanel(
          condition = "input.dg == 'pop'",
          selectInput("p", "Select population Series", c("Annual Average" = "aa",
                                                         "Density" = "den",
                                                         "MidYear Estimates" = "me",
                                                         "Total", "Growth"))
        ),
        conditionalPanel(
          condition = "input.dg == 'unem'",
          radioButtons(inputId = "choice_unem", NULL, choices = c("Annually","Quarterly"), inline = T),
          conditionalPanel(
            condition = "input.choice_unem == 'Annually'",
            checkboxInput("est", "Include ILO Estimate: % of Total Labour Force")
          )
        ),
        conditionalPanel(
          condition = "input.dg == 'ns'",
          checkboxInput("overall", "Overall Productivirty"),
          conditionalPanel(condition = "!(input.overall)",
          selectizeInput("ns_choice", label = NULL, choices = p_choice, multiple = TRUE,
                         options = list(placeholder = 'pick series'))
          )
        )
      ),
      conditionalPanel(
        condition = "input.index == 'gdp'",
        prettyToggle(
          inputId = "gdp",
          label_on = "Quarterly",
          label_off = "Annually",
          icon_on = icon("money-bill-alt "),
          icon_off = icon("money-bill-alt ")),
        conditionalPanel(
          condition = "input.gdp",
          selectInput("price", "Select GDP indicators", choices = c("Real Price (2010p)" = "rp",
                                                                    "Current Price" = "cp",
                                                                    "GDP deflator" = 'gd')),
          conditionalPanel(
            condition = "input.price == 'rp'",
            selectizeInput("q1_index", label = NULL, choices = q_gdp1, multiple = TRUE,
                           options = list(placeholder = 'pick series'))
          ),
          conditionalPanel(
            condition = "input.price == 'cp'",
            selectizeInput("q2_index", label = NULL, choices = q_gdp2, multiple = TRUE,
                           options = list(placeholder = 'pick series'))
          ),
          conditionalPanel(
            condition = "input.price == 'gd'",
            selectizeInput("q3_index", label = NULL, choices = q_gdp3, multiple = TRUE,
                           options = list(placeholder = 'pick series'))
          )
        ),
        conditionalPanel(
          condition = "!input.gdp",
          selectInput("y_gdp", "Select GDP indicators", choices = c("GDP", "Index")),
          conditionalPanel(
            condition = "input.y_gdp == 'GDP'",
            selectInput("a_rgdp", label = NULL, choices = a_gdp1)
          ),
          conditionalPanel(
            condition = "input.y_gdp == 'Index'",
            selectizeInput("a_index", label = NULL, choices = a_gdp2, multiple = TRUE,
                           options = list(placeholder = 'pick series'))
          )
        )
      ),
      # Money supply ----------------
      conditionalPanel(condition =  "input.index == 'm1'",
                       pickerInput("m1_cho", label = NULL,
                                   choices = c("Money Supply: Quarter",
                                               "Money Supply: Annual"),
                                   options = list(
                                     title = "Choose value")),
                       conditionalPanel(condition = "input.m1_cho == 'Money Supply: Quarter'",
                                        awesomeCheckboxGroup("chom1", label = NULL,
                                                             choices = c("Net Foreign Assets" = "1",
                                                                         "M1" = "2",
                                                                         "M2" = "3"),
                                                             status = "danger")),
                       conditionalPanel(condition = "input.m1_cho == 'Money Supply: Annual'",
                                        awesomeCheckboxGroup("chom1_1", label = NULL,
                                                             choices = c("M1" = "1",
                                                                         "M2" = "2"),
                                                             status = "danger"))
      ),
      # GD ---------------------------
      conditionalPanel(condition = "input.index == 'm2'",
                       awesomeRadio("m2_cho", label = NULL,
                                    choices = c("proportion","total"),
                                    inline = TRUE),
                       conditionalPanel(condition = "input.m2_cho == 'total'",
                                        awesomeCheckboxGroup("chom2", label = NULL,
                                                             choices = c("Government Debt (GD): Outstanding: VND: Total" = "1",
                                                                         "GD: Disbursemnent: VND: Total" = "2",
                                                                         "GD: Debt Service (DS): VND: Total" = "3",
                                                                         "GD: DS: Principle Payment: VND: Total" = "4",
                                                                         "GD: DS: Interest Payment: VND: Total" = "5",
                                                                         "Government Guaranteed Debt (GGD): Outstanding: VND: Total" = "6",
                                                                         "GGD: Disbursemnent: VND: Total" = "7",
                                                                         "GGD: Debt Service (DS): VND: Total" = "8",
                                                                         "GGD: DS: Principle Payment: VND: Total" = "9",
                                                                         "GGD: DS: Interest Payment: VND: Total" = "10",
                                                                         "Public Debt: VND" = "11",
                                                                         "Public Debt: Government Debt: VND" = "12",
                                                                         "Public Debt: Government Guaranteed Debt: VND" = "13",
                                                                         "Public Debt: Local Government Debt: VND" = "14"),
                                                             status = "danger",
                                                             inline = TRUE)),
                       conditionalPanel(condition = "input.m2_cho == 'proportion'",
                                        awesomeCheckboxGroup("chom2_1", label = NULL,
                                                             choices = c("Public Debt: % of GDP" = "1",
                                                                         "Government Debt: % of GDP" = "2"),
                                                             status = "danger")
                       )
      ),
      # State budget -----------------
      conditionalPanel(condition = "input.index == 'm3'",
                       pickerInput("m3_cho", label = NULL,
                                   choices = c("State Budget: Final (Gov_final)" = "m31",
                                               "State Budget: Plan (Gov_plan)" = "m32"),
                                   options = list(
                                     title = "Choose value")),
                       conditionalPanel(condition = "input.m3_cho == 'm31'",
                                        awesomeCheckboxGroup("chom3", label = NULL,
                                                             choices = c("Revenue (RE)" = "3",
                                                                         "Expenditure (EP)" = "2",
                                                                         "Balance" = "1"),
                                                             status = "danger")
                       ),
                       conditionalPanel(condition = "input.m3_cho == 'm32'",
                                        awesomeCheckboxGroup("chom3_1", label = NULL,
                                                             choices = c("Revenue (RE)" = "4",
                                                                         "Expenditure (EP)" = "3",
                                                                         "Balance: Classified by VN" = "2",
                                                                         "Total Financing: Classified by VN" = "5"),
                                                             status = "danger",
                                                             inline = FALSE)
                       )
      ),
      # Retail -------------------------------
      conditionalPanel(condition = "input.index == 'm4'",
                       awesomeRadio("m4_cho", label = NULL,
                                    choices = c("proportion", "total"),
                                    inline = TRUE),
                       conditionalPanel(condition = "input.m4_cho == 'proportion'",
                                        awesomeCheckboxGroup("chom41", label = NULL,
                                                             choices = c("Retail Sales: Total" = "1",
                                                                         "Accomodation, Food & Beverage Service" = "2",
                                                                         "Goods" = "3",
                                                                         "Tourism" = "5",
                                                                         "Other Service" = "4"),
                                                             status = "danger",
                                                             inline = FALSE)),
                       conditionalPanel(condition = "input.m4_cho == 'total'",
                                        awesomeCheckboxGroup("chom42", label = NULL,
                                                             choices = c("Retail Sales: Total" = "6",
                                                                         "Goods" = "2",
                                                                         "Accomodation, Food & Beverage Service" = "1",
                                                                         "Services and Tourism" = "3",
                                                                         "Services and Tourism: Other Services" = "4",
                                                                         "Services and Tourism: Tourism" = "5"),
                                                             status = "danger",
                                                             inline = FALSE))
      ),
      # fdi --------------------------------
      conditionalPanel(condition = "input.index == 'm5'",
                       pickerInput("m5_cho", label = NULL,
                                   choices = c("FDI_monthly", "FDI_quarterly", "FDI_country"),
                                   options = list(
                                     title = "Choose value")),
                       conditionalPanel(condition = "input.m5_cho == 'FDI_monthly'",
                                        awesomeCheckboxGroup("chom51", label = NULL,
                                                             choices = c("FDI: Implementation Capital: ytd" = "1",
                                                                         "Accommodation, Food Service" = "2",
                                                                         "Administration, Support Service" = "3",
                                                                         "Agriculture, Forestry & Fishery" = "4",
                                                                         "Arts, Entertainment, Recreation" = "5",
                                                                         "Construction" = "6",
                                                                         "Education, Training" = "7",
                                                                         "Electricity, Gas, Air Con Supply" = "8",
                                                                         "Financial, Banking, Insurance" = "9",
                                                                         "Human Health, Social Work" = "10",
                                                                         "Information, Communication" = "11",
                                                                         "Manufacturing" = "12",
                                                                         "Mining & Quarrying" = "13",
                                                                         "Other Service Activities" = "14",
                                                                         "Professional, Scientific, Tech" = "15",
                                                                         "Real Estate Activities" = "16",
                                                                         "Transportation, Storage" = "17",
                                                                         "Water Supply, Sewerage, Remediation" = "18",
                                                                         "Whlse, Retail Trade, MV Repair" = "19"),
                                                             status = "danger",
                                                             inline = FALSE)),
                       conditionalPanel(condition = "input.m5_cho == 'FDI_quarterly'",
                                        awesomeCheckboxGroup("chom52", label = NULL,
                                                             choices = c("FDI: Implementation Capital" ="1",                                 
                                                                         "FDI: Registered Capital" = "2",                                      
                                                                         "Accommodation, Food Service" = "3",        
                                                                         "Administration, Support Service" = "4",     
                                                                         "Agriculture, Forestry & Fishery" = "5",    
                                                                         "Arts, Entertainment, Recreation" = "6",    
                                                                         "Construction" = "7",                        
                                                                         "Education, Training" = "8",                
                                                                         "Electricity, Gas, Air Con Supply" = "9",    
                                                                         "Financial, Banking, Insurance" = "10",      
                                                                         "Human Health, Social Work" = "11",         
                                                                         "Information, Communication" = "12",          
                                                                         "Manufacturing" = "13",                     
                                                                         "Mining & Quarrying" = "14",                   
                                                                         "Other Service Activities" = "15",            
                                                                         "Professional, Scientific, Tech" = "16",   
                                                                         "Real Estate Activities" = "17",             
                                                                         "Transportation, Storage" = "18",              
                                                                         "Water Supply, Sewerage, Remediation" = "19", 
                                                                         "Wholesale, Retail Trade, Motor Repair" = "20"),
                                                             status = "danger",
                                                             inline = FALSE)),
                       conditionalPanel(condition = "input.m5_cho == 'FDI_country'",
                                        awesomeCheckboxGroup("chom53", label = NULL,
                                                             choices = c("Australia" = "1", "Austria" = "2", "Belgium" = "3",
                                                                         "British Virgin Islands" = "4", "British West Indies" = "5", "Brunei" = "6",
                                                                         "Bulgaria" = "7", "Cambodia" = "8",
                                                                         "Canada" = "9","China" = "10",              
                                                                         "Czech Rep" = "11","Denmark" = "12" ,             
                                                                         "El Salvador" = "13","France" = "14",             
                                                                         "Germany" = "15","Guatemala" = "16",         
                                                                         "Hong Kong SAR" = "17","India" = "18" ,               
                                                                         "Iraq" = "19", "Italy"  = "20",               
                                                                         "Japan" = "21", "Luxembourg" ="22",           
                                                                         "Macau SAR" = "23", "Malaysia"  = "24",            
                                                                         "Mauritius" = "25", "Netherlands" = "26",         
                                                                         "New Zealand" = "27", "Norway" = "28",            
                                                                         "Philippines" = "29" ,"Russia" = "30",              
                                                                         "Samoa" = "31", "Singapore" = "32",         
                                                                         "South Korea" = "33",  "Spain"  = "34",               
                                                                         "Sri Lanka" = "35",  "Sweden" = "36",           
                                                                         "Switzerland" = "37","Taiwan" = "38",             
                                                                         "Thailand" = "39", "Turkey" = "40",           
                                                                         "United Kingdom" = "41","United States" = "42"),
                                                             status = "danger"))
      ),
      # investment---------------------------
      conditionalPanel(condition = "input.index == 'm6'",
                       radioButtons("m6_cho", label = NULL,
                                    choices = c("2010", "current"),
                                    inline = TRUE),
                       conditionalPanel(condition = "input.m6_cho == '2010'",
                                        awesomeCheckboxGroup("chom61", label = NULL,
                                                             choices = c("Investment: Total" = "1",                                               
                                                                         "Accommodation, Food Service" = "2",                   
                                                                         "Administration, Support Service" = "3",               
                                                                         "Agriculture, Forestry & Fishery" = "4",               
                                                                         "Arts, Entertainment, Recreation" = "5",               
                                                                         "Community Party, Public Administration" = "6",        
                                                                         "Construction" = "7",                                  
                                                                         "Education, Training" = "8",                           
                                                                         "Electricity, Gas, Air Con Supply" = "9",             
                                                                         "Finance, Banking, Insurance" = "10",                   
                                                                         "Foreign Invested Sector" = "11",                       
                                                                         "Human Health, Social Work" = "12",                     
                                                                         "Information, Communication" = "13",                    
                                                                         "Manufacturing" = "14",                                 
                                                                         "Mining & Quarrying" = "15",                            
                                                                         "Non State" = "16",                                     
                                                                         "Other Activities" = "17",                              
                                                                         "Professional, Scientific, Tech" = "18",                
                                                                         "Real Estate Activities" = "19",                        
                                                                         "State" = "20",                                         
                                                                         "Transportation, Storage" = "21",                       
                                                                         "Water Supply, Sewerage, Remediation" = "22",           
                                                                         "Wholesale, Retail Trade, Motor Vehicles Repair" = "23"),
                                                             status = "danger",
                                                             inline = FALSE)),
                       conditionalPanel(condition = "input.m6_cho == 'current'",
                                        awesomeCheckboxGroup("chom62", label = NULL,
                                                             choices = c("Investment: Total" = "1",                                               
                                                                         "Accommodation, Food Service" = "2",                   
                                                                         "Administration, Support Service" = "3",               
                                                                         "Agriculture, Forestry & Fishery" = "4",               
                                                                         "Arts, Entertainment, Recreation" = "5",               
                                                                         "Community Party, Public Administration" = "6",        
                                                                         "Construction" = "7",                                  
                                                                         "Education, Training" = "8",                           
                                                                         "Electricity, Gas, Air Con Supply" = "9",             
                                                                         "Finance, Banking, Insurance" = "10",                   
                                                                         "Foreign Invested Sector" = "11",                       
                                                                         "Human Health, Social Work" = "12",                     
                                                                         "Information, Communication" = "13",                    
                                                                         "Manufacturing" = "14",                                 
                                                                         "Mining & Quarrying" = "15",                            
                                                                         "Non State" = "16",                                     
                                                                         "Other Activities" = "17",                              
                                                                         "Professional, Scientific, Tech" = "18",                
                                                                         "Real Estate Activities" = "19",                        
                                                                         "State" = "20",                                         
                                                                         "Transportation, Storage" = "21",                       
                                                                         "Water Supply, Sewerage, Remediation" = "22",           
                                                                         "Wholesale, Retail Trade, Motor Vehicles Repair" = "23"),
                                                             status = "danger",
                                                             inline = FALSE))
      ),
      # gdp ------------------------
      conditionalPanel(condition = "input.index == 'm7'",
                       radioButtons("m7_cho", label = NULL,
                                    choices = c("2010", "current"),
                                    inline = TRUE),
                       conditionalPanel(condition = "input.m7_cho == '2010'",
                                        awesomeCheckboxGroup("chom71", label = NULL,
                                                             choices = c("Agriculture, Forestry and Fishery" = "1",
                                                                         "Industry and Construction" = "2",
                                                                         "Product Tax Excluding Product Subsidy" = "3",
                                                                         "Gross Domestic Product: total" = "4",
                                                                         "Service" = "5"),
                                                             status = "danger",
                                                             inline = FALSE)),
                       conditionalPanel(condition = "input.m7_cho == 'current'",
                                        awesomeCheckboxGroup("chom72", label = NULL,
                                                             choices = c("Agriculture, Forestry and Fishery" = "1",
                                                                         "Industry and Construction" = "2",
                                                                         "Product Tax Excluding Product Subsidy" = "3",
                                                                         "Service" = "4",
                                                                         "Gross Domestic Product: Current Price" = "5"),
                                                             status = "danger",
                                                             inline = FALSE))
      ),
      # Exchange rate---------------------
      conditionalPanel(condition = "input.index == 'm8'",
                       radioButtons("m8_cho", label = NULL,
                                    choices = c("Buy", "Transfer", "Sell"),
                                    inline = TRUE),
                       conditionalPanel(condition = "input.m8_cho == 'Buy'",
                                        pickerInput("chom81", label = NULL,
                                                    choices = c("Australian Dollar" = "1",
                                                                "British Pound" = "2",
                                                                "Canadian Dollar" = "3",
                                                                "Euro" = "4", "Hong Kong Dollar" = "5",
                                                                "Japanese Yen" = "6", "Singapore Dollar" = "7",
                                                                "Swiss Franc" = "8", "Thailand Baht" = "9",
                                                                "US Dollar" = "10"),
                                                    options =list(title = "Choose value"))),
                       conditionalPanel(condition = "input.m8_cho == 'Transfer'",
                                        pickerInput("chom82", label = NULL,
                                                    choices = c("Australian Dollar" = "1",
                                                                "British Pound" = "2",
                                                                "Canadian Dollar" = "3",
                                                                "Euro" = "4", "Hong Kong Dollar" = "5",
                                                                "Japanese Yen" = "6", "Singapore Dollar" = "7",
                                                                "Swiss Franc" = "8", "Thailand Baht" = "9",
                                                                "US Dollar" = "10"),
                                                    options =list(title = "Choose value"))),
                       conditionalPanel(condition = "input.m8_cho == 'Sell'",
                                        pickerInput("chom83", label = NULL,
                                                    choices = c("Australian Dollar" = "1",
                                                                "British Pound" = "2",
                                                                "Canadian Dollar" = "3",
                                                                "Euro" = "4", "Hong Kong Dollar" = "5",
                                                                "Japanese Yen" = "6", "Singapore Dollar" = "7",
                                                                "Swiss Franc" = "8", "Thailand Baht" = "9",
                                                                "US Dollar" = "10"),
                                                    options =list(title = "Choose value")))
      ),
      prettyToggle(
        inputId = "grid",
        label_on = "Grid",
        label_off = "No grid",
        icon_on = icon("th"),
        icon_off = icon("th"))
    ),
    mainPanel(
      dygraphOutput("dg"), br(),br(),
      conditionalPanel(condition = "input.index == 'm1'",
                       dataTableOutput("table1")),
      conditionalPanel(condition = "input.index =='m2'",
                       dataTableOutput("table2")),
      conditionalPanel(condition = "input.index == 'm3'",
                       dataTableOutput("table3")),
      conditionalPanel(condition = "input.index == 'm4'",
                       dataTableOutput("table4")),
      conditionalPanel(condition = "input.index == 'm5'",
                       dataTableOutput("table5")),
      conditionalPanel(condition = "input.index == 'm6'",
                       dataTableOutput("table6")),
      conditionalPanel(condition = "input.index == 'm7'",
                       dataTableOutput("table7")),
      conditionalPanel(condition = "input.index == 'm8'",
                       dataTableOutput("table8"))
    )
  )
)

server <- function(input, output) {
  output$dg <- renderDygraph({
    if (input$index == "General") {
    common <- index_1[,as.numeric(c(1,input$macro))] %>% na.omit()
    graph_data <- xts(x = unlist(common[,2],use.names = F), order.by = common$Time)
    dygraph(graph_data, main = paste0(names(macro_choice[macro_choice == input$macro]), " in Vietnam")) %>%
       dyAxis("y", label = names(macro_choice_unit[macro_choice_unit == input$macro])) %>%
      dySeries("V1", label = names(macro_choice[macro_choice == input$macro])) %>% dyRangeSelector() %>% dyOptions(includeZero = TRUE, drawGrid = input$grid,
                                                                                                                   axisLineColor = "navy", strokeWidth = 2
      ) %>% dyLegend(show = "follow", width = 350)
    }
    else if (input$index == "bop") {
     if (input$bop == "CA") {
      if (input$bop.ca == "CA") {
        data <- xts(x = index_2$`Balance of Payments (BoP): Current Account (CA)`, order.by = index_2$Time)
        dygraph(data, main = "Balance of Payments: Current Account") %>%
          dyAxis("y", label = "USD mn") %>% dySeries("V1", "CA") %>% dyRangeSelector() %>% dyOptions(includeZero = TRUE, drawGrid = input$grid,
                                                                                                  axisLineColor = "navy",strokeWidth = 2
          ) %>% dyLegend(show = "follow", width = 350)
      }
      else if (input$bop.ca == "Goods") {
        data <- xts(x = index_2[,c(3:5)], order.by = index_2$Time)
        dygraph(data, main = "Current Account in Goods") %>%
          dyAxis("y", label = "USD") %>% dyRangeSelector() %>% dyOptions(includeZero = TRUE, drawGrid = input$grid,
                                                                                                  axisLineColor = "navy",
                                                                         colors = RColorBrewer::brewer.pal(3, "Set2"),strokeWidth = 2
          ) %>% dyLegend(show = "follow", width = 350)
      }
      else if (input$bop.ca == "Services") {
        data <- xts(x = index_2[,c(12:14)], order.by = index_2$Time)
        dygraph(data, main = "Current Account in Services") %>%
          dyAxis("y", label = "USD mn") %>% dyRangeSelector() %>% dyOptions(includeZero = TRUE, drawGrid = input$grid,
                                                                         axisLineColor = "navy",
                                                                         colors = RColorBrewer::brewer.pal(3, "Set2"),strokeWidth = 2
          ) %>% dyLegend(show = "follow", width = 350)
      }
      else if (input$bop.ca == "pi") {
        data <- xts(x = index_2[,c(9:11)], order.by = index_2$Time)
        dygraph(data, main = "Current Account in Primary Income") %>%
          dyAxis("y", label = "USD mn") %>% dyRangeSelector() %>% dyOptions(includeZero = TRUE, drawGrid = input$grid,
                                                                         axisLineColor = "navy",strokeWidth = 2,
                                                                         colors = RColorBrewer::brewer.pal(3, "Set2")
          ) %>% dyLegend(show = "follow", width = 350)
      }
      else if (input$bop.ca == "si"){
        data <- xts(x = index_2[,c(6:8)], order.by = index_2$Time)
        dygraph(data, main = "Current Account in Secondary Income") %>%
          dyAxis("y", label = "USD mn") %>% dyRangeSelector() %>% dyOptions(includeZero = TRUE, drawGrid = input$grid,
                                                                         axisLineColor = "navy",strokeWidth = 2,
                                                                         colors = RColorBrewer::brewer.pal(3, "Set2")
          ) %>% dyLegend(show = "follow", width = 350)
      }
     }
    else if (input$bop == "cA") {
      data <- xts(x = index_2[,c(15:17)], order.by = index_2$Time)
      dygraph(data, main = "Capital Account") %>%
        dyAxis("y", label = "USD mn") %>% dyRangeSelector() %>% dyOptions(includeZero = TRUE, drawGrid = input$grid,
                                                                       axisLineColor = "navy",strokeWidth = 2,
                                                                       colors = RColorBrewer::brewer.pal(3, "Set2")
        ) %>% dyLegend(show = "follow", width = 350)
    }
    else if (input$bop == "FA") {
       if (input$bop_fa == "FA") {
         data <- xts(x = index_2$`BoP: Financial Account (FA)`, order.by = index_2$Time)
       }
      else if (input$bop_fa == "di") {
        data <- xts(x = index_2[,c(19:21)], order.by = index_2$Time)
      }
      else if (input$bop_fa == "pi") {
        data <- xts(x = index_2[,c(47:49)], order.by = index_2$Time)
      }
      else if (input$bop_fa == "oi") {
        data <- xts(x = index_2[,as.numeric(input$fa_oi)], order.by = index_2$Time)
      }
      dygraph(data, main = "Financial Account") %>%
        dyAxis("y", label = "USD mn") %>% dyRangeSelector() %>% dyOptions(includeZero = TRUE, drawGrid = input$grid,
                                                                       axisLineColor = "navy",strokeWidth = 2) %>%
        dyLegend(show = "follow", width = 400)
    }
      else if (input$bop == "NE") {
        data <- xts(x = index_2$`BoP: Net Errors & Omissions`, order.by = index_2$Time)
        dygraph(data, main = "Net Error & Omissions") %>%
          dyAxis("y", label = "USD mn") %>% dyRangeSelector() %>% dyOptions(includeZero = TRUE, drawGrid = input$grid,
                                                                         axisLineColor = "navy",strokeWidth = 2
          ) %>% dyLegend(show = "follow")
      }
      else if (input$bop == "OB") {
        data <- xts(x = index_2$`BoP: Overall Balance`, order.by = index_2$Time)
        dygraph(data, main = "Overall Balance") %>%
          dyAxis("y", label = "USD mn") %>% dyRangeSelector() %>% dyOptions(includeZero = TRUE, drawGrid = input$grid,
                                                                         axisLineColor = "navy",strokeWidth = 2
          ) %>% dyLegend(show = "follow")
      }
      else {
        data <- xts(x = index_2[,c(53:56)], order.by = index_2$Time)
        dygraph(data, main = "Reserve/Related Item") %>%
          dyAxis("y", label = "USD mn") %>% dyRangeSelector() %>% dyOptions(includeZero = TRUE, drawGrid = input$grid,
                                                                         axisLineColor = "navy",strokeWidth = 2
          ) %>% dyLegend(show = "follow")
      }
    }
    else if (input$index == "demo") {
      if (input$dg == "pop") {
        if (input$p == "aa") {
          data <- index_4 %>% select(Time, `Population: Annual Avg`) %>% na.omit()
        }
        else if (input$p == "den") {
          data <- index_4 %>% select(Time, `Population Density: People per Square km`) %>% na.omit()
        }
        else if (input$p == "me") {
          data <- index_4 %>% select(Time, `VN: Population: Midyear Estimates`) %>% na.omit()
        }
        else if (input$p == "Total") {
          data <- index_4 %>% select(Time, `VN: Population: Total`) %>% na.omit()
        }
        else {
          data <- index_4 %>% select(Time, `VN: Population: Growth`) %>% na.omit()
        }
        data <- xts(x = data[,2], order.by = data$Time)
        dygraph(data, main = "Vietnam Population") %>%
          dyAxis("y", label = names(pop_unit[pop_unit == input$p]), axisLabelFormatter = htmlwidgets::JS("function(x) {return x.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,')}")
                 ) %>% dyRangeSelector() %>% dyOptions(includeZero = TRUE, drawGrid = input$grid,
                                                                         axisLineColor = "navy",strokeWidth = 2
          ) %>% dyLegend(show = "follow")
      }
      else if (input$dg == "epm") {
        data <- index_4 %>% select(Time, `Employment: Total`,`Employment: Rural`, `Employment: Urban`) %>% filter(Time >= "2012-06-01")
        data <- xts(x = data[,c(2:4)], order.by = data$Time)
        dygraph(data, main = "Vietnam Employment") %>%
          dyAxis("y", label = "Person th", axisLabelFormatter = htmlwidgets::JS("function(x) {return x.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,')}")
          ) %>% dyRangeSelector() %>% dyOptions(includeZero = TRUE, drawGrid = input$grid,
                                                axisLineColor = "navy",strokeWidth = 2
          ) %>% dyLegend(show = "follow")
      }
      else if (input$dg == "Labour") {
        data <- index_4[,c(1,11:13)] %>% filter(Time >= "2011-03-01")
        data <- xts(x = data[,c(2:4)], order.by = data$Time)
        dygraph(data, main = "Vietnam Labour Participation Rate") %>%
          dyAxis("y", label = "%"
          ) %>% dyRangeSelector() %>% dyOptions(includeZero = TRUE, drawGrid = input$grid,
                                                axisLineColor = "navy",strokeWidth = 2
          ) %>% dyLegend(show = "follow")
      }
      else if (input$dg == "unem") {
        if (input$choice_unem == "Quarterly") {
          data <- index_4 %>% select(Time, `Unemployment Rate: Rural`,
                                     `Unemployment Rate: Urban`,
                                     `Unemployment Rate: Whole Country`) %>% filter(Time >= "2010-12-01")
          data <- xts(x = data[,c(2:4)], order.by = data$Time)
        }
        else {
          data <- index_4 %>% select(Time, `Unemployment Rate: Whole Country: Annual`, `Unemployment Rate: Urban Area`,
                                     `VN: Unemployment: Modeled ILO Estimate: % of Total Labour Force`) %>%
            filter(Time >= "1991-12-01")
          if (input$est) {
            data <- xts(x = data[,c(2:4)], order.by = data$Time)
          }
          else {
            data <- xts(x = data[,c(2:3)], order.by = data$Time)
          }
        }
        dygraph(data, main = "Unemployment Rate") %>% dyAxis("y", label = "%"
        ) %>% dyRangeSelector() %>% dyOptions(includeZero = TRUE, drawGrid = input$grid,
                                              axisLineColor = "navy",strokeWidth = 2,
                                              connectSeparatedPoints = T
        ) %>% dyLegend(show = "follow")
      }
      else if (input$dg == "earn") {
        data <- index_4[,c(1:7)] %>% filter(Time >= "2011-12-01")
        data <- xts(x = data[,c(2:7)], order.by = data$Time)
        dygraph(data, main = "Average Monthly Earning") %>% dyAxis("y", label = "Thousand VND"
        ) %>% dyRangeSelector() %>% dyOptions(includeZero = TRUE, drawGrid = input$grid,
                                              axisLineColor = "navy",strokeWidth = 2
        ) %>% dyLegend(show = "follow")
      }
      else {
        if (input$overall) {
          data <- index_4 %>% select(Time, `Productivity: Overall`) %>% filter(Time >= "2003-12-01")
          data <- xts(x = data$`Productivity: Overall`, order.by = data$Time)
        }
        else {
          data <- index_4 %>% filter(Time >= "2005-12-01")
          data <- xts(x = data[,as.numeric(input$ns_choice)], order.by = data$Time)
        }
        dygraph(data, main = "Productivity") %>% dyAxis("y", label = "Millon VND"
        ) %>% dyRangeSelector() %>% dyOptions(includeZero = TRUE, drawGrid = input$grid,
                                              axisLineColor = "navy",strokeWidth = 2, connectSeparatedPoints = T
        ) %>% dyLegend(show = "follow")
      }
    }
    else if (input$index == "gdp") {
      if (!input$gdp) {
          data <- index_6 %>% filter(Time >= "2015-12-01")
        if (input$y_gdp == "GDP") {
          data <- xts(x = data[,as.numeric(c(1, input$a_rgdp))], order.by = data$Time)
        }
        else {
          data <- xts(x = data[,as.numeric(c(1, input$a_index))], order.by = data$Time)
        }
      }
      else {
        if (input$price == "rp") {
          data <- index_6 %>% filter(Time >= '2012-12-01')
          data <- xts(x = data[,as.numeric(c(1,input$q1_index))], order.by = data$Time)
        }
        else if (input$price == "cp") {
          data <- index_6 %>% filter(Time >= "1998-12-01")
          data <- xts(x = data[,as.numeric(c(1,input$q2_index))], order.by = data$Time)
        }
        else {
          data <- index_6 %>% filter(Time >= "2013-03-01")
          data <- xts(x = data[,as.numeric(c(1,input$q3_index))], order.by = data$Time)
        }
      }
      dygraph(data, main = "Vietnam Gross Domestric Product") %>% dyRangeSelector() %>% dyOptions(includeZero = TRUE, drawGrid = input$grid,
                                                                                     axisLineColor = "navy",strokeWidth = 2, connectSeparatedPoints = T
      ) %>% dyLegend(show = "follow") %>% dyAxis("y", axisLabelFormatter = htmlwidgets::JS("function(x) {return x.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,')}")
      )
    }
    else if (input$index == "Gold") {
      data <- xts(x = index_8[,as.numeric(input$gold)], order.by = index_8$Time)
      dygraph(data, main = "Gold price") %>% dyAxis("y", label = "Millon VND/Tael"
      ) %>% dyRangeSelector() %>% dyOptions(includeZero = TRUE, drawGrid = input$grid,
                                            axisLineColor = "navy",strokeWidth = 1, connectSeparatedPoints = T
      ) %>% dyLegend(show = "follow")
    }
    else if (input$index == "inf") {
      if (input$inf == "cpi") {
        data <- index_9 %>% select(Time, input$cpi) %>% na.omit()
        data <- xts(x = data[,-1], order.by = data$Time)
      }
      else if (input$inf == "cpi_y") {
        data <- index_9 %>% select(Time, input$cpi_y) %>% na.omit()
        data <- xts(x = data[,-1], order.by = data$Time)
      }
      else if (input$inf == "cpi_m") {
        data <- index_9 %>% select(Time, input$cpi_m) %>% na.omit()
        data <- xts(x = data[,-1], order.by = data$Time)
      }
      else if (input$inf == "cpi_w2014") {
        data <- index_9 %>% select(Time, input$cpi_w2014) %>% na.omit()
        data <- xts(x = data[,-1], order.by = data$Time)
      }
      else if (input$inf == "arp") {
        data <- index_9 %>% select(Time, input$arp) %>% na.omit()
        data <- xts(x = data[,-1], order.by = data$Time)
      }
      else if (input$inf == "ExPI: YOY") {
        data <- index_9 %>% select(Time, input$expi) %>% na.omit()
        data <- xts(x = data[,-1], order.by = data$Time)
      }
      else if (input$inf == "PPI: YOY") {
        data <- index_9 %>% select(Time, input$ppi) %>% na.omit()
        data <- xts(x = data[,-1], order.by = data$Time)
      }
      else if (input$inf == "ImPI: YOY") {
        data <- index_9 %>% select(Time, input$impi) %>% na.omit()
        data <- xts(x = data[,-1], order.by = data$Time)
      }
      else {
        data <- index_9 %>% select(Time, input$core) %>% na.omit()
        data <- xts(x = data[,-1], order.by = data$Time)
      }
      dygraph(data, main = "Macro data: Inflation") %>% dyAxis("y", label = ifelse(input$inf == "arp", "VND/kg","%")
      ) %>% dyRangeSelector() %>% dyOptions(includeZero = TRUE, drawGrid = input$grid,
                                            axisLineColor = "navy",strokeWidth = 1, connectSeparatedPoints = T
      ) %>% dyLegend(show = "follow")
    }
    else if (input$index == "m1") {
      if(input$m1_cho == "Money Supply: Quarter" ){
        data <- Money_supply_2[,as.numeric(input$chom1)]
        dygraph(data, main = "Money Supply") %>% 
          dyRangeSelector(dateWindow = c("2007-03-01", "2018-03-01")) %>% 
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
          dyLegend(width = 400) %>% 
          dyAxis("y",label = "VND",
                 valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}',
                 axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}') %>% 
          dyOptions(drawGrid = input$grid)
      } else {
        data <- Money_supply_1[,as.numeric(input$chom1_1)]
        dygraph(data, main = "Money Supply") %>% 
          dyRangeSelector(dateWindow = c("2007-03-01", "2018-03-01")) %>% 
          dyLegend(width = 400) %>% 
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
          dyAxis("y",label = "VND",
                 valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}',
                 axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}') %>% 
          dyOptions(drawGrid = input$grid)
      }
    }
    else if (input$index == "m2") {
      if (input$m2_cho == "total"){
        data <- Gov_debt_1[, as.numeric(input$chom2)]
        dygraph(data, main = "Government Debt") %>% 
          dyRangeSelector() %>% 
          dyLegend(width = 400) %>% 
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
          dyAxis("y",label = "VND",
                 valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}',
                 axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}')%>% 
          dyOptions(drawGrid = input$grid)
      } else {
        data <- Gov_debt_2[, as.numeric(input$chom2_1)]
        dygraph(data, main = "Government Debt") %>% 
          dyRangeSelector() %>% 
          dyLegend(width = 400) %>% 
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
          dyAxis("y",label = "Percentage",
                 independentTicks = TRUE,
                 valueFormatter = 'function(d){return Math.round(d*100)/1e2 + "%"}',
                 axisLabelFormatter = 'function(d){return Math.round(d*100)/1e2 + "%"}')%>% 
          dyOptions(drawGrid = input$grid)
      }
    }
    else if (input$index == "m3") {
      if (input$m3_cho == "m31"){
        data <- Gov_final_xts[, as.numeric(input$chom3)]
        dygraph(data, main = "State Budget") %>% 
          dyRangeSelector() %>% 
          dyLegend(width = 400) %>% 
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
          dyAxis("y",label = "VND",
                 valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}',
                 axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}')%>% 
          dyOptions(drawGrid = input$grid)
      } else {
        data <- Gov_plan[, as.numeric(c(1,input$chom3_1))]
        dygraph(data, main = "State Budget") %>% 
          dyRangeSelector() %>% 
          dyLegend(width = 400) %>% 
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
          dyAxis("y",label = "VND",
                 valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}',
                 axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}')%>% 
          dyOptions(drawGrid = input$grid)
      }
    }
    else if (input$index == "m4") {
      if (input$m4_cho == "proportion"){
        data <- Retail_pe_xts[, as.numeric(input$chom41)]
        dygraph(data, main = "Retail Sales") %>% 
          dyRangeSelector() %>% 
          dyLegend(width = 400) %>% 
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
          dyAxis("y",label = "Percentage",
                 independentTicks = TRUE,
                 valueFormatter = 'function(d){return Math.round(d*100)/1e2 + "%"}',
                 axisLabelFormatter = 'function(d){return Math.round(d*100)/1e2 + "%"}')%>% 
          dyOptions(drawGrid = input$grid)
      } else {
        data <- Retail_vnd_xts[,as.numeric(input$chom42)]
        dygraph(data, main = "Retail Sales") %>% 
          dyRangeSelector() %>% 
          dyLegend(width = 400) %>% 
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
          dyAxis("y",label = "VND",
                 valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}',
                 axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}')%>% 
          dyOptions(drawGrid = input$grid)
      }
    }
    else if (input$index == "m5") {
      if (input$m5_cho == "FDI_monthly"){
        data <- FDI_monthly_xts[, as.numeric(input$chom51)]
        dygraph(data, main = "Foreign Direct Investment") %>% 
          dyRangeSelector(dateWindow = c("2012-01-01", "2019-01-01")) %>% 
          dyLegend(width = 400) %>% 
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
          dyAxis("y",label = "USD",
                 valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}',
                 axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}')%>% 
          dyOptions(drawGrid = input$grid)
      } else if (input$m5_cho == "FDI_quarterly"){
        data <- FDI_quarterly_xts[, as.numeric(input$chom52)]
        dygraph(data, main = "Foreign Direct Investment") %>% 
          dyRangeSelector(dateWindow = c("2012-06-01", "2018-12-01")) %>% 
          dyLegend(width = 400) %>% 
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
          dyAxis("y",label = "USD",
                 valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}',
                 axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}')%>% 
          dyOptions(drawGrid = input$grid)
      } else {
        data <- FDI_country_xts[, as.numeric(input$chom53)]
        dygraph(data, main = "Foreign Direct Investment") %>% 
          dyRangeSelector() %>% 
          dyLegend(width = 400) %>% 
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
          dyAxis("y",label = "USD",
                 valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}',
                 axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}')%>% 
          dyOptions(drawGrid = input$grid)
      }
    }
    else if (input$index == "m6") {
      if (input$m6_cho == "2010"){
        data <- Investment_2010p_xts[, as.numeric(input$chom61)]
        dygraph(data, main = "Investment") %>% 
          dyRangeSelector(dateWindow = c("2009-12-01", "2017-12-01")) %>% 
          dyLegend(width = 400) %>% 
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
          dyAxis("y",label = "VND",
                 valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}',
                 axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}')%>% 
          dyOptions(drawGrid = input$grid)
      } else {
        data <- Investment_current_xts[, as.numeric(input$chom62)]
        dygraph(data, main = "Investment") %>% 
          dyRangeSelector(dateWindow = c("2007-12-01", "2017-12-01")) %>% 
          dyLegend(width = 400) %>% 
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
          dyAxis("y",label = "VND",
                 valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}',
                 axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}')%>% 
          dyOptions(drawGrid = input$grid)
      }
    }
    else if (input$index == "m7") {
      if (input$m7_cho == "2010"){
        data <- GDP_2010_xts[, as.numeric(input$chom71)]
        dygraph(data, main = "Gross Domestic Product") %>% 
          dyRangeSelector() %>% 
          dyLegend(width = 400) %>% 
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
          dyAxis("y",label = "VND",
                 valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}',
                 axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}')%>% 
          dyOptions(drawGrid = input$grid)
      } else {
        data <- GDP_current_xts[, as.numeric(input$chom72)]
        dygraph(data, main = "Gross Domestic Product") %>% 
          dyRangeSelector() %>% 
          dyLegend(width = 400) %>% 
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
          dyAxis("y",label = "VND",
                 valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}',
                 axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}')%>% 
          dyOptions(drawGrid = input$grid)
      }
    }
    else if (input$index == "m8") {
      if (input$m8_cho == "Buy"){
        data <- ER_buy_xts[, as.numeric(input$chom81)]
        dygraph(data, main = "Exchange rate") %>% 
          dyRangeSelector() %>% 
          dyLegend(width = 400) %>% 
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
          dyAxis("y",label = names(unit[unit == input$chom81]),
                 valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}',
                 axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}')%>% 
          dyOptions(drawGrid = input$grid)
      } else if (input$m8_cho == "Transfer"){
        data <- ER_transfer_xts[, as.numeric(input$chom82)]
        dygraph(data, main = "Exchange rate") %>% 
          dyRangeSelector() %>% 
          dyLegend(width = 400) %>% 
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
          dyAxis("y",label = names(unit[unit == input$chom82]),
                 valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}',
                 axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}')%>% 
          dyOptions(drawGrid = input$grid)
      } else {
        data <- ER_sell_xts[, as.numeric(input$chom83)]
        dygraph(data, main = "Exchange rate") %>% 
          dyRangeSelector() %>% 
          dyLegend(width = 400) %>% 
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
          dyAxis("y",label = names(unit[unit == input$chom83]),
                 valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}',
                 axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ".");}')%>% 
          dyOptions(drawGrid = input$grid)
      }
    }
  })
  output$table1 <- renderDT({
    datatable(Money_supply,
              extensions = 'Buttons',
              options = list(dom = 'Bfrtip',
                             buttons =list('colvis','copy','print',
                                           list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download'))
              )
    ) %>% 
      formatRound(c("Monetary Survey: Net Foreign Assets","Money Supply M1",
                    "Money Supply M1: Annual","Money Supply M2",
                    "Money Supply M2: Annual"),3)
  })
  output$table2 <- renderDT({
    datatable(Gov_debt,
              extensions = c('Buttons','FixedColumns'),
              options = list(
                dom ='Bfrtip',
                scrollX = TRUE,
                buttons =list('colvis','copy','print',
                              list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download')),
                fixedColumns = list(leftColumns = 2, rightColumns = 1)
              )
    ) %>% 
      formatRound(c(2:37),3)
  })
  output$table3 <- renderDT({
    if (input$m3_cho == "m31"){
      datatable(Gov_final,
                extensions = c('Buttons','FixedColumns'),
                options = list(
                  dom ='Bfrtip',
                  scrollX = TRUE,
                  buttons =list('colvis','copy','print',
                                list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download')),
                  fixedColumns = list(leftColumns = 2, rightColumns = 1)
                )
      )
    } else {
      datatable(Gov_plan,
                extensions = c('Buttons','FixedColumns'),
                options = list(
                  dom ='Bfrtip',
                  scrollX = TRUE,
                  buttons =list('colvis','copy','print',
                                list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download')),
                  fixedColumns = list(leftColumns = 2, rightColumns = 1)
                )
      )
    }
  })
  output$table4 <- renderDT({
    if (input$m4_cho == "proportion"){
      datatable(Retail_pe,
                extensions = c('Buttons','FixedColumns'),
                options = list(
                  dom ='Bfrtip',
                  scrollX = TRUE,
                  buttons =list('colvis','copy','print',
                                list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download')),
                  fixedColumns = list(leftColumns = 2, rightColumns = 1)
                )
      )
    } else {
      datatable(Retail_vnd,
                extensions = c('Buttons','FixedColumns'),
                options = list(
                  dom ='Bfrtip',
                  scrollX = TRUE,
                  buttons =list('colvis','copy','print',
                                list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download')),
                  fixedColumns = list(leftColumns = 2, rightColumns = 1)
                )
      )
    }
  })
  output$table5 <- renderDT({
    if (input$m5_cho == "FDI_monthly"){
      datatable(FDI_monthly,
                extensions = c('Buttons','FixedColumns'),
                options = list(
                  dom ='Bfrtip',
                  scrollX = TRUE,
                  buttons =list('colvis','copy','print',
                                list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download')),
                  fixedColumns = list(leftColumns = 2, rightColumns = 1)
                )
      )
    } else if (input$m5_cho == "FDI_quarterly"){
      datatable(FDI_quarterly,
                extensions = c('Buttons','FixedColumns'),
                options = list(
                  dom ='Bfrtip',
                  scrollX = TRUE,
                  buttons =list('colvis','copy','print',
                                list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download')),
                  fixedColumns = list(leftColumns = 2, rightColumns = 1)
                )
      )
    } else {
      datatable(FDI_country,
                extensions = c('Buttons','FixedColumns'),
                options = list(
                  dom ='Bfrtip',
                  scrollX = TRUE,
                  buttons =list('colvis','copy','print',
                                list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download')),
                  fixedColumns = list(leftColumns = 2, rightColumns = 1)
                )
      )
    }
  })
  output$table6 <- renderDT({
    if (input$m6_cho == "2010"){
      datatable(Investment_2010p,
                extensions = c('Buttons','FixedColumns'),
                options = list(
                  dom ='Bfrtip',
                  scrollX = TRUE,
                  buttons =list('colvis','copy','print',
                                list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download')),
                  fixedColumns = list(leftColumns = 2, rightColumns = 1)
                )
      )
    } else{
      datatable(Investment_current,
                extensions = c('Buttons','FixedColumns'),
                options = list(
                  dom ='Bfrtip',
                  scrollX = TRUE,
                  buttons =list('colvis','copy','print',
                                list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download')),
                  fixedColumns = list(leftColumns = 2, rightColumns = 1)
                )
      )
    }
  })
  output$table7 <- renderDT({
    if (input$m7_cho == "2010"){
      datatable(GDP_2010,
                extensions = c('Buttons','FixedColumns'),
                options = list(
                  dom ='Bfrtip',
                  scrollX = TRUE,
                  buttons =list('colvis','copy','print',
                                list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download')),
                  fixedColumns = list(leftColumns = 2, rightColumns = 1)
                )
      )
    } else{
      datatable(GDP_current,
                extensions = c('Buttons','FixedColumns'),
                options = list(
                  dom ='Bfrtip',
                  scrollX = TRUE,
                  buttons =list('colvis','copy','print',
                                list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download')),
                  fixedColumns = list(leftColumns = 2, rightColumns = 1)
                )
      )
    }
  })
  output$table8 <- renderDT({
    if (input$m8_cho == "Buy"){
      datatable(ER_buy,
                extensions = c('Buttons','FixedColumns'),
                options = list(
                  dom ='Bfrtip',
                  scrollX = TRUE,
                  buttons =list('colvis','copy','print',
                                list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download')),
                  fixedColumns = list(leftColumns = 2, rightColumns = 1)
                )
      )
    } else if (input$m8_cho == "Transfer"){
      datatable(ER_transfer,
                extensions = c('Buttons','FixedColumns'),
                options = list(
                  dom ='Bfrtip',
                  scrollX = TRUE,
                  buttons =list('colvis','copy','print',
                                list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download')),
                  fixedColumns = list(leftColumns = 2, rightColumns = 1)
                )
      )
    } else {
      datatable(ER_sell,
                extensions = c('Buttons','FixedColumns'),
                options = list(
                  dom ='Bfrtip',
                  scrollX = TRUE,
                  buttons =list('colvis','copy','print',
                                list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download')),
                  fixedColumns = list(leftColumns = 2, rightColumns = 1)
                )
      )
    }
  })
}

shinyApp(ui, server)

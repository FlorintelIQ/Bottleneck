# Mon Feb  2 15:24:12 2026 ------------------------------
# App for display

# app.R

library(shiny)
library(shinydashboard)
library(DT)
library(randomForest)
library(shinyjs)
library(plotly)
library(ggplot2)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- LOGIN CREDENTIALS ----
cred_raw <- Sys.getenv("FLORENTIL_CREDENTIALS", unset = "")
if (cred_raw == "") stop("FLORENTIL_CREDENTIALS ontbreekt in .Renviron")

pairs <- trimws(strsplit(cred_raw, ",")[[1]])

cred_list <- lapply(pairs, function(p) {
  parts <- strsplit(p, ":", fixed = TRUE)[[1]]
  if (length(parts) < 2) return(NULL)
  user <- trimws(parts[1])
  pass <- paste0(parts[-1], collapse = ":")  # laat ':' toe in password
  if (user == "") return(NULL)
  list(user = user, pass = pass)
})
cred_list <- Filter(Negate(is.null), cred_list)

VALID_USERS  <- vapply(cred_list, `[[`, character(1), "user")
VALID_PASSES <- vapply(cred_list, `[[`, character(1), "pass")
names(VALID_PASSES) <- VALID_USERS

# ---- MODEL LADEN ----
model_path <- "rf.RDS"
model <- if (file.exists(model_path)) readRDS(model_path) else NULL

# ---- DATA ----
data_path <- "data.rds"

read_rds_object <- function(path) {
  if (!file.exists(path)) stop("Bestand niet gevonden: ", path)
  readRDS(path)
}

initial_data <- read_rds_object(data_path)

# ---- UI ----
ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "Tiresias"),
  
  # id laten staan (handig), maar we sturen zichtbaarheid via JS/CSS
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("table")),
      menuItem("Invoer & Predictie", tabName = "predict", icon = icon("chart-line")),
      menuItem("Over", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        /* zacht achtergrondje */
        .content-wrapper, .right-side { background-color: #f5f7f6; }

        /* login layout */
        .login-wrap {
          min-height: calc(100vh - 100px);
          display: flex;
          align-items: center;
          justify-content: center;
          padding: 20px;
        }
        .login-card {
          width: 440px;
          max-width: 95vw;
          border-radius: 14px;
          overflow: hidden;
          box-shadow: 0 10px 30px rgba(0,0,0,0.10);
        }
        .login-header {
          padding: 18px 20px;
          color: #fff;
          background: linear-gradient(135deg, #00a65a, #008d4c);
        }
        .login-header h2 {
          margin: 0;
          font-size: 20px;
          font-weight: 700;
          letter-spacing: 0.2px;
        }
        .login-header .sub {
          opacity: 0.9;
          margin-top: 4px;
          font-size: 13px;
        }
        .login-body {
          background: #fff;
          padding: 18px 20px 20px 20px;
        }
        .login-footer {
          background: #fff;
          padding: 0 20px 18px 20px;
        }
        .btn-login {
          width: 100%;
          font-weight: 600;
        }
        .login-note {
          color: #7a7a7a;
          font-size: 12px;
          margin-top: 10px;
        }
        .whoami {
          color: #666;
          font-size: 12px;
          margin-top: 6px;
        }
      "))
    ),
    
    ## ---- LOGIN PANEL ----
    div(
      id = "login_panel",
      div(
        class = "login-wrap",
        div(
          class = "login-card",
          div(
            class = "login-header",
            tags$h2(icon("leaf"), " Florintel IQ"),
            div(class = "sub", "Inloggen om het dashboard te openen")
          ),
          div(
            class = "login-body",
            textInput("login_user", "Gebruiker", placeholder = "bijv. admin"),
            passwordInput("login_pass", "Wachtwoord", placeholder = "••••••••"),
            uiOutput("login_alert")
          ),
          div(
            class = "login-footer",
            actionButton("btn_login", "Inloggen", class = "btn btn-success btn-login"),
            div(class = "login-note", icon("lock"), "© Florintel")
          )
        )
      )
    ),
    
    ## ---- APP PANEL ----
    hidden(
      div(
        id = "app_panel",
        
        fluidRow(
          column(
            width = 12,
            div(style = "display:flex; justify-content: space-between; align-items:center; margin-bottom: 10px;",
                div(class = "whoami", textOutput("whoami")),
                actionButton("btn_logout", "Uitloggen", class = "btn btn-default")
            )
          )
        ),
        
        tabItems(
          tabItem(
            tabName = "dashboard",
            tabBox(
              width = 12,
              tabPanel(
                title = "Dagelijkse data",
                value = "daily_data",
                
                fluidRow(
                  box(
                    width = 2,
                    title = "Selectie",
                    status = "success",
                    solidHeader = TRUE,
                    
                    # Calendar selectie (server vult later min/max of default)
                    dateInput(
                      inputId = "daily_date",
                      label   = "Kies datum",
                      value   = min(initial_data$date, na.rm = TRUE),
                      format  = "yyyy-mm-dd",
                      language = "nl"
                    )
                  )
                ),
                
                fluidRow(
                  box(
                    width = 12,
                    title = "Deliveries per kwartier",
                    status = "success",
                    solidHeader = TRUE,
                    
                    # Plotly figuur (server komt later)
                    plotlyOutput(
                      outputId = "deliveries_q15_plot",
                      height   = "450px"
                    )
                  )
                )
              ), # end tabPanel
              tabPanel("Data tabel", DTOutput("data_table"))
            )# end tabBox
          ), # end dashboard
          
          tabItem(
            tabName = "predict",
            fluidRow(
              box(
                title = "Nieuwe invoer",
                width = 10,
                status = "primary",
                solidHeader = TRUE,
                
                numericInput("f1", "577", 10),
                numericInput("f2", "566", 10),
                numericInput("f3", "544", 10),
                numericInput("f4", "533", 10),
                numericInput("f5", "580", 10),
                numericInput("f6", "50cm", 10),
                numericInput("f7", "60cm", 10),
                numericInput("f8", "70cm", 10),
                numericInput("f9", "80cm", 10),
                
                actionButton("btn_predict", "Voorspel", class = "btn-primary"),
                actionButton("btn_add", "Voeg toe aan data")
              ),
              
              box(
                title = "Voorspelling",
                width = 6,
                status = "info",
                solidHeader = TRUE,
                verbatimTextOutput("prediction")
              )
            )
          ),
          
          tabItem(
            tabName = "about",
            box(
              width = 12,
              title = "Over dit dashboard",
              status = "warning",
              solidHeader = TRUE,
              p("© Florintel IQ. Alle rechten voorbehouden."),
              p("Hoewel de inhoud met zorg is samengesteld, kunnen aan de getoonde analyses en voorspellingen geen rechten worden ontleend."),
              p("Florintel IQ aanvaardt geen aansprakelijkheid voor beslissingen die op basis van deze informatie worden genomen.")
            )
          )
        )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  logged_in    <- reactiveVal(FALSE)
  login_error  <- reactiveVal(NULL)
  current_user <- reactiveVal(NULL)
  
  # ✅ ROBUUST: sidebar echt verbergen + content full width als je NIET ingelogd bent
  observe({
    invalidateLater(200, session)  # geeft AdminLTE tijd om DOM te renderen
    
    if (!logged_in()) {
      runjs("
        // verberg sidebar + zet content naar links
        $('.main-sidebar').hide();
        $('.content-wrapper, .right-side').css('margin-left','0');
      ")
    } else {
      runjs("
        // toon sidebar + herstel standaard marge
        $('.main-sidebar').show();
        $('.content-wrapper, .right-side').css('margin-left','230px');
      ")
    }
  })
  
  # Enter-to-login in wachtwoord veld
  observe({
    runjs("
      $(document).off('keypress.florintelLogin');
      $(document).on('keypress.florintelLogin', '#login_pass', function(e){
        if(e.which === 13){ $('#btn_login').click(); }
      });
    ")
  })
  
  output$login_alert <- renderUI({
    msg <- login_error()
    if (is.null(msg)) return(NULL)
    div(class = "callout callout-danger", style = "margin-top: 10px;",
        tags$b("Inloggen mislukt: "), msg
    )
  })
  
  observeEvent(input$btn_login, {
    user <- input$login_user
    pass <- input$login_pass
    
    ok <- (!is.null(VALID_PASSES[[user]])) && identical(pass, VALID_PASSES[[user]])
    
    if (ok) {
      logged_in(TRUE)
      current_user(user)
      login_error(NULL)
      hide("login_panel"); show("app_panel")
      updateTextInput(session, "login_user", value = "")
      updateTextInput(session, "login_pass", value = "")
    } else {
      login_error("Onjuiste gebruikersnaam of wachtwoord.")
    }
  })
  
  observeEvent(input$btn_logout, {
    logged_in(FALSE)
    current_user(NULL)
    login_error(NULL)
    show("login_panel"); hide("app_panel")
  })
  
  output$whoami <- renderText({
    req(logged_in())
    paste("Ingelogd als:", current_user())
  })
  
  #---- APP LOGICA ----
  rv <- reactiveValues(data = initial_data)
  
  output$data_table <- renderDT({
    req(logged_in())
    datatable(rv$data, options = list(scrollX = TRUE, dom = "tip"), editable = TRUE)
  })
  
  # output$data_summary <- renderPrint({
  #   req(logged_in())
  #   summary(rv$data)
  # })
  
  observe({
    req(rv$data)
    req("date" %in% names(rv$data))
    
    dmin <- min(rv$data$date, na.rm = TRUE)
    dmax <- max(rv$data$date, na.rm = TRUE)
    
    updateDateInput(
      session,
      inputId = "daily_date",
      min = dmin,
      max = dmax
      # value = dmax   # evt: standaard meest recente dag
    )
  })
  
  # --- reactive: deliveries per kwartier voor gekozen dag ---
  daily_q15 <- reactive({
    req(rv$data, input$daily_date)
    req(all(c("date", "t_time", "deliveries") %in% names(rv$data)))
    
    x <- rv$data
    
    # filter op gekozen datum
    x <- x[x$date == input$daily_date, , drop = FALSE]
    
    if (nrow(x) == 0) {
      return(data.frame(
        datetime = as.POSIXct(character()),
        deliveries = numeric()
      ))
    }
    
    # combineer date + t_time -> POSIXct (voor nette plotly x-as)
    x$datetime <- as.POSIXct(
      paste(x$date, x$t_time),
      format = "%Y-%m-%d %H:%M",
      tz = "Europe/Amsterdam"
    )
    
    # aggregatie (voor het geval meerdere panel-rows per kwartier)
    agg <- aggregate(
      deliveries ~ datetime,
      data = x,
      FUN = sum,
      na.rm = TRUE
    )
    
    # volledige kwartier-as (00:00–23:45)
    day_start <- as.POSIXct(input$daily_date, tz = "Europe/Amsterdam")
    full_time <- seq(day_start, day_start + 24*60*60 - 15*60, by = "15 min")
    
    out <- merge(
      data.frame(datetime = full_time),
      agg,
      by = "datetime",
      all.x = TRUE
    )
    
    out$deliveries[is.na(out$deliveries)] <- 0
    
    out
  })
  
  # --- plotly figuur ---
  output$deliveries_q15_plot <- plotly::renderPlotly({
    d <- daily_q15()
    
    validate(
      need(nrow(d) > 0, "Geen data voor deze datum.")
    )
    
    plotly::plot_ly(
      data = d,
      x = ~datetime,
      y = ~deliveries,
      type = "scatter",
      mode = "lines+markers",
      hovertemplate = paste0(
        "%{x|%H:%M}<br>",
        "Deliveries: %{y}<extra></extra>"
      )
    ) %>%
      plotly::layout(
        xaxis = list(
          title = "Tijd (per 15 minuten)",
          tickformat = "%H:%M",
          range = list(
            as.POSIXct(
              paste(input$daily_date, "07:00"),
              tz = "Europe/Amsterdam"
            ),
            as.POSIXct(
              paste(input$daily_date, "12:00"),
              tz = "Europe/Amsterdam"
            )
          )
        ),
        yaxis = list(
          title = "Aantal deliveries"
        ),
        margin = list(l = 60, r = 20, b = 60, t = 30)
      )
  })
  
}

shinyApp(ui, server)

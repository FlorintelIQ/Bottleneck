# app.R
# Tiresias (Florintel IQ) — Shiny dashboard with login + DT + Plotly + floating chatbot
# Fixed: no fragile hide()/show() for main panels, no 200ms polling; robust login rendering.

library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(plotly)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Optional for debugging; comment out for production
options(shiny.sanitize.errors = FALSE)

# ---- LOGIN CREDENTIALS ----
cred_raw <- Sys.getenv("FLORENTIL_CREDENTIALS", unset = "")
if (cred_raw == "") stop("FLORENTIL_CREDENTIALS ontbreekt in .Renviron")

pairs <- trimws(strsplit(cred_raw, ",")[[1]])
cred_list <- lapply(pairs, function(p) {
  parts <- strsplit(p, ":", fixed = TRUE)[[1]]
  if (length(parts) < 2) return(NULL)
  user <- trimws(parts[1])
  pass <- paste0(parts[-1], collapse = ":") # allow ':' in password
  if (user == "") return(NULL)
  list(user = user, pass = pass)
})
cred_list <- Filter(Negate(is.null), cred_list)

VALID_USERS  <- vapply(cred_list, `[[`, character(1), "user")
VALID_PASSES <- vapply(cred_list, `[[`, character(1), "pass")
names(VALID_PASSES) <- VALID_USERS

# ---- DATA ----
data_path <- "data.rds"
if (!file.exists(data_path)) stop("Bestand niet gevonden: ", data_path)
initial_data <- readRDS(data_path)

# Normalize types (prevents “no data after filter”)
stopifnot(is.data.frame(initial_data))

if ("date" %in% names(initial_data)) {
  initial_data$date <- as.Date(initial_data$date)
}
if ("t_time" %in% names(initial_data)) {
  initial_data$t_time <- substr(as.character(initial_data$t_time), 1, 5) # handles "07:00:00"
}
if ("deliveries" %in% names(initial_data)) {
  initial_data$deliveries <- as.numeric(initial_data$deliveries)
}

default_date <- if ("date" %in% names(initial_data) && any(!is.na(initial_data$date))) {
  max(initial_data$date, na.rm = TRUE)
} else {
  Sys.Date()
}

# ---- UI ----
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Tiresias"),
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
        .content-wrapper, .right-side { background-color: #f5f7f6; }

        /* Login layout */
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
          background: #fff;
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
        }
        .login-header .sub {
          opacity: 0.9;
          margin-top: 4px;
          font-size: 13px;
        }
        .login-body { padding: 18px 20px 20px 20px; }
        .login-footer { padding: 0 20px 18px 20px; }
        .btn-login { width: 100%; font-weight: 600; }
        .whoami { color: #666; font-size: 12px; margin-top: 6px; }

        /* Floating chatbot button */
        #chatbot-fab-wrap{
          position: fixed;
          right: 20px;
          bottom: 20px;
          z-index: 10000;
        }

        /* Chatbot panel */
        #chatbot-container{
          position: fixed;
          right: 20px;
          bottom: 20px;
          width: 340px;
          z-index: 9999;
          box-shadow: 0 8px 24px rgba(0,0,0,0.18);
          border-radius: 10px;
          overflow: hidden;
          background: #ffffff;
        }
      "))
    ),
    
    uiOutput("main_ui")
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  logged_in    <- reactiveVal(FALSE)
  login_error  <- reactiveVal(NULL)
  current_user <- reactiveVal(NULL)
  
  rv <- reactiveValues(data = initial_data)
  
  # --- Chat open/close state for conditionalPanel (0/1) ---
  chatbot_open <- reactiveVal(0)
  output$chatbot_open <- renderText(chatbot_open())
  outputOptions(output, "chatbot_open", suspendWhenHidden = FALSE)
  
  # Render either login UI or the app UI (robust; no hide()/show() for main layout)
  output$main_ui <- renderUI({
    if (!logged_in()) {
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
            div(style = "color:#7a7a7a; font-size:12px; margin-top:10px;", icon("lock"), "© Florintel")
          )
        )
      )
    } else {
      tagList(
        # top row with whoami + logout
        fluidRow(
          column(
            width = 12,
            div(
              style = "display:flex; justify-content: space-between; align-items:center; margin-bottom: 10px;",
              div(class = "whoami", textOutput("whoami")),
              actionButton("btn_logout", "Uitloggen", class = "btn btn-default")
            )
          )
        ),
        
        # main content tabs
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
                    dateInput(
                      inputId = "daily_date",
                      label   = "Kies datum",
                      value   = default_date,
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
                    plotlyOutput("deliveries_q15_plot", height = "450px")
                  )
                )
              ),
              tabPanel("Data tabel", DTOutput("data_table"))
            )
          ),
          
          tabItem(
            tabName = "predict",
            box(
              width = 12,
              title = "Predictie",
              status = "primary",
              solidHeader = TRUE,
              p("Predictie-tab kan hier later worden ingevuld.")
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
        ),
        
        # ---- CHATBOT FAB + PANEL (only after login) ----
        div(
          id = "chatbot-fab-wrap",
          actionButton("chatbot_toggle", "Advies", class = "btn btn-success")
        ),
        
        # helper output for conditionalPanel
        hidden(textOutput("chatbot_open")),
        
        conditionalPanel(
          condition = "output.chatbot_open == '1'",
          tags$div(
            id = "chatbot-container",
            tags$div(
              style = "background:#2e7d32; color:#fff; padding:10px 12px; font-weight:600;
                       display:flex; justify-content:space-between; align-items:center;",
              "Advies-assistent",
              actionButton(
                "chatbot_close",
                label = HTML("&times;"),
                class = "btn btn-link",
                style = "color:#fff; font-size:18px; padding:0; line-height:1;"
              )
            ),
            tags$div(
              style = "padding:10px 12px;",
              uiOutput("chatbot_log"),
              fluidRow(
                column(9, textInput("chatbot_msg", NULL, placeholder = "Typ je vraag...", width = "100%")),
                column(3, actionButton("chatbot_send", "Send", width = "100%"))
              )
            )
          )
        )
      )
    }
  })
  
  # Sidebar show/hide based on login (event-based, no polling)
  observeEvent(logged_in(), {
    if (!logged_in()) {
      runjs("$('.main-sidebar').hide(); $('.content-wrapper, .right-side').css('margin-left','0');")
    } else {
      runjs("$('.main-sidebar').show(); $('.content-wrapper, .right-side').css('margin-left','230px');")
    }
  }, ignoreInit = FALSE)
  
  # Enter-to-login (bind once per session start; safe even if login UI re-renders)
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
    div(
      class = "callout callout-danger",
      style = "margin-top:10px;",
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
      chatbot_open(0)
    } else {
      login_error("Onjuiste gebruikersnaam of wachtwoord.")
    }
  })
  
  observeEvent(input$btn_logout, {
    logged_in(FALSE)
    current_user(NULL)
    login_error(NULL)
    chatbot_open(0)
  })
  
  output$whoami <- renderText({
    paste("Ingelogd als:", current_user() %||% "")
  })
  
  # ---- DT table ----
  output$data_table <- renderDT({
    req(logged_in())
    datatable(rv$data, options = list(scrollX = TRUE, dom = "tip"), editable = TRUE)
  })
  
  # Update date input bounds after login
  observe({
    req(logged_in())
    req(rv$data)
    req("date" %in% names(rv$data))
    
    dmin <- min(rv$data$date, na.rm = TRUE)
    dmax <- max(rv$data$date, na.rm = TRUE)
    
    updateDateInput(session, "daily_date", min = dmin, max = dmax, value = dmax)
  })
  
  # ---- Daily aggregation for plot ----
  daily_q15 <- reactive({
    req(logged_in())
    req(rv$data, input$daily_date)
    req(all(c("date", "t_time", "deliveries") %in% names(rv$data)))
    
    x <- rv$data
    x$date <- as.Date(x$date)
    x$t_time <- substr(as.character(x$t_time), 1, 5)
    
    x <- x[x$date == as.Date(input$daily_date), , drop = FALSE]
    if (nrow(x) == 0) {
      return(data.frame(datetime = as.POSIXct(character()), deliveries = numeric()))
    }
    
    x$datetime <- as.POSIXct(
      paste(x$date, x$t_time),
      format = "%Y-%m-%d %H:%M",
      tz = "Europe/Amsterdam"
    )
    
    x <- x[!is.na(x$datetime), , drop = FALSE]
    if (nrow(x) == 0) {
      return(data.frame(datetime = as.POSIXct(character()), deliveries = numeric()))
    }
    
    agg <- aggregate(deliveries ~ datetime, data = x, FUN = sum, na.rm = TRUE)
    
    day_start <- as.POSIXct(as.Date(input$daily_date), tz = "Europe/Amsterdam")
    full_time <- seq(day_start, day_start + 24*60*60 - 15*60, by = "15 min")
    
    out <- merge(data.frame(datetime = full_time), agg, by = "datetime", all.x = TRUE)
    out$deliveries[is.na(out$deliveries)] <- 0
    out
  })
  
  # ---- Plotly ----
  output$deliveries_q15_plot <- renderPlotly({
    req(logged_in())
    d <- daily_q15()
    
    validate(need(nrow(d) > 0, "Geen data voor deze datum."))
    
    plot_ly(
      data = d,
      x = ~datetime,
      y = ~deliveries,
      type = "scatter",
      mode = "lines+markers",
      hovertemplate = "%{x|%H:%M}<br>Deliveries: %{y}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(
          title = "Tijd (per 15 minuten)",
          tickformat = "%H:%M",
          range = list(
            as.POSIXct(paste(as.Date(input$daily_date), "07:00"), tz = "Europe/Amsterdam"),
            as.POSIXct(paste(as.Date(input$daily_date), "12:00"), tz = "Europe/Amsterdam")
          )
        ),
        yaxis = list(title = "Aantal deliveries"),
        margin = list(l = 60, r = 20, b = 60, t = 30)
      )
  })
  
  # ---- Chatbot (dummy advice) ----
  chat <- reactiveValues(messages = list(
    list(role = "assistant", text = "Hoi ik ben Tiri! Ik kan advies geven op basis van je data. Wat wil je weten?")
  ))
  
  output$chatbot_log <- renderUI({
    items <- lapply(chat$messages, function(m) {
      if (m$role == "user") {
        tags$div(style = "margin:6px 0; font-weight:600;", paste("Jij:", m$text))
      } else {
        tags$div(style = "margin:6px 0; color:#2e7d32;", paste("Assistent:", m$text))
      }
    })
    
    tags$div(
      style = "height:210px; overflow-y:auto; border:1px solid #e5e5e5; border-radius:8px;
               padding:8px; background:#fafafa; margin-bottom:10px; font-size:13px; line-height:1.35;",
      items
    )
  })
  
  outputOptions(output, "chatbot_log", suspendWhenHidden = FALSE)
  
  observeEvent(input$chatbot_toggle, {
    req(logged_in())
    chatbot_open(1)
  })
  
  observeEvent(input$chatbot_close, {
    chatbot_open(0)
  })
  
  observeEvent(input$chatbot_send, {
    req(logged_in())
    req(nzchar(input$chatbot_msg))
    
    chat$messages <- append(chat$messages, list(list(role = "user", text = input$chatbot_msg)))
    
    reply <- paste0(
      "Dank voor je vraag. Op basis van de huidige data zie ik dat:\n",
      "- De deliveries zich concentreren tussen 07:00 en 12:00\n",
      "- Pieken meestal samenhangen met vaste tijdslots\n\n",
      "Tip: let vooral op afwijkingen t.o.v. dit patroon. ",
      "Als een kwartier plots sterk afwijkt, kan dat duiden op vertragingen ",
      "of operationele issues.\n\n",
      "(Dit advies is voorlopig algemeen; straks wordt dit dynamisch.)"
    )
    
    chat$messages <- append(chat$messages, list(list(role = "assistant", text = reply)))
    updateTextInput(session, "chatbot_msg", value = "")
  })
  
  # Enter-to-send for chatbot input
  observe({
    runjs("
      $(document).off('keypress.florintelChat');
      $(document).on('keypress.florintelChat', 'input#chatbot_msg', function(e){
        if(e.which === 13){
          $('#chatbot_send').click();
          e.preventDefault();
        }
      });
    ")
  })
}

shinyApp(ui, server)

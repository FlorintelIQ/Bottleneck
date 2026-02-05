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

        /* -------------------------
           Floating chatbot (rechts-onder)
           ------------------------- */
        #chatbot-container {
          position: fixed;
          right: 20px;
          bottom: 20px;
          width: 340px;
          z-index: 9999;
          box-shadow: 0 8px 24px rgba(0,0,0,0.18);
          border-radius: 10px;
          overflow: hidden;
          background: #ffffff;
          display: none; /* start hidden; toggle via button */
        }
        #chatbot-header {
          background: #2e7d32;
          color: white;
          padding: 10px 12px;
          font-weight: 600;
          display: flex;
          justify-content: space-between;
          align-items: center;
        }
        #chatbot-body {
          padding: 10px 12px;
        }
        #chatbot-log {
          height: 210px;
          overflow-y: auto;
          border: 1px solid #e5e5e5;
          border-radius: 8px;
          padding: 8px;
          background: #fafafa;
          margin-bottom: 10px;
          font-size: 13px;
          line-height: 1.35;
        }
        #chatbot-toggle {
          position: fixed;
          right: 20px;
          bottom: 20px;
          z-index: 10000;
          border-radius: 999px;
          padding: 10px 14px;
          background: #2e7d32;
          color: white;
          border: none;
          cursor: pointer;
          box-shadow: 0 8px 24px rgba(0,0,0,0.18);
        }
        #chatbot-close {
          background: transparent;
          border: 0;
          color: white;
          font-size: 18px;
          cursor: pointer;
          line-height: 1;
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
            div(
              style = "display:flex; justify-content: space-between; align-items:center; margin-bottom: 10px;",
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
                    
                    plotlyOutput(
                      outputId = "deliveries_q15_plot",
                      height   = "450px"
                    )
                  )
                )
              ), # end tabPanel
              
              tabPanel("Data tabel", DTOutput("data_table"))
            ) # end tabBox
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
        ),
        
        # -------------------------
        # Chatbot UI (alleen zichtbaar NA login, want binnen app_panel)
        # -------------------------
        tags$button(
          id = "chatbot-toggle",
          type = "button",
          "Advies"
        ),
        
        tags$div(
          id = "chatbot-container",
          tags$div(
            id = "chatbot-header",
            "Advies-assistent",
            tags$button(
              id = "chatbot-close",
              type = "button",
              HTML("&times;")
            )
          ),
          tags$div(
            id = "chatbot-body",
            uiOutput("chatbot_log"),
            fluidRow(
              column(
                width = 9,
                textInput("chatbot_msg", NULL, placeholder = "Typ je vraag...", width = "100%")
              ),
              column(
                width = 3,
                actionButton("chatbot_send", "Send", width = "100%")
              )
            )
          )
        ),
        
        # JS open/close + Enter-to-send voor chatbot
        tags$script(HTML("
          document.addEventListener('DOMContentLoaded', function() {
            var toggle = document.getElementById('chatbot-toggle');
            var box = document.getElementById('chatbot-container');
            var closeBtn = document.getElementById('chatbot-close');

            if (toggle && box && closeBtn) {
              toggle.addEventListener('click', function() {
                box.style.display = 'block';
                toggle.style.display = 'none';
              });

              closeBtn.addEventListener('click', function() {
                box.style.display = 'none';
                toggle.style.display = 'block';
              });
            }

            // Enter-to-send
            $(document).off('keypress.florintelChat');
            $(document).on('keypress.florintelChat', '#chatbot_msg', function(e){
              if(e.which === 13){
                $('#chatbot_send').click();
                e.preventDefault();
              }
            });
          });
        "))
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
    div(
      class = "callout callout-danger",
      style = "margin-top: 10px;",
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
  
  # ---- DateInput min/max dynamisch zetten op basis van rv$data ----
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
      # value = dmax
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
            as.POSIXct(paste(input$daily_date, "07:00"), tz = "Europe/Amsterdam"),
            as.POSIXct(paste(input$daily_date, "12:00"), tz = "Europe/Amsterdam")
          )
        ),
        yaxis = list(title = "Aantal deliveries"),
        margin = list(l = 60, r = 20, b = 60, t = 30)
      )
  })
  
  # -------------------------
  # Chatbot (dummy advies) — server state
  # -------------------------
  chat <- reactiveValues(messages = list(
    list(role = "Tiri", text = "Hoi ik ben Tiri! Ik kan advies geven op basis van je data. Wat wil je weten?")
  ))
  
  output$chatbot_log <- renderUI({
    items <- lapply(chat$messages, function(m) {
      if (m$role == "user") {
        tags$div(style = "margin:6px 0; font-weight:600;", paste("Jij:", m$text))
      } else {
        tags$div(style = "margin:6px 0; color:#2e7d32;", paste("Assistent:", m$text))
      }
    })
    tags$div(id = "chatbot-log", items)
  })
  
  outputOptions(output, "chatbot_log", suspendWhenHidden = FALSE)
  
  observeEvent(input$chatbot_send, {
    req(logged_in())
    req(nzchar(input$chatbot_msg))
    
    user_msg <- input$chatbot_msg
    
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
}

shinyApp(ui, server)

# app.R
# Tiresias (Florintel IQ) — Shiny dashboard with login + DT + Plotly + floating ellmer chatbot
# Includes:
# - Login (credentials via FLORENTIL_CREDENTIALS)
# - Dashboard: daily plotly line (deliveries per 15 min)
# - Planner (rush): create predict_df + GLM Poisson prediction with 95% CI (lower_ci/upper_ci)
# - Planner outputs: DT table + Plotly chart (rush bars on y2 + prediction line + CI ribbon)
# - Floating ellmer chatbot with tools (register_tool)
# Notes:
# - jsonlite removed (avoids validate() namespace conflicts)
# - shiny::validate()/shiny::need() used explicitly

library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(plotly)
library(ellmer)

`%||%` <- function(a, b) if (!is.null(a)) a else b
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
stopifnot(is.data.frame(initial_data))

# Normalize types
if ("date" %in% names(initial_data)) {
  initial_data$date <- as.Date(initial_data$date)
}
if ("t_time" %in% names(initial_data)) {
  initial_data$t_time <- substr(as.character(initial_data$t_time), 1, 5)
}
if ("deliveries" %in% names(initial_data)) {
  initial_data$deliveries <- as.numeric(initial_data$deliveries)
}

default_date <- if ("date" %in% names(initial_data) && any(!is.na(initial_data$date))) {
  max(initial_data$date, na.rm = TRUE)
} else {
  Sys.Date()
}

# ---- MODEL ----
mod1_path <- "mod2.Rdata"
if (!file.exists(mod1_path)) stop("Bestand niet gevonden: ", mod1_path)
mod1fit <- get(load(mod1_path))

# ---- UI ----
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Tiresias"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("table")),
      menuItem("Planner", tabName = "rush", icon = icon("clock")),
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
  
  # =============================================================================
  # RUSH PLANNER
  # =============================================================================
  rv_rush <- reactiveValues(
    rush_df = NULL,
    predict_df = NULL
  )
  
  make_rush_grid <- function(date, tz = "Europe/Amsterdam") {
    start <- as.POSIXct(paste(as.Date(date), "07:00:00"), tz = tz)
    end   <- as.POSIXct(paste(as.Date(date), "12:00:00"), tz = tz)
    
    ts <- seq(from = start, to = end, by = "15 min")
    ts <- ts[ts < end]  # 20 rows: 07:00..11:45
    
    data.frame(
      datetime = ts,
      time     = format(ts, "%H:%M"),
      rush     = 0L,
      stringsAsFactors = FALSE
    )
  }
  
  gen_newdata <- function(rush, day, val = 0, mom = 0, xmas = 0,
                          weekday = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
                          tz = "Europe/Amsterdam") {
    
    day <- as.Date(day)
    if (length(rush) != 20) stop("rush moet lengte 20 hebben (t=0..19, 07:00–11:45).")
    
    doy <- as.integer(strftime(day, format = "%j", tz = tz))
    wd  <- strftime(day, format = "%a", tz = tz)
    
    data.frame(
      t            = 0:19,
      rush         = as.integer(rush),
      weekday      = wd,
      seasonality1 = sin(2 * pi * doy / 365),
      seasonality2 = cos(2 * pi * doy / 365),
      val_window   = val,
      mom_window   = mom,
      xmas_window  = xmas,
      stringsAsFactors = FALSE
    )
  }
  
  observeEvent(input$rush_date, {
    req(logged_in())
    rv_rush$rush_df <- make_rush_grid(input$rush_date)
    rv_rush$predict_df <- NULL
  }, ignoreInit = FALSE)
  
  observeEvent(input$rush_reset, {
    req(logged_in(), rv_rush$rush_df)
    rv_rush$rush_df$rush <- 0L
    rv_rush$predict_df <- NULL
  })
  
  observeEvent(input$rush_fill_1, {
    req(logged_in(), rv_rush$rush_df)
    rv_rush$rush_df$rush <- 1L
    rv_rush$predict_df <- NULL
  })
  
  output$rush_table <- renderDT({
    req(logged_in(), rv_rush$rush_df)
    
    datatable(
      rv_rush$rush_df[, 2:3],
      rownames = FALSE,
      editable = list(
        target = "cell",
        disable = list(columns = c(0)),
        options = list(
          list(
            column = 2,
            editor = "select",
            options = list(c(0, 1))
          )
        )
      ),
      options = list(dom = "tip", ordering = FALSE)
    )
  })
  
  observeEvent(input$rush_table_cell_edit, {
    req(logged_in(), rv_rush$rush_df)
    info <- input$rush_table_cell_edit
    
    i <- info$row
    j <- info$col + 1 # DT 0-based
    if (j != 3) return()
    
    v <- suppressWarnings(as.integer(info$value))
    if (is.na(v)) v <- 0L
    v <- ifelse(v >= 1, 1L, 0L)
    
    rv_rush$rush_df[i, j] <- v
    rv_rush$predict_df <- NULL
  })
  
  observeEvent(input$rush_make_features, {
    req(logged_in(), rv_rush$rush_df, input$rush_date)
    
    rush_vec <- as.integer(rv_rush$rush_df$rush)
    if (length(rush_vec) != 20) {
      showNotification("Rush-profiel moet 20 slots hebben (07:00–11:45).", type = "error")
      return()
    }
    
    weekday_levels <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
    if (!is.null(rv$data) && "weekday" %in% names(rv$data) && is.factor(rv$data$weekday)) {
      weekday_levels <- levels(rv$data$weekday)
    }
    
    rv_rush$predict_df <- gen_newdata(
      rush = rush_vec,
      day  = input$rush_date,
      val  = 0,
      mom  = 0,
      xmas = 0,
      weekday = weekday_levels
    )
    
    # ---- PREDICT + (GLM) 95% CI ----
    tryCatch({
      
      if (inherits(mod1fit, "glm")) {
        
        # GLM Poisson: link-scale prediction with SE
        pred <- predict(
          mod1fit,
          newdata = rv_rush$predict_df,
          type = "link",
          se.fit = TRUE
        )
        
        fit_link <- as.numeric(pred$fit)
        se_link  <- as.numeric(pred$se.fit)
        
        rv_rush$predict_df$pred_deliveries <- exp(fit_link)
        rv_rush$predict_df$lower_ci <- exp(fit_link - 1.96 * se_link)
        rv_rush$predict_df$upper_ci <- exp(fit_link + 1.96 * se_link)
        
      } else if (inherits(mod1fit, "randomForest")) {
        
        yhat <- as.numeric(predict(mod1fit, newdata = rv_rush$predict_df))
        rv_rush$predict_df$pred_deliveries <- yhat
        rv_rush$predict_df$lower_ci <- NA_real_
        rv_rush$predict_df$upper_ci <- NA_real_
        
      } else {
        
        yhat <- as.numeric(predict(mod1fit, newdata = rv_rush$predict_df))
        rv_rush$predict_df$pred_deliveries <- yhat
        rv_rush$predict_df$lower_ci <- NA_real_
        rv_rush$predict_df$upper_ci <- NA_real_
      }
      
    }, error = function(e) {
      showNotification(paste("Predict fout:", e$message), type = "error", duration = 8)
      rv_rush$predict_df <- NULL
      return(NULL)
    })
    
    req(!is.null(rv_rush$predict_df))
    req("pred_deliveries" %in% names(rv_rush$predict_df))
    rv_rush$predict_df
  })
  
  # output$predict_input_preview <- renderDT({
  #   req(logged_in(), rv_rush$predict_df)
  #   datatable(
  #     rv_rush$predict_df,
  #     rownames = FALSE,
  #     options  = list(pageLength = 25, scrollX = TRUE, dom = "tip")
  #   )
  # })
  # 
  # ---- Plotly: rush input bars + prediction line + CI ribbon ----
  output$rush_pred_plot <- renderPlotly({
    req(logged_in())
    req(rv_rush$predict_df)
    
    df <- rv_rush$predict_df
    req(all(c("t", "rush", "pred_deliveries") %in% names(df)))
    
    df$t <- as.integer(df$t)
    df$rush <- as.integer(df$rush)
    df$pred_deliveries <- as.numeric(df$pred_deliveries)
    
    has_ci <- all(c("lower_ci", "upper_ci") %in% names(df)) &&
      any(is.finite(df$lower_ci)) && any(is.finite(df$upper_ci))
    
    df <- df[order(df$t), ]
    shiny::validate(shiny::need(nrow(df) > 0, "Nog geen voorspelling. Klik eerst op 'Maak voorspelling'."))
    
    p <- plot_ly(df, x = ~t)
    
    # 1) Input rush (0/1) as bars on y2
    # p <- p %>%
    #   add_bars(
    #     y = ~rush,
    #     name = "Rush (input 0/1)",
    #     yaxis = "y2",
    #     opacity = 0.35,
    #     hoverinfo = "text",
    #     text = ~paste0("t = ", t, "<br>rush = ", rush)
    #   )
    
    # 2) CI ribbon (if available)
    if (has_ci) {
      df$lower_ci <- as.numeric(df$lower_ci)
      df$upper_ci <- as.numeric(df$upper_ci)
      
      p <- p %>%
        add_ribbons(
          ymin = ~lower_ci,
          ymax = ~upper_ci,
          name = "95% CI (GLM)",
          hoverinfo = "skip"
        )
    }
    
    # 3) Point prediction line
    p <- p %>%
      add_lines(
        y = ~pred_deliveries,
        name = "Puntvoorspelling",
        mode = "lines+markers",
        hoverinfo = "text",
        text = ~paste0(
          "t = ", t,
          "<br>pred = ", round(pred_deliveries, 2),
          if (has_ci) paste0("<br>95% CI: [", round(lower_ci, 2), ", ", round(upper_ci, 2), "]") else ""
        )
      )
    
    p %>%
      layout(
        #barmode = "overlay",
        xaxis = list(
          title = "Kwartier index (t = 0..19)",
          tickmode = "linear",
          dtick = 1
        ),
        yaxis = list(title = "Voorspelde deliveries"),
        # yaxis2 = list(
        #   title = "Rush (0/1)",
        #   overlaying = "y",
        #   side = "right",
        #   range = c(-0.05, 1.05),
        #   showgrid = FALSE
        # ),
        margin = list(l = 60, r = 60, b = 60, t = 30),
        legend = list(orientation = "h", x = 0, y = -0.15)
      )
  })
  
  # =============================================================================
  # CHAT open/close state
  # =============================================================================
  chatbot_open <- reactiveVal(0)
  output$chatbot_open <- renderText(chatbot_open())
  outputOptions(output, "chatbot_open", suspendWhenHidden = FALSE)
  
  # =============================================================================
  # MAIN UI RENDER
  # =============================================================================
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
            tabName = "rush",
            
            fluidRow(
              box(
                width = 4,
                title = "Dag + rush-profiel",
                status = "success",
                solidHeader = TRUE,
                
                dateInput("rush_date", "Dag", value = Sys.Date(), format = "yyyy-mm-dd"),
                tags$div(style="display:flex; gap:8px; flex-wrap:wrap;",
                         actionButton("rush_reset", "Reset (alles 0)", icon = icon("rotate-left")),
                         actionButton("rush_fill_1", "Alles 1", icon = icon("check")),
                         actionButton("rush_make_features", "Maak voorspelling", icon = icon("wand-magic-sparkles"))
                ),
                tags$hr()
              ),
              
              box(
                width = 8,
                title = "07:00–12:00 (per 15 min) — zet rush op 0/1",
                status = "success",
                solidHeader = TRUE,
                DTOutput("rush_table")
              )
            ),
            
            # fluidRow(
            #   box(
            #     width = 12,
            #     title = "Voorspelde deliveries (mod1) — tabel",
            #     status = "info",
            #     solidHeader = TRUE,
            #     DTOutput("predict_input_preview")
            #   )
            # ),
            
            fluidRow(
              box(
                width = 12,
                title = "Voorspelde deliveries (mod1) — grafiek (incl. CI + rush input)",
                status = "success",
                solidHeader = TRUE,
                plotlyOutput("rush_pred_plot", height = "380px")
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
        
        # ---- CHATBOT FAB + PANEL ----
        div(
          id = "chatbot-fab-wrap",
          actionButton("chatbot_toggle", "Advies", class = "btn btn-success")
        ),
        
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
  
  # Sidebar show/hide based on login
  observeEvent(logged_in(), {
    if (!logged_in()) {
      runjs("$('.main-sidebar').hide(); $('.content-wrapper, .right-side').css('margin-left','0');")
    } else {
      runjs("$('.main-sidebar').show(); $('.content-wrapper, .right-side').css('margin-left','230px');")
    }
  }, ignoreInit = FALSE)
  
  # Enter-to-login
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
  
  # =============================================================================
  # DATA TABLE + DAILY PLOT
  # =============================================================================
  output$data_table <- renderDT({
    req(logged_in())
    datatable(
      rv$data,
      options = list(scrollX = TRUE, dom = "ftip"),
      editable = TRUE
    )
  })
  
  # Update date input bounds after login
  observe({
    req(logged_in(), rv$data)
    req("date" %in% names(rv$data))
    
    dmin <- min(rv$data$date, na.rm = TRUE)
    dmax <- max(rv$data$date, na.rm = TRUE)
    updateDateInput(session, "daily_date", min = dmin, max = dmax, value = dmax)
  })
  
  daily_q15 <- reactive({
    req(logged_in(), rv$data, input$daily_date)
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
    
    out$datetime <- as.POSIXct(out$datetime, tz = "Europe/Amsterdam")
    out$deliveries <- as.numeric(out$deliveries)
    out
  })
  
  output$deliveries_q15_plot <- renderPlotly({
    req(logged_in())
    d <- daily_q15()
    shiny::validate(shiny::need(nrow(d) > 0, "Geen data voor deze datum."))
    
    plot_ly(
      data = d,
      x = ~datetime,
      y = ~deliveries,
      type = "scatter",
      mode = "lines+markers",
      text = ~paste0(format(datetime, "%H:%M"), "<br>Deliveries: ", deliveries),
      hoverinfo = "text"
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
  
  # =============================================================================
  # CHATBOT (ELLMEER) — tools + register_tool
  # =============================================================================
  chat <- reactiveValues(messages = list(
    list(
      role = "assistant",
      text = "Hoi, ik ben Tiri (data-assistent). Stel je vraag. Als ik iets uit de data moet halen, zoek ik het eerst op."
    )
  ))
  
  tool_daily_summary <- function(date) {
    req(rv$data)
    
    if (!all(c("date", "t_time", "deliveries") %in% names(rv$data))) {
      return(list(ok = FALSE, error = "Dataset mist kolommen: date/t_time/deliveries"))
    }
    
    d <- as.Date(date)
    x <- rv$data
    x$date <- as.Date(x$date)
    x$t_time <- substr(as.character(x$t_time), 1, 5)
    x <- x[x$date == d, , drop = FALSE]
    
    if (nrow(x) == 0) {
      return(list(ok = TRUE, date = as.character(d), n = 0, message = "Geen rijen voor deze datum."))
    }
    
    x$datetime <- as.POSIXct(
      paste(x$date, x$t_time),
      format = "%Y-%m-%d %H:%M",
      tz = "Europe/Amsterdam"
    )
    x <- x[!is.na(x$datetime), , drop = FALSE]
    
    start <- as.POSIXct(paste(d, "07:00"), tz = "Europe/Amsterdam")
    end   <- as.POSIXct(paste(d, "12:00"), tz = "Europe/Amsterdam")
    x <- x[x$datetime >= start & x$datetime < end, , drop = FALSE]
    
    if (nrow(x) == 0) {
      return(list(ok = TRUE, date = as.character(d), n = 0, message = "Geen data tussen 07:00 en 12:00."))
    }
    
    agg <- aggregate(deliveries ~ datetime, data = x, FUN = sum, na.rm = TRUE)
    
    total <- sum(agg$deliveries, na.rm = TRUE)
    peak_i <- which.max(agg$deliveries)
    peak_time <- format(agg$datetime[peak_i], "%H:%M")
    peak_val  <- as.numeric(agg$deliveries[peak_i])
    
    list(
      ok = TRUE,
      date = as.character(d),
      n_slots = nrow(agg),
      total_deliveries_7_12 = total,
      peak_time = peak_time,
      peak_deliveries = peak_val
    )
  }
  
  tool_rush_prediction <- function() {
    if (is.null(rv_rush$predict_df)) {
      return(list(ok = TRUE, message = "Nog geen rush-voorspelling gemaakt. Klik eerst op 'Maak voorspelling'."))
    }
    df <- rv_rush$predict_df
    keep <- intersect(c("t", "rush", "pred_deliveries", "lower_ci", "upper_ci"), names(df))
    out <- df[, keep, drop = FALSE]
    list(ok = TRUE, n = nrow(out), preview = head(out, 20))
  }
  
  llm <- chat_openai(
    model = "gpt-4.1-mini",
    system_prompt = paste(
      "Je bent Tiri, een data-assistent in een R Shiny dashboard voor deliveries.",
      "Regels:",
      "1) Je mag NOOIT data/waarden verzinnen.",
      "2) Als een vraag gaat over concrete cijfers of patronen op een datum: gebruik daily_summary(date).",
      "3) Als een vraag gaat over rush-voorspelling: gebruik rush_prediction().",
      "4) Vat tool-resultaten kort samen met exacte waarden. Als tool geen data heeft, zeg wat de gebruiker moet klikken/doen."
    )
  )
  
  tooldef_daily_summary <- ellmer::tool(
    fun = tool_daily_summary,
    name = "daily_summary",
    description = "Geeft kengetallen voor deliveries tussen 07:00 en 12:00 op een datum.",
    arguments = list(
      date = type_string("Datum in formaat YYYY-MM-DD")
    )
  )
  
  tooldef_rush_prediction <- ellmer::tool(
    fun = tool_rush_prediction,
    name = "rush_prediction",
    description = "Geeft (preview van) de laatste rush-voorspelling als die is gemaakt."
  )
  
  llm$register_tool(tooldef_daily_summary)
  llm$register_tool(tooldef_rush_prediction)
  
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
    
    user_msg <- input$chatbot_msg
    chat$messages <- append(chat$messages, list(list(role = "user", text = user_msg)))
    updateTextInput(session, "chatbot_msg", value = "")
    
    ctx <- list(
      selected_daily_date = as.character(input$daily_date %||% NA),
      selected_rush_date  = as.character(input$rush_date %||% NA),
      has_rush_prediction = !is.null(rv_rush$predict_df)
    )
    
    reply <- tryCatch({
      prompt <- paste0(
        user_msg,
        "\n\n[Context]\n",
        "daily_date = ", ctx$selected_daily_date, "\n",
        "rush_date = ", ctx$selected_rush_date, "\n",
        "has_rush_prediction = ", ctx$has_rush_prediction, "\n"
      )
      as.character(llm$chat(prompt))
    }, error = function(e) {
      paste("Er ging iets mis bij de AI-call:", e$message)
    })
    
    chat$messages <- append(chat$messages, list(list(role = "assistant", text = reply)))
  })
  
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

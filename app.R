# --- SETUP ---
library(shiny)
library(dplyr)
library(lubridate)
library(shinyjs)

log_file <- "time_log.csv"
balances_file <- "leave_balances.csv"
accrual_date_file <- "last_accrual_date.txt"

# --- PPE LOGIC ---
pay_period_anchor_date <- as.Date("2023-12-31") 
get_current_pay_period <- function(current_date) {
  days_since_anchor <- as.numeric(current_date - pay_period_anchor_date)
  pay_periods_since_anchor <- floor(days_since_anchor / 14)
  pp_start_date <- pay_period_anchor_date + (pay_periods_since_anchor * 14)
  all_days <- seq(pp_start_date, by = "day", length.out = 14)
  workdays <- all_days[!weekdays(all_days) %in% c("Saturday", "Sunday")]
  return(workdays)
}

# --- UI ---
ui <- fluidPage(
  useShinyjs(), 
  titlePanel("Daily Work Hours Tracker"),
  sidebarLayout(
    sidebarPanel(
      h4("Enter Daily Hours"),
      dateInput("date", "Date:", value = Sys.Date()),
      hr(),
      fluidRow(
        column(6, numericInput("RG", "RG (Regular)", 0, min = 0, step = 0.5)),
        column(6, numericInput("RG_T", "RG-T (Telework)", 0, min = 0, step = 0.5)),
        column(6, numericInput("LA", "LA (Annual Leave)", 0, min = 0, step = 0.5)),
        column(6, numericInput("LH", "LH (Holiday)", 0, min = 0, step = 0.5)),
        column(6, numericInput("LN_PF", "LN-PF (Fitness)", 0, min = 0, step = 0.5)),
        column(6, numericInput("LN_BL", "LN-BL (Blood)", 0, min = 0, step = 0.5)),
        column(6, numericInput("OH", "OH (Overhead)", 0, min = 0, step = 0.5)),
        column(6, numericInput("OU", "OU (Overtime)", 0, min = 0, step = 0.5)),
        column(6, numericInput("CD", "CD (Credit Earned)", 0, min = 0, step = 0.5)),
        column(6, numericInput("CN", "CN (Credit Taken)", 0, min = 0, step = 0.5)),
        column(6, numericInput("CE", "CE (Comp Earned)", 0, min = 0, step = 0.5)),
        column(6, numericInput("CT", "CT (Comp Taken)", 0, min = 0, step = 0.5))
      ),
      textAreaInput("description", "Description of Work:", "", rows = 2),
      actionButton("save", "Save Entry", class = "btn-primary")
    ),
    mainPanel(
      h3("Leave Balances"),
      tableOutput("balance_table"),
      hr(),
      h3("Current Pay Period Status"),
      tableOutput("pay_period_table")
    )
  )
)

# --- SERVER  ---
server <- function(input, output, session) {
  
  required_files <- c(log_file, balances_file, accrual_date_file)
  files_exist <- sapply(required_files, file.exists)
  
  if (!all(files_exist)) {
    missing_files <- paste(required_files[!files_exist], collapse = ", ")
    error_msg <- paste("Error: Missing required data file(s). Please create the following file(s) in your app directory:", missing_files)
    stopApp(error_msg) 
  }
  
  log_col_types <- c(
    Date = "Date", RG = "numeric", RG_T = "numeric", LA = "numeric",
    CN = "numeric", CT = "numeric", CE = "numeric", CD = "numeric",
    OU = "numeric", LH = "numeric", OH = "numeric", LN_PF = "numeric",
    LN_BL = "numeric", Total_Hours = "numeric", Description = "character"
  )
  
  temp_log <- read.csv(log_file, colClasses = log_col_types)
  temp_log$Description <- as.character(temp_log$Description)
  
  inputs_to_reset <- c(
    "RG", "RG_T", "LA", "LH", "LN_PF", "LN_BL", "OH", "OU",
    "CD", "CN", "CE", "CT", "description"
  )
  
  rv <- reactiveValues(
    balances = read.csv(balances_file),
    last_accrual_date = as.Date(readLines(accrual_date_file)),
    log = temp_log
  )
  
  observe({
    last_check_date <- rv$last_accrual_date
    current_pp_start <- get_current_pay_period(Sys.Date())[1]
    
    if (current_pp_start > last_check_date) {
      rv$balances <- rv$balances %>%
        mutate(
          Hours = case_when(
            Category == "Annual" ~ Hours + 4.0,
            Category == "Sick"   ~ Hours + 4.0,
            TRUE ~ Hours
          )
        )
      rv$last_accrual_date <- current_pp_start
      
      write.csv(rv$balances, balances_file, row.names = FALSE)
      writeLines(as.character(rv$last_accrual_date), accrual_date_file)
      
      showNotification("Leave accrued for the new pay period!", type = "message")
    }
  })
  
  output$balance_table <- renderTable({ rv$balances })
  output$pay_period_table <- renderTable({
    pay_period_dates <- get_current_pay_period(Sys.Date())
    data.frame(
      Date = format(pay_period_dates, "%a, %Y-%m-%d"),
      Status = ifelse(pay_period_dates %in% rv$log$Date, "✔️ Logged", "–")
    )
  })
  
  save_entry <- function() {
    old_entry <- filter(rv$log, Date == input$date)
    la_delta <- if(nrow(old_entry) > 0) old_entry$LA else 0
    cn_delta <- if(nrow(old_entry) > 0) old_entry$CN else 0
    ct_delta <- if(nrow(old_entry) > 0) old_entry$CT else 0
    ce_delta <- if(nrow(old_entry) > 0) old_entry$CE else 0
    cd_delta <- if(nrow(old_entry) > 0) old_entry$CD else 0
    ln_pf_delta <- if(nrow(old_entry) > 0) old_entry$LN_PF else 0
    ln_bl_delta <- if(nrow(old_entry) > 0) old_entry$LN_BL else 0
    
    rv$balances <- rv$balances %>%
      mutate(Hours = case_when(
        Category == "Annual"    ~ Hours + la_delta - input$LA,
        Category == "Credit"    ~ Hours + cn_delta - input$CN + input$CD - cd_delta,
        Category == "Comp"      ~ Hours + ct_delta - input$CT + input$CE - ce_delta,
        Category == "LN Used This Year" ~ Hours - (ln_pf_delta + ln_bl_delta) + (input$LN_PF + input$LN_BL),
        TRUE ~ Hours
      ))
    
    new_entry <- data.frame(
      Date = input$date,
      RG = input$RG, RG_T = input$RG_T, LA = input$LA, CN = input$CN, 
      CT = input$CT, CE = input$CE, CD = input$CD, OU = input$OU, 
      LH = input$LH, OH = input$OH, LN_PF = input$LN_PF, LN_BL = input$LN_BL,
      Total_Hours = sum(input$RG, input$RG_T, input$LA, input$CN, input$CT, input$CE, input$CD, input$OU, input$LH, input$OH, input$LN_PF, input$LN_BL),
      Description = input$description
    )
    
    rv$log <- rv$log %>%
      filter(Date != input$date) %>%
      bind_rows(new_entry) %>%
      arrange(Date)
    
    write.csv(rv$log, log_file, row.names = FALSE)
    write.csv(rv$balances, balances_file, row.names = FALSE)
    
    showNotification("Entry saved successfully!", type = "message")
  }
  
  # Save button logic
  observeEvent(input$save, {
    total_hours <- sum(
      input$RG, input$RG_T, input$LA, input$CN, input$CT, input$CE, 
      input$CD, input$OU, input$LH, input$OH, input$LN_PF, input$LN_BL
    )
    if (total_hours < 8) {
      showModal(modalDialog(
        title = "Input Error", "Total hours for the day must be at least 8.", easyClose = TRUE
      ))
      return()
    }
    
    if (input$date %in% rv$log$Date) {
      showModal(modalDialog(
        title = "Date Already Exists",
        paste("An entry for", input$date, "already exists. Do you want to overwrite it?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_overwrite", "Overwrite", class = "btn-warning")
        )
      ))
    } else {
      shinyjs::disable("save")
      save_entry()
      shinyjs::enable("save")
      for (id in inputs_to_reset) {
        shinyjs::reset(id)
      }
    }
  })
  
  observeEvent(input$confirm_overwrite, {
    removeModal()
    shinyjs::disable("save")
    save_entry()
    shinyjs::enable("save")
    for (id in inputs_to_reset) {
      shinyjs::reset(id)
    }
  })
}

# --- RUN APP ---
shinyApp(ui = ui, server = server)
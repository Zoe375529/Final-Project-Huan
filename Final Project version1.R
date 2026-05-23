# =============================================================================
# A Macroeconomic Dashboard for Individual Investors
# Author: Huan Hong
# Prototype Check-In
# =============================================================================
#
# Project description:
#   This Shiny app helps individual investors put macroeconomic warning
#   signals into historical context. For example, when the press warns
#   that the yield curve has inverted, what did the equity market actually
#
#   do over the following 3/6/12/24 months historically? This app shows
#   the answer directly using FRED data.
#
#   All three proposed modules are implemented:
#
#     Module 1: Signal Explorer
#     Module 2: Recession Dashboard
#     Module 3: "What If" Counterfactual Rule Simulator
# =============================================================================


# =============================================================================
# Step 1: Clean environment + load packages
# -----------------------------------------------------------------------------
# Rationale: ensure a clean environment on each launch and load every
# package the app needs. shinyapps.io will NOT inherit packages loaded
# in your local RStudio session, so every package must be library()'d here.
# =============================================================================

# Clear the current R environment (helpful for local testing; harmless at deploy)
rm(list = ls())

# If you don't have these installed locally, uncomment the install line once:
# install.packages(c('shiny', 'shinydashboard', 'fredr', 'dplyr', 'tidyr',
#                    'ggplot2', 'plotly', 'lubridate', 'DT', 'scales', 'purrr'))

library(shiny)            # Shiny core framework
library(shinydashboard)   # Dashboard layout (header + sidebar + tabs)
library(fredr)            # FRED economic-data API client
library(dplyr)            # Data manipulation
library(tidyr)            # Long/wide reshape
library(ggplot2)          # Static plotting
library(plotly)           # Convert ggplot to interactive plots
library(lubridate)        # Date handling
library(DT)               # Interactive data tables
library(scales)           # Axis formatting helpers
library(purrr)            # Functional map() helpers

# -----------------------------------------------------------------------------
# CRITICAL FIX: Force dplyr's lead/lag to take precedence
# -----------------------------------------------------------------------------
# Without this, other packages (plyr, stats) may override lead/lag and cause:
#   "no applicable method for 'lead' applied to an object of class 'numeric'"
# These aliases ensure every lead()/lag() call below uses dplyr's version.
lead <- dplyr::lead
lag  <- dplyr::lag


# =============================================================================
# Step 2: Set the FRED API key (CRITICAL - without this, no charts render)
# -----------------------------------------------------------------------------
# Rationale: fredr requires fredr_set_key() before any API call. Without it,
#            every fetch fails silently and all plots come back empty.
#
# How to get a free key (~1 minute):
#   1. Go to https://fredaccount.stlouisfed.org/login/secure/
#   2. Register an account
#   3. Click 'Request API Key' on the API Keys page
#   4. Copy the 32-character string and paste it below
# =============================================================================

# !!! Replace the string below with YOUR OWN FRED API key !!!
FRED_API_KEY <- "7c4a5e851c099755aaf977607da9424c"

# Actually register the key with the fredr package
fredr_set_key(FRED_API_KEY)


# =============================================================================
# Step 3: Define the indicator catalog (which economic series we track)
# -----------------------------------------------------------------------------
# Rationale: keep every FRED series in a single table. Each row has:
#   id        - FRED series code
#   label     - User-friendly display name
#   group     - Category (Financial Market / Real Economy / Equity / Recession)
#   direction - 'high' = bigger value is more dangerous (e.g. unemployment)
#               'low'  = smaller value is more dangerous (e.g. yield-curve spread)
#               This drives the WARNING/Calm logic in Module 2.
#   freq      - Native frequency (d=daily, m=monthly, q=quarterly)
# =============================================================================

series_catalog <- tibble::tribble(
  ~id,               ~label,                                ~group,             ~direction, ~freq,
  # ---- Financial market indicators ----
  "T10Y2Y",          "10Y - 2Y Treasury Spread",            "Financial Market", "low",      "d",
  "T10Y3M",          "10Y - 3M Treasury Spread",            "Financial Market", "low",      "d",
  "BAA10Y",          "Baa Corporate - 10Y Treasury Spread", "Financial Market", "high",     "d",
  "BAMLH0A0HYM2",    "ICE BofA US High Yield Spread",       "Financial Market", "high",     "d",
  "VIXCLS",          "VIX (Volatility Index)",              "Financial Market", "high",     "d",
  # ---- Real economy indicators ----
  "UNRATE",          "Unemployment Rate (%)",               "Real Economy",     "high",     "m",
  "INDPRO",          "Industrial Production Index",         "Real Economy",     "low",      "m",
  "A191RL1Q225SBEA", "Real GDP Growth (% q/q ann.)",        "Real Economy",     "low",      "q",
  "UMCSENT",         "U. Michigan Consumer Sentiment",      "Real Economy",     "low",      "m",
  # ---- Equity (outcome variable) ----
  # Note: we DON'T use the SP500 series, because under FRED's licensing
  #       agreement with S&P Dow Jones, SP500 only goes back ~10 years.
  #       I originally planned Wilshire 5000 (WILL5000IND) but FRED removed
  #       all Wilshire indices in June 2024 (licensing change). We settled on
  #       NASDAQCOM (NASDAQ Composite): full daily history since 1971,
  "NASDAQCOM",     "U.S. Equity Market (Wilshire 5000)",  "Equity",           "high",     "d",
  # ---- Recession anchor variable ----
  "USREC",           "NBER Recession Indicator",            "Recession",        "high",     "m"
)


# =============================================================================
# Step 4: Safe data-fetching function (with retry/backoff)
# -----------------------------------------------------------------------------
# Rationale: the FRED API occasionally returns 500 errors - these are
#            intermittent server-side hiccups, not bugs in our code. We retry.
#
# Retry strategy (exponential backoff):
#   - 1st failure -> wait 2s and retry
#   - 2nd failure -> wait 4s and retry
#   - 3rd failure -> give up and log a warning
#
# This handles most transient outages without forcing a restart.
# =============================================================================

fetch_series <- function(id, max_attempts = 3) {
  attempt <- 1
  wait_seconds <- 2
  
  while (attempt <= max_attempts) {
    result <- tryCatch({
      fredr(series_id = id, observation_start = as.Date("1960-01-01")) |>
        select(date, value) |>
        mutate(series_id = id)
    }, error = function(e) {
      # Store the error so the while-loop can decide what to do next
      structure(list(error = e$message), class = "fredr_fetch_error")
    })
    
    # Success - return immediately
    if (!inherits(result, "fredr_fetch_error")) {
      if (attempt > 1) message("  -> ", id, " succeeded on attempt ", attempt)
      return(result)
    }
    
    # Failure: if we have retries left, wait and try again
    if (attempt < max_attempts) {
      message("  Attempt ", attempt, " for ", id, " failed (",
              result$error, "), retrying in ", wait_seconds, "s...")
      Sys.sleep(wait_seconds)
      wait_seconds <- wait_seconds * 2  # exponential backoff
      attempt <- attempt + 1
    } else {
      # All retries exhausted - log final warning and return empty tibble
      message("Could not fetch ", id, " after ", max_attempts,
              " attempts: ", result$error)
      return(tibble(date = as.Date(character()), value = numeric(),
                    series_id = character()))
    }
  }
}


# =============================================================================
# Step 5: Fetch every series ONCE at startup
# -----------------------------------------------------------------------------
# Rationale: FRED updates at most once a day, so we cache everything in
#            memory at launch. UI switches between indicators are then instant.
# =============================================================================

message("Fetching FRED series...")
# 0.3s pause between requests to avoid hitting FRED's rate limit
raw_data <- lapply(series_catalog$id, function(id) {
  result <- fetch_series(id)
  Sys.sleep(0.3)
  result
})
names(raw_data) <- series_catalog$id

# After startup, log how many series loaded successfully
.fetch_summary <- sapply(raw_data, nrow)
message("Fetch summary: ", sum(.fetch_summary > 0), "/",
        length(.fetch_summary), " series loaded successfully")
if (any(.fetch_summary == 0)) {
  message("  Failed series: ",
          paste(names(.fetch_summary)[.fetch_summary == 0], collapse = ", "))
}

# Convenience accessor: drop NAs and sort by date
get_series <- function(id) {
  raw_data[[id]] |>
    filter(!is.na(value)) |>
    arrange(date)
}


# =============================================================================
# Step 6: Align any-frequency series to monthly
# -----------------------------------------------------------------------------
# Rationale: FRED has daily (VIX), monthly (unemployment), and quarterly
#            (GDP) series. To compare them in the same chart/table we unify
#            to monthly frequency, taking each month's last observation.
# =============================================================================

to_monthly <- function(df) {
  df |>
    mutate(month = floor_date(date, "month")) |>
    group_by(month) |>
    summarise(value = last(value), .groups = "drop") |>
    rename(date = month)
}


# =============================================================================
# Step 7: Pre-compute equity-market forward returns (core of Module 1)
# -----------------------------------------------------------------------------
# Rationale: Module 1 answers 'after a signal triggers, how did the equity
#            market do over the next N months?' Pre-compute +3M / +6M / +12M
#            / +24M returns; later code just joins them in by trigger date.
# =============================================================================

sp500_monthly <- get_series("NASDAQCOM") |>
  to_monthly() |>
  arrange(date) |>
  mutate(
    ret_3m  = (dplyr::lead(value, 3)  / value - 1) * 100,
    ret_6m  = (dplyr::lead(value, 6)  / value - 1) * 100,
    ret_12m = (dplyr::lead(value, 12) / value - 1) * 100,
    ret_24m = (dplyr::lead(value, 24) / value - 1) * 100
  )


# =============================================================================
# Step 8: Extract historical recession periods (for shading time-series plots)
# -----------------------------------------------------------------------------
# Rationale: USREC is a 0/1 indicator - 1 means the month is in an NBER
#            recession, 0 means expansion. We convert the 0/1 series into
#            (start_date, end_date) ranges:
#               start = month is 1 and previous month is 0
# =============================================================================

recession_monthly <- get_series("USREC") |> to_monthly()

# Recession start months
rec_starts <- recession_monthly |>
  arrange(date) |>
  mutate(starts = value == 1 & dplyr::lag(value, default = 0) == 0) |>
  filter(starts) |>
  pull(date)

# Recession end months
rec_ends <- recession_monthly |>
  arrange(date) |>
  mutate(ends = value == 1 & dplyr::lead(value, default = 0) == 0) |>
  filter(ends) |>
  pull(date)

# Pair starts and ends into a range table
n_eps <- min(length(rec_starts), length(rec_ends))
recession_periods <- tibble(
  start = rec_starts[seq_len(n_eps)],
  end   = rec_ends[seq_len(n_eps)]
)


# =============================================================================
# Step 9: Professional finance-dashboard color palette
# -----------------------------------------------------------------------------
# Rationale: a polished finance visualization rests on three things:
#   1) muted, low-saturation colors instead of pure RGB
#   2) restrained primary (deep navy / slate) with ONE accent color
#   3) generous whitespace and consistent low-contrast gridlines
#
# Below: deep-navy + matte-gold scheme (Bloomberg / FT style)
# =============================================================================

# Primary palette
COL_BG       <- "#F7F8FA"   # Page background: very light cool grey
COL_PANEL    <- "#FFFFFF"   # Card background: pure white
COL_PRIMARY  <- "#1F3A5F"   # Primary: deep navy
COL_PRIMARY2 <- "#2C5282"   # Primary (lighter): secondary elements
COL_ACCENT   <- "#C9A961"   # Accent: matte gold
COL_TEXT     <- "#2C3E50"   # Body text: dark slate
COL_MUTED    <- "#8492A6"   # Secondary text: cool grey
COL_GRID     <- "#ECEFF3"   # Gridlines: very light cool grey

# Data-visualization palette
COL_LINE     <- "#1F3A5F"   # Time-series main line
COL_THRESH   <- "#C9A961"   # Threshold reference (gold dashed)
COL_RECESS   <- "#C8CFD9"   # Recession shading (soft cool grey)

# Up/down accent colors (muted)
COL_GOOD     <- "#5B8C5A"   # Calm green
COL_BAD      <- "#A4453E"   # Brick red
COL_NEUTRAL  <- "#F8F4EC"   # Neutral cream (heat-map midpoint)

# Status badge colors (used in Module 2)
STATUS_BG <- c(
  "Calm"    = "#E5EFE5",
  "Watch"   = "#FAF1E0",
  "WARNING" = "#F2DDD9",
  "no data" = "#ECEFF3"
)
STATUS_FG <- c(
  "Calm"    = "#3D6B4D",
  "Watch"   = "#8B6914",
  "WARNING" = "#8B2C28",
  "no data" = "#7F8C8D"
)


# =============================================================================
# Step 10: Shared ggplot theme
# -----------------------------------------------------------------------------
# Rationale: every chart uses the same theme so the app's visual language
#            stays consistent. Key change vs default: white panel + faint grid.
# =============================================================================

theme_finance <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      text             = element_text(color = COL_TEXT),
      plot.title       = element_text(color = COL_PRIMARY, size = base_size + 1,
                                      face = "bold", margin = margin(b = 8)),
      axis.title       = element_text(color = COL_MUTED,   size = base_size - 1),
      axis.text        = element_text(color = COL_TEXT,    size = base_size - 2),
      panel.background = element_rect(fill = COL_PANEL, color = NA),
      plot.background  = element_rect(fill = COL_PANEL, color = NA),
      panel.grid.major.y = element_line(color = COL_GRID, linewidth = 0.4),
      panel.grid.major.x = element_line(color = COL_GRID, linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      legend.position    = "bottom",
      legend.title       = element_text(color = COL_MUTED, size = base_size - 2),
      legend.text        = element_text(color = COL_TEXT,  size = base_size - 2),
      legend.background  = element_rect(fill = COL_PANEL, color = NA),
      plot.margin        = margin(10, 14, 10, 14)
    )
}

# Helper to add recession shading to any plot
# NOTE: geom_rect(ymin=-Inf, ymax=Inf) is unreliable when ggplotly() converts
#        to plotly. So the caller passes a concrete y-range and we draw the
#        rectangle with explicit numbers - this renders reliably.
add_recession_shading <- function(p, periods = recession_periods,
                                  y_min = NULL, y_max = NULL) {
  if (nrow(periods) == 0) return(p)
  
  # If no y-range is passed, fall back to a very large range that should
  # cover any plausible data (plotly dislikes Inf, so we use big numbers)
  if (is.null(y_min)) y_min <- -1e6
  if (is.null(y_max)) y_max <-  1e6
  
  p + geom_rect(
    data = periods,
    aes(xmin = start, xmax = end, ymin = y_min, ymax = y_max),
    fill = COL_RECESS, alpha = 0.55, inherit.aes = FALSE
  )
}


# =============================================================================
# Step 11: Indicator-dropdown choices for Module 1 (exclude equity & USREC)
# -----------------------------------------------------------------------------
# Rationale: in Signal Explorer the user picks which indicator to use as a
#            signal. The equity series is the OUTCOME and USREC is recession
# =============================================================================

explorer_indicators <- series_catalog |>
  filter(!id %in% c("NASDAQCOM", "USREC"))


# =============================================================================
# Step 12: Custom CSS string (defined standalone to avoid sprintf issues)
# -----------------------------------------------------------------------------
# Rationale: assemble the CSS via paste0(), interpolating color variables
#            directly. Avoiding sprintf prevents %% escaping headaches.
# =============================================================================

custom_css <- paste0("
/* ---- Body background ---- */
body, .content-wrapper, .right-side {
  background-color: ", COL_BG, ";
  font-family: 'Helvetica Neue', Arial, sans-serif;
}
/* ---- Header ---- */
.skin-black .main-header .logo {
  background-color: ", COL_PRIMARY, ";
  color: #FFFFFF;
  font-weight: 600;
  letter-spacing: 0.4px;
  border-bottom: 2px solid ", COL_ACCENT, ";
}
.skin-black .main-header .logo:hover { background-color: ", COL_PRIMARY2, "; }
.skin-black .main-header .navbar { background-color: ", COL_PRIMARY, "; }
.skin-black .main-header .navbar .sidebar-toggle { color: #FFFFFF; }
.skin-black .main-header .navbar .sidebar-toggle:hover {
  background-color: ", COL_PRIMARY2, "; color: ", COL_ACCENT, ";
}
/* ---- Sidebar ---- */
.skin-black .main-sidebar { background-color: #1A2F4A; }
.skin-black .sidebar-menu > li > a {
  color: #B8C4CC; padding: 14px 20px; font-size: 14px;
  border-left: 3px solid transparent;
}
.skin-black .sidebar-menu > li.active > a,
.skin-black .sidebar-menu > li:hover > a {
  background-color: ", COL_PRIMARY, "; color: ", COL_ACCENT, ";
  border-left-color: ", COL_ACCENT, ";
}
/* ---- Box cards ---- */
.box {
  border-top: 3px solid ", COL_ACCENT, ";
  border-radius: 4px;
  box-shadow: 0 2px 6px rgba(0,0,0,0.05);
  margin-bottom: 18px;
}
.box-primary { border-top-color: ", COL_PRIMARY, "; }
.box.box-solid.box-primary > .box-header {
  background-color: ", COL_PRIMARY, "; color: #FFFFFF;
}
.box-info { border-top-color: ", COL_TEXT, "; }
.box.box-solid.box-info > .box-header {
  background-color: ", COL_TEXT, "; color: #FFFFFF;
}
.box-success { border-top-color: ", COL_ACCENT, "; }
.box.box-solid.box-success > .box-header {
  background-color: #A88E51; color: #FFFFFF;
}
.box-title {
  font-weight: 600; font-size: 14px; letter-spacing: 0.3px;
}
/* ---- chart-help: how-to-read box ---- */
.chart-help {
  background-color: #FAF7EE;
  border-left: 3px solid ", COL_ACCENT, ";
  padding: 11px 15px;
  margin-bottom: 14px;
  font-size: 13px;
  color: ", COL_TEXT, ";
  line-height: 1.55;
  border-radius: 2px;
}
.chart-help strong { color: ", COL_PRIMARY, "; }
/* ---- Input controls ---- */
.form-control { border-radius: 3px; border-color: #D5DBDF; }
.form-control:focus {
  border-color: ", COL_PRIMARY, "; box-shadow: 0 0 0 2px rgba(31,58,95,0.1);
}
/* Slider */
.irs--shiny .irs-bar { background: ", COL_ACCENT, "; border-color: ", COL_ACCENT, "; }
.irs--shiny .irs-from, .irs--shiny .irs-to,
.irs--shiny .irs-single { background: ", COL_PRIMARY, "; }
.irs--shiny .irs-handle { border-color: ", COL_PRIMARY, "; }
/* Radio buttons */
.radio label, .checkbox label { color: ", COL_TEXT, "; font-size: 13px; }
/* ---- Data table ---- */
table.dataTable thead th {
  background-color: ", COL_PRIMARY, " !important;
  color: #FFFFFF !important;
  font-weight: 600 !important;
  font-size: 13px !important;
  letter-spacing: 0.3px !important;
  border-bottom: none !important;
}
table.dataTable tbody tr:hover { background-color: #FAF7EE !important; }
.dataTables_wrapper { font-size: 13px; }
")


# =============================================================================
# Step 13: UI definition (what the page looks like)
# -----------------------------------------------------------------------------
# Rationale: dashboardPage has three parts:
#   - dashboardHeader   : top title bar
#   - dashboardSidebar  : left-hand menu
#   - dashboardBody     : main content area (tabItems holds multiple tabs)
# =============================================================================

ui <- dashboardPage(
  
  skin = "black",
  
  # ---- Header ----
  dashboardHeader(
    title = span("Macro Investor Dashboard", style = "font-weight: 600;"),
    titleWidth = 320
  ),
  
  # ---- Sidebar menu ----
  dashboardSidebar(
    width = 320,
    sidebarMenu(
      id = "tabs",
      menuItem("About",                  tabName = "about",     icon = icon("info-circle")),
      menuItem("1. Signal Explorer",     tabName = "signal",    icon = icon("chart-line")),
      menuItem("2. Recession Dashboard", tabName = "dashboard", icon = icon("gauge")),
      menuItem("3. What If Simulator",   tabName = "whatif",    icon = icon("flask"))
    )
  ),
  
  # ---- Main body ----
  dashboardBody(
    
    # Inject the custom CSS
    tags$head(tags$style(HTML(custom_css))),
    
    tabItems(
      
      # ===========================================================
      # ABOUT tab
      # ===========================================================
      tabItem(tabName = "about",
              fluidRow(
                box(width = 12, title = "About this Project",
                    status = "primary", solidHeader = TRUE,
                    HTML(paste0("
              <h3 style='color:", COL_PRIMARY, ";'>A Macroeconomic Dashboard for Individual Investors</h3>
              <p style='font-size:14px;'><b>Author:</b> Huan Hong</p>
              <p>This dashboard helps individual investors put macro warning
              signals into historical context. When the press warns that the
              yield curve has inverted or that credit spreads are widening,
              what do the data actually say tends to follow?</p>

              <h4 style='color:", COL_PRIMARY, "; margin-top:22px;'>Indicator groups</h4>
              <ul>
                <li><b>Financial Market:</b> 10Y-2Y &amp; 10Y-3M yield-curve
                spreads, Baa-10Y and high-yield credit spreads, VIX.</li>
                <li><b>Real Economy:</b> unemployment rate, industrial
                production, real GDP growth, U-Mich consumer sentiment.</li>
                <li><b>Equity (outcome):</b> Wilshire 5000 Total Market Index
                (a close proxy for the S&amp;P 500, with daily history back to
                1971).</li>
              </ul>

              <h4 style='color:", COL_PRIMARY, "; margin-top:22px;'>Modules</h4>
              <ul>
                <li><b>Signal Explorer:</b> pick an indicator and a threshold,
                see forward equity-market returns after every historical
                trigger event.</li>
                <li><b>Recession Dashboard:</b> current readings vs typical
                pre-recession readings, with a heat-map summary of each
                indicator's predictive track record.</li>
                <li><b>What If Simulator:</b> design a defensive trading rule
                (e.g. cut equity exposure by 50% when the yield curve inverts)
                and simulate how it would have performed over the past several
                decades vs simple buy-and-hold. Highlights the trade-off between
                avoiding drawdowns and missing recoveries.</li>
              </ul>

              <h4 style='color:", COL_PRIMARY, "; margin-top:22px;'>How to use the Signal Explorer</h4>
              <p>The threshold defines what counts as a 'warning signal' for the
              indicator you've chosen. For example, with the 10Y-2Y Treasury
              spread, a value below 0 means the yield curve is inverted &mdash;
              a classic recession warning. Set the threshold to <code>0</code>
              and the direction to <em>Below</em>, and the app finds every
              historical month where the curve was inverted and shows what the
              equity market did over the next 3 / 6 / 12 / 24 months.</p>
              <table style='border-collapse: collapse; font-size: 13px; margin-top: 8px;'>
                <thead>
                  <tr style='background-color: ", COL_PRIMARY, "; color: white;'>
                    <th style='padding: 6px 12px; text-align: left;'>Indicator</th>
                    <th style='padding: 6px 12px; text-align: left;'>Direction</th>
                    <th style='padding: 6px 12px; text-align: left;'>Typical threshold</th>
                    <th style='padding: 6px 12px; text-align: left;'>What it means</th>
                  </tr>
                </thead>
                <tbody>
                  <tr><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>10Y-2Y / 10Y-3M Spread</td><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>Below</td><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>0</td><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>Yield curve inverted</td></tr>
                  <tr><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>Baa-10Y Spread</td><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>Above</td><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>3</td><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>Investment-grade credit stress</td></tr>
                  <tr><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>HY Spread</td><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>Above</td><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>6</td><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>High-yield risk-off</td></tr>
                  <tr><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>VIX</td><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>Above</td><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>25</td><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>Market fear</td></tr>
                  <tr><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>Unemployment Rate</td><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>Above</td><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>5</td><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>Labor market weakness</td></tr>
                  <tr><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>U-Mich Consumer Sentiment</td><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>Below</td><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>75</td><td style='padding: 6px 12px; border-bottom: 1px solid #ECEFF3;'>Consumer pessimism</td></tr>
                </tbody>
              </table>
              <p style='margin-top:14px;'><strong>Tip:</strong> stricter thresholds
              find fewer historical events but produce stronger signals; looser
              thresholds find many events but more of them are false alarms.
              The 'Min consecutive months' filter further removes short-lived
              noise &mdash; e.g. requiring 3+ months excludes single-month spikes
              that quickly reversed.</p>

              <p style='margin-top:22px;'><b>Data:</b> FRED via the
              <code>fredr</code> package.</p>
            "))
                )
              )
      ),
      
      # ===========================================================
      # MODULE 1: Signal Explorer
      # ===========================================================
      tabItem(tabName = "signal",
              
              # ---- Row 1: parameter panel + time-series chart ----
              fluidRow(
                box(width = 4, title = "Configure Signal",
                    status = "primary", solidHeader = TRUE,
                    selectInput("sig_indicator", "Indicator:",
                                choices = setNames(
                                  explorer_indicators$id,
                                  paste0("[", explorer_indicators$group, "] ",
                                         explorer_indicators$label)
                                ),
                                selected = "T10Y2Y"),
                    radioButtons("sig_direction", "Trigger when value is:",
                                 choices = c("Below threshold" = "below",
                                             "Above threshold" = "above"),
                                 selected = "below"),
                    numericInput("sig_threshold", "Threshold value:",
                                 value = 0, step = 0.1),
                    numericInput("sig_min_months",
                                 "Min consecutive months condition holds:",
                                 value = 1, min = 1, max = 24, step = 1),
                    helpText("Example: T10Y2Y, below 0, for 3+ months finds every yield-curve inversion."),
                    br(),
                    div(style = "background-color: #FAF7EE; border-left: 3px solid #C9A961;
                         padding: 10px 12px; margin-top: 8px; font-size: 12px;
                         color: #2C3E50; line-height: 1.5; border-radius: 2px;",
                        HTML("<strong style='color: #1F3A5F;'>How threshold works:</strong>
                You define what counts as a 'warning signal' for the chosen
                indicator. The app then finds every historical month where the
                indicator crossed that line and shows what the equity market did
                next.<br><br>
                <strong style='color: #1F3A5F;'>Common rules of thumb:</strong>
                <ul style='margin: 4px 0 0 -18px; padding: 0;'>
                  <li>Yield curve (T10Y2Y / T10Y3M): <em>below 0</em> = inversion</li>
                  <li>Credit spread (BAA10Y): <em>above 3</em> = wide</li>
                  <li>HY spread: <em>above 6</em> = stress</li>
                  <li>VIX: <em>above 25</em> = market fear</li>
                  <li>Unemployment: <em>above 5</em> = labor weakness</li>
                  <li>Consumer sentiment: <em>below 75</em> = consumer pessimism</li>
                </ul>
                <br>
                <strong style='color: #1F3A5F;'>Tip:</strong> stricter thresholds
                give fewer events but stronger signals; loose thresholds give
                many events but more noise."
                        )
                    )
                ),
                box(width = 8, title = "Indicator Over Time",
                    status = "info", solidHeader = TRUE,
                    div(class = "chart-help", HTML(
                      "<strong>How to read:</strong> The <strong>dark blue line</strong>
               is the indicator's history. The <strong>vertical grey bands</strong>
               are NBER recessions (e.g. 1981, 1990, 2001, 2008, 2020 - they show
               up as light grey columns spanning the chart vertically). The
               <strong>gold dashed line</strong> is your threshold. Every time
               the indicator crosses the threshold in your chosen direction for
               at least your minimum number of months, that's a 'trigger event'
               listed in the table at the bottom of this tab."
                    )),
                    plotlyOutput("sig_timeseries", height = "320px")
                )
              ),
              
              # ---- Row 2: scatter + distribution ----
              fluidRow(
                box(width = 6, title = "Signal Strength vs Forward Return",
                    status = "success", solidHeader = TRUE,
                    div(class = "chart-help", HTML(
                      "<strong>How to read:</strong> Each dot is one historical trigger event.
               <strong>X-axis</strong> = how extreme the signal was during that event
               (its average value). <strong>Y-axis</strong> = the S&amp;P 500's return
               over the chosen horizon afterwards. The
               <strong style='color:#C9A961;'>gold line</strong> is a linear fit.
               If it slopes downward, stronger signals tended to be followed by
               worse market outcomes."
                    )),
                    selectInput("sig_scatter_horizon", "Horizon:",
                                choices = c("3 months"  = "ret_3m",
                                            "6 months"  = "ret_6m",
                                            "12 months" = "ret_12m",
                                            "24 months" = "ret_24m"),
                                selected = "ret_12m"),
                    plotlyOutput("sig_scatter", height = "320px")
                ),
                box(width = 6, title = "Distribution of Post-Trigger Returns",
                    status = "success", solidHeader = TRUE,
                    div(class = "chart-help", HTML(
                      "<strong>How to read:</strong> Each box shows the spread of S&amp;P 500
               returns after every historical trigger, for that horizon.
               The <strong>line inside the box</strong> is the median; the box covers
               the middle 50% of cases. <strong>If the box sits below zero</strong>,
               the market <em>usually</em> fell after this signal. A
               <strong>wide box</strong> means historical outcomes varied a lot,
               i.e. the signal is noisy."
                    )),
                    plotlyOutput("sig_distribution", height = "350px")
                )
              ),
              
              # ---- Row 3: trigger events table ----
              fluidRow(
                box(width = 12, title = "Trigger Events Table",
                    status = "success", solidHeader = TRUE,
                    div(class = "chart-help", HTML(
                      "<strong>How to read:</strong> Each row is one historical event matching
               your settings.
               <span style='color:#5B8C5A;font-weight:600;'>Green numbers</span>
               = the S&amp;P 500 was up that many percent after the trigger;
               <span style='color:#A4453E;font-weight:600;'>red</span> = down."
                    )),
                    DTOutput("sig_table")
                )
              )
      ),
      
      # ===========================================================
      # MODULE 2: Recession Dashboard
      # ===========================================================
      tabItem(tabName = "dashboard",
              
              # ---- Row 1: status table (current vs pre-recession) ----
              fluidRow(
                box(width = 12, title = "Current Readings vs Typical Pre-Recession Levels",
                    status = "primary", solidHeader = TRUE,
                    div(class = "chart-help", HTML(
                      "<strong>How to read this table:</strong>
               <em>Current</em> = the latest reading. <em>Pre-recession avg</em>
               = the average reading in the months leading up to past NBER recessions
               (window adjustable below). <em>Status</em> compares the two:
               <span style='background:#E5EFE5;color:#3D6B4D;padding:1px 8px;border-radius:3px;font-weight:600;'>Calm</span>
               = far from the danger zone,
               <span style='background:#FAF1E0;color:#8B6914;padding:1px 8px;border-radius:3px;font-weight:600;'>Watch</span>
               = approaching pre-recession typical level,
               <span style='background:#F2DDD9;color:#8B2C28;padding:1px 8px;border-radius:3px;font-weight:600;'>WARNING</span>
               = at or past it."
                    )),
                    sliderInput("dash_lookback", "Pre-recession window (months):",
                                min = 1, max = 12, value = 6, step = 1, width = "400px"),
                    DTOutput("dash_table")
                )
              ),
              
              # ---- Composite recession-pressure index (new - option D) ----
              fluidRow(
                box(width = 12, title = "Composite Recession Pressure Index",
                    status = "primary", solidHeader = TRUE,
                    div(class = "chart-help", HTML(
                      "<strong>How to read this chart:</strong> A single composite index
               that summarizes how stressed the macro-financial system is
               <em>right now</em> compared to history. We standardize each of
               the 9 indicators (z-score) so they're comparable, flip the
               'good when low' ones, and average them. Higher = more
               recession-like conditions.
               <strong>Vertical grey bands</strong> are NBER recessions.
               The <strong>gold dot</strong> is today.
               Use this for the question: <em>'Is the current macro picture
               more like 2008, more like 2019, or more like a normal year?'</em>"
                    )),
                    plotlyOutput("dash_pressure_index", height = "360px")
                )
              ),
              
              # ---- Indicator snapshot panel grid (new - option B) ----
              fluidRow(
                box(width = 12, title = "Indicator Snapshot Panel",
                    status = "primary", solidHeader = TRUE,
                    div(class = "chart-help", HTML(
                      "<strong>How to read these panels:</strong> One small chart per
               indicator showing the past 10 years.
               The <strong>dark blue line</strong> is the indicator's trajectory.
               The <strong>red dashed line</strong> is the typical pre-recession
               level (averaged across past NBER recessions).
               The <strong>gold dot</strong> is the current reading.
               This lets you see at a glance not just <em>where</em> each
               indicator is, but also <em>which direction it's moving</em>
               toward or away from the danger zone."
                    )),
                    plotOutput("dash_snapshot_panels", height = "550px")
                )
              ),
              
              # ---- Heat map ----
              fluidRow(
                box(width = 12, title = "Predictive Heat Map",
                    status = "info", solidHeader = TRUE,
                    div(class = "chart-help", HTML(
                      "<strong>How to read this heat map:</strong> Each <strong>row</strong>
               is one indicator with a default trigger rule (e.g. yield curve &lt; 0).
               Each <strong>column</strong> is a forward time horizon. The cell
               shows the average S&amp;P 500 return across all historical events
               where that indicator triggered.
               <span style='color:#A4453E;font-weight:600;'>Red</span>
               = the market averaged a negative return after this signal,
               <span style='color:#5B8C5A;font-weight:600;'>green</span> = positive.
               Use this to compare which signals have the strongest historical
               track record at which horizons."
                    )),
                    plotlyOutput("dash_heatmap", height = "440px")
                )
              )
      ),
      
      # ===========================================================
      # MODULE 3: What If Counterfactual Rule Simulator
      # ===========================================================
      tabItem(tabName = "whatif",
              
              # ---- Row 1: rule configuration panel + portfolio chart ----
              fluidRow(
                box(width = 4, title = "Design Your Defensive Rule",
                    status = "primary", solidHeader = TRUE,
                    selectInput("wi_indicator", "Signal indicator:",
                                choices = setNames(
                                  explorer_indicators$id,
                                  paste0("[", explorer_indicators$group, "] ",
                                         explorer_indicators$label)
                                ),
                                selected = "T10Y2Y"),
                    radioButtons("wi_direction", "Trigger when value is:",
                                 choices = c("Below threshold" = "below",
                                             "Above threshold" = "above"),
                                 selected = "below"),
                    numericInput("wi_threshold", "Threshold value:",
                                 value = 0, step = 0.1),
                    sliderInput("wi_reduce_pct",
                                "Reduce equity exposure to (% invested when signal is ON):",
                                min = 0, max = 100, value = 50, step = 10,
                                post = "%"),
                    sliderInput("wi_hold_months",
                                "Hold defensive position for (months after signal turns off):",
                                min = 0, max = 24, value = 6, step = 1),
                    numericInput("wi_start_year", "Backtest start year:",
                                 value = 1980, min = 1971, max = 2020, step = 1),
                    helpText("Cash earns 0% in this simplified simulation. The benchmark is 100% buy-and-hold.")
                ),
                box(width = 8, title = "Portfolio Value Over Time",
                    status = "info", solidHeader = TRUE,
                    div(class = "chart-help", HTML(
                      "<strong>How to read:</strong> Both lines start at $100 in your
                chosen start year.
                The <strong style='color:#1F3A5F;'>dark blue line</strong> is the
                buy-and-hold benchmark (100% invested in equities the entire
                time).
                The <strong style='color:#C9A961;'>gold line</strong> is your
                defensive rule: it pulls money out of equities whenever your
                signal is triggered and waits the chosen hold period before
                going back in.
                The <strong>grey bands</strong> are NBER recessions.
                If the gold line ends above the blue line, your rule beat
                buy-and-hold; if it ends below, the false alarms cost you
                more than the avoided drawdowns saved you."
                    )),
                    plotlyOutput("wi_portfolio_chart", height = "350px")
                )
              ),
              
              # ---- Row 2: summary stats ----
              fluidRow(
                box(width = 12, title = "Summary Statistics",
                    status = "success", solidHeader = TRUE,
                    div(class = "chart-help", HTML(
                      "<strong>How to read:</strong> The numbers below compare your
                rule against buy-and-hold over the chosen backtest window.
                <em>Total return</em> is the cumulative percentage growth.
                <em>Annualized return</em> annualizes that to a per-year rate.
                <em>Max drawdown</em> is the worst peak-to-trough decline
                (smaller is better &mdash; less painful to live through).
                <em>Months out of market</em> shows how often the rule had
                you on the sidelines."
                    )),
                    DTOutput("wi_stats_table")
                )
              ),
              
              # ---- Row 3: signal-on periods visualization ----
              fluidRow(
                box(width = 12, title = "When Was the Rule Defensive?",
                    status = "info", solidHeader = TRUE,
                    div(class = "chart-help", HTML(
                      "<strong>How to read:</strong> Shows the indicator's history
                over the backtest period. The <strong>gold dashed line</strong>
                is your threshold. <strong>Pink shaded regions</strong> are
                months when your rule was in defensive mode (equity exposure
                reduced). Compare these to the <strong>grey recession bands</strong>:
                early warnings before recessions are good; defensive periods
                far from any recession are false alarms that hurt returns."
                    )),
                    plotlyOutput("wi_signal_timeline", height = "300px")
                )
              ),
              
              # ---- Row 4: interpretation guide ----
              fluidRow(
                box(width = 12, title = "Interpreting Your Results",
                    status = "info", solidHeader = TRUE,
                    uiOutput("wi_interpretation")
                )
              )
      )
    )
  )
)


# =============================================================================
# Step 14: SERVER (all reactive logic)
# -----------------------------------------------------------------------------
# Rationale: UI defines the layout; server defines how data is processed
#            and rendered. reactive() blocks re-run when their inputs change.
# =============================================================================

server <- function(input, output, session) {
  
  # ============================================================
  # MODULE 1: SIGNAL EXPLORER
  # ============================================================
  
  # The currently selected indicator (monthly version)
  selected_series <- reactive({
    req(input$sig_indicator)
    get_series(input$sig_indicator) |> to_monthly()
  })
  
  # Time-series chart
  output$sig_timeseries <- renderPlotly({
    df <- selected_series()
    if (nrow(df) == 0) return(NULL)
    lab <- series_catalog$label[series_catalog$id == input$sig_indicator]
    
    # Compute the y-range including the threshold line so shading covers it
    # Can't use -Inf/Inf - plotly drops them in conversion
    y_range <- range(df$value, input$sig_threshold, na.rm = TRUE)
    y_pad <- diff(y_range) * 0.05  # 5% padding for breathing room
    y_min <- y_range[1] - y_pad
    y_max <- y_range[2] + y_pad
    
    # CRITICAL: draw shading FIRST (bottom), data line on top - otherwise
    p <- ggplot(df, aes(x = date, y = value))
    p <- add_recession_shading(p, y_min = y_min, y_max = y_max)
    p <- p +
      geom_line(color = COL_LINE, linewidth = 0.6) +
      geom_hline(yintercept = input$sig_threshold,
                 linetype = "dashed", color = COL_THRESH, linewidth = 0.6) +
      coord_cartesian(ylim = c(y_min, y_max)) +  # lock y-axis so shading fills
      labs(x = NULL, y = lab) +
      theme_finance()
    
    ggplotly(p) |> config(displayModeBar = FALSE)
  })
  
  # Find trigger events (a run of consecutive months meeting the condition)
  trigger_events <- reactive({
    df <- selected_series()
    if (nrow(df) == 0) return(tibble())
    
    # 1) Flag each month: does it meet the condition?
    df <- df |>
      mutate(meets = if (input$sig_direction == "below") {
        value < input$sig_threshold
      } else {
        value > input$sig_threshold
      })
    
    # 2) Run-length cumsum groups consecutive TRUE months together
    df |>
      mutate(group_id = cumsum(meets != dplyr::lag(meets, default = FALSE))) |>
      filter(meets) |>
      group_by(group_id) |>
      summarise(
        trigger_date = min(date),
        end_date     = max(date),
        n_months     = n(),
        avg_value    = mean(value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      filter(n_months >= input$sig_min_months) |>
      select(-group_id) |>
      # 3) Join each event's start to equity forward returns
      left_join(
        sp500_monthly |> select(date, ret_3m, ret_6m, ret_12m, ret_24m),
        by = c("trigger_date" = "date")
      )
  })
  
  # Scatter plot
  output$sig_scatter <- renderPlotly({
    ev <- trigger_events()
    if (nrow(ev) == 0) return(NULL)
    
    horizon_col <- input$sig_scatter_horizon
    horizon_lab <- switch(horizon_col,
                          "ret_3m"  = "3-month",
                          "ret_6m"  = "6-month",
                          "ret_12m" = "12-month",
                          "ret_24m" = "24-month")
    
    df <- ev |>
      filter(!is.na(.data[[horizon_col]])) |>
      mutate(ret = .data[[horizon_col]])
    if (nrow(df) == 0) return(NULL)
    
    p <- ggplot(df, aes(x = avg_value, y = ret,
                        text = paste0("Trigger: ", trigger_date,
                                      "<br>Avg value: ", round(avg_value, 2),
                                      "<br>Return: ", round(ret, 1), "%"))) +
      geom_hline(yintercept = 0, linetype = "dashed",
                 color = COL_MUTED, linewidth = 0.4) +
      geom_point(color = COL_PRIMARY, size = 3, alpha = 0.75) +
      geom_smooth(aes(group = 1), method = "lm", se = FALSE,
                  color = COL_ACCENT, linewidth = 0.7) +
      labs(x = "Signal strength at trigger (avg value)",
           y = paste0(horizon_lab, " S&P 500 return (%)")) +
      theme_finance()
    
    ggplotly(p, tooltip = "text") |> config(displayModeBar = FALSE)
  })
  
  # Distribution boxplots over the four horizons
  output$sig_distribution <- renderPlotly({
    ev <- trigger_events()
    if (nrow(ev) == 0) return(NULL)
    
    long <- ev |>
      select(starts_with("ret_")) |>
      pivot_longer(everything(), names_to = "horizon", values_to = "ret") |>
      filter(!is.na(ret)) |>
      mutate(horizon = factor(horizon,
                              levels = c("ret_3m", "ret_6m", "ret_12m", "ret_24m"),
                              labels = c("3 months", "6 months", "12 months", "24 months")))
    
    p <- ggplot(long, aes(x = horizon, y = ret)) +
      geom_hline(yintercept = 0, linetype = "dashed",
                 color = COL_MUTED, linewidth = 0.4) +
      geom_boxplot(fill = COL_PRIMARY, alpha = 0.15, color = COL_PRIMARY,
                   outlier.alpha = 0.5, outlier.color = COL_PRIMARY,
                   linewidth = 0.5, width = 0.5) +
      geom_jitter(width = 0.12, alpha = 0.55, size = 1.6, color = COL_ACCENT) +
      labs(x = "Horizon after trigger", y = "S&P 500 return (%)") +
      theme_finance() +
      theme(legend.position = "none")
    
    ggplotly(p) |> config(displayModeBar = FALSE)
  })
  
  # Trigger events table
  output$sig_table <- renderDT({
    ev <- trigger_events()
    if (nrow(ev) == 0) {
      return(datatable(
        data.frame(Message = "No trigger events found with these settings."),
        options = list(dom = "t"), rownames = FALSE
      ))
    }
    ev |>
      mutate(
        across(starts_with("ret_"), ~ round(.x, 1)),
        avg_value = round(avg_value, 2)
      ) |>
      rename(
        `Trigger start` = trigger_date,
        `Trigger end`   = end_date,
        `Months`        = n_months,
        `Avg value`     = avg_value,
        `+3M return %`  = ret_3m,
        `+6M return %`  = ret_6m,
        `+12M return %` = ret_12m,
        `+24M return %` = ret_24m
      ) |>
      datatable(options = list(pageLength = 10, scrollX = TRUE, dom = "tip"),
                rownames = FALSE,
                class = "cell-border stripe") |>
      formatStyle(c("+3M return %", "+6M return %",
                    "+12M return %", "+24M return %"),
                  color = styleInterval(0, c(COL_BAD, COL_GOOD)),
                  fontWeight = "600")
  })
  
  
  # ============================================================
  # MODULE 2: RECESSION DASHBOARD
  # ============================================================
  
  rec_start_dates <- reactive({
    recession_monthly |>
      arrange(date) |>
      mutate(start = value == 1 & dplyr::lag(value, default = 0) == 0) |>
      filter(start) |>
      pull(date)
  })
  
  # Main status table data
  dashboard_table <- reactive({
    lookback <- input$dash_lookback
    rec <- rec_start_dates()
    indicators <- series_catalog |>
      filter(!id %in% c("NASDAQCOM", "USREC"))
    
    purrr::map_dfr(seq_len(nrow(indicators)), function(i) {
      id    <- indicators$id[i]
      label <- indicators$label[i]
      group <- indicators$group[i]
      dir   <- indicators$direction[i]
      
      df <- get_series(id) |> to_monthly()
      if (nrow(df) == 0) {
        return(tibble(Group = group, Indicator = label,
                      Current = NA_real_,
                      `Pre-recession avg` = NA_real_,
                      Status = "no data"))
      }
      
      latest <- df |> arrange(desc(date)) |> slice(1) |> pull(value)
      
      pre_vals <- purrr::map_dbl(rec, function(rd) {
        window <- df |>
          filter(date >= rd %m-% months(lookback), date < rd) |>
          pull(value)
        if (length(window) == 0) NA_real_ else mean(window, na.rm = TRUE)
      })
      pre_avg <- mean(pre_vals, na.rm = TRUE)
      
      # Status determination logic
      status <- if (is.na(pre_avg) || is.na(latest)) {
        "no data"
      } else if (dir == "high") {
        if (latest >= pre_avg)            "WARNING"
        else if (latest >= pre_avg * 0.8) "Watch"
        else                              "Calm"
      } else {
        if (latest <= pre_avg)            "WARNING"
        else if (latest <= pre_avg * 1.2) "Watch"
        else                              "Calm"
      }
      
      tibble(
        Group = group,
        Indicator = label,
        Current = round(latest, 2),
        `Pre-recession avg` = round(pre_avg, 2),
        Status = status
      )
    }) |>
      arrange(factor(Group, levels = c("Financial Market", "Real Economy")),
              Indicator)
  })
  
  output$dash_table <- renderDT({
    df <- dashboard_table()
    datatable(df,
              options = list(pageLength = 15, dom = "t"),
              rownames = FALSE,
              class = "cell-border stripe") |>
      formatStyle("Status",
                  backgroundColor = styleEqual(names(STATUS_BG), unname(STATUS_BG)),
                  color           = styleEqual(names(STATUS_FG), unname(STATUS_FG)),
                  fontWeight = "600") |>
      formatStyle("Group", fontWeight = "600", color = COL_PRIMARY)
  })
  
  # Predictive-performance heat map
  default_trigger_rules <- tibble::tribble(
    ~id,               ~rule_label,                 ~direction,  ~threshold,
    "T10Y2Y",          "T10Y2Y < 0",                "below",      0,
    "T10Y3M",          "T10Y3M < 0",                "below",      0,
    "BAA10Y",          "Baa-10Y > 3",               "above",      3,
    "BAMLH0A0HYM2",    "HY spread > 6",             "above",      6,
    "VIXCLS",          "VIX > 25",                  "above",     25,
    "UNRATE",          "UNRATE > 5",                "above",      5,
    "INDPRO",          "INDPRO YoY change < 0",     "below_yoy",  0,
    "A191RL1Q225SBEA", "Real GDP growth < 0",       "below",      0,
    "UMCSENT",         "U-Mich sentiment < 75",     "below",     75
  )
  
  heatmap_data <- reactive({
    rules <- default_trigger_rules
    
    purrr::map_dfr(seq_len(nrow(rules)), function(i) {
      id  <- rules$id[i]
      rl  <- rules$rule_label[i]
      dir <- rules$direction[i]
      thr <- rules$threshold[i]
      
      df <- get_series(id) |> to_monthly()
      if (nrow(df) == 0) return(tibble())
      
      if (dir == "below_yoy") {
        df <- df |>
          arrange(date) |>
          mutate(value = (value / dplyr::lag(value, 12) - 1) * 100) |>
          filter(!is.na(value))
        eff_dir <- "below"
      } else {
        eff_dir <- dir
      }
      
      meets <- if (eff_dir == "below") df$value < thr else df$value > thr
      df$meets <- meets
      
      events <- df |>
        mutate(group_id = cumsum(meets != dplyr::lag(meets, default = FALSE))) |>
        filter(meets) |>
        group_by(group_id) |>
        summarise(trigger_date = min(date), .groups = "drop")
      if (nrow(events) == 0) return(tibble())
      
      ev <- events |>
        left_join(
          sp500_monthly |> select(date, ret_3m, ret_6m, ret_12m, ret_24m),
          by = c("trigger_date" = "date")
        )
      
      tibble(
        Indicator = paste0(series_catalog$label[series_catalog$id == id],
                           "  (", rl, ")"),
        `3M`  = mean(ev$ret_3m,  na.rm = TRUE),
        `6M`  = mean(ev$ret_6m,  na.rm = TRUE),
        `12M` = mean(ev$ret_12m, na.rm = TRUE),
        `24M` = mean(ev$ret_24m, na.rm = TRUE),
        n_events = sum(!is.na(ev$ret_12m))
      )
    })
  })
  
  output$dash_heatmap <- renderPlotly({
    hd <- heatmap_data()
    if (nrow(hd) == 0) return(NULL)
    
    long <- hd |>
      select(-n_events) |>
      pivot_longer(c(`3M`, `6M`, `12M`, `24M`),
                   names_to = "Horizon", values_to = "AvgReturn") |>
      mutate(Horizon = factor(Horizon,
                              levels = c("3M", "6M", "12M", "24M")))
    
    p <- ggplot(long, aes(x = Horizon, y = Indicator, fill = AvgReturn,
                          text = paste0("Indicator: ", Indicator,
                                        "<br>Horizon: ", Horizon,
                                        "<br>Avg return: ",
                                        round(AvgReturn, 1), "%"))) +
      geom_tile(color = "white", linewidth = 1) +
      geom_text(aes(label = ifelse(is.na(AvgReturn), "-",
                                   paste0(round(AvgReturn, 1), "%"))),
                size = 3.2, color = COL_TEXT) +
      scale_fill_gradient2(low = COL_BAD, mid = COL_NEUTRAL, high = COL_GOOD,
                           midpoint = 0, name = "Avg return %",
                           na.value = "#F0F0F2") +
      labs(x = "Forward horizon", y = NULL) +
      theme_finance() +
      theme(axis.text.y = element_text(size = 9),
            panel.grid = element_blank())
    
    ggplotly(p, tooltip = "text") |> config(displayModeBar = FALSE)
  })
  
  # ============================================================
  # NEW: Composite recession-pressure index (option D)
  # ============================================================
  # Logic:
  #   1. For each indicator, pull its monthly series
  #   2. Standardize each one to a z-score (subtract mean, divide by sd)
  #   3. For 'low'-direction indicators (small = bad), flip the sign
  #   4. Average across all indicators per month -> composite pressure score
  #   5. Plot time series with recession shading; mark today in gold
  pressure_index_data <- reactive({
    indicators <- series_catalog |>
      filter(!id %in% c("NASDAQCOM", "USREC"))
    
    # Pull all monthly series and compute z-scores
    z_list <- purrr::map(seq_len(nrow(indicators)), function(i) {
      id  <- indicators$id[i]
      dir <- indicators$direction[i]
      df  <- get_series(id) |> to_monthly()
      if (nrow(df) < 24) return(NULL)
      
      mu <- mean(df$value, na.rm = TRUE)
      sd_val <- sd(df$value, na.rm = TRUE)
      if (is.na(sd_val) || sd_val == 0) return(NULL)
      df$z <- (df$value - mu) / sd_val
      
      # 'low' direction means small values are dangerous; flip sign so
      if (dir == "low") df$z <- -df$z
      
      df |> select(date, z) |> mutate(series_id = id)
    }) |> compact()
    
    if (length(z_list) == 0) return(tibble())
    
    bind_rows(z_list) |>
      group_by(date) |>
      summarise(pressure = mean(z, na.rm = TRUE),
                n_indicators = n(),
                .groups = "drop") |>
      filter(n_indicators >= 5)
  })
  
  output$dash_pressure_index <- renderPlotly({
    df <- pressure_index_data()
    if (nrow(df) == 0) return(NULL)
    
    # Current point (latest month)
    latest <- df |> arrange(desc(date)) |> slice(1)
    
    # y-range for the recession shading
    y_range <- range(df$pressure, na.rm = TRUE)
    y_pad <- diff(y_range) * 0.1
    y_min <- y_range[1] - y_pad
    y_max <- y_range[2] + y_pad
    
    p <- ggplot(df, aes(x = date, y = pressure))
    p <- add_recession_shading(p, y_min = y_min, y_max = y_max)
    p <- p +
      geom_hline(yintercept = 0, linetype = "dashed",
                 color = COL_MUTED, linewidth = 0.4) +
      geom_line(color = COL_LINE, linewidth = 0.5) +
      geom_point(data = latest, aes(x = date, y = pressure),
                 color = COL_ACCENT, size = 4, inherit.aes = FALSE) +
      geom_point(data = latest, aes(x = date, y = pressure),
                 color = COL_PRIMARY, size = 1.8, inherit.aes = FALSE) +
      coord_cartesian(ylim = c(y_min, y_max)) +
      labs(x = NULL,
           y = "Composite pressure index (avg z-score)") +
      theme_finance()
    
    ggplotly(p) |> config(displayModeBar = FALSE)
  })
  
  # ============================================================
  # NEW: Per-indicator small-multiples panel (option B)
  # ============================================================
  # Logic:
  #   1. For each indicator, pull the past 10 years of monthly data
  #   2. Compute its pre-recession average (same logic as the main table)
  #   3. Each panel: blue line + red dashed line (threshold) + gold dot (now)
  #   4. facet_wrap into a 3x3 grid
  output$dash_snapshot_panels <- renderPlot({
    lookback <- input$dash_lookback
    rec <- rec_start_dates()
    indicators <- series_catalog |>
      filter(!id %in% c("NASDAQCOM", "USREC"))
    
    cutoff <- Sys.Date() - years(10)
    
    plot_data <- purrr::map_dfr(seq_len(nrow(indicators)), function(i) {
      id    <- indicators$id[i]
      label <- indicators$label[i]
      df <- get_series(id) |> to_monthly() |> filter(date >= cutoff)
      if (nrow(df) == 0) return(tibble())
      
      pre_vals <- purrr::map_dbl(rec, function(rd) {
        window <- get_series(id) |>
          to_monthly() |>
          filter(date >= rd %m-% months(lookback), date < rd) |>
          pull(value)
        if (length(window) == 0) NA_real_ else mean(window, na.rm = TRUE)
      })
      pre_avg <- mean(pre_vals, na.rm = TRUE)
      
      df |>
        mutate(indicator = label,
               pre_avg = pre_avg)
    })
    
    if (nrow(plot_data) == 0) return(NULL)
    
    latest_pts <- plot_data |>
      group_by(indicator) |>
      filter(date == max(date)) |>
      ungroup()
    
    threshold_lines <- plot_data |>
      group_by(indicator) |>
      summarise(pre_avg = first(pre_avg), .groups = "drop") |>
      filter(!is.na(pre_avg))
    
    ggplot(plot_data, aes(x = date, y = value)) +
      geom_line(color = COL_LINE, linewidth = 0.5) +
      geom_hline(data = threshold_lines,
                 aes(yintercept = pre_avg),
                 color = COL_BAD, linetype = "dashed", linewidth = 0.5) +
      geom_point(data = latest_pts,
                 aes(x = date, y = value),
                 color = COL_ACCENT, size = 3.5) +
      geom_point(data = latest_pts,
                 aes(x = date, y = value),
                 color = COL_PRIMARY, size = 1.5) +
      facet_wrap(~ indicator, scales = "free_y", ncol = 3) +
      labs(x = NULL, y = NULL) +
      theme_finance() +
      theme(strip.text = element_text(face = "bold", size = 10,
                                      color = COL_PRIMARY,
                                      margin = margin(b = 6)),
            strip.background = element_rect(fill = "#F4F1E8", color = NA),
            axis.text = element_text(size = 8),
            panel.spacing = unit(1.2, "lines"))
  })
  
  
  # ============================================================
  # MODULE 3: WHAT IF COUNTERFACTUAL SIMULATOR
  # ============================================================
  # Logic flow:
  #   1. Build a monthly time series of: equity returns + signal status
  #   2. For each month, decide how much equity exposure the rule has
  #      (100% when signal off; user-chosen % when signal on or within
  #      the "hold" window after signal turned off)
  #   3. Compound returns to get the rule's portfolio value vs buy-and-hold
  #   4. Report summary stats and visualize
  
  # Core backtest engine - returns a tibble of monthly portfolio values
  whatif_backtest <- reactive({
    req(input$wi_indicator, input$wi_threshold,
        input$wi_reduce_pct, input$wi_start_year)
    
    # ---- 1. Get the signal indicator monthly ----
    sig_df <- get_series(input$wi_indicator) |> to_monthly()
    if (nrow(sig_df) == 0) return(NULL)
    
    # ---- 2. Get equity monthly returns ----
    # Compute month-over-month % change of NASDAQCOM
    eq <- sp500_monthly |>
      arrange(date) |>
      mutate(eq_ret = value / dplyr::lag(value) - 1) |>
      select(date, eq_ret)
    
    # ---- 3. Merge signal + equity, restrict to backtest window ----
    df <- sig_df |>
      rename(signal_val = value) |>
      inner_join(eq, by = "date") |>
      filter(year(date) >= input$wi_start_year) |>
      filter(!is.na(eq_ret)) |>
      arrange(date)
    
    if (nrow(df) < 24) return(NULL)
    
    # ---- 4. Flag months where the signal is "on" ----
    df <- df |>
      mutate(signal_on = if (input$wi_direction == "below") {
        signal_val < input$wi_threshold
      } else {
        signal_val > input$wi_threshold
      })
    
    # ---- 5. Extend "signal on" forward by the hold period ----
    # When signal turns off, the rule stays defensive for wi_hold_months
    # extra months before going back to 100% equity.
    hold <- input$wi_hold_months
    defensive <- df$signal_on
    if (hold > 0 && any(df$signal_on)) {
      for (i in seq_along(defensive)) {
        if (df$signal_on[i]) {
          # mark next `hold` months as defensive too
          end_idx <- min(i + hold, length(defensive))
          defensive[i:end_idx] <- TRUE
        }
      }
    }
    df$defensive <- defensive
    
    # ---- 6. Compute equity exposure each month ----
    # 100% when not defensive; user-chosen % when defensive
    # Cash portion earns 3% annualized (approx. T-bill / money market yield)
    reduce_frac <- input$wi_reduce_pct / 100
    cash_monthly_ret <- 0.03 / 12
    df <- df |>
      mutate(exposure = ifelse(defensive, reduce_frac, 1),
             rule_ret = exposure * eq_ret + (1 - exposure) * cash_monthly_ret)
    
    # ---- 7. Compound returns into portfolio values starting at $100 ----
    df <- df |>
      mutate(buyhold_val = 100 * cumprod(1 + eq_ret),
             rule_val    = 100 * cumprod(1 + rule_ret))
    
    df
  })
  
  # Portfolio value over time
  output$wi_portfolio_chart <- renderPlotly({
    df <- whatif_backtest()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    long <- df |>
      select(date, `Buy & Hold` = buyhold_val, `Your Rule` = rule_val) |>
      pivot_longer(-date, names_to = "Strategy", values_to = "Value")
    
    y_range <- range(long$Value, na.rm = TRUE)
    y_pad <- diff(y_range) * 0.05
    y_min <- y_range[1] - y_pad
    y_max <- y_range[2] + y_pad
    
    # CRITICAL: clip recession periods to the backtest window AT THE DATA LEVEL
    # ggplotly() ignores coord_cartesian(xlim), so we must filter & clip the
    # rectangle coordinates themselves
    bt_start <- min(df$date)
    bt_end   <- max(df$date)
    rec_in_window <- recession_periods |>
      filter(end >= bt_start, start <= bt_end) |>
      mutate(start = pmax(start, bt_start),
             end   = pmin(end,   bt_end))
    
    p <- ggplot(long, aes(x = date, y = Value, color = Strategy))
    if (nrow(rec_in_window) > 0) {
      p <- p + geom_rect(
        data = rec_in_window,
        aes(xmin = start, xmax = end, ymin = y_min, ymax = y_max),
        fill = COL_RECESS, alpha = 0.55, inherit.aes = FALSE
      )
    }
    p <- p +
      geom_line(linewidth = 0.7) +
      scale_color_manual(values = c("Buy & Hold" = COL_PRIMARY,
                                    "Your Rule"  = COL_ACCENT)) +
      labs(x = NULL, y = "Portfolio value (starting at $100)") +
      theme_finance()
    
    ggplotly(p) |>
      config(displayModeBar = FALSE)
  })
  
  # Summary statistics table
  output$wi_stats_table <- renderDT({
    df <- whatif_backtest()
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(
        data.frame(Message = "Not enough data for this configuration."),
        options = list(dom = "t"), rownames = FALSE
      ))
    }
    
    # Helper: compute summary stats for a returns vector & value series
    stat_block <- function(ret_vec, val_vec) {
      total_ret <- last(val_vec) / first(val_vec) - 1
      n_years <- as.numeric(difftime(max(df$date), min(df$date),
                                     units = "days")) / 365.25
      ann_ret <- (1 + total_ret)^(1 / n_years) - 1
      ann_vol <- sd(ret_vec, na.rm = TRUE) * sqrt(12)
      sharpe <- ann_ret / ann_vol
      # max drawdown
      running_max <- cummax(val_vec)
      drawdowns <- val_vec / running_max - 1
      max_dd <- min(drawdowns, na.rm = TRUE)
      list(total_ret = total_ret, ann_ret = ann_ret,
           ann_vol = ann_vol, sharpe = sharpe, max_dd = max_dd)
    }
    
    bh <- stat_block(df$eq_ret,   df$buyhold_val)
    rl <- stat_block(df$rule_ret, df$rule_val)
    n_defensive <- sum(df$defensive)
    pct_defensive <- 100 * n_defensive / nrow(df)
    
    pct_fmt <- function(x) paste0(round(x * 100, 1), "%")
    num_fmt <- function(x) round(x, 2)
    
    stats <- tibble(
      Metric = c("Total return",
                 "Annualized return",
                 "Annualized volatility",
                 "Sharpe ratio (rf = 0)",
                 "Max drawdown",
                 "Months in defensive mode"),
      `Buy & Hold` = c(pct_fmt(bh$total_ret),
                       pct_fmt(bh$ann_ret),
                       pct_fmt(bh$ann_vol),
                       num_fmt(bh$sharpe),
                       pct_fmt(bh$max_dd),
                       "0 (0.0%)"),
      `Your Rule` = c(pct_fmt(rl$total_ret),
                      pct_fmt(rl$ann_ret),
                      pct_fmt(rl$ann_vol),
                      num_fmt(rl$sharpe),
                      pct_fmt(rl$max_dd),
                      paste0(n_defensive, " (",
                             round(pct_defensive, 1), "%)"))
    )
    
    datatable(stats,
              options = list(dom = "t", pageLength = 10),
              rownames = FALSE,
              class = "cell-border stripe")
  })
  
  # Signal-on timeline plot
  output$wi_signal_timeline <- renderPlotly({
    df <- whatif_backtest()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    # Defensive periods as start/end ranges for shading
    df_d <- df |>
      arrange(date) |>
      mutate(grp = cumsum(defensive != dplyr::lag(defensive, default = FALSE))) |>
      filter(defensive) |>
      group_by(grp) |>
      summarise(start = min(date), end = max(date), .groups = "drop")
    
    y_range <- range(df$signal_val, input$wi_threshold, na.rm = TRUE)
    y_pad <- diff(y_range) * 0.05
    y_min <- y_range[1] - y_pad
    y_max <- y_range[2] + y_pad
    
    lab <- series_catalog$label[series_catalog$id == input$wi_indicator]
    
    # Clip recession periods to backtest window at the data level
    # (plotly ignores coord_cartesian/scale limits)
    bt_start <- min(df$date)
    bt_end   <- max(df$date)
    rec_in_window <- recession_periods |>
      filter(end >= bt_start, start <= bt_end) |>
      mutate(start = pmax(start, bt_start),
             end   = pmin(end,   bt_end))
    
    p <- ggplot(df, aes(x = date, y = signal_val))
    
    if (nrow(rec_in_window) > 0) {
      p <- p + geom_rect(
        data = rec_in_window,
        aes(xmin = start, xmax = end, ymin = y_min, ymax = y_max),
        fill = COL_RECESS, alpha = 0.55, inherit.aes = FALSE
      )
    }
    
    if (nrow(df_d) > 0) {
      p <- p + geom_rect(
        data = df_d,
        aes(xmin = start, xmax = end, ymin = y_min, ymax = y_max),
        fill = "#E8B4B0", alpha = 0.45, inherit.aes = FALSE
      )
    }
    
    p <- p +
      geom_line(color = COL_LINE, linewidth = 0.5) +
      geom_hline(yintercept = input$wi_threshold,
                 linetype = "dashed", color = COL_THRESH, linewidth = 0.6) +
      scale_x_date(limits = c(bt_start, bt_end), expand = c(0, 0)) +
      scale_y_continuous(limits = c(y_min, y_max), expand = c(0, 0)) +
      labs(x = NULL, y = lab) +
      theme_finance()
    
    ggplotly(p) |>
      layout(xaxis = list(range = list(as.character(bt_start),
                                       as.character(bt_end)))) |>
      config(displayModeBar = FALSE)
  })
  
  # Text interpretation generated dynamically
  output$wi_interpretation <- renderUI({
    df <- whatif_backtest()
    if (is.null(df) || nrow(df) == 0) {
      return(HTML("<p>Not enough data to interpret.</p>"))
    }
    
    # Quick stats for the narrative
    total_bh <- last(df$buyhold_val) / first(df$buyhold_val) - 1
    total_rl <- last(df$rule_val)    / first(df$rule_val) - 1
    diff_ret <- total_rl - total_bh
    
    # Max drawdowns
    dd_bh <- min(df$buyhold_val / cummax(df$buyhold_val) - 1)
    dd_rl <- min(df$rule_val    / cummax(df$rule_val)    - 1)
    
    pct_def <- 100 * sum(df$defensive) / nrow(df)
    
    winner <- if (diff_ret > 0) "your rule" else "buy-and-hold"
    less_painful <- if (dd_rl > dd_bh) "your rule" else "buy-and-hold"
    
    HTML(paste0(
      "<div style='font-size:13px; line-height:1.6; color:", COL_TEXT, ";'>",
      "<p>Over this backtest window, <strong>", winner, "</strong> ended up with the higher
      total return (difference: ", round(diff_ret * 100, 1), " percentage points).
      Your rule was in defensive mode for <strong>", round(pct_def, 1),
      "%</strong> of all months.</p>",
      
      "<p>On the downside-protection front, <strong>", less_painful,
      "</strong> had the milder worst drawdown
      (buy-and-hold: ", round(dd_bh * 100, 1), "%, your rule: ",
      round(dd_rl * 100, 1), "%).</p>",
      
      "<p><strong style='color:", COL_PRIMARY, ";'>What this tells you:</strong>
      If your rule beat buy-and-hold, the signal you chose triggered close enough
      to actual market downturns to save you more than the false alarms cost.
      If buy-and-hold won, the rule was too eager to step out: the signal generated
      enough false alarms that you missed bull markets and never made it back.
      Try adjusting the threshold (stricter = fewer false alarms but later warnings)
      or the hold period (shorter = quicker reentry, but less protection if the
      drawdown drags on).</p>",
      "</div>"
    ))
  })
  
}


# =============================================================================
# Step 15: Launch the app
# =============================================================================
shinyApp(ui, server)

# ============================================================
# WormTagDB: Database of endogenously tagged C. elegans genes
# Author: Jake Leyhr
# Contact: jake.leyhr@duke.edu
# Last updated: 2025-11-25
# License: MIT / CC-BY
# ============================================================

##### Setup #####
# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(emayili)
  library(bslib)
  library(glue)
  library(tidyverse)
  library(DT)
  library(scales)
  library(plotly)
  library(ggpubr)
  library(grid)
  library(eulerr)
  library(VennDiagram)
  library(futile.logger)
  library(rhandsontable)
  library(knitr)
  library(RColorBrewer)
  library(qs)
})

# Bootstrap theme
my_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = font_google("Roboto"),
  heading_font = font_google("Roboto Slab"),
  "form-check-input-checked-bg-color" = "#243746",
  "form-check-input-checked-border-color" = "#243746"
)

##### Universal Email Configuration #####
# App can be deployed from virtual machine (VM), shinyapps.io (shinyapps) and locally for testing (local)
# This section ensures that the email functionality works in all cases

# Detect environment
detect_environment <- function() {
  hostname <- tryCatch(system("hostname", intern = TRUE), error = function(e) NA)
  
  if (!is.na(hostname) && hostname == "rapid-2430.vm.duke.edu" && file.exists("/etc/shiny-server")) {
    return("vm")
  } else if (nzchar(Sys.getenv("SHINY_PORT"))) {
    return("shinyapps")
  } else {
    return("local")
  }
}

ENVIRONMENT <- detect_environment()
message("Detected environment: ", ENVIRONMENT)

# DEBUG: Environment variables
message("=== Environment Variables Debug ===")
message("SHINYAPPS_INSTANCE: '", Sys.getenv("SHINYAPPS_INSTANCE"), "'")
message("R_CONFIG_ACTIVE: '", Sys.getenv("R_CONFIG_ACTIVE"), "'")
message("SHINY_PORT: '", Sys.getenv("SHINY_PORT"), "'")
message("EMAIL_USER: '", Sys.getenv("EMAIL_USER"), "'")
message("EMAIL_PASS: ", if(nzchar(Sys.getenv("EMAIL_PASS"))) "***SET***" else "***NOT SET***")
message("===================================")

##### Method 1: Gmail OAuth2 (VM only) #####
GMAIL_OAUTH_AVAILABLE <- FALSE
if (file.exists("~/.config/wormtagdb/client_secret.json")) {
  if (requireNamespace("gmailr", quietly = TRUE)) {
    tryCatch({
      options(gargle_oauth_cache = "~/.R/gargle/gargle-oauth")
      gmailr::gm_auth_configure(path = "~/.config/wormtagdb/client_secret.json")
      gmailr::gm_auth(email = "wormtagdb@gmail.com",
                      scopes = "https://www.googleapis.com/auth/gmail.send")
      GMAIL_OAUTH_AVAILABLE <- TRUE
      message("Gmail authentication successful")
    }, error = function(e) {
      message("Gmail authentication failed - running in local mode")
    })
  }
} else {
  message("Gmailr credentials not found")
}

##### Method 2: SMTP (shinyapps.io only) #####
SMTP_AVAILABLE <- FALSE
smtp_server <- NULL
if (ENVIRONMENT == "shinyapps") {
  if (requireNamespace("emayili", quietly = TRUE)) {
    email_user <- Sys.getenv("EMAIL_USER") # Saved in .Renviron
    email_pass <- Sys.getenv("EMAIL_PASS") # Saved in .Renviron
    
    if (email_user != "" && email_pass != "") {
      tryCatch({
        smtp_server <- emayili::server(
          host = "smtp.gmail.com",
          port = 587,
          username = email_user,
          password = email_pass
        )
        SMTP_AVAILABLE <- TRUE
        message("✓ SMTP configuration successful")
      }, error = function(e) {
        message("✗ SMTP configuration failed: ", e$message)
      })
    } else {
      message("✗ EMAIL_USER or EMAIL_PASS environment variables not set")
    }
  }
}

##### Method 3: Local fallback #####
LOCAL_FALLBACK <- (!GMAIL_OAUTH_AVAILABLE && !SMTP_AVAILABLE)
if (LOCAL_FALLBACK) {
  message("⚠ Running in LOCAL mode - emails will be saved to files")
  if (!dir.exists("local_submissions")) dir.create("local_submissions", recursive = TRUE)
}

##### Unified send_email function #####
send_email <- function(to, subject, html_body, from = "wormtagdb@gmail.com") {
  # 1) Gmail OAuth2 (VM only)
  if (ENVIRONMENT == "vm" && isTRUE(GMAIL_OAUTH_AVAILABLE) &&
      requireNamespace("gmailr", quietly = TRUE)) {
    gmail_ok <- tryCatch({
      msg <- gmailr::gm_mime() |>
        gmailr::gm_from(from) |>
        gmailr::gm_to(to) |>
        gmailr::gm_subject(subject) |>
        gmailr::gm_html_body(html_body)
      gmailr::gm_send_message(msg)
      message("✓ Email sent via Gmail OAuth2 to: ", to)
      TRUE
    }, error = function(e) {
      message("✗ Gmail OAuth2 send failed: ", e$message)
      FALSE
    })
    if (isTRUE(gmail_ok)) return(TRUE)
  }
  
  # 2) SMTP (shinyapps.io)
  if (exists("SMTP_AVAILABLE", inherits = TRUE) && isTRUE(SMTP_AVAILABLE) &&
      exists("smtp_server", inherits = TRUE) && !is.null(smtp_server) &&
      requireNamespace("emayili", quietly = TRUE)) {
    
    envelope <- tryCatch({
      emayili::envelope() %>%
        emayili::from(ifelse(nzchar(Sys.getenv("EMAIL_USER")), Sys.getenv("EMAIL_USER"), from)) %>%
        emayili::to(to) %>%
        emayili::subject(subject) %>%
        emayili::html(as.character(html_body))
    }, error = function(e) {
      message("✗ Failed to build emayili envelope: ", e$message)
      NULL
    })
    
    if (!is.null(envelope)) {
      smtp_ok <- tryCatch({
        smtp_server(envelope)
        message("✓ Email sent via SMTP to: ", to)
        TRUE
      }, error = function(e) {
        message("✗ SMTP send failed: ", e$message)
        FALSE
      })
      if (isTRUE(smtp_ok)) return(TRUE)
    }
  }
  
  # 3) Local fallback
  if (!dir.exists("local_submissions")) dir.create("local_submissions", recursive = TRUE)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  sanitized_to <- gsub("[^a-zA-Z0-9_@.-]", "_", to)
  filename <- file.path("local_submissions", paste0("email_", timestamp, "_", sanitized_to, ".html"))
  
  saved_ok <- tryCatch({
    writeLines(c(
      "<!DOCTYPE html>",
      "<html><head><meta charset='UTF-8'></head><body>",
      paste0("<p><strong>To:</strong> ", to, "</p>"),
      paste0("<p><strong>From:</strong> ", from), "</p>",
      paste0("<p><strong>Subject:</strong> ", subject, "</p>"),
      "<hr>",
      as.character(html_body),
      "</body></html>"
    ), filename)
    message("⚠ Email saved to file: ", normalizePath(filename))
    TRUE
  }, error = function(e) {
    message("✗ File save failed: ", e$message)
    FALSE
  })
  
  # Return FALSE if only saved locally
  return(FALSE)
}



##### Load Data and Filter #####
# Add the logo and plots filepath
shiny::addResourcePath("logo", normalizePath("logo"))
shiny::addResourcePath("summary_plots", normalizePath("summary_plots"))
message("Deployed images: ", paste(list.files("summary_plots"), collapse=", "))

##### Load precomputed data #####
DATA_DIR <- "data_derived"  # data folder with .qs files
combined_strains_with_GO <- qread(file.path(DATA_DIR, "combined_strains_with_GO.qs"))
GO_merged_final         <- qread(file.path(DATA_DIR, "GO_merged_final.qs"))
GO_combined             <- qread(file.path(DATA_DIR, "GO_combined.qs"))
GO_with_parents         <- qread(file.path(DATA_DIR, "GO_with_parents.qs"))
GO_parent_summary       <- qread(file.path(DATA_DIR, "GO_parent_summary.qs"))
expected_proportion     <- qread(file.path(DATA_DIR, "expected_proportion.qs"))
sig_diseases            <- qread(file.path(DATA_DIR, "sig_diseases.qs"))
humandiseaseproportion  <- qread(file.path(DATA_DIR, "humandiseaseproportion.qs"))

tagged_genes            <- qread(file.path(DATA_DIR, "tagged_genes.qs"))
inprogress_genes        <- qread(file.path(DATA_DIR, "inprogress_genes.qs"))
n_zero_tagged           <- qread(file.path(DATA_DIR, "n_zero_tagged.qs"))
n_nonzero_tagged        <- qread(file.path(DATA_DIR, "n_nonzero_tagged.qs"))
genes_zero_tagged       <- qread(file.path(DATA_DIR, "genes_zero_tagged.qs"))
genes_nonzero_tagged    <- qread(file.path(DATA_DIR, "genes_nonzero_tagged.qs"))
genes_all               <- qread(file.path(DATA_DIR, "genes_all.qs"))
total_named_alleles     <- qread(file.path(DATA_DIR, "total_named_alleles.qs"))
realallelecount         <- qread(file.path(DATA_DIR, "realallelecount.qs"))

n_papers_included       <- qread(file.path(DATA_DIR, "n_papers_included.qs"))
n_papers_reviewed       <- qread(file.path(DATA_DIR, "n_papers_reviewed.qs"))

genes_lit               <- qread(file.path(DATA_DIR, "genes_lit.qs"))
genes_cgc               <- qread(file.path(DATA_DIR, "genes_cgc.qs"))
lit_alleles_first       <- qread(file.path(DATA_DIR, "lit_alleles_first.qs"))
cgc_alleles_first       <- qread(file.path(DATA_DIR, "cgc_alleles_first.qs"))
yearly_counts           <- qread(file.path(DATA_DIR, "yearly_counts.qs"))
cumulative_counts       <- qread(file.path(DATA_DIR, "cumulative_counts.qs"))

cumulative_by_date      <- qread(file.path(DATA_DIR, "cumulative_by_date.qs"))
fit_data                <- qread(file.path(DATA_DIR, "fit_data.qs"))
r_squared               <- qread(file.path(DATA_DIR, "r_squared.qs"))
p_value                 <- qread(file.path(DATA_DIR, "p_value.qs"))
target_date1            <- qread(file.path(DATA_DIR, "target_date1.qs"))
genes_per_year          <- qread(file.path(DATA_DIR, "genes_per_year.qs"))

matched_pairs           <- qread(file.path(DATA_DIR, "matched_pairs.qs"))
median_delay            <- qread(file.path(DATA_DIR, "median_delay.qs"))
min_date                <- qread(file.path(DATA_DIR, "min_date.qs"))
max_date                <- qread(file.path(DATA_DIR, "max_date.qs"))

tag_location_allele_proportion <- qread(file.path(DATA_DIR, "tag_location_proportion.qs"))

gene_tag_categories     <- qread(file.path(DATA_DIR, "gene_tag_categories.qs"))
fluor_genes             <- qread(file.path(DATA_DIR, "fluor_genes.qs"))
degron_genes            <- qread(file.path(DATA_DIR, "degron_genes.qs"))
epitope_genes           <- qread(file.path(DATA_DIR, "epitope_genes.qs"))
affinity_genes          <- qread(file.path(DATA_DIR, "affinity_genes.qs"))
other_tagged_genes      <- qread(file.path(DATA_DIR, "other_tagged_genes.qs"))
fitgenes                <- qread(file.path(DATA_DIR, "fitgenes.qs"))

gene_bins_final         <- qread(file.path(DATA_DIR, "gene_bins_final.qs"))

combined_counts         <- qread(file.path(DATA_DIR, "combined_counts.qs"))
label_data              <- qread(file.path(DATA_DIR, "label_data.qs"))

guide_predictions       <- qread(file.path(DATA_DIR, "guide_predictions.qs"))

##### User interface #####
ui <- fluidPage(
  theme = my_theme,
  shinybusy::use_busy_spinner(spin = "fading-circle", position = "bottom-right"),
  tags$head(
    tags$script(src = "https://igv.org/web/release/2.15.5/dist/igv.min.js"),
    tags$script(HTML("
// Close IGV popups
function closeIGVPopups() {
  if (typeof igvBrowser !== 'undefined' && igvBrowser) {
    // Method 1: Use IGV API
    if (igvBrowser.popover) {
      igvBrowser.popover.dispose();
    }
    // Method 2: Direct DOM manipulation - try multiple possible class names
    var selectors = ['.igv-popover', '.igv-popup', '[class*=\"popover\"]', '[class*=\"popup\"]'];
    selectors.forEach(function(selector) {
      var elements = document.querySelectorAll(selector);
      elements.forEach(function(el) {
        if (el.parentNode) {
          el.parentNode.removeChild(el);
        }
      });
    });
  }
}

Shiny.addCustomMessageHandler('igv-close-popups', function(message) {
  closeIGVPopups();
});

Shiny.addCustomMessageHandler('igv-goto', function(locus) {
  closeIGVPopups();
  if (typeof igvBrowser !== 'undefined' && igvBrowser) {
    igvBrowser.search(locus);
  }
});

Shiny.addCustomMessageHandler('igv-load-track', function(message) {
  closeIGVPopups();
  var existing = igvBrowser.trackViews.find(tv => tv.track.name === message.name);
  if (existing) igvBrowser.removeTrack(existing.track);
  var trackConfig = { name: message.name, type: 'annotation', format: 'bed', features: message.features, displayMode: message.displayMode, color: undefined, colorBy: undefined, altColor: undefined };
  igvBrowser.loadTrack(trackConfig).then(function(loadedTrack) {
    setTimeout(function() { loadedTrack.trackView.setTrackHeight(message.trackHeight); loadedTrack.trackView.repaint(); }, 50);
  });
});

Shiny.addCustomMessageHandler('init-igv', function(message) {
  setTimeout(function() {
    var container = document.getElementById('igv-container');
    if (typeof window.igvBrowser === 'undefined' && container) {
      igv.createBrowser(container, {genome: 'ce11', locus: 'chrI:1-1000'}).then(function(browser) {
        window.igvBrowser = browser;
        if (window.Shiny) Shiny.setInputValue('igv_ready', true, {priority: 'event'});
      });
    }
  }, 200);
});
")),
    tags$script(HTML("
      $(document).ready(function() {
        // Enable Bootstrap tooltips if available
        if (typeof $().tooltip === 'function') {
          $('[data-toggle=\"tooltip\"]').tooltip();
        }
        
        // Help modal functionality
        var modal = document.getElementById('helpModal');
        var btn = document.getElementById('helpButton');
        var span = document.getElementsByClassName('help-modal-close')[0];
        
        if (btn && modal) {
          btn.onclick = function() {
            modal.style.display = 'block';
          }
        }
        
        if (span && modal) {
          span.onclick = function() {
            modal.style.display = 'none';
          }
        }
        
        if (modal) {
          window.onclick = function(event) {
            if (event.target == modal) {
              modal.style.display = 'none';
            }
          }
        }
      });
    ")),
    tags$style(HTML("
  /* Bar background + default tab color */
  .navbar { background-color: #243746 !important; } /* dark blue navbar */
  .navbar .navbar-brand, .navbar .navbar-nav > li > a { color: #FFFFFF !important; } /* white tab titles */

  /* Size the bar/logo */
  :root { --bar-h: 72px; }
  .navbar { min-height: var(--bar-h); padding-top:0; padding-bottom:0; margin-bottom:4px; }
  .navbar-brand { height: var(--bar-h); display:flex; align-items:stretch; gap:6px; }
  .navbar-brand img { height:100%; width:auto; display:block; }

  /* Right-justified orange items */
  .navbar-nav > li:nth-last-child(2) { margin-left: auto; }
  .navbar-nav > li:nth-last-child(-n+2) > a { color:#FF7F50 !important; font-weight:bold; }

  /* Guide Predictions page styles - from app4.R */
  .igv-wrap { border:1px solid #ddd; border-radius:8px } 
  .search-header { background: #f8f9fa; padding: 20px; border-bottom: 2px solid #dee2e6; margin-bottom: 20px; } 
  .search-box { max-width: 600px; margin: 0 auto; } 
  .search-results { max-width: 600px; margin: 10px auto; max-height: 300px; overflow-y: auto; background: white; border: 1px solid #ddd; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); } 
  .result-item { padding: 12px 20px; cursor: pointer; border-bottom: 1px solid #eee; transition: background-color 0.2s; } 
  .result-item:hover { background-color: #f5f5f5; } 
  .gene-name { font-weight: bold; color: #333; margin-right: 10px; } 
  .wbid { color: #666; font-size: 0.9em; } 
  .content-section { padding: 0 20px; } 
  .selected-gene-banner { background: #e3f2fd; padding: 15px 20px; margin-bottom: 20px; border-left: 4px solid #2196F3; } 
  .clear-btn { margin-left: 15px; } 
  table.dataTable tbody tr.parent td.child { padding: 0; } 
  table.dataTable tbody tr.parent td.child > div { animation: slideDown 0.3s ease-out; overflow: hidden; } 
  @keyframes slideDown { from { opacity: 0; max-height: 0; transform: translateY(-10px); } to 
  { opacity: 1; max-height: 500px; transform: translateY(0); } } 
  @keyframes slideUp { from { opacity: 1; max-height: 500px; transform: translateY(0); } to 
  { opacity: 0; max-height: 0; transform: translateY(-10px); } } 
  .help-icon { display: inline-block; width: 16px; height: 16px; line-height: 14px; 
  text-align: center; border-radius: 50%; background-color: #6c757d; color: white; font-size: 10.5px; 
  font-weight: bold; margin-left: 5px; cursor: help; vertical-align: middle; position: relative; } 
  .help-icon:hover { background-color: #5a6268; } 
  .tooltip-text { visibility: hidden; min-width: 150px; max-width: 400px; width: max-content; 
  background-color: #333; color: #fff; text-align: left; border-radius: 6px; padding: 10px; 
  position: absolute; z-index: 1000; bottom: 50%; left: 125%; transform: translateY(50%); 
  opacity: 0; transition: opacity 0.3s; font-size: 12px; font-weight: normal; line-height: 1.5; 
  white-space: normal; } 
  .tooltip-text::after { content: ''; position: absolute; top: 50%; right: 100%; 
  margin-top: -5px; border-width: 5px; border-style: solid; border-color: transparent #333 transparent transparent; } 
  .help-icon:hover .tooltip-text { visibility: visible; opacity: 1; }
  .help-button { background-color: #2196F3; color: white; border: none; padding: 8px 16px; 
  border-radius: 5px; font-size: 14px; font-weight: bold; cursor: pointer; 
  box-shadow: 0 2px 5px rgba(0,0,0,0.2); }
  .help-button:hover { background-color: #1976D2; }
  .help-modal { display: none; position: fixed; z-index: 10000; left: 0; top: 0; 
  width: 100%; height: 100%; overflow: auto; background-color: rgba(0,0,0,0.5); }
  .help-modal-content { background-color: white; margin: 2% auto; padding: 0; 
  width: 90%; max-width: 1200px; height: 90vh; border-radius: 8px; 
  box-shadow: 0 4px 6px rgba(0,0,0,0.1); display: flex; flex-direction: column; }
  .help-modal-header { padding: 20px 30px; background-color: #2196F3; color: white; 
  border-radius: 8px 8px 0 0; display: flex; justify-content: space-between; align-items: center; }
  .help-modal-header h2 { margin: 0; }
  .help-modal-close { color: white; font-size: 28px; font-weight: bold; 
  cursor: pointer; background: none; border: none; padding: 0; line-height: 1; }
  .help-modal-close:hover { color: #ddd; }
  .help-modal-body { padding: 30px; overflow-y: auto; flex: 1; }
  .help-modal-body h3 { color: #2196F3; margin-top: 30px; margin-bottom: 15px; }
  .help-modal-body h3:first-child { margin-top: 0; }
  .help-modal-body p { line-height: 1.6; margin-bottom: 15px; }
  .help-modal-body ul { margin-bottom: 15px; }
  .help-modal-body li { margin-bottom: 8px; line-height: 1.6; }
  .help-modal-body code { background-color: #f5f5f5; padding: 2px 6px; 
  border-radius: 3px; font-family: monospace; }
"))
),
  
  navbarPage(
    title = div(
      tags$span("WormTagDB", style = "font-size:22px; font-weight:bold; margin-right:6px; vertical-align:middle;")
      #tags$img(src = "logo/wormtagdb_logo.png", style = "margin-right:6px; vertical-align:middle;")
    ), 
    id = "main_tabs",
    
    # ---- Tab 1: Search ----
    tabPanel("Search",
             sidebarLayout(
               
               # Sidebar with search controls
               sidebarPanel(
                 textInput("search_term", "Search term", placeholder = "e.g. daf-16, hq23, DNA binding"),
                 checkboxGroupInput("search_columns", "Search in columns:",
                                    choices = c("Strain", "Genotype", "Allele", "Gene", "Fluor", "Other tags", "Tag Location", "WormBase ID", "GO Term Names"),
                                    selected = c("Strain", "Genotype", "Allele", "Gene", "GO Term Names"),
                                    inline = FALSE
                 ),
                 downloadButton("download_search_csv", "Download CSV"),
                 width = 3
               ),
               # Main panel with data table
               mainPanel(
                 h4("Matching Strains/Alleles"),
                 DTOutput("search_results"),
                 width = 9
               )
             )
    ),
    
    # ---- Tab 2: GO Term Analysis ----
    tabPanel("GO Term Analysis",
             fluidRow(
               column(
                 width = 12,
                 tags$h5("GO Terms: Enrichment or Underrepresentation"),
                 helpText("Volcano plot of all GO terms. Hover over point to see details. Click and drag to zoom in, click the home icon to reset view."),
                 plotlyOutput("GOtermplot"),
                 br(),
                 tags$h5("GO Parent Terms: Enrichment or Underrepresentation"),
                 helpText("Volcano plot of parent GO terms following semantic clustering, divided into 3 major categories. Selecting a parent updates the table below to only display GO terms in the selected cluster. Click 'Clear selection' to reset."),
                 plotlyOutput("GOparentplot", height = 450),
                 div(
                   style = "margin-top: 12px;",
                   actionLink("clear_parent", "Clear selection", style = "margin-left:12px;")
                 ),
                 br(),
                 uiOutput("go_table_title"),   # dynamic title
                 helpText("GO Term enrichment table with numbers of tagged/untagged genes, odds ratio, and FDR-adjusted p-value. Use the column filters or search function to refine"),
                 dataTableOutput("go_enrichment_table")
               )
             )
    ),
    
    # ---- Tab 3: Summary Statistics ----
    tabPanel("Summary Statistics",
             fluidRow(
               column(
                 width = 12,
                 #h4("Visual Summary of Tagged Strains"),
                 # Venn diagrams side by side
                 fluidRow(
                   column(
                     width = 6,
                     tags$div(
                       tags$img(src = "summary_plots/genesvenn.png", style = "width:100%; height:400px; object-fit:contain;"),
                       tags$figcaption("There is a substantial overlap between the endogenously tagged genes described in the literature and deposited in the CGC. However, almost half are published but not in the CGC.", 
                                       style = "text-align:center; margin-bottom: 20px;")
                     )
                   ),
                   column(
                     width = 6,
                     tags$div(
                       tags$img(src = "summary_plots/allelesvenn.png", style = "width:100%; height:400px; object-fit:contain;"),
                       tags$figcaption("As many genes have been tagged more than once (with different tags or independently by multiple labs), there are more unique alleles than tagged genes. The majority are only found in the literature.", 
                                       style = "text-align:center; margin-bottom: 20px;")
                     )
                   )
                 ),
                 
                 fluidRow(
                   column(
                     width = 6,
                     tags$div(
                       tags$img(src = "summary_plots/lineartrend.png", style = "width:100%; height:400px; object-fit:contain;"),
                       tags$figcaption(
                         as.character(glue::glue(
                           "The number of tagged genes has increased linearly since 2019, at {genes_per_year} genes per year. ",
                           "At this rate, 10,000 genes will be tagged by {format(target_date1, '%Y')}."
                         )),
                         style = "text-align:center; margin-bottom: 20px;"
                       )
                     )
                   ),
                   column(
                     width = 6,
                     tags$div(
                       tags$img(src = "summary_plots/yearlyandcumulativealleles.png", style = "width:100%; height:400px; object-fit:contain;"),
                       tags$figcaption("The adoption of CRISPR techniques in 2013 led to the rise in endogenous tagging, and now hundreds of alleles are generated each year.",
                                       style = "text-align:center; margin-bottom: 20px;")
                     )
                   )
                 ),
                 
                 fluidRow(
                   column(
                     width = 6,
                     # Matched dates
                     tags$div(
                       tags$img(src = "summary_plots/cgcpubdates.png", style = "width:100%; height:400px; object-fit:contain;"),
                       tags$figcaption(
                         glue::glue("Of the published alleles that are also deposited in CGC, the median deposition date is {round(median_delay)} days after publication."),
                         style = "text-align:center; margin-bottom: 20px;"
                       ))
                   ),
                   column(
                     width = 6,
                     # Tag Locations
                     tags$div(
                       tags$img(src = "summary_plots/allelecounts.png", style = "width:100%; height:400px; object-fit:contain;"),
                       tags$figcaption("The majority of tagged genes have only one allele.", 
                                       style = "text-align:center; margin-bottom: 20px;")
                     )
                   )
                 ),
                 
                 fluidRow(
                   column(
                     width = 6,
                     # Matched dates
                     tags$div(
                       tags$img(src = "summary_plots/tagtypes.png", style = "width:100%; height:400px; object-fit:contain;"),
                       tags$figcaption("More than 90% of endogenously tagged genes have a fluorophore allele, and often have additional epitopes such as FLAG.", 
                                       style = "text-align:center; margin-bottom: 20px;")
                     )
                   ),
                   column(
                     width = 6,
                     # Tag Locations
                     tags$div(
                       #plotOutput("taglocations", height = "400px", width = "100%"),
                       tags$img(src = "summary_plots/taglocations.png", style = "width:100%; height:400px; object-fit:contain;"),
                       tags$figcaption("The majority of alleles have the gene tagged at the C-terminus.", 
                                       style = "text-align:center; margin-bottom: 20px;")
                     )
                   )
                 ),
                 
                 # Top labs
                 tags$div(
                   tags$img(src = "summary_plots/toplabs.png", style = "width:100%; height:400px; object-fit:contain;"),
                   tags$figcaption("Allele ID prefixes identify the sources of the alleles. Suny Biotech (syb) is the largest single contributor, followed by the Waterston (st), Sherwood (qy), Goldstein (cp), Du (dev), and Guang (ust) labs.", 
                                   style = "text-align:center; margin-bottom: 20px;")
                 ),
                 
                 # Disease associations
                 tags$div(
                   tags$img(src = "summary_plots/sigdiseases.png", style = "width:100%; height:400px; object-fit:contain;"),
                   tags$figcaption(
                     glue::glue("{humandiseaseproportion} of genes with a human disease association have been tagged. Of 4485 human disease categories, just {nrow(sig_diseases)} are significantly enriched or depleted among endogenously tagged genes (FDR cutoff = 0.05)."), 
                     style = "text-align:center; margin-bottom: 20px;")
                 )
               )
             )
    ),
    
    
    # ---- Tab 4: Search guide RNAs ----
    tabPanel("Guide Predictions",
             # Search header section with Help button
             div(class = "search-header",
                 div(style = "text-align: center; margin-bottom: 20px; position: relative;",
                     tags$h2(style = "display: inline-block; margin: 0;", "Gene Search"),
                     # Help button positioned on the right
                     tags$button(id = "helpButton", class = "help-button", "Help", 
                                 style = "position: absolute; right: 0; top: 50%; transform: translateY(-50%);")
                 ),
                 div(class = "search-box", textInput("search_query", NULL, placeholder = "Search by gene name or WBGene ID...", width = "100%")),
                 uiOutput("search_results_ui")
             ),
             
             # Help modal
             div(id = "helpModal", class = "help-modal",
                 div(class = "help-modal-content",
                     div(class = "help-modal-header",
                         tags$h2("Guide to CRISPR Primer Designs"),
                         tags$button(class = "help-modal-close", "×")
                     ),
                     div(class = "help-modal-body",
                         tags$h3("About This Tool"),
                         tags$p("This page provides computationally predicted CRISPR guide RNA and primer designs for endogenous protein tagging in C. elegans. 
                                         Each design includes guide RNAs, CRISPR primers for homology arms, and genotyping primers."),
  
                         tags$h3("How to Use This Tool"),
                         tags$p(tags$strong("1. Search for a gene:"), " Use the search box to find genes by name (e.g., unc-6) or WormBase ID (e.g., WBGene00006746)."),
                         tags$p(tags$strong("2. Select a guide design:"), " Click on a row in the table to view the primers and guide RNA in the genome browser. Click the ⊕ symbol to expand and see full primer sequences."),
                         tags$p(tags$strong("3. Review the genome browser:"), " The interactive IGV browser shows guide RNA locations (green), CRISPR primers (orange/red), and genotyping primers (purple)."),
                         
                         tags$h3("Understanding the Primers"),
                         tags$p(tags$strong("Primer 1:"), " Outer primer for left homology arm amplification."),
                         tags$p(tags$strong("Primer 2/2B:"), " Inner primer for left homology arm. Lowercase bases indicate silent mutations to prevent Cas9 from cutting the knock-in vector."),
                         tags$p(tags$strong("Primer 3/3B:"), " Inner primer for right homology arm. Lowercase bases indicate silent mutations."),
                         tags$p(tags$strong("Primer 4:"), " Outer primer for right homology arm amplification."),
                         tags$p(tags$strong("Primer 2A/3A:"), " Additional primers for cases where the guide sequence is too far from the insertion site for the standard inner primers to reach."),
                         tags$p(tags$strong("Genotyping Primers:"), " Forward and reverse primers to verify successful insertion via PCR. They can also be used for amplifying the genomic region for subsequent homology arm amplification, to avoid tricky PCR reactions with long/mutated/overhang-containing primers on genomic DNA directly."),
                         
                         tags$div(style = "text-align: center; margin: 30px 0;",
                                  tags$img(src = "Helpschematic.png", style = "max-width: 100%; height: auto; border: 1px solid #ddd; border-radius: 4px;")
                         ),
                         
                         tags$h3("Important Notes"),
                         tags$ul(
                           tags$li(tags$strong("Overhang addition:"), " When ordering primers, append the appropriate overhangs to your knock-in vector sequence to Primers 1-4. If Primer 2A or 3A exists, only append overhangs to Primers 2B and 3B (not to 2A and 3A)."),
                           tags$li(tags$strong("Lowercase bases:"), " These indicate silent mutations introduced to disrupt the PAM site and prevent Cas9 from cutting your knock-in vector."),
                           tags$li(tags$strong("Off-targets:"), " Check the off-target column to see the number of off-target sequences with up to 4 bases mismatched. Designs with fewer off-targets are preferred."),
                           tags$li(tags$strong("Distance to insert:"), " Shows how far the guide cuts from the desired insertion site."),
                           tags$li(tags$strong("Amplicon size:"), " Expected PCR product size."),
                           tags$li(tags$strong("Check before ordering:"), " While every effort has been made to ensure the predicted sequences are correct and specific, errors are possible, so always verify the sequences before ordering and beginning cloning experiments.")
                         ),
                         
                         tags$h3("Workflow Summary"),
                         tags$p(tags$strong("1.")," Design vector overhang sequences onto the 5' end of Primers 1-4 (try to keep total length below ~60bp to reduce cost)"),
                         tags$p(tags$strong("2.")," Order guide DNA and all required primers"),
                         tags$p(tags$strong("3a.")," (Optional) Amplify the genomic region using the Genotyping Primers"),
                         tags$p(tags$strong("3b.")," Amplify left and right homology arms using Primers 1+2 and Primers 3+4"),
                         tags$p(tags$strong("3c.")," If a Primer 2A/3A exists, amplify with Primers 1+2A or Primers 3A+4 first, then add the rest of the HA sequence using Primers 1+2B or Primers 3B+4"),
                         tags$p(tags$strong("4.")," Clone homology arms into your knock-in vector with appropriate overhangs (Gibson assembly)"),
                         tags$p(tags$strong("5.")," Inject the completed repair template into young adult worms along with the guide and Cas9 (on DNA vector or as in vitro translated guide RNA + Cas9 protein)"),
                         tags$p(tags$strong("6.")," Use genotyping primers to validate successful insertions")
                     )
                 )
             ),
             
             # Content section
             div(class = "content-section",
                 uiOutput("selected_gene_banner"),
                 conditionalPanel(condition = "output.has_selection",
                                  uiOutput("gene_metadata"),
                                  div(style = "margin-bottom: 10px;",
                                      tags$h4(style = "display: inline-block; margin: 0;", "Guide Details"),
                                      tags$span(class = "help-icon",
                                                tags$span(class = "tooltip-text", "Click on a row to visualize primers and guide RNA in the genome browser below. Click the ⊕ symbol to expand and see primer sequences."),
                                                "?")
                                  ),
                                  DTOutput("tbl"),
                                  br(),
                                  div(style = "margin-bottom: 10px;",
                                      tags$h4(style = "display: inline-block; margin: 0;", "Genome Browser"),
                                      tags$span(class = "help-icon",
                                                tags$span(class = "tooltip-text", "Interactive genome browser showing guide sequence with PAM site (green), homology arm primers (orange/red), and genotyping primers (purple). 
                                                   Click and drag to pan, double-click to zoom in. Click on guide/primer features to view genomic coordinates."),
                                                "?")
                                  ),
                                  div(id = "igv-container", class = "igv-wrap", style = "height: 400px;")
                 )
             )
    ),
    
    # ---- Tab 5: About ----
    tabPanel("About",
             fluidRow(
               column(
                 width = 8, offset = 2,
                 h2("About This Site"),
                 tags$p(HTML(glue::glue("
                            <p>
                            WormTagDB is a manually curated database of <i>C. elegans</i> genes that have had a fluorophore, epitope tag, degron tag, etc., knocked in at the native genomic locus to tag the endogenous protein.
                            </p>
                            <p>
                            To generate this database, we first performed a keyword-based Textpresso search to identify papers likely to contain endogenously tagged strains. 
                             After manually reviewing {n_papers_reviewed} published papers, we identified {n_papers_included} that performed such knock-ins, and extracted all reported endogenously tagged alleles. 
                             Analysis of the dataset's discovery curve indicates ~90% of all unique genes with published endogenous tags were captured.
                             This list was then supplemented with strains available through the CGC. We invite researchers to submit their alleles to the database to make it more complete.
                             </p>
                             <p>
                            In addition to cataloguing existing knock-in strains, we generated optimal guide RNA and homology-arm primer sequences for every <i>C. elegans</i> protein-coding gene, allowing other labs to bypass this tedious design step and immediately begin building new knock-ins.
                             <p>
                             The goal is to provide a comprehensive, searchable resource of strains and reagents for researchers interested in protein regulation, localization, and function <i>in vivo</i>. 
                             For more information contact Jake Leyhr (<a href='https://sites.duke.edu/sherwoodlab/' target='_blank'>Sherwood Lab @ Duke University</a>).
                            </p>
                            "
                 ))),
                 h4("Features"),
                 tags$ul(
                   tags$li("Search across allele, gene, tag fields"),
                   tags$li("Search by GO terms and human disease associations"),
                   tags$li("GO term enrichment analysis"),
                   tags$li("Graphs of summary statistics"),
                   tags$li("Guide and homology arm primer predictions for all genes"),
                   tags$li("Reserve genes you want to tag in the near future"),
                   tags$li("Submit your new alleles to the database")
                 ),
                 br(),
                 h6("Cite this app: (Publication coming soon)"),
                 h6(HTML(glue::glue("<a href='https://github.com/jakeleyhr/WormTagDB' target='_blank'>WormTagDB Shiny Code on GitHub</a>"))),
                 h6(HTML(glue::glue("<a href='https://github.com/jakeleyhr/CRISPR-Guide-and-Primer-Design-Pipeline-for-C.-elegans' target='_blank'>WormTagDB Guide and Primer Prediction Code on GitHub</a>"))),
                 br()
               )
             )
    ),
    
    # ---- Tab 6: Reservation ----
    tabPanel("Reserve Genes", value = "reserve_tab",
             fluidRow(
               column(
                 width = 10, offset = -0.5,
                 h3("Reserve Genes You Intend To Tag"),
                 HTML(glue::glue("
                              <p>
                                Plan to tag some currently untagged genes in the coming 3 months? Reserve them through this submission form so the community is aware.
                              </p>
                              <p>
                                (Text can be pasted from an Excel document if convenient). 
                               </p>
                              <p>
                                Add details of the type and location of tag you intend to tag each gene with, in addition to your lab information e.g. 'Sherwood Lab at Duke University'.
                              </p>
                              <p>
                                Check your email for a confirmation message. After a manual review of the information, we'll add it to the database in the form of an 'in progress' entry.
                              </p>
                            ")),
                 textInput("reserve_email", "Your Email Address (Required)", placeholder = "e.g. your@email.com"),
                 actionButton("add_row_reserve", "Add Row"),
                 actionButton("submit_entries_r", "Submit", class = "btn btn-primary"),
                 br(),
                 conditionalPanel(
                   condition = "output.submit_status_r !== ''",
                   tags$div(
                     style = "margin-top:10px; padding: 10px; border: 1px solid #a3d3a1; background-color: #eaf8ea; color: #2e7d32; border-radius: 5px;",
                     textOutput("submit_status_r")
                   )
                 ),                        
                 br(),
                 rHandsontableOutput("reservation_table")
               )
             )
    ),
    
    # ---- Tab 7: Submission ----
    tabPanel("Submit New Alleles", value = "submit_tab",
             fluidRow(
               column(
                 width = 11.5, offset = -0.5,
                 h3("Submit Endogenously Tagged Alleles"),
                 HTML(glue::glue("
                              <p>
                                Noticed an endogenously tagged allele missing from the database? Add as many details as possible to the submission form below and click submit.
                               </p>
                               <p>
                                (Text can be pasted from an Excel document if convenient). 
                               </p>
                               <p>
                               If an allele is unpublished, please provide your lab details (e.g. 'Sherwood Lab at Duke University') in the Publication field.
                               </p>
                               <p>
                                <strong>To report an attempted knock-in that was non-viable, describe the details in the comments field.</strong>
                              </p>
                              <p>
                              Check your email for a confirmation message. After a manual review of the information, we'll add it to the database. 
                              </p>
                            ")),
                 textInput("submit_email", "Your Email Address (Required)", placeholder = "e.g. your@email.com"),
                 actionButton("add_row_submit", "Add Row"),
                 actionButton("submit_entries", "Submit", class = "btn btn-primary"),
                 br(),
                 conditionalPanel(
                   condition = "output.submit_status !== ''",
                   tags$div(
                     style = "margin-top:10px; padding: 10px; border: 1px solid #a3d3a1; background-color: #eaf8ea; color: #2e7d32; border-radius: 5px;",
                     textOutput("submit_status")
                   )
                 ),                        
                 br(),
                 rHandsontableOutput("submission_table")
               )
             )
    )
  )
)


##### Define server logic  #####
server <- function(input, output, session) {
  
  ##### Helper functions for Guide Predictions page #####
  parse_locus <- function(locus) {
    if (is.null(locus) || is.na(locus) || !nzchar(locus)) {
      return(list(chr = NA_character_, start = NA_integer_, end = NA_integer_, strand = NA_character_))
    }
    s <- trimws(as.character(locus))
    m <- regexec("^\\s*([^:]+?)\\s*:\\s*([0-9,]+)\\s*-\\s*([0-9,]+)\\s*(?:\\(\\s*([^\\)]+)\\s*\\))?\\s*$", s, perl = TRUE)
    parts <- regmatches(s, m)[[1]]
    if (length(parts) == 0) stop(sprintf("Invalid locus format: '%s'", locus))
    chr <- parts[2]
    if (!grepl("^chr", chr, ignore.case = TRUE)) chr <- paste0("chr", chr)
    start <- as.integer(gsub(",", "", parts[3]))
    end   <- as.integer(gsub(",", "", parts[4]))
    if (!is.na(start) && !is.na(end) && start > end) {
      tmp <- start; start <- end; end <- tmp
    }
    raw_strand <- if (length(parts) >= 5 && nzchar(parts[5])) trimws(parts[5]) else NA_character_
    normalize_strand <- function(x) {
      if (is.na(x)) return(NA_character_)
      x_lower <- tolower(x)
      if (x_lower %in% c("+", "plus", "f", "forward", "for", "sense")) return("+")
      if (x_lower %in% c("-", "minus", "r", "reverse", "rev", "antisense")) return("-")
      if (x %in% c("+", "-")) return(x)
      NA_character_
    }
    strand <- normalize_strand(raw_strand)
    list(chr = chr, start = start, end = end, strand = strand)
  }
  
  compute_row_zoom_locus <- function(row, factor = 2, min_span = 400L) {
    loci <- lapply(c(row$genotyping_primer_left_locus, row$genotyping_primer_right_locus), parse_locus)
    guide_chr <- loci[[1]]$chr
    loci <- Filter(function(L) L$chr == guide_chr, loci)
    starts <- vapply(loci, function(L) L$start, integer(1))
    ends   <- vapply(loci, function(L) L$end,   integer(1))
    min_start <- min(starts, na.rm = TRUE)
    max_end   <- max(ends,   na.rm = TRUE)
    span <- max(1L, max_end - min_start)
    span <- max(span, min_span)
    center <- as.integer((min_start + max_end)/2)
    half_width <- as.integer((factor * span) / 2)
    new_start  <- max(1L, center - half_width)
    new_end    <- center + half_width
    paste0(guide_chr, ":", new_start, "-", new_end)
  }
  
  make_feature <- function(locus, name, strand, popup = NULL, color_hex = NULL) {
    L <- parse_locus(locus)
    feat <- list(chr = L$chr, start = L$start - 1L, end = L$end, name = name, strand = strand)
    if (!is.null(color_hex)) feat$color <- color_hex
    if (!is.null(popup)) feat$description <- popup
    feat
  }
  
  build_guide_features <- function(df) {
    if (!"guide_strand" %in% names(df)) df$guide_strand <- NA_character_
    purrr::pmap(.l = df[, c("WBGene", "gene", "guide_seq", "guide_locus", "guide_strand")],
                .f = function(WBGene, gene, guide_seq, guide_locus, guide_strand) {
                  nm <- paste0("GUIDE: ", as.character(gene))
                  strand_val <- NA_character_
                  if (!is.null(guide_strand) && !is.na(guide_strand) && nzchar(as.character(guide_strand))) {
                    strand_val <- as.character(guide_strand)
                  } else {
                    parsed <- tryCatch(parse_locus(guide_locus), error = function(e) NULL)
                    if (!is.null(parsed) && !is.na(parsed$strand)) strand_val <- parsed$strand else strand_val <- "+"
                  }
                  make_feature(guide_locus, nm, strand = strand_val, popup = paste0("5'-", guide_seq, "-3'"), color_hex = "#2CA02C")
                }
    )
  }
  
  build_primer_features <- function(df) {
    purrr::reduce(seq_len(nrow(df)), .init = list(), .f = function(acc, i) {
      row <- df[i, ]
      cols_locus <- c("primer_1_locus","primer_2_locus","primer_3_locus","primer_4_locus","primer_2a_locus","primer_3a_locus")
      cols_seq   <- c("primer_1_seq","primer_2_seq","primer_3_seq","primer_4_seq","primer_2a_seq","primer_3a_seq")
      primer_colors  <- c("orange","orange", "#FF6A36","#FF6A36","orange", "#FF6A36")
      has_2a <- ("primer_2a_locus" %in% names(row)) && !is.na(row[["primer_2a_locus"]]) && nzchar(as.character(row[["primer_2a_locus"]]))
      has_3a <- ("primer_3a_locus" %in% names(row)) && !is.na(row[["primer_3a_locus"]]) && nzchar(as.character(row[["primer_3a_locus"]]))
      for (k in seq_along(cols_locus)) {
        loc <- row[[cols_locus[k]]]
        if (is.na(loc) || !nzchar(loc)) next
        colname <- cols_locus[k]
        name_val <- switch(colname, "primer_1_locus" = "P1", "primer_2_locus" = if (has_2a) "P2B" else "P2",
                           "primer_2a_locus" = "P2A", "primer_3_locus" = if (has_3a) "P3B" else "P3",
                           "primer_3a_locus" = "P3A", "primer_4_locus" = "P4", paste0("P", k))
        parsed <- tryCatch(parse_locus(as.character(loc)), error = function(e) NULL)
        strand_val <- if (!is.null(parsed) && !is.na(parsed$strand)) parsed$strand else "+"
        seq_val <- if (cols_seq[k] %in% names(row)) as.character(row[[cols_seq[k]]]) else NA_character_
        popup_text <- if (!is.na(seq_val) && nzchar(seq_val)) paste0("5'-", seq_val, "-3'") else NULL
        acc <- append(acc, list(make_feature(locus = loc, name = name_val, strand = strand_val, popup = popup_text, color_hex = primer_colors[k])))
      }
      acc
    })
  }
  
  build_genotyping_features <- function(df) {
    purrr::reduce(seq_len(nrow(df)), .init = list(), .f = function(acc, i) {
      row <- df[i, ]
      cols_locus <- c("genotyping_primer_left_locus", "genotyping_primer_right_locus")
      cols_seq   <- c("genotyping_primer_left_seq",   "genotyping_primer_right_seq")
      strands_fallback <- c("+", "-")
      colors <- c("#6A51A3", "#6A51A3")
      for (k in seq_along(cols_locus)) {
        loc <- row[[cols_locus[k]]]
        if (is.na(loc) || !nzchar(loc)) next
        parsed <- tryCatch(parse_locus(as.character(loc)), error = function(e) NULL)
        strand_val <- if (!is.null(parsed) && !is.na(parsed$strand)) parsed$strand else strands_fallback[k]
        seq_val <- if (cols_seq[k] %in% names(row)) as.character(row[[cols_seq[k]]]) else NA_character_
        popup_text <- if (!is.na(seq_val) && nzchar(seq_val)) paste0("5'-", seq_val, "-3'") else NULL
        acc <- append(acc, list(make_feature(locus = loc, name = if (k == 1) "Genotyping Primer F" else "Genotyping Primer R", strand = strand_val, popup = popup_text, color_hex = colors[k])))
      }
      acc
    })
  }
  
  # Load designs data for Guide Predictions page
  designs <- tryCatch({
    guide_predictions
  }, error = function(e) {
    data.frame()
  })
  
  # Prepare clean display dataframe
  df_clean <- if (nrow(designs) > 0) {
    designs %>%
      mutate(
        Terminal = ifelse(!is.na(terminal) & nzchar(as.character(terminal)), as.character(terminal), "N/A"),
        `Guide Sequence` = ifelse(!is.na(guide_seq) & nzchar(as.character(guide_seq)), 
                                  paste0("5'-", substr(as.character(guide_seq), 1, nchar(as.character(guide_seq)) - 3), "-3'"), "N/A"),
        `Off-Targets` = ifelse(!is.na(off_target_count), as.character(off_target_count), "N/A"),
        Transcription = ifelse(!is.na(transcription_type) & nzchar(as.character(transcription_type)), 
                               as.character(transcription_type), "N/A"),
        `Distance to Insert` = ifelse(!is.na(distance_to_insert), paste(as.character(distance_to_insert), "bp"), "N/A")
      ) %>%
      rowwise() %>%
      mutate(
        ` ` = as.character(icon),
        PrimerHTML = {
          p1 <- if (!is.na(primer_1_seq) && nzchar(as.character(primer_1_seq))) 
            paste0("<strong>P1:</strong> 5'-", as.character(primer_1_seq), "-3'<br>") else ""
          p2 <- if (!is.na(primer_2_seq) && nzchar(as.character(primer_2_seq))) 
            paste0("<strong>P2:</strong> 5'-", as.character(primer_2_seq), "-3'<br>") else ""
          p2a <- if (!is.na(primer_2a_seq) && nzchar(as.character(primer_2a_seq))) 
            paste0("<strong>P2A:</strong> 5'-", as.character(primer_2a_seq), "-3'<br>") else ""
          p3 <- if (!is.na(primer_3_seq) && nzchar(as.character(primer_3_seq))) 
            paste0("<strong>P3:</strong> 5'-", as.character(primer_3_seq), "-3'<br>") else ""
          p3a <- if (!is.na(primer_3a_seq) && nzchar(as.character(primer_3a_seq))) 
            paste0("<strong>P3A:</strong> 5'-", as.character(primer_3a_seq), "-3'<br>") else ""
          p4 <- if (!is.na(primer_4_seq) && nzchar(as.character(primer_4_seq))) 
            paste0("<strong>P4:</strong> 5'-", as.character(primer_4_seq), "-3'<br>") else ""
          gL <- if (!is.na(genotyping_primer_left_seq) && nzchar(as.character(genotyping_primer_left_seq))) 
            paste0("<strong>Genotyping F:</strong> 5'-", as.character(genotyping_primer_left_seq), "-3'<br>") else ""
          gR <- if (!is.na(genotyping_primer_right_seq) && nzchar(as.character(genotyping_primer_right_seq))) 
            paste0("<strong>Genotyping R:</strong> 5'-", as.character(genotyping_primer_right_seq), "-3'<br>") else ""
          paste0(p1, p2, p2a, p3, p3a, p4, gL, gR)
        },
        Gene = as.character(gene),
        WBID = as.character(WBGene)
      ) %>%
      ungroup() %>%
      select(` `, PrimerHTML, Terminal, `Guide Sequence`, `Off-Targets`, Transcription, `Distance to Insert`, Gene, WBID)
  } else {
    data.frame()
  }
  
  # Reactive value to store selected gene
  selected_gene <- reactiveVal(NULL)
  
  ##### Guide Predictions page outputs #####
  output$has_selection <- reactive({ !is.null(selected_gene()) })
  outputOptions(output, "has_selection", suspendWhenHidden = FALSE)
  
  output$selected_gene_banner <- renderUI({
    # Don't show banner anymore - moved to gene_metadata
    return(NULL)
  })
  
  output$gene_metadata <- renderUI({
    req(selected_gene())
    gene_col <- if ("gene" %in% names(designs)) designs$gene else character(0)
    wbgene_col <- if ("WBGene" %in% names(designs)) designs$WBGene else character(0)
    matching_rows <- which(gene_col == selected_gene()$gene & wbgene_col == selected_gene()$WBGene)
    if (length(matching_rows) == 0) return(NULL)
    first_row <- designs[matching_rows[1], ]
    
    div(style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
        # Gene name as title with WormBase ID
        div(style = "margin-top: 0; margin-bottom: 15px;",
            tags$span(first_row$gene, style = "font-size: 1.75em; font-weight: bold; color: #333;"),
            tags$span(paste0(" (", first_row$WBGene, ")"), style = "font-size: 1em; color: #666;")
        ),
        
        div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
            # Left column
            div(style = "display: grid; grid-template-columns: 120px 1fr; gap: 10px; align-items: center;",
                div(style = "display: flex; align-items: center;",
                    tags$strong("Transcript:"),
                    tags$span(class = "help-icon",
                              tags$span(class = "tooltip-text", "The specific transcript isoform considered for this knock-in design."),
                              "?")
                ), tags$span(first_row$Transcript),
                div(style = "display: flex; align-items: center;",
                    tags$strong("Protein Length:"),
                    tags$span(class = "help-icon",
                              tags$span(class = "tooltip-text", "Length of the mature protein in amino acids, considering any signal peptides or transit peptides that are cleaved."),
                              "?")
                ), tags$span(first_row$Protein_Length),
                div(style = "display: flex; align-items: center;",
                    tags$strong("UniProt ID:"),
                    tags$span(class = "help-icon",
                              tags$span(class = "tooltip-text", "UniProt database identifier for the protein product considered for this knock-in design."),
                              "?")
                ), tags$span(first_row$Uniprot_ID)
            ),
            # Right column
            div(style = "display: grid; grid-template-columns: 120px 1fr; gap: 10px; align-items: center;",
                div(style = "display: flex; align-items: center;",
                    tags$strong("Chain Type:"),
                    tags$span(class = "help-icon",
                              tags$span(class = "tooltip-text", "Classification of the mature protein structure: full or chain. Chain indicates that some N/C-terminal residues are cleaved off in the mature protein."),
                              "?")
                ), tags$span(first_row$Chain_Type),
                div(style = "display: flex; align-items: center;",
                    tags$strong("Chain Reason:"),
                    tags$span(class = "help-icon",
                              tags$span(class = "tooltip-text", "Explanation for the chain type classification."),
                              "?")
                ), tags$span(first_row$Chain_Reason),
                div(style = "display: flex; align-items: center;",
                    tags$strong("Warning:"),
                    tags$span(class = "help-icon",
                              tags$span(class = "tooltip-text", "Cautions about predicted protein features that may be relevant to knock-in design."),
                              "?")
                ), tags$span(ifelse(is.na(first_row$Warning) || first_row$Warning == "", "None", first_row$Warning))
            )
        )
    )
  })
  
  search_results <- reactive({
    query <- input$search_query
    if (is.null(query) || query == "") return(NULL)
    query_lower <- tolower(query)
    gene_col <- if ("gene" %in% names(designs)) designs$gene else character(0)
    wbgene_col <- if ("WBGene" %in% names(designs)) designs$WBGene else character(0)
    match_indices <- which(grepl(query_lower, tolower(gene_col), fixed = TRUE) | grepl(query_lower, tolower(wbgene_col), fixed = TRUE))
    if (length(match_indices) == 0) return(NULL)
    data.frame(gene = gene_col[match_indices], WBGene = wbgene_col[match_indices], stringsAsFactors = FALSE) %>% distinct()
  })
  
  output$search_results_ui <- renderUI({
    results <- search_results()
    if (is.null(results) || nrow(results) == 0) return(NULL)
    if (is.null(input$search_query) || input$search_query == "") return(NULL)
    
    result_items <- lapply(1:nrow(results), function(i) {
      tags$div(class = "result-item",
               onclick = sprintf("Shiny.setInputValue('gene_selected', {gene: '%s', wbid: '%s'}, {priority: 'event'})", results$gene[i], results$WBGene[i]),
               span(class = "gene-name", results$gene[i]), span(class = "wbid", results$WBGene[i]))
    })
    div(class = "search-results", result_items)
  })
  
  observeEvent(input$gene_selected, {
    req(input$gene_selected)
    selected_gene(list(gene = input$gene_selected$gene, WBGene = input$gene_selected$wbid))
    updateTextInput(session, "search_query", value = "")
  })
  
  output$tbl <- DT::renderDT({
    req(selected_gene())
    gene_data <- df_clean
    if ("Gene" %in% names(df_clean) && "WBID" %in% names(df_clean)) {
      gene_matches <- df_clean[["Gene"]] == selected_gene()$gene
      wbid_matches <- df_clean[["WBID"]] == selected_gene()$WBGene
      gene_data <- df_clean[gene_matches & wbid_matches, ]
    }
    if (nrow(gene_data) == 0) return(datatable(data.frame()))
    
    primer_html <- gene_data[[" "]]
    display_data <- data.frame(
      ` ` = rep("&oplus;", nrow(gene_data)),
      PrimerHTML = primer_html,
      Terminal = gene_data$Terminal,
      `Guide Sequence` = gene_data$`Guide Sequence`,
      `Off-Targets` = gene_data$`Off-Targets`,
      Transcription = gene_data$Transcription,
      `Distance to Insert` = gene_data$`Distance to Insert`,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    
    DT::datatable(display_data, escape = FALSE, rownames = FALSE, selection = "single",
                  options = list(pageLength = 10, scrollX = TRUE, dom = 't',
                                 columnDefs = list(list(className = 'dt-control', targets = 0, orderable = FALSE, width = '30px'),
                                                   list(visible = FALSE, targets = 1))),
                  callback = JS("
        $(table.table().container()).on('click', 'td.dt-control', function(e) {
          e.stopPropagation();
          
          var td = $(this);
          var tr = td.closest('tr');
          
          if (tr.hasClass('child')) {
            return;
          }
          
          var row = table.row(tr); 
          
          if (row.child.isShown()) { 
            row.child.hide();
            tr.find('td.dt-control').html('&oplus;');
            tr.removeClass('shown');
          } else { 
            table.rows().every(function() {
              if (this.child.isShown()) {
                var otherTr = $(this.node());
                this.child.hide();
                otherTr.find('td.dt-control').html('&oplus;');
                otherTr.removeClass('shown');
              }
            });
            
            var data = row.data();
            var childContent = $('<div style=\"padding:10px;\">' + data[1] + '</div>');
            row.child(childContent).show();
            tr.find('td.dt-control').html('&ominus;');
            tr.addClass('shown');
          } 
        });
      ")
    )
  })
  
  observeEvent(input$tbl_rows_selected, {
    req(selected_gene())
    idx <- input$tbl_rows_selected
    req(idx)
    gene_data <- df_clean
    if ("Gene" %in% names(df_clean) && "WBID" %in% names(df_clean)) {
      gene_matches <- df_clean[["Gene"]] == selected_gene()$gene
      wbid_matches <- df_clean[["WBID"]] == selected_gene()$WBGene
      gene_data <- df_clean[gene_matches & wbid_matches, ]
    }
    if (nrow(gene_data) < idx) return()
    selected_row <- gene_data[idx, ]
    guide_seq <- gsub("5'-|-3'", "", selected_row[["Guide Sequence"]])
    gene_col <- if ("gene" %in% names(designs)) designs$gene else character(0)
    wbgene_col <- if ("WBGene" %in% names(designs)) designs$WBGene else character(0)
    guide_seq_col <- if ("guide_seq" %in% names(designs)) designs$guide_seq else character(0)
    guide_seq_col_trimmed <- substr(guide_seq_col, 1, nchar(guide_seq_col) - 3)
    matching_idx <- which(gene_col == selected_gene()$gene & wbgene_col == selected_gene()$WBGene & guide_seq_col_trimmed == guide_seq)
    if (length(matching_idx) == 0) return()
    row <- designs[matching_idx[1], , drop = FALSE]
    locus <- compute_row_zoom_locus(row, factor = 1.3)
    session$sendCustomMessage("igv-close-popups", TRUE)
    session$sendCustomMessage("igv-goto", locus)
    guide_feats <- build_guide_features(row)
    primer_feats <- build_primer_features(row)
    genotyping_feats <- build_genotyping_features(row)
    session$sendCustomMessage("igv-load-track", list(name = "Guides", features = guide_feats, displayMode = "EXPANDED", trackHeight = 35))
    session$sendCustomMessage("igv-load-track", list(name = "Primers", features = primer_feats, displayMode = "EXPANDED", trackHeight = 70))
    session$sendCustomMessage("igv-load-track", list(name = "Genotyping", features = genotyping_feats, displayMode = "EXPANDED", trackHeight = 35))
  })
  
  observeEvent(selected_gene(), {
    req(selected_gene())
    session$sendCustomMessage("init-igv", list(dummy = TRUE))
    Sys.sleep(0.5)
    tryCatch({ DT::dataTableProxy("tbl") %>% DT::selectRows(1) }, error = function(e) {})
  }, ignoreInit = TRUE)
  
  ##### Reactive filtered data for allele search #####
  filtered_data <- reactive({
    req(input$search_columns)
    term <- tolower(input$search_term)
    
    df <- combined_strains_with_GO
    
    # Return full table if no term is entered
    if (term == "") return(df)
    
    # Logical matrix for each column
    match_matrix <- sapply(input$search_columns, function(col) {
      col_data <- tolower(as.character(df[[col]]))
      # Handle NA safely and suppress warnings
      str_detect(ifelse(is.na(col_data), "", col_data), fixed(term))
    })
    
    # Convert to logical vector (TRUE if any column matches)
    matched_rows <- rowSums(match_matrix) > 0
    
    df[matched_rows, , drop = FALSE]
  })
  
  # Render the strain/allele data table
  output$search_results <- DT::renderDT({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(datatable(df))
    }
    
    # Build expandable HTML block per row
    df <- df %>%
      mutate(`GO Details` = paste0(
        # Comments at the top only if non-empty
        ifelse(!is.na(Comments) & nzchar(trimws(Comments)),
               paste0("<b>Comments:</b><br>", Comments, "<br><br>"), ""),
        "<b>Publication:</b><br>", dplyr::coalesce(Publication, ""), "<br><br>",
        "<b>Accession:</b><br>", dplyr::coalesce(Accession, ""), "<br><br>",
        "<b>GO Terms:</b><ul><li>",
        gsub(";", "</li><li>", dplyr::coalesce(`GO Term Names`, "")),
        "</li></ul>",
        "<b>Human Orthologs:</b><ul><li>",
        gsub(",", "</li><li>", dplyr::coalesce(`Human_Gene`, "")),
        "</li></ul>",
        "<b>Human Disease Associations:</b><ul><li>",
        gsub(";", "</li><li>", dplyr::coalesce(`DOtermName`, "")),
        "</li></ul>"
      ))
    
    # Remove detailed columns from visible view
    df_clean <- df %>%
      dplyr::select(`GO Details`, Strain, Genotype, Allele, Gene, Fluor, `Other tags`, `Tag Location`, Year, `WormBase ID`) %>%
      mutate(` ` = "&oplus;") %>%
      dplyr::select(` `, everything())  # move expansion icon first
    
    datatable(
      df_clean,
      escape = FALSE,
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        dom = 'Bfrtip',
        columnDefs = list(
          list(className = 'dt-control', targets = 0),
          list(orderable = FALSE, targets = 0),
          list(visible = FALSE, targets = 1)  # hide 'GO Details' column
        )
      ),
      callback = JS("
          table.on('click', 'tbody tr', function() {
            var tr = $(this);
            var row = table.row(tr);
            var iconCell = tr.find('td.dt-control');
        
            // Skip if the clicked row is a child row
            if (tr.hasClass('child')) return;
        
            if (row.child.isShown()) {
              row.child.hide();
              iconCell.html('&oplus;');
            } else {
              var detail_html = row.data()[1];  // Column index 1 = 'GO Details'
              row.child('<div style=\"padding:10px\">' + detail_html + '</div>').show();
              iconCell.html('&ominus;');
            }
          });
        "),
      selection = "none",
      rownames = FALSE
    )
  })
  
  output$download_search_csv <- downloadHandler(
    filename = function() {
      paste0("filtered_strains_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- filtered_data()
      if (is.null(df) || nrow(df) == 0) {
        write.csv(data.frame(Message = "No results to export."), file, row.names = FALSE)
      } else {
        safe_cols <- c("Strain", "Genotype", "Allele", "Gene", "Fluor", "Other tags", "Tag Location", "WormBase ID", "Publication", "Accession", "GO Term Names")
        df_export <- df[, intersect(safe_cols, colnames(df)), drop = FALSE]
        readr::write_csv(df_export, file)
      }
    }
  )
  outputOptions(output, "download_search_csv", suspendWhenHidden = FALSE)
  
  
  ##### Render the GO term enrichment plot #####
  output$GOtermplot <- renderPlotly({
    # Map each GO term to its parent (from rrvgo join)
    # GO_with_parents must have: GO_Term_ID, parentTerm, Ontology
    sel <- selected_parent()
    GO_terms_flagged <- GO_merged_final %>%
      left_join(
        GO_with_parents %>% distinct(GO_Term_ID, parentTerm, Ontology),
        by = "GO_Term_ID"
      ) %>%
      mutate(
        SelectedChild = if (!is.null(sel)) parentTerm == sel$parentTerm & Ontology == sel$Ontology else FALSE
      ) %>%
      filter(!(Count == 1 & is.na(Tagged_Count))) #️ drop rows where Count == 0 and Tagged_Count is NA
    
    p <- ggplot(
      GO_terms_flagged,
      aes(
        x = log2_or, 
        y = -log10(FDR),
        size = Count,
        text = paste0(
          "GO Term: ", GO_Term_Name, "\n",
          "Total Genes: ", Count, "\n",
          "Tagged Genes: ", Tagged_Count, "\n",
          "Proportion Tagged: ", round(Proportion_Tagged * 100, 1), "%\n",
          "log2 OR: ", round(log2_or, 2), "\n",
          "FDR: ", signif(FDR, 3)
        ),
        color = SelectedChild,
        alpha = SelectedChild
      )
    ) + annotate(
      "text",
      x = -5, y = 90,
      label = paste0(
        "GO terms with ≥1 tagged gene: ", nrow(GO_merged_final) - n_zero_tagged, "\n",
        "GO terms with 0 tagged genes: ", n_zero_tagged
      ),
      hjust = 0, vjust = 1, color = "black", size = 3
    ) +
      geom_vline(xintercept = -1, linetype = "dashed", color = "red") +
      geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
      geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "blue") +  # FDR significance line
      geom_point() +
      scale_size_continuous(name = "Total Genes\nin Parent Category", range = c(0.1, 10)) +
      scale_color_manual(values = c(`TRUE` = "red", `FALSE` = "gray")) +
      scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.6)) +
      guides(color = "none", alpha = "none") +
      labs(
        x = "log2(Odds Ratio)",
        y = "-log10(FDR adjusted P-value)",
        #title = "GO Terms: Enrichment or Underrepresentation"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = c(0.02, 0.98),
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = alpha("white", 0.7), color = "gray"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      ) +
      scale_x_continuous(
        limits = c(-7, 7),
        breaks = seq(-6, 6, by = 2),
        minor_breaks = NULL
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  selected_parent <- reactiveVal(NULL)
  GO_parent_summary_sel <- reactive({
    sel <- selected_parent()
    GO_parent_summary %>%
      dplyr::mutate(
        Selected = if (!is.null(sel)) (parentTerm == sel$parentTerm & Ontology == sel$Ontology) else FALSE
      )  %>%
      filter(!(Total_Genes == 1 & is.na(Total_Tagged))) #️ drop rows where Count == 0 and Tagged_Count is NA
  })
  # Render the GO parent enrichment plot
  output$GOparentplot <- renderPlotly({
    ontology_labels <- c(BP="Biological Process", CC="Cellular Component", MF="Molecular Function")
    df <- GO_parent_summary_sel()
    
    p_parent <- ggplot(
      df,
      aes(
        x = log2_or,
        y = -log10(FDR),
        size = Total_Genes,
        key  = paste(parentTerm, Ontology, sep = "||"),
        text = paste0(
          "Parent term: ", parentTerm, "\n",
          "Child GO terms merged: ", Num_GO_terms, "\n",
          "Total genes: ", Total_Genes, "\n",
          "Tagged genes: ", Total_Tagged, "\n",
          "Proportion tagged: ", round(Proportion_Tagged_parent * 100, 1), "%\n",
          "log2 OR: ", round(log2_or, 2), "\n",
          "FDR: ", signif(FDR, 3)  #
        ),
        color = Selected,
        alpha = Selected
      )
    ) +
      geom_vline(xintercept = -1, linetype = "dashed", color = "red") +
      geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
      geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "blue") +  # FDR significance line
      geom_point() +
      scale_size_continuous(name = "Total Genes\nin Parent Category", range = c(0.1, 10)) +
      scale_color_manual(values = c(`TRUE` = "red", `FALSE` = "gray")) +
      scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.6)) +
      guides(color = "none", alpha = "none") +
      labs(
        x = "log2(Odds Ratio)",
        y = "-log10(FDR adjusted P-value)",
        #title = "GO Parent Categories: Enrichment or Underrepresentation"
      ) +
      facet_wrap(~ Ontology, nrow = 1, labeller = labeller(Ontology = ontology_labels)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      scale_x_continuous(
        limits = c(-7, 7),
        breaks = seq(-6, 6, by = 2),
        minor_breaks = NULL
      )
    
    ggplotly(p_parent, tooltip = "text", source = "go_parents")
  })
  
  observeEvent(plotly::event_data("plotly_click", source = "go_parents"), {
    ed <- plotly::event_data("plotly_click", source = "go_parents")
    req(!is.null(ed$key))
    parts <- strsplit(ed$key, "\\|\\|")[[1]]
    selected_parent(list(parentTerm = parts[1], Ontology = parts[2]))
  })
  
  observeEvent(input$clear_parent, {
    selected_parent(NULL)   # this will remove the highlight
  })
  
  output$go_table_title <- renderUI({
    sel <- selected_parent()
    
    # No selection: show default title
    if (is.null(sel)) return(h5("Table of GO Terms: All"))
    
    # Try to pull parent-level stats from GO_parent_summary (fast)
    ps <- GO_parent_summary %>%
      dplyr::filter(parentTerm == sel$parentTerm, Ontology == sel$Ontology)
    
    if (nrow(ps) > 0) {
      n_children   <- ps$Num_GO_terms[1]
      total_genes  <- ps$Total_Genes[1]
      total_tagged <- ps$Total_Tagged[1]
      prop_tagged  <- ps$Proportion_Tagged_parent[1]
    } else {
      # Fallback: compute from GO_with_parents on the fly
      kids <- GO_with_parents %>%
        dplyr::filter(parentTerm == sel$parentTerm, Ontology == sel$Ontology)
      n_children   <- dplyr::n_distinct(kids$GO_Term_ID)
      total_genes  <- sum(kids$Count, na.rm = TRUE)
      total_tagged <- sum(kids$Tagged_Count, na.rm = TRUE)
      prop_tagged  <- ifelse(total_genes > 0, total_tagged / total_genes, NA_real_)
    }
    
    # Nicely formatted dynamic title
    tags$h5(
      HTML(paste0(
        "Table of GO Terms: ",
        "<span style='font-weight:600;'>", sel$parentTerm, "</span>",
        " <span style='color:#666;'>(", sel$Ontology, ")</span>",
        " — Child terms: <span style='font-weight:600;'>", n_children, "</span>",
        " | Tagged: <span style='font-weight:600;'>", formatC(total_tagged, big.mark=","), "</span>",
        " / ", formatC(total_genes, big.mark=","),
        " (", ifelse(is.na(prop_tagged), "NA", paste0(round(100*prop_tagged, 1), "%")), ")"
      ))
    )
  })
  
  output$go_enrichment_table <- DT::renderDT({
    # join a parent mapping so we can filter by the clicked bubble
    df_base <- GO_combined %>%
      left_join(
        GO_with_parents %>% 
          distinct(GO_Term_ID, parentTerm, Ontology),
        by = "GO_Term_ID"
      )
    
    # if a parent is selected from the plot, filter to its child terms
    sel <- selected_parent()
    if (!is.null(sel)) {
      df_base <- df_base %>%
        filter(parentTerm == sel$parentTerm, Ontology == sel$Ontology)
    }
    
    # pretty column names
    df_base <- df_base %>%
      rename_with(~ gsub("_", " ", .x))
    
    # sort by gene count
    df <- df_base %>%
      arrange(desc(`Gene Count`))
    
    if (nrow(df) == 0) return(datatable(df))
    
    # build expandable details
    df <- df %>%
      mutate(
        `GO Details` = paste0(
          "<div style='padding-bottom:8px'><b>GO Term Type:</b><br>", Ontology, "</div>",
          "<div style='padding-bottom:8px'><b>GO Term Definition:</b><br>", Definition, "</div>",
          "<div style='padding-bottom:8px'><b>Parent GO Term:</b><br>", parentTerm, "</div>",
          
          "<div style='display: flex; gap: 10px;'>",
          "<div style='flex: 1; word-wrap: break-word;'><b>Tagged Genes:</b><br><div style='white-space: normal;'>",
          ifelse(`Tagged Genes List` == "" | is.na(`Tagged Genes List`), "(None)", gsub(", ", ", ", `Tagged Genes List`)),
          "</div></div>",
          "<div style='flex: 0.5; word-wrap: break-word;'><b>In Progress Genes:</b><br><div style='white-space: normal;'>",
          ifelse(`In Progress Genes List` == "" | is.na(`In Progress Genes List`), "(None)", gsub(", ", ", ", `In Progress Genes List`)),
          "</div></div>",
          "<div style='flex: 1; word-wrap: break-word;'><b>Untagged Genes:</b><br><div style='white-space: normal;'>",
          ifelse(`Untagged Genes List` == "" | is.na(`Untagged Genes List`), "(None)", gsub(", ", ", ", `Untagged Genes List`)),
          "</div></div>",
          "</div>"
        )
      )
    
    # visible columns
    df_clean <- df %>%
      dplyr::select(
        `GO Details`, `GO Term ID`, `GO Term Name`,
        `Gene Count`, `Tagged Count`, `In Progress Count`, `Untagged Count`,
        `Proportion Tagged`, `log2 or`, `FDR`
      ) %>%
      mutate(
        `Proportion Tagged` = ifelse(is.na(`Proportion Tagged`), NA, round(`Proportion Tagged`, 2)),
        `log2 or` = ifelse(is.na(`log2 or`), NA, round(`log2 or`, 2)),
        ` ` = "&oplus;"
      ) %>%
      dplyr::rename(`FDR P-value` = `FDR`) %>%
      dplyr::select(` `, everything())
    
    DT::datatable(
      df_clean,
      escape = FALSE,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip',
        columnDefs = list(
          list(className = 'dt-control', targets = 0),
          list(orderable = FALSE, targets = 0),
          list(visible = FALSE, targets = 1)  # GO Details column
        )
      ),
      callback = JS(
        "table.on('click', 'tbody tr', function() {
      var tr = $(this);
      var row = table.row(tr);
      var iconCell = tr.find('td.dt-control');
      if (tr.hasClass('child')) return;
      if (row.child.isShown()) {
        row.child.hide();
        iconCell.html('&oplus;');
      } else {
        var data = row.data();
        var detail_html = data[1];
        row.child('<div style=\"padding:10px\">' + detail_html + '</div>').show();
        iconCell.html('&ominus;');
      }
    });"
      ),
      selection = "none",
      rownames = FALSE
    ) %>%
      DT::formatSignif("FDR P-value", digits = 2) %>%   # display like 1e-20, keeps numeric sorting
      DT::formatStyle("FDR P-value", whiteSpace = "nowrap")
  })
  

  ##### Reserve genes #####
  # Define reservation table once
  required_cols_r <- c("Gene","Fluor","Other_tags","Location","Lab","Comments")
  
  normalize_df_r <- function(df) {
    # coerce to character and add missing columns
    for (nm in required_cols_r) if (!nm %in% names(df)) df[[nm]] <- ""
    df[required_cols_r]
  }
  
  # Initialize reservation data and message
  reservation_data <- reactiveVal(data.frame(
    Gene = "", Fluor = "",
    Other_tags = "", Location = "", Lab = "", Comments = "",
    stringsAsFactors = FALSE
  ))
  
  submit_status_r <- reactiveVal("")
  
  # Render the editable reservation table WITH placeholders
  output$reservation_table <- renderRHandsontable({
    # use your normalizer for reservation data (rename if yours is normalize_df_r)
    df <- normalize_df_r(reservation_data())
    df[is.na(df)] <- ""   # placeholders show only on ""
    
    # Column-wise placeholder texts (reservation fields only)
    placeholders <- c(
      Gene        = "e.g. unc-59",
      Fluor       = "e.g. GFP",
      Other_tags  = "e.g. 3xFLAG; AID",
      Location    = "e.g. C-term",
      Lab         = "e.g. Sherwood Lab at Duke University",
      Comments    = "notes, timeline, etc."
    )
    
    tbl <- rhandsontable(df, rowHeaders = NULL) %>%
      hot_table(
        stretchH        = "all",
        minSpareRows    = 1,     # keep one empty row so placeholders are visible
        contextMenu     = TRUE,
        manualRowMove   = TRUE,
        manualColumnMove= TRUE
      )
    
    # Apply placeholders programmatically
    for (nm in names(placeholders)) {
      if (nm %in% names(df)) {
        tbl <- hot_col(
          tbl, nm,
          placeholder = placeholders[[nm]],
          wordWrap = (nm == "Comments")
        )
      }
    }
    tbl
  })
  
  # Add row logic
  observeEvent(input$add_row_reserve, {
    current_data <- if (!is.null(hot_to_r(input$reservation_table))) {
      hot_to_r(input$reservation_table)
    } else {
      reservation_data()
    }
    current_data <- normalize_df_r(current_data)
    
    new_row <- as.data.frame(
      setNames(as.list(rep("", length(required_cols_r))), required_cols_r),
      stringsAsFactors = FALSE
    )
    
    reservation_data(normalize_df_r(rbind(current_data, new_row)))
  })
  
  # Keep reservation updated with any table edits
  observeEvent(input$reservation_table, {
    req(input$reservation_table)
    df <- rhandsontable::hot_to_r(input$reservation_table)
    reservation_data(normalize_df_r(df))
  }, ignoreInit = TRUE)
  

  # Send emails
  observeEvent(input$submit_entries_r, {
    message("========================================")
    message("[DEBUG] Reserve button clicked!")
    message("[DEBUG] Email input: ", input$reserve_email)
    message("========================================")
    
    df <- reservation_data()
    user_email <- input$reserve_email
    
    message("[DEBUG] Got reservation data, rows: ", nrow(df))
    
    # Basic email format validation
    if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", user_email)) {
      message("[DEBUG] Email validation FAILED")
      submit_status_r("❌ Invalid email address. Please enter a valid email.")
      return()
    }
    
    message("[DEBUG] Email validation passed")
    
    nonempty <- apply(df, 1, function(x) any(nzchar(trimws(as.character(x)))))
    df_clean <- df[nonempty, , drop = FALSE]
    
    message("[DEBUG] Clean rows: ", nrow(df_clean))
    
    # error if the table has no real entries
    if (nrow(df_clean) == 0) {
      message("[DEBUG] No entries to submit")
      submit_status_r("❌ No entries to submit. Please add at least one row to the table.")
      return()
    }
    
    submit_status_r("Please wait...")
    
    shinybusy::show_modal_spinner(text = "Sending reservation...")
    on.exit(shinybusy::remove_modal_spinner(), add = TRUE)
    
    html_table <- paste(as.character(knitr::kable(df_clean, format = "html")), collapse = "\n")
    
    # Email to user
    message("[reserve] sending to user: ", user_email)
    user_ok <- send_email(
      to = user_email,
      subject = "Your WormTagDB reservation",
      html_body = glue::glue("
      <p>Thank you for your gene reservations in WormTagDB!</p>
      <p>Here is the information you provided about the genes you intend to tag:</p>
      {html_table}
    ")
    )
    message("[reserve] user_ok: ", user_ok)
    
    # Email to admin
    message("[reserve] sending to admin")
    admin_ok <- send_email(
      to = "wormtagdb@gmail.com",
      subject = "New WormTagDB reservation",
      html_body = glue::glue("
      <p>New reservation received from: {user_email}</p>
      {html_table}
    ")
    )
    message("[reserve] admin_ok: ", admin_ok)
    message("[reserve] Emails done.")
    
    # Final message
    if (user_ok && admin_ok) {
      submit_status_r("✅ Reservation received. A confirmation email has been sent.")
    } else if (user_ok && !admin_ok) {
      submit_status_r("⚠️ Reservation failed.")
    } else {
      submit_status_r("✅  Local test reservation received. Check local_submissions folder.")
    }
  })
  

  # Show the thank you message
  output$submit_status_r <- renderText({
    submit_status_r()
  })
  
  # Make it reactive for conditionalPanel
  outputOptions(output, "submit_status_r", suspendWhenHidden = FALSE)
  
  
  ##### Submit new alleles #####
  # Define submission table once
  required_cols <- c("Gene","Allele","Fluor","Other_tags", "Genotype", "Location","Publication", "Date", "Comments")
  
  normalize_df <- function(df) {
    # coerce to character and add missing columns
    for (nm in required_cols) if (!nm %in% names(df)) df[[nm]] <- ""
    df[required_cols]
  }
  
  # Initialize submission data and message
  submission_data <- reactiveVal(normalize_df(data.frame(
    Gene = "", Allele = "", Fluor = "",
    Other_tags = "", Location = "", Publication = "", Date = "", Comments = "",
    stringsAsFactors = FALSE
  )))
  
  submit_status <- reactiveVal("")
  
  # Render the editable table
  output$submission_table <- renderRHandsontable({
    df <- normalize_df(submission_data())
    df[is.na(df)] <- ""   # placeholders show only on ""
    
    # Column-wise placeholder texts
    placeholders <- c(
      Gene        = "e.g. unc-59",
      Allele      = "e.g. qy50",
      Fluor       = "e.g. GFP",
      Other_tags  = "e.g. 3xFLAG; AID",
      Genotype    = "e.g. qy50[unc-59::GFP::3xFLAG::AID]) I",
      Location    = "e.g. C-terminal",
      Publication = "e.g. Authors (2025) Title. Journal.",
      Date        = "e.g. 2025-05-16",
      Comments    = "precise location, health, etc."
    )
    
    tbl <- rhandsontable(df, rowHeaders = NULL) %>%
      hot_table(
        stretchH        = "all",
        minSpareRows    = 1,     # keep one empty row so placeholders are visible
        contextMenu     = TRUE,
        manualRowMove   = TRUE,
        manualColumnMove= TRUE
      )
    
    # Apply placeholders programmatically
    for (nm in names(placeholders)) {
      if (nm %in% names(df)) {
        tbl <- hot_col(
          tbl, nm,
          placeholder = placeholders[[nm]],
          wordWrap = (nm == "Comments")
        )
      }
    }
    tbl
  })
  
  # Add row logic
  observeEvent(input$add_row_submit, {
    current_data <- if (!is.null(input$submission_table)) {
      hot_to_r(input$submission_table)
    } else {
      submission_data()
    }
    current_data <- normalize_df(current_data)
    
    new_row <- as.data.frame(
      setNames(as.list(rep("", length(required_cols))), required_cols),
      stringsAsFactors = FALSE
    )
    
    submission_data(normalize_df(rbind(current_data, new_row)))
  })
  
  # Keep submission_data updated with any table edits
  observeEvent(input$submission_table, {
    req(input$submission_table)
    df <- rhandsontable::hot_to_r(input$submission_table)
    submission_data(normalize_df(df))
  }, ignoreInit = TRUE)
  
  # Send emails
  observeEvent(input$submit_entries, {
    message("========================================")
    message("[DEBUG] Submit button clicked!")
    message("[DEBUG] Email input: ", input$submit_email)
    message("========================================")
    
    df <- submission_data()
    user_email <- input$submit_email
    
    message("[DEBUG] Got submission data, rows: ", nrow(df))
    
    # Basic email format validation
    if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", user_email)) {
      message("[DEBUG] Email validation FAILED")
      submit_status("❌ Invalid email address. Please enter a valid email.")
      return()
    }
    
    message("[DEBUG] Email validation passed")
    
    nonempty <- apply(df, 1, function(x) any(nzchar(trimws(as.character(x)))))
    df_clean <- df[nonempty, , drop = FALSE]
    
    message("[DEBUG] Clean rows: ", nrow(df_clean))
    
    # error if the table has no real entries
    if (nrow(df_clean) == 0) {
      message("[DEBUG] No entries to submit")
      submit_status("❌ No entries to submit. Please add at least one row to the table.")
      return()
    }
    
    submit_status("Please wait...")
    
    shinybusy::show_modal_spinner(text = "Sending submission...")
    on.exit(shinybusy::remove_modal_spinner(), add = TRUE)
    
    html_table <- paste(as.character(knitr::kable(df, format = "html")), collapse = "\n")
    
    # Email to user
    message("[submit] sending to user: ", user_email)
    user_ok <- send_email(
      to = user_email,
      subject = "Your WormTagDB submission",
      html_body = glue::glue("
      <p>Thank you for your submission to WormTagDB!</p>
      <p>Here is the information you submitted:</p>
      {html_table}
    ")
    )
    message("[submit] user_ok: ", user_ok)
    
    # Email to admin
    message("[submit] sending to admin")
    admin_ok <- send_email(
      to = "wormtagdb@gmail.com",
      subject = "New WormTagDB submission",
      html_body = glue::glue("
      <p>New submission received from: {user_email}</p>
      {html_table}
    ")
    )
    message("[submit] admin_ok: ", admin_ok)
    message("[submit] Emails done.")
    
    # Final message
    if (user_ok && admin_ok) {
      submit_status("✅ Submission received. A confirmation email has been sent.")
    } else if (user_ok && !admin_ok) {
      submit_status("⚠️ Submission failed.")
    } else {
      submit_status("✅  Local test submission received. Check local_submissions folder.")
    }
  })
  
  # Show the thank you message
  output$submit_status <- renderText({
    submit_status()
  })
  
  # Make it reactive for conditionalPanel
  outputOptions(output, "submit_status", suspendWhenHidden = FALSE)
  
  
  ##### Reset form when revisiting submission tabs #####
  observeEvent(input$main_tabs, {
    if (input$main_tabs %in% c("reserve_tab", "submit_tab")) {
      
      updateTextInput(session, "submit_email", value = "")
      
      submission_data(data.frame(
        Gene = "", Allele = "", Fluor = "",
        Other_tags = "", Location = "", Publication = "",
        stringsAsFactors = FALSE
      ))
      
      reservation_data(data.frame(
        Gene = "", Fluor = "",
        Other_tags = "", Location = "", Lab = "",
        stringsAsFactors = FALSE
      ))
      
      submit_status("")  # Reset message
      submit_status_r("")  # Reset message
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

# Deploy
#library(rsconnect)
#rsconnect::deployApp(appDir = "/Users/Jake/Library/Mobile Documents/com~apple~CloudDocs/Work/Postdocs/Sherwood/Research/Reviewing endogenously tagged proteins/Final App/finalapp4", appName = "wormtagdb", appTitle = "WormTagDB")
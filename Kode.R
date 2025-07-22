library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(skimr)
library(ggplot2)
library(sf)
library(car)
library(dplyr)
library(EnvStats)
library(lmtest)
library(utils)
library(rmarkdown)

# --- UI (User Interface) ---
ui <- dashboardPage(
  skin = "yellow",
  
  dashboardHeader(title = "Dashboard FaAruQ"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen_data", icon = icon("database")),
      menuItem("Eksplorasi Data", icon = icon("chart-bar"),
               menuSubItem("Statistik Deskriptif", tabName = "statistik_deskriptif"),
               menuSubItem("Visualisasi Data", tabName = "visualisasi_data"),
               menuSubItem("Tabel Data", tabName = "tabel_data")
      ),
      menuItem("Uji Asumsi Data", tabName = "uji_asumsi", icon = icon("ruler")),
      menuItem("Statistik Inferensia", icon = icon("chart-line"),
               menuSubItem("Uji Beda Rata-rata", tabName = "uji_beda_rata2"),
               menuSubItem("Uji Proporsi dan Varians", tabName = "uji_proporsi_varians"),
               menuSubItem("Uji Anova", tabName = "uji_anova")
      ),
      menuItem("Regresi Linear Berganda", tabName = "regresi_linear", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "beranda",
              fluidRow(
                box(
                  title = "Selamat Datang di Dashboard Analisis - Dashboard FaAruQ",
                  status = "primary", solidHeader = TRUE, width = 12,
                  p("Dashboard ini merupakan alat interaktif untuk menganalisis data kerentanan sosial (Social Vulnerability - SoVI®) di 511 kabupaten/kota di Indonesia. Anda dapat melakukan berbagai analisis, mulai dari manajemen data, eksplorasi, hingga analisis statistik inferensia dan pemodelan regresi. Dashboard ini dibuat oleh saya, M. Faruq Hafdizullah Erfaringga, dengan NIM 222313186, absen 19, dan kelas 2KS3 - Politeknik Statistika STIS.")
                )
              ),
              fluidRow(
                box(
                  title = "Metadata dan Sumber Data",
                  status = "info", solidHeader = TRUE, width = 12,
                  p(tags$b("Sumber Data Utama:")),
                  tags$ul(
                    tags$li("Data Indikator Kerentanan Sosial (sovi_data.csv)."),
                    tags$li("Data Jarak Antar Wilayah (distance.csv).")
                  ),
                  p(tags$b("Referensi Ilmiah:")),
                  p("Analisis dan variabel yang digunakan didasarkan pada penelitian yang dipublikasikan di ScienceDirect. Anda dapat mengakses artikel lengkapnya di sini:"),
                  tags$a(href = "https://www.sciencedirect.com/science/article/pii/S2352340921010180", target = "_blank", "Link ke Artikel Jurnal")
                )
              ),
              fluidRow(
                box(
                  title = "Definisi Variabel",
                  status = "info", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(6,
                           h4("Karakteristik Demografi"),
                           tags$ul(
                             tags$li(tags$b("CHILDREN:"), " Persentase penduduk berusia di bawah 5 tahun."),
                             tags$li(tags$b("ELDERLY:"), " Persentase penduduk berusia 65 tahun ke atas."),
                             tags$li(tags$b("FEMALE:"), " Persentase penduduk perempuan."),
                             tags$li(tags$b("FAMILYSIZE:"), " Jumlah rata-rata anggota dalam satu rumah tangga."),
                             tags$li(tags$b("GROWTH:"), " Persentase perubahan (pertumbuhan) penduduk."),
                             tags$li(tags$b("POPULATION:"), " Jumlah total penduduk.")
                           ),
                           h4("Kondisi Sosial Ekonomi"),
                           tags$ul(
                             tags$li(tags$b("FHEAD:"), " Persentase rumah tangga dengan kepala rumah tangga perempuan."),
                             tags$li(tags$b("POVERTY:"), " Persentase penduduk miskin."),
                             tags$li(tags$b("LOWEDU:"), " Persentase penduduk berusia 15+ dengan pendidikan rendah."),
                             tags$li(tags$b("ILLITERATE:"), " Persentase penduduk yang buta huruf.")
                           )
                    ),
                    column(6,
                           h4("Perumahan dan Infrastruktur"),
                           tags$ul(
                             tags$li(tags$b("NOELECTRIC:"), " Persentase rumah tangga tanpa akses listrik."),
                             tags$li(tags$b("RENTED:"), " Persentase rumah tangga yang menyewa rumah."),
                             tags$li(tags$b("NOSEWER:"), " Persentase rumah tangga yang tidak memiliki sistem drainase."),
                             tags$li(tags$b("TAPWATER:"), " Persentase rumah tangga yang menggunakan air ledeng/pipa.")
                           ),
                           h4("Kerentanan terhadap Bencana"),
                           tags$ul(
                             tags$li(tags$b("NOTRAINING:"), " Persentase rumah tangga yang tidak pernah mendapat pelatihan kebencanaan."),
                             tags$li(tags$b("DPRONE:"), " Persentase rumah tangga yang tinggal di daerah rawan bencana.")
                           )
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Fitur Dashboard",
                  status = "info", solidHeader = TRUE, width = 12,
                  tags$ul(
                    tags$li(tags$b("Manajemen Data:"), " Mengubah variabel numerik menjadi kategorik."),
                    tags$li(tags$b("Eksplorasi Data:"), " Melihat ringkasan statistik, visualisasi data dalam bentuk grafik (histogram/bar chart) dan peta spasial, serta tabel data mentah."),
                    tags$li(tags$b("Uji Asumsi Data:"), " Melakukan uji normalitas (Shapiro-Wilk) dan uji homogenitas varians (Bartlett's Test)."),
                    tags$li(tags$b("Statistik Inferensia:"), " Melakukan berbagai uji hipotesis seperti Uji-t, Uji Proporsi, Uji Varians, dan ANOVA."),
                    tags$li(tags$b("Regresi Linear Berganda:"), " Membangun model regresi dan mengevaluasi asumsi-asumsinya.")
                  )
                )
              )
      ),
      
      tabItem(tabName = "manajemen_data",
              h2("Manajemen Data"),
              fluidRow(
                box(
                  title = "Fitur Kategorisasi Data", status = "warning", solidHeader = TRUE, width = 4,
                  p("Ubah variabel kontinyu menjadi kategorik."),
                  selectInput("var_to_cut", "1. Pilih Variabel Kontinyu:", choices = NULL),
                  numericInput("num_bins", "2. Jumlah Kategori (2-5):", value = 3, min = 2, max = 5),
                  uiOutput("interval_inputs"), 
                  uiOutput("label_inputs"),   
                  actionButton("cut_data", "Kategorikan Data", icon = icon("cut"))
                ),
                box(
                  title = "Tabel Data Hasil Kategorisasi", status = "primary", solidHeader = TRUE, width = 8,
                  DT::dataTableOutput("processed_data_table"),
                  downloadButton("download_processed_data_csv", "Download Data (.csv)")
                )
              ),
              hr(),
              fluidRow(
                box(
                  title = "Interpretasi", status = "info", solidHeader = TRUE, width = 12,
                  uiOutput("interpretation_cut"),
                  downloadButton("download_manajemen_data_local", "Download Interpretasi (.pdf)")
                )
              )
      ),
      
      tabItem(tabName = "statistik_deskriptif",
              h2("Eksplorasi Data: Statistik Deskriptif"),
              box(
                title = "Ringkasan Statistik", status = "primary", solidHeader = TRUE, width = 12,
                verbatimTextOutput("descriptive_stats"),
                hr(),
                uiOutput("descriptive_interpretation"),
                hr(),
                downloadButton("download_deskriptif_report", "Download Laporan (.pdf)")
              )
      ),
      
      tabItem(tabName = "visualisasi_data",
              h2("Eksplorasi Data: Visualisasi Data"),
              fluidRow(
                box(
                  title = "Visualisasi Grafik", status = "primary", solidHeader = TRUE, width = 6,
                  selectInput("plot_variable", "Pilih Variabel:", choices = NULL),
                  plotOutput("dynamic_plot"),
                  hr(),
                  uiOutput("plot_interpretation"),
                  downloadButton("download_plot_png", "Download Grafik (.png)")
                ),
                box(
                  title = "Visualisasi Peta", status = "primary", solidHeader = TRUE, width = 6,
                  selectInput("map_variable", "Pilih Variabel:", choices = NULL),
                  plotOutput("sovi_map"),
                  hr(),
                  uiOutput("map_interpretation"),
                  downloadButton("download_map_png", "Download Peta (.png)")
                )
              )
      ),
      
      tabItem(tabName = "tabel_data",
              h2("Eksplorasi Data: Tabel Data"),
              box(
                title = "Tabel Data Mentah", status = "primary", solidHeader = TRUE, width = 12,
                DT::dataTableOutput("raw_data_table"),
                hr(),
                downloadButton("download_raw_data_csv", "Download Data (.csv)")
              )
      ),
      
      tabItem(tabName = "uji_asumsi",
              h2("Uji Asumsi Data"),
              fluidRow(
                box(
                  title = "Uji Normalitas (Shapiro-Wilk)", status = "warning", solidHeader = TRUE, width = 6,
                  p("Memeriksa apakah data terdistribusi normal."),
                  selectInput("normality_variable", "Pilih Variabel Numerik:", choices = NULL),
                  verbatimTextOutput("normality_test_output"),
                  plotOutput("normality_plot"),
                  hr(),
                  uiOutput("normality_interpretation"),
                  hr(),
                  downloadButton("download_normality_plot_png", "Download QQ Plot (.png)")
                ),
                uiOutput("homogeneity_ui")
              ),
              fluidRow(
                column(width = 12, align = "center",
                       downloadButton("download_asumsi_report_pdf", "Download Laporan Uji Asumsi (.pdf)", style = "margin-top: 20px;")
                )
              )
      ),
      
      tabItem(tabName = "uji_beda_rata2",
              h2("Uji Beda Rata-rata (1 & 2 Kelompok)"),
              fluidRow(
                box(
                  title = "Uji t Satu Kelompok", status = "primary", solidHeader = TRUE, width = 6,
                  p("Membandingkan rata-rata sampel dengan nilai hipotesis."),
                  selectInput("t_test_one_sample_var", "Pilih Variabel Numerik:", choices = NULL),
                  numericInput("t_test_one_sample_mu", "Nilai Rata-rata Hipotesis (μ):", value = 0),
                  verbatimTextOutput("t_test_one_sample_output"),
                  hr(),
                  uiOutput("t_test_one_sample_interpretation")
                ),
                box(
                  title = "Uji t Dua Kelompok", status = "primary", solidHeader = TRUE, width = 6,
                  p("Membandingkan rata-rata dari dua kelompok."),
                  selectInput("t_test_two_sample_response", "Pilih Variabel Respon (Numerik):", choices = NULL),
                  selectInput("t_test_two_sample_group", "Pilih Variabel Grup (Kategorik):", choices = NULL),
                  verbatimTextOutput("t_test_two_sample_output"),
                  hr(),
                  uiOutput("t_test_two_sample_interpretation")
                )
              ),
              fluidRow(
                column(width=12, align="center", 
                       downloadButton("download_beda_rata_report", "Download Laporan (.pdf)", style="margin-top:20px;"))
              )
      ),
      
      tabItem(tabName = "uji_proporsi_varians",
              h2("Uji Proporsi dan Varians"),
              fluidRow(
                box(
                  title = "Uji Proporsi Satu Kelompok", status = "success", solidHeader = TRUE, width = 6,
                  selectInput("prop_test_one_group_var", "Pilih Variabel Kategorik:", choices = NULL),
                  selectInput("prop_test_one_success", "Pilih Kategori 'Sukses':", choices = NULL),
                  numericInput("prop_test_one_p", "Proporsi Hipotesis (p):", value = 0.5, min = 0, max = 1),
                  actionButton("run_prop_test_one", "Jalankan Uji", icon = icon("play")),
                  hr(),
                  verbatimTextOutput("prop_test_one_output"),
                  uiOutput("prop_test_one_interpretation")
                ),
                box(
                  title = "Uji Varians Satu Kelompok", status = "success", solidHeader = TRUE, width = 6,
                  selectInput("var_test_one_sample_var", "Pilih Variabel Numerik:", choices = NULL),
                  numericInput("var_test_one_sample_sigma", "Varians Hipotesis (σ²):", value = 1, min = 0),
                  actionButton("run_var_test_one", "Jalankan Uji", icon = icon("play")),
                  hr(),
                  verbatimTextOutput("var_test_one_sample_output"),
                  uiOutput("var_test_one_sample_interpretation")
                )
              ),
              fluidRow(
                box(
                  title = "Uji Varians Dua Kelompok (F-test)", status = "success", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(6, selectInput("var_test_two_sample_response", "Pilih Variabel Respon (Numerik):", choices = NULL)),
                    column(6, selectInput("var_test_two_sample_group", "Pilih Variabel Grup (2 Kategori):", choices = NULL))
                  ),
                  actionButton("run_var_test_two", "Jalankan Uji", icon = icon("play")),
                  hr(),
                  verbatimTextOutput("var_test_two_sample_output"),
                  uiOutput("var_test_two_sample_interpretation")
                )
              ),
              fluidRow(
                column(width=12, align="center", 
                       downloadButton("download_prop_varians_report", "Download Laporan (.pdf)", style="margin-top:20px;"))
              )
      ),
      
      tabItem(tabName = "uji_anova",
              h2("Uji Anova"),
              fluidRow(
                box(
                  title = "Pengaturan Uji Anova", status = "info", solidHeader = TRUE, width = 12,
                  radioButtons("anova_type", "Pilih Jenis Anova:", choices = c("Satu Arah" = "one_way", "Dua Arah" = "two_way")),
                  conditionalPanel(
                    condition = "input.anova_type == 'one_way'",
                    selectInput("anova_one_response_var", "Variabel Respon (Numerik):", choices = NULL),
                    selectInput("anova_one_group_var", "Variabel Grup (Kategorik):", choices = NULL)
                  ),
                  conditionalPanel(
                    condition = "input.anova_type == 'two_way'",
                    selectInput("anova_two_response_var", "Variabel Respon (Numerik):", choices = NULL),
                    selectInput("anova_two_group_var1", "Variabel Grup 1:", choices = NULL),
                    selectInput("anova_two_group_var2", "Variabel Grup 2:", choices = NULL)
                  ),
                  actionButton("run_anova_button", "Jalankan Uji Anova", icon = icon("play")),
                  hr(),
                  verbatimTextOutput("anova_result_table"),
                  hr(),
                  uiOutput("anova_interpretation"),
                  hr(),
                  downloadButton("download_anova_report", "Download Laporan (.pdf)")
                )
              )
      ),
      
      tabItem(tabName = "regresi_linear",
              h2("Regresi Linear Berganda"),
              fluidRow(
                box(
                  title = "1. Pemodelan Regresi", status = "primary", solidHeader = TRUE, width = 12,
                  p("Bangun model regresi untuk memprediksi variabel dependen."),
                  fluidRow(
                    column(6, selectInput("reg_response_var", "Variabel Dependen (Y):", choices = NULL)),
                    column(6, selectInput("reg_predictor_vars", "Variabel Independen (X):", choices = NULL, multiple = TRUE))
                  ),
                  actionButton("run_reg_button", "Jalankan Regresi", icon = icon("play"))
                )
              ),
              conditionalPanel(
                condition = "input.run_reg_button > 0",
                fluidRow(
                  box(
                    title = "Hasil dan Interpretasi Model", status = "info", solidHeader = TRUE, width=12,
                    h4("Ringkasan Model"),
                    verbatimTextOutput("reg_model_summary"),
                    hr(),
                    uiOutput("reg_model_interpretation")
                  )
                ),
                fluidRow(
                  box(
                    title = "2. Uji Asumsi", status = "warning", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = FALSE,
                    h4("Asumsi Normalitas Residual"),
                    plotOutput("reg_normality_plot"),
                    downloadButton("download_reg_qq_png", "Download QQ Plot (.png)"),
                    verbatimTextOutput("reg_normality_test"),
                    uiOutput("reg_normality_interpretation"),
                    hr(),
                    h4("Asumsi Homoskedastisitas (Constant Variance)"),
                    plotOutput("reg_homoscedasticity_plot"),
                    downloadButton("download_reg_resid_png", "Download Residuals Plot (.png)"),
                    uiOutput("reg_homoscedasticity_interpretation"),
                    hr(),
                    h4("Asumsi Multikolinearitas"),
                    verbatimTextOutput("reg_multicollinearity_test"),
                    uiOutput("reg_multicollinearity_interpretation")
                  )
                ),
                fluidRow(
                  column(width = 12, align="center",
                         downloadButton("download_regresi_report", "Download Laporan Teks (.pdf)", style="margin-top:20px;")
                  )
                )
              )
      )
    )
  )
)

# --- Server (Logika Aplikasi) ---
server <- function(input, output, session) {
  
  # Bagian 1: Inisialisasi dan Membaca Data
  data_csv <- reactive({
    file_path <- "data/sovi_data.csv"
    if (!file.exists(file_path)) {
      shiny::validate(paste("File data tidak ditemukan di:", file_path, ". Pastikan struktur folder Anda benar."))
    }
    read_csv(file_path) %>% mutate(across(where(is.character), as.factor))
  })
  
  data_geojson <- reactive({ sf::st_read("peta/petafix.geojson") })
  data_reactive <- reactiveVal()
  observe({ data_reactive(data_csv()) })
  
  message_reactive <- reactiveVal("")
  manajemen_interpretations_history <- reactiveVal(list())
  kategorisasi_params <- reactiveVal(list())
  
  rendered_interpretations <- reactiveValues()
  
  # Bagian 2 & 3: Manajemen & Eksplorasi Data
  numeric_vars <- reactive({ req(data_reactive()); names(select_if(data_reactive(), is.numeric)) })
  categorical_vars <- reactive({ req(data_reactive()); names(select_if(data_reactive(), function(col) is.factor(col) | is.character(col))) })
  binary_vars <- reactive({ req(data_reactive()); data_reactive() %>% select(where(is.factor)) %>% select(where(~length(levels(.)) == 2)) %>% names() })
  
  observe({
    req(data_reactive())
    updateSelectInput(session, "var_to_cut", choices = numeric_vars())
    plot_choices <- names(data_reactive())
    updateSelectInput(session, "plot_variable", choices = plot_choices)
    updateSelectInput(session, "map_variable", choices = names(data_geojson()))
    updateSelectInput(session, "normality_variable", choices = numeric_vars())
    updateSelectInput(session, "homogeneity_response", choices = numeric_vars())
    updateSelectInput(session, "homogeneity_group", choices = categorical_vars())
    updateSelectInput(session, "t_test_one_sample_var", choices = numeric_vars())
    updateSelectInput(session, "t_test_two_sample_response", choices = numeric_vars())
    updateSelectInput(session, "t_test_two_sample_group", choices = categorical_vars())
    updateSelectInput(session, "var_test_one_sample_var", choices = numeric_vars())
    updateSelectInput(session, "var_test_two_sample_response", choices = numeric_vars())
    updateSelectInput(session, "var_test_two_sample_group", choices = binary_vars())
    updateSelectInput(session, "prop_test_one_group_var", choices = categorical_vars())
    updateSelectInput(session, "prop_test_one_success", choices = NULL)
    updateSelectInput(session, "anova_one_response_var", choices = numeric_vars())
    updateSelectInput(session, "anova_one_group_var", choices = categorical_vars())
    updateSelectInput(session, "anova_two_response_var", choices = numeric_vars())
    updateSelectInput(session, "anova_two_group_var1", choices = categorical_vars())
    updateSelectInput(session, "anova_two_group_var2", choices = categorical_vars())
    updateSelectInput(session, "reg_response_var", choices = numeric_vars())
    updateSelectInput(session, "reg_predictor_vars", choices = numeric_vars(), selected = NULL)
  })
  
  observeEvent(input$prop_test_one_group_var, {
    req(data_reactive(), input$prop_test_one_group_var)
    levels <- unique(data_reactive()[[input$prop_test_one_group_var]])
    updateSelectInput(session, "prop_test_one_success", choices = levels)
  })
  
  output$interval_inputs <- renderUI({
    req(input$num_bins >= 2, input$num_bins <= 5, data_csv(), input$var_to_cut)
    num_breaks <- input$num_bins - 1
    req(data_csv(), input$var_to_cut)
    var_data <- data_csv()[[input$var_to_cut]]
    if (length(var_data) == 0 || all(is.na(var_data))) { return(p("Tidak ada data valid.")) }
    default_breaks <- round(quantile(var_data, probs = (1:num_breaks) / input$num_bins, na.rm = TRUE), 2)
    lapply(1:num_breaks, function(i) numericInput(paste0("break_", i), paste0("Batas Interval ", i, ":"), value = default_breaks[[i]]))
  })
  
  output$label_inputs <- renderUI({
    req(input$num_bins >= 2, input$num_bins <= 5)
    lapply(1:input$num_bins, function(i) textInput(paste0("label_", i), paste0("Nama Kategori ", i, ":"), value = paste0("Kategori ", i)))
  })
  
  observeEvent(input$cut_data, {
    req(input$var_to_cut, input$num_bins)
    if (input$num_bins < 2 || input$num_bins > 5) { message_reactive("Kesalahan: Jumlah kategori harus antara 2 dan 5."); return(NULL) }
    current_data <- data_reactive()
    var_name <- input$var_to_cut
    num_bins <- input$num_bins
    user_breaks <- sapply(1:(num_bins - 1), function(i) input[[paste0("break_", i)]])
    if (any(is.na(user_breaks)) || is.unsorted(user_breaks)) { message_reactive("Kesalahan: Batas interval harus diisi dan berurutan."); return(NULL) }
    user_labels <- sapply(1:num_bins, function(i) {
      label_input <- input[[paste0("label_", i)]]
      if (is.null(label_input) || label_input == "") paste0("Kategori ", i) else label_input
    })
    min_val <- min(current_data[[var_name]], na.rm = TRUE)
    max_val <- max(current_data[[var_name]], na.rm = TRUE)
    cut_var_name <- paste0("Kategori_", var_name)
    current_data[[cut_var_name]] <- cut(
      current_data[[var_name]],
      breaks = c(min_val, user_breaks, max_val), labels = user_labels, right = TRUE, include.lowest = TRUE
    )
    data_reactive(current_data)
    message_reactive("Kategorisasi berhasil!")
    
    new_params <- list(
      var_name_param = var_name, num_bins_param = num_bins, user_breaks_param = user_breaks,
      user_labels_param = user_labels, min_val_param = min_val, max_val_param = max_val
    )
    current_params_history <- kategorisasi_params(); current_params_history[[length(current_params_history) + 1]] <- new_params; kategorisasi_params(current_params_history)
    
    new_interpretation_html <- tagList(
      h4(paste0("Interpretasi Proses Kategorisasi (", Sys.time(), ")")),
      p("Kategorisasi berhasil! Variabel ", tags$strong(var_name), " telah dibagi menjadi ", num_bins, " kelompok."),
      p("Tabel data kini memiliki kolom baru bernama ", tags$strong(paste0("Kategori_", var_name)), "."),
      tags$ul(lapply(1:num_bins, function(i) {
        all_breaks <- c(min_val, user_breaks, max_val)
        lower <- round(all_breaks[[i]], 2); upper <- round(all_breaks[[i+1]], 2)
        interval_str <- if (i == 1) paste0("[", lower, ", ", upper, "]") else paste0("(", lower, ", ", upper, "]")
        tags$li(tags$b(user_labels[[i]]), ": ", interval_str)
      }))
    )
    current_history <- manajemen_interpretations_history(); current_history[[length(current_history) + 1]] <- as.character(new_interpretation_html); manajemen_interpretations_history(current_history)
  })
  
  output$processed_data_table <- DT::renderDataTable({ DT::datatable(data_reactive(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) })
  
  output$interpretation_cut <- renderUI({
    req(input$cut_data)
    if (message_reactive() != "Kategorisasi berhasil!") return(tags$p(message_reactive(), style = "color: red;"))
    history <- manajemen_interpretations_history()
    if (length(history) == 0) return(p("Lakukan kategorisasi untuk melihat interpretasi."))
    HTML(paste(rev(history), collapse = "<hr>"))
  })
  
  output$descriptive_stats <- renderPrint({
    req(data_reactive())
    data_out <- skim(data_reactive())
    rendered_interpretations$descriptive_stats_object <- data_out
    data_out
  })
  
  output$raw_data_table <- DT::renderDataTable({ DT::datatable(data_reactive(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) })
  
  output$dynamic_plot <- renderPlot({
    req(input$plot_variable, data_reactive())
    selected_var <- data_reactive()[[input$plot_variable]]
    p <- if (is.numeric(selected_var)) {
      ggplot(data_reactive(), aes(x = .data[[input$plot_variable]])) +
        geom_histogram(bins = 30, fill = "lightblue", color = "black")
    } else {
      ggplot(data_reactive(), aes(x = .data[[input$plot_variable]])) +
        geom_bar(fill = "lightblue", color = "black")
    }
    p <- p + labs(title = paste("Distribusi Variabel", input$plot_variable), x = input$plot_variable, y = "Frekuensi") + theme_minimal()
    rendered_interpretations$plot_obj <- p
    p
  })
  
  output$sovi_map <- renderPlot({
    req(input$map_variable, data_geojson())
    p <- ggplot(data_geojson()) +
      geom_sf(aes(fill = .data[[input$map_variable]]), color = "black") +
      labs(title = paste("Peta Visualisasi Variabel", input$map_variable), fill = input$map_variable) +
      scale_fill_viridis_c(option = "plasma") +
      theme_void()
    rendered_interpretations$map_obj <- p
    p
  })
  
  output$plot_interpretation <- renderUI({
    req(input$plot_variable, data_reactive())
    selected_var <- data_reactive()[[input$plot_variable]]
    html_output <- if (is.numeric(selected_var)) {
      tagList(
        h4("Interpretasi Visualisasi Grafik"),
        p("Histogram di atas menunjukkan distribusi dari variabel ", tags$b(input$plot_variable), ". Anda dapat melihat frekuensi kemunculan nilai-nilai dalam rentang tertentu."),
        p("Puncak tertinggi menunjukkan nilai yang paling sering muncul dalam dataset ini.")
      )
    } else {
      tagList(
        h4("Interpretasi Visualisasi Grafik"),
        p("Bar plot di atas menunjukkan frekuensi kemunculan setiap kategori dari variabel ", tags$b(input$plot_variable), ".")
      )
    }
    rendered_interpretations$plot <- as.character(html_output)
    html_output
  })
  
  output$map_interpretation <- renderUI({
    req(input$map_variable, data_geojson())
    html_output <- tagList(
      h4("Interpretasi Visualisasi Peta"),
      p("Peta ini memvisualisasikan distribusi spasial dari variabel ", tags$b(input$map_variable), ". Warna yang lebih terang menunjukkan nilai yang lebih rendah, sementara warna yang lebih gelap menunjukkan nilai yang lebih tinggi."),
      p("Visualisasi ini berguna untuk mengidentifikasi pola geografis.")
    )
    rendered_interpretations$map <- as.character(html_output)
    html_output
  })
  
  output$descriptive_interpretation <- renderUI({
    req(data_reactive())
    tagList(
      h4("Interpretasi Statistik Deskriptif"),
      p("Tabel di atas menampilkan ringkasan statistik untuk setiap variabel."),
      tags$ul(
        tags$li(tags$b("n_missing:"), " Jumlah data yang hilang."),
        tags$li(tags$b("mean/sd:"), " Nilai rata-rata dan standar deviasi."),
        tags$li(tags$b("p0/p25/p50/p75/p100:"), " Nilai minimum, kuartil, dan maksimum.")
      )
    )
  })
  
  # Bagian 4: Uji Asumsi Data
  
  output$homogeneity_ui <- renderUI({
    box(title = "Uji Homogenitas (Bartlett's Test)", status = "warning", solidHeader = TRUE, width = 6,
        if (length(categorical_vars()) == 0) {
          p("Tidak ada variabel kategorik yang ditemukan dalam data.")
        } else {
          tagList(
            p("Uji ini memeriksa apakah varians antar kelompok sama (homogen)."),
            selectInput("homogeneity_response", "Pilih Variabel Respon (Numerik):", choices = numeric_vars()),
            selectInput("homogeneity_group", "Pilih Variabel Grup (Kategorik):", choices = categorical_vars()),
            verbatimTextOutput("homogeneity_test_output"),
            hr(),
            uiOutput("homogeneity_interpretation")
          )
        })
  })
  
  normality_result <- reactive({ req(input$normality_variable, data_reactive()); shapiro.test(data_reactive()[[input$normality_variable]]) })
  
  output$normality_test_output <- renderPrint({
    res <- normality_result()
    rendered_interpretations$normality_test_output <- paste(capture.output(res), collapse = "\n")
    res
  })
  
  output$normality_plot <- renderPlot({
    req(input$normality_variable, data_reactive())
    plot_data <- data.frame(value = data_reactive()[[input$normality_variable]])
    p <- ggplot(plot_data, aes(sample = value)) +
      stat_qq(color = "blue") + stat_qq_line(color = "red", linewidth = 1) +
      labs(title = paste("QQ Plot untuk", input$normality_variable), x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_minimal()
    rendered_interpretations$normality_plot_obj <- p
    p
  })
  
  homogeneity_result <- reactive({
    req(input$homogeneity_response, input$homogeneity_group, data_reactive())
    bartlett.test(as.formula(paste(input$homogeneity_response, "~", input$homogeneity_group)), data = data_reactive())
  })
  
  output$homogeneity_test_output <- renderPrint({
    res <- homogeneity_result()
    rendered_interpretations$homogeneity_test_output <- paste(capture.output(res), collapse = "\n")
    res
  })
  
  output$normality_interpretation <- renderUI({
    req(normality_result())
    p_value <- normality_result()$p.value
    interpretation <- if (p_value > 0.05) {
      "Berdasarkan hasil uji Shapiro-Wilk, nilai p adalah signifikan (p > 0.05), yang menunjukkan bahwa data diasumsikan terdistribusi normal."
    } else {
      "Berdasarkan hasil uji Shapiro-Wilk, nilai p tidak signifikan (p ≤ 0.05), yang menunjukkan bahwa data tidak terdistribusi normal."
    }
    rendered_interpretations$normality_interpretation <- interpretation
    HTML(paste0("<h4>Interpretasi Uji Normalitas</h4><p>", interpretation, "</p>"))
  })
  
  output$homogeneity_interpretation <- renderUI({
    req(homogeneity_result())
    p_value <- homogeneity_result()$p.value
    interpretation <- if (p_value > 0.05) {
      "Berdasarkan hasil uji Bartlett, nilai p adalah signifikan (p > 0.05), yang menunjukkan bahwa varians antar kelompok diasumsikan homogen."
    } else {
      "Berdasarkan hasil uji Bartlett, nilai p tidak signifikan (p ≤ 0.05), yang menunjukkan bahwa varians antar kelompok tidak homogen."
    }
    rendered_interpretations$homogeneity_interpretation <- interpretation
    HTML(paste0("<h4>Interpretasi Uji Homogenitas</h4><p>", interpretation, "</p>"))
  })
  
  # Bagian 5-10: Uji Statistik Inferensia & Regresi
  
  t_test_one_sample_result <- reactive({
    req(input$t_test_one_sample_var, !is.na(input$t_test_one_sample_mu), data_reactive())
    t.test(data_reactive()[[input$t_test_one_sample_var]], mu = input$t_test_one_sample_mu)
  })
  output$t_test_one_sample_output <- renderPrint({
    res <- t_test_one_sample_result(); rendered_interpretations$t_one_test_output <- paste(capture.output(res), collapse="\n"); res
  })
  output$t_test_one_sample_interpretation <- renderUI({
    req(t_test_one_sample_result()); p_val <- t_test_one_sample_result()$p.value
    interp <- if (p_val < 0.05) "berbeda signifikan" else "tidak berbeda signifikan"
    text <- paste0("<b>Hasil:</b> Rata-rata sampel ", interp, " dari nilai hipotesis (p-value = ", round(p_val, 4), ").")
    rendered_interpretations$t_one_interpretation <- text; HTML(text)
  })
  
  t_test_two_sample_result <- reactive({
    req(input$t_test_two_sample_response, input$t_test_two_sample_group)
    df <- data_reactive()
    formula <- as.formula(paste(input$t_test_two_sample_response, "~", input$t_test_two_sample_group))
    var_eq <- bartlett.test(formula, data=df)$p.value > 0.05
    t.test(formula, data=df, var.equal = var_eq)
  })
  output$t_test_two_sample_output <- renderPrint({
    res <- t_test_two_sample_result(); rendered_interpretations$t_two_test_output <- paste(capture.output(res), collapse="\n"); res
  })
  output$t_test_two_sample_interpretation <- renderUI({
    req(t_test_two_sample_result()); p_val <- t_test_two_sample_result()$p.value
    interp <- if (p_val < 0.05) "terdapat perbedaan signifikan" else "tidak terdapat perbedaan signifikan"
    text <- paste0("<b>Hasil:</b> ", interp, " rata-rata antara kedua kelompok (p-value = ", round(p_val, 4), ").")
    rendered_interpretations$t_two_interpretation <- text; HTML(text)
  })
  
  prop_one_result <- eventReactive(input$run_prop_test_one, {
    req(input$prop_test_one_group_var, input$prop_test_one_success)
    tbl <- table(data_reactive()[[input$prop_test_one_group_var]])
    success_count <- tbl[[input$prop_test_one_success]]
    prop.test(x = success_count, n = sum(tbl), p = input$prop_test_one_p)
  })
  output$prop_test_one_output <- renderPrint({ res <- prop_one_result(); rendered_interpretations$prop_one_test_output <- paste(capture.output(res), collapse="\n"); res })
  output$prop_test_one_interpretation <- renderUI({
    req(prop_one_result()); p_val <- prop_one_result()$p.value
    interp <- if(p_val < 0.05) "berbeda signifikan" else "tidak berbeda signifikan"
    text <- paste0("<b>Hasil:</b> Proporsi sampel ", interp, " dari proporsi hipotesis (p-value = ", round(p_val, 4), ").")
    rendered_interpretations$prop_one_interpretation <- text; HTML(text)
  })
  
  var_one_result <- eventReactive(input$run_var_test_one, {
    req(input$var_test_one_sample_var)
    EnvStats::varTest(data_reactive()[[input$var_test_one_sample_var]], sigma.squared = input$var_test_one_sample_sigma)
  })
  output$var_test_one_sample_output <- renderPrint({ res <- var_one_result(); rendered_interpretations$var_one_test_output <- paste(capture.output(res), collapse="\n"); res })
  output$var_test_one_sample_interpretation <- renderUI({
    req(var_one_result()); p_val <- var_one_result()$p.value
    interp <- if(p_val < 0.05) "berbeda signifikan" else "tidak berbeda signifikan"
    text <- paste0("<b>Hasil:</b> Varians sampel ", interp, " dari varians hipotesis (p-value = ", round(p_val, 4), ").")
    rendered_interpretations$var_one_interpretation <- text; HTML(text)
  })
  
  var_two_result <- eventReactive(input$run_var_test_two, {
    req(input$var_test_two_sample_response, input$var_test_two_sample_group)
    var.test(as.formula(paste(input$var_test_two_sample_response, "~", input$var_test_two_sample_group)), data = data_reactive())
  })
  output$var_test_two_sample_output <- renderPrint({ res <- var_two_result(); rendered_interpretations$var_two_test_output <- paste(capture.output(res), collapse="\n"); res })
  output$var_test_two_sample_interpretation <- renderUI({
    req(var_two_result()); p_val <- var_two_result()$p.value
    interp <- if(p_val < 0.05) "terdapat perbedaan signifikan" else "tidak terdapat perbedaan signifikan"
    text <- paste0("<b>Hasil:</b> ", interp, " varians antara kedua kelompok (p-value = ", round(p_val, 4), ").")
    rendered_interpretations$var_two_interpretation <- text; HTML(text)
  })
  
  anova_result <- eventReactive(input$run_anova_button, {
    req(input$anova_type)
    if (input$anova_type == "one_way") {
      req(input$anova_one_response_var, input$anova_one_group_var)
      summary(aov(as.formula(paste(input$anova_one_response_var, "~", input$anova_one_group_var)), data = data_reactive()))
    } else {
      req(input$anova_two_response_var, input$anova_two_group_var1, input$anova_two_group_var2)
      summary(aov(as.formula(paste(input$anova_two_response_var, "~", input$anova_two_group_var1, "*", input$anova_two_group_var2)), data = data_reactive()))
    }
  })
  output$anova_result_table <- renderPrint({ res <- anova_result(); rendered_interpretations$anova_test_output <- paste(capture.output(res), collapse="\n"); res })
  output$anova_interpretation <- renderUI({
    req(anova_result())
    p_values <- anova_result()[[1]]$`Pr(>F)`
    interp_text <- "Analisis Anova selesai. Periksa nilai Pr(>F) untuk signifikansi."
    rendered_interpretations$anova_interpretation <- interp_text
    HTML(interp_text)
  })
  
  reg_model_reactive <- eventReactive(input$run_reg_button, {
    req(input$reg_response_var, input$reg_predictor_vars)
    formula_text <- paste(input$reg_response_var, "~", paste(input$reg_predictor_vars, collapse = " + "))
    lm(as.formula(formula_text), data = data_reactive())
  })
  
  output$reg_model_summary <- renderPrint({
    model <- reg_model_reactive()
    summary_text <- capture.output(summary(model))
    rendered_interpretations$reg_model_summary_output <- paste(summary_text, collapse = "\n")
    summary(model)
  })
  
  output$reg_model_interpretation <- renderUI({
    model_summary <- summary(reg_model_reactive())
    adj_r_squared <- round(model_summary$adj.r.squared, 4)
    f_stat <- model_summary$fstatistic
    p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
    
    interp_text <- paste0(
      "<p><b>Adjusted R-squared:</b> ", adj_r_squared, ". Ini berarti sekitar ", round(adj_r_squared * 100, 2), "% variasi variabel dependen dapat dijelaskan oleh model.</p>",
      "<p><b>Uji F-statistik:</b> p-value model secara keseluruhan adalah ", round(p_value, 4), ". ",
      if(p_value < 0.05) "Ini menunjukkan bahwa setidaknya satu variabel independen secara signifikan mempengaruhi variabel dependen." else "Ini menunjukkan bahwa secara keseluruhan, model tidak signifikan secara statistik."
    )
    rendered_interpretations$reg_model_interpretation <- interp_text
    HTML(interp_text)
  })
  
  output$reg_normality_plot <- renderPlot({
    model <- reg_model_reactive()
    residuals <- residuals(model)
    plot_data <- data.frame(residuals = residuals)
    p <- ggplot(plot_data, aes(sample = residuals)) +
      stat_qq(color = "blue") + stat_qq_line(color = "red", linewidth = 1) +
      labs(title = "QQ Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_minimal()
    rendered_interpretations$reg_normality_plot_obj <- p
    p
  })
  
  output$reg_normality_test <- renderPrint({
    res <- shapiro.test(residuals(reg_model_reactive()))
    rendered_interpretations$reg_normality_test_output <- paste(capture.output(res), collapse="\n")
    res
  })
  
  output$reg_normality_interpretation <- renderUI({
    p_value <- shapiro.test(residuals(reg_model_reactive()))$p.value
    HTML(if(p_value > 0.05) "<p><b>Interpretasi:</b> Residual terdistribusi normal (p > 0.05), asumsi normalitas terpenuhi.</p>" else "<p><b>Interpretasi:</b> Residual tidak terdistribusi normal (p <= 0.05), asumsi normalitas tidak terpenuhi.</p>")
  })
  
  output$reg_homoscedasticity_plot <- renderPlot({
    model <- reg_model_reactive()
    plot_data <- data.frame(Fitted = fitted(model), Residuals = residuals(model))
    p <- ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
      geom_point(alpha = 0.5) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      labs(title = "Residuals vs. Fitted Values Plot", x = "Fitted Values", y = "Residuals") +
      theme_minimal()
    rendered_interpretations$reg_homoscedasticity_plot_obj <- p
    p
  })
  
  output$reg_homoscedasticity_interpretation <- renderUI({
    HTML("<p><b>Interpretasi:</b> Asumsi homoskedastisitas terpenuhi jika titik-titik tersebar secara acak di sekitar garis horizontal tanpa membentuk pola yang jelas (seperti corong atau kurva).</p>")
  })
  
  output$reg_multicollinearity_test <- renderPrint({
    req(length(input$reg_predictor_vars) >= 2)
    vif_results <- car::vif(reg_model_reactive())
    rendered_interpretations$reg_multicollinearity_test_output <- paste(capture.output(vif_results), collapse="\n")
    vif_results
  })
  
  output$reg_multicollinearity_interpretation <- renderUI({
    if(length(input$reg_predictor_vars) < 2){
      return(HTML("<p>Uji VIF memerlukan minimal dua variabel independen.</p>"))
    }
    HTML("<p><b>Interpretasi:</b> Nilai VIF di atas 5 atau 10 menunjukkan adanya potensi masalah multikolinearitas.</p>")
  })
  
  # Bagian 11: Logika Download Laporan
  
  output$download_manajemen_data_local <- downloadHandler(
    filename = function() { paste0("interpretasi_kategorisasi_", Sys.Date(), ".pdf") },
    content = function(file) {
      params_history_list <- kategorisasi_params()
      if (length(params_history_list) == 0) {
        temp_rmd <- tempfile(fileext = ".Rmd")
        writeLines(c("---", "title: 'Informasi'", "output: pdf_document", "---", "Lakukan proses kategorisasi terlebih dahulu."), temp_rmd)
        rmarkdown::render(temp_rmd, output_file = file)
      } else {
        temp_rmd <- tempfile(fileext = ".Rmd")
        file.copy("manajemen_interpretasi_template.Rmd", temp_rmd, overwrite = TRUE)
        rmarkdown::render(temp_rmd, output_file = file, params = list(params_history = params_history_list), envir = new.env(parent = globalenv()))
      }
    }, contentType = "application/pdf"
  )
  
  output$download_processed_data_csv <- downloadHandler(
    filename = function() { paste0("data_hasil_kategorisasi_", Sys.Date(), ".csv") },
    content = function(file) { write_csv(data_reactive(), file) }
  )
  
  output$download_deskriptif_report <- downloadHandler(
    filename = function() { paste0("laporan_statistik_deskriptif_", Sys.Date(), ".pdf") },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      file.copy("deskriptif_report_template.Rmd", temp_rmd, overwrite = TRUE)
      params <- list(descriptive_stats_object = rendered_interpretations$descriptive_stats_object)
      rmarkdown::render(temp_rmd, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  output$download_plot_png <- downloadHandler(
    filename = function() { paste0("grafik_", input$plot_variable, "_", Sys.Date(), ".png") },
    content = function(file) {
      req(rendered_interpretations$plot_obj)
      ggsave(file, plot = rendered_interpretations$plot_obj, device = "png", width = 8, height = 6, dpi = 300)
    }
  )
  
  output$download_map_png <- downloadHandler(
    filename = function() { paste0("peta_", input$map_variable, "_", Sys.Date(), ".png") },
    content = function(file) {
      req(rendered_interpretations$map_obj)
      ggsave(file, plot = rendered_interpretations$map_obj, device = "png", width = 7, height = 7, dpi = 300)
    }
  )
  
  output$download_raw_data_csv <- downloadHandler(
    filename = function() { paste0("data_mentah_", Sys.Date(), ".csv") },
    content = function(file) { write_csv(data_reactive(), file) }
  )
  
  output$download_normality_plot_png <- downloadHandler(
    filename = function() { paste0("qqplot_normalitas_", input$normality_variable, "_", Sys.Date(), ".png") },
    content = function(file) {
      req(rendered_interpretations$normality_plot_obj)
      ggsave(file, plot = rendered_interpretations$normality_plot_obj, device = "png", width = 8, height = 6, dpi = 300)
    }
  )
  
  output$download_asumsi_report_pdf <- downloadHandler(
    filename = function() { paste0("laporan_uji_asumsi_", Sys.Date(), ".pdf") },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      file.copy("asumsi_report_template.Rmd", temp_rmd, overwrite = TRUE)
      params <- list(
        normality_test = rendered_interpretations$normality_test_output,
        normality_interp = rendered_interpretations$normality_interpretation,
        homogeneity_test = rendered_interpretations$homogeneity_test_output,
        homogeneity_interp = rendered_interpretations$homogeneity_interpretation
      )
      rmarkdown::render(temp_rmd, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  output$download_beda_rata_report <- downloadHandler(
    filename = function() { paste0("laporan_uji_beda_rata_", Sys.Date(), ".pdf") },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      file.copy("beda_rata_report_template.Rmd", temp_rmd, overwrite = TRUE)
      params <- list(
        t_one_output = rendered_interpretations$t_one_test_output,
        t_one_interp = rendered_interpretations$t_one_interpretation,
        t_two_output = rendered_interpretations$t_two_test_output,
        t_two_interp = rendered_interpretations$t_two_interpretation
      )
      rmarkdown::render(temp_rmd, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  output$download_prop_varians_report <- downloadHandler(
    filename = function() { paste0("laporan_prop_varians_", Sys.Date(), ".pdf") },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      file.copy("prop_varians_report_template.Rmd", temp_rmd, overwrite = TRUE)
      params <- list(
        prop_one_output = rendered_interpretations$prop_one_test_output,
        prop_one_interp = rendered_interpretations$prop_one_interpretation,
        var_one_output = rendered_interpretations$var_one_test_output,
        var_one_interp = rendered_interpretations$var_one_interpretation,
        var_two_output = rendered_interpretations$var_two_test_output,
        var_two_interp = rendered_interpretations$var_two_interpretation
      )
      rmarkdown::render(temp_rmd, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  output$download_anova_report <- downloadHandler(
    filename = function() { paste0("laporan_anova_", Sys.Date(), ".pdf") },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      file.copy("anova_report_template.Rmd", temp_rmd, overwrite = TRUE)
      params <- list(
        anova_output = rendered_interpretations$anova_test_output,
        anova_interp = rendered_interpretations$anova_interpretation
      )
      rmarkdown::render(temp_rmd, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  output$download_regresi_report <- downloadHandler(
    filename = function() { paste0("laporan_regresi_teks_", Sys.Date(), ".pdf") },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      file.copy("regresi_report_template.Rmd", temp_rmd, overwrite = TRUE)
      params <- list(
        model_summary = rendered_interpretations$reg_model_summary_output,
        model_interp = rendered_interpretations$reg_model_interpretation,
        normality_test = rendered_interpretations$reg_normality_test_output,
        multicollinearity_test = rendered_interpretations$reg_multicollinearity_test_output
      )
      rmarkdown::render(temp_rmd, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  output$download_reg_qq_png <- downloadHandler(
    filename = function() { paste0("regresi_qqplot_", Sys.Date(), ".png") },
    content = function(file) {
      req(rendered_interpretations$reg_normality_plot_obj)
      ggsave(file, plot = rendered_interpretations$reg_normality_plot_obj, device="png", width=8, height=6, dpi=300)
    }
  )
  
  output$download_reg_resid_png <- downloadHandler(
    filename = function() { paste0("regresi_residuals_", Sys.Date(), ".png") },
    content = function(file) {
      req(rendered_interpretations$reg_homoscedasticity_plot_obj)
      ggsave(file, plot = rendered_interpretations$reg_homoscedasticity_plot_obj, device="png", width=8, height=6, dpi=300)
    }
  )
}

# Jalankan aplikasi Shiny
shinyApp(ui = ui, server = server)
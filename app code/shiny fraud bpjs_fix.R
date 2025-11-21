library(glue)
library(shiny)
library(xgboost)
library(gemini.R)
library(tidyverse)
library(shinycssloaders)
library(openai)
library(waiter)
library(htmltools)
library(DT)



#API GEMINI
setAPI("AIzaSyAFYzJTKe4Hur_nl9eve_fSzXwtrtfpjHw")

#API OpenAI
Sys.setenv(
  OPENAI_API_KEY = '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'
)

#Open model
setwd("C:/Users/dhihr/OneDrive/riset bu yeni")
modeldt <- readRDS("xgb500_rfe.RData")
modeldt_util <- readRDS("xgb500_test_utilization.RData")

# Nilai rata-rata dan IQR kami ekstraksi dari data sampel BPJS lansia kami. 
#cost
mean_cost <- 12740059
Q1_cost <- 1959675
Q3_cost <- 16266725
IQR_cost <- Q3_cost - Q1_cost
lower_bound_cost <- max(0, Q1_cost - 1.5 * IQR_cost)
upper_bound_cost <- Q3_cost + 1.5 * IQR_cost

#util
mean_util <- 15.58
Q1_util <- 2
Q3_util <- 3
IQR_util <- Q3_util - Q1_util
lower_bound_util <- max(0,Q1_util - 1.5 * IQR_util)
upper_bound_util <- Q3_util + 1.5 * IQR_util

#icd chapter 14
mean_chap_14 <- round(3.5992,0)
Q1_chap_14 <- 3
Q3_chap_14 <- 4
IQR_util <- Q3_chap_14 - Q1_chap_14
lower_bound_14 <- max(0,Q1_chap_14 - 1.5 * IQR_util)
upper_bound_14 <- Q3_chap_14 + 1.5 * IQR_util


#icd chapter 9
mean_chap_9 <- round(0.632,0)
Q1_chap_9 <- 0
Q3_chap_9 <- 1
IQR_util <- Q3_chap_9 - Q1_chap_9
lower_bound_9 <- max(0,Q1_chap_9 - 1.5 * IQR_util)
upper_bound_9 <- Q3_chap_9 + 1.5 * IQR_util


ui <- navbarPage(
  title = " ",
  id = "main_navbar",
  theme = shinythemes::shinytheme("flatly"),  # optional
  
  # ===============================
  # LOADING SCREEN (Waiter)
  # ===============================
  header = tagList(
    use_waiter(),
    waiter_show_on_load(
      html = tagList(
        tags$img(
          src   = "https://raw.githubusercontent.com/Dhihram/makara_syndicate/refs/heads/main/makara_syndicate2.gif",
          height = "100px",
          style  = "margin-bottom:30px;"
        ),
        h3("Memuat aplikasi…", 
           style = "color:#821d7a; animation: pulse 1.5s infinite;"
        ),
        p("Silakan tunggu sebentar", 
          style = "color:#821d7a; animation: pulse 1.5s infinite;"
        ),
        tags$style(HTML("
          @keyframes pulse {
            0% { opacity: 0.3; }
            50% { opacity: 1; }
            100% { opacity: 0.3; }
          }
        "))
      ),
      color = "#FFFDFE"
    )
  ),
  
  # ===============================
  # HEADER LOGO AND TITLE
  # ===============================
  tabPanel("Manual Input",
           fluidPage(
             fluidRow(
               column(
                 width = 9,
                 div(
                   style = "text-align:left; margin-left:20px;",
                   h2(strong("Kalkulator Risiko Anomali BPJS")),
                   p(em("oleh Makara Syndicate"))
                 )
               ),
               column(
                 width = 3,
                 div(
                   align = "right",
                   tags$img(
                     src = "https://raw.githubusercontent.com/Dhihram/makara_syndicate/refs/heads/main/logo_tim.png",
                     height = "70px",
                     style = "margin-top:10px; margin-right:18px;"
                   )
                 )
               )
             ),
             
             # ===============================
             # MAIN CONTENT
             # ===============================
             sidebarLayout(
               sidebarPanel(
                 width = 7,
                 h4("Input Pasien"),
                 fluidRow(
                   column(4, numericInput("usia", "Usia Pasien:", value = 60, min = 0, max = 120, step = 1)),
                   column(4, selectInput("gender", "Jenis Kelamin:",
                                         choices = list("Laki-laki" = "LAKI-LAKI", "Perempuan" = "PEREMPUAN"))),
                   column(4, selectInput("pernikahan", "Status Pernikahan:",
                                         choices = list("Belum Kawin" = "BELUM KAWIN", "Kawin" = "KAWIN", 
                                                        "Cerai" = "CERAI", "Tidak Terdefinisi" = "TIDAK TERDEFINISI")))
                 ),
                 fluidRow(
                   column(4, selectInput("peserta", "Kepesertaan:",
                                         choices = list("Bukan Pekerja" = "BUKAN PEKERJA", "PBI APBD", "PBI APBN", "PBPU", "PPU")))
                 ),
                 
                 hr(),
                 h4("Total Utilisasi Fasilitas"),
                 fluidRow(
                   column(4, numericInput("los", "Total Hari Lama Rawat:", 1, 0, 999, 1)),
                   column(4, numericInput("RJTL", "Utilisasi RJTL:", 1, 0, 999, 1)),
                   column(4, numericInput("RS.Swasta", "RS Swasta:", 0, 0, 999, 1))
                 ),
                 fluidRow(
                   column(4, numericInput("RS.Milik.Pemerintah", "RS Pemerintah:", 1, 0, 999, 1)),
                   column(4, numericInput("RS.Kelas.D", "RS Kelas D:", 0, 0, 999, 1)),
                   column(4, numericInput("RS.Kelas.C", "RS Kelas C:", 0, 0, 999, 1))
                 ),
                 fluidRow(
                   column(4, numericInput("RS.Kelas.B", "RS Kelas B:", 1, 0, 999, 1)),
                   column(4, numericInput("RS.Kelas.A", "RS Kelas A:", 0, 0, 999, 1)),
                   column(4, numericInput("RS.Khusus", "RS Khusus:", 0, 0, 999, 1))
                 ),
                 fluidRow(
                   column(4, numericInput("KELAS.I", "Lama Utilisasi Kelas I:", 1, 0, 999, 1))
                 ),
                 
                 hr(),
                 h4("Total Kategorisasi Keparahan"),
                 fluidRow(
                   column(4, numericInput("ringan", "Keparahan Ringan:", 1, 0, 999, 1)),
                   column(4, numericInput("sedang", "Keparahan Sedang:", 0, 0, 999, 1))
                 ),
                 
                 hr(),
                 h4("Total ICD-10 Utama"),
                 fluidRow(
                   column(3, numericInput("chap_1", "Chapter I:", 0, 0, 999)),
                   column(3, numericInput("chap_2", "Chapter II:", 0, 0, 999)),
                   column(3, numericInput("chap_3", "Chapter III:", 0, 0, 999)),
                   column(3, numericInput("chap_4", "Chapter IV:", 0, 0, 999))
                 ),
                 fluidRow(
                   column(3, numericInput("chap_5", "Chapter V:", 0, 0, 999)),
                   column(3, numericInput("chap_6", "Chapter VI:", 0, 0, 999)),
                   column(3, numericInput("chap_7", "Chapter VII:", 1, 0, 999)),
                   column(3, numericInput("chap_8", "Chapter VIII:", 0, 0, 999))
                 ),
                 fluidRow(
                   column(3, numericInput("chap_9", "Chapter IX:", 0, 0, 999)),
                   column(3, numericInput("chap_10", "Chapter X:", 0, 0, 999)),
                   column(3, numericInput("chap_11", "Chapter XI:", 0, 0, 999)),
                   column(3, numericInput("chap_12", "Chapter XII:", 0, 0, 999))
                 ),
                 fluidRow(
                   column(3, numericInput("chap_13", "Chapter XIII:", 0, 0, 999)),
                   column(3, numericInput("chap_14", "Chapter XIV:", 1, 0, 999)),
                   column(3, numericInput("chap_15", "Chapter XV:", 0, 0, 999)),
                   column(3, numericInput("chap_17", "Chapter XVII:", 0, 0, 999))
                 ),
                 fluidRow(
                   column(3, numericInput("chap_18", "Chapter XVIII:", 0, 0, 999)),
                   column(3, numericInput("chap_19", "Chapter XIX:", 0, 0, 999)),
                   column(3, numericInput("chap_21", "Chapter XXI:", 0, 0, 999))
                 ),
                 
                 hr(),
                 actionButton(
                   inputId = "go",
                   label = "Prediksi!",
                   style = "background-color:#821d7a; color:white; font-weight:bold; border:none;"
                 )
               ),
               
               mainPanel(
                 width = 5,
                 h4("Hasil Prediksi"),
                 withSpinner(htmlOutput("userOutput_util"), type = 4, color = "#821d7a", size = 1.5),
                 hr(),
                 withSpinner(htmlOutput("userOutput"), type = 4, color = "#821d7a", size = 1.5),
                 withSpinner(htmlOutput("geminioutput"), type = 4, color = "#821d7a", size = 1.5)
               )
             )
           )
  ),
  
  # ===============================
  # TAB: Upload CSV
  # ===============================
  tabPanel(
    "Upload CSV",
    fluidPage(
      # ===============================
      # HEADER
      # ===============================
      fluidRow(
        column(
          width = 9,
          div(
            style = "text-align:left; margin-left:20px;",
            h2(strong("Kalkulator Risiko Anomali BPJS")),
            p(em("oleh Makara Syndicate"))
          )
        ),
        column(
          width = 3,
          div(
            align = "right",
            tags$img(
              src = "https://raw.githubusercontent.com/Dhihram/makara_syndicate/refs/heads/main/logo_tim.png",
              height = "70px",
              style = "margin-top:10px; margin-right:18px;"
            )
          )
        )
      ),
      
      # ===============================
      # MAIN CONTENT
      # ===============================
      sidebarLayout(
        sidebarPanel(
          width = 4,
          h4("Unggah File CSV"),
          fileInput(
            inputId = "csv_file",
            label = "Pilih file CSV:",
            accept = c(".csv"),
            buttonLabel = "Browse...",
            placeholder = "Belum ada file dipilih"
          ),
          helpText("Pastikan file CSV berisi kolom sesuai format prediksi (usia, gender, keparahan, dll)."),
          actionButton(
            inputId = "process_csv",
            label = "Proses Data",
            style = "background-color:#821d7a; color:white; font-weight:bold; border:none;"
          )
        ),
        
        mainPanel(
          width = 8,
          h4("Hasil Prediksi"),
          withSpinner(DTOutput("table"), type = 4, color = "#821d7a", size = 1.5),
          hr(),
          withSpinner(htmlOutput("geminioutput2"), type = 4, color = "#821d7a", size = 1.5),
          hr(),
          withSpinner(DTOutput("table2"), type = 4, color = "#821d7a", size = 1.5),
          hr(),
          withSpinner(htmlOutput("openai2"), type = 4, color = "#821d7a", size = 1.5),
        )
      )
    )
  ),
  
  tabPanel("Tentang", 
           h4(HTML("<b>Tentang Aplikasi</b>")),
           p("Kalkulator Risiko Anomali Klaim BPJS (KARA) dikembangkan oleh Makara Syndicate untuk mendukung deteksi dini anomali klaim berdasarkan data sampel BPJS Kesehatan (2017-2024).
             Aplikasi ini berawal dari penelitian tim ini yang telah diterima untuk publish di Journal of Preventive Medicine & Public Health (Manuscript ID: JPMPH-25-350) berjudul 
             ‘Development of Machine Learning Models to Predict Health Insurance Claim Costs Among Elderly Indonesians: A Retrospective Predictive Modeling Study’. 
             Aplikasi ini menggunakan pendekatan model machine learning untuk memprediksi total pembiayaan dan total utilisasi untuk 7 tahun ke depan dari seorang pasien. 
             Model-model tersebut menggunakan data kohort sampel BPJS Kesehatan tahun 2017-2023. Selain itu, aplikasi ini menggunakan Generative AI 
             untuk melakukan interpretasi dari hasil prediksi dan penilaian risiko anomali pembiayaan, dan utilisasi berlebih terkait penyakit-penyakit katastropik (penyakit ginjal dan penyakit jantung)."),
           hr(),
           h4(HTML("<b>Tutorial Penggunaan</b>")),
           tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/GucY3BLEnoc?si=hV_tMKrGWPIAuF9T" , frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)   
  )
)


server <- function(input, output, session) {
  # Menampilkan loading selama 5 detik
  Sys.sleep(13)
  waiter_hide()
  data <- eventReactive(input$go, {
    data <- data.frame(
      usia = c(as.numeric(input$usia)),
      gender = c(as.factor(input$gender)),
      pernikahan = c(as.factor(input$pernikahan)),
      peserta = c(as.factor(input$peserta)),
      los = c(as.numeric(input$los)),
      RJTL = c(as.numeric(input$RJTL)),
      RS.Swasta = c(as.numeric(input$RS.Swasta)),
      RS.Milik.Pemerintah = c(as.numeric(input$RS.Milik.Pemerintah)),
      RS.Kelas.D = c(as.numeric(input$RS.Kelas.D)),
      RS.Kelas.C = c(as.numeric(input$RS.Kelas.C)),
      RS.Kelas.B = c(as.numeric(input$RS.Kelas.B)),
      RS.Kelas.A = c(as.numeric(input$RS.Kelas.A)),
      RS.Khusus = c(as.numeric(input$RS.Khusus)),
      KELAS.I = c(as.numeric(input$KELAS.I)),
      ringan = c(as.numeric(input$ringan)),
      sedang = c(as.numeric(input$sedang)),
      chap_1 = c(as.numeric(input$chap_1)),
      chap_2 = c(as.numeric(input$chap_2)),
      chap_3 = c(as.numeric(input$chap_3)),
      chap_4 = c(as.numeric(input$chap_4)),
      chap_5 = c(as.numeric(input$chap_5)),
      chap_6 = c(as.numeric(input$chap_6)),
      chap_7 = c(as.numeric(input$chap_7)),
      chap_8 = c(as.numeric(input$chap_8)),
      chap_9 = c(as.numeric(input$chap_9)),
      chap_10 = c(as.numeric(input$chap_10)),
      chap_11 = c(as.numeric(input$chap_11)),
      chap_12 = c(as.numeric(input$chap_12)),
      chap_13 = c(as.numeric(input$chap_13)),
      chap_14 = c(as.numeric(input$chap_14)),
      chap_15 = c(as.numeric(input$chap_15)),
      chap_17 = c(as.numeric(input$chap_17)),
      chap_18 = c(as.numeric(input$chap_18)),
      chap_19 = c(as.numeric(input$chap_19)),
      chap_21 = c(as.numeric(input$chap_21))
    )
    data
})

# Reactive expression to read the uploaded CSV
uploaded_data <- reactive({
  req(input$csv_file)
  req(input$process_csv > 0)

  tryCatch({
    read.csv(input$csv_file$datapath, stringsAsFactors = FALSE)
  }, error = function(e) {
    showNotification("Gagal membaca file CSV. Pastikan format benar.", type = "error")
    return(NULL)
  })
})



#prediction      
prediction <- reactive({
    req(input$go > 0)  
    newdata <- data()
    pred <- predict(modeldt, newdata)
    pred
  })

prediction_util <- reactive({
  req(input$go > 0)  
  newdata <- data()
  pred <- predict(modeldt_util, newdata)
  pred
})
  
prediction_upload <- reactive({
  req(input$process_csv > 0)
  dat <- uploaded_data()
  dat <- dat %>% mutate(across(where(is.character), as.factor))
  
  dat$pred_val_util <- round(predict(modeldt_util, dat), 0)
  dat$pred_val <- round(predict(modeldt, dat), 0)
  
  dat <- dat %>%
    mutate(
      risk_level_util = case_when(
        pred_val_util < lower_bound_util ~ "Rendah",
        pred_val_util > upper_bound_util ~ "Tinggi",
        TRUE ~ "Sedang"),
      risk_level_cost = case_when(
        pred_val < lower_bound_cost ~ "Rendah",
        pred_val > upper_bound_cost ~ "Tinggi",
        TRUE ~ "Sedang"),
      risk_level_icd_14 = case_when(
        chap_14 < lower_bound_14 ~ "Rendah",
        chap_14 > upper_bound_14 ~ "Tinggi",
        TRUE ~ "Sedang"),
      risk_level_icd_9 = case_when(
        chap_9 < lower_bound_9 ~ "Rendah",
        chap_9 > upper_bound_9 ~ "Tinggi",
        TRUE ~ "Sedang"),
      risk_level = case_when(
        risk_level_util == "Tinggi" | risk_level_cost == "Tinggi" ~ "Tinggi",
        risk_level_util == "Rendah" & risk_level_cost == "Rendah" ~ "Rendah",
        TRUE ~ "Sedang"),
      risk_level_icd = case_when(
        risk_level_icd_14 == "Tinggi" | risk_level_icd_9 == "Tinggi" ~ "Tinggi",
        risk_level_icd_14 == "Rendah" & risk_level_icd_9 == "Rendah" ~ "Rendah",
        TRUE ~ "Sedang")
    )
  dat
})

#output
output$userOutput <- renderUI({
  pred_val <- prediction()
  
  # Only format if numeric
  if (is.numeric(pred_val)) {
    formatted_pred <- format(
      round(pred_val, 0),   # no decimals
      big.mark = ".",       # thousand separator
      decimal.mark = ","     # no decimal part
    )
    text_pred <- paste0("Rp. ",formatted_pred)
    HTML(glue("
        Dari data input, hasil prediksi total biaya untuk 7 tahun kedepan adalah:
        <b><div style='color:#0073C2; font-size:22px; margin-top:10px;'>
          {text_pred}
        </div></b>
      "))
  } else {
    HTML(glue("
        Dari data input, hasil prediksi total biaya untuk 7 tahun kedepan adalah:
        <b><div style='color:#D9534F; font-size:22px; margin-top:10px;'>
          {pred_val}
        </div></b>
      "))
  }
 })

output$userOutput_util <- renderUI({
  pred_val <- prediction_util()
  
  # Only format if numeric
  if (is.numeric(pred_val)) {
    formatted_pred <- format(
      round(pred_val, 0)   # no decimals
    )
    HTML(glue("
        Dari data input, hasil prediksi total utilisasi untuk 7 tahun kedepan adalah:
        <b><div style='color:#0073C2; font-size:22px; margin-top:10px;'>
          {formatted_pred}
        </div></b>
      "))
  } else {
    HTML(glue("
        Dari data input, hasil prediksi total utilisasi untuk 7 tahun kedepan adalah:
        <b><div style='color:#D9534F; font-size:22px; margin-top:10px;'>
          {pred_val}
        </div></b>
      "))
  }
})
  
output$geminioutput <- renderUI({
  req(input$go > 0)
  
  # Nilai prediksi dari model
  pred_val  <- prediction()
  pred_val_util <- prediction_util()
  
  #nilai batas ICD 14
  icd_val_14  <- as.numeric(input$chap_14)
  icd_val_9 <- as.numeric(input$chap_9)
  
  # Klasifikasi risiko berdasarkan prediksi
  risk_level_util <- case_when(
    pred_val_util < lower_bound_util ~ "Rendah",
    pred_val_util > upper_bound_util ~ "Tinggi",
    TRUE ~ "Sedang"
  )
  
  risk_level_cost <- case_when(
    pred_val < lower_bound_cost ~ "Rendah",
    pred_val > upper_bound_cost ~ "Tinggi",
    TRUE ~ "Sedang"
  )
  
  risk_level_icd_14 <- case_when(
    icd_val_14 < lower_bound_14~ "Rendah",
    icd_val_14 > upper_bound_14 ~ "Tinggi",
    TRUE ~ "Sedang"
  )
  
  risk_level_icd_9 <- case_when(
    icd_val_9 < lower_bound_9 ~ "Rendah",
    icd_val_9 > upper_bound_9 ~ "Tinggi",
    TRUE ~ "Sedang"
  )
  
  risk_level <- case_when(
    risk_level_util == "Tinggi" | risk_level_cost == "Tinggi" ~ "Tinggi",
    risk_level_util == "Rendah" & risk_level_cost == "Rendah" ~ "Rendah",
    TRUE ~ "Sedang"
  )
  
  risk_level_icd <- case_when(
    risk_level_icd_14 == "Tinggi" | risk_level_icd_9 == "Tinggi" ~ "Tinggi",
    risk_level_icd_14 == "Rendah" & risk_level_icd_9 == "Rendah" ~ "Rendah",
    TRUE ~ "Sedang"
  )
  # Tentukan warna sesuai risiko icd_14
  risk_color <- case_when(
    risk_level == "Rendah" ~ "#4CAF50",   # hijau
    risk_level == "Sedang" ~ "#FFEB3B",   # kuning
    risk_level == "Tinggi" ~ "#eb5074",   # merah
    TRUE ~ "#CCCCCC"
  )
  
  # Tentukan warna sesuai risiko
  risk_level_icd_color <- case_when(
    risk_level_icd == "Rendah" ~ "#4CAF50",   # hijau
    risk_level_icd == "Sedang" ~ "#FFEB3B",   # kuning
    risk_level_icd == "Tinggi" ~ "#eb5074",   # merah
    TRUE ~ "#CCCCCC"
  )
  
  # Buat prompt untuk Gemini
  gemini_prompt <- glue("
  Kamu adalah seorang analis verifikator klaim BPJS.
Model memprediksi potensi biaya pasien ini untuk 7 tahun ke depan sebesar {format(round(pred_val, 0), big.mark='.', decimal.mark=',')}.
Nilai rata-rata biaya seluruh populasi adalah {format(round(mean_cost, 0), big.mark='.', decimal.mark=',')} .

Model juga memprediksi potensi utilisasi perawatan di rumah sakit untuk 7 tahun ke depan sebesar {format(round(pred_val_util, 0), big.mark='.', decimal.mark=',')}.
Nilai rata-rata utilisasi perawatan seluruh populasi adalah {format(round(mean_util, 0), big.mark='.', decimal.mark=',')}.
Dari analisis data di atas diperoleh kategori risiko utilisasi berlebih dan pembiayaan berlebih: {risk_level}.

Tolong tambahkan kategori risiko tersebut terkait beban pembiayaan BPJS dan fraud untuk pasien ini dalam kalimat pendek.

Tuliskan kesimpulan pendek dari analisis di atas.
Gunakan gaya bahasa profesional, formal, dan mudah dipahami.  
Jangan gunakan tanda bintang, garis bawah, atau simbol penekanan lainnya dalam teks.
")
  
  # Panggil Gemini
  paraphrased <- gemini(gemini_prompt)
  paraphrased <- gsub("\\*", "", paraphrased)     # hapus asterisk
  paraphrased <- gsub("_", "", paraphrased)       # hapus underscore (opsional)
  
  # Bungkus dengan <p> ... </p> agar rapi
  paraphrased_formatted <- paste0("<p>", paraphrased, "</p>")
  
#menggunakan OPENAI
  
  answer = create_chat_completion(
    model = "gpt-4o-mini",
    temperature = 0,
    messages = list(
      list(
        "role" = "system",
        "content" = "You are a professional clinical analyst that generates concise and clear risk explanations in Indonesian for health assessment tools."
      ),
      list(
        "role" = "user",
        "content" = glue::glue("
        Dari data pasien ini, tercatat jumlah diagnosis terkait penyakit ginjal (ICD Chapter 14) selama 7 tahun terakhir sebanyak {format(as.numeric(input$chap_14), big.mark='.', decimal.mark=',')}.
        Dari data pasien ini, tercatat jumlah diagnosis terkait penyakit jantung (ICD Chapter 9) selama 7 tahun terakhir sebanyak {format(as.numeric(input$chap_9), big.mark='.', decimal.mark=',')}.
        Nilai rata-rata diagnosis terkait dari seluruh populasi terkait penyakit ginjal (ICD Chapter 14) adalah {format(round(mean_chap_14, 0), big.mark='.', decimal.mark=',')}.
        Nilai rata-rata diagnosis terkait dari seluruh populasi terkait penyakit ginjal (ICD Chapter 14) adalah {format(round(mean_chap_9, 0), big.mark='.', decimal.mark=',')}.
        Dari analisis data di atas diperoleh kategori risiko utilisasi berlebih terkait penyakit ginjal dan penyakit jantung untuk 7 tahun kedepan adalah: {risk_level_icd}.
        Tolong tambahkan kategori risiko tersebut terkait beban pembiayaan BPJS dan fraud untuk pasien ini.
        Tuliskan 2 kalimat kesimpulan dari analisis diatas.
      ")
      )
    )
  )
  
  paraphrased2 <- answer$choices$message.content
  paraphrased2 <- gsub("\\*", "", paraphrased2)     # hapus asterisk
  paraphrased2 <- gsub("_", "", paraphrased2)       # hapus underscore (opsional)
  
  # Bungkus dengan <p> ... </p> agar rapi
  paraphrased_formatted2 <- paste0("<p>", paraphrased2, "</p>")
  
  # Render output HTML
  HTML(glue("
  <hr>
  </div>
  <div style='margin-top:10px;'>
    <b>Risiko Anomali Pembiayaan dan Utilisasi:</b><br>
    <div style='margin-top:5px; padding:10px; border-radius:8px;
                background-color:{risk_color}; color:black;
                font-weight:bold; width:fit-content;'>
      {risk_level}
    </div>
  </div>
  <br>
     <i>Penjelasan AI:</i><br>
  <div style='margin-top:10px; font-size:16px;'>{paraphrased_formatted}</div>
  <hr>
  <b>Risiko Anomali Diagnosis Penyakit Jantung dan Ginjal:</b><br>
  <div style='margin-top:5px; padding:10px; border-radius:8px;
                background-color:{risk_level_icd_color}; color:black;
                font-weight:bold; width:fit-content;'>
      {risk_level_icd}
   </div>
  </div>
  <br>
     <i>Penjelasan AI:</i><br>
  <div style='margin-top:10px; font-size:16px;'>{paraphrased_formatted2}</div>
"))
})

# =========================
# TABLE OUTPUT
# =========================
output$table <- renderDataTable({
  req(input$process_csv > 0)
  dat <- prediction_upload()
  
  tab1 <- dat %>%
    select(PSTV01, pred_val_util, pred_val, risk_level)
  
  colnames(tab1) <- c("PSTV01", "Predict Utilization", "Predict Cost", "Risk Level")
  
  datatable(
    tab1,
    class = 'cell-border stripe',
    options = list(pageLength = 5),
    caption = 'Tabel 1: Tabel prediksi dan risiko pembiayaan-utilisasi.',
    rownames = FALSE
  ) %>%
    formatCurrency(
      columns = "Predict Cost",
      currency = "Rp. ",
      mark = ",",
      digits = 0
    ) %>%
    formatStyle(
      "Risk Level",
      backgroundColor = styleEqual(
        c("Rendah", "Sedang", "Tinggi"),
        c("#4CAF50", "#FFEB3B", "#eb5074"))
    )
})

output$table2 <- renderDataTable({
  req(input$process_csv > 0)
  dat <- prediction_upload()
  
  tab1 <- dat %>% select(PSTV01, chap_9,chap_14, risk_level_icd)
  
  colnames(tab1) <- c("PSTV01", "Total Diagnosed ICD Chapter 9", 
                      "Total Diagnosed ICD Chapter 14", "Risk Level")
  datatable(
    tab1,
    class = 'cell-border stripe',
    options = list(pageLength = 5),
    caption = 'Tabel 2: Tabel jumlah dan risiko diagnosis ICD Chapter 9 dan Chapter 14.',
    rownames = FALSE
  )  %>% 
    formatStyle(
      'Risk Level',
      backgroundColor = styleEqual(c('Rendah', 'Sedang', 'Tinggi'), 
                                   c('#4CAF50', '#FFEB3B', '#eb5074'))
    )
})

# =========================
# AI OUTPUT
# =========================

output$geminioutput2 <- renderUI({
  req(input$process_csv > 0)
  dat <- prediction_upload()
  req(dat)
  
  tab1 <- dat %>%
    select(PSTV01, pred_val_util, pred_val, risk_level) %>%
    head(10)
  
  colnames(tab1) <- c("PSTV01", "Prediksi Utilisasi", "Prediksi Biaya", "Tingkat Risiko")
  
  # --- Buat prompt untuk Gemini ---
  text_input <- paste(
    "Kamu adalah seorang analis untuk BPJS.",
    "Tabel berikut menampilkan hasil prediksi utilisasi dan biaya pasien BPJS:",
    paste(capture.output(print(tab1)), collapse = "\n"),
    "\n\nTuliskan satu paragraf pendek dalam Bahasa Indonesia",
    "yang menjelaskan tren umum biaya dan utilisasi pasien serta interpretasi tingkat risiko anomali (Rendah, Sedang, Tinggi).",
    "Interpretasikan anomali ini terkait potensi fraud",
    "PSTV01 adalah ID pasien, dan tidak perlu dianalisis.",
    "Tunjukkan pasien mana yang menonjol dalam data tersebut.",
    "Gunakan gaya bahasa profesional, formal, dan mudah dipahami.",
    "Jangan gunakan tanda bintang, garis bawah, atau simbol penekanan lainnya dalam teks."
  )
  
  # --- Panggilan ke Gemini API ---
  response <- gemini(prompt = text_input)
  
  # --- Tampilkan hasil dalam HTML ---
  HTML(
    paste0(
      "<i>Penjelasan AI:</i><br>
      <div style='margin-top:10px; font-size:16px;'>",
      response,
      "</div>"
    )
  )
})

output$openai2 <- renderUI({
  req(input$process_csv > 0)
  dat <- prediction_upload()
  req(dat)
  
  tab1 <- dat %>% select(chap_9,chap_14, risk_level_icd)
  
  colnames(tab1) <- c("Total Diagnosed ICD Chapter 9", 
                      "Total Diagnosed ICD Chapter 14", "Risk Level")
  answer = create_chat_completion(
    model = "gpt-4o-mini",
    temperature = 0,
    messages = list(
      list(
        "role" = "system",
        "content" = "You are a professional clinical analyst that generates concise and clear risk explanations in Indonesian for health assessment tools."
      ),
      list(
        "role" = "user",
        "content" = paste(
          "Tabel berikut menampilkan hasil prediksi utilisasi dan biaya pasien BPJS:",
          paste(capture.output(print(head(tab1, 10))), collapse = "\n"),
          "\n\nTuliskan satu paragraf pendek dalam Bahasa Indonesia",
          "yang menjelaskan tren umum diagnosis ICD 10 Chapter 9 dan Chapter 14 serta interpretasi tingkat risiko anomali utilisasi (Rendah, Sedang, Tinggi).",
          "Interpretasikan anomali ini terkait potensi fraud over utilisasi dari total diagnosis ICD 10 Chapter 9 dan Chapter 14 pada pasien, dengan rata-rata ICD Chapter 9 dari database pasien:", paste(mean_chap_9), "dan rata-rata ICD Chapter 14 dari database pasien:",
          paste(mean_chap_14),
          "Gunakan istilah ICD 10 Chapter 9 = penyakit jantung dan ICD 10 Chapter 14 = penyakit ginjal",
          "Tunjukkan pasien mana yang mencurigakan dalam data tersebut jika Total ICD 10 Chapter 9 dibandingkan  rata-rata ICD Chapter 9 dari database pasien:", paste(mean_chap_9), 
          "dan Total ICD 10 Chapter 14 darirata-rata ICD Chapter 14 dari database pasien:",
          paste(mean_chap_14),
          "Gunakan gaya bahasa profesional, formal, dan mudah dipahami.",  
          "Jangan gunakan tanda bintang, garis bawah, atau simbol penekanan lainnya dalam teks.")
      )
    )
  )
  
  response <- answer$choices$message.content
  # --- Tampilkan hasil dalam HTML ---
  HTML(
    paste0(
      "<i>Penjelasan AI:</i><br>
      <div style='margin-top:10px; font-size:16px;'>",
      response,
      "</div>"
    )
  )
})

}
shinyApp(ui, server)

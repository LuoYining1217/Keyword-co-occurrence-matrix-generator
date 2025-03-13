library(shiny)
library(XML)
library(stringr)
library(dplyr)
library(DT)
library(stringdist)

ui <- fluidPage(
  titlePanel("📊 关键词共现矩阵生成器"),
  
  sidebarLayout(
    sidebarPanel(
      h4("📂 数据导入"),
      fileInput("txt_files", "选择多个 TXT 文件", multiple = TRUE, accept = ".txt"),
      actionButton("generate", "🚀 生成关键词统计", class = "btn-primary"),
      br(), hr(),
      
      h4("📝 关键词管理"),
      actionButton("recommend_synonyms", "🔍 推荐同义词", class = "btn-info"),
      actionButton("confirm_merge", "✅ 确认合并", class = "btn-success"),
      actionButton("update_matrix", "📈 计算共现矩阵", class = "btn-warning"),
      br(), hr(),
      
      h4("📥 下载结果"),
      downloadButton("download_matrix", "📂 下载共现矩阵"),
      br(), br(),
      verbatimTextOutput("status")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("📜 关键词统计", DTOutput("keyword_table")),
        tabPanel("🔍 同义词推荐", DTOutput("synonym_table")),
        tabPanel("ℹ 状态信息", verbatimTextOutput("log_info"))
      )
    )
  )
)

server <- function(input, output, session) {
  keyword_data <- reactiveVal(data.frame(Keyword = character(), Frequency = integer(), stringsAsFactors = FALSE))
  all_keywords <- reactiveVal(character())  
  synonym_map <- reactiveVal(data.frame(原关键词 = character(), 建议合并的关键词 = character(), 合并为 = character(), stringsAsFactors = FALSE))
  
  observeEvent(input$generate, {
    req(input$txt_files)
    output$status <- renderText("⏳ 正在处理，请稍候...")
    
    tryCatch({
      keyword_list_all <- character()
      
      for (file in input$txt_files$datapath) {
        txt_content <- readLines(file, encoding = "UTF-8")
        xml_text <- paste(txt_content, collapse = "\n")
        xml_data <- xmlParse(xml_text)
        
        keywords_nodes <- xpathSApply(xml_data, "//Keywords/Keyword[@Lang='zh-CHS']", xmlValue)
        if (length(keywords_nodes) == 0) next  
        
        keyword_list <- unlist(lapply(keywords_nodes, function(x) trimws(strsplit(x, ";")[[1]])))
        keyword_list_all <- c(keyword_list_all, keyword_list)
      }
      
      if (length(keyword_list_all) == 0) stop("❌ 未找到任何关键词，请检查文件格式！")
      
      keyword_freq <- as.data.frame(table(keyword_list_all), stringsAsFactors = FALSE)
      colnames(keyword_freq) <- c("Keyword", "Frequency")
      keyword_freq <- keyword_freq[order(-keyword_freq$Frequency), ]
      
      keyword_data(keyword_freq)
      all_keywords(keyword_list_all)
      
      output$status <- renderText("✅ 关键词统计完成！请检查 '同义词推荐' 标签页。")
    }, error = function(e) {
      output$status <- renderText(paste("❌ 发生错误：", e$message))
    })
  })
  
  output$keyword_table <- renderDT({
    req(nrow(keyword_data()) > 0)
    datatable(
      keyword_data(), rownames = FALSE, options = list(pageLength = 50, autoWidth = TRUE)
    )
  }, server = FALSE)
  
  observeEvent(input$recommend_synonyms, {
    req(nrow(keyword_data()) > 0)
    
    keywords <- keyword_data()$Keyword  
    distance_matrix <- stringdistmatrix(keywords, keywords, method = "jw")  
    rownames(distance_matrix) <- colnames(distance_matrix) <- keywords  
    
    threshold <- 0.15  
    synonym_map_list <- list()
    
    for (i in seq_along(keywords)) {
      similar_keywords <- keywords[distance_matrix[i, ] < threshold & distance_matrix[i, ] > 0]  
      if (length(similar_keywords) > 0) {
        synonym_map_list[[keywords[i]]] <- similar_keywords
      }
    }
    
    if (length(synonym_map_list) == 0) {
      output$status <- renderText("✅ 没有检测到明显的同义词！")
      return()
    }
    
    synonym_df <- data.frame(
      原关键词 = names(synonym_map_list),
      建议合并的关键词 = sapply(synonym_map_list, function(x) paste(x, collapse = ", ")),
      合并为 = names(synonym_map_list),  
      stringsAsFactors = FALSE
    )
    
    synonym_map(synonym_df)
    output$status <- renderText("✅ 推荐同义词已生成，请在 '同义词推荐' 中编辑 '合并为' 目标后，点击 '确认合并'。")
  })
  
  output$synonym_table <- renderDT({
    req(nrow(synonym_map()) > 0)
    datatable(
      synonym_map(), editable = "cell", rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  }, server = FALSE)
  
  observeEvent(input$confirm_merge, {
    req(nrow(synonym_map()) > 0)
    
    synonym_df <- synonym_map()
    updated_keywords <- keyword_data()
    
    for (i in seq_len(nrow(synonym_df))) {
      original_keyword <- synonym_df$原关键词[i]
      target_keyword <- synonym_df$合并为[i]
      synonyms <- strsplit(synonym_df$建议合并的关键词[i], ", ")[[1]]
      
      updated_keywords$Keyword[updated_keywords$Keyword %in% synonyms] <- target_keyword
    }
    
    keyword_data(updated_keywords)
    output$status <- renderText("✅ 选定的同义词已合并！请点击 '计算共现矩阵' 以更新数据。")
  })
  
  observeEvent(input$update_matrix, {
    req(nrow(keyword_data()) > 0)
    
    keywords <- keyword_data()$Keyword
    unique_keywords <- unique(keywords)
    keyword_count <- length(unique_keywords)
    
    if (keyword_count == 0) {
      output$status <- renderText("❌ 关键词列表为空，无法生成共现矩阵！")
      return()
    }
    
    cooccur_matrix <- matrix(0, nrow = keyword_count, ncol = keyword_count,
                             dimnames = list(unique_keywords, unique_keywords))
    
    for (kw_group in all_keywords()) {
      kw_list <- unlist(strsplit(kw_group, ";"))
      kw_list <- trimws(kw_list)
      
      if (length(kw_list) > 1) {
        keyword_pairs <- combn(kw_list, 2, simplify = FALSE)
        for (pair in keyword_pairs) {
          kw1 <- pair[1]
          kw2 <- pair[2]
          cooccur_matrix[kw1, kw2] <- cooccur_matrix[kw1, kw2] + 1
          cooccur_matrix[kw2, kw1] <- cooccur_matrix[kw2, kw1] + 1  
        }
      }
    }
    
    output$download_matrix <- downloadHandler(
      filename = function() { paste0("关键词共现矩阵-", Sys.Date(), ".csv") },
      content = function(file) { write.csv(cooccur_matrix, file, row.names = TRUE) }
    )
    
    output$status <- renderText("✅ 共现矩阵计算成功！请下载文件。")
  })
}

shinyApp(ui = ui, server = server)

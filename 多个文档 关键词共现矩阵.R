install.packages("shiny")    # 安装 Shiny
install.packages("XML")      # 安装 XML 解析库
install.packages("stringr")  # 安装字符串处理库
install.packages("dplyr")    # 安装数据操作库


library(shiny)
library(XML)
library(stringr)
library(dplyr)

ui <- fluidPage(
  titlePanel("多文档关键词共现矩阵生成器"),
  sidebarLayout(
    sidebarPanel(
      fileInput("txt_files", "选择多个 TXT 文件", multiple = TRUE, accept = ".txt"),
      textInput("output_path", "保存路径（包括文件名）", value = "D:/关键词共现矩阵.csv"),
      actionButton("generate", "生成关键词共现矩阵"),
      downloadButton("download_matrix", "下载共现矩阵")
    ),
    mainPanel(
      verbatimTextOutput("status"),
      h3("关键词共现矩阵预览"),
      tableOutput("matrix_preview")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$generate, {
    req(input$txt_files)  # 确保至少有一个文件上传
    req(input$output_path)  # 确保保存路径已输入
    
    output$status <- renderText("正在处理，请稍候...")
    
    tryCatch({
      all_keywords <- list()  # 用于存储所有文件的关键词
      
      # 遍历用户上传的所有文件
      for (file in input$txt_files$datapath) {
        # 读取 TXT 文件内容
        txt_content <- readLines(file, encoding = "UTF-8")
        xml_text <- paste(txt_content, collapse = "\n")
        xml_data <- xmlParse(xml_text)  # 解析 XML
        
        # 提取所有中文关键词
        keywords_nodes <- xpathSApply(xml_data, "//Keywords/Keyword[@Lang='zh-CHS']", xmlValue)
        
        # 处理关键词字段（按 `;` 拆分）
        keyword_list <- lapply(keywords_nodes, function(x) trimws(strsplit(x, ";")[[1]]))
        
        # 添加到全局关键词列表
        all_keywords <- c(all_keywords, keyword_list)
      }
      
      # 获取所有唯一关键词
      unique_keywords <- unique(unlist(all_keywords))
      keyword_count <- length(unique_keywords)
      
      # 初始化共现矩阵
      cooccur_matrix <- matrix(0, nrow = keyword_count, ncol = keyword_count,
                               dimnames = list(unique_keywords, unique_keywords))
      
      # 计算共现次数
      for (keywords in all_keywords) {
        if (length(keywords) > 1) {
          keyword_pairs <- combn(keywords, 2, simplify = TRUE)
          for (pair in 1:ncol(keyword_pairs)) {
            kw1 <- keyword_pairs[1, pair]
            kw2 <- keyword_pairs[2, pair]
            cooccur_matrix[kw1, kw2] <- cooccur_matrix[kw1, kw2] + 1
            cooccur_matrix[kw2, kw1] <- cooccur_matrix[kw2, kw1] + 1  # 矩阵对称
          }
        }
      }
      
      # 保存共现矩阵为 CSV
      write.csv(cooccur_matrix, input$output_path, row.names = TRUE)
      
      # 预览矩阵前10行
      output$matrix_preview <- renderTable({
        as.data.frame(head(cooccur_matrix, 10))
      })
      
      # 提供下载
      output$download_matrix <- downloadHandler(
        filename = function() {
          paste0("关键词共现矩阵-", Sys.Date(), ".csv")
        },
        content = function(file) {
          write.csv(cooccur_matrix, file, row.names = TRUE)
        }
      )
      
      output$status <- renderText("关键词共现矩阵生成成功！")
    }, error = function(e) {
      output$status <- renderText(paste("发生错误：", e$message))
    })
  })
}

# 运行 Shiny 应用
shinyApp(ui = ui, server = server)

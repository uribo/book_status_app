box::use(
  shiny,
  rclipboard,
  here[here],
  dplyr[arrange, filter],
  htmltools[css],
  bslib
)

googlesheets4::gs4_auth(token = gargle::secret_read_rds(
  here::here(".secrets/gs4-token.rds"),
  key = "GS_KEY"
))
df_lib_status <-
  googlesheets4::read_sheet("1P6y3ZO0pNNDCo6hzYe6ZbwFnf-JGgiwMzqKXVn0F-eQ")

df_library <-
  readr::read_rds(here::here("data/libraries.rds"))

# [TODO] 国立国会図書館のmarcデータに変更
df_openbd <-
  readr::read_rds("~/Library/Mobile Documents/com~apple~CloudDocs/projects2023/openbd_coverage/data/openbd_summary_df.rds")

ui <- function() {
  page_fluid(
    rclipboardSetup(),
    title = "図書館の蔵書状況を示すバッジを生成するウェブサービス",
    bslib::layout_columns(
      col_widths = c(12),
      card(
        full_screen = FALSE,
        card_header("図書館蔵書状況バッジ生成サービス"),
        layout_sidebar(
          # sidebar -----------------------------------------------------------------
          sidebar = sidebar(selectInput("prefecture",
                                        "都道府県",
                                        choices = c(zipangu::jpnprefs$prefecture_kanji[c(1, 36)])),
                            selectInput("library",
                                        "図書館",
                                        choices = c("徳島県立図書館", "徳島大学附属図書館")),
                            textInput("isbn", "ISBN", value = "4-560-02849-4"), # "978-4-7741-9390-1"
                            radioButtons("format",
                                         "蔵書確認日の表示",
                                         choices = c("あり", "なし"),
                                         selected = "あり",
                                         inline = TRUE),
                            input_task_button("generate", "Generate")
          ),
          # main contents -----------------------------------------------------------
          # cardで2カラムを表現
          bslib::layout_column_wrap(
            width = NULL,
            #style = css(grid_template_columns = "2fr 1fr")
          card(
            full_screen = FALSE,
            card_header(textOutput("book_title")),
            # imageOutput("cover"),
            htmlOutput("cover"),
            ),
          card(
            full_screen = FALSE,
            htmlOutput("badge"),
            verbatimTextOutput("badge_vt"),
            uiOutput("clip")
          ))
        )
      )
    )
 )
}

server <- function(input, output, session) {
  observe({
    x <- input$prefecture

    # Can use character(0) to remove all choices
    if (is.null(x)) {
      x <- character(0)
    } else {
      x <-
        df_library |>
        dplyr::filter(prefecture == {{ x }}) |>
        dplyr::pull(name)
      # x <-
      #   append("すべての図書館", x)
    }
    updateSelectInput(session, "library",
                      choices = x,
                      selected = x[1]
    )
  })

  tgt_book <- reactive({
    req(input$generate)
    if (is.null(input$isbn) || input$isbn == "") {
      tibble::tibble(title = "ISBNが入力されていません",
                     isbn = NA_character_)
    } else {
      d <-
        df_openbd |>
        dplyr::filter(isbn == isbn::normalize(input$isbn))
      if (nrow(d) == 0) {
        tibble::tibble(
          title = "ISBNが見つかりません",
          isbn = NA_character_)
      } else if (nrow(d) > 1) {
        tibble::tibble(
          title = "複数の書籍が見つかりました",
          isbn = NA_character_)
      } else if (nrow(d) == 1) {
        df_openbd |>
          dplyr::filter(isbn == isbn::normalize(input$isbn))
      }
    }
  })
  output$book_title <- renderText({
    req(input$generate)
    tgt_book()$title
  })
  output$cover <- renderText({
    req(input$generate)
    paste0("<img src=",
      glue::glue("https://img.hanmoto.com/bd/img/{isbn}.jpg",
                 isbn = isbn::normalize(input$isbn)),
      " width=40%>")
  })
  # output$isbn <- renderText({
  #   req(input$generate)
  #   isbn::normalize(input$isbn)
  # })

  badge_1 <-
    reactive({
    x <- tgt_book()$isbn
    if (is.na(x)) {
      NULL
    } else {
      d <-
        df_lib_status |>
        dplyr::filter(isbn == x) |>
        dplyr::arrange(lib)
      # if (input$library != "すべての図書館") {
        # d <-
        #   d |>
      # input$library を英語表記にする
        #   dplyr::filter(lib == input$library)
      #}
      if (nrow(d) == 0) {
        d <-
          d |>
          dplyr::mutate(libkey = dplyr::case_when(libkey %in%  c("貸出可", "貸出中", "予約中") ~ "蔵書あり",
                                                  .default = libkey)) |>
          dplyr::mutate(reserveurl = tidyr::replace_na(reserveurl, ""),
                        color = dplyr::case_when(libkey == "貸出可" ~ "blue",
                                                 libkey == "蔵書あり" ~ "green",
                                                 libkey == "館内のみ" ~ "yellowgreen",
                                                 libkey == "貸出中" ~ "orange",
                                                 libkey == "予約中" ~ "orange",
                                                 libkey == "準備中" ~ "yellow",
                                                 libkey == "休館中" ~ "yellow",
                                                 .default = "red")) |>
          dplyr::mutate(libkey = tidyr::replace_na(as.character(libkey),
                                                   paste0("蔵書なし", "(", stringr::str_replace_all(lubridate::today(), "-", "--"), "時点)")),
                        lib = dplyr::case_when(lib == "Univ_Tokushima" ~ "徳島大学附属図書館",
                                               lib == "Tokushima_Tokushima" ~ "徳島市立図書館",
                                               lib == "Tokushima_Pref" ~ "徳島県立図書館"))
        d |>
          glue::glue_data(
            "<a href='https://calil.jp/book/{isbn}'><img src='https://img.shields.io/badge/{lib}-{libkey}-{color}'></a>")
      } else {
        d <-
          d |>
          dplyr::mutate(libkey = dplyr::case_when(libkey %in%  c("貸出可", "貸出中", "予約中") ~ "蔵書あり",
                                                  .default = libkey)) |>
          dplyr::mutate(color = dplyr::case_when(libkey == "貸出可" ~ "blue",
                                                 libkey == "蔵書あり" ~ "green",
                                                 libkey == "館内のみ" ~ "yellowgreen",
                                                 libkey == "貸出中" ~ "orange",
                                                 libkey == "予約中" ~ "orange",
                                                 libkey == "準備中" ~ "yellow",
                                                 libkey == "休館中" ~ "yellow",
                                                 .default = "red")) |>
          dplyr::rowwise() |>
          dplyr::mutate(libkey = tidyr::replace_na(libkey,
                                                   paste0("蔵書なし",
                                                          "(",
                                                          stringr::str_replace_all(lubridate::as_date(request_time),
                                                                                   "-", "--"),
                                                          "時点)"))) |>
          dplyr::ungroup()
        d |>
          glue::glue_data(
          "<a href='https://calil.jp/book/{isbn}'><img src='https://img.shields.io/badge/{lib}-{libkey}-{color}'></a>")

      }
    }
  })

  output$badge <- renderText({
    req(input$generate)
    badge_1()
  })
  output$badge_vt <- renderText({
    req(input$generate)
    badge_1()
  })

  output$clip <- renderUI({
    req(input$generate)
    rclipButton(
      inputId = "clipbtn",
      label = "Copy",
      clipText = badge_1(),
      icon = icon("clipboard"),
      # tooltip = "",
      placement = "top",
      options = list(delay = list(show = 800, hide = 100), trigger = "hover"),
      width = 100
    )
  })

}

shinyApp(ui, server)

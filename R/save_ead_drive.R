#' Função para conversão de horários EAD
#'
#' função para transformar xlsx dos horários em tabelas do EAD
#'
#' @param .file_xltx arquivo contendo horários dos professores em xltx
#' @param .ss_id do arquivo dos horário do google planilhas
#' @export
save_ead_drive <- function(.file_xltx, .ss_id) {
  df_xlsx <-
    readxl::read_excel(
      path = .file_xltx,
      col_names = FALSE
    )

  df_professor <- df_xlsx %>%
    mutate(professor = ifelse(str_detect(...1, "Hor.rio"), lag(...1, 2), NA)) %>%
    fill(professor) %>%
    mutate_at("...1", openxlsx::convertToDateTime) %>%
    drop_na(...1) %>%
    set_names(
      "hora",
      "Segunda-feira",
      "Terça-feira",
      "Quarta-feira",
      "Quinta-feira",
      "Sexta-feira",
      "professor"
    )

  vct_professor <- c(
    "Carol",
    "Carlos2",
    "Carolina",
    "Darlan 2",
    "Eliane",
    "Gilberto",
    "Isanete",
    "Rafael",
    "Rovaris"
  )

  df_ead <- df_professor %>%
    filter(professor %in% vct_professor) %>%
    pivot_longer(c(-hora,-professor)) %>%
    filter(value == "EAD") %>%
    rename("início" = "hora") %>%
    mutate(`término` = `início` + lubridate::hms("00:45:00")) %>%
    mutate_at(c("início", "término"), ~ format(.x, "%H:%M")) %>%
    mutate_at(
      "professor",
      ~ .x %>%
        str_replace("Isanete", "Izanete") %>%
        str_replace("Darlan 2", "Darlan")
    ) %>%
    arrange(`in?cio`) %>%
    pivot_wider(names_from = name, values_from = professor) %>%
    mutate_all(as.character) %>%
    mutate_all(
      ~ .x %>% str_remove_all("c\\(\\\"") %>%
        str_remove_all("\\\"\\)") %>%
        str_replace_all("\\\", \\\"", ", ") %>%
        str_remove_all("NULL")

    ) %>% select(
      "início",
      "término",
      "Segunda-feira",
      "Terça-feira",
      "Quarta-feira",
      "Quinta-feira",
      "Sexta-feira"
    )


  googlesheets4::write_sheet(df_ead, .ss_id, "EAD - Agendamentos")

  list(df_ead, df_professor)
}

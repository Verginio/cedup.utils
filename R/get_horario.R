#' Função para conversão de horários
#'
#' função para transformar xlsx dos horários em tabelas
#'
#' @param .file_xltx arquivo contendo horários dos professores em xltx
#'
#' @export
get_horario <- function(.file_xltx) {
  df_xlsx <-
    readxl::read_excel(
      path = .file_xltx,
      col_names = FALSE
    )

  df_xlsx %>%
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
    ) %>%
    pivot_longer(c(-hora,-professor)) %>%
    mutate_at(
      "professor",
      ~ .x %>%
        str_replace("Isanete", "Izanete") %>%
        str_replace("Darlan 2", "Darlan")
    ) %>%
    mutate(turma = str_remove(value, "/.*")) %>%
    filter(!is.na(parse_number(turma))) %>%
    mutate_at("value", ~ str_remove(.x, ".*/")) %>%
    mutate_at("value", ~ paste0(.x, " (", professor, ")")) %>%
    mutate_at("hora", ~ format(.x, "%H:%M")) %>%
    select(-professor) %>%
    pivot_wider(names_from = name, values_from = value) %>%
    select(
      "hora",
      "turma",
      "Segunda-feira",
      "Terça-feira",
      "Quarta-feira",
      "Quinta-feira",
      "Sexta-feira"
    ) %>%
    arrange(hora)
}

#' Função extrair dados de cadastro
#'
#' função para carregar cadastro dos alunos de xlsx para o R
#'
#' @param .files_xlsx arquivo contendo cadastro dos alunos em xlsx
#'
#' @export
get_cedup_cadastro_alunos <- function(.files_xlsx) {
  map_df(
    .files_xlsx,
    ~ readxl::read_excel(.x, skip = 8) %>%
      janitor::clean_names() %>%
      mutate_at("matricula", as.numeric) %>%
      drop_na(matricula) %>%
      mutate_at("identidade", ~ str_remove_all(.x, "[.]")) %>%
      mutate_at("data_nasc", lubridate::dmy) %>%
      mutate(idade = str_remove(as.character(as.numeric(
        difftime(lubridate::as_date(Sys.time()), data_nasc, units = "days")
      ) / 365), "[.].*")) %>%

      rename(matriz_turma = turma) %>%
      mutate(turma = str_remove(matriz_turma, ".*-I-")) %>%
      mutate(serie = str_remove(turma, "-.*"))
  ) %>%
    distinct(matricula, .keep_all = T) %>%
    select(-inep, -identidade, -cpf, -nome_social) %>%
    mutate_at("no", as.numeric)
}

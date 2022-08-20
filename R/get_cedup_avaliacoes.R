#' Funcao para avaliacoes
#'
#' funcao para recuperar as avaliacoes por tipo e data a partir do diario de classe do professor
#'
#' @param .file_pdf arquivo contendo horarios dos professores em pdf
#'
#' @export
get_cedup_avaliacoes <- function(.file_pdf) {
  df_prev <- pdftools::pdf_text(.file_pdf) %>%
    read_lines() %>%
    enframe(name = NULL) %>%
    mutate(turma_avaliacao = ifelse(str_detect(value,
                                               "SECRETARIA DE ESTADO DA EDUCA"), value, NA)) %>%
    fill(turma_avaliacao) %>%
    mutate_at(
      "turma_avaliacao",
      ~ str_remove(.x, ".*INTEGRAL") %>%
        str_squish() %>% str_sub(1, 4) %>%
        str_remove_all(" ")
    ) %>%
    mutate(
      disciplina_professor = ifelse(str_detect(value,
                                               "Disciplina"), lead(value), NA) %>%
        str_remove(".*DE CLASSE ") %>%
        str_remove("../../.*") %>%
        str_replace_all("  ",
                        "@") %>%
        str_replace_all("@@", "@") %>%
        str_replace_all("@@",
                        "@") %>%
        str_replace_all("@@", "@") %>%
        str_replace_all("@@",
                        "@") %>%
        str_replace_all("@@", "@") %>%
        str_replace_all("@@",
                        "@") %>%
        str_trim() %>%
        str_remove("^@") %>%
        str_remove("@$")
    ) %>%
    fill(disciplina_professor) %>%
    separate(
      disciplina_professor,
      sep = "@",
      into = c("disciplina", "professor")
    ) %>%
    mutate_at(c("disciplina",
                "professor"),
              ~ str_trim(.x) %>%
                str_to_title()) %>%
    mutate(avaliacao_date = lag(value) %>%
             str_remove(" [0-9]{2} .*") %>%
             str_squish()) %>%
    filter(str_detect(value,
                      "Estudante .*cula")) %>%
    mutate(avaliacao_tipo = str_remove(value,
                                       ".*cula") %>%
             str_remove(" NT .*") %>%
             str_squish() %>%
             str_remove("Mi S.*") %>%
             str_remove("Sit Mi.*")

    ) %>%
    select(-value) %>%
    filter(str_detect(avaliacao_date, " [0-9]{2}/[0-9]{2} ")) %>%
    mutate_at("avaliacao_date", str_squish)



  df_prev_tipo <- df_prev %>%
    separate(avaliacao_tipo, letters,
             " ") %>%
    pivot_longer(a:z, names_to = "avaliacao_id",
                 values_to = "avaliacao_tipo") %>%
    drop_na(avaliacao_tipo) %>%
    select(-avaliacao_date) %>%
    filter(avaliacao_tipo != "")


  df_prev_date <- df_prev %>%
    separate(avaliacao_date, letters,
             " ") %>%
    pivot_longer(a:z, names_to = "avaliacao_id",
                 values_to = "avaliacao_date") %>%
    drop_na(avaliacao_date) %>%
    select(-avaliacao_tipo)


  df_avaliacao_tipo_date <- df_prev_tipo %>%
    left_join(df_prev_date,
              by = c("turma_avaliacao", "disciplina", "professor", "avaliacao_id")) %>%
    mutate_at("avaliacao_date", ~paste0(.x, "/2022") %>% lubridate::dmy())
  df_avaliacao_tipo_date
}

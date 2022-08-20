#' Funcao para avaliacoes
#'
#' funcao para recuperar as avaliacoes por tipo e data a partir do diario de classe do professor
#'
#' @param .file_pdf arquivo contendo horarios dos professores em pdf
#'
#' @export
get_cedup_avaliacoes_medias <- function(.file_pdf) {
  df_avaliacoes_medias <- pdftools::pdf_text(.file_pdf) %>%
    read_lines() %>%
    enframe(name = NULL) %>%
    mutate(matricula = str_extract(value, " [0-9]{4}.*") %>% parse_number()) %>%
    mutate(turma_avaliacao = ifelse(str_detect(value, "SECRETARIA DE ESTADO DA EDUCA..O"),
                                    value,
                                    NA)) %>%
    fill(turma_avaliacao) %>%
    mutate_at(
      "turma_avaliacao",
      ~ str_remove(.x, ".*INTEGRAL") %>%
        str_squish() %>%
        str_sub(1, 4) %>%
        str_remove_all(" ")
    ) %>%
    mutate(
      disciplina_professor = ifelse(str_detect(value, "Disciplina"), lead(value), NA) %>%
        str_remove(".*DE CLASSE ") %>%
        str_remove("../../.*") %>%
        str_replace("Literatura", "Literatura@") %>%
        str_replace_all("  ", "@") %>%
        str_replace_all("@@", "@") %>%
        str_replace_all("@@", "@") %>%
        str_replace_all("@@", "@") %>%
        str_replace_all("@@", "@") %>%
        str_replace_all("@@", "@") %>%
        str_trim() %>%
        str_remove("^@") %>%
        str_remove("@$")
    ) %>%
    fill(disciplina_professor) %>%
    separate(disciplina_professor,
             sep = "@",
             into = c("disciplina", "professor")) %>%
    mutate_at(c("disciplina",
                "professor"),
              ~ str_trim(.x) %>% str_to_title()) %>%
    mutate(numero = ifelse(str_detect(lag(value,
                                          2), "Estudante"), 1, NA)) %>%
    mutate_at("numero", ~ ifelse(str_detect(value, "Legenda|CONTE.DO"), -1, .x)) %>%
    fill(numero) %>%
    filter(str_detect(value, "[0-9]{5}")) %>%
    #mutate(notas = str_remove(value, ".*[0-9]{5}") %>% str_remove("[CM].*") %>% str_squish()) %>%

    filter(str_detect(value, ",")) %>%
    mutate(value = str_extract_all(value, ".[0-9],[0-9]{2}")) %>%
    mutate(media = map(value, last) %>% str_replace(",", ".") %>% as.numeric()) %>%

    #left_join(select(df_cadastro, -estudante), by = c("matricula", "turma")) %>%

    #filter(turma_avaliacao == turma) %>%
    #select(-turma_avaliacao) %>%

    mutate_at("turma_avaliacao", as.numeric)  %>%
    filter(media > 0) %>%
    right_join(df_cadastro, by = c("matricula","turma_avaliacao" = "turma"))

  df_avaliacoes_medias
}

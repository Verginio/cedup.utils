#' Função para extraçao de dados
#'
#' função para transformar pdf dos diarios em tabelas
#'
#' @param .file_pdf arquivo contendo diários de classes em pdf
#'
#' @export
get_cedup_faltas <- function(.file_pdf) {
  pdftools::pdf_text(.file_pdf) %>%
    read_lines() %>%
    enframe(name = NULL) %>%
    mutate(matricula = str_extract(value, " [0-9]{4}.*") %>% parse_number()) %>%
    mutate(turma = ifelse(
      str_detect(value, "SECRETARIA DE ESTADO DA EDUCA..O"),
      value,
      NA
    )) %>%
    fill(turma) %>%
    mutate_at(
      "turma",
      ~ str_remove(.x, ".*INTEGRAL") %>% str_squish() %>% str_sub(1, 4) %>% str_remove_all(" ")
    ) %>%

    mutate(
      disciplina_professor = ifelse(str_detect(value, "Disciplina"), lead(value), NA) %>%
        str_remove(".*DE CLASSE ") %>%
        str_remove("../../.*") %>%
        str_replace_all("    ", "@") %>%
        str_replace_all("@@", "@") %>%
        str_replace_all("@@", "@") %>%
        str_replace_all("@@", "@") %>%
        str_replace_all("@@", "@") %>%
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
    mutate_at(c("disciplina", "professor"), ~str_trim(.x) %>% str_to_title()) %>%

    mutate(numero = ifelse(str_detect(lag(value, 2), "Estudante"), 1, NA)) %>%
    mutate_at("numero", ~ ifelse(str_detect(value, "Legenda|CONTE.DO"),-1, .x)) %>%
    fill(numero) %>%
    filter(numero == 1) %>%
    fill(matricula) %>%
    mutate(txt_presenca = str_remove(value, ".*[0-9]{4} ")) %>%
    group_by(matricula, disciplina, professor, turma) %>%
    summarise(
      n = n(),
      presenca = str_count(txt_presenca, "C"),
      f1 = str_count(txt_presenca, "1F"),
      f2 = str_count(txt_presenca, "2F"),
      f3 = str_count(txt_presenca, "3F"),
      f4 = str_count(txt_presenca, "4F"),

      j1 = str_count(txt_presenca, "1J"),
      j2 = str_count(txt_presenca, "2J"),
      j3 = str_count(txt_presenca, "3J"),
      j4 = str_count(txt_presenca, "4J")
    ) %>%
    #mutate_at("matricula", as.factor) %>%
    #mutate(faltas = f1 + 2 * f2 + 3 * f3 + 4 * f4) %>%
    mutate(injustificadas = f1 + 2 * f2 + 3 * f3 + 4 * f4) %>%
    mutate(justificadas = j1 + 2*j2 + 3*j3 + 4*j4) %>%
    mutate(faltas = justificadas + injustificadas) %>%
    ungroup() %>%
    rename(turma_falta = turma)
}

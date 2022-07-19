#' Funcao para extracao de dados
#'
#' funcao para transformar pdf dos diarios em tabelas
#'
#' @param .file_pdf arquivo contendo diarios de classes em pdf
#'
#' @export
get_cedup_faltas <- function(.file_pdf) {
  pdf_faltas <- pdftools::pdf_text(.file_pdf) %>%
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
    mutate_at(c("disciplina", "professor"),
              ~ str_trim(.x) %>% str_to_title()) %>%

    mutate(numero = ifelse(str_detect(lag(value, 2), "Estudante"), 1, NA)) %>%
    mutate_at("numero", ~ ifelse(str_detect(value, "Legenda|CONTE.DO"), -1, .x)) %>%
    fill(numero)

  df_meses_aulas <- pdf_faltas %>%
    select(-matricula,-numero) %>%
    filter(str_detect(value, "Estudante")) %>%
    mutate_at("value",
              ~ str_remove(.x, ".*S") %>% str_remove("C.*") %>% str_squish()) %>%
    separate(value, letters, " ") %>%
    pivot_longer(a:z) %>%
    drop_na(value) %>%
    rename(mes = value)

  df_dias_aulas <- pdf_faltas %>%
    select(-matricula,-numero) %>%
    filter(str_detect(lead(value), "Estudante")) %>%
    mutate_at("value", ~ str_remove(.x, ".*/..") %>% str_squish()) %>%
    separate(value, letters, " ") %>%
    pivot_longer(a:z) %>%
    drop_na(value) %>%
    rename(dia = value)

  df_numero_aulas <- pdf_faltas %>%
    select(-matricula,-numero) %>%
    filter(str_detect(lag(value, 1), "Estudante")) %>%
    mutate_at("value", str_squish) %>%
    separate(value, letters, " ") %>%
    pivot_longer(a:z) %>%
    mutate_at("value", as.numeric) %>%
    drop_na(value) %>%
    rename(numero_aulas = value)

  df_presenca_aulas <- pdf_faltas  %>%
    filter(numero == 1) %>%
    fill(matricula) %>%
    mutate(txt_presenca = str_remove(value, ".*[0-9]{4} ")) %>%
    group_by(matricula, disciplina, professor, turma) %>%
    mutate(
      txt_pre = str_extract(txt_presenca, "[CM].*") %>%
        str_remove("......$") %>%
        str_remove("^.") %>%
        str_squish()
    ) %>%
    select(txt_pre) %>%
    separate(txt_pre, letters, " ") %>%
    pivot_longer(a:z) %>%
    drop_na(value) %>%
    rename(status = value)

  df_presenca_aulas_merge <- df_presenca_aulas %>%
    left_join(df_numero_aulas,
              by = c("disciplina", "professor", "turma", "name")) %>%
    left_join(df_dias_aulas,
              by = c("disciplina", "professor", "turma", "name")) %>%
    left_join(df_meses_aulas,
              by = c("disciplina", "professor", "turma", "name")) %>%
    mutate(data = lubridate::dmy(paste(dia, mes, "2022"))) %>%
    select(-name,-dia,-mes) %>%
    drop_na(numero_aulas) %>%
    mutate(presenca = ifelse(str_detect(status, "C"), numero_aulas, 0)) %>%
    mutate(
      injustificadas = case_when(
        str_detect(status, "1F") ~ 1,
        str_detect(status, "2F") ~ 2,
        str_detect(status, "3F") ~ 3,
        str_detect(status, "4F") ~ 4,
        str_detect(status, "5F") ~ 5,
        T ~ 0
      )
    ) %>%
    mutate(
      justificadas = case_when(
        str_detect(status, "1J") ~ 1,
        str_detect(status, "2J") ~ 2,
        str_detect(status, "3J") ~ 3,
        str_detect(status, "4J") ~ 4,
        str_detect(status, "5J") ~ 5,
        T ~ 0
      )
    )

  df_presenca_aulas_merge
}

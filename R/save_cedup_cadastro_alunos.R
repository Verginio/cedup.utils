#' Função para salvar cadastros na nuvem
#'
#' função para carregar cadastro dos alunos de xlsx para o R
#'
#' @param .df_alunos_xlsx arquivo contendo cadastro dos alunos em xlsx
#' @param .ss_id do arquivo dos horário do google planilhas
#' @export
save_cedup_cadastro_alunos <- function(.df_alunos_xlsx, .ss_id) {
  df_cadastro <- .df_alunos_xlsx %>%
    #filter(serie %in% 2:3) %>%
    filter(str_detect(turma, ".1.")) %>%
    mutate(email = paste0(matricula, "@estudante.sed.sc.gov.br")) %>%
    #select(estudante, matricula, email, turma) %>%
    arrange(estudante) %>%
    mutate_at("estudante", str_to_title) %>%
    select(
      no,
      matricula,
      estudante,
      turma,
      mae_1a_fil,
      pai_2a_fil,
      endereco,
      bairro,
      mun_resid,
      data_nasc,
      idade,
      turma,
      serie
    )


  googlesheets4::write_sheet(df_cadastro, .ss_id, sheet = "df_cadastro")
}

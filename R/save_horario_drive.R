#' Função para conversão de horários das aulas
#'
#' função para salvar horários das aulas no google drive
#'
#' @param .df data.frame contendo horários dos professores em xltx
#' @param .turma vetor contendo turmas que serão salvas
#' @param .ss_id do arquivo dos horário do google planilhas
#' @export
save_horario_drive <- function(.df, .turma, .ss_id) {
  df_turma = .df %>%
    filter(turma == .turma) %>%
    select(-turma)

  #Sys.sleep(1)

  googlesheets4::write_sheet(df_turma, .ss_id, .turma)
}

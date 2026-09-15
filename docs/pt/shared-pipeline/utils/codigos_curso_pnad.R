# ==============================================================================
# codigos_curso_pnad.R
#
# Codigos de "curso que frequenta" (V0603 ate 2006; V6003 de 2007 em diante)
# e de "curso mais elevado que frequentou anteriormente" (V0607 / V6007) na
# PNAD Anual, por regime de dicionario. Fonte: dicionarios oficiais do IBGE em
# 5-data/pnad_anual_raw/<ano>/temp_layout/<ano>/Dicion*rio/*pessoas*.xls
# (2001, 2005, 2007) e LAYOUT/sas/SAS_PES.TXT (1992; V0603 com 1 caractere).
#
#   1992-1999 (1 digito):  5 = superior, 8 = pre-vestibular, 9 = mestrado/dout.
#                          (idade media empirica: cod. 8 = 21,6; cod. 9 = 34,8)
#   2001-2006 (2 digitos): 5 = superior, 8 = PRE-ESCOLAR, 9 = PRE-VESTIBULAR,
#                          10 = mestrado/dout.
#                          (idade media empirica: 8 = 5,3; 9 = 22,0; 10 = 35,1)
#                          <-- o 035 tratava 9 como mestrado ate 2026-09-15
#                          (plano de correcao da auditoria, WP2); efeito
#                          +1,3 a +1,8 pp em ens_sup 18-24 nessa janela, ate
#                          +4,8 pp no D10 (2001). Ver D22 em
#                          docs/harmonization_decisions.md.
#   2007+:                 5 = superior (graduacao), 9 = maternal/jardim,
#                          10 = pre-vestibular, 11 = mestrado/dout.
#
# Curso anterior: 1992-2006: 6 = superior, 7 = mestrado/dout.;
#                 2007+:     8 = superior, 9 = mestrado/dout. (ja estava certo).
#
# Uso: source() este arquivo e chamar as funcoes vetorizadas com `ano` e o
# codigo ja convertido para inteiro (as.integer normaliza "05" e "5").
# Consumidores: 035_Splice_Microdados.R, 2026-05_PNADcIBGE/042_*.R e 043_*.R.
# Teste: validation/902_Verificar_Codigos_V0603.R.
# ==============================================================================

regime_pnad <- function(ano) {
  dplyr::case_when(ano <= 1999L ~ "90s", ano <= 2006L ~ "00s", TRUE ~ "07+")
}

# Frequenta ensino superior agora (graduacao ou pos), dado o codigo do curso.
cur_superior <- function(ano, cur) {
  r <- regime_pnad(ano)
  !is.na(cur) & ((r == "90s" & cur %in% c(5L, 9L)) |
                 (r == "00s" & cur %in% c(5L, 10L)) |
                 (r == "07+" & cur %in% c(5L, 11L)))
}

# Frequenta pre-vestibular agora.
cur_pre_vestibular <- function(ano, cur) {
  r <- regime_pnad(ano)
  !is.na(cur) & ((r == "90s" & cur == 8L) |
                 (r == "00s" & cur == 9L) |
                 (r == "07+" & cur == 10L))
}

# Curso anterior mais elevado foi superior (graduacao ou pos).
ant_superior <- function(ano, ant) {
  !is.na(ant) & ((ano < 2007L & ant %in% c(6L, 7L)) |
                 (ano >= 2007L & ant %in% c(8L, 9L)))
}

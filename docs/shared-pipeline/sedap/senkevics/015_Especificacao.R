# ==============================================================================
# ODA: portado de 4-DA-Code/2026-08_Replicacao_Senkevics2024/ em 2026-09-20; saidas em
#      data-raw/sedap/senkevics/coorte_<ano>/. Exige SEDAP_TOKEN (Tier B).
# 015_Especificacao.R — parametros, config de coorte e helpers de SQL
# ==============================================================================
# A especificacao veio de 5-data/Senkevicks-etal-2024/ (codigo SAS/Stata/R dos
# autores). Cada constante traz a referencia da linha de origem. Nada inferido.
#
# A COORTE E PARAMETRO. Defina antes de rodar qualquer script da pasta:
#   Sys.setenv(COORTE_ANO = "2019")   # ou 2012, a coorte do artigo
# Padrao: 2012.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Especificacao dos autores (invariante entre coortes)
# ------------------------------------------------------------------------------

# Pontos medios da faixa de renda, em salarios minimos.
# Fonte: 2. Codes in Stata/1.abrir_e_tratar_bases_em_stata.do, linhas 34-37
#   recode iequ_questao_06 ("A"=0) ("B"=1) ("C"=1.25) ... ("Q"=20), gen(pm_sm)
# NOTA: "B" (ate 1 SM) recebe 1 e nao 0,5, e "Q" (mais de 20 SM) recebe 20 sem
# imputacao de cauda. E discutivel, mas e o que os autores fizeram — replicar
# exige reproduzir, nao corrigir. Registrar como limitacao ao citar.
PM_SM <- c(
  A = 0.00, B = 1.00, C = 1.25, D = 1.75, E = 2.25, F = 2.75,
  G = 3.50, H = 4.50, I = 5.50, J = 6.50, K = 7.50, L = 8.50,
  M = 9.50, N = 11.00, O = 13.50, P = 17.50, Q = 20.00
)

# Etapas de ensino da serie final do EM.
# Fonte: 1. Codes in SAS/Code_SAS.sas, linhas 40-47
ETAPAS_SERIE_FINAL <- c(27, 28, 29, 32, 33, 34, 37, 38)

# Filtros de idade. O SAS filtra 15-29 no estagio do censo; o Stata reduz a
# 16-22 na amostra analitica final (2.cruzar_..._analise.do, ~linha 196).
IDADE_MIN <- 16
IDADE_MAX <- 22

# Setor da IES. Fonte: 1.abrir_e_tratar_bases_em_stata.do, linha 82
#   recode tp_categoria_administrativa (1/3=1) (4/9=2) -> 1 Publica, 2 Privada
CATEG_PUBLICA <- 1:3
CATEG_PRIVADA <- 4:9

# Desempenho: media das 4 objetivas, sem redacao.
# Fonte: 1.abrir_e_tratar_bases_em_stata.do, linha 30
AREAS <- c("CN", "CH", "LC", "MT")

# Deflator. Fonte: 1_DataPrep_and_Model.R, linhas 31-33 — base maio de cada
# ano -> 03/2023. ATENCAO: o TEXTO do artigo (p. 864) diz "julho de 2023"; o
# CODIGO diz "03/2023". Divergencia interna do pacote. Seguimos o codigo, e
# mantemos o mesmo alvo em todas as coortes para que sejam comparaveis em
# termos reais.
INPC_DATA_ALVO <- "03/2023"

# ------------------------------------------------------------------------------
# 2. Deriva de schema do ENEM, edicao a edicao
# ------------------------------------------------------------------------------
# Extraido de CATALOGO_VARIAVEIS_ENEM_2009_2025.xlsx (abas por ano) em
# 2026-08-08. Os autores trabalharam com nomes ja harmonizados pelo SEDAP
# (iequ_questao_05/06, iere_vl_nota_*); pela API vem o nome bruto de cada ano.
# NENHUM nome pode ser reaproveitado entre edicoes sem conferir aqui.
SCHEMA_ENEM <- list(
  "2012" = list(nota = "NU_NT_%s", presenca = "IN_PRESENCA_%s", renda = "Q003", hab = "Q004"),
  "2013" = list(nota = "NOTA_%s", presenca = "IN_PRESENCA_%s", renda = "Q003", hab = "Q004"),
  "2014" = list(nota = "NOTA_%s", presenca = "IN_PRESENCA_%s", renda = "Q003", hab = "Q004"),
  "2015" = list(nota = "NU_NOTA_%s", presenca = "TP_PRESENCA_%s", renda = "Q006", hab = "Q005"),
  "2016" = list(nota = "NU_NOTA_%s", presenca = "TP_PRESENCA_%s", renda = "Q006", hab = "Q005"),
  "2017" = list(nota = "NU_NOTA_%s", presenca = "TP_PRESENCA_%s", renda = "Q006", hab = "Q005"),
  "2018" = list(nota = "NU_NOTA_%s", presenca = "TP_PRESENCA_%s", renda = "Q006", hab = "Q005"),
  "2019" = list(nota = "NU_NOTA_%s", presenca = "TP_PRESENCA_%s", renda = "Q006", hab = "Q005"),
  "2020" = list(nota = "NU_NOTA_%s", presenca = "TP_PRESENCA_%s", renda = "Q006", hab = "Q005"),
  "2021" = list(nota = "NU_NOTA_%s", presenca = "TP_PRESENCA_%s", renda = "Q006", hab = "Q005"),
  "2022" = list(nota = "NU_NOTA_%s", presenca = "TP_PRESENCA_%s", renda = "Q006", hab = "Q005"),
  "2023" = list(nota = "NU_NOTA_%s", presenca = "TP_PRESENCA_%s", renda = "Q006", hab = "Q005")
)

# Base em R$ da faixa "B" (ate 1 salario minimo) DECLARADA NO QUESTIONARIO de
# cada edicao — lida do catalogo, nao da tabela de salario minimo legal.
#
# 🚨 POR QUE NAO USAR O SALARIO MINIMO LEGAL, como os autores fazem:
# em 2022 o questionario do ENEM manteve as faixas de 2021 (base R$ 1.100),
# enquanto o minimo legal era R$ 1.212. Usar 1.212 inflaria a renda declarada
# de 2022 em ~10%. Para 2012-2016 as duas tabelas coincidem (622, 678, 724,
# 788, 880), entao esta escolha e retrocompativel com a replicacao da coorte
# de 2012 — nao muda nenhum numero ja produzido.
BASE_FAIXA_RENDA <- c(
  "2012" = 622, "2013" = 678, "2014" = 724, "2015" = 788,
  "2016" = 880, "2017" = 937, "2018" = 954, "2019" = 998,
  "2020" = 1045, "2021" = 1100, "2022" = 1100, "2023" = 1320
)

# ------------------------------------------------------------------------------
# 3. Configuracao da coorte
# ------------------------------------------------------------------------------
# Desenho do artigo: concluintes do EM no ano C, acompanhados no ENEM de C a
# C+4 e no Censo Superior de C+1 a C+5 (primeira matricula em ate cinco anos).
COORTE_ANO <- as.integer(Sys.getenv("COORTE_ANO", "2012"))
ANOS_ENEM <- COORTE_ANO:(COORTE_ANO + 4)
ANOS_CENSUP <- (COORTE_ANO + 1):(COORTE_ANO + 5)

stopifnot(all(as.character(ANOS_ENEM) %in% names(SCHEMA_ENEM)))

DIR_DADOS <- here::here(
  "data-raw", "sedap", "senkevics",
  sprintf("coorte_%d", COORTE_ANO)
)
dir.create(DIR_DADOS, showWarnings = FALSE, recursive = TRUE)

message(sprintf(
  "[coorte %d] ENEM %d-%d | Censo Superior %d-%d | saidas em dados/coorte_%d/",
  COORTE_ANO, min(ANOS_ENEM), max(ANOS_ENEM),
  min(ANOS_CENSUP), max(ANOS_CENSUP), COORTE_ANO
))

# ------------------------------------------------------------------------------
# 4. Helpers de SQL (compartilhados por 020 e 030)
# ------------------------------------------------------------------------------
# Armadilhas de tipagem ja pagas, todas verificadas em 2026-08-07/08:
#   - TP_ETAPA_ENSINO e NU_IDADE sao STRING em BAS_MATRICULA -> aspas e CAST
#   - IN_PRESENCA_*/TP_PRESENCA_* sao STRING em TODAS as edicoes -> comparar '1'
#   - CPF_MASC nao pode ser referenciado fora de agregacao, nem em IS NOT NULL
#   - subconsulta aninhada com JOIN e rejeitada -> agregar direto

TABELA_COORTE <- sprintf("raw.BAS_MATRICULA_%d", COORTE_ANO)
ETAPAS_SQL <- paste(sprintf("'%d'", ETAPAS_SERIE_FINAL), collapse = ",")

filtro_coorte <- function(a = "b") {
  sprintf(
    "%s.TP_ETAPA_ENSINO IN (%s) AND CAST(%s.NU_IDADE AS INT64) >= %d AND CAST(%s.NU_IDADE AS INT64) <= %d",
    a, ETAPAS_SQL, a, IDADE_MIN, a, IDADE_MAX
  )
}

expr_media <- function(ano, a = "e") {
  cols <- sprintf(SCHEMA_ENEM[[as.character(ano)]]$nota, AREAS)
  sprintf("(%s)/4.0", paste(sprintf("%s.%s", a, cols), collapse = " + "))
}

expr_presenca <- function(ano, a = "e") {
  cols <- sprintf(SCHEMA_ENEM[[as.character(ano)]]$presenca, AREAS)
  paste(sprintf("%s.%s = '1'", a, cols), collapse = " AND ")
}

# Deduplicacao entre edicoes: cada individuo entra UMA vez, pela ultima edicao
# em que aparece — a regra de fallback dos autores (`ultimoenem`). A alternativa
# (MAX do ano agregando por CPF) e impossivel: agrupar por individuo cria grupos
# de tamanho 1, exatamente o que a privacidade diferencial suprime.
# A ausencia e detectada por NU_ANO, nao por CPF_MASC (unidade de privacidade).
join_ultima_edicao <- function(ano) {
  post <- ANOS_ENEM[ANOS_ENEM > ano]
  if (!length(post)) {
    return(list(join = "", where = ""))
  }
  list(
    join = paste(sprintf(
      "LEFT JOIN raw.ENEM_%d_SEDAP n%d ON b.CPF_MASC = n%d.CPF_MASC",
      post, post, post
    ), collapse = "\n"),
    where = paste(sprintf("  AND n%d.NU_ANO IS NULL", post), collapse = "\n")
  )
}

# Renda familiar per capita ja deflacionada, montada em SQL.
# base_real = base da faixa B do questionario, deflacionada pelo INPC.
expr_rfpc <- function(ano, base_real, a = "e") {
  sch <- SCHEMA_ENEM[[as.character(ano)]]
  pm <- paste(sprintf("WHEN '%s' THEN %s", names(PM_SM), format(PM_SM, trim = TRUE)), collapse = " ")
  sprintf(
    "(%.6f * (CASE %s.%s %s END) / CAST(%s.%s AS INT64))",
    base_real, a, sch$renda, pm, a, sch$hab
  )
}

# Valor NULL (questionario em branco, nota ausente) sai como 'NA' e e
# descartado localmente pelo consumidor. Ate 2026-09-14 nao havia o primeiro
# WHEN e o NULL caia no ELSE '10D': quem nao declarou renda era codificado
# como 10o decil. Na edicao 2023 da coorte 2019, 83% da celula
# 10D x desempenho 1 era isso (170_Verificar_Figura_SEDAP.R, V4). Desvio D7.
expr_decil <- function(valor, cortes_vec) {
  ramos <- paste(sprintf("WHEN %s < %.6f THEN '%dD'", valor, cortes_vec[2:10], 1:9), collapse = " ")
  sprintf("CASE WHEN %s IS NULL THEN 'NA' %s ELSE '10D' END", valor, ramos)
}

# Destino: o menor ano de ingresso no CENSUP manda.
expr_destino <- function() {
  ramos <- paste(sprintf(
    "WHEN c%d.TP_CATEGORIA_ADMINISTRATIVA IS NOT NULL THEN CAST(c%d.TP_CATEGORIA_ADMINISTRATIVA AS INT64)",
    ANOS_CENSUP, ANOS_CENSUP
  ), collapse = " ")
  categ <- sprintf("CASE %s ELSE NULL END", ramos)
  sprintf(
    "CASE WHEN (%s) BETWEEN %d AND %d THEN 'Public'
          WHEN (%s) BETWEEN %d AND %d THEN 'Private'
          ELSE 'No Access' END",
    categ, min(CATEG_PUBLICA), max(CATEG_PUBLICA),
    categ, min(CATEG_PRIVADA), max(CATEG_PRIVADA)
  )
}

join_censup <- paste(sprintf(
  "LEFT JOIN raw.SUP_ALUNO_%d c%d ON b.CPF_MASC = c%d.CPF_MASC",
  ANOS_CENSUP, ANOS_CENSUP, ANOS_CENSUP
), collapse = "\n")

# ------------------------------------------------------------------------------
# 5. Modelo
# ------------------------------------------------------------------------------
# Especificacao da figura publicada (1_DataPrep_and_Model.R, linha 86):
#   choice ~ income_D*performance + as.factor(ano_enem) + sexo + cor +
#            poly(idade,2) + depadm + rural + uf
# NAO REPLICAVEL por esta API: a tabela de contingencia teria ordem de 10^8
# celulas contra um GROUP BY de ~5 colunas e supressao por DP. Usamos a
# especificacao reduzida dos proprios autores (linha 168), que e saturada.
FORMULA <- choice ~ income_D * performance

# ==============================================================================
# DESVIOS ASSUMIDOS (declarar em toda legenda de figura)
# ==============================================================================
# D1. Coorte: matriculados na serie final do EM, nao concluintes aprovados —
#     BAS_SITUACAO (IN_CONCLUINTE, TP_SITUACAO) nao esta no projeto.
# D2. Especificacao reduzida, sem os oito controles.
# D3. Edicao do ENEM: so a regra de fallback dos autores (ultima edicao feita).
#     A regra principal (edicao imediatamente anterior ao ingresso) exige
#     agregacao por individuo, proibida sob privacidade diferencial.
# D4. Sem o filtro_aleatorio de individualizacao de vinculos multiplos.
# D5. Contagens sujeitas a ruido de privacidade diferencial (epsilon = 1).
# D6. Renda ancorada na base do questionario, nao no salario minimo legal
#     (ver BASE_FAIXA_RENDA). Diverge dos autores apenas em 2022.
# D7. Renda ou nota ausente e EXCLUIDA da matriz (expr_decil emite 'NA'; o 030
#     descarta e reporta a contagem). E o que o Stata dos autores faz por
#     construcao com missings. Entre 2026-08-07 e 2026-09-14 esses casos
#     entravam como 10o decil de renda (ELSE do CASE): 360 na coorte 2012,
#     749 em 2014, 2.071 em 2017, 2.119 em 2019.
# ==============================================================================

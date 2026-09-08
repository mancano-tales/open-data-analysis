# ==============================================================================
# 020_Extrair_Nicho_2020_2024.R
#
# Estende para 2020-2024 a serie de matriculas por setor x via de acesso x
# turno/modalidade que hoje alimenta @fig-matriculas-turno-via-acesso e para
# em 2019 (`nicho_agregado_2010_2019.csv`, gerado por
# 4-DA-Code/2026-07_Microdados_Turnos/011_Extrair_Nicho_Microdados_Completo.R).
#
# A serie para em 2019 porque o INEP deixou de publicar o microdado de ALUNO
# do Censo da Educacao Superior a partir de 2020. Duas fontes distintas
# cobrem o vao, e a diferenca entre elas NAO e cosmetica:
#
#   2024        SUP_ALUNO_2024 via SEDAP+ (nivel aluno). Cruzamento
#               setor x via x turno/modalidade EXATO, igual ao de 2010-2019.
#
#   2020-2023   SUP_ALUNO_2020 a _2023 via SEDAP+, tambem nivel aluno.
#
# CORRECAO DE UM ERRO ANTERIOR, registrada porque o vicio que a causou e
# recorrente: a primeira versao deste script usava os arquivos publicos de
# CURSO para 2020-2023 e afirmava, no cabecalho, que o SUP_ALUNO desses anos
# "nao esta disponivel nem no acervo publico nem no projeto SEDAP+". A segunda
# metade da afirmacao era falsa. Ela veio de ler a lista de bases deferidas
# — que mostra SUP_ALUNO de 2009 a 2019 e depois pula para 2024 — em vez de
# perguntar a API. As quatro tabelas existem e respondem:
#   SUP_ALUNO_2020  12.565.378 linhas
#   SUP_ALUNO_2021  13.204.605
#   SUP_ALUNO_2022  14.398.981
#   SUP_ALUNO_2023  15.196.578
# Nao confie na lista; teste a tabela.
#
# Com isso a serie fica homogenea em nivel de aluno de 2010 a 2024, e o corte
# diurno/noturno — que o arquivo de curso nao permitia cruzar com a via de
# acesso — volta a existir em todos os anos.
#
# Pre-requisito para a parte do SEDAP+: Sys.setenv(SEDAP_TOKEN = "...").
# Ver 010_SEDAP_Cliente.R para o protocolo da API e suas limitacoes.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(data.table)
  library(here)
})

source(here::here("4-DA-Code", "2026-08_SEDAP", "010_SEDAP_Cliente.R"))

OUT_DIR <- here::here("data-raw", "INEP", "derived")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# Helpers de classificacao — os mesmos criterios do script 011/028, replicados
# aqui de proposito em vez de importados: as duas fontes tem esquemas
# diferentes e a traducao para as categorias da figura precisa ser explicita
# em cada uma.
# ------------------------------------------------------------------------------
classificar_setor <- function(cat_admin) {
  ifelse(cat_admin %in% c(1, 2, 3), "Publico", "Privado")
}

classificar_turno <- function(modalidade, turno) {
  dplyr::case_when(
    modalidade == 2 ~ "Distance",
    turno == 3 ~ "Evening",
    turno %in% c(1, 2, 4) ~ "Day",
    TRUE ~ NA_character_
  )
}

# ==============================================================================
# PARTE A — 2024, nivel aluno, via SEDAP+
#
# DESENHO: duas consultas, uma por setor, agrupadas na granularidade que a
# figura usa — e nao mais fina. Esta e a decisao que faz o metodo funcionar.
#
# A primeira versao deste script agrupava pelas colunas cruas (categoria
# administrativa com seus 6 valores, mais quatro flags) e montava as vias por
# aritmetica entre cinco consultas. Dois problemas apareceram:
#
#   1. Granularidade fina demais gera celulas pequenas, e o SEDAP+ SUPRIME as
#      que tem poucos individuos. A consulta de 5 colunas produziu 116
#      celulas e devolveu 109: row_loss = 7/116 = 6,03%, ~113 mil matriculas
#      perdidas. Nao e ruido — repetindo com epsilon 1, 10 e 100 o total
#      variou 78 matriculas em 10 milhoes (0,0008%). E supressao
#      deterministica de celula pequena.
#
#   2. Subtrair o resultado de uma consulta do de outra mistura populacoes
#      com supressoes diferentes, porque cada recorte gera um conjunto de
#      celulas proprio. Aritmetica entre consultas e invalida aqui.
#
# A correcao e agrupar grosso. A figura precisa de setor x turno x via: umas
# 15 celulas por setor, todas com dezenas de milhares de alunos. Nessa
# granularidade row_loss = 0 — nenhuma celula suprimida, contagem integral.
#
# Para colapsar o setor sem CASE (que o parser recusa no GROUP BY), usa-se
# IN (...) no WHERE, que funciona. O WHERE aceita IN, <>, < e cadeias de AND;
# NAO aceita OR nem IS NULL.
#
# As categorias derivadas sao montadas localmente sobre o que voltou, nunca
# por diferenca entre consultas.
# ==============================================================================
SETOR_PUBLICO <- "1,2,3"
SETOR_PRIVADO <- "4,5,7"

#' Consulta a SUP_ALUNO_2024 restrita a matriculas ativas.
#'
#' @param grupo       colunas do GROUP BY, cruas e separadas por virgula. O
#'                    parser recusa expressao, CASE, alias e posicional.
#' @param where_extra condicao adicional. Aceita IN (...), <>, < e cadeia de
#'                    AND; nao aceita OR nem IS NULL.
#' Lista para clausula IN, no tipo que o ano usa.
#'
#' TP_CATEGORIA_ADMINISTRATIVA e INT64 nos anos recentes e STRING nos antigos.
#' O SEDAP+ nao coage: `IN (1,2,3)` contra coluna STRING devolve 422 com
#' "No matching signature for operator IN for argument types STRING and
#' {INT64}". Como nao ha como consultar o tipo antes (information_schema
#' exige agregacao), a estrategia e tentar numerico e cair para texto.
lista_in <- function(vals, texto = FALSE) {
  if (texto) paste0("'", vals, "'", collapse = ",") else paste(vals, collapse = ",")
}

q_sedap <- function(ano, grupo, where_extra = NULL) {
  w <- "IN_MATRICULA = 1"
  if (!is.null(where_extra)) w <- paste(w, "AND", where_extra)
  sql <- sprintf(
    "SELECT %s, COUNT(*) AS QT FROM raw.SUP_ALUNO_%d WHERE %s GROUP BY %s",
    grupo, ano, w, grupo
  )
  res <- sedap_query(sql)
  if (is.null(res)) stop("Consulta ao SEDAP+ falhou; ver mensagem acima.", call. = FALSE)

  # Guarda-chuva contra o modo de falha silencioso desta API: se o motor
  # suprimir celulas por serem pequenas, o total sai subestimado sem nenhum
  # aviso no resultado. row_loss > 0 significa que o agrupamento esta fino
  # demais para a granularidade que a figura precisa.
  perda <- attr(res, "dp_stats")$differential_privacy$row_loss
  if (!is.null(perda) && !is.na(perda) && perda > 0) {
    warning(
      sprintf(
        "SEDAP+ suprimiu %.2f%% das celulas neste agrupamento (%s). Agregue mais grosso.",
        100 * perda, grupo
      ),
      call. = FALSE
    )
  }
  res
}

extrair_ano <- function(ano) {
  # Tenta a lista IN numerica; se o ano guardar a categoria como STRING,
  # refaz com aspas. Uma tentativa perdida custa uma consulta, nao a rodada.
  q2 <- function(grupo, setor_vals) {
    tryCatch(
      q_sedap(
        ano, grupo,
        sprintf("TP_CATEGORIA_ADMINISTRATIVA IN (%s)", lista_in(setor_vals))
      ),
      error = function(e) {
        message("  (categoria como texto neste ano; refazendo)")
        q_sedap(
          ano, grupo,
          sprintf("TP_CATEGORIA_ADMINISTRATIVA IN (%s)", lista_in(setor_vals, texto = TRUE))
        )
      }
    )
  }

  # --- Publico: turno x modalidade x reserva de vaga -------------------------
  # IN_RESERVA_ENSINO_PUBLICO volta com tres valores. NULL significa que o
  # bloco de reserva nao foi preenchido, isto e, o aluno NAO entrou por vaga
  # reservada; 0 e 1 significam que entrou (o valor distingue apenas se a
  # reserva foi por escola publica). Por isso o colapso e "bloco preenchido =
  # cotista", nao "= 1". Usar apenas o valor 1 subestimaria as cotas ao
  # excluir quem entrou por renda, criterio etnico-racial ou deficiencia.
  message("SEDAP+ ", ano, ": rede publica...")
  pub <- q2(
    "TP_TURNO, TP_MODALIDADE_ENSINO, IN_RESERVA_ENSINO_PUBLICO",
    c(1, 2, 3)
  ) |>
    as.data.frame() |>
    mutate(
      setor = "Publico",
      via = ifelse(is.na(IN_RESERVA_ENSINO_PUBLICO), "Open", "Cotas")
    )

  # --- Privado: turno x modalidade x FIES x ProUni ---------------------------
  #
  # NAO usar IN_FINANCIAMENTO_ESTUDANTIL para inferir ProUni por residuo. Uma
  # versao anterior fez isso — classificou como ProUni todo aluno financiado
  # sem FIES — e produziu 2,85 milhoes de "bolsistas ProUni", SEIS VEZES o
  # tamanho do programa inteiro (453 mil em 2024). A flag e ampla: apanha
  # desconto institucional, financiamento da propria IES e bolsas estaduais e
  # municipais, que em EaD sao concedidos em massa. O erro teria entrado na
  # tese como "o ProUni explodiu no EaD", que e falso e inverte o argumento
  # que a figura sustenta.
  #
  # A unica leitura confiavel vem das flags nominais do programa. Integral e
  # parcial sao disjuntas e somadas aqui, porque a figura trata ProUni como
  # uma via so. Conferencia contra o dicionario: esta consulta devolve ProUni
  # 427.242 e FIES 153.658, contra 453.069 e 157.955 no universo inteiro da
  # tabela — a diferenca e rede publica e nao-matriculados, que o recorte
  # exclui de proposito.
  message("SEDAP+ ", ano, ": rede privada...")
  priv <- q2(paste(
    "TP_TURNO, TP_MODALIDADE_ENSINO, IN_FIN_REEMB_FIES,",
    "IN_FIN_NAOREEMB_PROUNI_INTEGR, IN_FIN_NAOREEMB_PROUNI_PARCIAL"
  ), c(4, 5, 7)) |>
    as.data.frame() |>
    mutate(
      setor = "Privado",
      .prouni = (!is.na(IN_FIN_NAOREEMB_PROUNI_INTEGR) & IN_FIN_NAOREEMB_PROUNI_INTEGR == 1) |
        (!is.na(IN_FIN_NAOREEMB_PROUNI_PARCIAL) & IN_FIN_NAOREEMB_PROUNI_PARCIAL == 1),
      .fies = !is.na(IN_FIN_REEMB_FIES) & IN_FIN_REEMB_FIES == 1,
      # Precedencia ProUni > FIES: quem tem bolsa parcial e financia o resto
      # conta como bolsista, coerente com 2010-2019.
      via = case_when(.prouni ~ "ProUni", .fies ~ "FIES", TRUE ~ "Open")
    )

  bind_rows(
    pub[, c("TP_TURNO", "TP_MODALIDADE_ENSINO", "QT", "setor", "via")],
    priv[, c("TP_TURNO", "TP_MODALIDADE_ENSINO", "QT", "setor", "via")]
  ) |>
    mutate(
      ano = as.integer(ano),
      turno = classificar_turno(TP_MODALIDADE_ENSINO, TP_TURNO),
      QT = as.numeric(QT)
    ) |>
    group_by(ano, setor, turno, via) |>
    summarise(QT_MATRICULAS = sum(QT), .groups = "drop") |>
    filter(QT_MATRICULAS > 0) |>
    mutate(
      fonte = sprintf("SUP_ALUNO_%d (SEDAP+, nivel aluno)", ano),
      exato = TRUE
    )
}

# ==============================================================================
# PARTE B — 2020-2023, nivel curso, arquivos locais
#
# QT_MAT_DIURNO/QT_MAT_NOTURNO sao marginais do curso e nao se cruzam com as
# vias de acesso. O turno so e recuperavel para a modalidade presencial, e
# ainda assim sem cruzamento — por isso as linhas destes anos saem com
# turno = NA e exato = FALSE, exceto a separacao presencial/EaD.
# ==============================================================================
extrair_curso <- function(ano) {
  f <- file.path(
    here::here("data-raw", "INEP", "CENSUP_Publico"), ano, "dados",
    sprintf("MICRODADOS_CADASTRO_CURSOS_%d.CSV", ano)
  )
  if (!file.exists(f)) {
    warning("Arquivo de cursos ausente para ", ano, ": ", f)
    return(NULL)
  }
  message("Lendo cursos de ", ano, "...")

  cols <- c(
    "TP_CATEGORIA_ADMINISTRATIVA", "TP_MODALIDADE_ENSINO", "QT_MAT",
    "QT_MAT_FIES", "QT_MAT_PROUNII", "QT_MAT_PROUNIP", "QT_MAT_RESERVA_VAGA"
  )
  df <- fread(f, select = cols, encoding = "Latin-1", showProgress = FALSE)

  df |>
    mutate(
      setor = classificar_setor(TP_CATEGORIA_ADMINISTRATIVA),
      turno = ifelse(TP_MODALIDADE_ENSINO == 2, "Distance", "Presencial"),
      across(starts_with("QT_"), ~ tidyr::replace_na(as.numeric(.x), 0))
    ) |>
    group_by(setor, turno) |>
    summarise(
      Cotas = sum(ifelse(setor == "Publico", QT_MAT_RESERVA_VAGA, 0)),
      Open_Publico = sum(ifelse(setor == "Publico",
        QT_MAT - QT_MAT_RESERVA_VAGA, 0
      )),
      ProUni = sum(ifelse(setor == "Privado", QT_MAT_PROUNII + QT_MAT_PROUNIP, 0)),
      FIES = sum(ifelse(setor == "Privado", QT_MAT_FIES, 0)),
      Open_Privado = sum(ifelse(setor == "Privado",
        QT_MAT - QT_MAT_PROUNII - QT_MAT_PROUNIP - QT_MAT_FIES, 0
      )),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      c(Open_Publico, Cotas, ProUni, FIES, Open_Privado),
      names_to = "via", values_to = "QT_MATRICULAS"
    ) |>
    filter(QT_MATRICULAS > 0) |>
    mutate(
      ano = ano,
      via = recode(via, Open_Publico = "Open", Open_Privado = "Open"),
      fonte = sprintf("MICRODADOS_CADASTRO_CURSOS_%d (nivel curso, marginais)", ano),
      # Presencial/EaD e exato; o que nao e exato e a ausencia do corte
      # diurno/noturno, sinalizada pelo proprio valor de `turno`.
      exato = FALSE
    )
}

# ==============================================================================
# EXECUCAO
# ==============================================================================
if (sys.nframe() == 0L) {
  partes <- list()

  # Todos os anos em nivel de aluno, mesma fonte e mesma granularidade. O
  # caminho pelos arquivos de curso (extrair_curso, mantido abaixo) ficou
  # obsoleto quando se verificou que SUP_ALUNO_2020..2023 existem no SEDAP+;
  # segue no arquivo apenas como conferencia independente — foi com ele que
  # se validou, por fonte que nao se fala com a API, que o EaD privado e
  # 95-97% pagante integral em todos os anos.
  # Periodo completo disponivel no SEDAP. O esquema esta harmonizado — TP_TURNO
  # responde em todos os anos, inclusive nos anteriores a 2017, que nos
  # arquivos publicos usavam o prefixo CO_ (CO_TURNO_ALUNO). Isso e vantagem
  # propria do SEDAP+ sobre o acervo local, onde 011_ precisa de logica
  # condicional por faixa de ano.
  ANOS <- as.integer(Sys.getenv("SEDAP_ANOS_INI", "2009")):
  as.integer(Sys.getenv("SEDAP_ANOS_FIM", "2024"))

  for (a in ANOS) {
    partes[[as.character(a)]] <- tryCatch(extrair_ano(a), error = function(e) {
      message("Ano ", a, " falhou: ", conditionMessage(e))
      NULL
    })
  }

  res <- bind_rows(partes) |>
    select(ano, setor, turno, via, QT_MATRICULAS, fonte, exato) |>
    arrange(ano, setor, turno, via)

  out <- file.path(OUT_DIR, sprintf("nicho_agregado_sedap_%d_%d.csv", min(ANOS), max(ANOS)))
  data.table::fwrite(res, out)
  message("Gravado: ", out, " (", nrow(res), " linhas)")
  print(as.data.frame(res))
}

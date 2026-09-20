# 047_Tese_Vagas_Focalizadas
# ODA: portado de 6-images-tables/final/vagas-focalizadas/2026-09-19_0903_047_Tese_Vagas_Focalizadas.R
#      em 2026-09-20; caminhos relativos a data-raw/; agregacao 2009-2024 sem cache externo.
# ==============================================================================
# FIGURA DEFINITIVA — vagas alocadas por politica FOCALIZADA no ensino
# superior brasileiro, 2009-2024 (Capitulo 2 / Parte II, versao PT).
#
# Variante da 046_Tese_Vagas_Descomodificadas.R pedida pelo autor em
# 2026-09-19: no lugar de contar TODA a rede publica gratuita no degrau de baixo
# da pilha, conta so' as matriculas de COTISTAS da rede publica. A pergunta da
# figura muda com isso, e vale deixar explicito:
#
#   • 046 mede DESCOMODIFICACAO — vaga sem preco. Por isso trata cotas como
#     outra dimensao (redistribuem quem ocupa a vaga gratuita, nao o preco) e
#     as poe em painel separado, com denominador proprio.
#   • 047 mede ACESSO FOCALIZADO — vaga alocada por criterio de necessidade
#     (escola publica / renda / raca / deficiencia). Cotas, ProUni e FIES tem
#     isso em comum, e por isso aqui ENTRAM NA MESMA PILHA. A rede publica de
#     ampla concorrencia (gratuita, mas nao focalizada) fica de fora.
#
#   Mantem-se o gradiente do 046 (a leitura "sem divida vs. divida" continua
#   valendo): cotas e ProUni em azul, FIES em laranja. A linha da fronteira
#   sem divida e a sua chamada, herdadas do 046, foram retiradas a pedido do
#   autor (2026-09-19): a troca de familia de cor ja' marca a divisa.
#
#   O achado muda de sinal em relacao a 046: la a parcela sem divida era
#   plana (~30%) e caia em 2023; aqui ela SOBE — de 7,0% (2009) para 13,7%
#   (2019) — e recua para 10,5% em 2024, porque o ProUni encolhe enquanto as
#   cotas seguem crescendo. O painel B mostra o que a proporcao esconde: em
#   volume, as cotas (0,64 M em 2024) sao o maior instrumento desde 2021, a
#   frente de ProUni (0,44 M) e FIES (0,16 M; pico de 1,34 M em 2015).
#
# ⚠️ RESSALVAS DOS DADOS:
#   • Matriculas_Cotas = QT_MAT_RESERVA_VAGA agregado por setor: soma TODOS os
#     tipos de reserva de vaga (escola publica, etnica, PcD, social, outros),
#     inclusive programas institucionais anteriores a Lei 12.711/2012. A
#     abertura por tipo (QT_MAT_RV_*) existe no microdado mas NAO esta' no
#     cache censup_agg_044.rds.
#   • Como no 046: ProUni e FIES contados so' no privado, cotas so' no publico;
#     os registros fora da rede legal sao ruido de preenchimento (<0,1%).
#   • Sobreposicao ProUni parcial + FIES nao e' separavel; os degraus sao
#     tratados como disjuntos.
#   • "Vagas" aqui sao MATRICULAS (estoque), nao vagas ofertadas — o mesmo
#     conceito das tres series.
#
# FONTE: microdados publicos do Censo da Educacao Superior (INEP), baixados
#   por shared-pipeline/censup/001 em data-raw/INEP/CENSUP_Publico/<ano>/dados/.
#   Cada ano 2009-2024 e' agregado aqui do CADASTRO_CURSOS (setor por
#   TP_CATEGORIA_ADMINISTRATIVA 1-3 = publico, ProUni = integral + parcial) e
#   o conjunto e' guardado em data-raw/INEP/derived/censup_agg_vagas_focalizadas.rds.
# Saida: output/graphs/ (rascunho) e finalizar_figura() -> output/figures/vagas-focalizadas/.
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())

# ------------------------------------------------------------------------------
# 1. DADOS — agregacao 2009-2024 direto do microdado publico do CENSUP
# ------------------------------------------------------------------------------
# ODA: a versao da tese lia 2009-2023 de um cache (censup_agg_044.rds) e so
# agregava 2024 do CSV. Aqui a serie inteira e agregada do CADASTRO_CURSOS com
# a mesma receita (verificado em 2026-09-20: os valores 2009-2023 recomputados
# sao identicos aos do cache da tese). Cache local para nao reler 16 CSVs.
base_dir <- here::here("data-raw", "INEP", "CENSUP_Publico")
cache_agg <- here::here("data-raw", "INEP", "derived", "censup_agg_vagas_focalizadas.rds")
ANOS <- 2009:2024

if (!dir.exists(base_dir)) {
  stop("Microdados publicos do CENSUP nao encontrados em data-raw/INEP/CENSUP_Publico/. ",
       "Rode antes: shared-pipeline/censup/001_Code_Download_Decompress_all_Public_CENSUP_data.R",
       call. = FALSE)
}

agregar_ano_censup <- function(ano) {
  csv <- list.files(
    file.path(base_dir, ano, "dados"),
    pattern = "CADASTRO_CURSOS.*\\.(CSV|csv)$", full.names = TRUE
  )
  if (length(csv) < 1) {
    stop("CADASTRO_CURSOS de ", ano, " nao encontrado em ", file.path(base_dir, ano, "dados"),
         call. = FALSE)
  }
  readr::read_delim(
    csv[1],
    delim = ";",
    col_select = any_of(c(
      "TP_CATEGORIA_ADMINISTRATIVA", "QT_MAT", "QT_MAT_RESERVA_VAGA",
      "QT_MAT_PROUNII", "QT_MAT_PROUNIP", "QT_MAT_FIES"
    )),
    locale = readr::locale(encoding = "latin1"), show_col_types = FALSE
  ) |>
    mutate(Setor = ifelse(TP_CATEGORIA_ADMINISTRATIVA %in% 1:3, "Público", "Privado")) |>
    group_by(Setor) |>
    summarise(
      Matriculas_Total = sum(QT_MAT, na.rm = TRUE),
      Matriculas_Cotas = sum(QT_MAT_RESERVA_VAGA, na.rm = TRUE),
      Matriculas_ProUni = sum(QT_MAT_PROUNII + QT_MAT_PROUNIP, na.rm = TRUE),
      Matriculas_FIES = sum(QT_MAT_FIES, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(Ano = ano)
}

if (file.exists(cache_agg)) {
  df_agg <- readRDS(cache_agg)
} else {
  df_agg <- bind_rows(lapply(ANOS, function(a) {
    cat("  agregando", a, "\n")
    agregar_ano_censup(a)
  }))
  dir.create(dirname(cache_agg), showWarnings = FALSE, recursive = TRUE)
  saveRDS(df_agg, cache_agg)
}
stopifnot(max(df_agg$Ano) == 2024)

stopifnot(setdiff(unique(df_agg$Setor), c("Privado", "Público")) |> length() == 0)

base <- df_agg |>
  mutate(rede = if_else(Setor == "Privado", "priv", "pub")) |>
  select(Ano, rede, Matriculas_Total, Matriculas_ProUni, Matriculas_FIES, Matriculas_Cotas) |>
  pivot_wider(
    names_from = rede,
    values_from = c(Matriculas_Total, Matriculas_ProUni, Matriculas_FIES, Matriculas_Cotas)
  ) |>
  arrange(Ano)

serie <- base |>
  transmute(
    Ano,
    total          = Matriculas_Total_priv + Matriculas_Total_pub,
    n_cotas        = Matriculas_Cotas_pub, # publico apenas
    n_prouni       = Matriculas_ProUni_priv, # privado apenas
    n_fies         = Matriculas_FIES_priv,
    pct_cotas      = 100 * n_cotas / total,
    pct_prouni     = 100 * n_prouni / total,
    pct_fies       = 100 * n_fies / total,
    pct_sem_divida = pct_cotas + pct_prouni, # fronteira sem divida
    pct_focalizado = pct_cotas + pct_prouni + pct_fies
  )

stopifnot(all(serie$pct_focalizado < 100), all(serie$n_cotas > 0))

# ------------------------------------------------------------------------------
# 2. CORES — as mesmas familias do 046: azul para os degraus SEM DIVIDA,
#    laranja para o emprestimo. O azul-escuro que no 046 era "rede publica
#    inteira" aqui e' "cotistas da rede publica".
# ------------------------------------------------------------------------------
COR_COTAS <- PALETA_QUALITATIVA[4] # #0072B2 azul-escuro — cota, gratuito
COR_PROUNI <- PALETA_QUALITATIVA[2] # #56B4E9 azul-ceu    — bolsa, sem divida
COR_FIES <- PALETA_QUALITATIVA[1] # #E69F00 laranja     — emprestimo

EXPAND_X <- expansion(mult = c(0.01, 0.01)) # IDENTICO nos dois paineis

# ------------------------------------------------------------------------------
# 3. PAINEL A — proporcao da matricula total, empilhada na ordem do gradiente
#    (primeiro nivel do fator = topo da pilha)
# ------------------------------------------------------------------------------
NIVEIS_A <- c("FIES", "ProUni", "Cotas")

dados_a <- serie |>
  select(Ano, Cotas = pct_cotas, ProUni = pct_prouni, FIES = pct_fies) |>
  pivot_longer(-Ano, names_to = "degrau", values_to = "share") |>
  mutate(degrau = factor(degrau, levels = NIVEIS_A))

# Anos de virada (calculados, nao digitados).
ano_pico_total <- serie$Ano[which.max(serie$pct_focalizado)]
pico_total <- serie |> filter(Ano == ano_pico_total)
fim <- serie |> filter(Ano == max(Ano))

Y_MAX_A <- 32

# Rotulos dentro das faixas. Espessuras reais:
#   • cotas: 1,4 -> 6,9 p.p.; so' comporta texto a partir de ~2017 (5,7 p.p.),
#     rotulo em 2020, no meio da faixa (~3,3).
#   • ProUni: 4-7 p.p., entre a faixa de cotas e a fronteira; rotulo em 2011,
#     quando a pilha ainda e' baixa e nao ha' anotacao por cima.
#   • FIES: espesso entre 2013 e 2018; rotulo em 2015, no meio da faixa.
rotulos_a <- tibble::tribble(
  ~x, ~y, ~label, ~cor,
  2020.0, 3.3, "Public-sector quota seats", "white",
  2011.0, 4.7, "ProUni scholarship", "#0B3A57",
  2015.4, 19.0, "FIES student loan", "#5A3D00"
)

p_a <- ggplot(dados_a, aes(Ano, share, fill = degrau)) +
  geom_area(colour = "white", linewidth = 0.15) +
  geom_text(
    data = rotulos_a, aes(x, y, label = label, colour = cor),
    inherit.aes = FALSE, size = 2.7, fontface = "bold", show.legend = FALSE
  ) +
  # Pico do total focalizado (com FIES).
  annotate("text",
    x = ano_pico_total, y = pico_total$pct_focalizado + 1.0, hjust = 0.5, vjust = 0,
    label = sprintf("%.1f%% (%d)", pico_total$pct_focalizado, ano_pico_total),
    size = 2.6, fontface = "bold", colour = "#3A3A3A"
  ) +
  # Ultimo ano: as duas quantidades, juntas, no vazio acima da pilha. "Without
  # debt" = topo do bloco azul (cotas + ProUni); a linha da fronteira e a sua
  # chamada foram retiradas a pedido do autor (2026-09-19) — a troca de familia
  # de cor ja' marca a divisa.
  annotate("text",
    x = fim$Ano - 0.1, y = 29.5, hjust = 1, vjust = 1,
    label = sprintf(
      "%d: %.1f%% with FIES\n%.1f%% without debt",
      fim$Ano, fim$pct_focalizado, fim$pct_sem_divida
    ),
    size = 2.6, lineheight = 0.95, fontface = "bold", colour = "#3A3A3A"
  ) +
  scale_fill_manual(values = c(
    "Cotas" = COR_COTAS, "ProUni" = COR_PROUNI, "FIES" = COR_FIES
  )) +
  scale_colour_identity() +
  scale_x_anos_tese(serie$Ano, expand = EXPAND_X) +
  scale_y_continuous(
    limits = c(0, Y_MAX_A), breaks = seq(0, 30, 10),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.01))
  ) +
  coord_cartesian(xlim = range(serie$Ano), clip = "off") +
  labs(
    subtitle = "A. Seats allocated by targeted policy, as a share of all enrolment (%)",
    x = NULL, y = NULL
  ) +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(face = "bold", size = 9, hjust = 0)
  )

# ------------------------------------------------------------------------------
# 4. PAINEL B — numero absoluto de matriculas em cada politica (milhares),
#    tres linhas NAO empilhadas, rotulo direto no fim de cada linha, DENTRO do
#    painel (hjust = 1), sem legenda.
# ------------------------------------------------------------------------------
dados_b <- serie |>
  select(Ano, Cotas = n_cotas, ProUni = n_prouni, FIES = n_fies) |>
  pivot_longer(-Ano, names_to = "politica", values_to = "n") |>
  mutate(
    n_mil = n / 1000,
    politica = factor(politica, levels = c("Cotas", "ProUni", "FIES"))
  )

fim_b <- dados_b |>
  filter(Ano == max(Ano)) |>
  mutate(
    label = c(
      Cotas = "Public-sector quota seats",
      ProUni = "ProUni scholarship",
      FIES = "FIES student loan"
    )[as.character(politica)],
    # Cotas (603 mil) acima da propria linha; ProUni (401 mil) e FIES (177 mil)
    # ABAIXO das suas — a linha do ProUni desce de 567 para 404 entre 2019 e
    # 2023 e um rotulo por cima, correndo para a esquerda, cairia sobre ela.
    y_lab = n_mil + c(Cotas = 70, ProUni = -60, FIES = -60)[as.character(politica)]
  )

pico_fies <- dados_b |>
  filter(politica == "FIES") |>
  slice_max(n_mil, n = 1)

p_b <- ggplot(dados_b, aes(Ano, n_mil, colour = politica)) +
  geom_line(linewidth = 0.8) +
  geom_point(data = fim_b, size = 1.4) +
  geom_text(
    data = fim_b, aes(x = Ano, y = y_lab, label = label),
    hjust = 1, size = 2.6, fontface = "bold", show.legend = FALSE
  ) +
  annotate("text",
    x = pico_fies$Ano, y = pico_fies$n_mil + 60, hjust = 0.5, vjust = 0,
    label = sprintf("%s thousand (%d)", format(round(pico_fies$n_mil), big.mark = ","), pico_fies$Ano),
    size = 2.6, fontface = "bold", colour = "#5A3D00"
  ) +
  scale_colour_manual(values = c(
    "Cotas" = COR_COTAS, "ProUni" = COR_PROUNI, "FIES" = COR_FIES
  )) +
  scale_x_anos_tese(serie$Ano, expand = EXPAND_X) +
  scale_y_continuous(
    limits = c(0, 1500), breaks = seq(0, 1500, 500),
    labels = function(x) format(x, big.mark = ","),
    expand = expansion(mult = c(0, 0.02))
  ) +
  coord_cartesian(xlim = range(serie$Ano), clip = "off") +
  labs(
    subtitle = "B. Enrolment under each policy (thousands of students)",
    x = NULL, y = NULL
  ) +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(face = "bold", size = 9, hjust = 0)
  )

# ------------------------------------------------------------------------------
# 5. COMPOSICAO
# ------------------------------------------------------------------------------
p_final <- (p_a / p_b) +
  plot_layout(heights = c(1.4, 1)) &
  theme(plot.margin = margin(t = 4, r = 5, b = 2, l = 2))

ALTURA_FIG <- 5.5

salvar_grafico(
  p_final,
  prefixo = "047-vagas-focalizadas",
  largura = LARGURA_TEXTO,
  altura  = ALTURA_FIG
)

# ------------------------------------------------------------------------------
# 6. PROMOCAO PARA O TEXTO (Fase 2)
# ------------------------------------------------------------------------------
finalizar_figura(
  plot = p_final,
  fig_label = "vagas-focalizadas",
  fig_cap = paste(
    "Seats allocated by targeted access policy in Brazilian higher education,",
    "2009–2024."
  ),
  nota = paste(
    "Panel A stacks the three instruments that allocate a seat by a need-based",
    "criterion, as a share of all enrolment: reserved (quota) seats in the",
    "public network, ProUni scholarships and FIES loans. Open-competition public",
    "seats and full-fee private enrolment are not drawn. Quota seats and ProUni",
    "share a colour family because neither creates debt; FIES is a loan and is",
    "kept apart. Panel B gives the same three series in absolute numbers. ProUni",
    "and FIES are counted in the private network only and quota seats in the",
    "public network only; quota seats cover every type of reservation the Census",
    "records (public school, income, race, disability and institutional",
    "programmes), before and after the 2012 Quota Law."
  ),
  fonte = paste(
    "INEP --- Censo da Educação Superior, microdata (2009--2024)"
  ),
  apendice = "sec-fignote-vagas-focalizadas",
  script_path = here::here("posts", "vagas-focalizadas", "plot.R"),
  largura = LARGURA_TEXTO,
  altura = ALTURA_FIG
)

# ------------------------------------------------------------------------------
# 7. DIAGNOSTICO
# ------------------------------------------------------------------------------
cat("\n--- Participacao no TOTAL de matriculas (%) e absolutos (milhares) ---\n")
print(as.data.frame(serie |>
  transmute(Ano,
    total,
    cotas = round(pct_cotas, 2),
    prouni = round(pct_prouni, 2),
    fies = round(pct_fies, 2),
    sem_divida = round(pct_sem_divida, 2),
    focalizado = round(pct_focalizado, 2),
    n_cotas_mil = round(n_cotas / 1000),
    n_prouni_mil = round(n_prouni / 1000),
    n_fies_mil = round(n_fies / 1000)
  )), row.names = FALSE)

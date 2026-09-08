# ==============================================================================
# 095_Fig_Distribuicao_Renda_Setor.R
#
# Distribuicao acumulada da renda familiar dos ingressantes de 2023, por
# destino, contra a distribuicao do POOL inteiro de inscritos no ENEM.
#
# ------------------------------------------------------------------------------
# POR QUE ESTA FIGURA EXISTE, TENDO JA A @fig-perfil-renda-modalidade
# ------------------------------------------------------------------------------
# Aquela figura reporta a fracao abaixo de UM limiar (1,5 SM) ao longo de nove
# anos. Um limiar unico esconde a forma: dois grupos podem ter a mesma fracao
# abaixo de 1,5 e perfis completamente diferentes acima dela. Aqui a
# distribuicao inteira e mostrada, e o leitor le qualquer limiar que queira —
# a distancia vertical entre curvas em qualquer ponto do eixo e interpretavel
# direto.
#
# ------------------------------------------------------------------------------
# A CURVA DO POOL, QUE E O PONTO DA FIGURA
# ------------------------------------------------------------------------------
# A quarta curva sao TODOS os inscritos no ENEM de 2023, tenham ingressado ou
# nao (4.018.232 registros). Ela transforma o principal defeito deste dado —
# a selecao pelo ENEM — em ferramenta.
#
# Sem ela, a pergunta e "qual a renda dos alunos de cada setor", e a selecao
# contamina a resposta: quem faz ENEM nao e amostra da populacao. Com ela, a
# pergunta passa a ser "dado o MESMO pool de candidatos, quem cada setor
# recruta dele" — uma pergunta de selecao condicional, para a qual o pool
# inteiro e o denominador honesto. Um setor cuja curva coincide com a do pool
# apenas reflete quem se candidata; um setor cuja curva se desloca para a
# esquerda recruta desproporcionalmente da cauda pobre.
#
# ------------------------------------------------------------------------------
# EIXO X
# ------------------------------------------------------------------------------
# Q006 e ordinal em 17 faixas. Plotar as letras seria ilegivel, entao o eixo usa
# o LIMITE SUPERIOR de cada faixa em salarios minimos (codebook do ENEM):
#   A=0  B=1  C=1,5  D=2  E=2,5  F=3  G=4  H=5  I=6  J=7  K=8  L=9  M=10
#   N=12  O=15  P=20  Q=aberta (acima de 20)
# A faixa Q e aberta e nao tem limite superior, e o eixo para em 10 SM; a nota
# registra o que fica de fora. Renda do DOMICILIO inteiro, nao per capita.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(here)
})

source(here::here("shared-pipeline", "utils", "plot_theme.R"))
source(here::here("4-DA-Code", "2026-08_SEDAP", "010_SEDAP_Cliente.R"))
theme_set(thesis_theme())

ANO <- 2023L

# Limite superior de cada faixa, em salarios minimos
LIM_SM <- c(
  A = 0, B = 1, C = 1.5, D = 2, E = 2.5, F = 3, G = 4, H = 5,
  I = 6, J = 7, K = 8, L = 9, M = 10, N = 12, O = 15, P = 20
)

# --- Ingressantes, por destino ------------------------------------------------
d <- fread(here::here(
  "4-DA-Code", "2026-08_SEDAP", "060_Analise_ENEM_Renda",
  "extraido_perfil_renda_modalidade_2014_2024.csv"
))[ano == ANO]

d[, grupo := fifelse(setor == "Privado" & Modalidade == "EaD", "Private, distance",
  fifelse(setor == "Privado", "Private, in person", "Public, in person")
)]
d <- d[!(grupo == "Public, in person" & Modalidade == "EaD")]

# --- Pool: todos os inscritos no ENEM daquele ano -----------------------------
message("SEDAP+: distribuicao de renda de todos os inscritos no ENEM ", ANO, "...")
pool <- sedap_query(sprintf(
  "SELECT Q006, COUNT(*) AS N FROM raw.ENEM_%d_SEDAP GROUP BY Q006", ANO
))
if (is.null(pool)) stop("Consulta do pool falhou.", call. = FALSE)
pool <- as.data.table(pool)[!is.na(Q006), .(grupo = "All ENEM candidates",
                                            Q006, N = as.numeric(N))]

todos <- rbind(d[, .(grupo, Q006, N = as.numeric(N))], pool)

# --- Acumulada ----------------------------------------------------------------
todos <- todos[Q006 %in% names(LIM_SM)] # exclui Q (faixa aberta)
todos[, sm := LIM_SM[Q006]]
todos <- todos[, .(N = sum(N)), by = .(grupo, sm)][order(grupo, sm)]
# O denominador inclui a faixa aberta, para a acumulada nao chegar a 100%
tot <- rbind(d[, .(grupo, N = as.numeric(N))], pool[, .(grupo, N)])[
  , .(total = sum(N)), by = grupo
]
todos <- merge(todos, tot, by = "grupo")
todos[, acum := 100 * cumsum(N) / total, by = grupo]

CORES <- c(
  "Private, distance" = "#D55E00",
  "Private, in person" = "#0072B2",
  "Public, in person" = "#009E73",
  "All ENEM candidates" = "#666666"
)
TIPO <- c(
  "Private, distance" = "solid", "Private, in person" = "solid",
  "Public, in person" = "solid", "All ENEM candidates" = "22"
)

# Legenda no rodape em vez de rotulo direto: com quatro series que CONVERGEM
# a direita, nao existe ponto do eixo onde as quatro estejam separadas o
# bastante para caber texto. Rotulo direto so paga quando as series terminam
# distantes, que nao e o caso aqui.
NIVEIS <- c(
  "Private, distance", "Public, in person",
  "Private, in person", "All ENEM candidates"
)
todos[, grupo := factor(grupo, levels = NIVEIS)]

# Eixo cortado em 10 SM: acima disso as quatro curvas passam de 95% e ficam
# indistinguiveis, entao o resto do painel seria espaco morto. A nota registra
# o que fica de fora.
LIM_X <- 10

p <- ggplot(todos, aes(x = sm, y = acum, colour = grupo, linetype = grupo)) +
  geom_step(linewidth = 1.0, direction = "hv") +
  scale_colour_manual(values = CORES, name = NULL, breaks = NIVEIS) +
  scale_linetype_manual(values = TIPO, name = NULL, breaks = NIVEIS) +
  scale_x_continuous(
    breaks = c(0, 1, 1.5, 2, 3, 4, 5, 6, 8, 10),
    labels = function(x) paste0(x, "×"),
    limits = c(0, LIM_X),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"), limits = c(0, 100),
    expand = expansion(mult = c(0.01, 0.04))
  ) +
  labs(
    x = "Household income, in multiples of the minimum wage",
    y = "Cumulative share of entrants",
    subtitle = sprintf(
      "Entrants of %d by destination, against the whole pool of that year's examinees", ANO
    )
  ) +
  theme(
    plot.subtitle = element_text(
      size = 8.5, colour = "#444444", margin = margin(b = 6)
    ),
    legend.position = "bottom",
    legend.text = element_text(size = 7.5),
    legend.key.width = unit(0.85, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.margin = margin(t = -2)
  )

salvar_grafico(p,
  prefixo = "095_Distribuicao_Renda_Setor",
  largura = LARGURA_TEXTO, altura = 3.6, unidades = "in"
)

cat("\n=== acumulada em limiares selecionados (%) ===\n")
print(dcast(todos[sm %in% c(1.5, 3, 5, 10)], grupo ~ sm, value.var = "acum")[
  , lapply(.SD, function(x) if (is.numeric(x)) round(x, 1) else x)
], row.names = FALSE)

finalizar_figura(
  plot = p,
  fig_label = "distribuicao-renda-setor",
  fig_cap = sprintf(
    "Household income of entrants by destination, against the pool of examinees, Brazil, %d.", ANO
  ),
  fonte = sprintf(
    "INEP — Censo da Educação Superior linked to ENEM by masked CPF, via SEDAP+ (%d)", ANO
  ),
  nota = paste(
    "Each curve is the cumulative share of a group whose total household income, as declared on the",
    "ENEM questionnaire, falls at or below the level on the horizontal axis --- for the household as",
    "a whole, not per capita. The dashed grey curve is every candidate who sat the examination that",
    "year, whether or not they enrolled anywhere, and it is the reference the other three are read",
    "against: a destination whose curve tracks it merely reflects who applies, while a curve to its",
    "left recruits disproportionately from the poorer part of the same pool. Reading the comparison",
    "this way is what makes the figure informative despite the examination being required for",
    "admission to the public network and not for distance study. Income is reported in seventeen",
    "ordered bands, so the curves are step functions; the highest band is open-ended above twenty",
    "minimum wages and is therefore not drawn, which is why no curve reaches 100\\%."
  ),
  apendice = "sec-fignote-distribuicao-renda-setor",
  script_path = here::here(
    "4-DA-Code", "2026-08_SEDAP", "060_Analise_ENEM_Renda",
    "095_Fig_Distribuicao_Renda_Setor.R"
  ),
  largura = LARGURA_TEXTO, altura = 3.6
)

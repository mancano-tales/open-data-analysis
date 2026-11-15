# ==============================================================================
# posts/gasto-federal-educacao-superior/plot.R
#
# Gasto federal com as universidades e com o subsidio aos estudantes do setor
# privado, Brasil, 1995-2025, em reais de 2024. Portado em 2026-11-14 do script
# 070_Plot_Gasto_Dois_Paineis.R da dissertacao (capitulo 3 e working paper do
# capitulo 0202).
#
#   Painel A: universidades federais sem inativos, no conceito de Carvalho
#             (2011, Tabela VII): a serie de Carvalho (1995-2008, convertida de
#             reais de 2008 para 2024 pelo IPCA medio) e a serie do SIOP no
#             mesmo conceito (2000-2025). As duas se sobrepoem em 2000-2008.
#   Painel B: FIES (desembolso com os contratos, liquidado) e renuncia fiscal
#             do ProUni.
#
# ENTRADAS (data/, redistribuidas):
#   gasto_federal_es_anual.csv  series univ_conceito_carvalho e fies_concessao
#       (SIOP, execucao orcamentaria da Uniao, consultada pelo endpoint SPARQL
#       publico em 2026-10-10, valores liquidados) e prouni_renuncia (Receita
#       Federal, Demonstrativo dos Gastos Tributarios - Bases Efetivas, edicao
#       de cada ano), em reais correntes e de 2024.
#   ipca_media_anual.csv  IPCA, media anual do numero-indice (SIDRA, tabela 1737).
#
# SAIDAS: fig-en.png, fig-pt.png e thumbnail.png (a figura em ingles).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(here)
  library(patchwork)
})

source(here::here("shared-pipeline", "utils", "plot_theme.R"))
theme_set(thesis_theme())

POST <- here::here("posts", "gasto-federal-educacao-superior")
if (!file.exists(file.path(POST, "data", "gasto_federal_es_anual.csv"))) {
  stop("Dados ausentes em data/. Rode antes, em ordem: shared-pipeline/gasto-federal/",
       "010_Coleta_SIOP_Execucao.R, 015_Download_RFB_Gastos_Tributarios.R, ",
       "020_ProUni_Renuncia_RFB.R e 030_Monta_Series_Gasto.R", call. = FALSE)
}
s <- read.csv(file.path(POST, "data", "gasto_federal_es_anual.csv")) |>
  mutate(bi = valor_real / 1e9)
ipca <- read.csv(file.path(POST, "data", "ipca_media_anual.csv"))
fator_2008_2024 <- ipca$indice[ipca$ano == 2024] / ipca$indice[ipca$ano == 2008]

# Carvalho (2011), Tabela VII (p. 331): IFES sem inativos, R$ de 2008.
carvalho <- data.frame(
  ano = 1995:2008,
  valor_2008 = c(8378349970, 7526325587, 7380856357, 7262226947, 7874736726, 8156114498,
                 7622318818, 7802489683, 7696758044, 8446693295, 8194655303, 9208625641,
                 9948342395, 10759865208)
) |>
  mutate(bi = valor_2008 * fator_2008_2024 / 1e9, serie = "carvalho")
proprio <- s |> filter(serie == "univ_conceito_carvalho") |> mutate(serie = "siop")
b <- s |> filter(serie %in% c("fies_concessao", "prouni_renuncia"))

INI <- 1995
FIM <- max(s$ano)
escala_x <- scale_x_anos_tese(anos = INI:FIM, limits = c(INI, FIM),
                              expand = expansion(mult = c(0.02, 0.02)))

TXT <- list(
  en = list(siop = "Federal budget data (SIOP), same concept",
            carvalho = "Carvalho (2011), converted to 2024 prices",
            fies = "FIES student loans disbursed", prouni = "ProUni tax expenditure",
            a = "A. Federal universities, excluding retirees and pensions",
            b = "B. Subsidies to private-sector students",
            y = "R$ billion (2024 prices)"),
  pt = list(siop = "Orçamento federal (SIOP), mesmo conceito",
            carvalho = "Carvalho (2011), convertido para preços de 2024",
            fies = "Desembolsos do FIES", prouni = "Renúncia fiscal do ProUni",
            a = "A. Universidades federais, sem aposentadorias e pensões",
            b = "B. Subsídios aos estudantes do setor privado",
            y = "R$ bilhões (preços de 2024)")
)

tema_painel <- theme(panel.grid.minor = element_blank(),
                     legend.position = "bottom",
                     legend.margin = margin(t = 0),
                     plot.title = element_text(face = "bold", size = 9.5))

figura <- function(txt) {
  rot_a <- c(siop = txt$siop, carvalho = txt$carvalho)
  a <- ggplot(mapping = aes(ano, bi, colour = serie, linetype = serie)) +
    linhas_transicao_gov() +
    geom_line(data = proprio, linewidth = 0.8) +
    geom_line(data = carvalho, linewidth = 0.8) +
    geom_point(data = carvalho, size = 1.0, show.legend = FALSE) +
    scale_colour_manual(name = NULL, breaks = names(rot_a), labels = rot_a,
                        values = c(siop = PALETA_QUALITATIVA[[4]], carvalho = PALETA_QUALITATIVA[[1]])) +
    scale_linetype_manual(name = NULL, breaks = names(rot_a), labels = rot_a,
                          values = c(siop = "solid", carvalho = "22")) +
    escala_x +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(title = txt$a, x = NULL, y = txt$y) +
    guides(colour = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1)) +
    tema_painel
  rot_b <- c(fies_concessao = txt$fies, prouni_renuncia = txt$prouni)
  pb <- ggplot(b, aes(ano, bi, colour = serie)) +
    linhas_transicao_gov() +
    geom_line(linewidth = 0.8) +
    geom_point(size = 0.9) +
    scale_colour_manual(name = NULL, breaks = names(rot_b), labels = rot_b,
                        values = c(fies_concessao = PALETA_QUALITATIVA[[5]],
                                   prouni_renuncia = PALETA_QUALITATIVA[[3]])) +
    escala_x +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(title = txt$b, x = NULL, y = txt$y) +
    tema_painel
  faixa <- banner_governos(fim_dados = FIM, scale_x = escala_x, ini_dados = INI)
  faixa / a / pb + plot_layout(heights = c(0.6, 8, 7))
}

for (idioma in names(TXT)) {
  ggsave(file.path(POST, paste0("fig-", idioma, ".png")), figura(TXT[[idioma]]),
         width = LARGURA_TEXTO, height = 6.6, units = "in", dpi = DPI_IMPRESSAO)
}
file.copy(file.path(POST, "fig-en.png"), file.path(POST, "thumbnail.png"), overwrite = TRUE)

print(as.data.frame(b |> filter(ano %in% c(2011, 2017, 2022)) |>
  transmute(ano, serie, bi = round(bi, 1))))
print(as.data.frame(proprio |> filter(ano %in% c(2003, 2014)) |> transmute(ano, bi = round(bi, 1))))
cat("OK\n")

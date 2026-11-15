# Resolve a complete population snapshot without mixing old and new matrices.
resolver_dados_coortes <- function(raiz, populacao = "inscritos", coortes = 2012:2021) {
  stopifnot(populacao %in% c("inscritos", "presentes"))
  publicado <- file.path(raiz, "posts", "portas-renda-coortes-anuais", "data")
  cache <- file.path(raiz, "data-raw", "sedap", "portas-anuais", populacao)
  arquivos <- c(as.vector(outer(coortes, c("porta", "modalidade"),
    function(ano, tipo) sprintf("matriz_%d_%s.csv", ano, tipo))), "row_loss.csv")
  existem <- file.exists(file.path(cache, arquivos))
  if (any(existem)) {
    if (!all(existem)) stop("Extracao incompleta em ", cache, ": ",
      paste(arquivos[!existem], collapse = ", "), call. = FALSE)
    return(list(diretorio = cache, arquivos = arquivos, populacao = populacao))
  }
  meta <- file.path(publicado, "populacao.csv")
  if (!file.exists(meta) || !identical(read.csv(meta)$populacao, populacao))
    stop("Sem agregado identificado para POPULACAO=", populacao,
      ". Rode 020_Extrair_Coortes_Anuais.R com essa populacao.", call. = FALSE)
  if (!all(file.exists(file.path(publicado, arquivos))))
    stop("Agregados publicados incompletos.", call. = FALSE)
  list(diretorio = publicado, arquivos = arquivos, populacao = populacao)
}

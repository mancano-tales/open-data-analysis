source("shared-pipeline/eleicoes/historico/tse.R")
suppressPackageStartupMessages(library(data.table))
x <- data.table(ANO_ELEICAO=2026L, NR_TURNO=1L, SG_UF="SP", CD_MUNICIPIO=71072L,
 NR_ZONA=1L, NR_SECAO=1L, NR_LOCAL_VOTACAO=1000L,
 CD_CARGO=c(1L,1L,6L,6L,6L,6L,5L), NR_VOTAVEL=c(13,95,1313,13,96,97123,131),
 NM_VOTAVEL=c("A","BRANCO","B","PT","NULO","C","D"), QT_VOTOS=c(10,2,3,4,1,2,10))
n <- tse_normalize(x, 2026)
stopifnot(identical(n$vote_type, c("candidate","special","candidate","party","special","candidate","candidate")))
stopifnot(sum(tse_place_votes(n)$votes)==29)
# Identical section numbers in different zones must remain distinct.
x2 <- copy(x[1]); x2[, NR_ZONA := 2L]
stopifnot(length(unique(tse_normalize(rbind(x[1],x2))$section_key))==2L)
# Supplements retain election identity even if local/candidate keys coincide.
y <- rbind(n[1],n[1]); y[, election_id := c(1L,2L)]
stopifnot(nrow(tse_place_votes(y))==2L)
x[1, QT_VOTOS := -1]
stopifnot(inherits(try(tse_normalize(x), silent=TRUE), "try-error"))
x[1, QT_VOTOS := 1.5]
stopifnot(inherits(try(tse_normalize(x), silent=TRUE), "try-error"))
z <- copy(n[1]); z[, place_key := NA_character_]
stopifnot(inherits(try(tse_place_votes(z), silent=TRUE),"try-error"))
meta <- list(result=list(resources=data.frame(url=c(
 "https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_secao/votacao_secao_2022_BR.zip",
 "https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_secao/votacao_secao_2022_AC.zip"))))
stopifnot(grepl("2022_BR.zip$",tse_resource(meta,2022)$url))
stopifnot(inherits(try(tse_resource(meta,1994),silent=TRUE),"try-error"))
cat("Normalization, ballot classification, identity and resource tests passed.\n")

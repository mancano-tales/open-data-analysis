# Fornecedores das campanhas de 2026 — agregados estáticos para o post
# posts/fornecedores-campanha-2026/. Rodar a partir da raiz do repositório.
#
# Fonte: TSE, Portal de Dados Abertos, pacote prestacao-de-contas-eleitorais-2026,
# recurso "Prestação de contas de candidatos" (despesas_contratadas_*_BRASIL.csv).
# Medida: despesa CONTRATADA declarada (inclui o que ainda não foi pago).
# Pessoas físicas fornecedoras aparecem agregadas, sem nome nem CPF.
suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
})

# O zip (~190 MB) fica no SSD externo (ver AGENTS.md e .data-source). Um zip já
# baixado em data-raw/fornecedores-2026/ é reaproveitado; downloads novos só vão
# para o SSD, nunca para o disco interno.
out <- file.path("posts", "fornecedores-campanha-2026", "data")
dir.create(file.path(out, "candidatos"), recursive = TRUE, showWarnings = FALSE)

url <- "https://cdn.tse.jus.br/estatistica/sead/odsele/prestacao_contas/prestacao_de_contas_eleitorais_candidatos_2026.zip"
local_zip <- file.path("data-raw", "fornecedores-2026", basename(url))
ssd <- Sys.getenv("MANCANO_SSD_ROOT")
ssd_dest <- if (nzchar(ssd)) file.path(ssd, "projects", "open-data-analysis", "fornecedores-2026") else ""
zip <- if (file.exists(local_zip)) local_zip else file.path(ssd_dest, basename(url))
if (!file.exists(zip)) {
  if (!nzchar(ssd) || !dir.exists(ssd)) {
    stop("SSD externo indisponível: conecte-o e defina MANCANO_SSD_ROOT antes de baixar o arquivo do TSE.")
  }
  dir.create(ssd_dest, recursive = TRUE, showWarnings = FALSE)
  options(timeout = max(900, getOption("timeout")))
  download.file(url, paste0(zip, ".part"), mode = "wb")
  file.rename(paste0(zip, ".part"), zip)
}
member <- "despesas_contratadas_candidatos_2026_BRASIL.csv"
tmp <- tempfile(); dir.create(tmp)
unzip(zip, member, exdir = tmp)
d <- fread(file.path(tmp, member), encoding = "Latin-1", colClasses = "character",
           select = c("DT_GERACAO", "SG_UF", "DS_CARGO", "SQ_CANDIDATO", "NR_CANDIDATO",
                      "NM_CANDIDATO", "SG_PARTIDO", "TP_PRESTACAO_CONTAS", "DS_TIPO_FORNECEDOR",
                      "NR_CPF_CNPJ_FORNECEDOR", "NM_FORNECEDOR", "NM_FORNECEDOR_RFB",
                      "DS_ORIGEM_DESPESA", "DS_DESPESA", "VR_DESPESA_CONTRATADA"))
unlink(tmp, recursive = TRUE)
generated <- d$DT_GERACAO[1]

d[, v := as.numeric(sub(",", ".", VR_DESPESA_CONTRATADA, fixed = TRUE))]
stopifnot(!anyNA(d$v))
d <- d[v > 0 & DS_TIPO_FORNECEDOR != "#NULO"]

# Identidade do fornecedor: CNPJ (pessoa jurídica) ou grupo anônimo (pessoa física).
# Qualquer documento com 11 dígitos é CPF, mesmo quando o tipo vem com outro rótulo.
d[, doc_digits := gsub("[^0-9]", "", NR_CPF_CNPJ_FORNECEDOR)]
d[, pf := DS_TIPO_FORNECEDOR == "PESSOA FÍSICA" | nchar(doc_digits) <= 11]
d[, nome := fifelse(NM_FORNECEDOR_RFB %in% c("", "#NULO"), NM_FORNECEDOR, NM_FORNECEDOR_RFB)]
d[, chave := fifelse(pf, "PF", NR_CPF_CNPJ_FORNECEDOR)]
d[(pf), nome := "Pessoas físicas (soma, nomes omitidos)"]
nomes <- d[!(pf), .N, .(chave, nome)][order(-N)][!duplicated(chave), .(chave, nome)]
d[nomes, on = "chave", nome := i.nome]

# Meta: pagamentos diretos aos CNPJs da Facebook Serviços Online do Brasil e da
# Meta Platforms. Intermediários de pagamento (dLocal, Ebanx) ficam separados:
# a descrição nem sempre informa a plataforma final.
d[, meta := !pf & (startsWith(chave, "13347016") | grepl("^META PLATFORMS", nome))]
d[, intermediario := !pf & (startsWith(chave, "25021356") | grepl("^EBANX", nome))]
d[, intermediario_meta := intermediario & grepl("FACEBOOK|META|INSTAGRAM", toupper(DS_DESPESA))]
d[, impulsionamento := DS_ORIGEM_DESPESA == "Despesa com Impulsionamento de Conteúdos"]

cargo_ordem <- c("Presidente", "Governador", "Senador", "Deputado Federal",
                 "Deputado Estadual", "Deputado Distrital")
r <- function(x) round(x, 2)

# ---- Candidatos: top 10 fornecedores + resto, um arquivo por UF ------------
top_n <- 10L
cand <- d[, .(total = sum(v), n = .N, meta = sum(v[meta]),
              intermediario = sum(v[intermediario]),
              impulsionamento = sum(v[impulsionamento])),
          .(uf = SG_UF, cargo = DS_CARGO, id = SQ_CANDIDATO, numero = NR_CANDIDATO,
            nome = NM_CANDIDATO, partido = SG_PARTIDO)]
forn <- d[, .(v = sum(v), n = .N), .(id = SQ_CANDIDATO, chave, nome, pf)]
setorder(forn, id, -v)
forn[, rank := seq_len(.N), id]
for (u in sort(unique(cand$uf))) {
  cu <- cand[uf == u][order(match(cargo, cargo_ordem), -total)]
  fu <- forn[id %in% cu$id & rank <= top_n]
  lst <- lapply(seq_len(nrow(cu)), function(i) {
    x <- cu[i]
    f <- fu[id == x$id]
    list(id = x$id, numero = x$numero, nome = x$nome, partido = x$partido, cargo = x$cargo,
         total = r(x$total), n = x$n, meta = r(x$meta), intermediario = r(x$intermediario),
         impulsionamento = r(x$impulsionamento),
         fornecedores = lapply(seq_len(nrow(f)), function(j)
           list(nome = f$nome[j], doc = if (f$pf[j]) NULL else f$chave[j], pf = f$pf[j],
                v = r(f$v[j]), n = f$n[j])))
  })
  write_json(list(uf = u, candidatos = lst), file.path(out, "candidatos", paste0(u, ".json")),
             auto_unbox = TRUE, null = "null", digits = NA)
}

# ---- Partidos: top 25 fornecedores, total e por cargo ------------------------
top_p <- 25L
part_tot <- d[, .(total = sum(v), n = .N, candidatos = uniqueN(SQ_CANDIDATO), meta = sum(v[meta]),
                  intermediario = sum(v[intermediario]), impulsionamento = sum(v[impulsionamento])),
              .(partido = SG_PARTIDO, cargo = DS_CARGO)]
part_tot <- rbind(part_tot,
                  d[, .(cargo = "Todos", total = sum(v), n = .N, candidatos = uniqueN(SQ_CANDIDATO),
                        meta = sum(v[meta]), intermediario = sum(v[intermediario]),
                        impulsionamento = sum(v[impulsionamento])), .(partido = SG_PARTIDO)])
pf <- rbind(d[, .(v = sum(v), n = .N, candidatos = uniqueN(SQ_CANDIDATO)),
              .(partido = SG_PARTIDO, cargo = DS_CARGO, chave, nome, pf)],
            d[, .(cargo = "Todos", v = sum(v), n = .N, candidatos = uniqueN(SQ_CANDIDATO)),
              .(partido = SG_PARTIDO, chave, nome, pf)])
setorder(pf, partido, cargo, -v)
pf <- pf[, head(.SD, top_p), .(partido, cargo)]
partidos <- lapply(split(part_tot, by = c("partido", "cargo")), function(x) {
  f <- pf[partido == x$partido & cargo == x$cargo]
  list(partido = x$partido, cargo = x$cargo, total = r(x$total), n = x$n,
       candidatos = x$candidatos, meta = r(x$meta), intermediario = r(x$intermediario),
       impulsionamento = r(x$impulsionamento),
       fornecedores = lapply(seq_len(nrow(f)), function(j)
         list(nome = f$nome[j], doc = if (f$pf[j]) NULL else f$chave[j], pf = f$pf[j],
              v = r(f$v[j]), n = f$n[j], candidatos = f$candidatos[j])))
})
names(partidos) <- NULL
write_json(partidos, file.path(out, "partidos.json"), auto_unbox = TRUE, null = "null", digits = NA)

# ---- Meta: total, por partido, por cargo e por candidato ---------------------
meta_cand <- cand[meta > 0, .(uf, cargo, numero, nome, partido, meta = r(meta), total = r(total),
                              impulsionamento = r(impulsionamento),
                              share = round(meta / total, 4))][order(-meta)]
fwrite(meta_cand, file.path(out, "meta-candidatos.csv"))
fwrite(cand[, .(uf, cargo, numero, nome, partido, total = r(total), despesas = n,
                meta = r(meta), intermediario = r(intermediario), impulsionamento = r(impulsionamento))],
       file.path(out, "candidatos.csv"))

geral <- d[, .(total = sum(v), meta = sum(v[meta]), intermediario = sum(v[intermediario]),
               intermediario_meta = sum(v[intermediario_meta]),
               impulsionamento = sum(v[impulsionamento]),
               candidatos = uniqueN(SQ_CANDIDATO),
               candidatos_meta = uniqueN(SQ_CANDIDATO[meta]))]
top_geral <- d[!(pf), .(v = sum(v), candidatos = uniqueN(SQ_CANDIDATO)), .(chave, nome)][order(-v)][1:30]
resumo <- list(
  gerado_tse = generated,
  fonte = url,
  geral = lapply(geral, r),
  pessoas_fisicas = r(d[(pf), sum(v)]),
  prestacao = d[, .(total = r(sum(v)), candidatos = uniqueN(SQ_CANDIDATO)), .(tipo = TP_PRESTACAO_CONTAS)],
  por_cargo = d[, .(total = r(sum(v)), meta = r(sum(v[meta])), candidatos = uniqueN(SQ_CANDIDATO),
                    candidatos_meta = uniqueN(SQ_CANDIDATO[meta])), .(cargo = DS_CARGO)
                ][order(match(cargo, cargo_ordem))],
  por_partido = d[, .(total = r(sum(v)), meta = r(sum(v[meta])), candidatos = uniqueN(SQ_CANDIDATO),
                      candidatos_meta = uniqueN(SQ_CANDIDATO[meta])), .(partido = SG_PARTIDO)
                  ][, share := round(meta / total, 4)][order(-meta)],
  top_fornecedores = top_geral[, .(nome, doc = chave, v = r(v), candidatos)],
  ufs = sort(unique(cand$uf))
)
write_json(resumo, file.path(out, "resumo.json"), auto_unbox = TRUE, digits = NA, pretty = TRUE)

# Trava de privacidade: nenhum documento publicado pode ter formato de CPF.
json_text <- vapply(list.files(out, "[.]json$", recursive = TRUE, full.names = TRUE),
                    function(f) paste(readLines(f, warn = FALSE), collapse = ""), "")
if (any(grepl('"doc":"[0-9]{1,11}"', json_text))) stop("CPF-shaped supplier document in published data.")

cat(sprintf("TSE %s | total R$ %.1f mi | Meta direta R$ %.1f mi (%.1f%%) | %d candidatos\n",
            generated, geral$total / 1e6, geral$meta / 1e6, 100 * geral$meta / geral$total,
            geral$candidatos))

# Aquisição histórica de votos por seção

Esta camada utiliza a API CKAN oficial do TSE para descobrir o URL de cada ZIP e lê somente as colunas necessárias. A API é de catálogo: os votos são arquivos CSV compactados, sem consultas remotas por candidato. O arquivo `BR` é de **presidência**; arquivos de cada UF incluem os cargos estaduais. Não baixar os 27 arquivos para obter apenas a presidência.

Execute a partir da raiz do repositório:

```sh
Rscript --vanilla shared-pipeline/eleicoes/historico/prepare.R
Rscript --vanilla shared-pipeline/eleicoes/historico/prepare.R 2026 AC,DF 3,5,6,7,8
Rscript --vanilla shared-pipeline/eleicoes/historico/test-tse.R
```

O primeiro comando prepara 2010, 2014, 2018 e 2022 (ambos os turnos), presidência. Argumentos opcionais: lista de anos, lista de UFs e lista de códigos TSE de cargos. Cada ZIP é baixado uma vez; os metadados originais e os MD5 ficam no cache `data-raw/eleicoes-renda/historico/`. Os resultados são `places-{ano}-{UF}-{cargos}.rds`, agregados por eleição, turno, local, cargo e candidato. Os números de local vêm do resultado daquela eleição; nenhuma coordenada de outro ano é substituída automaticamente.

Cargos: 1 presidente, 3 governador, 5 senador, 6 deputado federal, 7 deputado estadual, 8 deputado distrital, 11 prefeito, 13 vereador. O esquema normalizado usa `year`, `round`, `uf`, `municipality_tse`, `zone`, `section`, `place`, `office`, `candidate_id`, `candidate_name`, `votes`, `vote_type`, `election_id`, `election_type`, `election_date`, `section_key`, `place_key`. `candidate_id` é o número votável do TSE, e precisa ser interpretado em conjunto com ano, cargo e UF; não identifica uma pessoa ao longo do tempo.

`vote_type` distingue votos nominais (`candidate`), legenda (`party`), códigos especiais 95/96/97 (`special`) e códigos inválidos. Deputados possuem quatro/cinco dígitos: o filtro presidencial `NR_VOTAVEL < 95` não serve para estes cargos. A normalização mantém legenda e a agregação oferece essa informação para que o exportador escolha e declare o denominador. Para legislativos, o denominador de votos válidos inclui legenda; para senadores, soma votos atribuídos, podendo haver duas escolhas por eleitor conforme a eleição. Não chamar essa proporção de porcentagem de eleitores.

Por padrão são filtradas eleições ordinárias (`CD_TIPO_ELEICAO == 2`). O identificador da eleição é preservado para impedir soma acidental com suplementares. A preparação exclui exterior (`ZZ`), votos especiais e inválidos do agregado espacial, guardando uma auditoria em CSV. Um local ausente causa erro: requer cadastro de seções do mesmo ano antes de agregar.

## Disponibilidade confirmada em 15/10/2026 UTC

Consulta real ao CKAN encontrou arquivos por seção de 1998, 2002, 2006, 2010, 2014, 2018, 2022 e 2026. Em 1994 o TSE avisa que o acervo é incompleto e foi republicado somente por município e zona, sem seção. Não converter município/zona em observação de setor.

O catálogo da instalação `geobr 2.0.0`, consultado por `geobr::list_geobr()`, lista locais georreferenciados para **2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024 e 2026**. Assim a interseção direta de resultados presidenciais e coordenadas contemporâneas começa em 2010. Arquivos anteriores a 2010 podem ser baixados e normalizados, mas não há suporte espacial contemporâneo confirmado nesta camada; não são séries prontas para publicação. O helper `tse_geobr_years()` consulta o catálogo novamente, evitando congelar essa restrição no código.

Os ZIPs presidenciais 2010/2014/2018/2022 foram baixados e os esquemas reais inspecionados: todos trazem as colunas modernas esperadas, inclusive `NR_LOCAL_VOTACAO`. A existência do catálogo não garante cobertura de coordenadas, compatibilidade municipal ou dados completos; cada série precisa das auditorias espaciais e de votos. A renda e a ordem dos setores permanecem as do Censo 2022, mesmo quando os votos são históricos. Não se interpreta o gráfico como renda da população na data da eleição.

Fontes primárias: [TSE 1994](https://dadosabertos.tse.jus.br/dataset/resultados-1994), [TSE 1998](https://dadosabertos.tse.jus.br/dataset/resultados-1998), [TSE 2010](https://dadosabertos.tse.jus.br/dataset/resultados-2010), [TSE 2014](https://dadosabertos.tse.jus.br/dataset/resultados-2014), [TSE 2018](https://dadosabertos.tse.jus.br/dataset/resultados-2018), [TSE 2022](https://dadosabertos.tse.jus.br/dataset/resultados-2022), [TSE 2026](https://dadosabertos.tse.jus.br/dataset/resultados-2026), [geobr](https://github.com/ipeaGIT/geobr).

## Reconciliação dos votos legalmente válidos

O arquivo por seção inclui votos nominais e de legenda anulados ou sub judice.
Excluir apenas 95/96/97 **não** produz necessariamente o total válido oficial.
`legal-validity.R` cruza os votos com `votacao_candidato_munzona`,
`votacao_partido_munzona` e `detalhe_votacao_munzona` do mesmo ano. Votos de
candidatos convertidos oficialmente em legenda passam a compor esse
denominador, sem se tornarem votos nominais de outro candidato.

Para cada candidato ou legenda dentro de município/zona, só se aceita que
todos os votos sejam válidos ou que nenhum seja válido. Uma validade parcial
provoca erro: não há rateio de totais oficiais entre seções. Códigos nominais
sem correspondência no cadastro precisam fechar exatamente com os nulos
técnicos oficiais. A soma nominal válida, a legenda válida e o total válido
precisam coincidir em **todas** as zonas de uma disputa (ano, turno, eleição,
cargo e UF). Disputas divergentes são excluídas integralmente do cache
certificado e registradas em `vote_audit$unavailable`. Outras disputas da
mesma UF podem ser aceitas se todos os seus testes passarem. Os CSVs de
auditoria preservam também as divergências e ficam junto aos RDS.

Tabelas históricas sem campo de destinação/validade nominal usam um caminho
mais restrito: mantêm provisoriamente todos os votos nominais e só são
aceitas se os mesmos totais oficiais por zona coincidirem exatamente. Uma
diferença deixa a disputa indisponível; não se infere validade pela situação
de registro ou pelo nome do candidato.

```r
source("shared-pipeline/eleicoes/historico/legal-validity.R")
legal_reconcile_places(2026, "BR", offices = 1L)
legal_reconcile_places(2026, "DF", offices = c(3L, 5L, 6L, 8L))
```

Caches `*-legal.rds` incluem `normalizer_version = 2` e `vote_audit`, que o
exportador verifica novamente antes de publicar. Reconciliação eleitoral
não certifica geocodificação nem atribuição de votos aos setores.

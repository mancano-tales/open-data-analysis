# Preparação histórica estadual retomável

`prepare-historical-all-ufs.R` prepara, normaliza e reconcilia os votos estaduais com fontes oficiais, uma UF por vez em subprocessos isolados. O padrão é **somente 2022**. Os anos aceitos são 2022, 2018, 2014 e 2010; se uma lista explícita for passada, a ordem é sempre essa, sem executar anos não pedidos.

```sh
Rscript --vanilla shared-pipeline/eleicoes/historico/prepare-historical-all-ufs.R 2022 --dry-run
Rscript --vanilla shared-pipeline/eleicoes/historico/prepare-historical-all-ufs.R 2022
Rscript --vanilla shared-pipeline/eleicoes/historico/prepare-historical-all-ufs.R 2022 AC,DF
Rscript --vanilla shared-pipeline/eleicoes/historico/prepare-historical-all-ufs.R 2018
Rscript --vanilla shared-pipeline/eleicoes/historico/test-prepare-historical.R
```

Cada subprocesso usa no máximo duas threads de `data.table`; as UFs pequenas vêm primeiro e SP por último. Nenhum download ou processamento de UFs é paralelo. Os arquivos de 2026 permanecem intocados. Processar duas UFs grandes simultaneamente pode exceder a memória disponível; o orquestrador encerra cada subprocesso antes de iniciar o seguinte.

## Correção histórica reproduzível

O runner utiliza o código **versionado** `agregacao/validacao/legal-party-technical-wrapper.R`, que carrega `historico/legal-validity.R` em ambiente privado e aplica a correção já comprovada de legenda sem cadastro. Não depende de scripts locais de diagnóstico e não modifica o reconciliador comum em disco.

Em AC2022 foram encontrados três votos para o número de partido 29 sem registro na totalização partidária: um voto em uma zona e dois em outra. Esses votos só são classificados como nulos técnicos se o residual nominal + legenda fechar **exatamente em cada município/zona** contra `QT_VOTOS_NULOS_TECNICOS`. Nominais válidos, legenda válida e total válido também precisam fechar exatamente nas mesmas unidades. Não há rateio, inferência de elegibilidade ou tolerância de diferenças.

O probe real de AC2022 certificou os quatro cargos e suas 23 zonas, com diferença zero: governador 426.600, senador 411.949, deputado federal 434.253 e deputado estadual 435.770 votos válidos. Isso prova a viabilidade de 2022; não declara automaticamente viáveis as demais UFs ou anos. Cada execução posterior pode terminar `complete`, `partial` ou `failed` conforme sua própria reconciliação. Tabelas históricas com esquema insuficiente ou votos sem alocação jurídica demonstrável precisam de investigação; não são liberadas por aproximação.

## Retomada e proveniência

Checkpoints e logs ficam em `data-raw/eleicoes-renda/historico/historical-status/ANO/UF.json` e `UF.log`. Um resumo é gravado em `prepare-ANO-historical-status.csv`. Cada checkpoint registra status, disputas certificadas, indisponibilidades, duração e **MD5** de todos os ZIPs de entrada disponíveis (seção da UF, nominal, partidário e detalhe), metadados CKAN, código do runner/normalizador/reconciliador/wrapper e cache legal de saída.

A retomada só pula um resultado `complete` ou `partial` quando os hashes permanecem idênticos. `--retry-partial` repete resultados parciais; `--rebuild` reexecuta a reconciliação de todos os alvos pedidos reutilizando os downloads. Checkpoints `failed` ou `running` são tentados novamente. Mudança em qualquer código ou insumo também invalida a retomada. Os hashes são memoizados por tamanho/data de alteração durante o processo controlador, evitando reprocessar o mesmo ZIP nacional a cada verificação de checkpoint.

O runner exige que cada disputa ordinária do detalhe oficial apareça em `checks` certificados ou em `unavailable`; um desaparecimento silencioso causa erro. Só retorna exit 0 se todas as UFs pedidas estiverem `complete`. Resultados parciais são registrados e não impedem as UFs seguintes, mas o lote termina com exit 1 para não confundir progresso com conclusão. `--dry-run` não baixa nem escreve dados.

## Limpeza das extrações recriáveis

```sh
Rscript --vanilla shared-pipeline/eleicoes/historico/cleanup-historical-extractions.R 2022 --dry-run
Rscript --vanilla shared-pipeline/eleicoes/historico/cleanup-historical-extractions.R 2022 --apply
Rscript --vanilla shared-pipeline/eleicoes/historico/test-cleanup-historical.R
```

A limpeza exige checkpoint `complete` da UF. Confere o MD5 do ZIP contra o checkpoint, normaliza e valida os caminhos absolutos dentro de `historico`, exige correspondência exata entre diretório de extração e nome do ZIP e seleciona apenas o membro CSV daquela UF listado no arquivo. Antes de remover cada arquivo, também confere tamanho e **CRC32 do conteúdo**, preservando inclusive alterações de mesmo tamanho feitas por outra sessão. A remoção é individual: não remove diretórios e não usa exclusão recursiva. Mantém ZIPs, RDS, metadados, logs, catálogos e probes. Dependências adicionais: `zip` e `digest`.

O teste sintético cobre checkpoint parcial, MD5 incorreto, CSV alterado com tamanho diferente e igual, preservação de probes e ZIPs e idempotência. Um relatório da execução de limpeza fica no cache. Somente as extrações de UFs já completas são elegíveis; um worker ainda ativo não tem seus arquivos removidos.

## Eleições extraordinárias nas tabelas anuais

RR2022 mostrou outra particularidade real: o arquivo nominal daquele ano também inclui a eleição extraordinária 6278, denominada suplementar de governador de 2026 (21/06/2026). A fonte de seções é filtrada para eleição ordinária. O reconciliador agora restringe **todas** as tabelas jurídicas — candidatos, partidos e detalhe — aos mesmos `election_id` selecionados na fonte de seções. Isso impede comparar ou somar pleitos diferentes. `test-historical-ordinary.R` contém uma suplementar positiva na fixture e comprova a exclusão; também comprova que três votos técnicos fecham e que um quarto voto indevido é rejeitado. O teste preexistente da correção de legenda em 2026 passou sem alteração nos caches reais de 2026.

O worker usa também o agrupamento exato otimizado `candidate-group-fastpath.R`, já comparado à implementação de referência para chaves únicas, duplicadas e valores ausentes. As conferências jurídicas permanecem inalteradas. Os hashes do código são coletados antes de carregar os helpers; se o código mudar durante uma execução, o checkpoint exige repetição em vez de atribuir a uma execução antiga o hash novo.

Nos novos workers, a limpeza ocorre automaticamente depois do checkpoint completo. A remoção exige também uma **extração real de verificação** do membro em diretório temporário ignorado, seguida de tamanho/CRC da cópia recém-descomprimida. Assim um índice ZIP legível com payload corrompido não autoriza apagar o CSV original. O teste corrompe o payload, recalcula deliberadamente o MD5 do checkpoint e comprova a recusa. As cópias de verificação são removidas individualmente ao terminar.

`--cache-dir DIRETORIO` pode selecionar outro cache tanto no controlador quanto no worker filho; caminhos com espaços são testados por um subprocesso R real. O teste correspondente é `test-historical-cache-dir.R`. A proveniência inclui ainda o código da limpeza e do agrupamento rápido.

## Esquema partidário de 2018

A tabela oficial de partidos de AC2018 não publica `QT_VOTOS_LEGENDA_ANULADOS`. Quando exclusivamente essa categoria estiver ausente, o reconciliador usa as categorias publicadas (válidos e pendentes) como subtotal provisório. O subtotal ainda precisa coincidir exatamente com cada partido/município/zona nas seções; qualquer voto adicional sem explicação bloqueia a reconciliação. Não se presume a validade de votos pela situação da candidatura. `test-historical-ordinary.R` cobre a coluna ausente com subtotal exato e a recusa de um voto partidário adicional.

A prova real de AC2018 certificou governador (417.076) e senador (794.909), em 23 zonas, com diferenças zero. Os cargos proporcionais permaneceram explicitamente indisponíveis: existem votos nominais nas seções sem candidato correspondente no catálogo nominal anual (7.618 votos para federal e 16.926 para estadual), mas o detalhe publica zero nulos técnicos. O estadual também diverge em votos de legenda em algumas zonas. Excluir esses votos por aproximação ou assumir transferência para legenda não demonstra o fechamento oficial. O checkpoint registra `partial`; esses cargos não entram no cache certificado.

## Regra isolada de nulos totais de 2018

Uma investigação posterior demonstrou a semântica oficial necessária para AC2018. Os 6.412 votos do candidato estadual ausente 19019 coincidem exatamente com `QT_VOTOS_NOMINAIS_CONVR_LEG` do partido 19 nas 23 chaves município/zona. Os demais nominais excluídos entram em **nulos totais**, que já são diferentes dos nulos técnicos: federal `15.500 = 7.882 (código 96) + 7.618`; estadual `17.979 = 7.465 (código 96) + 16.926 - 6.412`. As identidades fecham também separadamente em todas as 46 chaves proporcionais.

`legal-historical-null-totals.R` implementa essa regra exclusivamente para 2018 em ambiente privado. Primeiro desconta as conversões já identificadas pelo catálogo nominal. A conversão oficial restante precisa ser zero ou corresponder integralmente ao conjunto nominal desconhecido daquele partido/município/zona; conversão parcial é recusada. O prefixo do número de urna determina somente o agrupamento partidário, sem inferir validade. Depois confere simultaneamente válidos, nominais válidos, legenda válida e nulos totais em cada zona. A última identidade é `nulos = código 96 + todos os votos nominais/partidários brutos - os votos certificados`, incluindo os nominais inválidos já identificados no catálogo e descontando as conversões preservadas como legenda.

Pendentes ou anulados positivos nas categorias oficiais de detalhe, e pendentes positivos nas fontes partidária ou nominal, bloqueiam conservadoramente essa regra; não são absorvidos em nulos. O método e as colunas de auditoria identificam `TOTAL NULL` e `null_total_*`, sem chamá-los de técnicos. As regras contemporâneas de 2022/2026 permanecem separadas. Os anos 2010/2014 também são recusados por este helper: seus campos de total válido proporcional precisam de ledger próprio.

O teste `test-historical-null-totals.R` verifica ZIP/CSV real do leitor de código 96, conversão conhecida mais desconhecida sem duplicação, conversão parcial recusada, três fontes de pendentes positivas recusadas, divergências de um voto e guardas de ano. O probe real isolado de AC2018 certificou os quatro cargos em 92 chaves cargo/zona, sem indisponibilidades: governador 417.076, senador 794.909, federal 417.502 e estadual 418.021 votos válidos. A legenda federal soma 26.987 e a estadual 35.627; os nulos totais proporcionais somam 15.500 e 17.979. Isso comprova AC; cada outra UF ainda precisa satisfazer seus próprios controles.

## Isolamento de disputas e Senado 2018

O runner de 2018 compõe o método acima com `historical-senate-nonvalid.R` e `historical-contest-isolation.R`. Cada combinação ordinária de eleição/turno/cargo é certificada de forma independente, reutilizando o cache bruto e uma única leitura agregada do código 96. Um cargo com conversão ambígua não impede publicar outros cargos demonstráveis da mesma UF. O certificado reúne somente disputas aprovadas; as demais constam em `unavailable`, com motivo. Se todas forem bloqueadas, o cache canônico anterior é preservado com nome diagnóstico `superseded` e substituído por um vazio de status `unreconciled`, que não pode ser publicado.

A extensão do Senado permite **somente** a identidade combinada `nulos + anulados = código 96 + nominais/legendas excluídos`, mantendo as contagens oficiais de nulos e anulados em colunas separadas. Ela não identifica a qual categoria judicial pertence cada candidato excluído e não chama componentes individuais de computados. Pendentes, apuração separada e anulados positivos fora do Senado continuam bloqueados. O método e os nomes `nonvalid_combined_*` expressam esse limite.

Probes reais separados comprovaram todas as zonas de AP (17), RR (16), MS (88) e MT (146). No Senado, os não válidos combinados são respectivamente 248.145 = 239.101 + 9.044; 56.567 = 46.066 + 10.501; 515.791 = 486.869 + 28.922; e 1.156.916 = 478.374 + 678.542. Válidos, nominais e legendas também fecharam em todas as zonas e todos os outros cargos desses probes.

As fixtures `test-historical-senate-nonvalid.R` e `test-historical-contest-isolation.R` cobrem diferença de um voto, componentes oficiais sem inferência individual, guardas de pendência/cargo, isolamento de disputa bloqueada, soma real dos votos contra certificado, restauração do ambiente privado e transição de certificado anterior para falha total. A repetição operacional usa uma lista explícita de UFs faltantes; certificados anteriores completos mantêm a proveniência da versão que os produziu.

Conversões de um subconjunto de candidatos ausentes continuam indisponíveis sem fonte oficial independente de destinação. Mesmo que um subconjunto de candidatos inteiros seja a única solução, distribuições parciais entre candidatos/locais podem preservar os mesmos totais de zona e alterar os centis. Não há fallback de busca de subconjunto por encaixe numérico.

## Destinações expressamente documentadas

`official-vote-destinations.json` é a tabela versionada de evidências primárias. Ela separa documentos (URL, data, caminho relativo de cache, bytes e SHA256) de mapeamentos por ano/UF/turno/cargo/número de urna/partido, destino expresso, total nominal e páginas. Os PDFs ficam no cache ignorado, sem incluir documentos externos no Git. O comando abaixo obtém somente os bytes exatos declarados; uma nova versão do documento exige revisão explícita da tabela, pois assinatura, tamanho e hash divergentes causam erro.

```sh
node shared-pipeline/eleicoes/historico/fetch-official-destinations.cjs
node shared-pipeline/eleicoes/historico/fetch-official-destinations.cjs --check-only
Rscript --vanilla shared-pipeline/eleicoes/historico/test-official-vote-destinations.R
```

O adaptador opt-in `official_destination_environment()` aplica somente destinações de candidatos ausentes da alocação nominal. Não sobrepõe uma alocação existente, exige o total integral do candidato na UF e recusa múltiplas identidades sob o número. As linhas explicitamente nulas e convertidas saem do pool de conversões desconhecidas; conversões documentadas preservam exatamente o vetor original de locais. A quantidade convertida continua sujeita aos campos explícitos partidários em cada município/zona e a todas as demais equações. O audit preserva `official_destination_sources`, filtrável por `year`, `uf`, `round`, `office`, com documento, páginas e hash. Não há inferência por situação de candidatura ou por busca de subconjuntos.

A evidência inicial é o relatório oficial TRE-RO de retotalização de 05/12/2022, referente a 2018: página 43 destina os 11.687 votos de 15800 a legenda; página 42 destina os 10.911 votos de 15000 a nulos; página 83 confirma os totais. PDF de 2.846.726 bytes, SHA256 `feb8696e8e62a9fd072693f1fc8aea50d2e0e69aff178ffeba5758d511e9b41f`. O probe real isolado comprovou todos os cinco recortes de RO em 56 zonas cada: estadual 744.799 válidos = 679.360 nominais + 65.439 legenda, sem divergências ou indisponibilidades.

A fixture demonstra que um encaixe numérico único (candidatos com 7 e 3 votos, conversão oficial 7) continua bloqueado sem documento. Só a destinação primária permite converter o candidato identificado; o teste também recusa total incorreto, reutilização de candidato explicitamente nulo, documento alterado, caminho fora do cache e identidades múltiplas. Esse mecanismo não libera AL nem outra disputa sem sua própria evidência primária.

## Agregados partidários repetidos em 2018

O `leiame.pdf` do [ZIP partidário oficial de 2018](https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_partido_munzona/votacao_partido_munzona_2018.zip), página 7, define as medidas explícitas de legenda e conversão no grão partido/cargo/município/zona. A agremiação é uma dimensão descritiva da candidatura. Em AM, partido 12 para deputado federal, existem 74 grupos com duas agremiações e os mesmos agregados de legenda: somar as duas linhas produz 14.096 votos, enquanto o agregado único e as seções têm 7.048. Em Apuí, município 2003/zona 67, ambas as linhas repetem legenda 24, mas os nominais são 10 e 0; deduplicar a tabela inteira seria incorreto.

O adaptador opt-in `historical-party-aggregates.R` projeta somente medidas de legenda/conversão, exigindo dependência funcional estrita dentro de cada partição de trânsito (`N`/`S`). Partições distintas permanecem aditivas, mesmo quando seus valores coincidem. Conflito de um voto entre descritores bloqueia o caso; não se escolhe a linha que coincide com as seções. As linhas nominais originais são mantidas separadas e suas pendências não podem ser mascaradas pela projeção. Todos os controles posteriores de direto/conversão/válidos/nulos permanecem obrigatórios.

Dicionário: SHA256 `6ed00027ba44c61bcefe531c52c763ab49190382d58c0bac5b47d1f15b3dbdf9`; ZIP MD5 `6d2796a2df286d350a0e6c0b7e4c79bd`. A fixture `test-historical-party-aggregates.R` cobre os dois trânsitos, nominais distintos preservados, conflito de um voto, pendentes e composição com o ledger. O helper recusa outros anos; o campo genérico de legenda de 2010/2014 não recebe essa interpretação.

## Código 97 e votos apurados separadamente em 2018

O `leiame.pdf` do [ZIP de seções BA2018](https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_secao/votacao_secao_2018_BA.zip), página 5, identifica 96 como nulo e 97 como anulado e apurado em separado. São categorias distintas. Dicionário de 192.420 bytes, SHA256 `fcd228746d87ef3509694de515e362aa12913afd1b2dc9ab2279bad31713f861`; ZIP MD5 `d6f4e931f4c8c95b29860e6c4d6fa57d`.

O adaptador opt-in `historical-separate-votes.R` agrega 96 e 97 em uma única leitura seletiva. Exige que o 97 observado coincida exatamente com `QT_VOTOS_ANULADOS_APU_SEP` em cada município/zona, preservando campos separados na auditoria e seu resumo. Nunca adiciona 97 aos nulos nem aos anulados ordinários. As equações de válidos/nominais/legenda/nulos(+anulados) continuam independentes, e pendentes permanecem bloqueados.

A leitura restringe explicitamente `ANO_ELEICAO`, UF, cargos e IDs de eleição. A extração CSV recebe uma marca com o MD5 do ZIP: arquivo ausente, marca ausente ou arquivo ZIP substituído causam nova extração verificada, mesmo quando o nome permanece igual. A fixture inclui outro ano com o mesmo ID eleitoral e substitui o conteúdo do ZIP sob o mesmo caminho, comprovando ambos os controles.

A investigação de BA identificou somente as seções 328/local 1376 e 202/local 1236, Salvador 38490/zona 18. Elas são integralmente 97, sem nominais, legenda ou 96. São 383 + 363 = 746 votos para governador/federal/estadual e 766 + 726 = 1.492 para senador, coincidentes com APU_SEP. Isso oferece identificação direta da exclusão, sem rateio. A fixture `test-historical-separate-votes.R` usa ZIP/CSV real pequeno, separa as categorias, recusa diferenças de um voto e pendentes positivos e verifica a composição com o adaptador partidário. A interpretação fica restrita ao dicionário de 2018.

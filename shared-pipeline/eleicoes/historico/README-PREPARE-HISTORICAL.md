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

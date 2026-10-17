# Referência censitária de 2010: preparação e equivalência de renda

Conferência de 16/10/2026. A opção Censo 2010 ainda não está integrada ao
explorador: as séries históricas já publicadas usam a referência de renda
do Censo 2022, como informado em seus metadados. Este documento registra a
preparação da segunda referência, sem afirmar que foi publicada.

## Fonte e variáveis conferidas

A [documentação oficial do censobr](https://ipea.github.io/censobr/reference/read_tracts.html)
oferece os agregados de 2010, incluindo `Basico` e `ResponsavelRenda`.
Na versão instalada 0.6.0.902, `data_dictionary(year=2010, dataset='tracts')`
baixou o [dicionário original de setores de 2010](https://github.com/ipea/censobr_prep_data/releases/download/censo_docs/2010_dictionary_tracts.pdf).
PDF com 201 páginas; MD5 `4efa5dd8a31361364a1aeda9d9e03dc1`.
As páginas físicas 43 e 44 foram extraídas e inspecionadas visualmente.
Cache ignorado: `data-raw/eleicoes-renda/dicionario-setores-2010.pdf`.

| Conceito | Censo 2010, arquivo Basico | Censo 2022, ResponsavelRenda |
|---|---|---|
| Média nominal mensal dos responsáveis com rendimento | `V007`, página 44 | `V06004` |
| Média dos responsáveis com e sem rendimento | `V005`, página 44 | Não usar como substituta de V06004 |
| Domicílios particulares permanentes ou responsáveis por esses domicílios | `V001`, página 43 | `V06001`, responsáveis em domicílios particulares permanentes ocupados |
| Situação urbana/rural | Códigos 1–3 urbanos e 4–8 rurais, página 43 | Atributos próprios da base de 2022 |

`V009` e `V011` de Basico referem-se às pessoas de dez anos ou mais, outro
universo; não substituem a renda dos responsáveis. A correspondência proposta
para ordenar renda é V007–V06004, mantendo a diferença de vintage explícita.
Antes de harmonizar pesos, conferir os universos e a ocupação dos domicílios
nas respectivas notas técnicas; a descrição de V001 não é idêntica à de V06001.

## Integração a realizar

- Ler Basico 2010 com `censobr`, preservando códigos, sigilo e ausências;
  conferir V007 contra a fonte original e registrar o hash do download.
- Usar malhas setoriais e municipais de 2010 para essa referência. Não unir
  códigos setoriais de 2010 diretamente aos de 2022 ou reaproveitar os seus
  pontos internos em cache.
- Manter as chaves eleitorais e datas originais dos locais. Municípios criados
  ou alterados entre 2010 e o pleito exigem verificação espacial/correspondência
  territorial explícita; não sobrescrever o município informado pelo TSE.
- Recalcular os percentis nacionais e estaduais no universo do Censo escolhido
  antes das exclusões espaciais, e identificar essa referência no seletor,
  no eixo, no tooltip e nos metadados.
- Registrar cobertura de setores e de votos para cada combinação de pleito e
  Censo. Comparar curvas em setores observados em comum para distinguir
  alteração de referência territorial de alteração na composição da cobertura.
- Se exibir salários mínimos ou valores reais, vincular a unidade ao período
  censitário correto e documentar a fonte da conversão. O ranking nominal não
  depende da divisão por uma constante comum; não reutilizar R$1.212 em 2010.

Os centis de censos diferentes são posições em distribuições distintas,
não acompanhamento das mesmas famílias. A ligação continua aproximando
locais próximos de setores; nenhuma opção revela renda individual do eleitor
ou identifica onde os moradores de cada setor efetivamente votaram.

# Driver isolado de 2014 e 2010

`prepare-legacy-all-ufs.R` usa os helpers independentes de
`agregacao/validacao/legal-2010-2014.R` e
`agregacao/validacao/legacy-2010-2014-environment.R`. Não modifica nem importa
o runner de 2018, não baixa arquivos, não exporta gráficos e não altera o
manifesto do explorador. Os dois argumentos posicionais são obrigatórios:
anos explícitos e lista explícita de UFs. O plano ordena 2014 antes de 2010.

```sh
Rscript --vanilla shared-pipeline/eleicoes/historico/test-prepare-legacy.R
Rscript --vanilla shared-pipeline/eleicoes/historico/prepare-legacy-all-ufs.R 2014 AC --dry-run
Rscript --vanilla shared-pipeline/eleicoes/historico/prepare-legacy-all-ufs.R 2014 AC
Rscript --vanilla shared-pipeline/eleicoes/historico/prepare-legacy-all-ufs.R 2014,2010 AC,DF
```

O cache de entrada padrão é `data-raw/eleicoes-renda/historico`. Cada ano/UF
precisa ter o ZIP de seções estadual e os três ZIPs oficiais nacionais de
candidatos, partidos e detalhe município/zona já baixados. Somente membros
da UF são extraídos. `--input-cache DIRETORIO` muda a origem, e
`--cache-dir DIRETORIO` muda a saída, que por padrão é o subdiretório
**`legacy-canonic`** do cache de entrada. Entrada e saída iguais são
recusadas. O cache principal de votos legais e os dados publicados do
explorador permanecem independentes.

Cada UF roda em subprocesso R próprio com dois threads de data.table,
liberando sua memória antes da próxima. Não lance esse processamento junto
de um cruzamento nacional que já esteja pressionando a RAM. Arquivos grandes
são lidos apenas quando um job real é iniciado; dry-run e fixtures não leem
tabelas reais. O driver não remove extrações ou fontes.

## Certificação e falhas

Antes de ler as fontes, o driver afasta um certificado anterior para
`legacy-canonic/superseded/*.snapshot`, fora do padrão de descoberta de
caches legais. Falha de parser, fonte ausente ou mudança de snapshot deixa
o nome canônico ausente; não restaura automaticamente um certificado antigo.

O controlador também faz essa quarentena **antes de lançar o subprocesso**,
para cobrir erros de sintaxe ou de inicialização que impeçam o worker de
começar. Exige saída zero, identidade nova da execução e fingerprints atuais
no checkpoint devolvido; um arquivo `complete` antigo não basta. Checkpoints
ativos registram PID. Um PID vivo, uma consulta de processo inconclusiva ou
um `running` antigo sem PID bloqueiam a retomada. Nesse caso, o operador
(agente ou pessoa) deve comprovar a ausência do processo e arquivar o
checkpoint interrompido, preservando staging e fontes, antes de repetir.

O helper opera em modo diagnóstico, em staging específico da execução.
Antes de instalar o RDS no cache isolado, o driver exige:

- identidade dos ZIPs e do código antes/depois da execução e MD5 dos CSVs
  efetivamente usados, iguais aos registrados pelo helper;
- escopo solicitado e partição exata de todas as disputas observadas em
  bruto/detalhe entre certificados e indisponíveis, sem duplicação,
  sobreposição, ausência ou recorte inesperado;
- denominadores do RDS iguais aos totais certificados por disputa, sem
  especiais ou votos fora do escopo;
- nominal oficial integral, legenda final identificável somente nos extremos
  zero/pool integral da zona e todos os ledgers exatos do helper;
- staging serializado consistente antes da instalação no cache isolado.

O campo genérico `QT_VOTOS_LEGENDA` nunca identifica conversões; funciona
somente como controle de compatibilidade. Destinações parciais, pendentes,
anulados e código 97 positivo continuam bloqueados. Disputas bloqueadas
ficam em `unavailable`, com motivo; não impedem certificar outras disputas.

Uma zona observada com zero válidos pode permanecer no ledger de uma disputa
com votos válidos. Uma disputa **inteira** com zero válidos não permite
porcentagem: vira indisponível, sem votos publicados, e seus checks exatos
ficam em `zero_valid_diagnostics`. O staging preserva o diagnóstico e o CSV
com ledger por zona. Se todas as disputas estiverem indisponíveis, o RDS
canônico é vazio, com status `unavailable` e `publishable=FALSE`; o checkpoint
é `partial`, nunca `complete` ou `reconciled`.

## Retomada e saídas

Checkpoints e logs ficam em `legacy-canonic/status/ANO/UF.{json,log}`.
Os fingerprints incluem código, quatro ZIPs, três membros CSV utilizados e
RDS final. Complete/partial são retomados somente enquanto os fingerprints
coincidirem. Use `--rebuild` com lista explícita de UFs para repetir parciais.

O nome final é `places-ANO-UF-3_5_6_7_8-legal.rds`, dentro do cache isolado.
Staging guarda o diagnóstico original, checks por zona/candidato/partido,
CSV selecionado e cópia certificada. A publicação no explorador exige um
passo separado de revisão e exportação; este driver não a executa.

Subprocesso que conclui complete/partial termina com código zero; falha
técnica termina com um. O processo coordenador continua as UFs restantes e
termina com zero para plano completo, dois quando há somente parciais e um
quando existe falha técnica. Dry-run termina com zero.

`test-prepare-legacy.R` utiliza somente fontes sintéticas. Cobre staging,
partição, denominadores, complete/partial, zeros, certificado anterior,
parser/fonte ausente, ZIP ou CSV alterado e repasse real dos caminhos para
um subprocesso mínimo, incluindo diretórios com espaços.

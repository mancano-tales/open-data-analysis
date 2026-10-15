# Test the composed private helper against the existing complete legal fixture.
# No production processing, downloads or changes to the active batch.
source("shared-pipeline/eleicoes/agregacao/resume-2026-optimized.R")
test_env <- new.env(parent = globalenv())
# The existing fixture resolves this binding when it creates its helper.
test_env$legal_party_technical_environment <- legal_optimized_environment
sys.source("shared-pipeline/eleicoes/agregacao/validacao/test-legal-party-wrapper.R", envir = test_env)
stopifnot(grepl("legal_group_candidates_fast",
  paste(deparse(body(test_env$env$legal_reconcile_places)), collapse = " ")))
plan <- eleicao_resume_optimized(dry_run = TRUE)
stopifnot(identical(plan$uf, c("MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF")),
          all(plan$status == "planned"), nrow(plan) == 11L)
message("PASS: composed fast candidate/technical-party helper, complete legal checks and sequential resumption dry run.")

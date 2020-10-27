.PHONY: tests tests-watch


tests: ## Run the tests with cabal
	@cabal test --flags=dev

tests-watch: ## Run the tests with ghcid, re-running each time the source files change
	ghcid \
		--command="cabal repl --flags=dev --repl-options=-isrc --repl-options=-itest obfuscate:obfuscate-hs-test" \
		--reload="src" \
		--test="Main.main"

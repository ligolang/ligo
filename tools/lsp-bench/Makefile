# examples: make bench filter="References/simple" ; make bench filter="Complex benchmarks/one_big_file.mligo/keystrokes"
# You can bench another executable, e.g. LIGO_LSP_TEST_EXE="ligo-lsp-exe" make bench

bench:
	git submodule update --init --recursive
	bash -c "cd projects/submodules/checker; cd -"
	$(BUILD_FLAGS) stack run -- "-v2" "$(filter)"

bench-no-logging:
	LIGO_LSP_TEST_EXE="ligo lsp --disable-lsp-requests-logging" make bench

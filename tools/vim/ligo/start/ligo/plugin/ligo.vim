" THIS FILE WAS AUTOMATICALLY GENERATED. DO NOT MODIFY MANUALLY OR YOUR CHANGES WILL BE LOST.
if executable('ligo')
  if !exists("autocommands_loaded")
    let autocommands_loaded=1
    augroup ligoRegisterLanguageServer
      autocmd User lsp_setup
          \ call lsp#register_server({
          \   'name': 'ligo_lsp',
          \   'cmd': {server_info->['ligo', 'lsp', 'all-capabilities']},
          \   'allowlist': ['jsligo', 'mligo'],
          \ })
    augroup END
  endif
endif

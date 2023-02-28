" THIS FILE WAS AUTOMATICALLY GENERATED. DO NOT MODIFY MANUALLY OR YOUR CHANGES WILL BE LOST.
if executable('ligo-squirrel')
  if !exists("autocommands_loaded")
    let autocommands_loaded=1
    augroup ligoRegisterLanguageServer
      autocmd User lsp_setup
          \ call lsp#register_server({
          \   'name': 'ligo_lsp',
          \   'cmd': {server_info->['ligo-squirrel']},
          \   'allowlist': ['ligo', 'mligo'],
          \ })
    augroup END
  endif
endif

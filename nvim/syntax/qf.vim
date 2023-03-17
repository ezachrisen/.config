if exists('b:current_syntax')
    finish
endif


" syn match qfFail "^--- FAIL"
syn match qfFileName ".*\.*go" " [^│]*go"
syn match qfNote "^[^-│]*: "
syn match qfLineNr " [0123456789].*:[0-9]* "
syn match qfSeparator "│"
syn match qfFail "^FAIL\t"
syn match qfPass "^PASS"
hi def link qfFileName DiagnosticError 
hi def link qfSeparator Delimiter
hi def link qfLineNr LineNr
hi def link qfError DiagnosticError
hi def link qfWarning DiagnosticWarn
hi def link qfInfo DiagnosticInfo
hi def link qfNote DiagnosticHint
hi def link qfFail ErrorMsg 
hi def link qfPass DiffAdd
let b:current_syntax = 'qf'

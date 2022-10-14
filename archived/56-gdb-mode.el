(require 'gdb)

(add-hook 'gdb-mode-hook
    (lambda ()
        (gud-def gud-break  "break %d/%f:%l"  "\C-b" "Set breakpoint at current line.")
        (gud-def gud-tbreak "tbreak %d/%f:%l" "\C-t" "Set temporary breakpoint at current line.")
        (gud-def gud-remove "clear %d/%f:%l" "\C-d" "Remove breakpoint at current line.")
        ))

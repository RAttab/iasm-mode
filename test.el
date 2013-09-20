(setq debug-on-error t)
(progn
  (load-file "~/code/iasm-mode/iasm-mode-2.el")
  (iasm-disasm "~/code/lockless/bin/tls_perf_test"))

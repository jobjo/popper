; This file is generated

; Expect test for `discard`
(executable
 (name discard)
 (modules discard)
 (libraries clean)
 (preprocess
  (pps ppx_deriving.show ppx_deriving.ord ppx_deriving_popper)))

(rule
 (with-stdout-to
  discard.output
  (run ./discard.exe)))

(rule
 (alias runtest)
 (action
  (diff discard.expected discard.output)))

; Expect test for `failing_suite`
(executable
 (name failing_suite)
 (modules failing_suite)
 (libraries clean)
 (preprocess
  (pps ppx_deriving.show ppx_deriving.ord ppx_deriving_popper)))

(rule
 (with-stdout-to
  failing_suite.output
  (run ./failing_suite.exe)))

(rule
 (alias runtest)
 (action
  (diff failing_suite.expected failing_suite.output)))

; Expect test for `passing_suite`
(executable
 (name passing_suite)
 (modules passing_suite)
 (libraries clean)
 (preprocess
  (pps ppx_deriving.show ppx_deriving.ord ppx_deriving_popper)))

(rule
 (with-stdout-to
  passing_suite.output
  (run ./passing_suite.exe)))

(rule
 (alias runtest)
 (action
  (diff passing_suite.expected passing_suite.output)))

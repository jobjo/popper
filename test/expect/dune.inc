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

; Expect test for `exception`
(executable
 (name exception)
 (modules exception)
 (libraries clean)
 (preprocess
  (pps ppx_deriving.show ppx_deriving.ord ppx_deriving_popper)))

(rule
 (with-stdout-to
  exception.output
  (run ./exception.exe)))

(rule
 (alias runtest)
 (action
  (diff exception.expected exception.output)))

; Expect test for `fail_suite`
(executable
 (name fail_suite)
 (modules fail_suite)
 (libraries clean)
 (preprocess
  (pps ppx_deriving.show ppx_deriving.ord ppx_deriving_popper)))

(rule
 (with-stdout-to
  fail_suite.output
  (run ./fail_suite.exe)))

(rule
 (alias runtest)
 (action
  (diff fail_suite.expected fail_suite.output)))

; Expect test for `fail_uchar`
(executable
 (name fail_uchar)
 (modules fail_uchar)
 (libraries clean)
 (preprocess
  (pps ppx_deriving.show ppx_deriving.ord ppx_deriving_popper)))

(rule
 (with-stdout-to
  fail_uchar.output
  (run ./fail_uchar.exe)))

(rule
 (alias runtest)
 (action
  (diff fail_uchar.expected fail_uchar.output)))

; Expect test for `fail_with_location`
(executable
 (name fail_with_location)
 (modules fail_with_location)
 (libraries clean)
 (preprocess
  (pps ppx_deriving.show ppx_deriving.ord ppx_deriving_popper)))

(rule
 (with-stdout-to
  fail_with_location.output
  (run ./fail_with_location.exe)))

(rule
 (alias runtest)
 (action
  (diff fail_with_location.expected fail_with_location.output)))

; Expect test for `fail_with_log`
(executable
 (name fail_with_log)
 (modules fail_with_log)
 (libraries clean)
 (preprocess
  (pps ppx_deriving.show ppx_deriving.ord ppx_deriving_popper)))

(rule
 (with-stdout-to
  fail_with_log.output
  (run ./fail_with_log.exe)))

(rule
 (alias runtest)
 (action
  (diff fail_with_log.expected fail_with_log.output)))

; Expect test for `fail_with_log_and_location`
(executable
 (name fail_with_log_and_location)
 (modules fail_with_log_and_location)
 (libraries clean)
 (preprocess
  (pps ppx_deriving.show ppx_deriving.ord ppx_deriving_popper)))

(rule
 (with-stdout-to
  fail_with_log_and_location.output
  (run ./fail_with_log_and_location.exe)))

(rule
 (alias runtest)
 (action
  (diff fail_with_log_and_location.expected fail_with_log_and_location.output)))

; Expect test for `fail_with_logs_and_location`
(executable
 (name fail_with_logs_and_location)
 (modules fail_with_logs_and_location)
 (libraries clean)
 (preprocess
  (pps ppx_deriving.show ppx_deriving.ord ppx_deriving_popper)))

(rule
 (with-stdout-to
  fail_with_logs_and_location.output
  (run ./fail_with_logs_and_location.exe)))

(rule
 (alias runtest)
 (action
  (diff fail_with_logs_and_location.expected fail_with_logs_and_location.output)))

; Expect test for `pass_single`
(executable
 (name pass_single)
 (modules pass_single)
 (libraries clean)
 (preprocess
  (pps ppx_deriving.show ppx_deriving.ord ppx_deriving_popper)))

(rule
 (with-stdout-to
  pass_single.output
  (run ./pass_single.exe)))

(rule
 (alias runtest)
 (action
  (diff pass_single.expected pass_single.output)))

; Expect test for `pass_suite`
(executable
 (name pass_suite)
 (modules pass_suite)
 (libraries clean)
 (preprocess
  (pps ppx_deriving.show ppx_deriving.ord ppx_deriving_popper)))

(rule
 (with-stdout-to
  pass_suite.output
  (run ./pass_suite.exe)))

(rule
 (alias runtest)
 (action
  (diff pass_suite.expected pass_suite.output)))

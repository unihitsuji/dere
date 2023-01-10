;#!
;;; -*- coding: utf-8 -*-
(use private.bas)
(use dere.st)
(use dere.st-data)
(use dere.units.basic-guest)
(use dere.units.basic)
(use dere.units.grand)
(use dere.units.mizuki-guest)
(use dere.units.mizuki)

(use gauche.dictionary)

;;; ロガー
(use gauche.logger)
(log-open "C:\\Users\\mogeba\\__desks__\\clones\\__mine__\\dere\\st-test.log"
          :prefix "~Y ~T ~P[~$]: ")
(log-format "~A~%" (make-string 60 #\-))
(log-format "(command-line) ~S / *program-name* ~S / *argv* ~S~%"
            (command-line)
            *program-name*
            *argv*)

(define (data . args)
  (format #t "~S~%" (sort (dict-keys (module-table (find-module 'dere.st-data))) string<? symbol->string))
  (format #t "~S~%" (symbols-from 'dere.st-data))
  (format #t "~S~%"
          (module-imports (find-module 'dere.st-data)))
  (format #t "~S~%" (sort (all-modules) string<? (lambda (x) (symbol->string (module-name x))))))

(define (format-symbols port mod)
  (format port "          ~A~%" mod)
  (for-each
   (lambda (x) (format port "~A~%" x))
   (wrap-right-limit 70 12 (symbols-from mod))))

(define (help . args)
  (formats
   (current-error-port)
   #""
   #"USAGE"
   #"  $ gosh ~(car args)"
   #"      下記のコマンド exam と同じ"
   #""
   #"  $ gosh ~(car args) コマンド"
   #"      exam: 単体テストを行う"
   #"      help: 本ヘルプを表示する"
   #"      hbar-top: Starry-Go-Round PIANO のトップランカーの編成の発動チャート"
   #"      hbar-ore: おれの編成の発動チャート"
   #""
   #"  $ gosh ~(car args) コマンド 引数 ..."
   #""
   #"      hbar-spec sec spec-string1 [ spec-string2 ... ]"
   #"        秒数 sec の曲で 指定した spec-string の編成で発動チャート"
   #""
   #"      hbar-desc sec desc-string1 [ desc-string2 ... ]"
   #"        秒数 sec の曲で 指定した desc-string の編成で発動チャート"
   #"        desc-string は dere/st-data.scm に記述された"
   #"        以下のシンボルが使用できる")
  (format-symbols (current-error-port) 'dere.units.basic-guest)
  (format-symbols (current-error-port) 'dere.units.basic)
  (format-symbols (current-error-port) 'dere.units.grand)
  (format-symbols (current-error-port) 'dere.units.mizuki-guest)
  (format-symbols (current-error-port) 'dere.units.mizuki)
  (format-symbols (current-error-port) 'dere.st-data))


(define (exam/string->spec . args)
  (unless (eq? exam (cadr args))
    (use gauche.test)
    (test-start "test for dere.st")
    (test-module 'dere.st))
  (test-section "string->spec")
  (test "0-01"  '(0  6.0 中 わずか) (^[] (string->spec "6中わずか")))
  (test "0-02"  '(0  6.0 中 わずか) (^[] (string->spec "06中わずか")))
  (test "0-03"  '(0 11.0 高 しばらく) (^[] (string->spec "11高しばらく")))
  (test "0-04"  '(0 11.0 高 しばらく) (^[] (string->spec "011高しばらく")))
  (test "0-05"  '(A 11.0 高 しばらく) (^[] (string->spec "A11高しばらく")))
  (test "0-06"  '(B  6.0 中 わずか) (^[] (string->spec "B6中わずか")))
  (test "0-07"  '(C 13.0 高 かなり) (^[] (string->spec "C13高かなり")))
  (unless (eq? exam (cadr args))
    (test-end)))

(define (exam . args)
  ;;; 単体テスト
  ;;; https://practical-scheme.net/gauche/man/gauche-refj/Dan-Ti-tesuto.html
  (use gauche.test)
  (test-start "test for dere.st")
  (test-module 'dere.st)
  ;;; 実際のテスト
  (apply exam/string->spec args)
  (test "test 0-08"  '(6 10.5) (^[] (act? 6 '((6 10.5) (12 16.5)))))
  (test "test 0-09"  #f        (^[] (act? 10.5 '((6 10.5) (12 16.5)))))
  (test "test 0-10"  #f        (^[] (act? 11.5 '((6 10.5) (12 16.5)))))
  (test "test 0-11"  '(0.0 0.5) (^[] (steps 0 1 0.5)))
  (test "test 0-12"  '(0.0 0.5 1.0) (^[] (steps 0 1.1 0.5)))
  (test "test 0-13"
        '((6.0 10.5) (12.0 16.5) (18.0 22.5) (24.0 28.5))
        (^[] (spec->covers 30 (string->spec "6中わずか"))))
  (test "test 0-14"
        '((6.0 10.5) (12.0 16.5) (18.0 22.5) (24.0 28.0))
        (^[] (spec->covers 28 (string->spec "6中わずか"))))
  (test "test 0-15"  "  *  *" (^[] (hbar 3 '((1.0 1.5) (2.5 3.0)))))
  (test "test 0-16"  "  *= *" (^[] (hbar 3 '((1.0 2.0) (2.5 3.0)))))
  (test "test 0-17"  "  *= *" (^[] (hbar 3 '((1.0 2.0) (2.5 3.5)))))
  (test "test 0-18"  "|---------+---------|-" (^[] (line 11)))
  (test "test 0-19"
        '(("A12中少し" "アンコール" "裕美"))
        (^[] (nusk-filter encore? A09BR-B06U-C13U)))
  (test "test 0-20"
        '(("B18中かなり" "アンコール" "つかさ")
          ("B18中かなり" "アンコール" "ゆっこ")
          ("B18中かなり" "アンコール" "フレ")
          ("B18中かなり" "アンコール" "麗奈")
          ("C12中少し" "アンコール" "あかり")
          ("C12中少し" "アンコール" "凛")
          ("C12中少し" "アンコール" "歌鈴")
          ("C12中少し" "アンコール" "ナターリア"))
        (^[] (nusk-filter encore? A09R-B18ES-C12ES)))
  (test "test 0-21" '()          (^[] (act-before  5 '((6.0 10.5) (12.0 16.5)))))
  (test "test 0-22" '( 6.0 10.5) (^[] (act-before  7 '((6.0 10.5) (12.0 16.5)))))
  (test "test 0-22" '(12.0 16.5) (^[] (act-before 17 '((6.0 10.5) (12.0 16.5)))))
  (test-end))

(define (hbar-top . args) (hbars 128.5 A09BR-B06U-C13U))
(define (hbar-ore . args) (hbars 128.5 A09R*-B18ES*-C12ES*))

(define (hbar-spec . args)
  (hbars
   (caddr args)
   (map
    (lambda (x)
      (if (string=? x "()") () (list x)))
    (cdddr args))))

(define (hbar-desc . args)
  (hbars
   (caddr args)
   (concatenate
    (map
     (lambda (desc)
       (cond
        ((string=? desc "()") '(()))
        (else (log-format "~S~%" (string->x desc))
              (string->x desc))))
     (cdddr args)))))

(define (main argv) (invoke argv :proc exam))
(log-format "st-test.scm done.~%")

;;; gle-mode.el --- Major mode to edit Graphics Layout Engine files  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2022  Free Software Foundation, Inc

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Package-Requires: ((cl-lib "0.5"))
;; Version: 1.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major mode for files using the GLE (Graphics Layout Engine)
;; language.  See http://glx.sourceforge.net/
;; [ Apparently the site uses "glx" while everything else seems to use
;;   "gle" instead, because "gle" was already occupied in sourceforge.  ]

;; It provides:
;; - Code highlighting
;; - Automatic indentation
;; - Flymake support (requires Emacs-26's fymake)
;; - Imenu support
;; - Electric bloc names (after begin/end)
;; - Completion of bloc names
;; - Skeletons/templates to insert or close blocs

;; If you have misspelled a keyword or a command (etc.),
;; it should jump into your eyes because it is not highlighted.

;;;; Known defects:

;; - `print' is not highlighted in:
;;
;;     if a==0 then print 0
;;     else if a==1 then print 1
;;     else print 2
;;
;; - The `gle--line-syntax' description of the GLE language was extracted
;;   by hand from the doc and it is probably incomplete, and it is hard
;;   to update.

;;;; Contributors:
;; Andrey Grozin <A.G.Grozin@inp.nsk.su> (2022-11-07)
;; provided the data to build the syntax description from which
;; the highlighting works.

;;;; TODO
;; - provide more completion

;;; Code:

(require 'smie)
(require 'cl-lib)

(defgroup gle-mode ()
  "Major mode for GLE (Graphics Layout Engine) files."
  :group 'tools)

;;;; Syntax table

(defvar gle-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Turn all symbol-constituent syntax into punctuation.
    (map-char-table (lambda (c v)
                      (and (equal v '(3))
                           (set-char-table-range st c '(1))))
                    st)
    ;; Make `_' into a symbol-constituent again.
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?! "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    ;; Backslash isn't used to escape a double quote in a string.
    (modify-syntax-entry ?\\ "." st)
    st))

(defconst gle-syntax-propertize
  (syntax-propertize-rules
   ;; The doc says that doubled quotes are used to escape quotes in a string,
   ;; tho running gle-4.2.5 on those strings gives me errors :-(
   ("\"\"\\|''"
    (0 (if (save-excursion (nth 3 (syntax-ppss (match-beginning 0))))
           (string-to-syntax ".")
         ;; If match-beg is not within a string, maybe it starts a string,
         ;; and maybe the second " doesn't end the string!
         (goto-char (1+ (match-beginning 0)))
         nil)))
   ;; Abuse the syntax-propertize scan to mark those places in the buffer
   ;; where we have a bloc name, to speed up the gle--before-change-function.
   ("\\(?:begin\\|end\\)[ \t]+\\(\\sw+\\)"
    (1 (prog1 nil
         (put-text-property (match-beginning 1) (match-end 1)
                            'gle-block-name t))))))

;;;; General tables about GLE's syntax

(defvar gle--bloc-names
  ;; Extracted with:
  ;;     sed -ne 's/\(^\|.*\\sf \)begin \([[:alnum:]]*\).*/\2/p' \
  ;;         **/*.tex | sort -u
  '("box" "clip" "config" "contour" "fitz" "graph" "key" "length" "letz"
    "name" "object" "origin" "path" "rotate" "scale" "surface"
    "table" "tex" "texpreamble" "text" "translate"))

;;;; SMIE support

(defvar gle-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((var)
       (exp)
       (inst-else-inst (inst) (inst "else bloc" inst))
       (for-body (exp ";" inst))
       (var=exp (var "=" exp))
       (for-head (var=exp "to" exp-step))
       (exp-step (exp) (exp "step" exp))
       (until-body (exp ";" inst))
       (inst (inst ";" inst)
             ("begin" inst "end <thing>")
             ;; You can have "single-line" ifs (with inst right after "then"),
             ;; which can be extended with single line "else if"s.
             ;; Or you can have "if ... end if" blocs.
             ("if bloc" inst-else-inst "end <thing>")
             ("sub" inst "end <thing>")
             ("for" for-body "next <var>")
             ("until" until-body "next")
             ("while" until-body "next")
             ("gsave" inst "grestore")))
     '((assoc ";"))))))

(defun gle-smie--disambiguate-if ()
  "Expects point to be right after `if`."
  (save-excursion
    (let ((eol (line-end-position))
          (then nil))
      (while (progn
               (forward-comment (point-max))
               (and (< (point) eol)
                    (not (setq then (looking-at "then\\_>")))))
        (or (> (skip-syntax-forward "w_") 0)
            (> (skip-syntax-forward ".") 0)
            (forward-char 1)))
      (if then
          "if bloc"
        "if line"))))

(defun gle-smie-forward-token ()
  (let ((start (point)))
    (forward-comment (point-max))
    (if (and (not (eq ?\n (char-before start)))
             (< start (line-beginning-position)))
        (progn (goto-char start)
               (forward-line 1)
               ";")
      (let ((bolp (save-excursion
                    (skip-chars-backward " \t")
                    (memq (char-before) '(nil ?\n ?\;)))))
        (if (not bolp)
            (cond
             ((not (zerop (skip-chars-forward "^ \t\n;!"))) "<exp>")
             ((eobp) "")
             (t (cl-assert (looking-at ";"))
                (forward-char 1)
                ";"))
          (let ((tok (buffer-substring (point)
                                       (progn (skip-chars-forward "^ \t\n;!")
                                              (point)))))
            (cond
             ((looking-at "[ \t]*=") "<var>")
             ((equal tok "end")
              (cond
               ;; ((looking-at "[ \t]+sub") (goto-char (match-end 0)) "end sub")
               ;; ((looking-at "[ \t]+if") (goto-char (match-end 0)) "end if")
               ((looking-at "[ \t]+\\w+")
                (goto-char (match-end 0)) "end <thing>")
               (t tok)))
             ((equal tok "next")
              (cond
               ((looking-at "[ \t]+\\w+")
                (goto-char (match-end 0)) "next <var>")
               (t tok)))
             ((equal tok "if") (gle-smie--disambiguate-if))
             ((equal tok "else")
              (if (looking-at "[ \t]+if") "else line" "else bloc"))
             (t tok))))))))
              
(defun gle-smie-backward-token ()
  (let ((start (point)))
    (forward-comment (- (point)))
    (if (> start (line-end-position))
        ";"
      (let* ((end (point))
             (assign (looking-at "[ \t]*="))
             (tok (buffer-substring (progn (skip-chars-backward "^ \t\n;")
                                           (point))
                                    end))
             (bolp (save-excursion
                     (skip-chars-backward " \t")
                     (memq (char-before) '(nil ?\n ?\;)))))
        (cond
         ((and bolp (equal tok ""))
          (if (bobp) tok
            (cl-assert (eq (char-before) ?\;))
            (forward-char -1)
            ";"))
         (assign "<var>")
         (bolp
          (cond
           ((equal tok "if")
            (save-excursion (forward-char 2) (gle-smie--disambiguate-if)))
           ((equal tok "else")
            (if (looking-at "else[ \t]+if") "else line" "else bloc"))
           (t tok)))
         ((save-excursion
            (skip-chars-backward " \t")
            (and (memq (char-before) '(?d ?t))
                 (looking-back "^[ \t]*\\(end\\|nex\\(t\\)\\)"
                               (line-beginning-position))))
          (goto-char (match-beginning 1))
          (cond
           ((match-beginning 2) "next <var>")
           ;; ((equal tok "sub") "end sub")
           ;; ((equal tok "if") "end if")
           (t "end <thing>")))
         (t "<exp>"))))))

(defun gle-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:after . ";")
     (cond
      ((smie-rule-parent-p "for" "while" "until" "sub" "begin" "gsave"
                           "if bloc")
       (smie-rule-parent smie-indent-basic))))
    (`(:before . "else bloc") (smie-rule-parent 0))))

;;;; Font-lock

(defvar gle--line-syntax
  (let ((blocnames
         ;; Extracted with:
         ;;     sed -ne 's/\(^\|.*\\sf \)begin \([[:alnum:]]*\).*/\2/p' \
         ;;         **/*.tex | sort -u
         '(font-lock-builtin-face
           ("box" any ("fill") ("add") ("nobox") ("name") ("round"))
           ("path" any ("fill") ("stroke") ("clip"))
           ("clip") ("config") ("contour") ("fitz") ("graph") ("key")
           ("length") ("letz") ("name") ("object") ("origin")
           ("rotate") ("scale") ("surface") ("table") ("tex") ("texpreamble")
           ("text" ("width")) ("translate")))
        (bitmaps '(font-lock-constant-face ("png") ("jpeg") ("gif") ("tiff")))
        (arcends '(font-lock-constant-face ("start") ("end") ("both"))))
    `(font-lock-keyword-face
      ("begin" ,@blocnames)
      ("end" ("if") ("sub")
             ,@(mapcar (lambda (x) (if (consp x) (list (car x)) x)) blocnames))
      ("if" any ("then"))               ;FIXME: What can come after `then'?
      ("else" ("if" any ("then")))      ;FIXME: What can come after `else'?
      ("for" any ("to") ("step"))
      ("next")
      ("sub") ;; font-lock-function-name-face ("[[:alpha:]][[:alnum:]_]*")
      ("next")
      ("local")
      ("return")
      font-lock-preprocessor-face
      ("include") ;; Arg is a string, so we don't highlight it specially
      font-lock-builtin-face
      ("define" ("marker"))
      ("bitmap"      any ("type" ,@bitmaps))
      ("bitmap_info" any ("type" ,@bitmaps))
      ("orientation" font-lock-constant-face ("portrait") ("landscape"))
      ("papersize" font-lock-constant-face
                   ("a0paper") ("a1paper") ("a2paper") ("a3paper") ("a4paper")
                   ("letterpaper"))
      ("tex"  any ("name") ("add"))
      ("draw" any ("name"))
      ("box"  any ("name") ("fill") ("justify") ("nobox") ("round"))
      ("circle"  any ("fill"))
      ("ellipse" any ("fill"))
      ("aline"          any ("arrow") ("curve") ,@arcends)
      ("rline"          any ("arrow") ("curve") ,@arcends)
      ("arc"            any ("arrow") ,@arcends)
      ("elliptical_arc" any ("arrow") ,@arcends)
      ("join" any ("curve"))
      ("colormap" any ("color") ("palette"))
      ("abound") ("amove") ("rmove") ("arcto")   ("bezier") ("rbezier") ("curve")
      ("closepath") ("gsave") ("grestore") ("defmarker") ("margins") ("marker")
      ("postscript") ("print") ("psbbtweak") ("pscomment") ("reverse") ("save")
      ("size") ("text") ("write")
      ("set" ("alabeldist") ("alabelscale") ("arrowangle") ("arrowsize")
       ("arrowstyle" font-lock-constant-face
                     ("simple") ("filled") ("empty"))
       ("atitledist") ("atitlescale") ("background ")
       ("cap" font-lock-constant-face ("butt") ("round") ("square"))
       ("color") ("dashlen") ("fill") ("font") ("fontlwidth") ("hei")
       ("join" font-lock-constant-face ("mitre") ("round") ("bevel"))
       ("just" font-lock-constant-face
               ("center") ("left") ("right") ("tl") ("tc") ("tr") ("lc") ("cc")
               ("rc") ("bl") ("bc") ("br"))
       ("lstyle") ("lwidth") ("pattern")
       ("texscale" font-lock-constant-face
                   ("scale") ("fixed") ("none"))
       ("titlescale")
       ("ticksscale"))
      )))

(defun gle--fontify-next (desc face &optional top)
  (let ((entries '())
        (rules '())
        (any (eq (car desc) 'any)))
    (if any (pop desc))
    (dolist (x desc)
      (cond
       ((consp x)
        (push (cons (car x) (gle--fontify-next (cdr x) face))
              entries))
       ((null entries) (setq face x))
       (t
        (push (cons face entries) rules)
        (setq entries nil)
        (setq face x))))
    (when entries
      (push (cons face entries) rules))
    (when rules
      (cl-assert (not (and any top)))
      (let ((arules
             ;; List of (MORE FACE . ENTRIES) where MORE is non-nil if
             ;; one of the ENTRIES needs further processing.
             (mapcar (lambda (x)
                       (cons (not (not (cl-some #'cdr (cdr x)))) x))
                     rules))
            (regexp
             (concat (cond (any ".*") (top "^[ \t]*") (t "[ \t]*"))
                     "\\_<\\("
                     (mapconcat (lambda (x)
                                  (concat (regexp-opt (mapcar #'car (cdr x)))
                                          "\\_>\\(\\)"))
                                rules
                                "\\|")
                     "\\)")))
        (lambda (limit)
          (while
              (when (if top (re-search-forward regexp limit t)
                      (looking-at regexp))
                (goto-char (match-end 0))
                (let ((i 2))
                  (while (not (match-beginning i)) (setq i (1+ i)))
                  (let ((arule (nth (- i 2) arules)))
                    (put-text-property (match-beginning 1) (match-end 1)
                                       'face (cadr arule))
                    (when (car arule)
                      (let* ((kw (match-string-no-properties 1))
                             (entry (assoc-string kw (cddr arule) t)))
                        (cl-assert entry t)
                        (when (cdr entry)
                          (funcall (cdr entry) limit))))))
                ;; Try again if applicable.
                (or top any))))))))

(defvar gle-font-lock-keywords
  `(("^[ \t]*\\(\\(?:\\sw\\|\\s_\\)+\\)[ \t]*="
     (1 'font-lock-variable-name-face))
    ("^[ \t]*if[ \t][^!\n;]*[ \t]\\(then\\)\\_>"
     (1 'font-lock-keyword-face))
    ("^[ \t]*for[ \t][^!\n;]*[ \t]\\(to\\)\\_>\\(?:[^!\n;]*[ \t]\\(step\\)\\_>\\)?"
     (1 'font-lock-keyword-face) (2 'font-lock-keyword-face nil t))
    ("^[ \t]*else[ \t]+\\(if\\)[ \t][^!\n;]*[ \t]\\(then\\)\\_>"
     (1 'font-lock-keyword-face) (2 'font-lock-keyword-face))
    (,(concat "^[ \t]*end[ \t]+\\("
              (regexp-opt `("if" "sub" ,@gle--bloc-names))
              "\\_>\\)")
     (1 'font-lock-keyword-face))
    (,(concat "^[ \t]*begin[ \t]+\\(" (regexp-opt gle--bloc-names) "\\_>\\)")
     (1 'font-lock-keyword-face))
    ("^[ \t]*sub[ \t]*\\(\\(?:\\sw\\|\\s_\\)+\\)"
     (1 'font-lock-function-name-face))
    ("^[ \t]*\\(@[[:alpha:]][[:alnum:]_]*\\)" (1 'font-lock-function-name-face))
    (,(gle--fontify-next gle--line-syntax nil 'top))
    ;; FIXME: Actually, this can also be a function call!
    ;; ("^[ \t]*\\([[:alpha:]][[:alnum:]_]*\\)" (1 'font-lock-keyword-face))
    ("\\_<\\([[:alpha:]][[:alnum:]_]*\\)(" (1 'font-lock-function-name-face))))

;;;; Flymake

(defcustom gle-program-name "gle"
  "Name of the `gle' program."
  :type 'string)

(defvar-local gle--flymake-proc nil)

(defun gle--flymake (report-fn &rest _args)
  "GLE backend for Flymake.
See `flymake-diagnostic-functions' for documentation of REPORT-FN."
  ;; Code largely inspired from `ruby-flymake'.
  (unless (executable-find gle-program-name)
    (error "Cannot find `gle' executable"))

  (when (process-live-p gle--flymake-proc)
    (delete-process gle--flymake-proc))

  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq
       gle--flymake-proc
       (make-process
        :name "gle-flymake" :noquery t :connection-type 'pipe
        :buffer (generate-new-buffer " *gle-flymake*")
        :command (list gle-program-name "-nosave" "-")
        :sentinel
        (lambda (proc _event)
          (when (eq 'exit (process-status proc))
            (let ((diagnostics '()))
              (unwind-protect
                  (if (with-current-buffer source
                        (not (eq proc gle--flymake-proc)))
                      (message "Skipping obsolete check for %s" proc)
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      (while (search-forward-regexp
                              "^>> .*? (\\([0-9]+\\)) |\\(.*\\)|\n>>.*\n>> *\\(\\w+\\): *\\(.*\\)"
                              nil t)
                        (let ((line (string-to-number (match-string 1)))
                              (txt (match-string 2))
                              (kind (intern
                                     (format ":%s"
                                             (downcase (match-string 3)))))
                              (msg (match-string 4)))
                          (with-current-buffer source
                            (save-excursion
                              (goto-char (point-min))
                              (forward-line (1- line))
                              (push
                               (if (search-forward txt (line-end-position) t)
                                   (flymake-make-diagnostic source
                                                            (match-beginning 0)
                                                            (match-end 0)
                                                            kind
                                                            msg)
                                 (skip-chars-forward " \t")
                                 (flymake-make-diagnostic source
                                                          (point)
                                                          (line-end-position)
                                                          kind
                                                          msg))
                               diagnostics)))))))
                (kill-buffer (process-buffer proc)))
              (funcall report-fn diagnostics))))))
      (process-send-region gle--flymake-proc (point-min) (point-max))
      (process-send-eof gle--flymake-proc))))

;;;; Imenu

(defvar gle-imenu-generic-expression
  '(("Funs" "^[ \t]*sub[ \t]+\\(\\(?:\\s_\\|\\sw\\)+\\)" 1)
    ("Vars" "^[ \t]*\\(\\(?:\\s_\\|\\sw\\)+\\)[ \t]*=" 1)))

;;;; Completion

(defun gle--capf-data ()
  ;; FIXME: Use `gle--line-syntax'!
  (save-excursion
    (skip-chars-backward "a-z")
    (when (looking-back "^[ \t]*\\(?:begin\\|end\\)[ \t]+"
                        (line-beginning-position))
      (let ((beg (point))
            (end (progn
                   (skip-chars-forward "a-z")
                   (point))))
        `(,beg ,end ,gle--bloc-names)))))

(defun gle--before-change-function (beg end)
  (when (get-text-property beg 'gle-block-name)
    (condition-case err
        (with-silent-modifications
          ;; Remove property even if we don't find a pair.
          (remove-text-properties
           (previous-single-property-change (1+ beg) 'gle-block-name)
           (next-single-property-change beg 'gle-block-name)
           '(gle-block-name))
          (unless (or (get-char-property beg 'text-clones)
                      (get-char-property (1+ beg) 'text-clones)
                      (save-excursion
                        (goto-char beg)
                        (not (looking-back
                              "^[ \t]*\\(?:begi\\(n\\)\\|end\\)[ \t]*\\([[:alnum:]]*\\)"
                              (line-beginning-position)))))
            (let ((cmd-start (match-beginning 0))
                  (type (match-end 1))  ;nil for end, else begin.
                  (arg-start (match-beginning 2)))
              (save-excursion
                (goto-char (match-end 0))
                (when (and (looking-at "[[:alnum:]]")
                           (>= (match-end 0) end))
                  (let ((arg-end (match-end 0)))
                    (if (null type)     ;end
                        (progn (goto-char arg-end)
                               (forward-sexp -1)
                               (skip-chars-forward "[:alnum:]")
                               (skip-chars-forward " \t"))
                      (goto-char cmd-start)
                      (forward-sexp 1)
                      (skip-chars-backward "[:alnum:]"))
                    (when (looking-at
                           (regexp-quote (buffer-substring arg-start arg-end)))
                      (text-clone-create arg-start arg-end
                                         'spread "[[:alnum:]]*"))))))))
      (scan-error nil)
      (error (message "Error in gle--before-change-function %S" err)))))
        

;;;; Skeletons

(defvar gle--begend-default "graph")

(define-skeleton gle-insert-begin-end
  "Insert a begin...end bloc."
  (if (consp gle--begend-default)
      (car gle--begend-default)
    (let ((choice (completing-read (format "GLE begin name [%s]: "
					   gle--begend-default)
                                   gle--bloc-names
				   nil nil nil nil gle--begend-default)))
      (setq gle--begend-default choice)
      choice))
  \n "begin " str > \n > _ \n "end " str > \n)

(define-skeleton gle-insert-sub
  "Insert a sub...end bloc."
  nil ;; "Subroutine name: "
  \n "sub " ;; str
  > \n > _ \n "end sub" ;; " !" str
  > \n)

(define-skeleton gle-insert-if
  "Insert a if...end bloc."
  nil
  \n "if " @ " then"
  > \n > _ \n "else"
  > \n > \n "end if"
  > \n)

(define-skeleton gle-insert-for
  "Insert a for...next bloc."
  "GLE var name: "
  \n "for " str " = " @ " to " @ " step 1" > \n > _ \n "next " str > \n)

(define-skeleton gle-insert-while
  "Insert a while...next bloc."
  nil
  \n "while " > \n > _ \n "next" > \n)

(define-skeleton gle-insert-until
  "Insert a until...next bloc."
  nil
  \n "until " > \n > _ \n "next" > \n)

(defvar gle--bloc-default "graph")

(defun gle-insert-bloc (name)
  "Insert some bloc (begin..end, while...next, sub...end, ...).
NAME is the kind of bloc to insert."
  (interactive
   (list
    (let ((choice (completing-read (format "GLE bloc name [%s]: "
				           gle--bloc-default)
                                   `("for" "if" "until" "sub" "while"
                                     ,@gle--bloc-names)
			           nil nil nil nil gle--bloc-default)))
      (setq gle--bloc-default choice)
      choice)))
  (pcase name
    ("for"   (call-interactively 'gle-insert-for))
    ("if"    (call-interactively 'gle-insert-if))
    ("until" (call-interactively 'gle-insert-until))
    ("sub"   (call-interactively 'gle-insert-sub))
    ("while" (call-interactively 'gle-insert-while))
    (_ (let ((gle--begend-default (list name)))
         (call-interactively 'gle-insert-begin-end)))))

(define-skeleton gle-insert-close
  "Insert an end or next instruction to close the current bloc."
  (save-excursion
    (with-demoted-errors "Beginning not found!"
      (let* ((options (mapcar (lambda (tok) (assoc tok smie-grammar))
                              '("next" "next <var>" "end <thing>")))
             (closer (caar (sort options (lambda (o1 o2)
                                           (>= (cadr o1) (cadr o2))))))
             (opener (smie-backward-sexp closer)))
        (pcase opener
          (`(,_ ,_ ,(or "while" "until")) "next")
          (`(,_ ,_ "if bloc") "end if")
          (`(,_ ,_ "sub") "end sub")
          (`(,_ ,_ "for")
           (looking-at "[[:alnum:]]*")
           (concat "next " (match-string 0)))
          (`(,_ ,_ "begin")
           (if (looking-at "begin[ \t]+\\([[:alnum:]]+\\)")
               (concat "end " (match-string 1))
             (message "Can't find bloc name after `begin'!")))
          (_ (error "Unexpected beginning!"))))))
  \n str > \n)

;;;; Top-level

(defvar gle-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?\C-e] #'gle-insert-close)
    (define-key map [?\C-c ?\C-o] #'gle-insert-bloc)
    map))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gle\\'" . gle-mode))

;;;###autoload
(define-derived-mode gle-mode prog-mode "GLE"
  "Major mode to edit Graphics Layout Engine files."
  (setq-local comment-start "!")
  (setq-local syntax-propertize-function gle-syntax-propertize)
  (smie-setup gle-smie-grammar #'gle-smie-rules
              :forward-token #'gle-smie-forward-token
              :backward-token #'gle-smie-backward-token)
  (setq-local font-lock-defaults
              '(gle-font-lock-keywords nil t))
  (setq-local imenu-generic-expression gle-imenu-generic-expression)
  (add-hook 'flymake-diagnostic-functions #'gle--flymake nil 'local)
  (add-hook 'completion-at-point-functions #'gle--capf-data nil 'local)
  (add-hook 'before-change-functions #'gle--before-change-function nil 'local)
  )

(provide 'gle-mode)
;;; gle-mode.el ends here

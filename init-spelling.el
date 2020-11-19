;; ===== Spelling =====

;; Hunspell checks ../share/hunspell for dictionaries (relative to hunspell's bin dir).
;; This setup keeps separate personal/custom dictionaries for each system, which is fine
;; Credit to http://www.nextpoint.se/?p=656
;; Info on unibyte strings and \ddd see https://www.gnu.org/software/emacs/manual/html_node/elisp/Non_002dASCII-in-Strings.html
;; personal dictionary must be iso-8859-1 encoded, perhaps must be same as CHARACTER-SET.

;; ispell-dictionary-alist is a list of lists containing:
;; 1. DICTIONARY-NAME
;; 2. CASECHARS: unibyte, valid chars in a word, \ddd are base 8 for chars in CHARACTER-SET
;; 3. NOT-CASECHARS: unibyte, must be opposite of CASECHARS
;; 4. OTHERCHARS: unibyte, e.g. ' is part of some words in swedish and english
;; 5. MANY-OTHERCHARS-P: t if many OTHERCHARS can occur, like in english (the legend says)
;; 6. ISPELL-ARGS: extra args to ispell subprocess (hunspell)
;; 7. EXTENDED-CHARACTER-MODE: nil because I don't think I need it..?
;; 8. CHARACTER-SET: utf-8 required to allow e.g. "couldn't"
;;    iso-8859-1 was otherwise good enough for my languages.
;;    Used to encode text sent to the ispell subprocess.
;;    2-4 must be unibyte strings containing bytes of CHARACTER-SET.
(use-package ispell
  :ensure nil ;; built-in
  :init
  (setq ispell-dictionary-alist
        `(
          ;; default is "american"
          (nil
           "[A-Za-z]" "[^A-Za-z]" "[']" t ("-d" "en_US" "-p" ,(concat user-emacs-directory ".hunspell_en_US")) nil utf-8)

          ;; Swedish
          ;; settings from ispell-dictionary-base-alist in ispell.el with adjustments:
          ;; - ISPELL-ARGS
          ;; - EXTENDED-CHARACTER-MODE: original was: "~list".
          ("svenska"
           "[A-Za-z\345\344\366\351\340\374\350\346\370\347\305\304\326\311\300\334\310\306\330\307]"
           "[^A-Za-z\345\344\366\351\340\374\350\346\370\347\305\304\326\311\300\334\310\306\330\307]"
           "[']" nil ("-d" "sv_SE" "-p" ,(concat user-emacs-directory ".hunspell_sv_SE")) nil iso-8859-1)

          ;; American English
          ;; settings from ispell-dictionary-base-alist in ispell.el with adjustments:
          ;; - ISPELL-ARGS
          ;; - MANY-OTHERCHARS-P: t because double contractions seem to be possible
          ("american"
           "[A-Za-z]" "[^A-Za-z]" "[']" t ("-d" "en_US" "-p" ,(concat user-emacs-directory ".hunspell_en_US")) nil utf-8)
          ))
  (setq ispell-program-name my-hunspell-binary)
  :config
  (setq ispell-local-dictionary-alist ispell-dictionary-alist)
  (setq ispell-hunspell-dictionary-alist ispell-dictionary-alist))

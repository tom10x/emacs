(defvar my-spelling-default-dictionary "en_US")

(defvar my-spelling-binary "hunspell")

(use-package ispell
  :ensure nil
  :config
  (setq
   ispell-program-name my-spelling-binary
   ispell-dictionary my-spelling-default-dictionary))
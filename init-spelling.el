(defvar
  my-spelling-default-dictionary
  "en_US"
  "The default dictionary to use, default is en_US = American English.")

(defvar
  my-spelling-binary
  "hunspell"
  "Binary to use, specify full path if its not on PATH.")

(use-package ispell
  :ensure nil
  :config
  (setq
   ispell-program-name my-spelling-binary
   ispell-dictionary my-spelling-default-dictionary))
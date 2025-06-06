(require 'eglot-java)

(defvar eglot-java-lombok/version nil
  "When non-nil, use the specified Lombok version, otherwise use the latest.")

(defvar eglot-java-lombok/jar-url-base "https://projectlombok.org/downloads/"
  "The base path to download Lombok jars from.")

(defun eglot-java-lombok/jar-file ()
  "Get the filename for the Lombok jar."
  (concat "lombok"
          (when eglot-java-lombok/version "-")
          eglot-java-lombok/version
          ".jar"))

(defun eglot-java-lombok/jar-path ()
  "Generate the path on disk for the Lombok jar."
  (concat (expand-file-name user-emacs-directory) (eglot-java-lombok/jar-file)))

(defun eglot-java-lombok/download-jar ()
  "Download the latest lombok jar for use with LSP."
  (let* ((lombok-url (url-generic-parse-url eglot-java-lombok/jar-url-base))
         (base-path (file-name-as-directory (url-filename lombok-url)))
         (file-path (concat base-path (eglot-java-lombok/jar-file))))
    (setf (url-filename lombok-url) file-path)
    (url-copy-file lombok-url (eglot-java-lombok/jar-path))))

(defun eglot-java-lombok/setup ()
  "Download Lombok if it hasn't been downloaded already."
  (when (not (file-exists-p (eglot-java-lombok/jar-path)))
    (message "Could not find lombok")
    (eglot-java-lombok/download-jar)))

(defun eglot-java-lombok/append-vmargs ()
  "Apply lombok args."
  (add-to-list 'eglot-java-eclipse-jdt-args (concat "-javaagent:" (eglot-java-lombok/jar-path))))

(defun eglot-java-lombok/init ()
  "Initialize eglot-java-lombok."
  (eglot-java-lombok/setup)
  (eglot-java-lombok/append-vmargs))

(provide 'eglot-java-lombok)

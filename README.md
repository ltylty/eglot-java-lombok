lombok support for java in emacs eglot
# install
for emacs30
```
(use-package eglot-java-lombok :ensure t 
  :vc (:url "https://github.com/ltylty/eglot-java-lombok" :branch "main" :rev :newest)
  :config
  (eglot-java-lombok/init))
```

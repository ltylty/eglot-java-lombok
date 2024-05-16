lombok support for java in emacs eglot
# install
for emacs29
```
    (unless (package-installed-p 'eglot-java-lombok)
     (package-vc-install "https://github.com/ltylty/eglot-java-lombok"))
    (require 'eglot-java-lombok)
    (eglot-java-lombok/init)
```

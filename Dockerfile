FROM ocaml/ocaml
MAINTAINER Floreal Morandat <florealm@gmail.com>

RUN git clone https://github.com/GumTreeDiff/cgum.git /tmp/cgum && \
      make -C /tmp/cgum && \
      cp /tmp/cgum/cgum /usr/local/bin && \
      cp /tmp/cgum/cgumw /usr/local/bin/ && \
      install -D /tmp/cgum/standard.h /root/cgum/standard.h && \
      rm -Rf /tmp/cgum && \
      /usr/local/bin/cgum /root/cgum/standard.h > /dev/null 
ENTRYPOINT /usr/local/bin/cgumw

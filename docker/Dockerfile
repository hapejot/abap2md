FROM python:3-bullseye

WORKDIR /app

COPY nwrfcsdk /nwrfcsdk
COPY sapnwrfc.conf /etc/ld.so.conf.d/sapnwrfc.conf
ENV SAPNWRFC_HOME=/nwrfcsdk
ENV CC=clang
ENV LDSHARED="clang -shared"
# RUN apt update && apt-get -y install binutils && pip3 install Cython && pip3 install pyrfc
# we even need the development environment, since the pyrfc needs it to compile some parts
RUN apt update && apt-get -y install pandoc clang texlive graphviz plantuml librsvg2-bin && pip3 install Cython && pip3 install pyrfc tabulate flask

#COPY . .

#EXPOSE 5000
#CMD [ "python3" ]
COPY read_documentation read_documentation
COPY diagram-generator.lua diagram-generator.lua
VOLUME ["/config", "/work"]
RUN ln -s /config/sapnwrfc.ini . && ls -l

ENTRYPOINT ["./read_documentation"]

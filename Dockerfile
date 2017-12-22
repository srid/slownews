FROM haskell:8.2

CWD backend
RUN stack build

CMD stack exec backend


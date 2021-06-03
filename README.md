### Temporary README

Make sure to run

```shell
eval $(opam config env)
```

#### Build Information

To Build Main Project:
```shell
dune build src/driver.exe
```
To Run Main Project:
```shell
dune exec src/driver.exe
```
or
```shell
./_build/default/src/driver.exe
```
To Run Tests:
```shell
dune runtest
```

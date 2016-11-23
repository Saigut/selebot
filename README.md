# selebot

* Firstlly, install the Chez Scheme: https://github.com/cisco/ChezScheme

* Go into this selebot repo, then:

```
gcc -fPIC -shared -o csocket.so ./csocket.c

git clone --depth 1 https://github.com/rotty/spells spells_repo
mv spells_repo/spells ./
rm -rf spells_repo

git clone --depth 1 https://github.com/fedeinthemix/chez-srfi chez-srfi_repo
cd chez-srfi_repo
make build CHEZ=scheme
cd ..
mv chez-srfi_repo/srfi ./
rm -rf chez-srfi_repo
```
Then you can run server:
```
scheme ./server.ss
```

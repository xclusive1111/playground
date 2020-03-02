### Diesel, r2d2 with MySql connection pool

* Install libmysqlclient
  * On Arch

  ```
  $ pacman -Sy mysql++
  ```

  * On Debian

  ```
  $ apt install lib-mysqlclient-dev
  ```

* Install Diesel cli

```
$ cargo install diesel_cli --no-default-features --features mysql
```

* Diesel setup

```
$ diesel setup
```

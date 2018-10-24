# cryptdb on Ubuntu 16.04.
fork from git://g.csail.mit.edu/cryptdb, http://css.csail.mit.edu/cryptdb/

My first attempt to compile cryptdb. 

**Error fixes**

Various error fixes were created, which can be found in the errfix folder. 
All of them where applied in scripts/install.rb.

Since it failed during "Building cryptdb ..." because of using a newer g++ version, I downgraded to gcc v.4.7 and g++ v.4.7:
```
sudo apt install gcc-4.7
sudo apt install g++-4.7
sudo update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.7 100
```

Here are some packages I have installed during compilation.
```
sudo apt-get install build-essential autoconf libtool pkg-config libssl-dev git gcc automake autoconf libtool php7.0 automake bzr cmake flex g++ git gtk-doc-tools libaio-dev libbsd-dev libevent-dev libglib2.0-dev libgmp-dev liblua5.1-0-dev libmysqlclient-dev libncurses5-dev libntl-dev libssl-dev
```

This is the system, I'm currently working with:
```
Linux vm.dash 4.10.0-37-generic #41~16.04.1-Ubuntu SMP Fri Oct 6 22:42:59 UTC 2017 x86_64 x86_64 x86_64 GNU/Linux
```

Get started by running (building cryptdb):
```
sudo ./scripts/install.rb .
```

To run cryptdb:
```
II. Proxy
    A) To Start: 
         > export EDBDIR=/path/to/cryptdb
         > $EDBDIR/bins/proxy-bin/bin/mysql-proxy         \
                     --plugins=proxy --event-threads=4             \
                     --max-open-files=1024                         \
                     --proxy-lua-script=$EDBDIR/mysqlproxy/wrapper.lua \
                     --proxy-address=127.0.0.1:3307                \
                     --proxy-backend-addresses=localhost:3306

    B) Connect to CryptDB: (where root/letmein are username/password)
       mysql -u root -pletmein -h 127.0.0.1 -P 3307
    C) CREATE a database; USE it; Then type queries that will execute
       on encrypted data at the DB server!
```

**Another working solution via Docker**

[agribu/CryptDB_Docker](https://github.com/agribu/CryptDB_Docker)
(forked from klevstad/CryptDB_Docker)

**Resources**

* https://css.csail.mit.edu/cryptdb/
* https://github.com/cryptdb-org/cryptdb-wiki/wiki
* https://m3ideas.org/2017/07/10/infor-grid-on-cryptdb/
* https://github.com/CryptDB/cryptdb (More information included in their README.md file)
* https://mshcruz.wordpress.com/2016/06/24/summary-cryptdb/
* https://whitehatty.com/2012/09/30/cryptdb-howto-compile-on-ubuntu-linux-12-04/
* https://github.com/yiwenshao/Practical-Cryptdb (also running on Ubuntu 16.04, **most actively maintained repository**)

**Limitations**

This version of CryptDB is only single principal.

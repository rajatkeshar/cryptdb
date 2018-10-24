rm -rf $EDBDIR/shadow/*
mysql -u root -pletmein -e "drop database if exists remote_db"
mysql -u root -pletmein -e "drop database if exists cryptdbtest"

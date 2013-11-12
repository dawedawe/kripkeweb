DUMPFILE=$1

pg_dump -c -C -f $DUMPFILE -h localhost -U saul kripkeweb

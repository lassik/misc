if [ "x$1" = "xstart" ]; then
   echo -n " noidentity"
   daemon /usr/local/bin/tcpserver -c 1 -u 65534 -g 65534 -D -R \
    -x /usr/local/etc/noidentity.tcprules.cdb \
    0 auth /usr/local/libexec/noidentity
fi

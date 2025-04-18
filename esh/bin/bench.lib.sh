#!/bin/sh
# -*- mode: sh -*-


SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`


echo2()
{
    echo >&2 "$@"
}


OS_TYPE=`uname -s`
######################################### cpu
cpu_count()
{
    case $OS_TYPE in
        Linux)
            cpu_count=`nproc`
            ;;
        *)
            echo2 "# $OS_TYPE OS is not supported"
            ;;
    esac
    echo $cpu_count
}


######################################### sysbench
sysbench_cpu()
{
    sysbench_cpu__threads="$1"
    if [ -z "$sysbench_cpu__threads" ]; then
        cpu_count >/dev/null && sysbench_cpu__threads="$cpu_count"
    fi
    sysbench cpu \
             --num-threads=$sysbench_cpu__threads \
             --cpu-max-prime=1000000000 \
             run
}

sysbench_io()
{
    sysbench --test=fileio --file-total-size=150G prepare
    sysbench --test=fileio --file-total-size=150G \
             --file-test-mode=rndrw --init-rng=on \
             --max-time=300 --max-requests=0 \
             run
    sysbench --test=fileio --file-total-size=150G cleanup
}

sysbench_mysql()
{
    sysbench --test=oltp --oltp-table-size=1000000 --db-driver=mysql \
             --mysql-db=test --mysql-user=root \
             --mysql-password=yourrootsqlpassword prepare
    sysbench --test=oltp --oltp-table-size=1000000 --db-driver=mysql \
             --mysql-db=test --mysql-user=root \
             --mysql-password=yourrootsqlpassword --max-time=60 \
             --oltp-read-only=on --max-requests=0 --num-threads=8 run
    sysbench --test=oltp --db-driver=mysql --mysql-db=test --mysql-user=root \
             --mysql-password=yourrootsqlpassword cleanup
}

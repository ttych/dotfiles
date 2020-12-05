#!/bin/sh
# -*- mode: sh -*-

cert_print_j()
{
    if [ $# -eq 0 ]; then
        keytool -printcert -V
        return $?
    fi

    for cert; do
        if [ ! -f "$cert" ]; then
            echo >&2 "$cert is not a file"
            continue
        fi

        if [ ! -r "$cert" ]; then
            echo >&2 "$cert is not readable"
            continue
        fi

        keytool -printcert -V -file $cert
    done
}

cert_print_o()
{
    if [ $# -eq 0 ]; then
        openssl x509 -text
        return $?
    fi

    for cert; do
        if [ ! -f "$cert" ]; then
            echo >&2 "$cert is not a file"
            continue
        fi

        if [ ! -r "$cert" ]; then
            echo >&2 "$cert is not readable"
            continue
        fi

        openssl x509 -in $cert -text
    done
}

cert_get()
{
    cert_get_o "$@"
}

cert_get_o()
{
    openssl s_client -showcerts -connect "$1" ${2:+-servername "$2"} -prexit </dev/null
}

cert_to_pem()
{
    openssl x509 -outform pem ${1:+-out "$1"}
}

cert_verify()
{
    openssl verify ${2:+-untrusted "$2"} "$@"
}

cert_self_signed()
{
    cert_self_signed="${1:-cert}"

    if [ -r "$cert_self_signed.pem" ]; then
	echo >&2 "$cert_self_signed.pem already exists"
	return 1
    fi

    openssl req -x509 -nodes -newkey rsa:4096 -keyout "$cert_self_signed.key" -out "$cert_self_signed.pem" -days "${2:-3650}"
}



### Main

case ${0##*/} in
    cert_*)
        ${0##*/} "$@"
        ;;
esac

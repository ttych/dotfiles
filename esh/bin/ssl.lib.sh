#!/bin/sh
# -*- mode: sh -*-


SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`


######################################### documentation

# https://web.archive.org/web/20170107050552/http://shib.kuleuven.be/docs/ssl_commands.shtml



#########################################
cat2()
{
    cat >&2 "$@"
}

echo2()
{
    echo >&2 "$@"
}

has_openssl()
{
    which openssl >/dev/null 2>/dev/null
}

has_keytool()
{
    which keytool >/dev/null 2>/dev/null
}


######################################### pem
ssl_cert_text()
{
    if [ -p /dev/stdin ]; then
        openssl x509 -noout -text
    else
        for cert; do
            if [ ! -f "$cert" ] || [ ! -r "$cert" ]; then
                echo >&2 "$cert is not accessible"
                continue
            fi

            openssl x509 -in $cert -noout -text
        done
    fi
}

ssl_cert_subject()
{
    if [ -p /dev/stdin ]; then
        openssl x509 -noout -subject -nameopt RFC2253 | sed 's/^subject=//'
    else
        for cert; do
            if [ ! -f "$cert" ] || [ ! -r "$cert" ]; then
                echo >&2 "$cert is not accessible"
                continue
            fi

            openssl x509 -in "$cert" -noout -subject -nameopt RFC2253 | sed 's/^subject=//'
        done
    fi
}

ssl_cert_dates()
{
    if [ -p /dev/stdin ]; then
        openssl x509 -noout -dates
    else
        for cert; do
            if [ ! -f "$cert" ] || [ ! -r "$cert" ]; then
                echo >&2 "$cert is not accessible"
                continue
            fi

            openssl x509 -in "$cert" -noout -dates
        done
    fi
}

ssl_cert_startdate()
{
    if [ -p /dev/stdin ]; then
        openssl x509 -noout -startdate
    else
        for cert; do
            if [ ! -f "$cert" ] || [ ! -r "$cert" ]; then
                echo >&2 "$cert is not accessible"
                continue
            fi

            openssl x509 -in "$cert" -noout -startdate
        done
    fi
}

ssl_cert_enddate()
{
    if [ -p /dev/stdin ]; then
        openssl x509 -noout -enddate
    else
        for cert; do
            if [ ! -f "$cert" ] || [ ! -r "$cert" ]; then
                echo >&2 "$cert is not accessible"
                continue
            fi

            openssl x509 -in "$cert" -noout -enddate
        done
    fi
}

ssl_cert_match_cert_key()
{
    [ $# -ne 2 ] &&
        echo2 "# ssl_match_cert_key <cert> <key>" &&
        return 1

    cert_pubkey=`openssl x509 -pubkey -noout -in "$1" | openssl md5`
    key_pubkey=`openssl pkey -pubout -in "$2" | openssl md5`

    [ "$cert_pubkey" = "$key_pubkey" ]
}

ssl_cert_match_req_key()
{
    [ $# -ne 2 ] &&
        echo2 "# ssl_match_req_key <req> <key>" &&
        return 1

    req_pubkey=`openssl req -pubkey -noout -in "$1" | openssl md5`
    key_pubkey=`openssl pkey -pubout -in "$2" | openssl md5`

    [ "$req_pubkey" = "$key_pubkey" ]
}

ssl_cert_match_mod_cert_key()
{
    [ $# -ne 2 ] &&
        echo2 "# ssl_match_cert_key <cert> <key>" &&
        return 1

    modulus_a=`openssl x509 -noout -modulus -in "$1" | openssl md5`
    modulus_b=`openssl rsa -noout -modulus -in "$2" | openssl md5`

    [ "$modulus_a" = "$modulus_b" ]
}

ssl_cert_match_mod_req_key()
{
    [ $# -ne 2 ] &&
        echo2 "# ssl_match_req_key <req> <key>" &&
        return 1

    modulus_a=`openssl req -noout -modulus -in "$1" | openssl md5`
    modulus_b=`openssl rsa -noout -modulus -in "$2" | openssl md5`

    [ "$modulus_a" = "$modulus_b" ]
}

ssl_verify()
{
    ssl_verify__usage="ssl_verify [-C CA_FILE] <cert>"

    OPTIND=1
    while getopts :hC: opt; do
        case $opt in
            h) echo2 "# $ssl_verify__usage"
               return 0
               ;;
            C) ssl_verify__ca_file="$OPTARG"
               ;;
        esac
    done
    shift $(($OPTIND - 1))

    openssl verify ${ssl_verify__ca_file:+-CAfile "$ssl_verify__ca_file"} "$@"
}

ssl_gen_self_signed()
{
    ssl_gen_self_signed__usage="ssl_gen_self_signed <name>"

    ssl_gen_self_signed__nodes=
    ssl_gen_self_signed__days=3655
    OPTIND=1
    while getopts :hd:P: opt; do
        case $opt in
            h) echo "# $ssl_gen_self_signed__usage"
               return 0
               ;;
            d) ssl_gen_self_signed__days="$OPTARG"
               ;;
            P) ssl_gen_self_signed__nodes=1
               ;;
        esac
    done
    shift $(($OPTIND - 1))

    ssl_gen_self_signed="$1"
    if [ -z "$ssl_gen_self_signed" ]; then
        echo2 "# please provide a name"
        echo2
        echo2 "# $ssl_gen_self_signed__usage"
        return 1
    fi
    if [ -r "${ssl_gen_self_signed}.pem" ]; then
        echo2 "# $ssl_gen_self_signed already exists"
        return 1
    fi

    openssl req -x509 \
            ${ssl_gen_self_signed__nodes:+-nodes} \
            -newkey rsa:4096 \
            -keyout "${ssl_gen_self_signed}.key" \
            -out "${ssl_gen_self_signed}.pem" \
            -days "${ssl_gen_self_signed__days}"
}


######################################### key
ssl_is_key()
{
    openssl pkey -in "$1" -check
}

ssl_key_gen()
{
    ssl_key_gen__usage="ssl_key_gen <key_filepath> -a <algo: RSA, ED25519, EC, ..> -b <algo_params> -p <pass>"

    OPTIND=1
    ssl_key_gen__algo="RSA"
    ssl_key_gen__keybits="rsa_keygen_bits:4096"
    ssl_key_gen__pass=
    while getopts :ha:b:p: opt; do
        case $opt in
            a)
                ssl_key_gen__algo="$OPTARG"
                case "$ssl_key_gen__algo" in
                    RSA|ED25519|X25519|EC)
                    ;;
                    *)
                        echo2 "# ssl_key_gen: unknown algo $ssl_key_gen__algo"
                        return 1
                        ;;
                esac
                ;;
            b)
                case "$ssl_key_gen__algo" in
                    RSA)
                        ssl_key_gen__keybits="rsa_keygen_bits:$OPTARG"
                        ;;
                esac
                ;;
            p)
                ssl_key_gen__pass="pass:$OPTARG"
                ;;
            h) echo2 "# $ssl_key_gen__usage"
               return 0
               ;;
        esac
    done
    shift $(($OPTIND - 1))

    ssl_key_gen="$1"

    if [ -z "$ssl_key_gen" ]; then
        echo2 "# please specify a key_filepath"
        echo2
        echo2 "# $ssl_key_gen__usage"
        return 1
    fi

    openssl genpkey \
            -algorithm "$ssl_key_gen__algo" \
            -out "$ssl_key_gen" \
            ${ssl_key_gen__keybits:+-pkeyopt "$ssl_key_gen__keybits"} \
            ${ssl_key_gen__pass:+-aes256} \
            ${ssl_key_gen__pass:+-pass "$ssl_key_gen__pass"}
}

ssl_key_remove_passphrase()
{
    ssl_key_remove_passphrase__usage="ssl_key_remove_passphrase [-p pass] [-P pass_file] <key_filepath>"

    OPTIND=1
    ssl_key_remove_passphrase__pass=
    while getopts :hp:P: opt; do
        case $opt in
            h)
                echo2 "# $ssl_key_remove_passphrase__usage"
                return 0
                ;;
            p)
                ssl_key_remove_passphrase__pass="$OPTARG"
                ;;
            P)  ssl_key_remove_passphrase__passfile="$OPTARG"
                ;;
        esac
    done
    shift $(($OPTIND - 1))

    if ! ssl_is_key "$1"; then
        echo >&2 "\"$1\" is not a keyfile, please specify a valid keyfile as argument."
        return 1
    fi

    openssl pkey -in "$1" -out "${1%.pem}.unencrypted.pem" \
            ${ssl_key_remove_passphrase__passfile:+-passin "file:$ssl_key_remove_passphrase__passfile"} \
            ${ssl_key_remove_passphrase__pass:+-passin "pass:$ssl_key_remove_passphrase__pass"}
}


######################################### client
ssl_client()
{
    openssl s_client "$@"
    # -showcerts
    # -prexit
    # -servername
    # -cert / -key / -pass
    # -verify_hostname
    # -verify_return_error
}


######################################### keytool
keytool_cert_text()
{
    for cert; do
        if [ ! -f "$cert" ] || [ ! -r "$cert" ]; then
            echo >&2 "$cert is not accessible"
            continue
        fi

        keytool -printcert -V -file "$cert"
    done
}



######################################### main
case "$SCRIPT_NAME" in
    ssl_*|cert_*|digest_*)
        "$SCRIPT_NAME" "$@"
        ;;
    keytool_*)
        "$SCRIPT_NAME" "$@"
        ;;
esac

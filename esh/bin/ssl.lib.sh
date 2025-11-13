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

print_usage()
{
    printf >&2 "# %s\n" "$@"
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

ssl_cert_subject_alt_name()
{
    ssl_cert_subject_alt_name__in="$1"
    if [ -p /dev/stdin ]; then
        ssl_cert_subject_alt_name__in=
    fi
    if [ -n "$ssl_cert_subject_alt_name__in" ] && [ ! -r "$ssl_cert_subject_alt_name__in" ]; then
        echo >&2 "$ssl_cert_subject_alt_name__in is not accessible"
        return 1
    fi

    openssl x509 ${ssl_cert_subject_alt_name__in:+-in "$ssl_cert_subject_alt_name__in"} -noout -ext subjectAltName |
        sed -e '/^X509v3 Subject Alternative Name:/d; s/^[[:space:]]*//; s/,[[:space:]]*/\n/g'
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

ssl_cert_verify()
{
    ssl_cert_verify__usage="ssl_cert_verify [-C CA_FILE] <cert>"

    OPTIND=1
    while getopts :hC: opt; do
        case $opt in
            h) echo2 "# $ssl_cert_verify__usage"
               return 0
               ;;
            C) ssl_cert_verify__ca_file="$OPTARG"
               ;;
        esac
    done
    shift $(($OPTIND - 1))

    openssl verify ${ssl_cert_verify__ca_file:+-CAfile "$ssl_cert_verify__ca_file"} "$@"
}

ssl_cert_gen_self_signed()
{
    ssl_cert_gen_self_signed__usage="ssl_cert_gen_self_signed <name>"

    ssl_cert_gen_self_signed__nodes=
    ssl_cert_gen_self_signed__days=3655
    OPTIND=1
    while getopts :hd:P: opt; do
        case $opt in
            h) echo "# $ssl_cert_gen_self_signed__usage"
               return 0
               ;;
            d) ssl_cert_gen_self_signed__days="$OPTARG"
               ;;
            P) ssl_cert_gen_self_signed__nodes=1
               ;;
        esac
    done
    shift $(($OPTIND - 1))

    ssl_cert_gen_self_signed="$1"
    if [ -z "$ssl_cert_gen_self_signed" ]; then
        echo2 "# please provide a name"
        echo2
        echo2 "# $ssl_cert_gen_self_signed__usage"
        return 1
    fi
    if [ -r "${ssl_cert_gen_self_signed}.pem" ]; then
        echo2 "# $ssl_cert_gen_self_signed already exists"
        return 1
    fi

    openssl req -x509 \
            ${ssl_cert_gen_self_signed__nodes:+-nodes} \
            -newkey rsa:4096 \
            -keyout "${ssl_cert_gen_self_signed}.key" \
            -out "${ssl_cert_gen_self_signed}.pem" \
            -days "${ssl_cert_gen_self_signed__days}"
}


######################################### request
ssl_req_text()
{
    if [ -p /dev/stdin ]; then
        openssl req -noout -text
    else
        for req; do
            if [ ! -f "$req" ] || [ ! -r "$req" ]; then
                echo >&2 "$req is not accessible"
                continue
            fi

            openssl req -in $req -noout -text
        done
    fi
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

ssl_client_get_cert()
{
    ssl_client_get_cert__usage="ssl_client_get_cert [-S] hostname port [servername]"

    OPTIND=1
    ssl_client_get_cert__starttls=
    while getopts :hS opt; do
        case $opt in
            h) print_usage "$ssl_client_get_cert__usage"
               return 0
               ;;
            S) ssl_client_get_cert__starttls="-starttls"
               ;;
        esac
    done
    shift $(($OPTIND - 1))

    ssl_client_get_cert__hostname="${1}"
    ssl_client_get_cert__port="${2:-443}"
    ssl_client_get_cert__servername="${3}"

    if [ -z "$ssl_client_get_cert__hostname" ]; then
        print_usage "$ssl_client_get_cert__usage"
        return 1
    fi

    openssl s_client -connect "${ssl_client_get_cert__hostname}:${ssl_client_get_cert__port}" \
            ${ssl_client_get_cert__servername:+-servername $ssl_client_get_cert__servername} \
            -showcerts $ssl_client_get_cert__starttls </dev/null 2>/dev/null |
        openssl x509 -outform PEM
}

ssl_client_get_cert_chain()
{
    ssl_client_get_cert_chain__usage="ssl_client_get_cert_chain"

    OPTIND=1
    ssl_client_get_cert_chain__starttls=
    while getopts :hS opt; do
        case $opt in
            h) echo "# $ssl_client_get_cert_chain__usage"
               return 0
               ;;
            S) ssl_client_get_cert_chain__starttls="-starttls"
               ;;
        esac
    done
    shift $(($OPTIND - 1))

    ssl_client_get_cert_chain__hostname="${1}"
    ssl_client_get_cert_chain__port="${2:-443}"
    ssl_client_get_cert_chain__servername="${3}"

    if [ -z "$ssl_client_get_cert_chain__hostname" ]; then
        print_usage "$ssl_client_get_cert_chain__usage"
        return 1
    fi

    openssl s_client -connect "${ssl_client_get_cert_chain__hostname}:${ssl_client_get_cert_chain__port}" \
            ${ssl_client_get_cert_chain__servername:+-servername $ssl_client_get_cert_chain__servername} \
            -showcerts $ssl_client_get_cert_chain__starttls </dev/null
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


######################################### convert
ssl_cert_pem_to_jks()
{
    ssl_cert_pem_to_jks__usage="ssl_cert_pem_to_jks [-h] [-a alias] [-c ca_file] <cert> <key> [jks]"

    if ! has_openssl; then
        echo2 "# openssl bin is missing"
        return 1
    fi
    if ! has_keytool; then
        echo2 "# keytool bin is missing"
        return 1
    fi

    ssl_cert_pem_to_jks__alias=
    ssl_cert_pem_to_jks__ca=
    ssl_cert_pem_to_jks__password=

    OPTIND=1
    while getopts :ha:c:p: opt; do
        case $opt in
            h) ;;
            a) ssl_cert_pem_to_jks__alias="$OPTARG" ;;
            c) ssl_cert_pem_to_jks__ca="$OPTARG" ;;
            p) ssl_cert_pem_to_jks__password="$OPTARG" ;;
        esac
    done
    shift $(($OPTIND - 1))

    ssl_cert_pem_to_jks__cert="$1"
    ssl_cert_pem_to_jks__key="$2"
    ssl_cert_pem_to_jks__jks="$3"

    if [ -z "$ssl_cert_pem_to_jks__cert" ] || [ -z "$ssl_cert_pem_to_jks__key" ]; then
        echo2 "# missing arguments : cert / key"
        print_usage "$ssl_cert_pem_to_jks__usage"
        return 1
    fi
    if [ ! -r "$ssl_cert_pem_to_jks__cert" ] || [ ! -r "$ssl_cert_pem_to_jks__key" ]; then
        echo2 "# unreadable cert / key"
        print_usage "$ssl_cert_pem_to_jks__usage"
        return 1
    fi
    if [ -z "$ssl_cert_pem_to_jks__jks" ]; then
        ssl_cert_pem_to_jks__jks="${ssl_cert_pem_to_jks__cert%.pem}.jks"
    fi
    if [ -z "$ssl_cert_pem_to_jks__alias" ]; then
        ssl_cert_pem_to_jks__alias=$(openssl x509 -in "$ssl_cert_pem_to_jks__cert" -subject -noout | sed 's/.*CN=\([^/]*\).*/\1/')
    fi

    openssl pkcs12 -export \
            -in "${ssl_cert_pem_to_jks__cert}" \
            -inkey "${ssl_cert_pem_to_jks__key}" \
            ${ssl_cert_pem_to_jks__ca:+-certfile "$ssl_cert_pem_to_jks__ca"} \
            -out "${ssl_cert_pem_to_jks__cert}.p12" \
            -name "$ssl_cert_pem_to_jks__alias" \
            ${ssl_cert_pem_to_jks__password:+-passout "pass:$ssl_cert_pem_to_jks__password"} \
        &&
        keytool -importkeystore \
                -srckeystore "${ssl_cert_pem_to_jks__cert}.p12" \
                -srcstoretype PKCS12 \
                ${ssl_cert_pem_to_jks__password:+-srcstorepass "$ssl_cert_pem_to_jks__password"} \
                -destkeystore "$ssl_cert_pem_to_jks__jks" \
                -deststoretype JKS \
                ${ssl_cert_pem_to_jks__password:+-deststorepass "$ssl_cert_pem_to_jks__password"} ||
            return 1

    rm -f "${ssl_cert_pem_to_jks__cert}.p12"
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

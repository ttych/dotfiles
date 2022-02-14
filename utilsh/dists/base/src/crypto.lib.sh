#!/bin/sh
# -*- mode: sh -*-

# ### doc

# https://web.archive.org/web/20170107050552/http://shib.kuleuven.be/docs/ssl_commands.shtml

# ###

has_openssl()
{
    which openssl >/dev/null 2>/dev/null
}

has_sha384sum()
{
    which sha384sum >/dev/null 2>/dev/null
}

_sha()
{
    case `uname -s` in
        Linux) "sha${2:-512}sum" "${1}" ;;
        FreeBSD) "$sha${2:-512}" -r "${1}" ;;
        *) echo >&2 "unsupported arch for _sha"
           return 1 ;;
    esac
}



# ### cert

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

cert_gen_key()
{
    openssl genrsa -out "$1" "${2:-4096}"
}

cert_gen_key_secured()
{
    openssl genrsa -aes128 -passout pass:"$3" -out "$1" "${2:-4096}"
}

cert_check_private_key()
{
    [ -n "$1" ] || return 1
    [ -r "$1" ] || return 1

    openssl rsa -check -in "$1"
}

cert_is_key_file()
{
    cert_check_private_key "$@"
}

cert_check_match_cert_key()
{
    modulus_a=`openssl x509 -noout -modulus -in "$1" | openssl md5`
    modulus_b=`openssl rsa -noout -modulus -in "$2" | openssl md5`

    [ "$modulus_a" = "$modulus_b" ]
}

cert_check_match_req_key()
{
    modulus_a=`openssl req -noout -modulus -in "$1" | openssl md5`
    modulus_b=`openssl rsa -noout -modulus -in "$2" | openssl md5`

    [ "$modulus_a" = "$modulus_b" ]
}

cert_remove_passphrase()
{
    if ! cert_is_key_file "$1"; then
        echo >&2 "\"$1\" is not a keyfile, please specify a valid keyfile as argument."
        return 1
    fi

    openssl rsa -in "$1" -out "${1%.pem}.nopp.pem"
}

cert_pkcs12_to_pem()
{
    [ $# -ne 1 ] || {
	echo >&2 "Usage: cert_pkcs12_to_pem <cert.p12>"
	return 1
    }
    openssl pkcs12 -in "$1" -out "${1%.p12}.crt.pem" -clcerts -nokeys && \
	openssl pkcs12 -in "$1" -out "${1%.p12}.key.pem" -nocerts -nodes
}

cert_jks_to_pkcs12()
{
    [ $# -ne 1 ] && return 1

    # keytool \
    # 	-importkeystore \
    # 	-srckeystore KEYSTORE.jks \
    # 	-srcstoretype JKS \
    # 	-srcstorepass mysecret \
    # 	-srcalias myalias \
    # 	-srckeypass mykeypass \
    # 	-destkeystore KEYSTORE.p12 \
    # 	-deststoretype PKCS12 \
    # 	-deststorepass mysecret \
    # 	-destalias myalias \
    # 	-destkeypass mykeypass \
    # 	-noprompt

    keytool \
	-importkeystore \
	-srckeystore "$1" \
	-srcstoretype JKS \
	-deststoretype PKCS12 \
	-destkeystore "${1%.jks}.p12"
}

cert_pkcs12_to_jks()
{
    [ $# -ne 1 ] && return 1

    keytool \
	-importkeystore \
	-srckeystore "$1" \
	-srcstoretype PKCS12 \
	-deststoretype JKS \
	-destkeystore "${1%.p12}.jks"
}


# ### digest

digest_integrity()
{
    if has_openssl; then
        _digest_integrity_o "$@"
        digest_integrity="$_digest_integrity_o"
    else
        _digest_integrity_s "$@"
        digest_integrity="$_digest_integrity_s"
    fi
    echo "$digest_integrity"
}

_digest_integrity_o()
{
    _digest_integrity_o=
    digest_integrity_o__file="$1"
    digest_integrity_o__digest="${2:-512}"

    if [ ! -f "$digest_integrity_o__file" ] || [ ! -r "$digest_integrity_o__file" ]; then
        echo >&2 "could not compute integrity for \"$digest_integrity_o__file\""
        return 1
    fi

    # cat FILENAME.js | openssl dgst -sha384 -binary | openssl base64 -A
    _digest_integrity_o=$(openssl dgst \
                                  -"sha$digest_integrity_o__digest" \
                                  -binary "$digest_integrity_o__file" |
                              openssl base64 -A)
}

_digest_integrity_s()
{
    _digest_integrity_s=
    digest_integrity_s__file="$1"
    digest_integrity_s__digest="${2:-512}"

    if [ ! -f "$digest_integrity_s__file" ] || [ ! -r "$digest_integrity_s__file" ]; then
        echo >&2 "could not compute integrity for \"$digest_integrity_s__file\""
        return 1
    fi

    # cat FILENAME.js | openssl dgst -sha384 -binary | openssl base64 -A
    _digest_integrity_s=$(_sha "$digest_integrity_s__file" "$digest_integrity_s__digest" |
                              awk '{ print $1 }' | xxd -r -p | base64)
}



# ### Main

case ${0##*/} in
    cert_*|digest_*)
        ${0##*/} "$@"
        ;;
esac

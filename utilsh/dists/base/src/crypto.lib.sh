#!/bin/sh
# -*- mode: sh -*-



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

cert_is_key_file()
{
    [ -n "$1" ] || return 1
    [ -r "$1" ] || return 1
    return 0
}

cert_remove_passphrase()
{
    if ! cert_is_key_file "$1"; then
        echo >&2 "\"$1\" is not a keyfile, please specify a valid keyfile as argument."
        return 1
    fi

    openssl rsa -in "$1" -out "${1%.pem}.nopp.pem"
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

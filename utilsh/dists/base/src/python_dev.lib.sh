#!/bin/sh
# -*- mode: sh -*-


TRUE()
{
    return 0
}

FALSE()
{
    return 1
}


#%%load%% devel.lib.sh


########## python

pip_exec()
{
    which pip 2>/dev/null >/dev/null || {
        echo >&2 "missing pip command"
        return 1
    }
    pip "$@"
}

pip_requirements()
{
    pip_exec install -r "$1"
}

python_exec()
{
    which python 2>/dev/null >/dev/null || {
        echo >&2 "missing python command"
        return 1
    }
    python "$@"
}

python_flake8()
{
    which flake8 2>/dev/null >/dev/null || {
        echo >&2 "missing flake8 command"
        return 1
    }
    flake8 "$@"
}

python_pylint()
{
    which pylint 2>/dev/null >/dev/null || {
        echo >&2 "missing pylint command"
        return 1
    }
    pylint "$@"
}

python_black()
{
    which black 2>/dev/null >/dev/null || {
        echo >&2 "missing black command"
        return 1
    }
    black "$@"
}

python_pytest()
{
    which pytest 2>/dev/null >/dev/null || {
        echo >&2 "missing pytest command"
        return 1
    }
    pytest "$@"
}

python_package()
{
    if [ -n "$1" ]; then
        python_exec "$1" check -ms
    fi
}

python_virtualenv_check()
{
    if [ -z "$VIRTUAL_ENV" ]; then
        echo >&2 "not in a virtual env"
        if "$RGR_CHECK"; then
            return 1
        fi
    fi
}


########## rgr

DEVEL_LANG="${DEVEL_LANG:+$DEVEL_LANG }python requirements"

PYTHON_LINTER="${PYTHON_LINTER:-flake8}"
PYTHON_AUTOLINTER="${PYTHON_AUTOLINTER:-black}"

is_python_file()
{
    [ -d "$1" ] && return 1

    case "$1" in
        *.py)
            return 0
    esac
    return 1
}

is_requirements_file()
{
    [ -d "$1" ] && return 1

    case "$1" in
        requirements.txt|requirements*.txt)
            return 0
    esac
    return 1
}

requirements_rgr()
{
    python_virtualenv_check &&
        pip_rgr_install "$@"
}

pip_rgr_install()
{
    "${2:-:}" "pip install -r $1"
    pip_requirements "$1"
}

python_rgr()
{
    python_rgr_test "$@" &&
        python_rgr_lint "$@"
}

python_rgr_lint()
{
    python_rgr_"$PYTHON_LINTER" "$@"
    python_rgr_lint__status=$?

    if [ $python_rgr_lint__status -ne 0 ] && $RGR_AUTO; then
        python_rgr_"$PYTHON_AUTOLINTER" "$@"
        python_rgr_"$PYTHON_LINTER" "$@"
        python_rgr_lint__status=$?
    fi
    return $python_rgr_lint__status
}

python_rgr_pylint()
{
    "${2:-:}"  "pylint $1"
    python_pylint "$1"
}

python_rgr_flake8()
{
    "${2:-:}"  "flake8 $1"
    python_flake8 "$1"
}

python_rgr_black()
{
    "${2:-:}"  "black $1"
    python_black "$1"
}

python_rgr_test()
{
    if ! python_test_identify "$1"; then
        "${2:-:}" "no test found for $1"
        return 1
    fi

    python_rgr_test_one "$python_test_identify__type" "$python_test_identify__file" "$2" &&
        python_rgr_test_all "$python_test_identify__type" "$2"
}

python_rgr_test_one()
{
    "${3:-:}" "test $2 ($1)"
    python_"$1" "$2"
}

python_rgr_test_all()
{
    "${2:-:}" "test all ($1)"
    python_"${1}"
}


########## test

PTYHON_TEST_DIRS="test/unit test tests/unit tests/unit_tests tests"

python_test_identify()
{
    python_test_identify__type=pytest
    python_test_identify__file=

    is_python_file "$1" || return 1

    case "$1" in
        test_*.py|*/test_*.py|*_test.py|*/*_test.py)
            python_test_identify__file="$1"
            ;;
        test/__init__.py|tests/__init__.py|test/*/__init__.py|tests/*/__init__.py)
            python_test_identify__file="${1%/__init__.py}/"
            ;;
        */conftest.py)
            python_test_identify__file="${1%/conftest.py}"
            ;;
        */__init__.py)
            python_test_guess || return 1
            python_test_identify__file="${1%/__init__.py}/"
            if [ -n "$python_test_guess__dir" ]; then
                python_test_identify__file="${python_test_guess__dir}/${python_test_identify__file#*/}"
             fi
            ;;
        setup.py)
            python_test_identify__file="$1"
            python_test_identify__type="package"
            ;;
        *.py|*/*.py)
            python_test_guess || return 1
            python_test_identify__filedir="${1%/*}"
            python_test_identify__filename="${1##*/}"
            for python_test_identify__file in "$python_test_identify__filedir/${python_test_identify__filename%.py}_test.py" "$python_test_identify__filedir/test_${python_test_identify__filename}"; do
                [ -r "$python_test_identify__file" ] && break
                [ -n "$python_test_guess__dir" ] && python_test_identify__file="${python_test_guess__dir}/${python_test_identify__file#*/}"
                [ -r "$python_test_identify__file" ] && break
            done
            ;;
    esac

    if $RGR_TEST_AUTOCREATE; then
        if [ -n "$python_test_identify__file" ] && [ "$python_test_identify__file" != "$1" ]; then
            [ -r "$python_test_identify__file" ] ||
                touch "$python_test_identify__file"
        fi
    fi
}

python_test_guess()
{
    python_test_guess__type=pytest
    for python_test_guess__dir in $PTYHON_TEST_DIRS; do
        if [ -d "$python_test_guess__dir" ]; then
            return
        fi
    done
    python_test_guess__dir=
}


########## clean

python_clean_cache()
{
    find . | grep -E '(__pycache__|\.pyc$|\.pyo$)' | xargs rm -rf
}

python_uninstall_all()
{
    pip freeze | xargs pip uninstall -y
}

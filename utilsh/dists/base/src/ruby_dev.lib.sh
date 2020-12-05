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


########## ruby

RUBY_RGR_SKIP_PATTERN='^db/schema.rb$'
RUBY_RGR_RUBOCOP_AUTO='-a'
RUBY_RGR_ERBLINT_AUTO='-a'
RUBY_TEST_FRAMEWORKS='minitest rspec cucumber'
RUBY_FEATURE_FRAMEWORKS='cucumber'
RUBY_MINITEST_DIRS='test tests'
RUBY_RSPEC_DIR=spec
RUBY_CUCUMBER_DIR=features

ruby_bundle()
{
    which bundle 2>/dev/null >/dev/null || {
        echo >&2 "missing bundle command"
        return 1
    }
    bundle "$@"
}

ruby_exec()
{
    if [ -r 'Gemfile' ]; then
        ruby_bundle exec "$@"
    else
        "$@"
    fi
}

ruby_ruby()
{
    ruby_exec ruby "$@"
}

ruby_rake()
{
    [ -r 'Rakefile' ] || return 1
    ruby_exec rake "$@"
}

ruby_rubocop()
{
    which rubocop 2>/dev/null >/dev/null || {
        echo >&2 "missing rubocop command"
        return 0
    }
    ruby_exec rubocop "$@"
}

ruby_reek()
{
    which reek 2>/dev/null >/dev/null || {
        echo >&2 "missing reek command"
        return 0
    }
    ruby_exec reek "$@"
}

ruby_flay()
{
    which flay 2>/dev/null >/dev/null || {
        echo >&2 "missing flay command"
        return 0
    }
    echo FIXME flay ...
}

ruby_flay_all_rb()
{
    which flay 2>/dev/null >/dev/null || {
        echo >&2 "missing flay command"
        return 0
    }
    ruby_flay_all_rb=.
    [ -d "lib" ] && ruby_flay_all_rb=lib

    find "$ruby_flay_all_rb" -name \*.rb | xargs flay
}

ruby_flog()
{
    which flog 2>/dev/null >/dev/null || {
        echo >&2 "missing flog command"
        return 0
    }
    echo FIXME flog ...
}

ruby_flog_all_rb()
{
    which flog 2>/dev/null >/dev/null || {
        echo >&2 "missing flog command"
        return 0
    }
    ruby_flog_all_rb=.
     [ -d "lib" ] && ruby_flog_all_rb=lib

    find "$ruby_flog_all_rb" -name \*.rb | xargs flog
}

ruby_erblint()
{
    which erblint 2>/dev/null >/dev/null || {
        echo >&2 "missing erblint command"
        return 0
    }
    ruby_exec erblint "$@"
}

ruby_bundle_install()
{
    ruby_bundle install
}


########## rgr

DEVEL_LANG="${DEVEL_LANG:+$DEVEL_LANG }ruby gemfile gemfile_lock embedded_ruby ruby_feature"

is_ruby_file()
{
    [ -d "$1" ] && return 1

    case "$1" in
        *.rb)
            return 0
    esac
    return 1
}

is_gemfile_file()
{
    [ -d "$1" ] && return 1

    case "$1" in
        "Gemfile"|*.gemspec)
            return 0 ;;
    esac
    return 1
}

gemfile_rgr()
{
    ruby_rgr_bundle_install "$@" &&
        ruby_rgr_rubocop "$@"
}

is_gemfile_lock_file()
{
    [ -f "$1" ] || return 1
    [ "$1" = "Gemfile.lock" ] || return 1

    is_git_tracked "$1" && return 0
    return 1
}

gemfile_lock_rgr()
{
    return 0
}

is_embedded_ruby_file()
{
    [ -d "$1" ] && return 1

    case "$1" in
        *.erb)
            return 0 ;;
    esac
    return 1
}

embedded_ruby_rgr()
{
    if ! ruby_rgr_test "$@"; then
        [ $RGR_STRICT_LEVEL -ge 1 ] && return 1
    fi

    if ! ruby_rgr_erblint "$@"; then
        [ $RGR_STRICT_LEVEL -ge 1 ] && return 1
    fi

    return 0
}

ruby_rgr_erblint()
{
    "${2:-:}" "erblint $1"
    ruby_rgr_erblint__flag=
    if $RGR_AUTO; then
        ruby_rgr_erblint__flag="$RUBY_RGR_ERBLINT_AUTO"
    fi
    ruby_erblint $ruby_rgr_erblint__flag "$1"
}

ruby_rgr_bundle_install()
{
    "${2:-:}" "bundle install"
    ruby_bundle_install
}

is_ruby_feature_file()
{
    [ -d "$1" ] && return 1

    case "$1" in
        *.feature)
            return 0
    esac
    return 1
}

ruby_feature_rgr()
{
    if expr "$1" : "$RUBY_FEATURE_RGR_SKIP_PATTERN" >/dev/null; then
        return 0
    fi

    if ! ruby_feature_rgr_test "$@"; then
        [ $RGR_STRICT_LEVEL -ge 1 ] && return 1
    fi
    if ! ruby_feature_rgr_lint "$@"; then
        [ $RGR_STRICT_LEVEL -ge 1 ] && return 1
    fi

}

ruby_feature_rgr_test()
{
    ruby_rgr_test_v2__FRAMEWORKS="$RUBY_FEATURE_FRAMEWORKS" ruby_rgr_test_v2 "$@"
}

ruby_feature_rgr_lint()
{
    "${2:-:}" "gherkin-lint $1"

    # FIXME: find correct gherkin linter
    :
}

ruby_rgr()
{
    if expr "$1" : "$RUBY_RGR_SKIP_PATTERN" >/dev/null; then
        return 0
    fi

    if ! ruby_rgr_test "$@"; then
        [ $RGR_STRICT_LEVEL -ge 1 ] && return 1
    fi
    if ! ruby_rgr_rubocop "$@"; then
        [ $RGR_STRICT_LEVEL -ge 1 ] && return 1
    fi
    if ! ruby_rgr_reek "$@"; then
        [ $RGR_STRICT_LEVEL -ge 3 ] && return 1
    fi
    if ! ruby_rgr_flay "$@"; then
        [ $RGR_STRICT_LEVEL -ge 3 ] && return 1
    fi
    if ! ruby_rgr_flog "$@"; then
        [ $RGR_STRICT_LEVEL -ge 3 ] && return 1
    fi

    return 0
}

ruby_rgr_rubocop()
{
    "${2:-:}" "rubocop $1"
    ruby_rgr_rubocop__flag=
    if $RGR_AUTO; then
        ruby_rgr_rubocop__flag="$RUBY_RGR_RUBOCOP_AUTO"
    fi
    ruby_rubocop -f fu $ruby_rgr_rubocop__flag "$1"
}

ruby_rgr_reek()
{
    "${2:-:}" "reek $1"
    ruby_reek "$1"
}

ruby_rgr_flay()
{
    "${2:-:}" "flay *rb"
    ruby_flay_all_rb
}

ruby_rgr_flog()
{
    "${2:-:}" "flog *rb"
    ruby_flog_all_rb
}

ruby_rgr_test()
{
    ruby_rgr_test_v2__FRAMEWORKS="$RUBY_TEST_FRAMEWORKS" ruby_rgr_test_v2 "$@"
}

ruby_rgr_test_v1()
{
    ruby_rgr_test__frameworks=

    ruby_rgr_test__BIFS="$IFS"
    IFS="
"
    for ruby_test_identify__t in $(ruby_test_identify "$1"); do
        ruby_test_identify__t_framework="${ruby_test_identify__t%%:*}"
        ruby_test_identify__t_file="${ruby_test_identify__t#$ruby_test_identify__t_framework}"
        ruby_test_identify__t_file="${ruby_test_identify__t_file#:}"

        case $ruby_rgr_test__frameworks in
            *"$ruby_rgr_test__frameworks"*) ;;
            *) ruby_rgr_test__frameworks="${ruby_rgr_test__frameworks:+$ruby_rgr_test__frameworks }$ruby_rgr_test__framework"
        esac
    done
    IFS=="$ruby_rgr_test__BIFS"

    # if ! ruby_test_identify "$1"; then
    #     "${2:-:}" "no test found for $1"
    #     return 1
    # fi

    # ruby_rgr_test__frameworks=
    # for ruby_rgr_test__t in $ruby_test_identify; do
    #     ruby_rgr_test__framework="${ruby_rgr_test__t%%:*}"
    #     ruby_rgr_test__test_file="${ruby_rgr_test__t#$ruby_rgr_test__framework}"
    #     ruby_rgr_test__test_file="${ruby_rgr_test__test_file#:}"

    #     case $ruby_rgr_test__frameworks in
    #         *"$ruby_rgr_test__framework"*) ;;
    #         *)
    #             ruby_rgr_test__frameworks="${ruby_rgr_test__frameworks:+$ruby_rgr_test__frameworks }$ruby_rgr_test__framework"
    #             ;;
    #     esac

    #     if [ -n "$ruby_rgr_test__test_file" ]; then
    #         ruby_rgr_test_one "$ruby_rgr_test__framework" "$ruby_rgr_test__test_file" "$2" || return 1
    #     fi
    # done

    # for ruby_rgr_test__framework in $ruby_rgr_test__frameworks; do
    #     ruby_rgr_test_all "$ruby_rgr_test__framework" "$2" || return 1
    # done
}

ruby_rgr_test_v2()
{
    ruby_rgr_test_v2__tested=FALSE
    ruby_rgr_test_v2__frameworks=

    case "$1" in
        db/*)
            ;;
        # config/*)
        #     ;;
        *)
            for ruby_rgr_test_v2__framework in ${ruby_rgr_test_v2__FRAMEWORKS:-$RUBY_TEST_FRAMEWORKS}; do
                if ruby_has_${ruby_rgr_test_v2__framework}; then
                    ruby_rgr_test_v2__frameworks="$ruby_rgr_test_v2__frameworks $ruby_rgr_test_v2__framework"

                    if ruby_${ruby_rgr_test_v2__framework}_identify "$1"; then
                        eval ruby_rgr_test_v2__test_file=\"\$ruby_${ruby_rgr_test_v2__framework}_identify\"
                        if [ -z "$ruby_rgr_test_v2__test_file" ] ; then
                            ruby_rgr_test_v2__tested=TRUE
                            : # just test all
                        else
                            if [ ! -r "$ruby_rgr_test_v2__test_file" ] && $RGR_TEST_AUTOCREATE; then
                                ruby_${ruby_rgr_test_v2__framework}_create_test_file "$ruby_rgr_test_v2__test_file"
                            fi

                            if [ -r "$ruby_rgr_test_v2__test_file" ]; then
                            ruby_rgr_test_v2__tested=TRUE
                            ruby_rgr_test_one "$ruby_rgr_test_v2__framework" "$ruby_rgr_test_v2__test_file" "$2" || return 1
                            # else
                            #     echo DBG: $ruby_rgr_test_v2__test_dir
                            #     echo DBG: $ruby_rgr_test_v2__test_file
                            fi
                        fi
                    # else
                    #     ruby_rgr_test_dont_know "$1" "$2" || return 1
                    fi
                fi
            done

            if ! $ruby_rgr_test_v2__tested; then
                if [ $RGR_STRICT_LEVEL -ge 2 ]; then
                    ruby_rgr_test_missing "$1" "$2"
                    return 1
                fi
            fi

            if [ -z "$ruby_rgr_test_v2__frameworks" ]; then
                ruby_rgr_test_dont_know "$1" "$2" || return 1
            fi

            for ruby_rgr_test_v2__framework in $ruby_rgr_test_v2__frameworks; do
                ruby_rgr_test_all "$ruby_rgr_test_v2__framework" "$2" || return 1
            done
            ;;
    esac
}

ruby_rgr_test_one()
{
    "${3:-:}" "test $2 ($1)"
    ruby_"$1" "$2"
}

ruby_rgr_test_all()
{
    "${2:-:}" "test all ($1)"
    ruby_"${1}"_all
}

ruby_rgr_test_missing()
{
    "${2:-:}" "test MISSING for $1"
    return 1
}

ruby_rgr_test_dont_know()
{
    "${2:-:}" "NO TEST RULES for $1"
    return 1
}


########## test

ruby_test_identify()
{
    ruby_test_identify=

    is_ruby_file "$1" || return 1

    ruby_test_frameworks
    ruby_test_identify__frameworks="$ruby_test_frameworks"

    case "$1" in
        spec/spec_helper.rb|spec/rails_helper.rb)
            ruby_test_identify__frameworks=rspec
            ;;
        test/test_helper.rb|tests/test_helper.rb)
            ruby_test_identify__frameworks=minitest
            ;;
        config/*)
            ;;
        db/*)
            ;;
        *_spec.rb)
            ruby_test_identify="rspec:$1"
            ruby_test_identify__frameworks=rspec
            ;;
        spec/*.rb)
            ruby_test_identify="rspec:${1%\.rb}_spec.rb"
            ruby_test_identify__frameworks=rspec
            ;;
        *_test.rb)
            ruby_test_identify="minitest:$1"
            ruby_test_identify__frameworks=minitest
            ;;
        test/*.rb|tests/*.rb)
            ruby_test_identify="minitest:${1%\.rb}_test.rb"
            ruby_test_identify__frameworks=minitest
            ;;
        *.rb)
            ruby_test_identify__should=
            for ruby_test_identify__framework in $ruby_test_frameworks; do
                ruby_${ruby_test_identify__framework}_identify_test "$1"
                ruby_test_identify__status=$?
                eval ruby_test_identify__found="\"$ruby_test_identify__framework:\$ruby_${ruby_test_identify__framework}_identify_test\""

                if [ $ruby_test_identify__status -eq 0 ]; then
                    ruby_test_identify="${ruby_test_identify:+$ruby_test_identify }$ruby_test_identify__found"
                else
                    [ -z "$ruby_test_identify__bkp" ] && ruby_test_identify__bkp="$ruby_test_identify__found"
                fi
            done

            [ -z "$ruby_test_identify" ] && ruby_test_identify="$ruby_test_identify__bkp"
            ;;
    esac

    # if $RUBY_TEST_AUTOCREATE; then
    #     if [ -n "$ruby_test_identify__file" ] && [ "$ruby_test_identify__file" != "$1" ]; then
    #         [ -r "$ruby_test_identify__file" ] ||
    #             touch "$ruby_test_identify__file"
    #     fi
    # fi

    ruby_test_identify="${ruby_test_identify:+$ruby_test_identify }$ruby_test_identify__frameworks"
}

ruby_test_frameworks()
{
    ruby_test_frameworks=

    if ruby_has_minitest; then
        ruby_test_frameworks="${ruby_test_frameworks:+$ruby_test_frameworks }minitest"
    fi
    if ruby_has_rspec; then
        ruby_test_frameworks="${ruby_test_frameworks:+$ruby_test_frameworks }rspec"
    fi
    if [ -z "$ruby_test_frameworks" ]; then
        ruby_test_frameworks=ruby
    fi
}

########## minitest

ruby_has_minitest()
{
    for ruby_has_minitest in $RUBY_MINITEST_DIRS; do
        [ -d "$ruby_has_minitest" ] && return 0
    done
    return 1
}

ruby_minitest_identify_test()
{
    ruby_minitest_identify_test=

    ruby_minitest_identify_test__file="$1"
    ruby_minitest_identify_test__test_file="${ruby_minitest_identify_test__file%\.rb}_test.rb"

    ruby_minitest_identify_test__test_file_pre="${ruby_minitest_identify_test__test_file%%/*}"
    ruby_minitest_identify_test__test_file_sub="${ruby_minitest_identify_test__test_file#$ruby_minitest_identify_test__test_file_pre/}"

    # check minitest dir
    for ruby_minitest_identify_test_d in $_RUBY_MINITEST_DIRS; do
        [ -d "$ruby_minitest_identify_test_d" ] || continue

        ruby_minitest_identify_test="$ruby_minitest_identify_test_d/$ruby_minitest_identify_test__test_file"
        [ -r "$ruby_minitest_identify_test" ] && return 0

        ruby_minitest_identify_test="$ruby_minitest_identify_test_d/$ruby_minitest_identify_test__test_file_sub"
        [ -r "$ruby_minitest_identify_test" ] && return 0
    done

    # along
    ruby_minitest_identify_test="$ruby_minitest_identify_test__test_file"
    [ -r "$ruby_minitest_identify_test" ] && return 0

    # should
    ruby_minitest_identify_test="$_RUBY_MINITEST_DIR/$ruby_minitest_identify_test__test_file_sub"
    [ -r "$ruby_minitest_identify_test" ] && return 0

    return 1
}

ruby_minitest_create_test_file()
{
    mkdir -p "${1%/*}" &&
        touch "$1"
}

ruby_minitest()
{
    ruby_rake test TEST="$1" ||
        ruby_ruby "$1"
}

ruby_minitest_all()
{
    ruby_rake test ||
        return 0
}


########## rspec

ruby_has_rspec()
{
    for ruby_has_rspec in $RUBY_RSPEC_DIR; do
        [ -d "$ruby_has_rspec" ] && return 0
    done
    return 1
}

ruby_rspec_identify()
{
    ruby_rspec_identify=

    ruby_rspec_identify__dir="$RUBY_RSPEC_DIR"
    ruby_rspec_identify__file="$1"

    ruby_rspec_identify__file_name="${1##*/}"
    ruby_rspec_identify__file_path="${1%$ruby_rspec_identify__file_name}"
    ruby_rspec_identify__file_path="${ruby_rspec_identify__file_path%/}"
    ruby_rspec_identify__spec_name="${ruby_rspec_identify__file_name%.rb}_spec.rb"

    case "$1" in
        config/*)
            ;;
        features/*)
            return 1
            ;;
        test/*|tests/*)
            return 1
            ;;
        "$ruby_rspec_identify__dir"/spec_helper.rb|"$ruby_rspec_identify__dir"/rails_helper.rb)
            ;;
        "$ruby_rspec_identify__dir"/*_spec.rb)
            ruby_rspec_identify="$1"
            ;;
        "$ruby_rspec_identify__dir"/*.rb)
            if [ -r "$ruby_rspec_identify__file_path/$ruby_rspec_identify__spec_name" ]; then
                ruby_rspec_identify="$ruby_rspec_identify__file_path/$ruby_rspec_identify__spec_name"
            else
                ruby_rspec_identify="$ruby_rspec_identify__file_path"
            fi
            ;;
        *.erb)
            ;;
        *.rb)
            ruby_rspec_identify__file_path_first="${ruby_rspec_identify__file_path%%/*}"
            ruby_rspec_identify__file_path_sub="${ruby_rspec_identify__file_path#$ruby_rspec_identify__file_path_first/}"

            ruby_rspec_identify="$ruby_rspec_identify__dir/$ruby_rspec_identify__file_path/$ruby_rspec_identify__spec_name"
            [ -r "$ruby_rspec_identify" ] && return 0

            ruby_rspec_identify="$ruby_rspec_identify__dir/$ruby_rspec_identify__file_path_sub/$ruby_rspec_identify__spec_name"
            [ -r "$ruby_rspec_identify" ] && return 0

            # try along ?

            return 0
            ;;
        *)
            return 1
            ;;
    esac
}

ruby_rspec_identify_test()
{
    ruby_rspec_identify_test=

    ruby_rspec_identify_test__file="$1"
    ruby_rspec_identify_test__test_file="${ruby_rspec_identify_test__file%\.rb}_spec.rb"

    ruby_rspec_identify_test__test_file_pre="${ruby_rspec_identify_test__test_file%%/*}"
    ruby_rspec_identify_test__test_file_sub="${ruby_rspec_identify_test__test_file#$ruby_rspec_identify_test__test_file_pre/}"

    # check rspec dir
    if [ -d "$_RUBY_RSPEC_DIR" ]; then
        ruby_rspec_identify_test="$_RUBY_RSPEC_DIR/$ruby_rspec_identify_test__test_file"
        [ -r "$ruby_rspec_identify_test" ] && return 0

        ruby_rspec_identify_test="$_RUBY_RSPEC_DIR/$ruby_rspec_identify_test__test_file_sub"
        [ -r "$ruby_rspec_identify_test" ] && return 0
    fi

    # along
    ruby_rspec_identify_test="$ruby_rspec_identify_test__test_file"
    [ -r "$ruby_rspec_identify_test" ] && return 0

    # should
    ruby_rspec_identify_test="$_RUBY_RSPEC_DIR/$ruby_rspec_identify_test__test_file_sub"
    [ -r "$ruby_rspec_identify_test" ] && return 0

    return 1
}

ruby_rspec_create_test_file()
{
    mkdir -p "${1%/*}" &&
        touch "$1"
}

ruby_rspec()
{
    if [ -x 'bin/rspec' ]; then
        ./bin/rspec "$@"
        return "$?"
    fi

    which rspec 2>/dev/null >/dev/null || {
        echo >&2 "missing rspec command"
        return 0
    }

    ruby_exec rspec "$@"
}

ruby_rspec_all()
{
    ruby_rspec
}


########## cucumber

ruby_has_cucumber_cmd()
{
    which cucumber 2>/dev/null >/dev/null
}

ruby_has_cucumber()
{
    ruby_has_cucumber_cmd || return 1

    for ruby_has_rspec in $RUBY_CUCUMBER_DIR; do
        [ -d "$ruby_has_rspec" ] && return 0
    done
    return 1
}

ruby_cucumber_identify()
{
    ruby_cucumber_identify=

    ruby_cucumber_identify__dir="$RUBY_CUCUMBER_DIR"
    ruby_cucumber_identify__file="$1"

    case "$1" in
        spec/*)
            return 1
            ;;
        test/*|tests/*)
            return 1
            ;;
        "$ruby_cucumber_identify__dir"/support/*.rb)
            ;;
        "$ruby_cucumber_identify__dir"/step_definitions/*.rb)
            ;;
        "$ruby_cucumber_identify__dir"/*.feature)
            ruby_cucumber_identify="$1"
            ;;
        *)
            return 1
            ;;
    esac
}

ruby_cucumber()
{
    for ruby_cucumber__shortcut in ./bin/cucumber ./script/cucumber; do
        [ -x "$ruby_cucumber__shortcut" ] || continue
        ./"$ruby_cucumber__shortcut" "$@"
        return "$?"
    done

    ruby_has_cucumber_cmd 2>/dev/null >/dev/null || {
        echo >&2 "missing cucumber command"
        return 0
    }

    ruby_exec cucumber "$@"
}

ruby_cucumber_all()
{
    ruby_cucumber
}


########## rails

rails_bootstrap()
{
    # 0. clean Gemfile
    if [ -r "Gemfile" ]; then
        if egrep "^gem ['\"](bootstrap|jquery-rails)['\"]" Gemfile >/dev/null; then
            egrep -v "^gem ['\"](bootstrap|jquery-rails)['\"]" Gemfile > Gemfile.new &&
                mv Gemfile.new Gemfile

            bundle
        fi
    fi

    # 1. yarn install
    yarn add bootstrap jquery popper.js

    # 2. config/webpack/environment.js
    if ! grep jQuery config/webpack/environment.js >/dev/null; then
        ruby -plne "print \"const webpack = require('webpack')
environment.plugins.append(
    'Provide',
    new webpack.ProvidePlugin({
        \$: 'jquery',
        jQuery: 'jquery',
        Popper: ['popper.js', 'default']
    })
)
\" if /module.exports = environment/" config/webpack/environment.js > config/webpack/environment.js.new &&
            mv config/webpack/environment.js.new config/webpack/environment.js
    fi

    # 3. app/javascript/packs/application.js
    grep '^require ("bootstrap")' app/javascript/packs/application.js >/dev/null ||
        echo 'require ("bootstrap")' >> app/javascript/packs/application.js

    # 4. app/assets/stylesheets/application.css
    if [ ! -r app/assets/stylesheets/application.scss ]; then
        if [ -r app/assets/stylesheets/application.css ]; then
            mv app/assets/stylesheets/application.css app/assets/stylesheets/application.scss
        else
            touch app/assets/stylesheets/application.scss
        fi
    fi
    grep '^@import "bootstrap/scss/bootstrap";' app/assets/stylesheets/application.scss >/dev/null ||
        echo '@import "bootstrap/scss/bootstrap";' >> app/assets/stylesheets/application.scss
}

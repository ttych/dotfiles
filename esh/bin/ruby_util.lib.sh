#!/bin/sh

SCRIPT_NAME="${0##*/}"
SCRIPT_RPATH="${0%$SCRIPT_NAME}"
SCRIPT_PATH=`cd "${SCRIPT_RPATH:-.}" && pwd`



echo2()
{
    echo >&2 "$@"
}

has_ruby()
{
    which ruby >/dev/null 2>/dev/null ||
        echo >&2 No ruby available
}



######################################### gem::pack
# text
GEM_PACK_ASCIIDOC="asciidoctor asciidoctor-pdf asciidoctor-revealjs rouge"
GEM_PACK_DOC="gem-man ronn"
GEM_PACK_TEXT="hexapdf octodown $GEM_PACK_ASCIIDOC $GEM_PACK_DOC"
# deployment
GEM_PACK_CAPISTRANO="capistrano"
GEM_PACK_MINA="mina"
# fluentd
GEM_PACK_FLUENTD="fluentd fluent-plugin-kafka fluent-plugin-elasticsearch fluent-plugin-rewrite-tag-filter fluent-plugin-rename-key fluent-plugin-record-modifier fluent-plugin-flowcounter-simple fluent-plugin-http-healthcheck"
# www
GEM_PACK_JEKYLL="jekyll"
GEM_PACK_MIDDLEMAN="middleman middleman-livereload middleman-autoprefixer middleman-blog"
GEM_PACK_NANOC="nanoc"
GEM_PACK_SINATRA="sinatra"
GEM_PACK_RACK="rack shotgun"
GEM_PACK_WWW="puma webrick $GEM_PACK_MIDDLEMAN $GEM_PACK_JEKYLL $GEM_PACK_NANOC $GEM_PACK_SINATRA"
# rails
GEM_PACK_RAILS="rails rails-erb-lint rspec-rails"
# puppet
GEM_PACK_PUPPET="metadata-json-lint pdk puppet-lint"
# test
GEM_PACK_MINITEST="minitest"
GEM_PACK_RSPEC="rspec"
GEM_PACK_TEST="$GEM_PACK_MINITEST $GEM_PACK_RSPEC $GEM_PACK_QUALITY aruba cucumber"
# tools / utils
GEM_PACK_TOOLS="quick_and_ruby tmuxinator"
GEM_PACK_UTILS="$GEM_PACK_TOOLS"
# dev / code
GEM_PACK_RUBY="prism pry rbs geminabox"
GEM_PACK_QUALITY="rubocop reek flay flog erb_lint"
GEM_PACK_CLI="gli main"
GEM_PACK_LSP="solargraph ruby-lsp"
GEM_PACK_CODE="bump rdoc semver yard $GEM_PACK_CLI $GEM_PACK_TEST $GEM_PACK_QUALITY $GEM_PACK_RUBY $GEM_PACK_LSP"
GEM_PACK_DEV="$GEM_PACK_CODE"
# default
GEM_PACK_DEFAULT="$GEM_PACK_RUBY $GEM_PACK_UTILS $GEM_PACK_DEV $GEM_PACK_MIDDLEMAN hexapdf"


_gem_pack_1()
{
    _gem_pack_1="$1"

    _gem_pack_1__content=
    if [ "$_gem_pack_1" = "all" ]; then
        for _gem_pack_1__pack in `set | egrep -a '^GEM_PACK_[A-Z]*=' | cut -d '=' -f 1`; do
            eval _gem_pack_1__content="\${_gem_pack_1__content:+$_gem_pack_1__content }\$$_gem_pack_1__pack"
        done
    else
        eval _gem_pack_1__content="\${GEM_PACK_`echo $gem_pack | tr '[a-z]' '[A-Z]'`}"
    fi

    if [ -z "$_gem_pack_1__content" ]; then
        echo >&2 "gem pack \"$gem_pack\" is empty"
        return 2
    fi

    gem install $_gem_pack_1__content
    return $?
}

gem_pack()
{
    if [ $# -eq 0 ]; then
        echo Choose pack in:
        for gem_pack in $(set | grep '^GEM_PACK_.*=' | sed -e 's/GEM_PACK_\([^=]*\).*/\1/g' | tr '[A-Z]' '[a-z]') ; do
            echo "  $gem_pack"
        done
        return 0
    fi

    gem_pack__ret=0
    for gem_pack; do
        _gem_pack_1 "$gem_pack" ||
            gem_pack__ret=1
    done
    return $gem_pack__ret
}



######################################### ruby::util
uuid_gen()
{
    has_ruby || return 1
    ruby -rsecurerandom -e 'puts SecureRandom.uuid'
}

file_inspect()
{
    has_ruby || return 1
    ruby -r pp -e "puts File.read('$1').inspect"
}

json_inspect()
{
    has_ruby || return 1
    ruby -r json -e "puts JSON.parse(ARGF.read).to_json.inspect" "$@"
}

json_pp()
{
    has_ruby || return 1
    ruby -r json -e "puts JSON.pretty_generate(JSON.parse(ARGF.read))" "$@"
}

yaml_inspect()
{
    has_ruby || return 1
    ruby  -r yaml -e "puts YAML.load(ARGF.read).to_yaml.inspect" "$@"
}

yaml_pp()
{
    has_ruby || return 1
    ruby  -r yaml -e "puts YAML.dump(YAML.load(ARGF.read))" "$@"
}

xml_inspect()
{
    has_ruby || return 1
    ruby -r nokogiri -e "def pp_xml(xml='') doc = Nokogiri.XML(xml) { |config| config.default_xml.noblanks } ; puts doc.to_xml(indent: 4) ; xml ; end ; pp_xml(File.read('$1'))"
}

now()
{
    digits="${1:-3}"
    utc="${utc:-false}"
    # ruby -r time -e "def p_now(format='', utc=false); t=Time.now ; format='%Y-%m-%dT%H:%M:%S.%L%z' if format.empty?; t=t.utc if utc ; puts t.strftime(format); end ; p_now('$1', $utc)"
    ruby -r time -e "t = Time.now ; t=t.utc if $utc ; puts t.iso8601($digits)"
}

now_utc()
{
    utc=true now "$@"
}

rcat()
{
    has_ruby || return 1
    ruby -e "puts ARGF.read" "$@"
}



######################################### main
case "$SCRIPT_NAME" in
    gem_*)
        "$SCRIPT_NAME" "$@"
        ;;
    uuid*|now*)
        "$SCRIPT_NAME" "$@"
        ;;
    *_inspect|*_pp)
        "$SCRIPT_NAME" "$@" ;;
    rcat)
        "$SCRIPT_NAME" "$@" ;;
esac

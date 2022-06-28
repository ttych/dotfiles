#!/bin/sh
# -*- mode: sh -*-


SCRIPT_NAME="${0##*/}"


has_ruby()
{
    which ruby >/dev/null 2>/dev/null ||
        echo >&2 No ruby available
}

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
    ruby -r json -e "puts JSON.parse(File.read('$1')).to_json.inspect"
}

json_pp()
{
    has_ruby || return 1
    ruby -r json -e "puts JSON.pretty_generate(JSON.parse(File.read('$1')))"
}

yaml_inspect()
{
    has_ruby || return 1
    ruby  -r yaml -e "puts YAML.load_file('$1').to_yaml.inspect"
}

yaml_pp()
{
    has_ruby || return 1
    ruby  -r yaml -e "puts YAML.dump(YAML.load_file('$1'))"
}

xml_inspect()
{
    has_ruby || return 1
    ruby -r nokogiri -e "def pp_xml(xml='') doc = Nokogiri.XML(xml) { |config| config.default_xml.noblanks } ; puts doc.to_xml(indent: 4) ; xml ; end ; pp_xml(File.read('$1'))"
}

now()
{
    utc="${utc:-false}"
    ruby -r time -e "def p_now(format='', utc=false); t=Time.now ; format='%Y-%m-%dT%H:%M:%S.%L%z' if format.empty?; t=t.utc if utc ; puts t.strftime(format); end ; p_now('$1', $utc)"
}

now_utc()
{
    utc=true now "$@"
}


### main

case "$SCRIPT_NAME" in
    uuid_gen|now|now_utc)
        "$SCRIPT_NAME" "$@" ;;
    *_inspect|*_pp)
        "$SCRIPT_NAME" "$@" ;;
esac

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
    ruby -r pp -e "pp File.read('$1')"
}

json_inspect()
{
    has_ruby || return 1
    ruby -r json -e "puts JSON.pretty_generate(JSON.parse(File.read('$1')))"
}

yaml_inspect()
{
    has_ruby || return 1
    ruby  -r yaml -e "puts YAML.dump(YAML.load_file('$1'))"
}

xml_inspect()
{
    has_ruby || return 1
    ruby -r nokogiri -e "def pp_xml(xml='') doc = Nokogiri.XML(xml) { |config| config.default_xml.noblanks } ; puts doc.to_xml(indent: 2) ; xml ; end ; pp_xml(File.read('$1'))"
}


### main

case "$SCRIPT_NAME" in
    uuid_gen)
        "$SCRIPT_NAME" "$@" ;;
    *_inspect)
        "$SCRIPT_NAME" "$@" ;;
esac

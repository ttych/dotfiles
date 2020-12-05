#!/bin/sh
# -*- mode: sh -*-


SCRIPT_NAME="${0##*/}"


### client

curl_json()
{
    curl -H 'Cache-Control: no-cache' \
         -H 'Content-Type: application/json' \
         -H 'Accept: application/json' \
         "$@"
}


### server
# from https://gist.github.com/willurd/5720255

httpd_python_2()
{
    python -m SimpleHTTPServer "${1:-8000}"
}

httpd_python_3()
{
    python -m http.server "${1:-8000}"
}

httpd_twisted()
{
    twistd -n web -p "${1:-8000}" --path .
}

httpd_twisted_alt()
{
    python -c "from twisted.web.server import Site; from twisted.web.static import File; from twisted.internet import reactor; reactor.listenTCP(${1:-8000}, Site(File('.'))); reactor.run()"
}

# from http://barkingiguana.com/2010/04/11/a-one-line-web-server-in-ruby/
httpd_ruby_webrick()
{
    ruby -rwebrick -e "WEBrick::HTTPServer.new(:Port => ${1:-8000}, :DocumentRoot => Dir.pwd).start"
}

# from https://gist.github.com/willurd/5720255#comment-855952
httpd_ruby()
{
    ruby -run -ehttpd . -p "${1:-8000}"
}

# from https://gist.github.com/willurd/5720255/#comment-841393
httpd_adsf()
{
    # gem install adsf
    adsf -p "${1:-8000}"
}

httpd_sinatra()
{
    # gem install sinatra
    ruby -rsinatra -e"set :public_folder, '.'; set :port, ${1:-8000}"
}

# from http://www.perlmonks.org/?node_id=865239
httpd_perl()
{
    # cpan HTTP::Server::Brick
    perl -MHTTP::Server::Brick -e '$s=HTTP::Server::Brick->new(port=>8000); $s->mount("/"=>{path=>"."}); $s->start'
}

# http://advent.plackperl.org/2009/12/day-5-run-a-static-file-web-server-with-plack.html
httpd_plack()
{
    # cpan Plack
    plackup -MPlack::App::Directory -e 'Plack::App::Directory->new(root=>".");' -p 8000
}

httpd_mojolicious()
{
    # cpan Mojolicious::Lite
    perl -MMojolicious::Lite -MCwd -e 'app->static->paths->[0]=getcwd; app->start' daemon -l http://*:8000
}

httpd_nodejs()
{
    # npm install -g http-server
    http-server -p 8000
}

httpd_node_static()
{
    # npm install -g node-static
    static -p 8000
}

# from http://www.reddit.com/r/webdev/comments/1fs45z/list_of_ad_hoc_http_server_oneliners/cad9ew3
# from https://gist.github.com/willurd/5720255#comment-841131
httpd_php()
{
    php -S 127.0.0.1:8000
}

# from https://gist.github.com/willurd/5720255/#comment-841166
httpd_erlang()
{
    erl -s inets -eval 'inets:start(httpd,[{server_name,"NAME"},{document_root, "."},{server_root, "."},{port, 8000},{mime_types,[{"html","text/html"},{"htm","text/html"},{"js","text/javascript"},{"css","text/css"},{"gif","image/gif"},{"jpg","image/jpeg"},{"jpeg","image/jpeg"},{"png","image/png"}]}]).'
}

# from https://gist.github.com/willurd/5720255#comment-841915
httpd_busybox()
{
    busybox httpd -f -p 8000
}

# from http://linux.bytesex.org/misc/webfs.html
httpd_webfs()
{
    webfsd -F -p 8000
}

whttpd()
{
    httpd_ruby ||
        httpd_python_3 ||
        httpd_python_2
}


### url

url_encode_p()
{
    python -c 'import sys; from urllib import parse; print(parse.quote(sys.argv[1], safe=""))' "$@"
}

url_encode_r()
{
    ruby -e "require 'erb'; ARGV.each {|l| puts ERB::Util.url_encode(l) }" -- "$@"
}

url_encode()
{
    if which ruby >/dev/null 2>/dev/null; then
        url_encode_r "$@"
        return $?
    fi

    if which python >/dev/null 2>/dev/null; then
        url_encode_p "$@"
        return $?
    fi

    return 1
}



### main

case "$SCRIPT_NAME" in
    whttpd) "$SCRIPT_NAME" "$@"
             ;;
    curl_*) "$SCRIPT_NAME" "$@"
            ;;
esac

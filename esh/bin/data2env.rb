#!/usr/bin/env ruby

require 'optparse'

class Data2env
  VERSION = '0.1.0'

  def initialize(prefix: nil, downcase: false, upcase: false, **options)
    @prefix = prefix
    @downcase = downcase
    @upcase = upcase
    @options = options
  end

  def convert(data, path = nil)
    case data
    when Hash
      data.flat_map { |k, v| convert(v, join_hash(path, k)) }
    when Array
      data.each_with_index.flat_map { |v, i| convert(v, join_index(path, i)) }
    else
      [assign(path, data)]
    end
  end

  def join_hash(path, key)
    path ? "#{path}__#{key}" : key.to_s
  end

  def join_index(path, index)
    path ? "#{path}_#{index}" : index.to_s
  end

  def assign(path, value)
    name = "#{@prefix}#{path}"
    name = name.upcase if @upcase
    name = name.downcase if @downcase
    val  = value.to_s.gsub(/["\\$`]/) { |m| "\\#{m}" }
    %(#{name}="#{val}")
  end

  class << self
    def run(program_name: $PROGRAM_NAME, argf: ARGF, argv: ARGV)
      @program_name = program_name
      @program_sname = File.basename(@program_name)

      options = parse_options(argv: argv)
      data = load_data(reader: argf)
      envs = new(**options).convert(data)
      puts envs
      0
    rescue  StandardError => e
      $stderr.puts "Error: #{e.message}"
      1
    end

    def parse_options(argv: ARGV.dup)

      options = {}

      OptionParser.new do |opts|
        opts.banner = "Usage: #{$0} [options]"
        opts.on('-p', '--prefix PREFIX', 'Préfixe pour toutes les variables') do |prefix|
          options[:prefix] = prefix
        end

        opts.on('-d', '--downcase', 'Garder les clés en minuscules (défaut: majuscules)') do
          options[:downcase] = true
        end

        opts.on('-u', '--upcase', 'Garder les clés en minuscules (défaut: majuscules)') do
          options[:upcase] = true
        end

        opts.on('-h', '--help', 'Afficher cette aide') do
          puts opts
          exit 0
        end

        opts.on('-v', '--version', 'Afficher la version') do
          puts "#{@program_sname} #{VERSION}"
          exit 0
        end
      end.parse!(argv)

      options
    end

    def load_data(reader:)
      data = reader.read
      case @program_sname
      when /^yaml/i
        require 'yaml'

        YAML.safe_load(
          data,
          permitted_classes: [Symbol, Date, Time],
          aliases: true
        )
      when /^json/i
        require 'json'

        JSON.parse(data)
      else
        raise "cannot guess format from #{@program_sname}"
      end
    end
  end
end

exit Data2env.run

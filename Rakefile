require 'rubygems'
require 'hoe'
require './lib/fuzed.rb'

Hoe.new('fuzed', Fuzed::VERSION) do |p|
  p.rubyforge_name = 'fuzed'
  p.author = ['Dave Fayram', 'Tom Preston-Werner']
  p.email = ['tom@rubyisawesome.com']
  p.summary = 'Leverage the YAWS webserver (and additional erlang-based infrastructure) to run Rails.'
  p.url = 'fuzed.rubyforge.org'
  # p.description = p.paragraphs_of('README.txt', 2..5).join("\n\n")
  # p.url = p.paragraphs_of('README.txt', 0).first.split(/\n/)[1..-1]
  p.changes = p.paragraphs_of('History.txt', 0..1).join("\n\n")
end

task :build do
  Dir['./**/*.erl'].each do |source|
    unless File.open(source).read =~ /#!/
      puts "compiling #{source}"
      Dir.chdir(File.dirname(source)) do |dir|
        puts `erlc #{File.basename(source)}`
      end
    end
  end
end

desc 'Generate manifest from git files'
task :manifest do
  sh 'git ls-tree --name-only -r HEAD > Manifest.txt'
end

desc "Upload site to Rubyforge"
task :site do
  sh "scp -r site/* mojombo@god.rubyforge.org:/var/www/gforge-projects/fuzed"
end
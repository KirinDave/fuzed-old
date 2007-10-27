require 'rubygems'
require 'hoe'

#Hoe.new('fuzed', "0.5.0") do |p|
#  p.rubyforge_name = 'fuzed'
#  p.author = ['Dave Fayram', 'Tom Preston-Werner']
#  p.email = ['tom@rubyisawesome.com']
#  p.summary = 'Leverage the YAWS webserver (and additional erlang-based infrastructure) to run Rails.'
#  p.url = 'fuzed.rubyforge.org'
#  # p.description = p.paragraphs_of('README.txt', 2..5).join("\n\n")
#  # p.url = p.paragraphs_of('README.txt', 0).first.split(/\n/)[1..-1]
#  p.changes = p.paragraphs_of('History.txt', 0..1).join("\n\n")
#  p.extra_deps << ['erlectricity', '>= 0.2.0']
#end

ERLC_TEST_FLAGS = "-pa ebin/eunit -I include/eunit -DTEST"
ERLC_FLAGS = "+debug_info -W2 -I include -I include/yaws -o ebin"
ERLR_FLAGS = "+Bc +K true -smp enable -pz /usr/local/lib/yaws/ebin -pz ./etest -pz ./ebin/ -pz ./ebin/eunit"

task :default do
  sh "erlc  #{ERLC_FLAGS} #{ERLC_TEST_FLAGS} elibs/*.erl"
end

task :build do
  sh "erlc  #{ERLC_FLAGS} elibs/*.erl"
end

task :econsole do
  sh "erl #{ERLR_FLAGS} -sname fuzed_console_#{$$}"
end

desc 'Generate manifest from git files'
task :manifest do
  sh 'git ls-tree --name-only -r HEAD > Manifest.txt'
end

task :test => [:default] do
  mods = []
  mod_directives = ""
  env_peek = ENV['MOD'] || ENV['MODS'] || ENV['MODULE'] || ENV['MODULES']
  if env_peek
    mods = env_peek.split(",")
  else 
    mods = Dir["etest/*_test.erl"].map { |x| x.match(/etest\/(.*)_test.erl/)[1] }
  end
  mod_directives = mods.map {|m| "-run #{m} test"}.join(" ")
  # -run #{ENV['MOD']} test
  sh %Q{erl +K true -smp enable -pz ./etest -pz ./ebin/yaws -pz ./ebin/ -pa ./ebin/eunit -sname local_console_#{$$} -noshell #{mod_directives} -run erlang halt}
end

desc "Upload site to Rubyforge"
task :site do
  sh "scp -r site/* mojombo@god.rubyforge.org:/var/www/gforge-projects/fuzed"
end
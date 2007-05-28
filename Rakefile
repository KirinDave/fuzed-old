task :default => [:build]

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


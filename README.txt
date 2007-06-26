fuzed
    by Dave Fayram, Tom Preston-Werner
    fuzed.rubyforge.org
    
    
== Summary
Leverage the YAWS webserver (and additional erlang-based infrastructure) to run Rails.


== Dependencies
* Erlang: http://www.erlang.org
* Yaws: http://yaws.hyber.org
* Ruby: http://www.ruby-lang.org
* Ruby Gems: 
  * rake: http://rake.rubyforge.org
  * erlectricty: http://code.google.com/p/erlectricity
  * rack: http://rack.rubyforge.org
  * chronic (for the demo): http://chronic.rubyforge.org


== Installation (from gem)

  gem install fuzed
  

== Installation (from git)

Get it from the git repo:

  git clone git://repo.or.cz/fuzed.git
  
Change to the fuzed working copy:

  cd fuzed
    
Build Fuzed:

  rake build


== Configuration

Generate a starter Yaws config file with:

  fuzedconf RAILS_ROOT
  
where RAILS_ROOT is the absolute path to the root directory of your Rails project.
This will generate a file called 'fuzed.conf' which contains a sample Yaws config file that should
be suitable for initial testing.


== Starting fuzed

Start the fuzed master server (yaws) locally:

  fuzed start -n server@127.0.0.1 -c fuzed.conf
  
In another terminal, start a fuzed client locally:

  fuzed join -n client@127.0.0.1 -m server@127.0.0.1 -a /Users/tom/dev/git/fuzed/helloworld/script/rack.rb
    
Point your browser at:

  http://localhost:8002/
  
If everything worked out, you'll see your Rails app!


== What is a Valid Hostname?
  Erlang has a funny notion about what a valid hostname is. Localhost won't
  cut it. I recommend using rendezvous to point to your local host. Short of
  that, 127.0.0.1 works.


== Contribution Notes
* Please note that empty directories should contain a .placeholder file
  (which should be empty), to facilitate the use of other version
  control systems which bridge to subversion but don't support empty
  directories.
fuzed
    by Dave Fayram, Tom Preston-Werner
    fuzed.rubyforge.org


== Summary
Leverage the YAWS webserver (and additional erlang-based infrastructure) to
run Rails.


== Dependencies
* Erlang: http://www.erlang.org
* Yaws: http://yaws.hyber.org
* Ruby: http://www.ruby-lang.org
* Ruby Gems: 
  * rake: http://rake.rubyforge.org
  * erlectricty: http://code.google.com/p/erlectricity
  * rack: http://rack.rubyforge.org


== Installation (from gem)

  sudo gem install fuzed


== Installation (from git)

Get it from the git repo:

  git clone git://repo.or.cz/fuzed.git

Change to the fuzed working copy:

  cd fuzed

Build Fuzed:

  rake build


== Configuration

Create a shared Erlang cookie on each machine. In order for Erlang processes in
different interpreters to communicate with each other, they each need to be
able to find a file called .erlang.cookie in the home directory of the user
under which they are running. The cookie should contain 20 uppercase alpha
characters on a single line (no newline).

Generate a starter Yaws config file with:

  fuzed-conf RAILS_ROOT 8080 > fuzed.conf

where RAILS_ROOT is the absolute path to the root directory of your Rails
project. You may optionally specify a port as the second argument. This will
generate a file called 'fuzed.conf' which contains a sample Yaws config file
that should be suitable for initial testing.


== Starting fuzed

Start the fuzed master server (yaws) locally:

  fuzed start -n server@127.0.0.1 -c fuzed.conf

In another terminal, start a fuzed client locally:

  fuzed join -n client@127.0.0.1 -m server@127.0.0.1 -r RAILS_ROOT

where RAILS_ROOT is the same as before.
    
Point your browser at:

  http://localhost:8080

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
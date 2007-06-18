This project's goal is to use the YAWS webserver (and additional erlang-based infrastructure) to create a rich environment for rails applications to run in.

== Dependencies
* Erlang: http://www.erlang.org
* Yaws: http://yaws.hyber.org
* Ruby: http://www.ruby-lang.org
* Ruby Gems: 
  * rake: http://rake.rubyforge.org
  * erlectricty: http://code.google.com/p/erlectricity
  * rack: http://rack.rubyforge.org
  * chronic (for the demo): http://chronic.rubyforge.org

== Getting Started
1. Download the source from Google Code:
    svn co http://fuzed.googlecode.com/svn/trunk fuzed
1a. Alternatively, get it from the git repo:
    git clone git://repo.or.cz/fuzed.git
2. Change to the fuzed working copy:
    cd fuzed
3. Build Fuzed:
    rake build
4. Start yaws using the built in script:
    ./start_yaws server@valid.hostname.
5. Hit enter in the YAWS console to drop into the REPL
6. In another terminal:
    ./start_node client@valid.hostname server@valid.hostname ./helloworld/script/rack.rb
7. Point your browser at http://localhost:8002/
8. Type various time related words into the box and watch the time appear below.
9. Rejoice!

== What is a Valid Hostname?
  Erlang has a funny notion about what a valid hostname is. Localhost won't
  cut it. I recommend using rendezvous to point to your local host. Short of
  that, 127.0.0.1 works.

== Contribution Notes
* Please note that empty directories should contain a .placeholder file
  (which should be empty), to facilitate the use of other version
  control systems which bridge to subversion but don't support empty
  directories.
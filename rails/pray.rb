#!/usr/bin/env ruby

require 'rubygems'
require 'erlectricity'
require 'rack'

messages_gotten = []

receive do
  match(:hello, string(:message)) do
    File.open("test.txt", "w+") { |fh| fh.puts message }
    messages_gotten << :hello.to_s
    receive_loop
  end
  
  match(:whee, string(:message)) do
    File.open("test2.txt", "w+") { |fh| fh.puts message }
    messages_gotten << :whee.to_s
    receive_loop
  end
  
  match(:end) do
  end
end

File.open("closed.txt", "w+") { |fh| fh.puts messages_gotten.to_s }
$:.unshift File.dirname(__FILE__)

module Fuzed
  VERSION = '0.1.0'
  
  def self.root
    File.expand_path(File.join(File.dirname(__FILE__), *%w[..]))
  end
  
  def self.relative(path)
    File.join(self.root, path)
  end
end
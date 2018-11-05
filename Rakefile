require 'pathname'
require 'rbconfig'
require 'shellwords'

EXE_EXT = RbConfig::CONFIG['EXEEXT']
SBCL = Shellwords.escape(ENV.fetch('SBCL', 'sbcl'))
root = Pathname.new(__FILE__).parent

desc "Build the lisp"
task :lisp => 'lisp:default'

namespace :lisp do
  EXECUTABLES = [ root.join("compiler#{EXE_EXT}"),
                  root.join("disassembler#{EXE_EXT}")
                ]
  
  task :default => EXECUTABLES

  task :clean do
    sh("rm -rf #{EXECUTABLES.collect { |s| Shellwords.escape(s) }.join(' ')}")
  end
end

file root.join("compiler#{EXE_EXT}") => Dir.glob(root.join('*.lisp')) do |t|
  pwd = Dir.pwd
  Dir.chdir(root)
  sh("#{SBCL} --script sbcl-image.lisp compiler")
  Dir.chdir(pwd)
end

file root.join("disassembler#{EXE_EXT}") => Dir.glob(root.join('*.lisp')) do |t|
  pwd = Dir.pwd
  Dir.chdir(root)
  sh("#{SBCL} --script sbcl-image.lisp disassembler")
  Dir.chdir(pwd)
end

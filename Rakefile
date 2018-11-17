require 'pathname'
require 'rbconfig'
require 'shellwords'

EXE_EXT = RbConfig::CONFIG['EXEEXT']
SBCL = Shellwords.escape(ENV.fetch('SBCL', 'sbcl'))

root = Pathname.new(__FILE__).parent.expand_path
buildroot ||= ENV.fetch('BUILDROOT', root.join('build'))
buildroot = buildroot.expand_path

desc "Build the lisp"
task :lisp => 'lisp:default'

directory buildroot

file buildroot.join("compiler#{EXE_EXT}") => [ *Dir.glob(root.join('*.lisp')), buildroot ] do |t|
  pwd = Dir.pwd
  Dir.chdir(root)
  sh("#{SBCL} --script sbcl-image.lisp compiler #{Shellwords.escape(t.name)}")
  Dir.chdir(pwd)
end

file buildroot.join("disassembler#{EXE_EXT}") => [ *Dir.glob(root.join('*.lisp')), buildroot ] do |t|
  pwd = Dir.pwd
  Dir.chdir(root)
  sh("#{SBCL} --script sbcl-image.lisp disassembler #{Shellwords.escape(t.name)}")
  Dir.chdir(pwd)
end

file buildroot.join('nolisp2.bin') => [ buildroot.join("compiler#{EXE_EXT}"), *Dir.glob(root.join('*.nl')), *Dir.glob(root.join('runtime/**/*.nl')), buildroot ] do |t|
  pwd = Dir.pwd
  Dir.chdir(root)
  cc = Shellwords.escape(buildroot.join("compiler#{EXE_EXT}"))
  sh("#{cc} -o #{Shellwords.escape(t.name)} repl-self.lisp")
  Dir.chdir(pwd)
end

file buildroot.join('cat.bin') => [ buildroot.join("compiler#{EXE_EXT}"), *Dir.glob(root.join('examples/cat.nl')), *Dir.glob(root.join('runtime/**/*.nl')), buildroot ] do |t|
  pwd = Dir.pwd
  Dir.chdir(root)
  cc = Shellwords.escape(buildroot.join("compiler#{EXE_EXT}"))
  sh("#{cc} -o #{Shellwords.escape(t.name)} examples/cat.nl")
  Dir.chdir(pwd)
end

namespace :lisp do
  EXECUTABLES = [ buildroot.join("compiler#{EXE_EXT}"),
                  buildroot.join("disassembler#{EXE_EXT}"),
                  buildroot.join("nolisp2.bin"),
                  buildroot.join("cat.bin")
                ]
  
  task :default => EXECUTABLES

  task :clean do
    sh("rm -rf #{Shellwords.escape(buildroot)}")
  end
end

task :default => 'lisp:default'
task :clean => 'lisp:clean'

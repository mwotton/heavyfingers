
# Add files and commands to this file, like the example:
#   watch(%r{file/path}) { `command(s)` }
#
guard 'shell' do
  watch(/(.*).elm$/) {|m| `elm --make #{m[0]}` }
end

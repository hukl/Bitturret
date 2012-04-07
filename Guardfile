# # -*- encoding : utf-8 -*-

# automatically run the eunit tests

def run_eunit_all
  cmd = "./rebar eunit skip_deps=true"
  puts "Executing #{cmd}"
  puts `#{cmd}`
  if $? == 0
    Growl.notify_ok "eunit: all tests passed."
  else
    Growl.notify_error "eunit: tests failed."
  end
end

def run_eunit(app, src, suite)
  if File.exist?(File.join(File.dirname(__FILE__), app, 'test', "#{suite}_tests.erl"))
    cmd = "./rebar eunit skip_deps=true suite=#{suite}"
    puts "Executing #{cmd}"
    puts `#{cmd}`
    if $? == 0
      Growl.notify_ok "#{suite}: eunit passed."
    else
      Growl.notify_error "#{suite}: eunit failed."
    end
  else
    puts "No tests for #{suite.inspect}"
    Growl.notify_warning "No tests for #{suite}!"
  end
end

def run_spec
end

guard 'shell' do
  watch(%r{(apps/.*?)(src|test)/([^.].*?)(_tests)?.erl}) {|m| run_eunit(m[1], m[2], m[3]) }
  watch(%r{apps/(.*?)/include/([^.].*).hrl}) {|m| run_eunit_all }
end

./rebar compile
./rebar eunit compile_only=true
erl -pa ebin -pa deps/goldrush/ebin/ -pa .eunit -config app.config -sname lager_test -s lager_massive_test start_apps -setcookie abc

all:
	rm -rf  *~ */*~ src/*.beam test/*.beam erl_cra*;
	rm -rf  logs *.pod_dir rebar.lock;
	rm -rf config sd;
	rm -rf deployments *_info_specs;
	rm -rf _build test_ebin ebin *_info_specs;
	rm -rf Mnesia.*;
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build test_ebin logs log;
	git add -f *;
	git commit -m $(m);
	git push;
	echo Ok there you go!
check:
	rebar3 check

eunit:
	rm -rf  *~ */*~ src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf _build logs log *.pod_dir;
	rm -rf deployments *_info_specs;
	rm -rf config sd;
	rm -rf rebar.lock;
	rm -rf Mnesia.*;
	rm -rf ebin;
	mkdir  application_info_specs;
	cp ../../specifications/application_info_specs/*.spec application_info_specs;
	mkdir  host_info_specs;
	cp ../../specifications/host_info_specs/*.host host_info_specs;
	mkdir deployment_info_specs;
	cp ../../specifications/deployment_info_specs/*.depl deployment_info_specs;
	mkdir deployments;
	cp ../../specifications/deployments/*.depl_spec deployments;
	mkdir test_ebin;
	mkdir ebin;
	rebar3 compile;
	cp _build/default/lib/*/ebin/* ebin;
	git clone https://github.com/joq62/config.git;
	erlc -o test_ebin ../host/src/install.erl;
	erlc -o test_ebin ../host/src/lib_host.erl;
	cp ../common/ebin/* test_ebin;
	erlc -I include -o test_ebin test/*.erl;
	erl -pa * -pa config/ebin -pa ebin -pa test_ebin -sname etcd_test -run $(m) start -setcookie cookie_test

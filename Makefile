all:
	rm -rf  *~ */*~ src/*.beam test/*.beam erl_cra*;
	rm -rf  logs *.pod_dir rebar.lock;
	rm -rf config sd;
	rm -rf deployments *_info_specs;
	rm -rf _build test_ebin ebin *_info_specs;
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build test_ebin logs log;
	echo Done
check:
	rebar3 check

eunit:
	rm -rf  *~ */*~ src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf _build logs log *.pod_dir;
	rm -rf deployments *_info_specs;
	rm -rf config sd;
	rm -rf rebar.lock;
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
	git clone https://github.com/joq62/sd.git;
	erlc -o test_ebin test/*.erl;
	erl -pa host_info_specs -pa */ebin -pa ebin -pa test_ebin -sname etcd_test -run basic_eunit start -setcookie cookie_test

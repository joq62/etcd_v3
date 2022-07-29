-define(TABLE,application_spec).
-define(RECORD,application_spec).
-record(application_spec,{
			  name,
			  vsn,
			  gitpath,
			  cmd
			 }).

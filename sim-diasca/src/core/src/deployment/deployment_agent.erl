% Copyright (C) 2008-2016 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)



% Agent to be sent, thanks to deployment workers, on all computing nodes, so
% that it can deploy automatically everything which is needed in order to run a
% simulation.
%
% Not using WOOPER here, in order to avoid needing extra dependencies and
% environment during this bootstrap phase.
%
-module(deployment_agent).


-export([ deploy/3 ]).



% Implementation notes:
%
% Includes are not a problem (they are seen at build time as any other module);
% however making use of other modules is a problem, as this pioneer module
% should be as self-contained as reasonably possible.
%
% Thus some functions defined in other modules were duplicated verbatim
% from other base modules (ex: system_utils), to avoid having to rely on too
% many prerequisite modules (the pioneer list must be lean and mean).
%
% Finally, we included more pioneer modules, as verbatim duplication is
% error-prone.
%
% Pioneer modules are listed in
% class_ComputingHostManager:send_deployment_agent/1.


% For host_static_info record:
-include("system_utils.hrl").


% For trace_aggregator_name:
-include("class_TraceAggregator.hrl").


% For TracingActivated:
-include("class_TraceEmitter.hrl").


% For the file_info record:
-include_lib("kernel/include/file.hrl").



% Performs the actual deployment; triggered by a rpc:cast/4 called by the
% associated computing host manager.
%
% io:format print-outs will end up in the user console.
%
% ComputerHostManagerPid is sent, as it is the main user-side interlocutor for
% remote deployment agents.
%
-spec deploy( pid(), unit_utils:seconds(), text_utils:bin_string() ) ->
					'onDatabaseStarted' | 'onDatabaseStopped'.
deploy( ComputerHostManagerPid, InterNodeTickTimeOut, BinDeployBaseDir ) ->

	% All nodes must behave the same:
	change_initiated = net_kernel:set_net_ticktime( InterNodeTickTimeOut ),

	TraceAggregatorPid = basic_utils:wait_for_global_registration_of(
						   ?trace_aggregator_name ),

	send_trace_fmt( TraceAggregatorPid,
		"Deployment agent running on node ~p, "
		"with version ~s of the virtual machine, "
		"requesting the simulation package from ~w. "
		"Current scheduler count: ~B.",
		[ node(), system_utils:get_interpreter_version(),
		  ComputerHostManagerPid, erlang:system_info( schedulers ) ], info ),

	ComputerHostManagerPid ! { requestPackage, node(), self() },

	% Prepare some operations in the meantime:
	{ DeployBaseDir, DeployBeamDir } = prepare_package( BinDeployBaseDir,
														TraceAggregatorPid ),

	receive

		{ wooper_result, deploy_time_out } ->
			terminate();

		{ wooper_result, send_starting } ->

			send_trace( TraceAggregatorPid,
					"Receiving of the simulation package started.", debug ),

			% The current directory is already correct:
			PackageFilename = net_utils:receive_file( ComputerHostManagerPid ),

			send_trace( TraceAggregatorPid,
						"Simulation package fully received.", debug ),

			PackageBin = file_utils:read_whole( PackageFilename ),

			manage_package( PackageBin, DeployBaseDir, DeployBeamDir,
							TraceAggregatorPid ),

			{ _UsedSwap, TotalSwap } = system_utils:get_swap_status(),

			HostInfo = #host_static_info{

			  total_ram = system_utils:get_total_physical_memory(),
			  total_swap = TotalSwap,
			  core_count = system_utils:get_core_count(),
			  erlang_version = system_utils:get_interpreter_version()

			 },

			ComputerHostManagerPid ! { onDeploymentReady, HostInfo },

			% Kept running, as could be useful later:
			final_main_loop( TraceAggregatorPid, ComputerHostManagerPid,
							 BinDeployBaseDir )

	end.



% Prepares to receive the deployment package.
%
-spec prepare_package( text_utils:bin_string(), pid() ) ->
		{ file_utils:directory_name(), file_utils:directory_name() }.
prepare_package( BinDeployBaseDir, TraceAggregatorPid ) ->

	DeployBaseDir = binary_to_list( BinDeployBaseDir ),

	case file_utils:exists( DeployBaseDir ) of

		true ->

			send_trace_fmt( TraceAggregatorPid,
				"Deployment directory '~s' already existing "
				"as a filesystem element, removing it fully first.",
				[ DeployBaseDir ], info ),

			Command = "/bin/rm -rf '" ++ DeployBaseDir ++ "' 1>/dev/null",

			case system_utils:run_executable( Command ) of

				{ _ErrorCode=0, _CmdOutput=[] } ->
					ok;

				{ _ErrorCode=0, CmdOutput } ->
					send_trace_fmt( TraceAggregatorPid,
									"Removal of deployment directory '~s' "
									"succeeded, yet output following "
									"message: '~s'.",
									[ DeployBaseDir, CmdOutput ], warning );

				{ ErrorCode, CmdOutput } ->
					send_trace_fmt( TraceAggregatorPid,
									"Error, removal of deployment "
									"directory '~s' failed (error code: ~B, "
									"output: '~s').",
									[ DeployBaseDir, ErrorCode, CmdOutput ],
									error )

			end;


		false ->

			send_trace_fmt( TraceAggregatorPid,
							"Deployment directory '~s' not already existing, "
							"will create it.", [ DeployBaseDir ], debug )

	end,

	DeployBeamDir = filename:join( DeployBaseDir, "deployed-elements" ),

	file_utils:create_directory( DeployBeamDir, create_parents ),

	file_utils:set_current_directory( DeployBeamDir ),

	{ DeployBaseDir, DeployBeamDir }.



% Manages the received deployment package.
%
-spec manage_package( binary(), file_utils:directory_name(),
		 file_utils:directory_name(), pid() ) -> basic_utils:void().
manage_package( PackageBin, DeployBaseDir, DeployBeamDir,
				TraceAggregatorPid ) ->

	send_trace_fmt( TraceAggregatorPid,
					"Received simulation package, whose size is ~B bytes, "
					"will extract it in deployment directory '~s'.~n",
					[ size( PackageBin ), DeployBeamDir ], debug ),

	FileNames = file_utils:zipped_term_to_unzipped_files( PackageBin ),

	send_trace_fmt( TraceAggregatorPid,
					"Following ~B files were extracted in '~s':~n~p.~n",
					[ length( FileNames ), DeployBeamDir,
					  lists:sort( FileNames ) ], debug ),

	% Now updating the code path according to the layout of the deployed tree:

	% Some directories not containing BEAMs could be removed:
	BeamDirs = file_utils:find_directories_from( "." ),

	% Dealing with absolute directories is easier to debug:
	AbsoluteBeamDirs = [ filename:join( DeployBeamDir, D ) || D <-BeamDirs ],

	%io:format( "Added BEAM dirs: ~p.~n", [ AbsoluteBeamDirs ] ),

	ok = code:add_paths( AbsoluteBeamDirs ),

	send_trace_fmt( TraceAggregatorPid,
					"Following BEAM directories were "
					"added to code path:~n~p.~n", [ BeamDirs ], trace ),

	%io:format( "Updated code path:~n~p.~n", [ code:get_path() ] ),

	OutputDir = filename:join( DeployBaseDir, "outputs" ),

	file_utils:create_directory( OutputDir, create_parents ),

	file_utils:set_current_directory( OutputDir ).



% Final loop of this deploy agent.
%
final_main_loop( TraceAggregatorPid, ComputerHostManagerPid,
				 BinDeployBaseDir ) ->

	receive

		{ start_database, CallerPid } ->

			% The mnesia directory must already have been set here (no
			% application:set_env( mnesia, dir, .. ) taken into account here).

			send_trace_fmt( TraceAggregatorPid,
							"Deployment agent starting database on node ~s.",
							[ node() ], info ),

			%io:format( "Deployment agent starting database on node ~s.~n",
			%		  [ node() ] ),

			% No prior loading accepted:

			%%case application:load(mnesia) of

			%%	ok ->
			%%		ok;

			%%	LoadError ->
			%%		throw( { mnesia_load_failed, node(), LoadError } )

			%%end,

			case application:start( mnesia ) of

				ok ->
					ok;

				StartError ->
					throw( { mnesia_start_failed, node(), StartError } )

			end,

			%io:format( "Database started on node ~s.~n", [ node() ] ),

			CallerPid ! onDatabaseStarted,

			final_main_loop( TraceAggregatorPid, ComputerHostManagerPid,
							 BinDeployBaseDir );


		{ stop_database, CallerPid } ->

			%io:format( "~w stopping database.~n", [ self() ] ),

			ok = application:stop( mnesia ),
			ok = application:unload( mnesia ),

			%io:format( "~w stopped database.~n", [ self() ] ),

			CallerPid ! onDatabaseStopped,

			% We must recurse, as we still want to properly terminate, otherwise
			% there would be lingering computing nodes:
			%
			final_main_loop( TraceAggregatorPid, ComputerHostManagerPid,
							 BinDeployBaseDir );


		terminate ->

			send_trace_fmt( TraceAggregatorPid,
				"Removing deployment directory '~s' and terminating now.",
				[ BinDeployBaseDir ], info ),

			RemoveCommand = "/bin/rm -rf '"
				++ binary_to_list( BinDeployBaseDir ) ++ "'",

			case system_utils:run_executable( RemoveCommand ) of

				{ _ReturnCode=0, _CmdOutput=[] } ->
					ok;

				{ ReturnCode, CmdOutput } ->
					send_trace_fmt( TraceAggregatorPid,
									"Problem while removing deployment "
									"directory" "'~s' (error code: ~B"
									", message '~s').",
									[ BinDeployBaseDir, ReturnCode, CmdOutput ],
									error )

			end,

			terminate()


		% No more 'after' clause to perform an automatic shutdown after a
		% time-out?

	end.



-spec terminate() -> no_return().
terminate() ->

	% We do not want nodes to wait any longer, otherwise old code of modules
	% could linger:
	%
	%io:format( "~n(deployment agent ~p terminating immediately)~n",
	%		   [ self() ] ),
	%timer:sleep( 1000 ),

	% Remote shutdown directly done by computing host manager:
	init:stop( _Success=0 ).




% Section to help sending traces from the deployment agent, which is not a trace
% emitter.


-ifdef(TracingActivated).


% Here all trace types are sent:

-spec send_trace( pid(), string(), traces:message_type() ) ->
						basic_utils:void().
send_trace( TraceAggregatorPid, Message, MessageType ) ->
	send_trace_helper( TraceAggregatorPid, Message, MessageType ).


-spec send_trace_fmt( pid(), text_utils:format_string(), [ any() ],
					  traces:message_type() ) -> basic_utils:void().
send_trace_fmt( TraceAggregatorPid, MessageFormat, FormatValues,
				MessageType ) ->
	Message = io_lib:format( MessageFormat, FormatValues ),
	send_trace_helper( TraceAggregatorPid, Message, MessageType ).


-else. % not TracingActivated:


% Avoids warnings:


-spec send_trace( pid(), string(), traces:message_type() ) ->
						basic_utils:void().
send_trace( TraceAggregatorPid, Message, MessageType ) ->
	deploy_trace( TraceAggregatorPid, Message, MessageType ).


-spec send_trace_fmt( pid(), string(), text_utils:format_string(),
					  traces:message_type() ) -> basic_utils:void().
send_trace_fmt( TraceAggregatorPid, MessageFormat, FormatValues,
				MessageType ) ->
	Message = io_lib:format( MessageFormat, FormatValues ),
	deploy_trace( TraceAggregatorPid, Message, MessageType ).


% Even when tracing is not activated, the most severe priorities are not
% filtered out:
%
deploy_trace( TraceAggregatorPid, Message, MessageType=fatal ) ->
	send_trace_helper( TraceAggregatorPid, Message, MessageType );

deploy_trace( TraceAggregatorPid, Message, MessageType=error ) ->
	send_trace_helper( TraceAggregatorPid, Message, MessageType );

deploy_trace( TraceAggregatorPid, Message, MessageType=warning ) ->
	send_trace_helper( TraceAggregatorPid, Message, MessageType );

deploy_trace( _TraceAggregatorPid, _Message, _MessageType ) ->
	trace_disabled.


-endif. % TracingActivated



% Helper to actually send a trace.
%
send_trace_helper( TraceAggregatorPid, Message, MessageType ) ->

	% We keep only the hostname, not the FQDN, otherwise the (last) dot in the
	% name would be interpreted as subcategory in the traces:
	TraceAggregatorPid ! { send,
		[ self(), "Deployment agent on "
			++ hd( string:tokens( net_adm:localhost(), "." ) ),
		  "Core.Deployment", _Tick=undefined, current_time_to_string(),
		  node(), "Standalone.Deployment", get_priority_for( MessageType ),
		  Message ] }.


% Corresponds to time_utims:get_textual_timestamp/0:
%
% Returns the current time and date as a string, with correct format.
%
% Example: "14/04/2008 04:41:24".
%
current_time_to_string() ->
	{ { Year, Month, Day }, { Hour, Minute, Second } } = erlang:localtime(),
	lists:flatten( io_lib:format( "~B/~B/~B ~B:~B:~B",
		[ Day, Month, Year, Hour, Minute, Second ] ) ).




% Duplication section: the deployment_agent is the only one which is to be run
% standalone (pioneer module with almost no prerequisite).

% Duplicated verbatim from class_TraceEmitter.erl:


% Returns the priority of specified trace type (i.e. fatal, error, etc.).
%
% Note: now that LogMX v1.3.2 and later only support 5 levels of detail
% (stack/error, warning/warn, info, fine, finest/debug, i.e. no more trace),
% fatal and error messages have been put at the same priority level, and
% Ceylan trace level has been kept, whereas others have been offset.
%
% See also: get_channel_name_for_priority/1.
%
% (static)
%
%-spec get_priority_for( traces:message_type() ) -> traces:priority().
% Corresponds to stack/error:

% Commented to silence Dialyzer (not used locally):
%get_priority_for( fatal ) ->
%	1 ;

% Corresponds to stack/error:
get_priority_for( error ) ->
	2 ;

% Corresponds to warning/warn:
get_priority_for( warning ) ->
	3 ;

% Corresponds to info:
get_priority_for( info ) ->
	4 ;

% Corresponds to fine:
get_priority_for( trace ) ->
	5 ;

% Corresponds to finest/debug:
get_priority_for( debug ) ->
	6.

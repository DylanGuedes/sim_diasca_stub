% Copyright (C) 2012-2016 EDF R&D

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


% This is the root Sim-Diasca module, to be called from most simulation cases,
% hiding the details of the simulation services being used underneath, and
% offering opportunities for single-place updates with no impact on existing
% cases.
%
-module(sim_diasca).


-export([ init/3, create_initial_instances/1, notify_hint/1,
		  run_simulation/2, run_simulation_and_browse_results/2,
		  shutdown/0 ]).


% For notify_warning_fmt and al:
-include("traces.hrl").


% For ?trace_aggregator_name:
-include("class_TraceAggregator.hrl").


% For simulation_settings:
-include("class_TimeManager.hrl").


% For deployment_settings:
-include("class_DeploymentManager.hrl").


% For load_balancing_settings:
-include("class_LoadBalancer.hrl").


% For user_process_name:
-include("sim_diasca.hrl").


% Simulation UUID (ex: for node cookies):
-type simulation_uuid() :: basic_utils:uuid().


% Simulation instance identifier (possibly user-defined):
-type sii() :: string().


-type simulation_identifiers() :: { simulation_uuid(), sii() }.


-export_type([ simulation_uuid/0, sii/0, simulation_identifiers/0 ]).



% Initialises the engine according to specified settings.
%
% Returns the PID of the deployment manager.
%
% (function)
%
-spec init( simulation_settings(), deployment_settings(),
		   load_balancing_settings() ) -> pid().
init( SimulationSettings, DeploymentSettings, LoadBalancingSettings ) ->

	% Takes care of the simulation UUID and SII:
	{ SimUUID, SII } = SimIdentifiers = get_simulation_identifiers(),

	io:format( "~nSimulation instance identifier is '~s'.~n", [ SII ] ),

	% We need to rename this user node so that it bears a conventional name
	% (deriving from the simulation case, and including the SII) - yet this can
	% only be done by starting from a non-distributed node (done by the
	% Sim-Diasca make rules, relying on the --nn option):

	SimulationName = SimulationSettings#simulation_settings.simulation_name,


	% We wanted the trace system to be autonomous (ex: so that it can be used
	% before the initialisation of the engine and after its shutdown); as a
	% result, from ?case_start a default trace filename was created and used
	% (ex: 'my_foobar_case.traces').
	%
	% Now that the engine is being initialised, we request the trace file to be
	% renamd, so that it includes the user and the SII (ex:
	% 'my_foobar_case-by-boudevil-94.traces'):
	%
	NewTraceFilename = file_utils:convert_to_filename( SimulationName ++ "-by-"
							  ++ system_utils:get_user_name() ++ "-" ++ SII
							  ++ ?TraceExtension ),

	TraceAggregatorPid = basic_utils:get_registered_pid_for(
						   ?trace_aggregator_name, global ),

	% We have to rename the trace file (ex: to include the SII):
	TraceAggregatorPid ! { renameTraceFile, [ NewTraceFilename ] },


	% Now is the first time at which we can run the trace supervisor, since the
	% trace filename is not expected to change anymore:
	% (slight race condition, if ever the renameTraceFile message arrived

	class_TraceSupervisor:init( NewTraceFilename, ?TraceType,
								TraceAggregatorPid ),

	% EPMD must be running prior to having a node go distributed:
	%
	% We select here at which port this launched EPMD will run, yet the current
	% VM apparently got the EPMD port that it will contact at start-up, and it
	% does not seem possible to change it.
	%
	% As a result, the EPMD ports in the launch command (see the EPMD_PORT
	% variable in common/GNUmakevars.inc) and in the deployment settings (see
	% its firewall_restrictions field) must match.
	%
	case class_DeploymentManager:interpret_firewall_options(
		   DeploymentSettings ) of

		{ _EPMDPort=undefined, _TcpRangeOption } ->
			% We use here the default Sim-Diasca EPMD port:
			net_utils:launch_epmd();

		{ EPMDPort, _TcpRangeOption } ->
			system_utils:set_environment_variable( "ERL_EPMD_PORT",
								   text_utils:format( "~B", [ EPMDPort ] ) ),
			net_utils:launch_epmd( EPMDPort )

	end,

	NodePrefix = class_DeploymentManager:get_node_name_prefix_from(
				   SimulationName, SII ),

	% We rename this user node accordingly:
	UserNodeName = NodePrefix ++ "-user-node",

	net_utils:enable_distribution( UserNodeName, long_name ),

	% Simulations will never step over others (previous ones):
	Cookie = text_utils:string_to_atom( SimUUID ),

	% All spawned nodes will be given later the current cookie of this node:
	erlang:set_cookie( node(), Cookie ),

	% Detailed checking of this field done later, by the deployment manager:
	%
	case DeploymentSettings#deployment_settings.crash_resilience of

		K when is_integer( K ) andalso K > 0 ->
			% Here, an actual resilience is wanted. As a result, this current
			% process (i.e. the one of the simulation case) shall resist to any
			% node loss, thus must trap exits (ex: for initial - linked - actors
			% that were running on a crashed node).  However, process crashes
			% should not remain silent, thus EXIT messages will be searched for
			% later.
			process_flag( trap_exit, _ResistExitMsg=true );

		_ ->
			ok

	end,

	% We register this process (the one of the simulation case), so that I can
	% be found by others, like the resilience manager:
	%
	basic_utils:register_as( ?user_process_name, global_only ),

	% Simply returns this PID, for later use:
	%
	% (we kept the link with the user process corresponding to the simulation
	% case, as if no resilience had been requested we want to stop whenever a
	% node crashed, and with resilience enabled we trap exits, and are thus able
	% to detect crashes nevertheless)
	%
	class_DeploymentManager:synchronous_new_link( SimulationSettings,
			DeploymentSettings, LoadBalancingSettings, SimIdentifiers,
			deploy_from_scratch ).



% Creates (synchronously) the initial instances, from specified file.
%
% (function)
%
-spec create_initial_instances( file_utils:file_name() ) -> basic_utils:void().
create_initial_instances( _Filename ) ->

	% Currently one shall use the initialisation_files field of the
	% simulation_settings instead:
	%
	throw( not_implemented_yet ).



% Notifies the user of a engine-level hint.
%
% This is typically used to suggest that some kind of model-level defect is the
% culprit for a detected error.
%
-spec notify_hint( text_utils:ustring() ) -> basic_utils:void().
notify_hint( Message ) ->
	io:format( "[hint] ~ts.~n", [ Message ] ).



% Runs the actual simulation, until reaching the stop tick.
%
% (function)
%
-spec run_simulation( class_TimeManager:tick(), pid() ) -> basic_utils:void().
run_simulation( StopTick, DeploymentManagerPid ) ->

	% As some processes (ex: the time manager) have the PID of this simulation
	% case process in their state, it must be declared too to the corresponding
	% instance tracker (now that it has been deployed):
	%
	class_InstanceTracker:register_agent( ?user_process_name ),

	DeploymentManagerPid ! { getRootTimeManager, [], self() },

	RootTimeManagerPid = traces:receive_applicative_message(),

	%?notify_info_fmt( "Starting simulation, "
	%				  "for a stop at tick offset ~B.", [ StopTick ] ),

	RootTimeManagerPid ! { start, [ StopTick, self() ] },

	%?notify_info( "Waiting for the simulation to end, "
	%			  "since having been declared as a simulation listener." ),

	receive

		simulation_stopped ->
			?notify_info( "Simulation stopped spontaneously, "
						  "specified stop tick must have been reached." )

	end.



% Runs the actual simulation, until reaching the stop tick, and allows the user
% to browse the corresponding results, if it succeeded.
%
% (function)
%
-spec run_simulation_and_browse_results( class_TimeManager:tick(), pid() ) ->
											   basic_utils:void().
run_simulation_and_browse_results( StopTick, DeploymentManagerPid ) ->

	run_simulation( StopTick, DeploymentManagerPid ),

	%?notify_info( "Browsing the report results, if in batch mode." ),

	class_ResultManager:browse_reports().



% Shutdowns the engine.
%
% (function)
%
-spec shutdown() -> basic_utils:void().
shutdown() ->

	% Stateless, hence resilience-friendly.

	case basic_utils:is_registered( ?deployment_manager_name, global ) of

		not_registered ->
			ok;

		DeployPid ->
			class_DeploymentManager:shutdown( DeployPid )

	end,

	basic_utils:unregister( ?user_process_name, global_only ),

	check_exit_messages().



% Lists any EXIT messages that would linger in mailbox.
%
-spec check_exit_messages() -> basic_utils:void().
check_exit_messages() ->

	receive

		{ 'EXIT', _From, _Reason=normal } ->
			% Ignored:
			check_exit_messages();

		{ 'EXIT', From, Reason } ->
			?notify_warning_fmt( "process whose PID was ~w had exited "
								 "with reason '~p'.~n", [ From, Reason ] ),
			check_exit_messages()

	after 0 ->

			% Stop recursing:
			ok

	end.




% Returns the simulation identifiers: determines both the simulation UUID and
% the SII (potentially derived from it, if not specified by the user).
%
-spec get_simulation_identifiers() -> simulation_identifiers().
get_simulation_identifiers() ->

	% In all cases an UUID will be needed:
	UUID = basic_utils:generate_uuid(),

	SII = case init:get_argument( '-simulation-instance-id' ) of

		{ ok, [ UserSpecifiedSII ] } when is_list( UserSpecifiedSII ) andalso
										  length( UserSpecifiedSII ) > 0 ->
			%	  ?notify_info_fmt( "Using simulation UUID '~s', and "
			%						"user-specified SSI '~s'.",
			%						[ UUID, UserSpecifiedSII ] ),
				  UserSpecifiedSII;

		{ ok, [ UserSpecifiedSII ] } ->
				  throw( { invalid_user_specified_simulation_instance_id,
						   UserSpecifiedSII } );

		{ ok, Unexpected } ->
				  throw( { multiple_user_specified_simulation_instance_id,
						   Unexpected } );

		error ->
				  % No SII defined by the user, hence deducing it from UUID.

				  % Will lead to a shorter, more human-tractable string (up to 9
				  % characters, instead of 36 for the UUID):
				  %
				  DeducedSSI = text_utils:integer_to_string(
								 erlang:phash2( UUID ) ),

				%  ?notify_info_fmt( "Using simulation UUID '~s', and, "
				%					"no SII having been specified by the user, "
				%					"a UUID-derived one, SSI '~s'.",
				%					[ UUID, DeducedSSI ] ),

				  DeducedSSI

	end,

	{ UUID, SII }.

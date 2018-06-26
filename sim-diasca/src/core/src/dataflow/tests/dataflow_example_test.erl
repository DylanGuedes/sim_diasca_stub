% Copyright (C) 2016-2016 EDF R&D

% This file is part of Sim-Diasca, on behalf of the EDF City Simulation
% project. It does not pertain to the free software release of Sim-Diasca.

% Creation date: Thursday, June 11, 2015
% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


% The purpose of this module is to test the dartaflow support.
%
% See also class_DataflowActor.erl and the 'Sim-Diasca Dataflow HOWTO'.
%
-module(dataflow_example_test).


% For facilities common to all cases:
-include("case_constructs.hrl").



% Runs the test, determining the settings from the command-line, otherwise using
% defaults.
%
-spec run() -> no_return().
run() ->

	?case_start,

	% Daily (in virtual seconds):
	TickDuration = 24 * 3600,

	%SimulationDurationInYears = 30,
	%SimulationDurationInYears = 1000,
	%SimulationDurationInYears = 2,

	% The official duration:
	SimulationDurationInYears = 5,


	ExpectedTimestepCount = round( time_utils:years_to_seconds(
							  SimulationDurationInYears ) / TickDuration ),

	test_facilities:display( "Evaluating specified Example Dataflow with a "
							 "timestep of ~s and a duration of ~B years (hence "
							 "corresponding to ~p expected timesteps).",
			   [ text_utils:duration_to_string( 1000 * TickDuration ),
				 SimulationDurationInYears, ExpectedTimestepCount ] ),


	% Use default simulation settings (batch reproducible):
	SimulationSettings = #simulation_settings{

	  simulation_name = "Sim-Diasca Dataflow Example",

	  tick_duration = float( TickDuration ),

	  %initialisation_files = [ InitFilename ],

	  result_specification = all_outputs

	},


	DeploymentSettings = #deployment_settings{

		computing_hosts = { use_host_file_otherwise_local,
							"sim-diasca-host-candidates.txt" }

	},


	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},


	% A deployment manager is created directly on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings,
								 DeploymentSettings, LoadBalancingSettings ),

	% Now that the engine is initialised, let's create the initial instances.

	% Creating first the five blocks, whose construction parameters are:
	% BlockName, ActivationPolicy, InputPorts, OutputPorts.

	% Starting from top-left to bottom-right, hence with the Source block:

	FuelOutputPort = { "Fuel", float },
	SparkOutputPort = { "Spark", boolean },

	% Source block:
	SourceBlockInputPorts = [],

	SourceBlockOutputPorts = [ FuelOutputPort, SparkOutputPort ],

	_SourceBlockPid = class_Actor:create_initial_actor( class_DataflowActor,
		[ "A Source Block", activate_when_all_ready, SourceBlockInputPorts,
		  SourceBlockOutputPorts ] ),

	% B1 block:

	I1InputPort = { "I1", integer },
	I2InputPort = { "I2", float },

	B1BlockInputPorts = [ I1InputPort, I2InputPort ],


	O1OutputPort = { "O1", boolean },
	O2OutputPort = { "O2", boolean },
	O3OutputPort = { "O3", boolean },

	% Note the ordering; will be listed alphabetically:
	B1BlockOutputPorts = [ O1OutputPort, O3OutputPort, O2OutputPort ],


	_B1BlockPid = class_Actor:create_initial_actor( class_DataflowActor,
		[ "Block B1", activate_on_new_ready, B1BlockInputPorts,
		  B1BlockOutputPorts ] ),


	% Sinky block:

	FinalInputPort = { "Final Results", float },
	AddInputPort = { "Additional Information", float },
	OtherInputPort = { "Other Results", float },

	SinkyInputPorts = [ FinalInputPort, AddInputPort, OtherInputPort ],

	% Sink:
	SinkyOutputPorts = [],

	_SinkyBlockPid = class_Actor:create_initial_actor( class_DataflowActor,
		[ "Sinky", activate_on_new_ready, SinkyInputPorts, SinkyOutputPorts ] ),


	% Pizzaiolo block:

	PepInputPort = { "Pepperoni", integer },
	TomatoInputPort = { "Tomato", integer },
	CheeseInputPort = { "Cheese", integer },

	PizzaioloInputPorts = [ PepInputPort, TomatoInputPort, CheeseInputPort ],

	PizzaOutputPort = { "Pizza", integer },
	PastaOutputPort = { "Pasta", integer },

	PizzaioloOutputPorts = [ PizzaOutputPort, PastaOutputPort ],

	_PizzaioloBlockPid = class_Actor:create_initial_actor( class_DataflowActor,
		[ "Strange Disconnected Pizzaiolo", activate_on_new_ready,
		  PizzaioloInputPorts, PizzaioloOutputPorts ] ),


	% My block:

	MyInputPorts = [ { "my_only_input", boolean } ],

	MyOutputPorts = [ { "an_output", atom },
					  { "another_output", integer },
					  { "a_third_output", list },
					  { "a_fourth_output", tuple } ],

	_MyBlockPid = class_Actor:create_initial_actor( class_DataflowActor,
		[ "My Block", activate_when_all_ready, MyInputPorts, MyOutputPorts ] ),


	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	% Let's start on 2020, January 1st at 00:00:00...
	StartYear = 2020,

	StartDate = { StartYear, 1, 1 },
	StartTime = { 0, 0, 0 },

	RootTimeManagerPid ! { setInitialSimulationDate, [ StartDate, StartTime ] },

	% ...and end specified years later:
	EndDate = { StartYear + SimulationDurationInYears, 1, 1 },
	EndTime = StartTime,

	RootTimeManagerPid ! { setFinalSimulationDate, [ EndDate, EndTime ] },

	StartTimestamp = { StartDate, StartTime },
	EndTimestamp = { EndDate, EndTime },

	?test_info_fmt( "Starting simulation at ~s, "
					"for a planned stop at ending timestamp ~s.",
					[ time_utils:get_textual_timestamp( StartTimestamp ),
					  time_utils:get_textual_timestamp( EndTimestamp ) ] ),

	RootTimeManagerPid ! { start, self() },

	?test_info( "Waiting for the simulation to end, "
				"since having been declared as a simulation listener." ),

	receive

		simulation_stopped ->
			?test_info( "Simulation stopped spontaneously, "
						"specified stop tick must have been reached." )

	end,

	?test_info( "Browsing the report results, if in batch mode." ),
	class_ResultManager:browse_reports(),

	sim_diasca:shutdown(),

	?case_stop.

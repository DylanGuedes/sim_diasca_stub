% Copyright (C) 2016-2016 EDF R&D

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



% Dataflow class, in charge of representing a full dataflow.
%
% Please refer to the 'Sim-Diasca Dataflow HOWTO' for further information.
%
-module(class_Dataflow).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, ActorSettings, DataflowName, 
		 ExperimentManagerPid ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, only two replacements were performed to update arities)
-define( wooper_construct_export, new/3, new_link/3,
		 synchronous_new/3, synchronous_new_link/3,
		 synchronous_timed_new/3, synchronous_timed_new_link/3,
		 remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		 remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
		 remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
		 construct/4, destruct/1 ).



% Member method declarations:
%
-define( wooper_method_export, onFirstDiasca/2, activate/1 ).



% Design notes:
%

% Name of a dataflow:
-type dataflow_name() :: string().


% PID of a dataflow (i.e. a dataflow actor):
-type dataflow_pid() :: class_Actor:actor_pid().



% Helpers:
-export([ to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Core.Dataflow.Dataflow-instance").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").




% Implementation notes:
%


% Dataflow-specific attributes are:
%
% - 



% Constructs a new dataflow, to account for an actual dataflow:
%
% - ActorSettings :: actor_settings() describes the actor abstract identifier
% (AAI) and seed of this actor, as assigned by the load balancer
%
% - DataflowName :: dataflow_name() is a human-readable name for that dataflow
% (as a plain, non-empty string)
%
% - ExperimentManagerPid :: 
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 dataflow_name(),  ) -> wooper:state().
construct( State, ActorSettings, BlockName, ActivationPolicy, InputPorts,
		   OutputPorts ) ->

	% First the direct mother class:
	ActorState = class_Actor:construct( State, ActorSettings, BlockName ),

	Policy = check_policy( ActivationPolicy ),

	InputTable = get_inputs( InputPorts ),

	OutputTable = get_outputs( OutputPorts ),

	% Then the class-specific actions:
	setAttributes( ActorState, [

		{ policy, Policy },
		{ input_ports, InputTable },
		{ output_ports, OutputTable },
		{ trace_categorization,
		  text_utils:string_to_binary( ?TraceEmitterCategorization ) }

								] ).



% To silence Dialyzer:
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->
	State.



% Methods section.



% Callback executed on the first diasca of existence of this block.
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _CallerPid ) ->

	?debug_fmt( "Created ~s.", [ to_string( State ) ] ),

	?wooper_return_state_only( State ).



% Callback executed automatically whenever the block is activated.
%
% Meant to be overridden.
%
% (oneway)
%
-spec activate( wooper:state() ) -> oneway_return().
activate( State ) ->

	?warning_fmt( "Default, do-nothing activation triggered for ~s.",
				  to_string( State ) ),

	State.




% Helper functions.


% Checks the block activation policy:
%
check_policy( P=activate_on_new_ready ) ->
	P;

check_policy( P=activate_when_all_ready ) ->
	P;

check_policy( P ) ->
	throw( { unsupported_activation_policy, P } ).



% Checks and registers the specified input ports:
%
get_inputs( InputPorts ) ->
	EmptyInputs = table:new(),
	get_inputs( InputPorts, EmptyInputs ).


get_inputs( _InputPorts=[], InputTable ) ->
	InputTable;

get_inputs( _InputPorts=[ _InputPort=Name | T ], InputTable )
  when is_list( Name ) ->
	InputRec = #input_port{},
	get_inputs( T, table:addEntry( _K=Name, _V=InputRec, InputTable ) );

get_inputs( _InputPorts=[ _InputPort={ Name, Type } | T ], InputTable )
  when is_list( Name ) ->

	case meta_utils:is_type( Type ) of

		true ->
			InputRec = #input_port{ value_type=Type },
			get_inputs( T, table:addEntry( _K=Name, _V=InputRec, InputTable ) );

		false ->
			throw( { unknown_port_type, Type } )

	end.


% Checks and registers the specified output ports:
%
get_outputs( OutputPorts ) ->
	EmptyOutputs = table:new(),
	get_outputs( OutputPorts, EmptyOutputs ).


get_outputs( _OutputPorts=[], OutputTable ) ->
	OutputTable;

get_outputs( _OutputPorts=[ _OutputPort=Name | T ], OutputTable )
  when is_list( Name ) ->
	OutputRec = #output_port{},
	get_outputs( T, table:addEntry( _K=Name, _V=OutputRec, OutputTable ) );

get_outputs( _OutputPorts=[ _OutputPort={ Name, Type } | T ], OutputTable )
  when is_list( Name ) ->

	case meta_utils:is_type( Type ) of

		true ->
			OutputRec = #output_port{ value_type=Type },
			get_outputs( T, table:addEntry( _K=Name, _V=OutputRec, OutputTable ) );

		false ->
			throw( { unknown_port_type, Type } )

	end.



% Returns a textual description of this block.
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	InputPorts = ?getAttr(input_ports),

	% Alphabetically, by port name:
	SortedInputs = lists:keysort( _N=1, table:enumerate( InputPorts ) ),

	InputDetailed = case SortedInputs of

		[] ->
			"no input port";

		[ { SingleIName, SingleIDesc } ] ->
			text_utils:format( "a single ~s",
					   [ input_port_to_string( SingleIName, SingleIDesc ) ] );

		_ ->
			InputStrings = [ input_port_to_string( IName, IPort ) ||
							   { IName, IPort } <- SortedInputs ],
			InputDesc = text_utils:strings_to_string( InputStrings ),
			text_utils:format( "~B input ports:~s",
							   [ table:size( InputPorts ), InputDesc ] )

	end,

	OutputPorts = ?getAttr(output_ports),

	OutputSize = table:size( OutputPorts ),

	% Alphabetically, by port name:
	SortedOutputs = lists:keysort( _N=1, table:enumerate( OutputPorts ) ),

	OutputDetailed = case SortedOutputs of

		[] ->
			"no output port";

		[ { SingleOName, SingleODesc } ] ->
			text_utils:format( "a single ~s",
					   [ output_port_to_string( SingleOName, SingleODesc ) ] );

		_ ->
			OutputStrings = [ output_port_to_string( IName, IPort ) ||
							   { IName, IPort } <- SortedOutputs ],
			OutputDesc = text_utils:strings_to_string( OutputStrings ),
			text_utils:format( "~B output ports:~s",
							   [ OutputSize, OutputDesc ] )

	end,

	text_utils:format( "Block '~s', applying the ~s policy, having "
					   "~s and ~s",
					   [ ?getAttr(name), ?getAttr(policy),
						 InputDetailed, OutputDetailed ] ).



% Returns a textual description of specified input port.
%
input_port_to_string( PortName, _InputPort=#input_port{
											  value_type=Type,
											  value_status =Status,
											  last_receiving=LastTimestamp,
											  feeder_port=Feeder
											 }) ->

	TypeString = case Type of

					 undefined ->
						 "untyped values";

					 _ ->
						 TextualType = meta_utils:type_to_string( Type ),
						 text_utils:format( "values of type ~s",
											[ TextualType ] )

	end,

	StatusString = case Status of

		unset ->
			"not set";

		{ ready, Value } ->
			text_utils:format( "set to value '~s'", [ Value ] )

	end,

	LastString = case LastTimestamp of

					 none ->
						 "never having received a value";

					 _ ->
						 text_utils:format(
						   "having last received a value at ~p",
						   [ LastTimestamp ] )

	end,

	FeederString = case Feeder of

					   undefined ->
						   "not fed by an output port";

					   { OutputBlockName, OutputPortName }  ->
						   text_utils:format(
							 "fed by output port ~s of block ~s",
							 [ OutputPortName, OutputBlockName ] )

	end,

	text_utils:format( "input port named '~s', expecting ~s, ~s, ~s, ~s",
					   [ PortName, TypeString, StatusString, LastString,
						 FeederString ] ).



% Returns a textual description of specified output port.
%
output_port_to_string( PortName, _OutputPort=#output_port{
											  value_type=Type,
											  last_sending=LastTimestamp,
											  fed_ports=FedPorts
											 }) ->

	TypeString = case Type of

					 undefined ->
						 "untyped values";

					 _ ->
						 TextualType = meta_utils:type_to_string( Type ),
						 text_utils:format( "values of type ~s",
											[ TextualType ] )

	end,

	LastString = case LastTimestamp of

					 none ->
						 "never having sent any value";

					 _ ->
						 text_utils:format(
						   "having last sent a value at ~p",
						   [ LastTimestamp ] )

	end,

	FedString = case FedPorts of

				   [] ->
					   "not feeding any input port";

				   [ { InputBlockName, InputPortName } ] ->
					   text_utils:format( "feeding input port ~s of block ~s",
										  [ InputPortName, InputBlockName ] );

					_ ->
						FedStrings = [ text_utils:format(
										 "port '~s' of block '~s'",
										 [ PName, BName ] ) ||
										 { PName, BName } <- FedPorts ],

						text_utils:format(
						  "feeding following ~B input ports:~n~s", [
							length( FedPorts ),
							text_utils:strings_to_string( FedStrings ) ] )

	end,

	text_utils:format( "output port named '~s', expecting ~s, ~s, ~s",
					   [ PortName, TypeString, LastString, FedString ] ).

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



% Base class for all dataflow elements, defining the constructs that are common
% to most of the actual parts constituting a dataflow.
%
% This is a specialization of the generic actor, meant to introduce the first
% transverse dataflow elements.
%
% This class should provide most of the basics needed to properly describe most
% blocks, including various built-in activation policies. It may be subclassed
% if needed to introduce variants.
%
% Please refer to the 'Sim-Diasca Dataflow HOWTO' for further information.
%
-module(class_DataflowElementActor).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, ActorSettings, ElementName, InputPorts,
		 OutputPorts, DataflowPid ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, only two replacements were performed to update arities)
-define( wooper_construct_export, new/5, new_link/5,
		 synchronous_new/5, synchronous_new_link/5,
		 synchronous_timed_new/5, synchronous_timed_new_link/5,
		 remote_new/6, remote_new_link/6, remote_synchronous_new/6,
		 remote_synchronous_new_link/6, remote_synchronisable_new_link/6,
		 remote_synchronous_timed_new/6, remote_synchronous_timed_new_link/6,
		 construct/6, destruct/1 ).



% Member method declarations:
%
-define( wooper_method_export, onFirstDiasca/2 ).



% Design notes:
%
% Input and output ports are translated as records (hence passive
% datastructures), owned by the dataflow element that comprises them.
%
% Channels (a pair made of an input port and the output port feeding it) are
% abstracted out.
%
% For more information please refer to the 'Sim-Diasca Dataflow HOWTO'.



% Name of a port (arbitrary non-empty string, unique among all ports of the same
% direction - input or output - of a block):
%
-type port_name() :: text_utils:bin_string().


% An identifier of a port:
%
-type port_id() :: { block_pid(), port_name() }.



% Semantics of a corresponding value, i.e. its associate meaning (the 'S' in
% SUTC).
%
% Ex: 'energy_demand', 'Number of tracks elapsed', 'unitary cost'.
%
-type value_semantics() :: atom().


% Unit of a corresponding value (the 'U' in SUTC), as a string in standard
% format (see the section about units in the technical manual of the 'Common'
% layer, and unit_utils.erl for more details).
%
% Ex: "km/h" or "mW.K^2.m^-3".
%
-type value_unit() :: unit_utils:unit_string().


% Type of a corresponding value (the 'T' in SUTC).
%
-type value_type() :: meta_utils:type().


% A constraint that may apply to a value.
%
-type value_constraint() :: { 'greater_than', number() }
						  | { 'lower_than', number() }
						  | { 'between', number(), number() }
						  | { 'in', list() }
						  | 'positive'
						  | 'strictly_positive'
						  | 'negative'
						  | 'strictly_negative'
						  | 'non_null'.



% Constraints applying to a corresponding value (the 'C' in SUTC).
%
-type value_constraints() :: [ value_constraint() ].



% The actual value possibly held by a port:
-type actual_value() :: any().


% Current value-related status of a port ("readiness"):
%
% (as a pair, so that values like 'unset' can be held as well)
%
-type value_status() :: 'unset' | { 'set', actual_value() }.


% An identifier of an input port:
-type input_port_id() :: port_id().


% An identifier of an output port:
-type output_port_id() :: port_id().


% Timestamp for port-related events (a pair made of a tick offset and a diasca):
%
-type port_timestamp() :: 'none' | class_TimeManager:logical_timestamp().



% Describes an input port of a block.
%
-record( input_port, {

		   % Name of that input port not stored here, as this name is the key
		   % associated to this record: name :: port_name(),

		   % SUTC information first:

		   % Semantics of the information carried by this port (may not be
		   % defined):
		   %
		   value_semantics = undefined :: 'undefined' | value_semantics(),

		   % Unit of the values that this port may receive (mandatory):
		   value_unit :: value_unit(),

		   % Type of the values that this port may receive (mandatory):
		   value_type :: value_type(),

		   % Constraints that apply to the values that this port may receive
		   % (may not be defined):
		   %
		   value_constraints = [] :: value_constraints(),

		   % Value (if any) being currently held by this port, with status
		   % information:
		   %
		   value_status = 'unset' :: value_status(),

		   % Timestamp of the last receiving (if any) performed by this port:
		   last_receiving = 'none' :: port_timestamp(),

		   % The output port (if any) feeding this input port:
		   feeder_port = undefined :: 'undefined' | output_port_id()

}).

-type input_port() :: #input_port{}.



% Describes an output port of a block.
%
-record( output_port, {

		   % Name of that input port not stored here, as this nam is the key
		   % associated to this record: name :: port_name(),

		   % SUTC information first:

		   % Semantics of the information carried by this port (may not be
		   % defined):
		   %
		   value_semantics = undefined :: 'undefined' | value_semantics(),

		   % Unit of the values that this port may send (mandatory):
		   value_unit :: value_unit(),

		   % Type of the values that this port may send (mandatory):
		   value_type :: value_type(),

		   % Constraints that apply to the values that this port may send (may
		   % not be defined):
		   %
		   value_constraints = [] :: value_constraints(),

		   % Type of the values that this port should send (if defined):
		   value_type = undefined :: 'undefined' | meta_utils:type(),

		   % Timestamp of the last sending (if any) performed by this port:
		   last_sending = 'none' :: port_timestamp(),

		   % The input ports (if any) fed by this output port:
		   fed_ports = [] :: [ input_port_id() ]

}).

-type output_port() :: #output_port{}.


-export_type([ port_name/0, port_id/0,
			   value_semantics/0, value_unit/0, value_type/0,
			   value_constraint/0, value_constraints/0, actual_value/0,
			   value_status/0,
			   input_port_id/0, output_port_id/0, port_timestamp/0,
			   input_port/0, output_port/0
			 ]).


% Helpers:
-export([ to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Core.Dataflow.Element").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").




% Implementation notes:
%


% The specific attributes of a dataflow element are:
%
% - input_ports :: table( port_name(), input_port() ), an associative table
% which stores the input ports of that dataflow element actor
%
% - output_ports :: table( port_name(), output_port() ), an associative table
% which stores the output ports of that dataflow element actor
%
% - dataflow_pid :: dataflow_pid() is the PID of the dataflow including this
% element


% Constructs a new dataflow element from:
%
% - ActorSettings :: actor_settings() describes the actor abstract identifier
% (AAI) and seed of this actor, as assigned by the load balancer
%
% - ElementName :: string() is a human-readable name for that dataflow element
% (as a plain, non-empty string)
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 string(), [ input_port() ], [ output_port() ],
				 dataflow_pid() ) -> wooper:state().
construct( State, ActorSettings, ElementName, InputPorts, OutputPorts,
		   DataflowPid ) ->

	% First the direct mother class:
	ActorState = class_Actor:construct( State, ActorSettings, ElementName ),

	InputTable = interpret_input_ports( InputPorts ),

	OutputTable = get_outputs( OutputPorts ),

	% Then the class-specific actions:
	setAttributes( ActorState, [

		{ input_ports, InputTable },
		{ output_ports, OutputTable },
		{ dataflow_pid, DataflowPid },
		{ trace_categorization,
		  text_utils:string_to_binary( ?TraceEmitterCategorization ) }

								] ).



% To silence Dialyzer:
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->
	State.



% Methods section.




% Checks and registers the specified input ports:
%
interpret_input_ports( InputPorts ) ->
	EmptyInputs = table:new(),
	interpret_input_ports( InputPorts, EmptyInputs ).


% Helper:
interpret_input_ports( _InputPorts=[], InputTable ) ->
	% Work done:
	InputTable;

interpret_input_ports( _InputPorts=[ _InputPort=Name | T ], InputTable )
  when is_list( Name ) ->
	InputRec = #input_port{},
	interpret_input_ports( T, table:addEntry( _K=Name, _V=InputRec,
											  InputTable ) );

interpret_input_ports( _InputPorts=[ _InputPort={ Name, Type } | T ], InputTable )
  when is_list( Name ) ->

	case meta_utils:is_type( Type ) of

		true ->
			InputRec = #input_port{ value_type=Type },
			interpret_input_ports( T, table:addEntry( _K=Name, _V=InputRec, InputTable ) );

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

	text_utils:format( "Element '~s', having ~s and ~s",
					   [ ?getAttr(name), InputDetailed, OutputDetailed ] ).



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

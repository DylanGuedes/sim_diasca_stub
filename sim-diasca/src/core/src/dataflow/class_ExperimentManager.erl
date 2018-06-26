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



% The experiment manager (EM) is a singleton instance in charge of driving the
% computations that shall be operated on the simulated world in a dataflow
% context.
%
-module(class_ExperimentManager).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, ActorSettings, WorldManagerPid ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, only two replacements were performed to update arities)
-define( wooper_construct_export, new/2, new_link/2,
		 synchronous_new/2, synchronous_new_link/2,
		 synchronous_timed_new/2, synchronous_timed_new_link/2,
		 remote_new/3, remote_new_link/3, remote_synchronous_new/3,
		 remote_synchronous_new_link/3, remote_synchronisable_new_link/3,
		 remote_synchronous_timed_new/3, remote_synchronous_timed_new_link/3,
		 construct/3, destruct/1 ).



% Member method declarations:
%
-define( wooper_method_export, onFirstDiasca/2 ).



% Design notes:
%
% The experiment manager is in charge of all the dataflow computations that are
% to happen, i.e. in practice of all the dataflow instances that have been
% defined.
%
% This singleton additionally interacts with its descriptive counterpart, the
% world manager.




% Helpers:
-export([ to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Core.Dataflow.ExperimentManager").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For dataflow-related types and names:
-include("dataflow_defines.hrl").


% Implementation notes:
%


% Attributes that are specific to the experiment manager are:
%
% - world_manager_pid :: world_manager_pid() is the PID of the world manager



% Constructs the experiment manager, from:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 world_manager_pid() ) -> wooper:state().
construct( State, ActorSettings, WorldManagerPid ) ->

	% Auto-subscribing:
	WorldManagerPid ! { setExperimentManager, self() },

	% First the direct mother class:
	ActorState = class_Actor:construct( State, ActorSettings,
										_Name="ExperimentManager" ),

	basic_utils:register_as( ?experiment_manager_name, global_only ),

	% Then the class-specific actions:
	setAttributes( ActorState, [

		{ world_manager_pid, WorldManagerPid },
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



% Helper functions.



% Returns a textual description of this actor.
%
% (helper)
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	WorldString = case ?getAttr(world_manager_pid) of

					undefined ->
						"not associated to the experiment manager";

					WorldPid ->
						text_utils:format( "associated to the world "
										   "manager ~w", [ WorldPid ] )

	end,

	text_utils:format( "Experiment manager ~s", [ WorldString ] ).

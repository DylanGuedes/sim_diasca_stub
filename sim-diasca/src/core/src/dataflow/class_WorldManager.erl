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



% The world manager (WM) is a singleton instance in charge of federating all the
% instance managers that account for the simulated world in a dataflow context.
%
-module(class_WorldManager).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, ActorSettings ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, only two replacements were performed to update arities)
-define( wooper_construct_export, new/1, new_link/1,
		 synchronous_new/1, synchronous_new_link/1,
		 synchronous_timed_new/1, synchronous_timed_new_link/1,
		 remote_new/2, remote_new_link/2, remote_synchronous_new/2,
		 remote_synchronous_new_link/2, remote_synchronisable_new_link/2,
		 remote_synchronous_timed_new/2, remote_synchronous_timed_new_link/2,
		 construct/2, destruct/1 ).



% Member method declarations:
%
-define( wooper_method_export, onFirstDiasca/2, setExperimentManager/2 ).



% Design notes:
%
% The world manager is in charge of all the instance managers.
%
% This singleton additionally interacts with its computational counterpart, the
% experiment manager (see class_ExperimentManager), which is expected to declare
% itself to this world manager.


% Helpers:
-export([ to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Core.Dataflow.WorldManager").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For dataflow-related types and names:
-include("dataflow_defines.hrl").


% Implementation notes:
%
% The world manager is, directly or not, the parent of all instance managers,
% yet is not one itself, as the root of this hierarchy has a sufficiently
% specific purpose and role.


% Note: an overall table associating to a classname its instance manager could
% be built here.


% Attributes that are specific to the world manager are:
%
% - experiment_manager_pid :: experiment_manager_pid() is the PID of the
% experiment manager, once it has subscribed



% Constructs the world manager, from:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
-spec construct( wooper:state(), class_Actor:actor_settings() ) ->
					   wooper:state().
construct( State, ActorSettings ) ->

	% First the direct mother class:
	ActorState = class_Actor:construct( State, ActorSettings,
										_Name="WorldManager" ),

	basic_utils:register_as( ?world_manager_name, global_only ),

	% Then the class-specific actions:
	setAttributes( ActorState, [

		{ experiment_manager_pid, undefined },
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



% Declares the specified experiment manager.
%
% (oneway)
%
-spec setExperimentManager( wooper:state(), experiment_manager_pid() ) ->
		oneway_return().
setExperimentManager( State, ExperimentManagerPid ) ->

	wooper_check_undefined( State, experiment_manager_pid ),

	NewState = setAttribute( State, experiment_manager_pid,
							 ExperimentManagerPid ),

	?wooper_return_state_only( NewState ).



% Helper functions.



% Returns a textual description of this actor.
%
% (helper)
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	ExpString = case ?getAttr(experiment_manager_pid) of

					undefined ->
						"not associated to the experiment manager";

					ExpPid ->
						text_utils:format( "associated to the experiment "
										   "manager ~w", [ ExpPid ] )

	end,

	text_utils:format( "World manager ~s", [ ExpString ] ).

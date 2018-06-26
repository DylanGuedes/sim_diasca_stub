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



% The instance manager is the base, abstract, mother class of all actual
% instance managers.
%
% For example buildings are to be managed by a BuildingManager inheriting from
% this class.
%
% For synchronisation purposes, instance managers are actors.
%
-module(class_InstanceManager).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, ActorSettings, ManagedClass, ParentPid ).



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
-define( wooper_method_export, declareInstanceManager/3, createInstance/2,
		 onFirstDiasca/2 ).



% Design notes:
%




% Helpers:
-export([ to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Core.Dataflow.InstanceManager").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For dataflow-related types and names:
-include("dataflow_defines.hrl").


% The parent of a given instance manager is either another one, or the world
% manager, the top of the hierarchy:
%
-type parent_pid() :: instance_manager_pid() | world_manager_pid().


% Implementation notes:
%


% Attributes that are specific to an instance manager are:
%
% - managed_class :: wooper:class_name() designates the class (as an atom; ex:
% 'class_Building') governed by this manager
%
% - parent_pid :: parent_pid() is the PID of the parent instance manager of this
% one (each instance manager has eaxctly one such parent)
%
% - child_managers :: [ { wooper:class_name(), instance_manager_pid() } ] is a
% list of pairs whose first element is a classname and second is the PID of the
% manager in charge of it, federated by this instance manager; a given instance
% manager owns its child managers and as a result is to manage their life cycle
%
% - instances :: [ dataflow_actor_pid() ] is a list of the currently known
% instances of the class managed (presumably all of them)



% Constructs an instance manager, from:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - ManagedClass is the dataflow actor class managed by this instance manager
% (ex: 'class_Building')
%
% - ParentManagerPid is the PID of the parent of this manager
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 wooper:class_name(), parent_pid() ) -> wooper:state().
construct( State, ActorSettings, ManagedClass, ParentPid ) ->

	% Auto-subscribing:
	ParentManagerPid ! { declareInstanceManager, [ ManagedClass, self() ] },

	% Name it "BuildingManager" for 'class_Building':
	Name = case text_utils:atom_to_string( ManagedClass ) of

			   "class_" ++ Classname ->
				   Classname ++ "Manager";

			   OtherName ->
				   throw( { invalid_class_name, OtherName } )

	end,

	% First the direct mother class:
	ActorState = class_Actor:construct( State, ActorSettings, Name ),

	?trace_fmt( "Managing from now the instances of class '~s', "
				"and having for parent ~p.", [ ManagedClass, ParentPid ] ),

	% Then the class-specific actions:
	setAttributes( ActorState, [

		{ managed_class, ManagedClass },
		{ parent_pid, ParentPid },
		{ child_managers, [] },
		{ instances, [] },
		{ trace_categorization,
		  text_utils:string_to_binary( ?TraceEmitterCategorization ) }

								] ).



% To silence Dialyzer:
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	Children = ?getAttr(child_managers),

	?trace_fmt( "Being deleted, deleting in turn the ~B child instance "
				"managers while having still ~B ~s instances managed.",
				[ length( Children ), length( ?getAttr(instances),
				  ?getAttr(managed_class) ) ] ),

	[ ManagerPid ! delete || { _Classname, ManagerPid } <- Children ],

	setAttribute( State, child_managers, [] ).



% Methods section.


% Declares a child instance manager.
%
% (oneway, to run prior to simulation start)
%
-spec declareInstanceManager( wooper:state(), wooper:class_name(),
							  instance_manager_pid() ) -> oneway_return().
declareInstanceManager( State, ManagedClass, ManagerPid ) ->

	ChildClasses = ?getAttr(child_managers),

	case lists:keymember( _Key=ManagedClass, _N=1, ChildClasses ) of

		true ->
			throw( { multiple_declaration_for_class, ManagedClass } );

		false ->
			ok

	end,



-spec createInstance
createInstance



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

	ChildString = case ?getAttr(child_managers) of

					  [] ->
						  "no child manager";

					  Children ->
						  text_utils:format( "~B child manager(s)",
											 [ length( Children ) ] )

	end,

	text_utils:format( "Instance manager for class '~s', "
					   "federating ~B instance(s), having for parent ~p, and ",
					   [ ?getAttr(managed_class), length( ?getAttr(instances) ),
						 ?getAttr(parent_pid), ChildString
					   ] ).

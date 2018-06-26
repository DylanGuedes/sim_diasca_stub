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


% List of common types defined for the Dataflow support, to avoid having to
% prefix them with a module name.
%
% A few component names are also defined.


% Shorthand:
-type actor_pid() :: class_Actor:actor_pid().


% PID of the world manager:
-type world_manager_pid() :: actor_pid().

% PID of the experiment manager:
-type experiment_manager_pid() :: actor_pid().


% PID of a dataflow:
-type dataflow_pid() :: actor_pid().


% PID of a dataflow element actor:
-type dataflow_element_pid() :: actor_pid().


% PID of an instance manager:
-type instance_manager_pid() :: actor_pid().

% PID of a dataflow actor:
-type dataflow_actor_pid() :: dataflow_element_pid().


% PID of a block manager:
-type instance_manager_pid() :: actor_pid().

% PID of a dataflow block actor:
-type dataflow_block_pid() :: dataflow_element_pid().

% Shorthand thereof:
-type block_pid() :: dataflow_block_pid().


% PID of a dataflow mockup block actor:
-type dataflow_mockup_block_pid() :: dataflow_block_pid().



% Registered names of components:

-define( world_manager_name, sim_diasca_world_manager ).
-define( experiment_manager_name, sim_diasca_experiment_manager ).

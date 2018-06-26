=========================
Sim-Diasca Dataflow HOWTO
=========================


+---------------------------------------------------------------------------------+
| .. image:: sim-diasca.png                                                       |
|   :scale: 40                                                                    |
|   :align: center                                                                |
+---------------------------------------------------------------------------------+


.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


:Organisation: Copyright (C) 2016 EDF R&D
:Authors: Olivier Boudeville, Samuel Thiriot
:Contact: olivier.boudeville@edf.fr, samuel.thiriot@edf.fr
:Creation Date: Thursday, February 25, 2016
:Lastly Updated on: Monday, July 25, 2016



:Status: Work in progress
:Version: 0.4.5
:Dedication: Sim-Diasca model implementers
:Abstract:

	This document describes how dataflows, i.e. graphs of processing blocks, are to be defined and evaluated in the Sim-Diasca simulations relying on them.

.. meta::
   :keywords: Sim-Diasca, dataflow, how-to, block


:raw-latex:`\pagebreak`

.. contents:: Table of Contents
	:depth: 3

.. section-numbering::



:raw-latex:`\pagebreak`



Foreword
========

The simulation of complex systems often relies on loosely-coupled agents exchanging varied signals based on a dynamic, potentially complex applicative protocol over a very flexible scheduling.

However, in some cases, the modelling activity results alternatively in the computations being at least partly described as *a static network of interconnected tasks that can send values to each other over channels* that applies to a simulated world - i.e. a **dataflow**.

Both approaches will be detailed and contrasted below, before focusing on how dataflows can be defined and used with Sim-Diasca.

.. Note::
  Most of the dataflow-related concepts mentioned in this document are illustrated on a **complete, runnable simulation case**: the ``Dataflow Urban Example``, whose sources are located in the ``mock-simulators/dataflow-urban-example`` directory of the standard Sim-Diasca distribution.

  Besides these case-specific elements, the sources of the **generic dataflow infrastructure** are also available, in the ``sim-diasca/src/core/src/dataflow`` directory.

  Please feel free to skim in these respective sources for a better practical understanding of the dataflow infrastructure.



Usual Organization of the Model Evaluation
==========================================

In most simulations of complex systems, the simulated world is sufficiently **disaggregated into numerous autonomous model instances** (be they named agents or actors) so that **the evaluation of their respective behaviours and interactions naturally leads to processing the simulation**. In this context, trying to constrain or even hard-code static sequences of events is often neither possible nor desirable.

For example, one can see a city as a set of buildings, roads, people, etc., each with its own state and behaviour, the overall city (including its districts, precincts, etc.) being the byproduct of their varied interactions - a possibly hierarchical, certainly *emergent* organisation.

This approach is probably the most commonly used when modelling a complex system, hence it is the one natively supported by Sim-Diasca: the target system is meant to be described as a (potentially large) collection of model instances (a.k.a. actors) possibly affected by scenarios and, provided that their respective state and behaviour have been adequately modelled, the engine is able to evaluate them in the course of the simulation, concurrently, while actors feed the probes that are needed in order to generate the intended results.

The (engine-synchronised) interactions between actors are at the very core of these simulations, which are determined by how actors get to know each other, exchange information, opt for a course of action, create or destroy others and, more generally, interact through an **implicit overall applicative protocol resulting from the superposition of their individual, respective behaviours**.

However other, quite different, organisational schemes can be devised, including the one discussed in this section, the **dataflow** paradigm.



:raw-latex:`\pagebreak`


An Alternate Mode of Operation: the Dataflow
============================================

In this more constrained organisation, rather than having actors freely exchanging various symbols and messages according to dynamically-decided patterns, we have specialised actors that are dataflow elements, which are:

- either *blocks* (instances of the ``DataflowBlock`` class), which set and listen for *values*, through *channels* that are delimited each by an *input port* and an *output port*

- or *dataflow actors* (instances of the ``DataflowActor`` class) that comprise *dataflow attributes*, each of these attributes being associated to one input port and one output port


These dataflow elements and the channels linking them form altogether a graph (whose nodes are the elements, and whose edges are the channels). This graph is by default:

- **statically defined**: its structure can be established before the simulation starts
- **static**: in the general case, its structure is not expected to change in the course of the simulation
- **directed**: channels are unidirectional, only from an output port of a block to an input port of a block
- **acyclic**: by following the declared (directed) channels, no path should go through the same block more than once

The graph can be explicit or not: either it is described as a whole (as a single, standalone entity), or it can be merely extrapolated from the union of the channels drawn between the declared blocks.


Dataflows of course have an immediate graphical counterpart. The conventional symbols we elected are represented in this key:

:raw-html:`<img src="">dataflow-legend.png</img>`
:raw-latex:`\includegraphics[scale=0.33]{dataflow-legend.png}`


By convention, input ports are in orange, output ports in green, blocks are in light-blue and comprise the symbol of their activation policy, and channels are in blue.

Still in blue, the ``SUTC`` quadruplet:

- the channel *Semantics* (i.e. the meaning of the conveyed values) can be specified, as an arbitrary domain-specific symbol (project conventions may apply) prefixed with ``"S:"`` (like in ``"S: 'produced heat'"``)
- the *Type* of the values conveyed by the channel, prefixed with ``"T:"`` (ex: ``"T: string"`` or ``"T: {integer,boolean}"``)
- the *Unit* of the value, prefixed with ``"U:"`` (ex: ``"U: kW.h"``, or ``"U: g/Gmol.s^-2"``); often the unit information implies a type (ex: ``"U: W"`` implies ``"T: float"``) and, as a consequence, this type information can be safely omitted
- the *Constraints* (if any) applying to the exchanged value, as a list of elementary constraints (ex: ``"C: [ {between,{2.0,8.0}} ]"`` means that a single constraint applies to the exchanged values, which is that they must be between 2 and 8)

These ``SUTC`` information shall preferably be specified close to the associated channel (if any) or output port.

Block activation, semantics, units, types and constraints are discussed more in-depth later in this document.

Specifying the names of blocks and ports is mandatory.

As a block is in charge of *performing* a specific task included in a more general computation graph (the dataflow), its name shall reflect that; one may consider that the name of a block is implicitly prefixed with a verb like ``compute_``. For example, a block named ``fuel_intake`` could be understood as ``compute_fuel_intake`` (and we expect it to have at least one output port dealing with fuel intake).

Finally, as some dataflow elements have for purpose to aggregate metrics across time and/or space, some scale indication may be given for documentation purposes, enclosed in an hexagon in pale yellow.

The dataflow actors are specifically discussed in a section of their own later in this document.

As a result, a dataflow, which shall be interpreted as **a graph of computations**, may look as this (meaningless) example:

:raw-html:`<img src="">dataflow-example.png</img>`
:raw-latex:`\includegraphics[scale=0.2]{dataflow-example.png}`


We can see that a dataflow does not need to be fully connected (the blocks may form disjoint subgraphs) and that ports (input and output ones alike) may not be connected either.

The global progress of the computations happens here from left to right.

A `more complete example`_ is given later in this document.

Now let's detail a bit all the elements involved.



:raw-latex:`\pagebreak`


On Ports and Channels
=====================

Following rules apply:

- a port is either an **input** one (listening to the update of a value conveyed by the corresponding channel) or an **output** one (able to update its corresponding value and notify its registered input listeners)
- each port is **named** (as a non-empty string, ex: ``"my foobar port"``) and no two input ports of a block can bear the same name, nor output ones can (however an input port and an output port of the same block can have the same name)
- a **port identifier** is defined from a pair made of an identifier of the block that defined it and from the name of that port [#]_ (ex: it could be ``("My Block","Port 24")``, or based on more technical identifiers)
- a port (input or output) may either hold a value (arbitrary data can be set; the port is then considered as ready, i.e. as ``set``), or not - in which case it holds the ``unset`` symbol (the port is then itself considered as ``unset``)
- an **output** port can be considered as being always unset: as soon as a new value is available, it notifies all its connected input ports and then reverts back to the unset status; therefore the set/unset status can be abstracted out for output ports, which just get punctually activated
- conversely, this status matters for **input** ports: a block starts with all its input ports to ``unset``, and, each time an input port is notified by an output port, this input port switches to ``set``; how a block is to react depending on none, one, some or all of its input ports being set is discussed below
- an output port will send data if and only if set:

 - if not set, no sending is performed
 - if set, one sending is performed, regardless of the value; as a result, a port may be explicitly set to a value that happens to be the same as the one that was already available - this will nevertheless trigger a sending (thus not setting a value and merely "touching" a constant value are operations that differ semantically)

- ports can convey arbitrary data (i.e. any Erlang term), yet any given port has a **type**, which defines what are the licit the values that it can hold (ex: "this port can be set to any pair of non-negative floats") [#]_
- **a block can declare any number of output ports** (possibly none, in which case it is an *exit block*, a sink)
- **a block can declare any number of input ports** (possibly none, in which case it is an *entry block*, a source)
- **a channel links exactly one output port to one input port**, and these two ports shall have the same types, units and semantics (which are the ones of the channel)
- **any number of channels may originate from an output port** (possibly none); when an output port is being set (i.e. when it performs a punctual transition from ``unset`` to ``set``), then all the input ports listening to it are notified of that [#]_
- **an input port may be the target of up to one channel**; if no channel feeds a port, then it remains in the ``unset`` state
- a port records the timestamp (in simulation time) of the last notification (possibly ``none``) it either sent (for output ports) or received (for input ones)

.. [#] A port identifier is typed as ``-type port_id() :: {dataflow_actor_pid(),port_name()}.`` where ``dataflow_actor_pid()`` is a PID (the one of the block) and ``port_name()`` is a binary string.

.. [#] The dataflow system may or may not check that typing information.

.. [#] Indeed the ``onInputPortSet/3`` actor oneway of their respective block is executed, specifying the port identifier of the triggered input port and the corresponding timestamped value (specifying the tick and diasca of the notification). Generally this information is not of interest for the block implementer, as defining a block activation policy allows to handle automatically input ports being triggered.

Even if conceptually it is sufficient that only the output port knows the input ports it may notify (and not the other way round), technically the input ports also know the (single, if any) output port that may notify them, for example for a simpler support of unsubscribing schemes.



:raw-latex:`\pagebreak`


On Values
=========

We saw that a value designates **a piece of data carried by a channel**, from an output port to any number of input ports.

Various information are associated to the output ports and to the values they carry (they are metadata), notably the ``SUTC`` quadruplet (for *Semantics-Units-Type-Constraints*), which the next sections detail in turn, yet in a different order for the sake of clarity - roughly from the most concrete to the highest-level.



Type of a Value
---------------

A channel is **typed**, in the sense that all the values that it conveys shall be of the same type - the one specified for the channel. The ``T`` in ``SUTC`` stands for this *type* information.

Most specifications rely only on scalar, atomic values (moreover, often floating-point ones), as opposed to compound ones. However, if deemed useful, more complex data structures may be specified, based on tuples (denoted as ``{}``), on lists (denoted as ``[]``) or even, in the future, on associative tables.

Following types [#]_ are built-in (they map to Erlang native types; some related technical details put between parentheses may be safely ignored by the reader):

- ``boolean``: a symbol being either ``'true'`` or ``'false'``
- ``atom``: any symbol in a user-defined set, like an enumeration, for example one in ``'burner_enabled' | 'burner_disabled' | 'burner_on_operation'``, the ``"|"`` characger representing the OR operator; atoms are made of alphanumeric characters and spaces, and must be enclosed in single quotes; specifying an atom may be seen as defining a symbol
- ``integer``: an (unbounded) integer value, ex: ``-112``
- ``float``: a floating-point value, ex: ``3.14159``
- ``number``: either an integer or a float
- ``list``: any kind of (proper) list (homogeneous or not, empty or not, etc.), represented between square brackets; ex: ``[1,'hello',[]]``, to account for a variable-size sequential container
- ``tuple``: any kind of tuple, represented between curly braces; ex: ``{'foobar',14.0}``, to account for a fixed-size container
- ``string``: any string (mapped as a binary one, displayed as a basic one), ex: ``"I am a string."``
- ``any``: this default type corresponds to a wildcard, i.e. to any type (no information given, hence no checking can be performed)

.. [#] Note that some of them overlap, notably some types are special cases of others, like booleans and atoms, integers and numbers, etc.

The type specification in a dataflow shall be prefixed with ``"T:"`` (ex: ``"T: integer"``).

In the absence of unit information (see next section), the type information is a mandatory information and must be specified by both port endpoints. It may or may not be checked, at build and/or run time.

.. comment  symbols not starting by a lowercase letter or containing spaces must be enclosed in single quotes (ex: ``'MySymbol'`` or ``'my symbol'``)



Unit of a Value
---------------

A value of a given type (typically a float) can actually correspond to quantities as different as meters and kilowatts per hour.

Therefore **units shall preferably be specified alongside with values**, and a language to express these units must be retained. The ``U`` in ``SUTC`` stands for this *unit* information.

One should refer to the documentation of the ``Common`` layer [#]_ for a description of how units can be specified, compared, checked and used.

.. [#] Please refer to the *Description of the Management of Units* section, in the technical manual of the Common layer (in ``Ceylan-Common-Layer-technical-manual-english.pdf``).

In a dataflow, the unit of the values that will be held by a port shall preferably be specified when declaring that port. This is done thanks to a string, prefixed with ``"U:"`` (ex: ``"U: kW.h"``, ``"U: g/Gmol.s^-2"`` or ``"U: {mm,mm,mm}"`` for a 3D vector in millimeters).

Specifying the unit of a scalar value implies declaring its type as ``float``.

If, for a value, no unit is given, then its type, as discussed in `Type of a Value`_, shall be specified.



Semantics of a Value
--------------------

Specifying the type and unit of a value is certainly useful, yet it would generally be insufficient to convey its *meaning*, i.e. to express how that value shall be interpreted.

For example, knowing that a port accepts floating-point values in kilojoules does not tell whether this value corresponds to an energy demand, an actual consumption or a production.

Therefore this domain-specific information shall be specified separately. It is to be done thanks to the specification of a symbol (corresponding to an Erlang atom), prefixed with ``"S:"``, standing for **semantics** (which is the ``S`` in ``SUTC``). For example: ``"S: 'security credentials'"`` or ``"S: 'energy_demand'"``.

As a result, the full chain (the output port, the channels, the value itself and the related input ports) can perform a basic check of the semantic consistency for each  exchange over the dataflow, and have an extra chance of detecting and rejecting any erroneous port connection (even if in technical terms, i.e. in terms of typing and unit, it may look perfectly legit).

Generally the channel is shown as bearing the semantics, implying that this formalised meaning is shared by the corresponding output port, the associated input ports and by the values that they exchange.



Constraints Applying to a Value
-------------------------------

The ``C`` in ``SUTC`` stands for this *constraints* information.

They allow to specify a set of rules by which the value must abide.

Following constraints can be mixed and matched:

- ``{greater_than,G}`` means that the (scalar) value must be greater than, or equal to, the number ``G``
- ``{lower_than,L}`` means that the (scalar) value must be lower than, or equal to, the number ``L``
- ``{between,A,B}`` means that the (scalar) value must be greater than, or equal to, the number ``A`` and lower than, or equal to, the number ``B``
- ``{in,L}`` means that the value must be an element of the list ``L``
- ``positive`` means that the (scalar) value must be positive (possibly null)
- ``strictly_positive`` means that the (scalar) value must be strictly positive (null not allowed)
- ``negative`` means that the (scalar) value must be negative (possibly null)
- ``strictly_negative`` means that the (scalar) value must be strictly negative (null not allowed)
- ``non_null`` means that the (scalar) value must not be null (strictly positive or negative, zero not allowed)


For example, constraints applying to a value could be::

  C: [ {between,2020,2040}, {in,[1989,2021,2030,2988]} ]


All constraints have to apply (as if they were associated by ``AND`` operators). The previous example would thus allow only two possible values, ``2021`` and ``2030``.

Various additional kinds of constraints may be supported, based on encountered needs.

Constraints are currently parametrised by *constants* (ex: ``{greather_than,10.0}``); maybe in the future they could also accept *references* onto other local ports (ex: to compare their values or base some constraints on operations, like ``sum``, performed on their values).



Accuracy of a Value
-------------------

This may the next value-level metadata to be handled by the dataflow infrastructure.

Depending on various factors like data quality and numerical errors (ex: floating-point rounding), the computed values might show a good precision and many digits, yet a poor `accuracy <https://en.wikipedia.org/wiki/Accuracy_and_precision>`_.

The first step to prevent it is to measure how accurate a computation is. This can be evaluated thanks to `relative error and ulps <https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html#689>`_ (for *units in the last place*).

So an accuracy may be associated to each value exchanged over the dataflow, and it may then be updated by each block relying on it.

By default accuracy is best measured in terms of relative error, as, if ulps are the most natural way to measure rounding error, they are less suitable to analyse the error caused by various formulas.

Anyway, often only the order of magnitude of rounding errors is of interest, and ulps and relative errors may be used interchangeably since their magnitude differ by at most a constant factor, the radix, typically equal to 2 (binary representation), or less frequently 10 (decimal one).

Another measure could be the "precision", once defined as the number of bits used to represent the significand of a floating-point number. Libraries like `MPFR <http://www.mpfr.org/>`_ can be given a target, arbitrary precision and may enforce it (hence we would expect the accuracy of the corresponding values to be constant across the corresponding ports).

Each project is free to retain its own conventions regarding how the accuracy is quantified (usually as a floating-point number). The dataflow infrastructure provides the mechanisms to keep track of it, and update it in blocks. An accuracy specification is to be prefixed with ``"A:"``, like in ``"A: 8"``.

Should no accuracy be used for a given value, it should be replaced by the ``'unknown_accuracy'`` atom (which is the default).

The accuracy could be also translated as a confidence interval, i.e. an interval that covers an unknown parameter with probability ``P=1-alpha``, where ``P`` is the confidence level, and ``alpha`` should be as close as possibly to 0 (typical values of ``P``: 0.95, 0.99 or 0.999) thanks to a sufficiently large number of samples.

As no general consensus exists about accuracy, it has not been included among the usual metadata associated to values. In the future this could added, accuracy becoming the ``A`` of ``SUTCA``.



Wrapping Up About Values
------------------------

So an output port may send a notification to a set of input ports, with the following information being associated:

- a semantics, like in ``S: 'energy_demand'``
- a unit, like in ``U: kW.h``
- a type, like in ``T: float``
- constraints, like in ``C: [{lower_than,100}]``
- an accuracy, like in ``A: 11.0``, to be understood here as the number of bits for the precision of the significand
- a value, like in ``6.7121``
- a timestamp, like in ``{117,3}``, i.e. tick offset #117, diasca 3
- the port identifier of the sender


The semantics, the unit, the type carried by endpoints and the sender port identifier are exchanged and checked at the channel creation, i.e. when the input port is linked to its output one.

The unit of the value, its associated constraints, its accuracy, its actual value and its timestamp are checked and sent to the input port each time it is triggered by its output port.




:raw-latex:`\pagebreak`


Logic of the Block Activation
=============================

We saw that a key element of a dataflow is its blocks.

For a given dataflow block, it must be decided:

- at which logical step the block may be activated, i.e. *when* the activation of a block shall be examined
- on which condition(s) it shall be activated, i.e. *how* the block update shall be determined as having to be triggered
- what results from such an activation, i.e. *what* are the operations this block should then perform


Such an activation translates to the execution of the ``activate/1`` method of that block. The role of this method is to be **the place where the block defines its actual processing**; for that, the block most probably overrode the corresponding default do-nothing implementation.

During this processing, as for any actor oneway, the block is free to perform **computations**, to send **actor messages** and to operate **state changes**. This includes notably reverting any of its input ports to the ``unset`` state, and activating any of its output ports.

Now that it has been determined *what* an activation entails (pretty much anything within the degrees of freedom of an actor), the conditions ruling *when* an activation shall occur are to be specified. Various policies are available for that.

For a given **activation policy**, these conditions should only depend on the readiness of the input ports of that block, and of its state.

Even if a given block might define its own activation rules, the set of built-in activation policies described below should be sufficient for most uses.

In all cases, under the hood the block will be notified thanks to an actor message that one of its input port has been triggered, knowing that during a diasca any number of such messages may be received (indeed a block may have multiple input ports; moreover, even if it may not be usual practise, an upstream block might have triggered one of its output ports more than once) and then reordered before being processed on the next diasca.


The 'Activate On New Set' Policy
--------------------------------

The first built-in activation policy is to update the block when **at least one of its input ports** went from ``unset`` to ``set``.

This policy, named ``activate_on_new_set``, will activate the block at most *once per diasca*, at the one immediately following the diasca at which these input ports were triggered, no matter of how many input ports were triggered on the previous diasca nor on how many times they were each triggered.

An (unordered) list of input port triggers, together with the corresponding values then set, will be available to the ``activate/1`` method when it will be automatically executed.

Either a bulk update may follow (the block taking them into account as a whole), or it may perform a fold on that list to react in turn to each trigger (to emulate the case where they would be received and processed one after the other [#]_).

.. [#] Note that, as all actor messages, the triggers have been reordered by the engine according to the simulation mode.

It is up to the block to reset the input ports (i.e. to set each of them back to the ``unset`` state) when deemed appropriate.

:raw-html:`<img src="">activate-on-new-set-policy.png</img>`
:raw-latex:`\includegraphics[scale=0.33]{activate-on-new-set-policy.png}`

This *Activate On New Set* policy (sometimes shortened as the "On New" policy) is graphically symbolized as an arrow, to denote that any update of an input port directly triggers the associated block computation.




The 'Activate When All Set' Policy
----------------------------------

The second built-in activation policy, named ``activate_when_all_set``, is to update the block if and only if **all of its input ports have been set**: each time an input port is triggered, this policy automatically determines if it was the last one still unset and, if yes, it executes the ``activate/1`` method.

This policy will also take care, once that method has been executed, to automatically set back all input ports to their ``unset`` state.

:raw-html:`<img src="">activate-when-all-set-policy.png</img>`
:raw-latex:`\includegraphics[scale=0.33]{activate-when-all-set-policy.png}`

This *Activate When All Set* policy (sometimes shortened as the "When All" policy) is graphically symbolized as a star resembling to a lock, to denote that no associated block computation will take place until all input ports have been enabled (i.e. are set).




Custom Activation Policies
--------------------------

Some blocks may require, under rare circumstances, a custom policy, i.e. **a policy of their own** that does not match any of the built-in ones.

For example source blocks, i.e. blocks not having any input port, can be defined, but of course then none of the policies above can apply (as they can never be triggered). Nevertheless such source blocks are typically needed in order to bootstrap the processing of a dataflow.

To solve this, rather than forcing the definition of at least one "dummy" input port per block, **all blocks can also be explicitly triggered**: they can rely on their ``activateExplicitly/2`` actor oneway for that, in charge of calling their ``activate/1`` oneway as other policies do.

This policy may for example also be used to account for blocks having fixed, active temporalities. A daily-activated block may schedule itself every 24 hours (declaring such a regular spontaneous scheduling, during which it may activate its output ports), while another block may be ruled per-hour.

So dataflows can federate mixed temporalities, knowing that the use of this policy of explicit activation is fully optional (as by default a dataflow is fully passive and is only driven by changes in its input ports) and shall be regarded only as a last resort, should the built-in policies be insufficient.

:raw-html:`<img src="">custom-activation-policy.png</img>`
:raw-latex:`\includegraphics[scale=0.33]{custom-activation-policy.png}`

This *Custom* policy is graphically symbolized as a sheet of paper, to denote that the block activation is driven by a freely chosen user-specified logic.



:raw-latex:`\pagebreak`


On Dataflow Actors
==================

Dataflow Blocks allow to describe the computations that shall be performed, yet generally they have to rely on the structure of the simulated world to feed their computations with relevant data.

Holding these information, and possibly making them change over time, is the purpose of the **dataflow actors**. They are standard actors, except that they may define *dataflow attributes*, i.e. state attributes that can be read and/or written from a dataflow.

A dataflow attribute is indeed associated to a pair of ports, an input one and an output one. These ports allow to bridge the gap between two worlds:

- the one of the **multi-agent, dynamic, loosely coupled actors**, serving the purpose of *describing* a disaggregated target system and its evolution
- the one of the (mostly statically connected) **dataflow blocks**, in charge of performing *computations* over a target system


A dataflow actor is represented as an actor, yet has the same light-blue background as the dataflow block (to underline their similarities) and each of its dataflow attribute is represented like a standard attribute except that it is surrounded by an input and an output port, like in:

:raw-html:`<img src="">dataflow-actors-example.png</img>`
:raw-latex:`\includegraphics[scale=0.5]{dataflow-actors-example.png}`

In this example, all attributes are standard, except the maintenance cost, which can be read and/or written by other elements of the dataflow.



:raw-latex:`\pagebreak`

.. _`more complete example`:

A More Complete Example
=======================

Here we took the case of an hypothetical modelling of a city, in which the target system happens to be disaggregated into districts, buildings, etc.

We propose additionally to enforce here a stricter convention, which is that no two computation blocks shall interact **directly** (i.e. with an output port of one being linked to an input one of the other); their exchanges shall be **mediated** by at least one dataflow actor instead. As a consequence a block interacts solely with the target system.

Respecting such a convention allows to **uncouple the blocks** more completely: one can be used autonomously, even if the other is not used (or does not even exist).

As a result, this example simulation consists on the **intersection of two mostly independent planes**, the one of the target system (in light orange, based on actors) and the one of the computations applied on it (in light blue, based on computation blocks).

This intersection is implemented thanks to *dataflow actors* and the related channels (in blue), since they are making the bridge between the two planes.


:raw-html:`<img src="dataflow-city-example.png"></img>`
:raw-latex:`\includegraphics[scale=0.2]{dataflow-city-example.png}`

One can also notice:

- two **dataflow probes** (on the right), should specific results have to be extracted from the dataflow (read here from the output ports of some blocks)
- external **state importer and exporter**, supposing here that this simulation is integrated into a wider computation chain (respectively in charge of providing an input state of the world at each time step, and, once evaluated and updated by the dataflow, of reading back this state)

We can see that we have still here a rather high-level, abstract view of the dataflow: types are mentioned (ex: ``Building``) instead of instances (ex: ``building_11``, ``building_12``, etc.), and managers (discussed later in this document) are omitted.



:raw-latex:`\pagebreak`

On Dataflow As Such
===================

Even though dataflows could remain only implicit data-structures (just a set of interlinked dataflow elements), we preferred introducing an actual **dataflow class**, in order to ease the interaction with such instances.

So overall operations on a given dataflow (ex: creations, modifications, report inquiries) shall be operated only through its corresponding federating ``class_Dataflow`` instance.

A dataflow instance is itself managed by the overall *Experiment Manager*, introduced later in this document.



:raw-latex:`\pagebreak`


More Advanced Dataflow Uses
===========================


Dynamic Update of the Dataflow
------------------------------

One may imagine, dynamically (i.e. in the course of the simulation):

- creating or destroying blocks
- creating or destroying channels
- updating the connectivity of channels and blocks
- creating or destroying input or output ports of a block

This would be useful as soon as the target system is itself **dynamic** in some way (ex: buildings being created in a city over the years, each building relying on its associated computation blocks - which thus have to be dynamic as well, at least with varying multiplicities).

Moreover, often all the overall layout cannot be statically defined, and the dataflow as a whole has to be dynamically connected to the components feeding it or waiting for its outputs (ex: a database reader having to connect in some way to some input ports) - so some amount of **flexibility** is definitively needed.



Domain-Specific Timestamps
--------------------------

By default, engine ticks directly translate to a real, quantified simulation time: depending on the starting timestamp and on the selected simulation frequency, a given tick corresponds to an **exact time and date** in the Gregorian calendar (ex: each month and year lasting for the right number of days - not respectively, for example, 30 and 365, a simplification that is done in some cases in order to remain in a constant time-step setting).

However, in some simulations, models are ruled by such a strange simplified time [#]_, so domain-specific timestamps may be useful. Their general form is ``{timescale(),index()}``, where ``timescale()`` designates the selected time granularity for the underlying channel (ex: ``constant_year``, ``month_of_30_days``) and ``index()`` is a counter (a positive integer corresponding to as many periods of the specified time granularity).

.. [#] Even if this oversimplification just by itself yields already significant relative errors (greater than 10%).


For example, in a simulation some models may be evaluated at a yearly timescale, while others would be at a daily one. Considering that initial year and day have been set beforehand, a timestamp may become ``{yearly,5}`` or ``{daily,421}``. This could be used to check that connected ports have indeed the same temporality (ex: no yearly port linked to a daily - a *timescale convertor block* needing to be inserted in that case), and that none of such timescale-specific timesteps (i.e. index) went amiss in the channel.



Usefulness of Cyclic Dataflows?
-------------------------------

One can notice that cyclic dataflow graphs are allowed by this scheme based on input and output ports, and that even "recursive dataflow actors" (i.e. dataflow actors having one of their output port connected to one of their input ones) can exist.

Of course some convergence criterion is needed in order to avoid a never-ending evaluation.



:raw-latex:`\pagebreak`


.. _`world manager`:

The World Manager and its Instance Managers
===========================================

This manager (singleton instance of the ``WorldManager`` class), whose shorthand is ``WM``, is used in order to **create, update and keep track of the simulated world and its structure**; this world is itself made of the target system of interest (ex: a city) and of its context (ex: its associated weather system, the other cities in the vicinity, etc.).

As a result, the world manager is specific to a given modelling structure (ex: to some way of describing a city), and has no direct relationship to the operations performed on it (its purpose is indeed to help uncoupling the description of the modelled world from the computations operated on it).

This virtual world is usually modelled based on **various types of simulation actors**; for example if the target system is a city, then districts, roads, buildings, weather elements, other cities, etc. may be defined in order to represent the whole.

For each of these types, a **dedicated instance manager** exists (for example the ``BuildingManager`` will take care of ``Building`` instances). Its purpose is to be the entry point whenever having to perform dataflow-level operations on the instances of that type.

All instance managers are themselves actors, as interacting with them in the course of the simulation may be necessary, for example if needing to determine what are the buildings located in a given district.

All instance managers are federated by this ``WM`` (*World Manager*), as it is the root of the hierarchy defining the simulated world. **A given instance manager may itself aggregate other instance managers**, should the simulated world be recursively subdivided (ex: based on geographical scales).

One use of the world manager and its instance managers is the enabling of **external state synchronization**.

Should, for example, the overall simulation be actually performed by a pipe-line of platforms (including of course at least one that is based on this dataflow infrastructure), at each time step the state of the simulation at hand will need to be updated from these other platforms and, reciprocally, once modified, will need to be passed to the next platforms.

Per-class changesets may be transmitted to the corresponding instance managers, so that all the relevant dataflow actors can be appropriately updated. Then the experiment can be evaluated for this time step, probably activating at least one dataflow for that.

This will result in the update of the dataflow actors; then the world manager and its instance managers may be used in the opposite direction, in order to update the external platforms with the newer state resulting from this simulation for this time step.

All actual instance managers (ex: ``DwellingManager``) are child classes of the ``InstanceManager`` class.



:raw-latex:`\pagebreak`

.. _`experiment manager`:

The Experiment Manager and its Block Managers
=============================================

This component (singleton instance of the ``ExperimentManager`` class), whose shorthand is ``EM``, is responsible for the **management of the dataflow(s)**, i.e. of the **computations** that are to be operated on the target system; this singleton federates all dataflow instances.

The experiment manager can be seen as an orthogonal counterpart of the `world manager`_. Having them separated allows to isolate and uncouple more easily the operations to be performed from the elements they are to operate on.

To each **type of block** involved in the dataflow (let's say for example that we have a ``EnergyDemand`` computation block that is to operate on a given building) corresponds a (singleton) instance of a **block manager** (for example the ``EnergyDemandManager`` will take care of all instances of the ``EnergyDemand`` block).

Each block manager federates all the instances of a given type of block so that it can perform, for computations and on behalf of the EM, exactly what instance managers perform for the simulated world on behalf of the WM.

Block managers may have to play a role in the course of the simulation and therefore must be properly synchronized; as such they are actors as well.

Block managers are federated indirectly, through dataflow instances, by the EM, which sits at the top of the computation hierarchy.

All actual block managers (ex: ``EnergyDemandManager``) are child classes of the ``BlockManager`` class.



:raw-latex:`\pagebreak`

Interactions Between Managers
=============================


The **instance managers** (in charge of the target system) communicate with the **block managers** (in charge of the computations) through respectively the ``WM`` and the ``EM``, notably **to account for the changes that happen in the simulated world and may impact the computations** (which tend to be more static by themselves, and are defined based on dataflow instances).

For example, should a new building instance be created on the target system, on the computation side an associated ``EnergyDemand`` block may have to be in turn created so that the building is taken into account operationally.

:raw-html:`<img src="">manager-interactions.png</img>`
:raw-latex:`\includegraphics[scale=0.28]{manager-interactions.png}`

For that, **various organizations can exist on both sides**; in the example above (left, orange, pane), three geographical scales have been defined to account for the simulated world: city, district and precinct.

Some urban objects may then be defined at their level (ex: buildings and dwellings pertain here to the precinct scale), and the scales may themselves have counterparts in terms of urban objects (ex: a district may exist as such in the simulation). The information path goes then through the red lines, possibly upward and downward.

For example, the creation of a building will propagate from the building manager to the precinct one, until climbing through coarser and coarser scales (district then city) and reaching finally the top-level (WM).

For other urban objects (ex: roads), perhaps that no specific scale applies, hence their instance manager may be directly connected to the WM.

Regarding computations this time (right, light blue pane), here two dataflow instances have been defined, to account for energy and for network. They will drive each their own blocks, yet a given type of block may be used in more than one dataflow (this is the case here of the energy production block type).

The notification that a building has been created would come from the WM and reach the EM. Both dataflow instances would become then aware of it, yet only the energy dataflow may be interested in that information, leading to the creation of a new energy demand block for that new building.

The net result is then that a change in the simulated world found the counterpart that was needed in terms of computations.

On a more technical side, the generic managers involved (ex: ``WM``, ``EM``) register themselves globally, to ease their integration. All case-specific managers (i.e. instance and block ones) shall follow the same convention.


:raw-latex:`\pagebreak`

About Mock-up Blocks
====================

*Mock-up blocks* are blocks that, instead of implementing a computation logic, **directly compose the state of their output ports based on the one of their input ports**, relying for that on static information (data more than code) that they embed.

As such, mock-up blocks merely *associate* pre-recorded outputs to inputs, instead of *implementing* computations allowing to determine the former from the latter ones.

These mock-up blocks can act as "termination plugs", i.e. placeholders inserted in a dataflow in order for example:

- to wait until the final, counterpart blocks are available
- to emulate the context of another block in order to better test it, or validate it
- to provide a simpler, less computation-demanding version of a block (see `model order reduction <https://en.wikipedia.org/wiki/Model_order_reduction>`_, called metamodel in some communities)

In practice, these mock-up blocks are instances of the ``DataflowMockupBlock`` class (or of a child class thereof), which provides a generic mock-up block able to be fed with data basically describing, in terms of ports, which outputs correspond to which inputs.



Definition of a Mock-up Block
-----------------------------

A mock-up block is specified exactly as any other dataflow block (ex: with a name, a temporality, a description of its input and output ports), yet it has to replace the inner computation logic that would be in a standard block by the (static) **association determining its outputs from its inputs**.

This association can be seen as a **simple function** which, based an (optional) given simulation time and an *input match specification* operating on its input ports, specifies in which state the output ports of this mock-up block shall be.

One should note that the result returned by this function (the actual state of the output ports) will depend *only* on its parameters (the specified time and the input match specs); for example no contextual data can intervene, and this (pure) function is stateless (i.e. it has no memory).

The general form of this mock-up function is::

  f( time(), input_match_spec() ) -> output_state()

This means that the purpose of this mock-up function is **to tell, for a given simulation time and for a configuration of the input ports that matches the supplied specification, what is the corresponding state of the output ports of that block**.

For that, multiple **clauses** can be specified in order to define that function: at runtime, when the engine transmits the actual current simulation time and the state of the input ports, these clauses are taken into account in turn, and the first one whose input specification matches the current state of the input ports will be executed. Examples in the next sections will clarify this mode of operation.

Let's call from now the mock-up function ``f/2``, i.e. a function named ``f`` taking two parameters, respectively the time and the input match spec.


Simulation Time Specification
-----------------------------

A mock-up function may **react differently at different time steps** of the simulation. So a clause of that function may specify, for its first parameter:

- either the precise time step at which it shall be applied, for example ``f(127,...)->...``
- or the ``any_time`` atom, to tell that the application of this clause does not depend on simulation time; as a result, ``f(any_time,...)->...`` will match irrespective of the current time step

Of course blocks (hence mock-up ones) may be atemporal, in which case only timeless clauses (using ``any_time``) would be used.


Input Match Specification
-------------------------

This second parameter of the mock-up function allows to specify the **configuration of input ports to which this clause is to apply**: the match specification describes the possible states in which the input ports of interest for the mock-up block may be for this clause to be selected, and thus to decide on the corresponding output state that shall be retained.

In practice, an input match spec is an (unordered) list of pairs, whose first element designates an input port, and whose second one specifies the associated state(s) that would match.

This first element is the name of the input port (ex: ``"I1"``), as in the context of a block it is a (unique) identifier.

The second element of the pair associated to a listed input port is among:

- the ``any_state`` atom, to specify that the state of this input port will be ignored, i.e. that it may or may not be set (if set, its value will not matter for the clause)
- the ``unset`` atom, to specify that this input port should not be set
- the ``set`` atom, telling that this input port may have any value, *provided it is set at all*
- a ``{set,V}`` pair, requiring that input port to be set exactly to this value ``V``
- a ``{set,{between,A,B}}`` pair, requiring that input port to be set to a scalar, numerical value in the ``[A,B]`` range (hence including bounds)
- a ``{set,{around,V,E}}`` pair, requiring that input port to be set to a value around ``V``, with a relative error [#]_ of up to ``E``; this is a way of better supporting floating-point values - for which strict equality is usually not meaningful
- a ``{set,{around,V}}`` pair, requiring that input port to be set to a value around ``V`` with a default relative error of ``1.0e-6``
- a ``{set,[V1,V2,..,Vn]}`` pair, this input port having to be set to one value in that list for the clause to possibly match

.. [#] The relative error between X and Y being the absolute value of their difference divided by their average value: ``2*abs((X-Y)/(X+Y)))``, for X different from -Y (otherwise ``abs(X-Y)`` is used instead).


Any input port that is not listed in the spec may be in any state (unset, or set to any value); the ``any_state`` atom is therefore a way of specifying the same, yet in an explicit manner.

For example, if a block has six input ports named ``"I1"``, ``"I2"``, ``"I3"``, ``"I4"``, ``"I5"`` and ``"I6"`` [#]_, and the input match specification is::

  [{"I2",{set,14.0}},{"I5",set},{"I4",{set,{between,{2,8}}}},
	{"I1",unset},{"I6",{set,[3,4,6]}}]

.. [#] Please note that the coupling layer allows port names to be any string; input ports do not have to be named ``"I1"``, ``"I2"``, etc.; therefore ``"attila woz here"`` and ``"FelixTheCat-1337"`` would be perfectly suitable (and of course the same applies to the names of output ports as well).


Then this function will match iff (if and only if), in terms of input ports for that mock-up block:

- ``"I1"`` is unset (i.e. not set to any value)
- and ``"I2"`` is (exactly) set to 14.00
- and ``"I4"`` is set to a value in [2,8]
- and ``"I5"`` is set (to any value)
- and ``"I6"`` is set to 3, 4 or 6

One can note that the order of the pairs does not matter, and that the input port ``"I3"``, not being listed, can thus be in any state.



Output State Specification
--------------------------

When a given clause is evaluated, if both its **current time and input specifications are matching the current runtime information**, then this clause is selected, and the **output ports of the mock-up block are then set as this clause specifies**.

An output state is defined as an (unordered) list of pairs, whose first element designates an output port (identified by its name), and whose second one specifies the associated state it should be set to.

Possible specified states are:

- the ``unset`` atom, so that the corresponding output port is (becomes or remains) not set
- a ``{set,V}`` pair, where ``V`` is the value to which this output port shall be set
- the ``const_state`` atom to leave this port in the same state, whatever it is
- a ``{state_of,I}`` pair, where ``I`` is the name of an input port of that block, in which case the state of the output port will be assigned to the one of the specified input port

If an output port is not listed in the specification, then it will keep in its current state (knowing that all output ports are initially, once created, ``unset``); the ``const_state`` atom is therefore a way of specifying the same, yet in an explicit manner.


For example, if the output state specification of a clause is::

  [{"O4",unset},{"O7",const_state},{"O1",{set,6}},{"O2",{set,3.14}},
   {"O5",{state_of,"I2"}}]

then, should this clause be selected, the output ports of this mock-up block will be assigned to following state:

- ``"O1"`` set to 6
- ``"O2"`` set to 3.14
- ``"O4"`` not set
- ``"O5"`` having the same state as input port ``"I2"``
- ``"O7"`` and all other output ports (if any) unchanged



Consistency and Checking
------------------------

Of course, both the input match specification and the output state specifications must respect the typing information (the ``T`` in ``SUTC``) of the ports that they may reference.

For example:

- ``{set,{between,1.1,1.7}}`` cannot apply to an input port typed as a boolean one
- ``{state_of,I}`` should not be specified if the corresponding output port has not the same associated type information as the input port ``I``

The dataflow infrastructure will perform some basic checking at runtime, yet provisions should be taken by the user to inject legit data.



Examples of a Mock-up Function
------------------------------

.. _`first mock-up function example`:

Now such a definition should be easy to interpret::

  f(0,[ {"ip_1",{set,true}},{"ip_3",{set,3}} ]) ->
	[ {"op_2",89}, {"op_3",false} ];
  f(0,[]) ->
	[ {"op_1",1}, {"op_2",1} ];
  f(1,[]) ->
	[ {"op_2",{state_of,"ip_7"}}];
  f(any_time,[]) ->
	[ {"op_3",unset} ].

Indeed that mock-up function ``f`` is defined thanks to four clauses, to be read as detailed below.

The **first clause** will require, if at time step ``0``, an exact match for both the input ports named ``"ip_1"`` (which must be set to true) and ``"ip_3"`` (which must be set to 3). If this occurs, then the evaluation of ``f`` is over and output ports ``"op_2"`` and ``"op_3"`` will be set respectively to ``89`` and ``false``, while the other ones will be unchanged.

Should this first clause not match, the **second one** will be tried. It references the same time step ``0``, yet has an empty input match spec. This means that, for that time step, it will be a "catch-all" clause, i.e. a clause that will match necessarily, regardless of the state of the input ports. In this case ``"op_1"`` and ``"op_2"`` will be set to ``1``, the state of the other output ports remaining as it is.

As the **third clause** deals with another time step than ``0``, it has a chance to match. We see that it behaves as a catch-all for time step ``1``, resulting in the ``"op_2"`` output port having the same state as the ``"ip_7"`` input port.

Finally, the **fourth clause** is an universal catch-all, for all time steps and all configurations of input ports. This implies that the corresponding mock-up block will be able to be evaluated in all possible cases; the role of this particular clause here is only to unset the  ``"op_3"`` output port.

Another example is a very simple one, the universally defined **identity mock-up function**, defined as::

  f(any_time,[]) ->
	[].



Data-Based Mock-up Definition
-----------------------------

A mock-up block can be seen more as data (output sets being matched to input ones) than as code.

Therefore, rather than *implementing* a mock-up function as done in the previous sections, a means should be provided  in order to define such a function based on an information stream (typically a file). Defining the syntax of these data is the purpose of our *Dataflow Block Mockup Format* (abbreviated as ``DBMF``), described here.

Fortunately, the corresponding data-based descriptions are directly similar to the implementations that were detailed above:

- a mock-up function was implemented as a series of clauses, its data counterpart is an (ordered) list of clause definitions
- each implemented clause corresponds then to an item of that list, i.e. a clause definition made of two elements:

  - the first element is a pair defining the time information and input match specification corresponding to this clause
  - the second element details the output state definition that will be applied, should the first element match

So the data-based version of the `first mock-up function example`_ may simply read as::

 [
  { {0,[{"ip_1",{set,true}},{"ip_3",{set,3}}]},
		[{"op_2",89},{"op_3",false}]},
  { {0,[]},
		[{"op_1",1},{"op_2",1}]},
  { {1,[]},
		[{"op_2",{state_of,"ip_7"}}],
  { {any_time,[]},
		[{"op_3",unset}]}
 ]

These two forms are basically the same.


Of course, in practice, the DBMF format is most probably too low-level, too textual for domain experts or model implementers to make a direct use of it.

Indeed blocks can easily have several dozens of input and output ports, and mock-up blocks may have to be defined over a very large number of time steps.

Two approaches could then allow to ultimately obtain the mock-up data in that targeted format:

- defining a lightweight GUI in order to ease (and check) the entering of these information, storing them in this mock-up format
- defining a spreadsheet-based template that would be filled by the persons closer to the models and then automatically translated into a proper mock-up data-based definition



Possible Enhancements
---------------------

- the patterns recognized could include the state of another input port (ex: ``{I4,I6}`` meaning that ``I4`` should be in the same state as ``I6``), of course provided that the dependency graph remains acyclic
- port names could also be pattern-matched; for example an input match spec could include ``{"ip_*",{set,3}}`` to specify that any input port whose name is prefixed with ``ip_`` and that is set to ``3`` would validate this part of the match
- during a time-step, a given block might be triggered any number of times (from none to more than once); even if introducing logical moments is probably not desirable (replacing timesteps with a pair made of a timestep and a logical moment, i.e. a diasca), maybe specifying whether a block is allowed to be triggered once (``once``) or any number of times (``always``) could be useful





:raw-latex:`\pagebreak`

Implementation Section
======================


Mode of Operation
-----------------

As already mentioned and represented in the class diagram below, **blocks** are implemented as instances of the ``DataflowBlock`` class, a child class of the ``DataflowElementActor`` one, itself a child class of the basic ``Actor`` one.

Most blocks are *passive* actors: they will be solely scheduled if/when some of their input ports are triggered, which, depending on their policy, might result in their activation. Some blocks (ex: source ones) may be *active*, in the sense that they may choose to develop a spontaneous behaviour (typically to auto-activate periodically).

A special case of block has been defined, the ``DataflowMockupBlock``. Such **mock-up block** provides a generic emulation of a block, associating output values to input ones based on the data (rules) it has been provided with. It is typically used to develop termination plugs that allow to wait until the implementation of the final block is ready.

**Ports** (input and output ones alike) are mere data-structures (records) held by their block (maintaining an associative table for both of them).

Implementation-wise, **channels** do not exist per se, they are abstracted out thanks to ports.

The static channel-related information (ex: ``SUTC``, for semantics, unit, type, constraints) is held by all endpoints (the output port and its linked input ports); as their metadata are checked for compliance when ports get connected, the exchanged values do not include them, since they are automatically checked in turn for compliance at port sending and receiving.

Dataflow **values** are records that store their accuracy, timestamp and, of course, actual value. As always, they are exchanged through actor messages, managed by the engine.

As for the **dataflow actors**, they unsurprisingly inherit from the ``DataflowActor`` class. These actors participate to the description of the simulated world and, thanks to their **dataflow attributes** (special attributes that map to a pair of input and output ports), they are ready to be integrated in a dataflow.

Finally, instances of dataflow actors of a given class (ex: ``Building``) are governed by an **instance manager** specific to this class (ex: ``BuildingManager``). All these managers inherit from the ``InstanceManager`` base, abstract class.

Similarly, all instances of a given type of block (ex: ``EnergyDemand``) are governed by a **block manager** specific to this class (ex: ``EnergyDemandManager``), and all these managers inherit from the ``BlockManager`` base, abstract class.

The overall **dataflow** could have remained implicit (in the sense that no specific instance could have been defined in order to designate it as such, a dataflow being just an abstract concept corresponding to a set of actual, interconnected blocks), yet having it in an explicit form (i.e. as an instance of a well-defined ``Dataflow`` class) has been deemed more convenient and future-proof.

So we end up with the following dataflow-related class hierarchy:

:raw-html:`<img src="">dataflow-class-hierarchy.png</img>`
:raw-latex:`\includegraphics[scale=0.384]{dataflow-class-hierarchy.png}`




Usage: Defining One's Dataflow
------------------------------

Preferably an overall schema of the dataflow is determined first. One must keep in mind that *a dataflow is a graph of computations*, i.e. a description of interlinked *tasks*, processed by blocks.

Then, for each type of block (as of course a dataflow may include multiple instances of the same block), a child class of ``DataflowBlock`` shall be defined.

The actual processing done by that kind of block is to be implemented by overriding its ``activate/1`` method. It will be based on the block state, including the value carried by its input ports that are set. A spontaneous behaviour might be defined as well, if appropriate for that block.

Once all block classes have been implemented, the target dataflow instance can be built. This is done by creating first the relevant block instances (either from a data stream listing their construction parameters, or programmatically), interconnecting their ports and registering themselves to their federating dataflow.

Then, once the simulation is started, some source blocks are expected to get activated (possibly thanks to active blocks, or to explicit block activation); their triggered ports shall in turn activate other blocks and trigger other ports, animating the whole dataflow so that it performs the processing that is expected from it.

Please refer to the ``Dataflow Urban Example``, whose sources are located in the ``mock-simulators/dataflow-urban-example`` directory, for a full source of inspiration.



Implementation Details
----------------------

The dataflow constructs are defined, in the code base, in the ``sim-diasca/src/core/src/dataflow`` source tree.

Traces sent by the dataflow architecture are available in the ``Core.Dataflow`` category and its children categories.

A dataflow initialization file has preferably for extension ``.init`` (ex: ``dataflow-urban-example.init``).

It generally lists the creations of:

- the relevant base components, i.e. the WM and its instance managers, the EM and its block managers
- at least one dataflow
- the relevant actual instances of dataflow actors and blocks



:raw-latex:`\pagebreak`

Design Questions
================

- Should all output ports of all blocks of the dataflow emit at each diasca a value, even if it did not change (event-based or synchronous)?; if yes, many useless sendings and schedulings, and not all blocks have the same temporality, so a year-based one, if included in a simulation with a daily one, would have to change its behaviour; so we preferred opting for an **event-based** mode of operation
- As not all state changes/operations are instantaneous, should **delays** (in diascas or ticks) induced by a block be managed? (ex: ignition spark received, explosion happening 4 milliseconds afterwards)
- Would it be possible and useful that blocks can be **composed**, i.e. that a given block can actually be recursively made of sub-blocks? If such a feature was wanted, then `ecore <http://www.eclipse.org/ecoretools/two approaches>`_ could be used to define the corresponding system, and two different approaches could be considered:

 - either defining, directly at this dataflow level, *nested blocks*, and manage the consequences thereof (ex: when the inputs of a "macro" block would change, its outputs would change in turn accordingly, yet only *after some delay in terms of logical moments* (i.e. only once some of them elapsed, with state transitions that may be arbitrarily deferred)
 - or introduce new, higher-level concepts, such as the one of *assemblies* that can be nested; an assembly would ultimately translate to a basic, non-nested dataflow, by collapsing a (multi-level, compounded, compact) assembly into a (single-layer, uniform, intricate) dataflow; for that assemblies (either user-defined or generated) would be recursively unboxed and expanded into their parts until only standard dataflow blocks are found (a bit like when going from a higher-level electronic schematics to its full, elementary counterpart at the level of the logic gates)
- Should large datastructures have to be carried by channels, maybe these data currently pushed by an output port shall be pulled by the input ports instead? (anyway this will most probably lead to a term duplication anyway)

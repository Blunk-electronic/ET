------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             RATSNEST                                     --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   to do:
--		- 
--		- 

with ada.text_io;					use ada.text_io;
with ada.containers; 				use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;

with et_pcb_coordinates_2;			use et_pcb_coordinates_2;
with et_conductor_segment.boards;	use et_conductor_segment.boards;

with et_vias;						use et_vias;
with et_pcb_stack;					use et_pcb_stack;


package et_ratsnest is

	use pac_geometry_brd;
	use pac_geometry_2;
	

	-- Airwires are machine made. For this reason
	-- they are lines whose start and end points are
	-- specified by floating point numbers:
	subtype type_airwire is type_line_fine;
	
	
	package pac_airwires is new doubly_linked_lists (type_airwire);
	use pac_airwires;


	function is_proposed (
		wire : in pac_airwires.cursor)
		return boolean;


	function is_selected (
		wire : in pac_airwires.cursor)
		return boolean;


	-- Returns the start and end points of an airwire
	-- as model coordinates (fixed point numbers).
	-- The reason is that for the operator these numbers
	-- with a sufficient accuracy are easier to understand:
	function to_string (
		wire : in pac_airwires.cursor)
		return string;

						   
	-- Returns from the list of airwires the shortes of them:
	function get_shortest_airwire (
		wires : in pac_airwires.list)
		return type_airwire;


	-- Already connected nodes form a fragment:
	type type_fragment is record
		nodes : pac_vectors.list;
	end record;

	-- Many fragments are collected in a simple list:
	package pac_isolated_fragments is new doubly_linked_lists (type_fragment);
	use pac_isolated_fragments;
	
	

	-- Returns the isolated fragments formed by the given conductor
	-- lines, arcs, vias and terminals of a net.
	-- NOTE: All conductor objects here belong to a single net.
	function get_fragments (
		lines	: in pac_conductor_lines.list;
		arcs	: in pac_conductor_arcs.list)
		return pac_isolated_fragments.list;

	

	
	-- Returns true if the container airwires contains the given
	-- candidate airwire:
	function contains_airwire (
		airwires	: in pac_airwires.list;
		airwire		: in type_airwire)
		return boolean;
	
	
	-- Iterates the given list of airwires. Aborts the process when the
	-- proceed-flag goes false:
	procedure iterate (
		airwires	: in pac_airwires.list;
		process		: not null access procedure (position : in pac_airwires.cursor);
		proceed		: not null access boolean);


	-- The airwires of a net are just a collection of lines.
	-- CS: In the future it might be interesting to hide the
	-- airwires of a net temporarily. This makes laying out tracks easier
	-- for the operator when too many airwires are confusing
	-- or obscuring the view. Currently the flag "hidden" is not
	-- changed by any process:
	type type_airwires is record
		lines	: pac_airwires.list;
		hidden	: boolean := false;
	end record;

	status_ratsnest_updated : constant string := "ratsnest updated";




	
	-- The neigboring node of a fragment:
	type type_neigbor is record
		-- the neigboring node itself:
		node		: type_vector; 

		-- the distance to the fragment:
		distance	: type_float_positive := 0.0; 

		-- the referencing node in the fragment:
		origin		: type_vector;
	end record;



	-- Returns the node (among isolated nodes) that is nearest to the given node.
	-- It does not probe the distance of the given node to itself (which would be zero).
	-- This function works according to principle 1 (P1) of the PRIM-algorithm.
	-- It searches in the list of isolated nodes: 
	function get_nearest_neighbor_of_node (
		isolated_nodes	: in pac_vectors.list;
		node_in 		: in type_vector)
		return type_vector;
	

	-- Returns the shortest distance of a node 
	-- to the given fragment:
	function get_distance_to_fragment (
		fragment_cursor	: in pac_isolated_fragments.cursor;
		node			: in type_vector)
		return type_float_positive;


	
	-- Nodes and their distances to a fragment:
	package pac_distances_table is new ordered_maps (
		key_type		=> type_vector,
		element_type	=> type_float_positive);

	-- Returns the distances of isolated nodes to a fragment:
	function get_distances_to_isoldated_nodes (
		fragment_cursor	: in pac_isolated_fragments.cursor;
		isolated_nodes	: in pac_vectors.list)
		return pac_distances_table.map;


	
	
	-- Returns the isolated node that is nearest to the given fragment:
	-- This function works according to principle 2 (P2) of the PRIM-algorithm.
	-- The general workflow is as follows:
	-- 1. For each node of the given fragment search the nearest neigbor.
	-- 2. Store the neigbor in an array.
	-- 3. Find in the array the neigbor that is closest to the fragment.
	-- 4. Return that neigbor to the caller.
	function get_nearest_neighbor_of_fragment (
		fragment_cursor : in pac_isolated_fragments.cursor;
		isolated_nodes	: in pac_vectors.list)
		return type_neigbor;


	
	
	type type_nearest_fragment is record
		neigbor		: type_neigbor;
		fragment	: pac_isolated_fragments.cursor;
	end record;

	-- Returns the isolated fragment and its node that is
	-- nearest to the given isolated fragment:
	function get_nearest_fragment (
		fragments	: in pac_isolated_fragments.list;   -- the fragments to be searched in
		reference	: in pac_isolated_fragments.cursor) -- the candidate fragment
		return type_nearest_fragment;


	-- Constructs from a list of isolated nodes and isolated fragments
	-- a list of airwires. 
	-- IMPORTANT: Initially ALL the given nodes are regarded as "isolated" from
	-- each other, regardless whether they are already connected with each other or not.
	-- Argument "strands" contains nodes of already connected nodes.
	--  The airwires are returned as a Shortest-Connection-Network (SCN)
	-- or a spanning-subgraph. The algorithm used here bases partly on the concept 
	-- written article: "Shortest Connection Networks And Some Generalizations" 
	-- by R.C.PRIM, date 1957-05-08. The algorithm has been extended here so that 
	-- already present isolated fragments are taken into account:
	function make_airwires (
		nodes	: in pac_vectors.list;	-- ALL nodes (routed and unrouted stuff)
		strands	: in pac_isolated_fragments.list) -- already routed stuff
		return pac_airwires.list;



end et_ratsnest;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

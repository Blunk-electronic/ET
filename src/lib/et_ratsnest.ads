------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             RATSNEST                                     --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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

with et_pcb_coordinates;			use et_pcb_coordinates;
with et_board_shapes_and_text;		use et_board_shapes_and_text;
with et_conductor_segment.boards;	use et_conductor_segment.boards;

with et_vias;						use et_vias;
with et_pcb_stack;					use et_pcb_stack;


package et_ratsnest is

	use pac_geometry_brd;
	use pac_geometry_2;
	

	type type_airwire is new type_line_fine;
	
	package pac_airwires is new doubly_linked_lists (type_airwire);
	use pac_airwires;


	-- Returns from the list of airwires the shortes of them:
	function get_shortest_airwire (
		wires : in pac_airwires.list)
		return type_airwire;

	
	type type_strand is record -- CS rename to type_fragment
		nodes : pac_vectors.list;
	end record;

	package pac_strands is new doubly_linked_lists (type_strand); -- CS rename to pac_fragments
	use pac_strands;
	
	
	
	
	function to_airwire (
		line : in type_conductor_line)
		return type_airwire;


	
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

	
	type type_airwires is record
		lines	: pac_airwires.list;
		hidden	: boolean := false;
	end record;

	status_ratsnest_updated : constant string := "ratsnest updated";

	-- GUI relevant only:
	airwire_line_width : constant type_float_positive := 0.01;


	-- Constructs from a list of nodes a list of airwires.
	-- The airwires are returned as a Shortest-Connection-Network (SCN)
	-- or a spanning-subgraph. 
	--  The given list of nodes must contain at least 2 nodes. Otherwise
	-- no airwire will be returned.
	--  CS: For the moment we assume that no node sits on top of another node,
	-- means each node has a unique x/y position.
	--  The algorithm used here bases on the article:
	-- "Shortest Connection Networks And Some Generalizations"
	-- written by R.C.PRIM, date 1957-05-08
	function make_airwires (
		nodes	: in pac_vectors.list;
		strands	: in pac_strands.list)
		return pac_airwires.list;

	
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
	
	
	-- Returns the isolated node that is nearest to the given fragment:
	-- This function works according to principle 2 (P2) of the PRIM-algorithm.
	-- The general workflow is as follows:
	-- 1. For each node of the given fragment search the nearest neigbor.
	-- 2. Store the neigbor in an array.
	-- 3. Find in the array the neigbor that is closest to the fragment.
	-- 4. Return that neigbor to the caller.
	function get_nearest_neighbor_of_fragment (
		fragment_cursor : in pac_strands.cursor;
		isolated_nodes	: in pac_vectors.list)
		return type_neigbor;

	
	type type_nearest_fragment is record
		neigbor		: type_neigbor;
		fragment	: pac_strands.cursor;
	end record;

	-- Returns the isolated fragment and its node that is
	-- nearest to the given isolated fragment:
	function get_nearest_fragment (
		fragments	: in pac_strands.list;   -- the fragments to be searched in
		reference	: in pac_strands.cursor) -- the candidate fragment
		return type_nearest_fragment;

	
	function make_airwires_2 (
		nodes	: in pac_vectors.list;
		strands	: in pac_strands.list)
		return pac_airwires.list;


	
	-- Returns the start and end points (nodes) of the lines which are
	-- connected with each other. Deletes the affected lines
	-- from the given list.
	-- If no lines given, then an empty list is returned.
	function get_connected_nodes (
		lines	: in out pac_conductor_lines.list)
		return pac_vectors.list;


	-- Returns the nodes of strands formed by the given
	-- lines, arcs, vias and terminals:
	function get_strands (
		lines		: in pac_conductor_lines.list;
		arcs		: in pac_conductor_arcs.list;
		vias		: in pac_vias.list;
		terminals	: in pac_vectors.list; -- THT terminals !
		deepest		: in type_signal_layer)
		return pac_strands.list;


	-- Returns true if the given airwire connects
	-- two nodes which are inside of any strand:
	function airwire_in_strand (
		wire	: in type_airwire;
		strands	: in pac_strands.list)
		return boolean;


	-- Returns true if the given airwire is connected with
	-- only one node of the given strand:
	function airwire_enters_strand (
		wire			: in type_airwire;
		strand_cursor	: in pac_strands.cursor)
		return boolean;
	
		
	
	-- Removes airwires which are no longer required due to
	-- already routed stuff (given in argument "strands");
	procedure post_process_airwires (
		airwires	: in out pac_airwires.list;
		strands		: in pac_strands.list);

	
end et_ratsnest;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             RATSNEST                                     --
--                                                                          --
--                              B o d y                                     --
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



package body et_ratsnest is


	function get_shortest_airwire (
		wires : in pac_airwires.list)
		return type_airwire
	is
		result : type_airwire;
		shortest : type_float_positive := type_float_positive'last;

		procedure query_wire (c : in pac_airwires.cursor) is
			candidate_length : type_float_positive := get_length (element (c));
		begin
			if candidate_length < shortest then
				shortest := candidate_length;
			end if;
		end query_wire;
		
	begin
		wires.iterate (query_wire'access);
		return result;
	end get_shortest_airwire;

	
	function to_airwire (
		line : in type_conductor_line)
		return type_airwire
	is begin
		return type_airwire (to_line_fine (line));
	end to_airwire;

	
	function contains_airwire (
		airwires	: in pac_airwires.list;
		airwire		: in type_airwire)
		return boolean
	is
		result : boolean := false;
	begin
		if airwires.contains (airwire) then
			result := true;

		-- The airwire could be reversed in the container
		-- (means start and end point swapped):
		elsif airwires.contains (reverse_line (airwire)) then
			result := true;

		else
			result := false;
		end if;

		return result;
	end contains_airwire;
	
	


	
	procedure iterate (
		airwires	: in pac_airwires.list;
		process		: not null access procedure (position : in pac_airwires.cursor);
		proceed		: not null access boolean)
	is
		use pac_airwires;
		c : pac_airwires.cursor := airwires.first;
	begin
		while c /= pac_airwires.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;


	
	
	function make_airwires (
		nodes : in pac_vectors.list)
		return pac_airwires.list
	is		
		use pac_airwires;
		result : pac_airwires.list := pac_airwires.empty_list; -- to be returned
		
		use pac_vectors;
		start : type_vector;

		-- This is the list of unconnected nodes. It will become
		-- shorter and shorter over time until it is empty. As soon as
		-- it is empty, the PRIM-algorithm ends:
		nodes_isolated : pac_vectors.list := nodes;

		-- These are the nodes of the spann-graph that we are going to build.
		-- Once a node gets linked (with an airwire), the node
		-- will be added to nodes_linked. So this list of nodes grows
		-- over time until all given nodes have been added to the graph.
		-- Initially it is empty:
		nodes_linked : pac_vectors.list := pac_vectors.empty_list;
		

		-- Moves the given node from the list of isolated nodes to
		-- the list of linked nodes. So the list nodes_isolated gets
		-- shorter by one node. The list nodes_linked gets longer by one node:
		procedure move_to_linked_nodes (node : in type_vector) is
			nc : pac_vectors.cursor;
		begin
			-- remove from isolated nodes:
			nc := nodes_isolated.find (node);
			nodes_isolated.delete (nc);

			-- add to linked nodes:
			nodes_linked.append (node);
		end move_to_linked_nodes;


		-- Appends the given airwire to the result:
		procedure add_airwire (aw : in type_airwire) is begin
			-- CS make sure length is greater zero ? Since we assume unique positions
			-- of the given nodes, this check should not be required.

			result.append (aw);
		end add_airwire;
		
		
		-- Returns the node that is nearest to the given node.
		-- It does not probe the distance of the given node to itself (which would be zero).
		-- This function works according to principle 1 (P1) of the PRIM-algorithm.
		-- It searches in the list of isolated nodes: 
		function get_nearest_neighbor_of_node (node_in : in type_vector)
			return type_vector
		is
			smallest_distance : type_float_positive := type_float_positive'last;
			node_nearest : type_vector; -- to be returned
			
			procedure query_node (c : in pac_vectors.cursor) is
				d_tmp : type_float_positive;
			begin
				-- ignore the given node. For others nodes: get the distance
				-- from node_in to the candidate node:
				if element (c) /= node_in then
					d_tmp := get_absolute (get_distance (element (c), node_in));

					-- Update the smallest distance and register the
					-- candidate node if current distance is smaller than
					-- the previous distance between the nodes:
					if d_tmp < smallest_distance then
						smallest_distance := d_tmp;
						node_nearest := element (c);
					end if;
				end if;
			end query_node;
			
		begin
			-- probe all nodes (except the given node):
			nodes_isolated.iterate (query_node'access);

			-- return the nearest node that has been found:
			return node_nearest;
		end get_nearest_neighbor_of_node;
		

		-- Returns the node that is nearest to the current graph nodes_linked:
		-- This function works according to principle 2 (P2) of the PRIM-algorithm.
		-- The general workflow is as follows:
		-- 1. For each node of the current graph search the nearest neigbor.
		-- 2. Store the neigbor in an array.
		-- 3. Find in the array the neigbor that is closest to the graph.
		-- 4. Return that neigbor to the caller.
		function get_nearest_neighbor_of_graph
			return type_vector
		is
			node_nearest : type_vector; -- to be returned

			-- The total number of nodes in the current graph. It is required
			-- in order to set up the array, because for each node of the graph
			-- there will be a nearest neigboring node:
			linked_total : constant count_type := nodes_linked.length;

			
			-- The neigboring node to be stored in the array:
			type type_neigbor is record
				-- the neigboring node itself:
				node		: type_vector; 

				-- the distance to the graph:
				distance	: type_float_positive := 0.0; 

				-- the referencing node in the graph:
				origin		: type_vector;
			end record;

			
			-- Set up the array of neigboring nodes:
			type type_neigbors is array (1 .. linked_total) of type_neigbor;
			neigbors : type_neigbors := (others => <>);

			-- Set up a pointer to the elements of the array:
			subtype type_neigbor_pointer is count_type range 1 .. linked_total + 1;
			pointer : type_neigbor_pointer := 1;

			
			-- Finds the nearest isolated neigbor of a linked node:
			procedure query_node_linked (c : in pac_vectors.cursor) is
				neigbor : type_neigbor;
			begin
				neigbor.node := get_nearest_neighbor_of_node (element (c));
				neigbor.distance := get_absolute (get_distance (element (c), neigbor.node));
				neigbor.origin := element (c);

				-- store neigbor in array:
				neigbors (pointer) := neigbor;

				-- prepare for next node:
				pointer := pointer + 1; 
			end query_node_linked;


			-- Searches in array "neigbors" for the neigbor that has the smallest
			-- distance to the graph.
			-- Generates an airwire to that neigbor.
			procedure find_nearest_among_neigbors is
				smallest_distance : type_float_positive := type_float_positive'last;
				aw_tmp : type_airwire;
			begin
				for i in neigbors'first .. neigbors'last loop

					-- Update the node_nearest if current neigbor is
					-- closer than the previous distance to the graph.
					if neigbors (i).distance < smallest_distance then
						smallest_distance := neigbors (i).distance;
						node_nearest := neigbors (i).node;

						-- make an airwire from the node of the grap to
						-- the neigboring node. If a closer neigbor has been
						-- found, then aw_tmp will be overwritten accordingly:
						--aw_tmp := type_line (make_line (neigbors (i).origin, neigbors (i).node));
						aw_tmp := make_line (neigbors (i).origin, neigbors (i).node);
					end if;
				end loop;

				-- Append the latest generated airwire to the result:
				add_airwire (aw_tmp);
			end find_nearest_among_neigbors;
			
			
		begin
			-- Probe the nodes of the current graph:
			nodes_linked.iterate (query_node_linked'access);

			find_nearest_among_neigbors;
			
			return node_nearest;
		end get_nearest_neighbor_of_graph;


		node_tmp : type_vector;

		
	begin -- make_airwires
		
		-- If there are at least two nodes then start constructing the SCN.
		-- Otherwise return an empty list of airwires.
		if nodes.length >= 2 then

			-- Set the start point of the SCN:
			start := first_element (nodes_isolated);

			-- Apply P1:
			node_tmp := get_nearest_neighbor_of_node (start);

			-- create the first fragment of the SCN:
			move_to_linked_nodes (start);
			move_to_linked_nodes (node_tmp);

			-- The list nodes_isolated has been shortened by two nodes.
			-- The graph nodes_linked now contains two nodes.

			-- create the first airwire
			add_airwire ((start, node_tmp));

			
			-- As long as there are any isolated nodes, apply P2.
			-- Each time P2 is applied
			-- - nodes_isolated becomes shorter by on node.
			-- - nodes_linked becomes longer by one node.
			while not nodes_isolated.is_empty loop
				-- Apply P2:
				node_tmp := get_nearest_neighbor_of_graph;
				move_to_linked_nodes (node_tmp);
			end loop;

		end if;

		-- Return the connection of airwires:
		return result;
	end make_airwires;
	



	function are_connected (
		line_1, line_2 : in type_conductor_line)
		return boolean
	is
		result : boolean := false;
	begin
		if line_1.start_point = line_2.start_point
		or line_1.start_point = line_2.end_point
		or line_1.end_point   = line_2.start_point
		or line_1.end_point   = line_2.end_point
		then
			result := true;
		else
			result := false;
		end if;
		
		return result;
	end are_connected;


	function is_connected (
		line	: in type_conductor_line;
		lines	: in pac_conductor_lines.list)
		return boolean
	is
		result : boolean := false;

		use pac_conductor_lines;

		procedure query_line (c : in pac_conductor_lines.cursor) is begin
			if are_connected (element (c), line) then
				result := true;
				-- CS abort iterator
			end if;
		end query_line;
		
	begin
		lines.iterate (query_line'access);
		return result;
	end is_connected;

	
		
	function get_connected_nodes (
		lines	: in out pac_conductor_lines.list)
		return pac_vectors.list
	is
		use pac_conductor_lines;
		use pac_vectors;
		nodes : pac_vectors.list;

		procedure to_nodes (l : type_conductor_line) is 
			lf : type_line_fine;
		begin
			lf := to_line_fine (l);
			nodes.append (lf.start_point);
			nodes.append (lf.end_point);
		end to_nodes;
		

		count : constant count_type := lines.length;
		

		collector : pac_conductor_lines.list;

		
		procedure search is
			found : boolean := false;
			
			procedure query_line (l : in type_conductor_line) is begin
				if is_connected (l, collector) then
					found := true;
					collector.append (l);
					to_nodes (l);
				end if;
			end query_line;
			
			lc : pac_conductor_lines.cursor;
			
		begin
			collector.append (lines.first_element);
			lines.delete_first;
			to_nodes (collector.first_element);

			lc := lines.first;
			while lc /= pac_conductor_lines.no_element loop

				query_element (lc, query_line'access);

				if found then 
					lines.delete (lc);
					lc := lines.first;
					found := false;
				else
					next (lc);					
				end if;
			end loop;
		end search;

		
	begin
		-- put_line ("get connected nodes from lines " & count_type'image (count));
		
		case count is
			when 0 => null;

			when 1 =>
				to_nodes (lines.first_element);
				lines.clear;

			when others =>
				search;
		end case;

		remove_redundant_vectors (nodes);

		return nodes;
	end get_connected_nodes;

	
		
	function get_strands (
		lines		: in pac_conductor_lines.list;
		arcs		: in pac_conductor_arcs.list;
		vias		: in pac_vias.list;
		terminals	: in pac_vectors.list;
		deepest		: in type_signal_layer)
		return pac_strands.list
	is
		result : pac_strands.list;

		line_count : constant count_type := lines.length;
		arc_count  : constant count_type := arcs.length;

		type type_objects is record
			lines	: pac_conductor_lines.list;
			arcs	: pac_conductor_arcs.list;
		end record;

		type type_layer is array (type_signal_layer'first .. deepest) 
			of type_objects;

		layers : type_layer;
			
		lines_in_layer : pac_conductor_lines.list;
	begin
		-- put_line ("get strands...");

		-- Separate the given lines and arcs by their signal layer:
		for ly in layers'first .. deepest loop
			layers (ly).lines := get_lines_by_layer (lines, ly);
			layers (ly).arcs  := get_arcs_by_layer  (arcs, ly);
		end loop;


		for ly in layers'first .. deepest loop
			while not layers (ly).lines.is_empty loop
				result.append ((nodes => get_connected_nodes (layers (ly).lines)));
			end loop;
		end loop;

		-- CS vias, tht terminals, arcs
		
		return result;
	end get_strands;
	

	function airwire_in_strand (
		wire	: in type_airwire;
		strands	: in pac_strands.list)
		return boolean
	is
		result : boolean := false;

		procedure query_strand (c : in pac_strands.cursor) is
			strand : type_strand renames element (c);
		begin
			if strand.nodes.contains (wire.start_point) 
			and strand.nodes.contains (wire.end_point) then
				result := true; 
				-- CS abort strand iterator
			end if;
		end query_strand;
		
	begin
		strands.iterate (query_strand'access);
		return result;
	end airwire_in_strand;


	function airwire_enters_strand (
		wire			: in type_airwire;
		strand_cursor	: in pac_strands.cursor)
		return boolean
	is
		strand : type_strand renames element (strand_cursor);
		
		result : boolean := false;

		-- procedure query_node (c : in pac_vectors.cursor) is begin
		-- 	if strand.nodes.contains (wire.start_point)
		-- 	xor strand.nodes.contains (wire.end_point) then
		-- 		result := true; 
		-- 		-- CS abort node iterator
		-- 	end if;
		-- end query_node;
		
	begin
		-- strand.nodes.iterate (query_node'access);

		if  strand.nodes.contains (wire.start_point)
		xor strand.nodes.contains (wire.end_point) then
			result := true; 
		end if;
		
		return result;
	end airwire_enters_strand;

	
	procedure post_process_airwires (
		airwires	: in out pac_airwires.list;
		strands		: in pac_strands.list)
	is
		airwires_new : pac_airwires.list;

		procedure query_airwire (c : in pac_airwires.cursor) is
			aw : type_airwire renames element (c);
		begin
			if not airwire_in_strand (aw, strands) then
				airwires_new.append (aw);
			end if;
		end query_airwire;


		procedure query_strand (c : in pac_strands.cursor) is
			strand : type_strand renames element (c);

			aws_entering : pac_airwires.list;
			shortest_airwire : type_airwire;

			procedure query_airwire (a : in pac_airwires.cursor) is
				wire : type_airwire renames element (a);
			begin
				if airwire_enters_strand (wire, c) then
					aws_entering.append (wire);
				end if;
			end query_airwire;
			
		begin
			-- Collect airwires that are entering the strand:
			airwires_new.iterate (query_airwire'access);

			shortest_airwire := get_shortest_airwire (aws_entering);
		end query_strand;

		
	begin
		put_line ("post processing ...");
		
		-- Remove those airwires which connect nodes inside strands:
		airwires.iterate (query_airwire'access);

		airwires := airwires_new;

		strands.iterate (query_strand'access);
	end post_process_airwires;

	

end et_ratsnest;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

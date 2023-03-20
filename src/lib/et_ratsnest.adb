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

with ada.containers.multiway_trees;

package body et_ratsnest is

	use pac_vectors;
	

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

	
		
	function get_fragments (
		lines		: in pac_conductor_lines.list;
		arcs		: in pac_conductor_arcs.list;
		vias		: in pac_vias.list;
		terminals	: in pac_vectors.list;
		deepest		: in type_signal_layer)
		return pac_isolated_fragments.list
	is
		result : pac_isolated_fragments.list;

		line_count : constant count_type := lines.length;
		arc_count  : constant count_type := arcs.length;

		type type_tracks_in_layer is record
			lines	: pac_conductor_lines.list;
			arcs	: pac_conductor_arcs.list;
		end record;

		type type_tracks_in_layers is array (type_signal_layer'first .. deepest) 
			of type_tracks_in_layer;

		tracks_in_layers : type_tracks_in_layers;
			


		type type_fragments_per_layer is array (type_signal_layer'first .. deepest)
			of pac_isolated_fragments.list;

		fragments_per_layer : type_fragments_per_layer;

		-- package pac_fragments is new multiway_trees (type_fragment);
		-- strand : pac_fragments.tree;
		
	begin
		-- put_line ("get strands...");

		-- put_line (" lines total" & count_type'image (lines.length));
		
		-- Separate the given lines and arcs by their signal layer:
		for ly in tracks_in_layers'first .. deepest loop
			-- put_line (" layer" & type_signal_layer'image (ly));
			tracks_in_layers (ly).lines := get_lines_by_layer (lines, ly);
			-- put_line ("  lines" & count_type'image (layers (ly).lines.length));
			tracks_in_layers (ly).arcs  := get_arcs_by_layer  (arcs, ly);
		end loop;


		for ly in tracks_in_layers'first .. deepest loop
			while not tracks_in_layers (ly).lines.is_empty loop
				result.append ((nodes => get_connected_nodes (tracks_in_layers (ly).lines)));

				-- fragments_per_layer (ly).append ((nodes => get_connected_nodes (tracks_in_layers (ly).lines)));
			end loop;
		end loop;

		-- CS vias, tht terminals, arcs
		
		
		return result;
	end get_fragments;


	function get_fragments_2 (
		lines		: in pac_conductor_lines.list;
		arcs		: in pac_conductor_arcs.list;
		vias		: in pac_vias.list;
		terminals	: in pac_vectors.list;
		deepest		: in type_signal_layer)
		return pac_isolated_fragments.list
	is
		result : pac_isolated_fragments.list;

		use pac_conductor_lines;
		
		lines_tmp		: pac_conductor_lines.list := lines;
		-- CS arcs
		vias_tmp		: pac_vias.list := vias;
		terminals_tmp	: pac_vectors.list := terminals;

		line_cursor : pac_conductor_lines.cursor;
		
		type type_strand is record
			lines		: pac_conductor_lines.list;
			-- CS arcs
			vias		: pac_vias.list;
			terminals	: pac_vectors.list;
		end record;

		strand : type_strand;


							 
		
		function is_connected_with_strand (
			line_in	: in type_conductor_line)
			return boolean
		is
			result : boolean := false;

			use pac_conductor_lines;

			procedure query_line (c : in pac_conductor_lines.cursor) is 
				candidate : type_conductor_line renames element (c);
			begin
				-- if line_in.layer = candidate.layer then
					if are_connected (line_in, candidate) then
						result := true;
						-- CS abort iterator
					end if;
				-- end if;
			end query_line;
			
		begin
			strand.lines.iterate (query_line'access);
			return result;
		end is_connected_with_strand;

		

		function get_nodes (strand : in type_strand)
			return pac_vectors.list
		is 
			result : pac_vectors.list;
			
			procedure query_line (c : in pac_conductor_lines.cursor) is begin
				result.append (to_vector (element (c).start_point));
				result.append (to_vector (element (c).end_point));
			end query_line;
				
		begin
			strand.lines.iterate (query_line'access);
			remove_redundant_vectors (result);
			return result;
		end get_nodes;
		

		
	begin
		-- put_line ("get fragments...");

		while not lines_tmp.is_empty loop
			strand.lines.append (lines_tmp.first_element);
			lines_tmp.delete_first;

			if lines_tmp.is_empty then
				result.append ((nodes => get_nodes (strand)));
			else

				-- Iterate the remaining conductor lines:
				line_cursor := lines_tmp.first;
				while line_cursor /= pac_conductor_lines.no_element loop
					-- If the candidate line is connected with the strand,
					-- then merge the candidate line into the strand and
					-- remove it from the remaining conductor lines (in lines_tmp).
					-- Then abort the iteration:
					if is_connected_with_strand (element (line_cursor)) then
						strand.lines.append (element (line_cursor));
						lines_tmp.delete (line_cursor);
						if lines_tmp.is_empty then -- nothing left
							exit;
						else
							line_cursor := lines_tmp.first;
						end if;

					-- If the candidate line is not connected with the strand,
					-- then advance to the next line (in lines_tmp):
					else						
						next (line_cursor);						
					end if;						
				end loop;

				result.append ((nodes => get_nodes (strand)));
				strand.lines.clear;

			end if;
		end loop;
		
		
		return result;
	end get_fragments_2;

	
	
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


	
	function get_nearest_neighbor_of_node (
		isolated_nodes	: in pac_vectors.list;
		node_in			: in type_vector)
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
		isolated_nodes.iterate (query_node'access);

		-- return the nearest node that has been found:
		return node_nearest;
	end get_nearest_neighbor_of_node;

	

	function get_distance_to_fragment (
		fragment_cursor	: in pac_isolated_fragments.cursor;
		node			: in type_vector)
		return type_float_positive
	is	
		fragment : type_fragment renames element (fragment_cursor);
		
		result : type_float_positive := type_float_positive'last;

		procedure query_node (c : in pac_vectors.cursor) is
			distance : type_float_positive := get_distance_total (element (c), node);
		begin
			if distance < result then
				result := distance;
			end if;
		end query_node;
		
	begin
		fragment.nodes.iterate (query_node'access);		
		return result;
	end get_distance_to_fragment;

	
	function get_distances_to_isoldated_nodes (
		fragment_cursor	: in pac_isolated_fragments.cursor;
		isolated_nodes	: in pac_vectors.list)
		return pac_distances_table.map
	is
		result : pac_distances_table.map;

		procedure query_node (c : in pac_vectors.cursor) is 
			node : type_vector renames element (c);
			position_in_table : pac_distances_table.cursor;
			inserted : boolean := true;
		begin
			result.insert (
				key			=> node,
				new_item	=> get_distance_to_fragment (fragment_cursor, node),
				position	=> position_in_table,
				inserted	=> inserted);

		end query_node;
		
	begin
		isolated_nodes.iterate (query_node'access);
		return result;
	end get_distances_to_isoldated_nodes;


	
	function get_nearest_neighbor_of_fragment (
		fragment_cursor : in pac_isolated_fragments.cursor;
		isolated_nodes	: in pac_vectors.list)
		return type_neigbor
	is
		fragment : type_fragment renames element (fragment_cursor);

		result : type_neigbor; -- to be returned

		-- The total number of nodes in the current fragment. It is required
		-- in order to set up the array, because for each node of the fragment
		-- there will be a nearest neigboring node:
		linked_total : constant count_type := fragment.nodes.length;


		
		-- Set up the array of neigboring nodes:
		type type_neigbors is array (1 .. linked_total) of type_neigbor;
		neigbors : type_neigbors := (others => <>);

		-- Set up a pointer to the elements of the array:
		subtype type_neigbor_pointer is count_type range 1 .. linked_total + 1;
		pointer : type_neigbor_pointer := 1;


		-- Finds the nearest isolated neigbor of a linked node:
		procedure query_node_of_fragment (c : in pac_vectors.cursor) is
			node	: type_vector renames element (c);
			neigbor	: type_neigbor;
		begin
			neigbor.node := get_nearest_neighbor_of_node (isolated_nodes, node);
			neigbor.distance := get_absolute (get_distance (node, neigbor.node));
			neigbor.origin := node;

			-- store neigbor in array:
			neigbors (pointer) := neigbor;

			-- prepare for next node:
			pointer := pointer + 1; 
		end query_node_of_fragment;


		-- Searches in array "neigbors" for the neigbor that has the smallest
		-- distance to the fragment.
		-- Generates an airwire to that neigbor.
		procedure find_nearest_among_neigbors is
			smallest_distance : type_float_positive := type_float_positive'last;
		begin
			for i in neigbors'first .. neigbors'last loop

				-- Update the result if current neigbor is
				-- closer than the previous distance to the fragment.
				if neigbors (i).distance < smallest_distance then
					smallest_distance := neigbors (i).distance;
					
					result := neigbors (i);
				end if;
			end loop;
		end find_nearest_among_neigbors;
		
		
	begin
		-- Probe the nodes of the given fragment:
		fragment.nodes.iterate (query_node_of_fragment'access);

		find_nearest_among_neigbors;
		
		return result;
	end get_nearest_neighbor_of_fragment;


	
	function get_nearest_fragment (
		fragments	: in pac_isolated_fragments.list;
		reference	: in pac_isolated_fragments.cursor)
		return type_nearest_fragment
	is
		result : type_nearest_fragment;

		type type_neigbor_fragments is array (1 .. fragments.length - 1) of type_nearest_fragment;
		neigbor_fragments : type_neigbor_fragments;
		
		ct : count_type := 0;
		
		procedure query_fragment (c : in pac_isolated_fragments.cursor) is
			fragment : type_fragment renames element (c); -- the candidate fragment
			neigbor : type_neigbor;
		begin
			if fragment /= element (reference) then
			-- CS if c /= reference then
				ct := ct + 1;
				-- We regard the nodes of the candidate fragment as if they where 
				-- isolated nodes:
				neigbor := get_nearest_neighbor_of_fragment (reference, fragment.nodes);
				neigbor_fragments (ct) := (neigbor, c);
			end if;
		end query_fragment;

		smallest_distance : type_float_positive := type_float_positive'last;
		
	begin
		-- Collect the nearest fragments in array neigbor_fragments:
		fragments.iterate (query_fragment'access);

		-- Find in array neigbor_fragments the one with the smallest distance
		-- to the given reference fragment:
		for i in neigbor_fragments'first .. neigbor_fragments'last loop
			if neigbor_fragments (i).neigbor.distance < smallest_distance then
				smallest_distance := neigbor_fragments (i).neigbor.distance;
				result := neigbor_fragments (i);
			end if;
		end loop;	
		
		return result;
	end get_nearest_fragment;

	

	function make_airwires (
		nodes	: in pac_vectors.list;
		strands	: in pac_isolated_fragments.list)
		return pac_airwires.list
	is		
		use pac_airwires;
		result : pac_airwires.list := pac_airwires.empty_list; -- to be returned
		

		-- This is the list of unconnected nodes. It will become
		-- shorter and shorter over time until it is empty. As soon as
		-- it is empty, the PRIM-algorithm ends:
		isolated_nodes : pac_vectors.list := nodes;

		-- For each given strand an initial "isolated fragment" must be built.
		-- These are the nodes of isolated fragments of the spann-graph that we are going to build.
		-- Once a node gets linked (with an airwire) with an isolated fragment, then node
		-- will be added to the affected fragment in nodes_linked. So the particual list of nodes grows
		-- over time until all given nodes have been added to the graph.
		-- Initially it is empty:
		isolated_fragments : pac_isolated_fragments.list;


		---------------------------------------------------------------------------------------------
		-- Moves the given node from the list of isolated nodes to
		-- the given list of linked nodes. So the list isolated_nodes gets
		-- shorter by one node. The given list "linked" gets longer by one node:
		procedure move_to_linked_nodes (
			linked	: in out pac_vectors.list;
			node	: in type_vector)
		is
			nc : pac_vectors.cursor;
		begin
			-- Find the given node in isolated_nodes:
			nc := isolated_nodes.find (node);

			-- If the node has already been removed from
			-- isolated nodes, then there is nothing to do.
			-- This results from isolated nodes sitting on top of each other.
			if nc /= pac_vectors.no_element then

				-- Remove the node from list isolated_nodes:
				isolated_nodes.delete (nc);
  
				-- Add the node to linked nodes:
				linked.append (node);
			end if;
		end move_to_linked_nodes;



		---------------------------------------------------------------------------------------------
		-- Appends the given airwire to the result:
		procedure add_airwire (aw : in type_airwire) is begin
			-- CS make sure length is greater zero ? Since we assume unique positions
			-- of the given nodes, this check should not be required.

			result.append (aw);
		end add_airwire;
		

		---------------------------------------------------------------------------------------------
		procedure query_given_strand (c : in pac_isolated_fragments.cursor) is
			strand : type_fragment renames element (c);

			linked : pac_vectors.list;
			
			procedure query_node (c : in pac_vectors.cursor) is
				node : type_vector renames element (c);
			begin
				move_to_linked_nodes (linked, node);
			end query_node;

		begin
			strand.nodes.iterate (query_node'access);
			isolated_fragments.append ((nodes => linked));
		end query_given_strand;



		---------------------------------------------------------------------------------------------
		procedure complete_fragments is
			fc : pac_isolated_fragments.cursor := isolated_fragments.first;

			type type_claimed_neigbor is record
				neigbor		: type_neigbor;
				fragment	: pac_isolated_fragments.cursor;
				processed	: boolean := false;
			end record;

			fragment_count : constant count_type := isolated_fragments.length;

			-- Prepare a table that contains for each fragment a nearest claimed neigbor:
			type type_claimed_neigbors is array (1 .. fragment_count) of type_claimed_neigbor;
			claimed_neigbors : type_claimed_neigbors;

			-- Prepare a table that contains the distances of all isolated nodes
			-- to the isolated fragments. Currently the table is empty:
			type type_distance_tables is array (1 .. fragment_count) of pac_distances_table.map;
			distance_tables : type_distance_tables;


			-----------------------------------------------------------------------------------------			
			-- Returns true if the given neigbor is closer to the given fragment
			-- than to any other fragment:
			function is_closest_to_fragment (
				f_idx	: in count_type; -- the index of the given fragment
				neigbor	: in type_neigbor)
				return boolean
			is
				use pac_distances_table;
				node_cursor : pac_distances_table.cursor;
				result : boolean := true;
			begin
				for i in distance_tables'first .. distance_tables'last loop
					if i /= f_idx then -- ignore the given fragment
						node_cursor := distance_tables (i).find (neigbor.node);
						if element (node_cursor) < neigbor.distance then
							return false;
						end if;
					end if;
				end loop;
				return result;
			end is_closest_to_fragment;
			
			
			idx : count_type := 1;

			-----------------------------------------------------------------------------------------			
			function is_unique (node : in type_vector) return boolean is
				occurences : count_type := 0;
			begin
				for i in claimed_neigbors'first .. claimed_neigbors'length loop
					if claimed_neigbors (i).neigbor.node = node then
						occurences := occurences + 1;
					end if;
				end loop;

				if occurences = 1 then
					return true;
				else
					return false;
				end if;

				-- CS if occurences = 0 then ?
			end is_unique;

			
			-----------------------------------------------------------------------------------------
			function get_idx_of_nearest (node : in type_vector) return count_type is
				result : count_type := 0;
				smallest_distance : type_float_positive := type_float_positive'last;
			begin
				for i in claimed_neigbors'first .. claimed_neigbors'length loop
					if claimed_neigbors (i).neigbor.node = node then
						if claimed_neigbors (i).neigbor.distance < smallest_distance then
							smallest_distance := claimed_neigbors (i).neigbor.distance;
							result := i;
						end if;
					end if;
				end loop;

				return result;
			end get_idx_of_nearest;


			-----------------------------------------------------------------------------------------
			procedure expand_fragment (i : in count_type) is
				
				procedure do_it (fr : in out type_fragment) is begin
					move_to_linked_nodes (fr.nodes, claimed_neigbors (i).neigbor.node);
					add_airwire (make_line (
						claimed_neigbors (i).neigbor.origin, claimed_neigbors (i).neigbor.node));
				end do_it;
				
			begin
				isolated_fragments.update_element (claimed_neigbors (i).fragment, do_it'access);			
			end expand_fragment;

		---------------------------------------------------------------------------------------------
		begin -- complete_fragments
				
			-- Iterate the isolated fragments. Each fragment
			-- claimes a nearest isolated neigbor. This neigbor
			-- is stored in an array.
			while fc /= pac_isolated_fragments.no_element loop
				
				claimed_neigbors (idx).neigbor := get_nearest_neighbor_of_fragment (fc, isolated_nodes);
				claimed_neigbors (idx).fragment := fc;

				-- Create a distances table for each fragment:
				distance_tables (idx) := get_distances_to_isoldated_nodes (fc, isolated_nodes);
				
				idx := idx + 1;
				next (fc);
			end loop;


			-- Iterate claimed neigbors:
			-- 1. If a neigbor is claimed by only one
			--    fragment, then this fragment gets expanded by that neigbor node.
			-- 2. If a neigbor is claimed by more than one fragment, then the fragment
			--    that is closest gets the expanded by that neigbor.
			for i in claimed_neigbors'first .. claimed_neigbors'length loop
				if is_unique (claimed_neigbors (i).neigbor.node) then -- case 1
					if is_closest_to_fragment (i, claimed_neigbors (i).neigbor) then
						expand_fragment (i);
					end if;
				else -- case 2
					idx := get_idx_of_nearest (claimed_neigbors (i).neigbor.node);
					if not claimed_neigbors (idx).processed then
						claimed_neigbors (idx).processed := true;
						expand_fragment (idx);
					end if;
				end if;						
			end loop;

		end complete_fragments;



		---------------------------------------------------------------------------------------------
		procedure make_single_fragment is

			procedure make_first_fragment (fragment : in out type_fragment) is
				start : type_vector;
				node : type_vector;
			begin
				-- Set the start point of the SCN:
				start := first_element (isolated_nodes);

				-- Apply P1:
				node := get_nearest_neighbor_of_node (isolated_nodes, start);

				move_to_linked_nodes (fragment.nodes, start);
				move_to_linked_nodes (fragment.nodes, node);
				-- The list isolated_nodes has been shortened by two nodes.
				-- The fragment now contains two nodes.

				-- create the first airwire
				add_airwire ((start, node));
			end make_first_fragment;


			-- We have to handle just a single fragment here.
			-- This container contains only one fragment:
			fragment : pac_isolated_fragments.list;
			
			neigbor : type_neigbor;
			
			procedure extend_fragment (fragment : in out type_fragment) is begin
				move_to_linked_nodes (fragment.nodes, neigbor.node);

				-- create the airwire
				add_airwire ((neigbor.origin, neigbor.node));
			end extend_fragment;

			
		begin
			-- If there are at least two nodes, then start constructing the SCN.
			-- Otherwise return an empty list of airwires.
			if isolated_nodes.length >= 2 then

				-- Create the first fragment of the SCN:
				fragment.append ((nodes => pac_vectors.empty_list));
				fragment.update_element (fragment.first, make_first_fragment'access);
				
				
				-- As long as there are any isolated nodes left over, apply P2.
				-- Each time P2 is applied
				-- - isolated_nodes becomes shorter by on node.
				-- - the list of nodes of the fragment becomes longer by one node.
				while not isolated_nodes.is_empty loop
					-- Apply P2:
					neigbor := get_nearest_neighbor_of_fragment (fragment.first, isolated_nodes);
					fragment.update_element (fragment.first, extend_fragment'access);
				end loop;
			end if;			
		end make_single_fragment;


		
		---------------------------------------------------------------------------------------------
		procedure connect_isolated_fragments is

			nearest_fragment : type_nearest_fragment;
			scratch : type_fragment;

			procedure query_fragment (fragment : in out type_fragment) is
			begin
				-- put_line ("nodes A" & count_type'image (fragment.nodes.length));
				-- put_line ("scratch" & count_type'image (scratch.nodes.length));
				splice (
					target	=> fragment.nodes,
					before	=> pac_vectors.no_element, 
					source	=> scratch.nodes);

				-- put_line ("nodes B" & count_type'image (fragment.nodes.length));
			end query_fragment;
			
		begin
			-- put_line ("isolated fragments init" & count_type'image (isolated_fragments.length));
			
			while isolated_fragments.length > 1 loop
				-- put_line ("isolated fragments left" & count_type'image (isolated_fragments.length));
				
				nearest_fragment := get_nearest_fragment (isolated_fragments, isolated_fragments.first);

				add_airwire ((nearest_fragment.neigbor.origin, nearest_fragment.neigbor.node));
				-- put_line (to_string (result.last_element));
				
				scratch := element (nearest_fragment.fragment);
				isolated_fragments.update_element (isolated_fragments.first, query_fragment'access);

				isolated_fragments.delete (nearest_fragment.fragment);
			end loop;

			null;
		end connect_isolated_fragments;

		
	-------------------------------------------------------------------------------------------------
	begin -- make_airwires

		if not strands.is_empty then -- means: if there are strands given
			-- put_line ("strands given");
			
			-- Fast forward: Build the isolated fragments from the given 
			-- strands:
			strands.iterate (query_given_strand'access);

			-- As long as there are isolated nodes left over,
			-- complete the isolated fragments:
			while not isolated_nodes.is_empty loop
				complete_fragments;
			end loop;

			-- The container "isolated_fragments" now contains all fragments.
			-- But there is no link between the fragments yet:
			connect_isolated_fragments;
			
		else
			-- no strands given
			-- put_line ("no strands given");
			make_single_fragment;
		end if;
			
		-- Return the collection of airwires:
		return result;
	end make_airwires;
	

end et_ratsnest;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

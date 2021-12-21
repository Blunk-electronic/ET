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


--with ada.containers.multiway_trees;

package body et_ratsnest is
	

	function contains_airwire (
		airwires	: in pac_airwires.list;
		airwire		: in type_line)
		return boolean
	is
		result : boolean := false;
	begin
		if airwires.contains (airwire) then
			result := true;

		-- The airwire could be reversed in the container
		-- (means start and end point swapped):
		elsif airwires.contains (type_line (reverse_line (airwire))) then
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
		while c /= no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;


	
	
	function make_airwires (
		nodes				: in pac_points.list;
		virtual_airwires	: in pac_airwires.list := pac_airwires.empty_list)
		return pac_airwires.list
	is		
		use pac_airwires;
		result : pac_airwires.list := pac_airwires.empty_list; -- to be returned
		
		use pac_points;
		start : type_point;

		-- This is the list of unconneced nodes. It will become
		-- shorter and shorter over time until it is empty. As soon as
		-- it is empty, the PRIM-algorithm ends:
		nodes_isolated : pac_points.list := nodes;

		-- These are the nodes of spann-graph that we are going to build.
		-- Once a node gets linked (with an airwire), the the node
		-- will be added to nodes_linked. So this list of nodes grows
		-- over time until all given nodes have been added to the graph.
		-- Initially it is empty:
		nodes_linked : pac_points.list := pac_points.empty_list;
		

		-- Moves the given node from the list of isolated nodes to
		-- the list of linked nodes. So the list nodes_isolated gets
		-- shorter by one node. The list nodes_linked gets longer by one node:
		procedure move_to_linked_nodes (node : in type_point) is
			nc : pac_points.cursor;
		begin
			-- remove from isolated nodes:
			nc := nodes_isolated.find (node);
			nodes_isolated.delete (nc);

			-- add to linked nodes:
			nodes_linked.append (node);
		end move_to_linked_nodes;


		-- Appends the given airwire to the result if it
		-- is not already in the given list of virtual_airwires:
		procedure add_airwire (aw : in type_line) is begin
			-- CS make sure length is greater zero ? Since we assume unique positions
			-- of the given nodes, this check should not be required.

			if not contains_airwire (virtual_airwires, aw) then
				result.append (aw);
			end if;
		end add_airwire;
		
		
		-- Returns the node that is nearest to the given node.
		-- It does not probe the distance of the given node to itself (which would be zero).
		-- This function works according to principle 1 (P1) of the PRIM-algorithm.
		-- It searches in the list of isolated nodes: 
		function get_nearest_neighbor_of_node (node_in : in type_point)
			return type_point
		is
			smallest_distance : type_distance_positive := type_distance'last;
			node_nearest : type_point; -- to be returned
			
			procedure query_node (c : in pac_points.cursor) is
				d_tmp : type_distance_positive;
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
			return type_point
		is
			node_nearest : type_point; -- to be returned

			-- The total number of nodes in the current graph. It is required
			-- in order to set up the array, because for each node of the graph
			-- there will be a nearest neigboring node:
			linked_total : constant count_type := nodes_linked.length;

			
			-- The neigboring node to be stored in the array:
			type type_neigbor is record
				-- the neigboring node itself:
				node		: type_point; 

				-- the distance to the graph:
				distance	: type_distance_positive := zero; 

				-- the referencing node in the graph:
				origin		: type_point;
			end record;

			-- Set up the array of neigboring nodes:
			type type_neigbors is array (1 .. linked_total) of type_neigbor;
			neigbors : type_neigbors := (others => <>);

			-- Set up a pointer to the elements of the array:
			subtype type_neigbor_pointer is count_type range 1 .. linked_total + 1;
			pointer : type_neigbor_pointer := 1;

			
			-- Finds the nearest isolated neigbor of a linked node:
			procedure query_node_linked (c : in pac_points.cursor) is
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
				smallest_distance : type_distance_positive := type_distance'last;
				aw_tmp : type_line;
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
						aw_tmp := type_line (make_line (neigbors (i).origin, neigbors (i).node));
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


		node_tmp : type_point;
				
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
	


end et_ratsnest;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

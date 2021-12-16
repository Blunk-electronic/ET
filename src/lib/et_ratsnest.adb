------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             RATSNEST                                     --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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



	--type type_nearest (connected : boolean) -- via secondary airwire
	--is record
		--case connected is
			--when TRUE => null;
			--when FALSE => cursor : pac_airwires.cursor := pac_airwires.no_element;
		--end case;
	--end record;
	
	
	--function get_nearest_primary_airwire (
		--a_in	: in pac_airwires.cursor)
		--return type_nearest
	--is 
		--smallest_distance : type_distance_positive := type_distance'last;

		--aw_tmp : type_line;

		---- the elements of the return:
		--connected : boolean := false;
		--nearest_airwire : pac_airwires.cursor := pac_airwires.no_element;

		----proceed : aliased boolean := true;
		--something_found : boolean := false;

		--length_min : type_distance_positive := zero;
		----d_min : type_distance_positive := zero;

		
		--procedure query_end_point (ca : in pac_airwires.cursor) is
			--d_tmp : type_distance_positive;
		--begin
			---- Query the end point of all primary airwires except the 
			---- given airwire indicted by cursor a_in:
			--if a_in /= ca then

				---- Get the distance from end point of a_in to the end point of the
				---- candidate airwire indicated by cursor ca:
				--d_tmp := get_absolute (get_distance (element (a_in).end_point, element (ca).end_point));
				----put_line (" end " & to_string (element (cp)) & " d " & to_string (d));

				---- The distance between the ends must be shorter than 
				---- the longest of the two primary airwires:
				--if d_tmp < get_greatest_length (element (a_in), element (ca)) then

					--something_found := true;
					
					---- Update smallest_distance if current distance (d_tmp) is
					---- smaller than the old smallest_distance:
					--if d_tmp > length_min and d_tmp < smallest_distance then
						--smallest_distance := d_tmp;
						--nearest_airwire := ca;
					--end if;
					
				--end if;
			--end if;
		--end query_end_point;


	--begin
		--airwires_primary.iterate (query_end_point'access);

		--if something_found then
			
			--aw_tmp := type_line (make_line (element (a_in).end_point, element (nearest_airwire).end_point));

			--if contains_airwire (airwires_to_add, aw_tmp) then
				
				--length_min := get_length (aw_tmp);
				--smallest_distance := type_distance_positive'last;
				--something_found := false;
				--airwires_primary.iterate (query_end_point'access);

				--if something_found then
					--if airwires_to_delete.contains (element (nearest_airwire)) then
						--connected := true;
					--end if;
				--else
					--connected := true;
				--end if;
				
			--else
				----return (connected => false, cursor => nearest_airwire);
				----cursor := nearest_airwire;
				--null;
			--end if;

		--else
			----return (connected => false, cursor => pac_airwires.no_element);
			--nearest_airwire := pac_airwires.no_element;
		--end if;

		
		--if connected then
			--return (connected => true);
		--else
			--return (connected => false, cursor => nearest_airwire);
		--end if;
	--end get_nearest_primary_airwire;
	

	
	--procedure query_airwire_primary (a : in pac_airwires.cursor) is
		--aw_tmp : type_line;
	--begin
		--put_line ("prim" & to_string (element (a)));
		
		--declare 
			--nearest : type_nearest := get_nearest_primary_airwire (a);
		--begin

			--if nearest.connected then

				--put_line ("is connected");
				
				---- remove obsolete primary airwire:
				--aw_tmp := element (a);
				--if not airwires_to_delete.contains (aw_tmp) then
					--airwires_to_delete.append (aw_tmp);
				--end if;
				
			--else

				--if nearest.cursor = pac_airwires.no_element then
					--null; -- keep primary airwire
				--else
					---- near primary airwire found
					--put_line ("nearest " & to_string (element (nearest.cursor)));

					---- make secondary airwire:
					--aw_tmp := type_line (make_line (element (a).end_point, element (nearest.cursor).end_point));

					--airwires_to_add.append (aw_tmp);

					---- if required, remove obsolete primary airwire:
					--aw_tmp := (type_line (get_longest (element (a), element (nearest.cursor))));
					--if not airwires_to_delete.contains (aw_tmp) then
						--airwires_to_delete.append (aw_tmp);
					--end if;
				--end if;
				
			--end if;
		--end;
	--end query_airwire_primary;


	
	function make_airwires (
		nodes	: in pac_points.list)
		return pac_airwires.list
	is		
		use pac_airwires;
		result : pac_airwires.list := pac_airwires.empty_list; -- to be returned
		
		use pac_points;
		start : type_point;

		-- This is the list of unconneced nodes. It will become
		-- shorter and shorter over time until it is empty.
		-- Initially it is a copy of the given nodes:
		nodes_isolated : pac_points.list := nodes;

		-- This is the spann-graph that we are going to build.
		-- Once a node gets linked (with an airwire), the the node
		-- will be added to nodes_linked. So this list of nodes grows
		-- over time until all given nodes have been added to the graph.
		-- Initially it is empty:
		nodes_linked : pac_points.list := pac_points.empty_list;
		

		-- Moves the given node from the list of isolated nodes to
		-- the list of linked nodes:
		procedure move_to_linked_nodes (node : in type_point) is
			nc : pac_points.cursor;
		begin
			-- remove from isolated nodes:
			nc := nodes_isolated.find (node);
			nodes_isolated.delete (nc);

			-- add to linked nodes:
			nodes_linked.append (node);
		end move_to_linked_nodes;


		-- Creates an airwire beweeen the given two nodes and
		-- appends it to the result:
		procedure make_airwire (node_1, node_2 : in type_point) is
			aw : type_line;
		begin
			-- CS make sure length is greater zero ?
			aw := type_line (make_line (node_1, node_2));
			result.append (aw);
		end make_airwire;
		
		
		-- Returns the node that is nearest to the given node.
		-- It does not probe the distance of the given node to itself (which would be zero).
		-- This function works according to principle 1 (P1).
		-- It searches in the list of isolated nodes: 
		function get_nearest_neighbor_of_node (node_in : in type_point)
			return type_point
		is
			smallest_distance : type_distance_positive := type_distance'last;
			node_nearest : type_point; -- to be returned
			
			procedure query_node (c : in pac_points.cursor) is
				d_tmp : type_distance_positive;
			begin
				if element (c) /= node_in then
					d_tmp := get_absolute (get_distance (element (c), node_in));

					if d_tmp < smallest_distance then
						smallest_distance := d_tmp;
						node_nearest := element (c);
					end if;
				end if;
			end query_node;
			
		begin
			nodes_isolated.iterate (query_node'access);
			return node_nearest;
		end get_nearest_neighbor_of_node;
		

		-- Returns the node that is nearest to the current graph nodes_linked:
		-- This function works according to principle 2 (P2):
		function get_nearest_neighbor_of_graph
			return type_point
		is
			node_nearest : type_point; -- to be returned
			linked_total : constant count_type := nodes_linked.length;
			
			type type_neigbor is record
				node		: type_point;
				distance	: type_distance_positive := zero;
				origin		: type_point;
			end record;

			type type_neigbors is array (1 .. linked_total) of type_neigbor;
			neigbors : type_neigbors := (others => <>);

			subtype type_neigbor_pointer is count_type range 1 .. linked_total + 1;
			neigbor_pointer : type_neigbor_pointer := 1;

			
			procedure query_node_linked (c : in pac_points.cursor) is
				neigbor : type_neigbor;
			begin
				neigbor.node := get_nearest_neighbor_of_node (element (c));
				neigbor.distance := get_absolute (get_distance (element (c), neigbor.node));
				neigbor.origin := element (c);

				neigbors (neigbor_pointer) := neigbor;
				neigbor_pointer := neigbor_pointer + 1;
			end query_node_linked;


			procedure find_nearest_among_neigbors is
				smallest_distance : type_distance_positive := type_distance'last;
				aw_tmp : type_line;
			begin
				for i in neigbors'first .. neigbors'last loop
					if neigbors (i).distance < smallest_distance then
						smallest_distance := neigbors (i).distance;
						node_nearest := neigbors (i).node;

						aw_tmp := type_line (make_line (neigbors (i).origin, neigbors (i).node));
					end if;
				end loop;

				--make_airwire (aw_tmp.start_point, aw_tmp.end_point);
				result.append (aw_tmp);
			end find_nearest_among_neigbors;
			
			
		begin
			nodes_linked.iterate (query_node_linked'access);

			find_nearest_among_neigbors;
			
			return node_nearest;
		end get_nearest_neighbor_of_graph;


		n : type_point;
				
	begin
		
		if nodes.length > 2 then

			-- Set the start point of the SCN:
			start := first_element (nodes_isolated);

			-- Apply P1:
			n := get_nearest_neighbor_of_node (start);

			-- create the first fragment of the SCN:
			move_to_linked_nodes (start);
			move_to_linked_nodes (n);

			-- create the first airwire
			--make_airwire (start, n);
			result.append ((start, n));


			while not nodes_isolated.is_empty loop
				n := get_nearest_neighbor_of_graph;

				move_to_linked_nodes (n);
			end loop;

		end if;

		return result;
	end make_airwires;
	


end et_ratsnest;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

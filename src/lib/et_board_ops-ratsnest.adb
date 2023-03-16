------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     BOARD OPERATIONS / RATSNEST                          --
--                                                                          --
--                               B o d y                                    --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 

with et_nets;							use et_nets;
with et_schematic;						use et_schematic;
with et_conductor_segment.boards;
with et_string_processing;				use et_string_processing;
with et_board_ops.devices;				use et_board_ops.devices;
with et_board_ops.vias;					use et_board_ops.vias;


package body et_board_ops.ratsnest is

	
	use pac_nets;


	function get_track_ends (
		net_cursor : in et_schematic.pac_nets.cursor)
		return pac_points.list
	is
		use et_conductor_segment.boards;
		use pac_points;
		result : pac_points.list;

		use pac_conductor_lines;
		procedure query_line (l : in pac_conductor_lines.cursor) is
		begin
			append (result, element (l).start_point);
			append (result, element (l).end_point);
		end query_line;

		
		use pac_conductor_arcs;
		procedure query_arc (a : in pac_conductor_arcs.cursor) is
		begin
			append (result, element (a).start_point);
			append (result, element (a).end_point);
		end query_arc;

		
	begin
		-- Query lines and arc segments:
		iterate (element (net_cursor).route.lines, query_line'access);
		iterate (element (net_cursor).route.arcs, query_arc'access);

		-- The ends of segments frequently overlap with those of other
		-- segments. This causes redundant points which must be removed:
		remove_redundant_points (result);
		
		return result;
	end get_track_ends;


	
	
	procedure update_ratsnest (
		module_cursor	: in pac_generic_modules.cursor;
		lth				: in type_log_level)
	is
		use et_conductor_segment.boards;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is				
				use pac_geometry_brd;
				use pac_vectors;
				nodes : pac_vectors.list;
				
				use pac_airwires;
				airwires : pac_airwires.list;
				
				tht_positions : pac_vectors.list;
				strands : et_ratsnest.pac_isolated_fragments.list;
				
			begin -- query_net
				log (text => "net " & to_string (net_name), level => lth + 1);

				-- get x/y positions of all terminals:
				nodes := get_terminal_positions (
					module_cursor	=> module_cursor, 
					net_cursor		=> net_cursor);
				
				-- Get x/y of all vias and append their positions to nodes.
				-- The via positions must be converted to location vectors:
				splice_vectors (nodes, to_vectors (get_via_positions (net_cursor)));

				-- Get x/y of track segments (start and end points)
				-- and append their positions to nodes.
				-- The end points of the tracks must be converted to location vectors:
				splice_vectors (nodes, to_vectors (get_track_ends (net_cursor)));

				-- CS submodules ?
				
				-- remove redundant/overlapping nodes
				remove_redundant_vectors (nodes);
				
					
				-- COMPUTE THE RATSNEST / AIRWIRES
				
				-- Remove excessive airwires due to already routed stuff.
				-- get x/y positions of all THT terminals:
				tht_positions := get_terminal_positions (
					module_cursor	=> module_cursor, 
					net_cursor		=> net_cursor,
					observe_techno	=> true,
					technology		=> THT);
			
				-- Compute the strands formed by lines, arcs, vias and terminals:
				strands := get_strands (
					lines		=> net.route.lines,
					arcs		=> net.route.arcs,
					vias		=> net.route.vias,
					terminals	=> tht_positions,
					deepest		=> deepest_conductor_layer (module_cursor));
				
				-- Make airwires from the list of nodes:
				airwires := make_airwires (nodes, strands);
				
				net.route.airwires.lines := airwires;				
			end query_net;

			
		begin
			while net_cursor /= pac_nets.no_element loop
				module.nets.update_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;				
		end query_module;

		
	begin
		log (text => "module " 
			& enclose_in_quotes (to_string (key (module_cursor)))
			& " updating ratsnest ...",
			level => lth);

		log_indentation_up;

		update_element (generic_modules, module_cursor, query_module'access);

		log_indentation_down;
	end update_ratsnest;



	function get_airwires (
		module_cursor	: in pac_generic_modules.cursor;
		point			: in type_point;
		catch_zone		: in type_catch_zone; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_proposed_airwires.list
	is
		result : pac_proposed_airwires.list;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) 
		is
			
			procedure query_net (nc : in pac_nets.cursor) is
				net : type_net renames element (nc);

				procedure query_airwire (ac : in pac_airwires.cursor) is
					use pac_airwires;
					use pac_geometry_brd;
					wire : type_airwire renames element (ac);
					d : type_float_positive := get_shortest_distance (to_vector (point), wire);
				begin
					if d <= catch_zone then
						result.append ((wire, key (nc)));
					end if;
				end query_airwire;
				
			begin
				net.route.airwires.lines.iterate (query_airwire'access);
			end query_net;
			
		begin
			module.nets.iterate (query_net'access);
		end query_module;
		
		
	begin
		log (text => "looking up airwires at" & to_string (point)
			 & " catch zone" & catch_zone_to_string (catch_zone),
			 level => log_threshold);

		-- log_indentation_up;

		query_element (module_cursor, query_module'access);

		-- log_indentation_down;
		return result;
	end get_airwires;

	
											
end et_board_ops.ratsnest;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

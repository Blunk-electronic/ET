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

with et_nets;
with et_net_names;						use et_net_names;
with et_schematic;						use et_schematic;
with et_conductor_segment.boards;
with et_ratsnest;						use et_ratsnest;
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

			procedure query_net (net_cursor : in pac_nets.cursor) is

				use et_nets;
				use pac_geometry_brd;
				use pac_vectors;
				nodes : pac_vectors.list;

				use pac_airwires;

				-- The function make_airwires generates airwires between terminals,
				-- vias and tracks. Each of them becomes a node.
				-- Regarding tracks: The start and end points of tracks also
				-- become nodes. But, an airwire between start and end of a single
				-- track segment is useless and must be supressed. For this
				-- reason we collect a list of such airwires in container virtual_airwires:
				virtual_airwires : pac_airwires.list;

				procedure make_virtual_airwires is
					use pac_conductor_lines;
					use pac_conductor_arcs;
					
					procedure query_line (l : in pac_conductor_lines.cursor) is begin
						--put_line ("virtual airwire: " & to_string (element (l)));
						virtual_airwires.append (to_airwire (element (l)));
					end query_line;

					procedure query_arc (a : in pac_conductor_arcs.cursor) is begin
						virtual_airwires.append ((
							start_point => to_vector (element (a).start_point), 
							end_point   => to_vector (element (a).end_point)));
					end query_arc;
					
				begin
					iterate (element (net_cursor).route.lines, query_line'access);
					iterate (element (net_cursor).route.arcs, query_arc'access);
				end make_virtual_airwires;
				

				procedure assign_airwires (
					net_name	: in pac_net_name.bounded_string;
					net			: in out type_net)
				is 
					airwires : pac_airwires.list;
				begin
					-- compute the ratsnest:
					-- Make airwires from the list of nodes. Suppress the 
					-- virtual airwires:
					airwires := make_airwires (nodes, virtual_airwires);				
					net.route.airwires.lines := airwires;
				end assign_airwires;


				procedure query_node (c : in pac_vectors.cursor) is begin
					put_line (to_string (element (c)));
				end query_node;
				
				
			begin -- query_net
				log (text => "net " & to_string (key (net_cursor)), level => lth + 1);
				
				-- get x/y of all terminals:
				nodes := get_terminal_positions (module_cursor, net_cursor);

				-- nodes.iterate (query_node'access); -- for debugging
				
				-- Get x/y of all vias and append their positions to nodes.
				-- The via positions must be converted to location vectors:
				splice_vectors (nodes, to_vectors (get_via_positions (net_cursor)));

				-- Get x/y track segments (start and end points)
				-- and append their positions to nodes.
				-- The end points of the tracks must be converted to location vectors:
				splice_vectors (nodes, to_vectors (get_track_ends (net_cursor)));

				-- CS submodules ?
				
				-- remove redundant/overlapping nodes
				remove_redundant_vectors (nodes);

				-- Create from the tracks a list of virtual_airwires. These airwires
				-- are later required in order to create the real airwires.
				make_virtual_airwires;
				
				update_element (module.nets, net_cursor, assign_airwires'access);
			end query_net;

			
		begin -- query_module
			module.nets.iterate (query_net'access);
		end query_module;

		
	begin -- update_ratsnest

		log (text => "module " 
			& enclose_in_quotes (to_string (key (module_cursor)))
			& " updating ratsnest ...",
			level => lth);

		log_indentation_up;

		update_element (generic_modules, module_cursor, query_module'access);

		log_indentation_down;

	end update_ratsnest;


											
end et_board_ops.ratsnest;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
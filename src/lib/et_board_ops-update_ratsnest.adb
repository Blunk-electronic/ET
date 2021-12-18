------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                  BOARD OPERATIONS / UPDATE RATSNEST                      --
--                                                                          --
--                               B o d y                                    --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.strings;					use ada.strings;
with ada.exceptions;
with et_exceptions;					use et_exceptions;
with et_ratsnest;					use et_ratsnest;
--with et_routing;					use et_routing;


separate (et_board_ops)

procedure update_ratsnest (
	module_cursor	: in pac_generic_modules.cursor;
	lth				: in type_log_level)
is
	
	procedure query_module (
		module_name	: in pac_module_name.bounded_string;
		module		: in out type_module) 
	is

		procedure query_net (net_cursor : in pac_nets.cursor) is

			use pac_points;
			nodes : pac_points.list;

			use pac_airwires;


			virtual_airwires : pac_airwires.list;

			procedure make_virtual_airwires is
				use pac_conductor_lines;
				procedure query_line (l : in pac_conductor_lines.cursor) is
				begin
					virtual_airwires.append (type_line (element (l)));
				end query_line;

				use pac_conductor_arcs;
				procedure query_arc (a : in pac_conductor_arcs.cursor) is
				begin
					virtual_airwires.append ((element (a).start_point, element (a).end_point));
				end query_arc;
				
			begin
				iterate (element (net_cursor).route.lines, query_line'access);
				iterate (element (net_cursor).route.arcs, query_arc'access);
			end make_virtual_airwires;
			

			procedure assign_airwires (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is 
				--aw : type_airwire := (type_line (make_line (1.0, 1.0, 10.0, 10.0)) with null record);
				airwires : pac_airwires.list;
			begin
				-- compute the ratsnest:
				airwires := make_airwires (nodes);
				
				-- CS remove airwires where track segments are already laid out ?
				
				net.route.airwires.lines := airwires;

			end assign_airwires;


			procedure query_node (c : in pac_points.cursor) is
			begin
				put_line (to_string (element (c)));
			end query_node;
			
			
		begin
			put_line ("net " & to_string (key (net_cursor)));
			
			-- get x/y of all terminals:
			nodes := get_terminal_positions (module_cursor, net_cursor);

			-- nodes.iterate (query_node'access); -- for debugging
			
			-- Get x/y of all vias and append their positions to nodes:
			splice_points (nodes, get_via_positions (net_cursor));

			-- Get x/y track segments (start and end points)
			-- and append their positions to nodes:
			splice_points (nodes, get_track_ends (net_cursor));

			-- remove redundant/overlapping nodes
			remove_redundant_points (nodes);
			
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
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

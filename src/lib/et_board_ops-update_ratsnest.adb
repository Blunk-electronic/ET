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
		net_cursor : pac_nets.cursor := module.nets.first;

		--procedure query_net (
			--net_name	: in pac_net_name.bounded_string;
			--net			: in out type_net)
		--is 
			--aw : type_airwire := (type_line (make_line (1.0, 1.0, 10.0, 10.0)) with null record);

		--begin
			--log (text => "net " & to_string (net_name), level => lth + 1);
			
			--net.route.airwires.lines.append (aw);

		--lines 		: pac_conductor_lines.list;
		--arcs		: pac_conductor_arcs.list;
		--vias		: pac_vias.list;


		points : pac_points.list;

		procedure query_net (n : in pac_nets.cursor) is
		begin
			-- get x/y of all terminals:
			points := get_terminal_positions (module_cursor, net_cursor);

			-- get x/y of all vias and append to points:
			splice_points (points, get_via_positions (net_cursor));

		end query_net;

		
	begin -- query_module

		iterate (module.nets, query_net'access);


		
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

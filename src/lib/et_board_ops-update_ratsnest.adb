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

		procedure query_net (
			net_name	: in pac_net_name.bounded_string;
			net			: in out type_net)
		is 
			--aw : type_airwire := (1.0;
		begin
			log (text => "net " & to_string (net_name), level => lth + 1);

			--append 
			null;
		end query_net;

		
	begin -- query_module
		while net_cursor /= pac_nets.no_element loop
			update_element (module.nets, net_cursor, query_net'access);
			next (net_cursor);
		end loop;
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

------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       BOARD OPERATIONS / NET CLASS                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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


with et_string_processing;			use et_string_processing;
with et_schematic_ops.nets;			use et_schematic_ops.nets;
with et_net_classes;				use et_net_classes;



package body et_board_ops.net_class is



	function get_net_class (
		module	: in pac_generic_modules.cursor; -- the module like motor_driver
		class	: in pac_net_class_name.bounded_string) -- hi-voltage, si-critical
		return type_net_class
	is
		result : type_net_class;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is
			use pac_net_classes;
			use pac_net_class_name;
		begin
			if class = net_class_name_default then
				null;
				-- CS load result with DRU settings (min track clearance, min track width, 
				-- min via drill size)
			else
				result := element (find (module.net_classes, class));
			end if;
		end query_module;

		
	begin
		query_element (module, query_module'access);
		
		return result;
	end get_net_class;



	

	

	function get_net_class (
		module	: in pac_generic_modules.cursor; -- the module like motor_driver
		net		: in pac_nets.cursor) -- GND, RESET_N, ...
		return type_net_class
	is

		result : type_net_class;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is
			use pac_net_classes;
			use pac_net_class_name;
			use pac_nets;
		begin
			if net = et_nets.pac_nets.no_element then -- freetrack
				null;
				-- CS load result with DRU settings (min track clearance, min track width, 
				-- min via drill size)
			else
				if element (net).class = net_class_name_default then
					null;
					-- CS load result with DRU settings (min track clearance, min track width, 
					-- min via drill size)
				else
					result := element (find (module.net_classes, element (net).class));
				end if;
			end if;
		end query_module;

		
	begin
		query_element (module, query_module'access);
		return result;
	end get_net_class;





	



	function get_class_name (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in pac_nets.cursor)
		return pac_net_class_name.bounded_string
	is
		result : pac_net_class_name.bounded_string;
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is
			use pac_nets;

			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net)
			is begin
				result := get_class_name (net);
			end;
			
		begin
			query_element (net_cursor, query_net'access);
		end query_module;
		
		
	begin
		query_element (module_cursor, query_module'access);
		return result;
	end get_class_name;


	

	


	
	procedure set_net_class (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		net_class		: in pac_net_class_name.bounded_string; -- pwr
		log_threshold	: in type_log_level)
	is
		net_cursor : pac_nets.cursor; -- points to the net

		use pac_nets;
		
		
		procedure query_module (
			name	: in pac_module_name.bounded_string;
			module	: in out type_generic_module)
		is

			
			procedure set_class (
				name	: in pac_net_name.bounded_string;
				net		: in out type_net)
			is 
				use pac_net_class_name;
			begin
				if net.class = net_class then
					log (text => "Net already in class " 
							& enclose_in_quotes (et_net_class_name.to_string (net_class)),
						level => log_threshold + 1);
				else
					log (text => "Changing net class from "
						 & enclose_in_quotes (et_net_class_name.to_string (net.class)) 
						 & " to " & enclose_in_quotes (et_net_class_name.to_string (net_class)),
						level => log_threshold + 1);

					net.class := net_class;
				end if;
			end set_class;
			
			
		begin
			pac_nets.update_element (
				container	=> module.nets,
				position	=> net_cursor,
				process		=> set_class'access);

		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " setting class of net " & net_name_to_string (net_name) 
			& " to " & enclose_in_quotes (to_string (net_class)),
			level => log_threshold);
		

		-- The net can be in the module already. Locate the requested net in the module.
		-- net_cursor will point to no_element if the net is not already there.
		net_cursor := locate_net (module_cursor, net_name);

		log_indentation_up;

		-- CS test whether given net class exists
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
			
		log_indentation_down;
	end set_net_class;

	
	

end et_board_ops.net_class;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

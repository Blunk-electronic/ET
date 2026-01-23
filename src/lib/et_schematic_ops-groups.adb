------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    SCHEMATIC OPERATIONS ON GROUPS                        --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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


with et_schematic_ops_nets;
with et_schematic_ops.units;



package body et_schematic_ops.groups is

	
	procedure reset_objects (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure reset_nets is 
			use et_schematic_ops_nets;
		begin
			log (text => "nets", level => log_threshold + 1);
			log_indentation_up;
			reset_status_objects (module_cursor, log_threshold + 2);
			log_indentation_down;
		end;

		
		procedure reset_devices is 
			use et_schematic_ops.units;
		begin
			log (text => "electrical devices and units", level => log_threshold + 1);
			log_indentation_up;
			reset_status_objects (module_cursor, log_threshold + 2);
			log_indentation_down;
		end;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			 & " reset objects (schematic)",
			level => log_threshold);

		log_indentation_up;
		
		reset_nets;
		reset_devices;		

		-- CS reset texts, ... ?
		
		log_indentation_down;
	end reset_objects;
	
	
end et_schematic_ops.groups;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

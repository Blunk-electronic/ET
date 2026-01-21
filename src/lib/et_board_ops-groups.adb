------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       BOARD OPERATIONS ON GROUPS                         --
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


with et_board_ops_devices;
with et_board_ops_assy_doc;
with et_board_ops.silkscreen;
with et_board_ops.stopmask;
with et_board_ops.stencil;
with et_board_ops.keepout;
with et_board_ops.outline;
with et_board_ops_conductors;
with et_board_ops.vias;

with et_ripup;
with et_board_ops.ratsnest;


package body et_board_ops.groups is


	procedure reset_objects (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure reset_devices is 
			use et_board_ops_devices;
		begin
			log (text => "devices (electrical and non-electrical)", level => log_threshold + 1);
			log_indentation_up;
			reset_status_objects (module_cursor, log_threshold + 2);
			log_indentation_down;
		end;


	begin
		log (text => "module " & to_string (module_cursor) 
			 & " reset objects (board)",
			level => log_threshold);

		log_indentation_up;
		
		reset_devices;

		-- CS reset board placeholders, texts, ... ?

		et_board_ops_assy_doc.reset_status_objects (active_module, log_threshold + 1);
		et_board_ops.silkscreen.reset_proposed_objects (active_module, log_threshold + 1);
		et_board_ops.stopmask.reset_proposed_objects (active_module, log_threshold + 1);
		et_board_ops.stencil.reset_proposed_objects (active_module, log_threshold + 1);
		et_board_ops.keepout.reset_proposed_objects (active_module, log_threshold + 1);
		et_board_ops.outline.reset_proposed_objects (active_module, log_threshold + 1);
		et_board_ops_conductors.reset_proposed_objects (active_module, log_threshold + 1);
		et_board_ops.vias.reset_proposed_vias (active_module, log_threshold + 1);

		et_board_ops.ratsnest.reset_proposed_airwires (active_module, log_threshold + 1);
		et_ripup.reset_ripup_mode;

		log_indentation_down;
	end reset_objects;

	
	
end et_board_ops.groups;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

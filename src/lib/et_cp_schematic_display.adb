------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--             COMMAND PROCESSOR / SCHEMATIC / LAYER DISPLAY                --
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
-- To Do:
--
--
--

with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;
with ada.strings; 						use ada.strings;

with et_display;						use et_display;
with et_display.schematic;				use et_display.schematic;

with et_generic_modules;				use et_generic_modules;
with et_modes.schematic;				use et_modes.schematic;



package body et_cp_schematic_display is

	

	procedure display (
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);

		
		procedure do_it ( 
			layer	: in type_noun;
			status	: in string := "") 
		is
			ls : type_layer_status;
		begin
			-- Convert the given status to type_layer_status.
			-- If no status given, assume status ON:
			if status = "" then
				ls := ON;
			else
				ls := to_layer_status (status);
			end if;
			
			log (text => "display " & to_lower (to_string (layer)) 
					& space & to_string (ls),
					level => log_threshold + 1);
			
			case layer is
				when NOUN_NAMES		=> layers.device_names := ls;
				when NOUN_NETS		=> layers.nets := ls;
				when NOUN_PORTS		=> layers.ports := ls;
				when NOUN_PURPOSES	=> layers.device_purposes := ls;
				when NOUN_TEXTS		=> layers.texts := ls;
				when NOUN_VALUES	=> layers.device_values := ls;
				
				when others => 
					log (importance => ERROR, text => "invalid layer !", console => true);
			end case;

			-- CS exception handler if status is invalid
		end do_it;

		
	begin
		case cmd_field_count is
			when 4 => do_it (noun); -- if status is omitted
			
			when 5 => do_it (noun, get_field (cmd, 5));
			
			when 6 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end display;

	
		
end et_cp_schematic_display;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

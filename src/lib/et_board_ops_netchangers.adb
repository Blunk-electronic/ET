------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     BOARD OPERATIONS / NETCHANGERS                       --
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
-- To Do: 
--
--


with ada.text_io;						use ada.text_io;
with ada.containers;

with ada.exceptions;					use ada.exceptions;
with et_string_processing;				use et_string_processing;

with et_module;							use et_module;
with et_schematic_ops_netchangers;		use et_schematic_ops_netchangers;

with et_netchangers.board;				use et_netchangers.board;


package body et_board_ops_netchangers is


	use pac_netchangers;
	

	
	

	procedure move_netchanger (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id;
		coordinates		: in type_coordinates;
		point			: in type_vector_model;
		log_threshold	: in type_log_level) 
	is

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			netchanger_cursor : pac_netchangers.cursor;
			
			
			procedure move (
				index		: in type_netchanger_id;
				netchanger	: in out type_netchanger) 
			is 
				place : type_vector_model;
			begin
				-- calculate the new position 
				case coordinates is
					when ABSOLUTE =>
						-- The absolute position is defined 
						-- by the given point (x/y):
						set_place (netchanger, point);

					when RELATIVE =>
						-- The relative position is the 
						-- netchanger position BEFORE 
						-- the move operation shifted by 
						-- the given point (x/y):
						place := get_place (netchanger);
						add (place, point);
						set_place (netchanger, place);
				end case;
			end move;

			
		begin
			-- Locate given netchanger in the module:
			netchanger_cursor := get_netchanger (module_cursor, index);

			-- Move the netchanger:
			update_element (
				container	=> module.netchangers,
				position	=> netchanger_cursor,
				process		=> move'access);

		end query_module;

		
	begin
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_cursor)
					& " move netchanger " & to_string (index) 
					& " to " & to_string (point),
					 level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_cursor)
					& " move netchanger " & to_string (index) 
					& " by " & to_string (point),
					level => log_threshold);
		end case;

		
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end move_netchanger;

	
	
end et_board_ops_netchangers;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

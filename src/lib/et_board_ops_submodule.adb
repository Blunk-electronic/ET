------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     BOARD OPERATIONS / SUBMODULE                         --
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
-- - clean up, rework
--

with ada.exceptions;				use ada.exceptions;

with et_module;						use et_module;
with et_submodules;					use et_submodules;
with et_schematic_ops_submodules;	use et_schematic_ops_submodules;



package body et_board_ops_submodule is




	function get_position (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string) -- OSC1
		return type_position 
	is		
		position : type_position := origin_zero_rotation; -- to be returned

		module_cursor : pac_generic_modules.cursor; -- points to the module

		
		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use et_submodules.pac_submodules;
			submod_cursor : et_submodules.pac_submodules.cursor;
		begin
			submod_cursor := find (module.submods, instance);
			position := element (submod_cursor).position_in_board;
		end;

		
	begin -- get_position
		-- locate the given module
		module_cursor := locate_module (module_name);

		pac_generic_modules.query_element (
			position	=> module_cursor,
			process		=> query_submodules'access);

		return position;
	end get_position;





	

	-- Moves a submodule instance within the parent module layout in x/y direction.
	-- Leaves rotation and face (top/bottom) as it is.
	procedure move_submodule (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		coordinates		: in type_coordinates; -- relative/absolute		
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level)
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		
		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use et_submodules.pac_submodules;
			submod_cursor : et_submodules.pac_submodules.cursor;

			
			procedure move (
				instance	: in pac_module_instance_name.bounded_string;
				submodule	: in out et_submodules.type_submodule) 
			is begin
				case coordinates is
					when ABSOLUTE =>
						set_place (submodule.position_in_board, point);

					when RELATIVE =>
						move_by (submodule.position_in_board.place, point);
				end case;

				exception
					when event: others =>
						log (ERROR, "coordinates invalid !", console => true); -- CS required more details
						log (text => ada.exceptions.exception_information (event), console => true);
						raise;
				
			end move;

			
		begin -- query_submodules
			if contains (module.submods, instance) then

				submod_cursor := find (module.submods, instance); -- the submodule should be there

				update_element (
					container	=> module.submods,
					position	=> submod_cursor,
					process		=> move'access);

			else
				submodule_not_found (instance);
			end if;

		end;

		
	begin -- move_submodule
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" moving submodule instance " & to_string (instance) &
					" to" & to_string (point), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" moving submodule instance " & to_string (instance) &
					" by" & to_string (point), level => log_threshold);
		end case;

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_submodules'access);

		-- CS update_ratsnest (module_cursor, log_threshold + 1);
		-- requires to move this procedure to a child package
		-- for operations on submodules.
	end move_submodule;


	

	
end et_board_ops_submodule;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

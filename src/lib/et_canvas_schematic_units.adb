------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        CANVAS SCHEMATIC UNITS                            --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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
--                                                                          --
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

with ada.text_io;					use ada.text_io;

with et_canvas_schematic;			use et_canvas_schematic;

package body et_canvas_schematic_units is

	use et_canvas_schematic.pac_canvas;
	
	-- Deletes a unit in the vicinity of given point.
	-- If more than one unit near point found, then it sets the
	-- cursor selected_unit to the first unit and requests
	-- for clarification.
	procedure delete_unit (point : in type_point) is 
		use et_schematic_ops.units;
		use pac_selected_units;
		unit_cursor : pac_selected_units.cursor;
	begin
		log (text => "deleting unit ...", level => log_threshold);
		log_indentation_up;
		
		-- Collect all units in the vicinity of the given point:
		selected_units := collect_units (
			module			=> current_active_module,
			place			=> to_position (point, current_active_sheet),
			catch_zone		=> catch_zone_default, -- CS should depend on current scale
			log_threshold	=> log_threshold + 1);

		-- evaluate the number of units found here:
		case length (selected_units) is
			when 0 =>
				reset_request_clarification;
				
			when 1 =>
				unit_cursor := selected_units.first;
			
				delete_selected_unit (
					module_cursor	=> current_active_module,
					unit			=> element (unit_cursor),
					log_threshold	=> log_threshold + 1);

				reset_request_clarification;
				set_status (status_click_left & "delete unit." & status_hint_for_abort);
				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;

				-- preselect the first unit
				selected_unit := selected_units.first;
		end case;
		
		log_indentation_down;
	end delete_unit;

	-- Advances cursor selected_unit to next unit in list selected_units.
	procedure clarify_unit is
		use et_schematic;
		use et_schematic_ops.units;
		use pac_selected_units;
		u : type_units.cursor;
	begin
		-- On every call of this procedure we must advance from one
		-- unit to the next in a circular manner. So if the end 
		-- of the list is reached, then the cursor selected_unit
		-- moves back to the start of the unit list.
		if next (selected_unit) /= pac_selected_units.no_element then
			next (selected_unit);
		else
			selected_unit := selected_units.first;
		end if;

		-- show the selected unit in the status bar
		u := element (selected_unit).unit;
	
		set_status ("unit " & to_string (u));
	end clarify_unit;
	
	-- Deletes the unit being pointed at by cursor selected_unit.
	procedure delete_selected_unit is
		use et_schematic_ops.units;
		use pac_selected_units;
	begin
		log (text => "deleting unit after clarification ...", level => log_threshold);
		log_indentation_up;

		delete_selected_unit (
			module_cursor	=> current_active_module,
			unit			=> element (selected_unit),
			log_threshold	=> log_threshold + 1);

		-- Update list of selected units:
		delete (selected_units, selected_unit);
		
		reset_request_clarification;
		set_status (status_click_left & "delete unit." & status_hint_for_abort);
		
		log_indentation_down;
	end delete_selected_unit;

	
end et_canvas_schematic_units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

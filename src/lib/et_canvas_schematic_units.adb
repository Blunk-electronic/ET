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
with et_general;					use et_general;
with et_symbols;					use et_symbols;
with et_devices;					use et_devices;
with et_schematic;					use et_schematic;

with et_canvas_schematic;			use et_canvas_schematic;

package body et_canvas_schematic_units is

	use et_canvas_schematic.pac_canvas;

	function collect_units (
		module			: in pac_generic_modules.cursor;
		place			: in et_coordinates.type_position; -- sheet/x/y
		catch_zone		: in type_catch_zone; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_selected_units.list
	is
		use pac_selected_units;
		result : pac_selected_units.list;

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) 
		is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor := module.devices.first;

			procedure query_units (
				device_name	: in type_name;
				device		: in et_schematic.type_device)
			is
				use et_schematic.type_units;
				unit_cursor : et_schematic.type_units.cursor := device.units.first;
			begin
				while unit_cursor /= et_schematic.type_units.no_element loop
					
					-- We are interested in units on the given sheet only:
					if sheet (element (unit_cursor).position) = sheet (place) then

						log (text => "probing unit " & to_string (unit_cursor),
							level => log_threshold + 1);

						if in_catch_zone (place, catch_zone, element (unit_cursor).position) then
							log_indentation_up;

							log (text => "in catch zone", level => log_threshold + 1);
							result.append ((device_cursor, unit_cursor));
							
							log_indentation_down;
						end if;
					end if;

					next (unit_cursor);
				end loop;
				
			end query_units;
			
		begin -- query_devices
			while device_cursor /= et_schematic.type_devices.no_element loop

				log (text => "probing device " & to_string (key (device_cursor)),
					 level => log_threshold + 1);
				log_indentation_up;
					 
				query_element (
					position	=> device_cursor,
					process		=> query_units'access);

				next (device_cursor);

				log_indentation_down;
			end loop;
		end query_devices;

	begin -- collect_units
		log (text => "looking up units at" & to_string (place) 
			 & " catch zone" & to_string (catch_zone), level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module,
			process		=> query_devices'access);

		log_indentation_down;
		
		return result;
		
	end collect_units;


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


	
-- DELETE UNIT

	-- Deletes a unit of a device. 
	-- In case the last unit has been deleted, then the device is 
	-- deleted entirely from the module.
	-- Mind that the parameter unit is an in/out !
	procedure delete_unit (
		module_cursor	: in pac_generic_modules.cursor;
		unit			: in out type_selected_unit;
		log_threshold	: in type_log_level)
	is
		use et_schematic.type_devices;
		use et_schematic.type_units;

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			
			-- temporarily storage of unit coordinates.
			-- There will be only one unit in this container.
			position_of_unit : type_unit_positions.map;

			ports : et_symbols.type_ports.map;

			procedure query_units (
				device_name	: in type_name;
				device		: in out et_schematic.type_device) is
			begin
				-- Load the single unit position and insert in container "position_of_unit"
				type_unit_positions.insert (
					container	=> position_of_unit, 
					key			=> key (unit.unit),
					new_item	=> element (unit.unit).position);

				log_unit_positions (position_of_unit, log_threshold + 1);
				
				-- delete the unit
				delete (device.units, key (unit.unit));
				
			end query_units;
			
			units_invoked : boolean := true; -- goes false if no unit used anymore

			procedure query_number_of_invoked_units (
				device_name	: in type_name;
				device		: in et_schematic.type_device) is
			begin
				if length (device.units) = 0 then
					units_invoked := false;
				end if;
			end query_number_of_invoked_units;

		begin -- query_devices
			-- Fetch the ports of the unit to be deleted.
			ports := ports_of_unit (unit.device, key (unit.unit));

			-- locate the unit, load position and then delete the targeted unit
			update_element (
				container	=> module.devices,
				position	=> unit.device,
				process		=> query_units'access);
			
			-- Delete the ports of the targeted unit from module.nets
			delete_ports (
				module			=> module_cursor,
				device			=> key (unit.device),
				ports			=> ports,
				sheets			=> position_of_unit, -- there is only one unit -> only one sheet to look at
				log_threshold	=> log_threshold + 1);

			-- In case no more units are invoked then the device must be
			-- deleted entirely from module.devices.
			-- First we query the number of still invoked units. If none invoked,
			-- the flag units_invoked goes false.
			query_element (
				position	=> unit.device,
				process		=> query_number_of_invoked_units'access);

			if not units_invoked then
				delete (module.devices, unit.device);
			end if;

		end query_devices;

	begin -- delete_unit
		log (text => "module " & to_string (key (module_cursor)) &
			 " deleting " & to_string (key (unit.device)) & " unit " & 
			 to_string (key (unit.unit)) & " ...", level => log_threshold);

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);
		
		log_indentation_down;				
	end delete_unit;
	
	procedure delete (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		unit			: in type_selected_unit; -- device/unit
		log_threshold	: in type_log_level)
	is 
		su : type_selected_unit := unit;		
	begin
		delete_unit (module_cursor, su, log_threshold);
		-- NOTE: su has been modified !
	end delete;
	
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
			
				delete (
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

	procedure delete_selected_unit is
		use et_schematic_ops.units;
		use pac_selected_units;
	begin
		log (text => "deleting unit after clarification ...", level => log_threshold);
		log_indentation_up;

		delete (
			module_cursor	=> current_active_module,
			unit			=> element (selected_unit),
			log_threshold	=> log_threshold + 1);

		-- clear list of selected units:
		delete (selected_units, selected_unit);
		
		reset_request_clarification;
		set_status (status_click_left & "delete unit." & status_hint_for_abort);
		
		log_indentation_down;
	end delete_selected_unit;
	

-- MOVE UNIT
	
	procedure move (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		unit			: in type_selected_unit; -- device/unit
		log_threshold	: in type_log_level)
	is 
		su : type_selected_unit := unit;		
	begin
		-- 		delete_unit (module_cursor, su, log_threshold);
		null;
		-- NOTE: su has been modified !
	end move;

	
	procedure move_unit (point : in type_point) is 
		use et_schematic_ops.units;
		use pac_selected_units;
		unit_cursor : pac_selected_units.cursor;
	begin
		log (text => "moving unit ...", level => log_threshold);
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
			
				move (
					module_cursor	=> current_active_module,
					unit			=> element (unit_cursor),
					log_threshold	=> log_threshold + 1);

				reset_request_clarification;
				set_status (status_click_left & "move unit." & status_hint_for_abort);
				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;

				-- preselect the first unit
				selected_unit := selected_units.first;
		end case;
		
		log_indentation_down;
	end move_unit;


	procedure move_selected_unit is
		use et_schematic_ops.units;
		use pac_selected_units;
	begin
		log (text => "moving unit after clarification ...", level => log_threshold);
		log_indentation_up;

		move (
			module_cursor	=> current_active_module,
			unit			=> element (selected_unit),
			log_threshold	=> log_threshold + 1);

		-- clear list of selected units:
		delete (selected_units, selected_unit);
		
		reset_request_clarification;
		set_status (status_click_left & "move unit." & status_hint_for_abort);
		
		log_indentation_down;
	end move_selected_unit;

	
end et_canvas_schematic_units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--              SCHEMATIC OPERATIONS / ROTATING UNITS                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

separate (et_schematic_ops)

procedure rotate_unit (
-- Rotates the given unit within the schematic. Disconnects the unit from
-- start or end points of net segments.
-- Rotates the placeholders around the unit.
	module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
	device_name		: in type_name; -- IC45
	unit_name		: in type_unit_name.bounded_string; -- A
	coordinates		: in type_coordinates; -- relative/absolute
	rotation		: in et_coordinates.type_rotation; -- 90
	log_threshold	: in type_log_level) is

	module_cursor : pac_generic_modules.cursor; -- points to the module being modified

	procedure query_devices (
		module_name	: in type_module_name.bounded_string;
		module		: in out type_module) is
		use et_schematic.type_devices;
		device_cursor : et_schematic.type_devices.cursor;

		position_of_unit : et_coordinates.type_position;
		rotation_before : et_coordinates.type_rotation;

		ports_lib, ports_scratch : et_symbols.type_ports.map;

		procedure query_units (
			device_name	: in type_name;
			device		: in out et_schematic.type_device) is
			use et_schematic.type_units;
			unit_cursor : et_schematic.type_units.cursor;

			procedure rotate_unit (
				name	: in type_unit_name.bounded_string; -- A
				unit	: in out type_unit) is

				preamble : constant string := " placeholder now at";
				
				procedure rotate_placeholders_absolute (rot : in type_rotation) is 

					-- Get the default positions of texts and placeholders as
					-- specified in symbol model. The default positions are
					-- later rotated by the given rotation rot.
					default_positions : et_symbols.type_default_text_positions := 
											default_text_positions (device_cursor, name);
					
					-- Rotates the position by the given rotation rot:
					function add_rot (p : in type_point) return type_rotation is begin
						return pac_geometry_sch.rotation (p) + rot;
					end;

					use pac_text;
				begin
					-- The current positions of the placeholders are overwritten by
					-- the defaults as specified in the symbol mode.
					-- Then the position of placeholders around the origin of the unit
					-- are rotated.
					
					-- NAME
					-- Rotate the placeholder around its own anchor point so that it
					-- it is readable from the front or from the right.
					unit.name.rotation := snap (default_positions.name.rotation + rot);

					-- reset the placeholder anchor point to the position as specified in the symbol model
					unit.name.position := default_positions.name.position;
					
					-- rotate the placeholder anchor point around the symbol origin:
					rotate_to (unit.name.position, add_rot (default_positions.name.position));
								
					log (text => "name" & preamble & to_string (unit.name.position), 
							level => log_threshold + 2);


					-- VALUE
					-- Rotate the placeholder around its own anchor point so that it
					-- it is readable from the front or from the right.
					unit.value.rotation := snap (default_positions.value.rotation + rot);
					
					-- reset the placeholder anchor point to the position as specified in the symbol model
					unit.value.position := default_positions.value.position;
					
					-- rotate the placeholder anchor point around the symbol origin:
					rotate_to (unit.value.position, add_rot (default_positions.value.position));

					log (text => "value" & preamble & to_string (unit.value.position), 
							level => log_threshold + 2);


					-- PURPOSE
					-- Rotate the placeholder around its own anchor point so that it
					-- it is readable from the front or from the right.
					unit.purpose.rotation := snap (default_positions.purpose.rotation + rot);

					-- reset the placeholder anchor point to the position as specified in the symbol model
					unit.purpose.position := default_positions.purpose.position;
					
					-- rotate the placeholder anchor point around the symbol origin:
					rotate_to (unit.purpose.position, add_rot (default_positions.purpose.position));

					log (text => "purpose" & preamble & to_string (unit.purpose.position), 
							level => log_threshold + 2);

				end rotate_placeholders_absolute;
				
				procedure rotate_placeholders_relative (rot : in type_rotation) is 
					use pac_text;
				begin
					-- Rotate position of placeholders around the unit origin. 
				
					-- NAME
					-- Rotate the placeholder around its own anchor point so that it
					-- it is readable from the front or from the right.
					unit.name.rotation := snap (unit.name.rotation + rot);

					-- rotate the placeholder anchor point around the symbol origin:
					rotate_by (unit.name.position, rot);

					log (text => "name" & preamble & to_string (unit.name.position), 
							level => log_threshold + 2);


					-- VALUE
					-- Rotate the placeholder around its own anchor point so that it
					-- it is readable from the front or from the right.
					unit.value.rotation := snap (unit.value.rotation + rot);

					-- rotate the placeholder anchor point around the symbol origin:
					rotate_by (unit.value.position, rot);

					log (text => "value" & preamble & to_string (unit.value.position), 
							level => log_threshold + 2);


					-- PURPOSE
					-- Rotate the placeholder around its own anchor point so that it
					-- it is readable from the front or from the right.
					unit.purpose.rotation := snap (unit.purpose.rotation + rot);

					-- rotate the placeholder anchor point around the symbol origin:
					rotate_by (unit.purpose.position, rot);

					log (text => "purpose" & preamble & to_string (unit.purpose.position), 
							level => log_threshold + 2);

				end rotate_placeholders_relative;

			begin -- rotate_unit
				case coordinates is
					when ABSOLUTE =>
						set (unit.position, rotation);
						rotate_placeholders_absolute (rotation);
						
					when RELATIVE =>
						set (unit.position, add (rotation_before, rotation));
						
						log (text => "rotation now" & to_string (rot (unit.position)),
								level => log_threshold + 1);

						rotate_placeholders_relative (rotation);
				end case;
			end rotate_unit;
			
		begin -- query_units
			if contains (device.units, unit_name) then
				-- locate unit by its name
				unit_cursor := find (device.units, unit_name);

				-- load unit position and current rotation
				position_of_unit := element (unit_cursor).position;
				rotation_before := rot (element (unit_cursor).position);

				-- log unit position and current rotation
				log (text => to_string (position => position_of_unit) &
						" rotation before" & to_string (rotation_before),
						level => log_threshold + 1);

				log_indentation_up;
				
				type_units.update_element (
					container	=> device.units,
					position	=> unit_cursor,
					process		=> rotate_unit'access);

				log_indentation_down;
			else
				unit_not_found (unit_name);
			end if;
		end query_units;

	begin -- query_devices
		if contains (module.devices, device_name) then

			-- Before the rotation, the coordinates of the
			-- unit must be fetched. These coordinates will later assist
			-- in deleting the port names from connected net segments.
			device_cursor := find (module.devices, device_name); -- the device should be there

			-- rotate the unit
			update_element (
				container	=> module.devices,
				position	=> device_cursor,
				process		=> query_units'access);
			
			log_indentation_up;

			-- Fetch the ports of the unit to be rotated.
			-- The coordinates here are the default positions (in the library model)
			-- relative to the center of the units.
			ports_lib := ports_of_unit (device_cursor, unit_name);
			
			ports_scratch := ports_lib;						 

			-- Calculate the absolute positions of the unit ports in the schematic
			-- as they are BEFORE the rotation:
			rotate_ports (ports_scratch, rotation_before);
			move_ports (ports_scratch, position_of_unit);
			
			-- Delete the old ports of the targeted unit from module.nets.
			-- The unit is on a certain sheet. The procedure delete_ports however
			-- requires a list of unit positions (containing sheet numbers).
			-- So we create a list "sheets", put the unit name and position in it,
			-- and pass it to procedure delete_ports:
			declare
				sheets : type_unit_positions.map;
			begin
				type_unit_positions.insert (
					container	=> sheets,
					key			=> unit_name,
					new_item	=> position_of_unit);

				delete_ports (
					module			=> module_cursor,
					device			=> device_name,
					ports			=> ports_scratch,
					sheets			=> sheets, 
					log_threshold	=> log_threshold + 1);
			end;

			-- Calculate the new positions of the unit ports.
			case coordinates is
				when ABSOLUTE =>
					rotate_ports (ports_lib, rotation);
				when RELATIVE =>
						-- The given angle of rotation adds to the rotation_before:
					rotate_ports (ports_lib, add (rotation_before, rotation));
			end case;
			
			move_ports (ports_lib, position_of_unit);
			
			-- Insert the new unit ports in the nets (type_module.nets):
			insert_ports (
				module			=> module_cursor,
				device			=> device_name,
				unit			=> unit_name,
				ports			=> ports_lib,
				sheet			=> et_coordinates.sheet (position_of_unit),
				log_threshold	=> log_threshold + 1);
			
			log_indentation_down;				
		else
			device_not_found (device_name);
		end if;
	end query_devices;
	
begin -- rotate_unit
	case coordinates is
		when ABSOLUTE =>
			log (text => "module " & to_string (module_name) &
				" rotating " & to_string (device_name) & " unit " & 
				to_string (unit_name) & " to" & to_string (rotation), level => log_threshold);

		when RELATIVE =>
			if rotation in type_rotation_relative then
				log (text => "module " & to_string (module_name) &
					" rotating " & to_string (device_name) & " unit " & 
					to_string (unit_name) & " by" & to_string (rotation), level => log_threshold);
			else
				relative_rotation_invalid;
			end if;
	end case;
	
	-- locate module
	module_cursor := locate_module (module_name);
	
	update_element (
		container	=> generic_modules,
		position	=> module_cursor,
		process		=> query_devices'access);

end rotate_unit;


-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           BOARD OPERATIONS                               --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.unbounded;
with ada.text_io;				use ada.text_io;

with ada.exceptions;
with ada.directories;
-- with gnat.directory_operations;

with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;
-- with ada.containers.indefinite_ordered_sets;

with et_general;				use et_general;
-- with et_coordinates;
with et_string_processing;		use et_string_processing;
-- with et_libraries;				use et_libraries;
with et_schematic;				use et_schematic;
with et_pcb;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_project;				use et_project;
with schematic_ops;				use schematic_ops;
with assembly_variants;
with pick_and_place;
--with submodules;
-- with numbering;
-- with conventions;
-- with material;
-- with netlists;
-- with et_geometry;

package body board_ops is

	procedure move_device (
	-- Moves a device in the board layout in x/y direction.
	-- Leaves rotation and face (top/bottom) as it is.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		coordinates		: in type_coordinates; -- relative/absolute		
		point			: in et_pcb_coordinates.type_point_2d; -- x/y
		log_threshold	: in type_log_level) is

		use et_project.type_modules;
		module_cursor : type_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;

			procedure set_position (
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) is
			begin
				case coordinates is
					when ABSOLUTE =>
						set_xy (point => device.position, position => point); -- preserve angle and face

					when RELATIVE =>
						move_point (point => device.position, offset => point); -- preserve angle and face
						
				end case;
			end;
			
		begin -- query_devices
			if contains (module.devices, device_name) then

				device_cursor := find (module.devices, device_name); -- the device should be there

				-- set new position
				update_element (
					container	=> module.devices,
					position	=> device_cursor,
					process		=> set_position'access);

			else
				device_not_found (device_name);
			end if;
		end query_devices;
		
	begin -- move_device
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" moving " & to_string (device_name) &
					" to" & to_string (point), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" moving " & to_string (device_name) &
					" by" & to_string (point), level => log_threshold);
		end case;

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end move_device;

	procedure rotate_device (
	-- Rotates a device in the board layout.
	-- Leaves x/y and face (top/bottom) as it is.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		coordinates		: in type_coordinates; -- relative/absolute		
		rotation		: in et_pcb_coordinates.type_angle; -- 90
		log_threshold	: in type_log_level) is

		use et_project.type_modules;
		module_cursor : type_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;

			procedure set_rotation (
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) is
			begin
				case coordinates is
					when ABSOLUTE =>
						set_angle (point => device.position, value => rotation); -- preserve x/y and face

					when RELATIVE =>
						rotate (point => device.position, rotation => rotation); -- preserve x/y and face
				end case;
			end;
			
		begin -- query_devices
			if contains (module.devices, device_name) then

				device_cursor := find (module.devices, device_name); -- the device should be there

				-- set new position
				update_element (
					container	=> module.devices,
					position	=> device_cursor,
					process		=> set_rotation'access);

			else
				device_not_found (device_name);
			end if;
		end query_devices;
		
	begin -- rotate_device
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" rotating " & to_string (device_name) &
					" to" & to_string (rotation), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" rotating " & to_string (device_name) &
					" by" & to_string (rotation), level => log_threshold);
		end case;

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end rotate_device;

	procedure flip_device (
	-- Flips a device in the board layout from top to bottom or vice versa.
	-- Leaves x/y and rotation as it is.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		face			: in type_face; -- top/bottom
		log_threshold	: in type_log_level) is

		use et_project.type_modules;
		module_cursor : type_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;

			procedure flip (
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) is
			begin
				set_face (position => device.position, face => face); -- preserve x/y and rotation
			end;
			
		begin -- query_devices
			if contains (module.devices, device_name) then

				device_cursor := find (module.devices, device_name); -- the device should be there

				-- set new position
				update_element (
					container	=> module.devices,
					position	=> device_cursor,
					process		=> flip'access);

			else
				device_not_found (device_name);
			end if;
		end query_devices;
		
	begin -- flip_device
		log (text => "module " & to_string (module_name) &
			" flipping " & to_string (device_name) &
			" to" & to_string (face), level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end flip_device;

	procedure make_pick_and_place (
	-- Exports a pick & place file from the given top module and assembly variant.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		variant_top		: in assembly_variants.type_variant_name.bounded_string; -- low_cost
		pnp_file		: in pick_and_place.type_file_name.bounded_string; -- CAM/motor_driver_bom.pnp
		log_threshold	: in type_log_level) is
		use pick_and_place;
		use assembly_variants;

		-- Here we collect the pick and place data in the first step. It will then
		-- be passed to procedure pick_and_place.write_pnp.
		pnp : pick_and_place.type_devices.map;

		use et_project.type_modules;
		module_cursor : type_modules.cursor; -- points to the module being modified

		procedure query_devices (
		-- Collects devices which are of appearance SCH_PCB. Virtual devices of
		-- appearance SCH are skipped (like GND symbols).
		-- If the package is virtual, then the device will also be skipped.
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor := module.devices.first;
		
		begin -- query_devices
			while device_cursor /= et_schematic.type_devices.no_element loop

				-- the device must be real (appearance SCH_PCB)
				if element (device_cursor).appearance = SCH_PCB then

					-- the package must be real
					if has_real_package (device_cursor) then
						
						pick_and_place.type_devices.insert 
							(
							container	=> pnp,
							key			=> key (device_cursor),
							new_item	=> (
									position => element (device_cursor).position
									-- CS value, package, partcode ?
									)
							);
						
					end if;
						
				end if;
				
				next (device_cursor);
			end loop;
			
		end query_devices;

		
	begin -- make_pick_and_place
		-- The variant name is optional. If not provided, the default variant will be exported.
		if assembly_variants.is_default (variant_top) then
			log (text => "module " & enclose_in_quotes (to_string (module_name)) &
				" default variant" &
				" exporting pick & place data to file " & to_string (pnp_file),
				level => log_threshold);
		else
			log (text => "module " & enclose_in_quotes (to_string (module_name)) &
				" variant " & enclose_in_quotes (to_variant (variant_top)) &
				" exporting pick & place data to file " & to_string (pnp_file),
				level => log_threshold);
		end if;

		-- locate module
		module_cursor := locate_module (module_name);
		
		query_element (
			position	=> module_cursor,
			process		=> query_devices'access);
		
		pick_and_place.write_pnp (
			pnp				=> pnp,
			file_name		=> pnp_file,
			log_threshold	=> log_threshold + 1);
		
	end make_pick_and_place;

	
	function terminal_position (
	-- Returns the coordinates of a terminal.
		module_cursor	: in et_project.type_modules.cursor;
		device_cursor	: in et_schematic.type_devices.cursor; -- IC45
		terminal_cursor	: in et_pcb.type_terminals.cursor) -- H7, 14
		return type_terminal_position is
		use et_pcb;
		pos : type_terminal_position (SMT); -- to be returned
	begin
		return pos; -- CS
	end terminal_position;
	
end board_ops;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     SCHEMATIC OPERATIONS / DEVICE                        --
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
-- - clean up, rework
--

with ada.text_io;				use ada.text_io;

with et_string_processing;		use et_string_processing;
with et_module;					use et_module;

with et_sheets;					use et_sheets;
with et_schematic_coordinates;	use et_schematic_coordinates;
with et_schematic_ops.units;	use et_schematic_ops.units;
with et_exceptions;				use et_exceptions;

with et_unit_name;				use et_unit_name;
with et_units;					use et_units;

with et_device_category;		use et_device_category;
with et_devices_electrical;		use et_devices_electrical;

with et_conventions;			use et_conventions;


package body et_schematic_ops_device is


	
-- 	
-- 	procedure device_not_found (name : in type_device_name) is begin
-- 		raise semantic_error_1 
-- 			with "ERROR: Device " & to_string (name) & " not found !";
-- 	end device_not_found;
-- 
-- 	
-- 	procedure device_already_exists (name : in type_device_name) is begin
-- 		raise semantic_error_1
-- 			with "ERROR: Device " & to_string (name) & " already exists !";
-- 	end device_already_exists;
-- 
-- 	


	function sort_by_coordinates_2 (
		module_cursor 	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level) 
		return et_numbering.pac_devices.map 
	is
		use et_numbering;
		devices : et_numbering.pac_devices.map; -- to be returned

		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is

			procedure query_units (device_cursor : in pac_devices_electrical.cursor) is
				use pac_units;
				device_name : type_device_name := pac_devices_electrical.key (device_cursor); -- R1

				
				procedure sort (
					unit_cursor : in pac_units.cursor) 
				is 
					unit_name : pac_unit_name.bounded_string := key (unit_cursor);  -- 1, C, IO_BANK1
					unit_position : type_object_position := element (unit_cursor).position;
					inserted : boolean := false;
					cursor_sort : et_numbering.pac_devices.cursor;

					use pac_unit_name;
				begin
					log (text => "unit " & to_string (unit_name) &
						" at " & to_string (position => unit_position),
						 level => log_threshold + 2);
					
					et_numbering.pac_devices.insert 
						(
						container	=> devices,
						key			=> unit_position, -- sheet/x/y
						inserted	=> inserted,
						position	=> cursor_sort,
						new_item	=> (
									name => device_name, -- R1, IC3
									unit => unit_name, -- 1, C, IO_BANK1
									done => false -- not renumbered yet
									)
						);

					if not inserted then
						log (ERROR, "device " & to_string (device_name) &
							 " unit " & to_string (unit_name) &
							 " at " & to_string (position => unit_position) &
							 " sits on top of another unit !",
							 console => true);
						raise constraint_error;
					end if;							 
				end sort;

				
			begin				
				log (text => "device " & to_string (device_name), -- R1, IC3
					 level => log_threshold + 1);
				
				log_indentation_up;
				
				pac_units.iterate (
					container	=> pac_devices_electrical.element (device_cursor).units,
					process		=> sort'access);

				log_indentation_down;
			end query_units;
			
			
		begin -- query_devices
			pac_devices_electrical.iterate (
				container	=> module.devices,
				process		=> query_units'access);
		end query_devices;

		
	begin -- sort_by_coordinates_2
		log (text => "sorting devices/units by schematic coordinates ...", level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_devices'access);


		log_indentation_down;
		
		return devices;
	end sort_by_coordinates_2;



	




	
	-- Renumbers devices according to the sheet number.
	procedure renumber_devices (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		step_width		: in type_name_index;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_device_category;
		use et_conventions;
		use et_numbering;
		use pac_unit_name;
		

		-- The list of devices sorted by their coordinates.
		-- By their order in this list the devices will be renumbered.
		devices : et_numbering.pac_devices.map;

		
		-- Renumbers devices of given category. Returns true if all devices
		-- have been renamed.
		-- Marks every renamed unit in the device list so that the second
		-- run of this function does not try to renumber them again.
		function renumber (
			cat : in type_device_category) 
			return boolean 
		is
			result : boolean := true;
			
			use et_numbering.pac_devices;
			cursor : et_numbering.pac_devices.cursor := devices.first;
			name_before, name_after : type_device_name; -- R1
			sheet_before, sheet_now : type_sheet := type_sheet'first;

			index_on_sheet : type_name_index := type_name_index'first;
			device_index : type_name_index;

			
			-- Detects when the sheet number changes. In this case
			-- resets the index_on_sheet so that the indexing starts anew.
			procedure update_index is begin
				sheet_now := get_sheet (key (cursor));

				if sheet_now = sheet_before then -- no change
					index_on_sheet := index_on_sheet + 1;
				else -- sheet has changed
					sheet_before := sheet_now;
					index_on_sheet := type_name_index'first + 1;
				end if;
			end update_index;


			
			-- Sets the "done" flag of all devices with name_before in the device list.
			procedure mark_units_done is

				-- Start with the unit indicated by cursor. All other
				-- units of the device come after this one.
				cursor_done : et_numbering.pac_devices.cursor := cursor;

				procedure set_done (
					coordinates : in type_object_position;
					device		: in out et_numbering.type_device) is
				begin
					device.done := true;
				end;

				
			begin -- mark_units_done
				log (text => "marking all units done ...", level => log_threshold + 2);
				
				while cursor_done /= et_numbering.pac_devices.no_element loop
					
					if element (cursor_done).name = name_before then -- IC5
						
						log (text => " unit " & to_string (element (cursor_done).unit),
							 level => log_threshold + 2);

						update_element (
							container	=> devices,
							position	=> cursor_done,
							process		=> set_done'access);
						
					end if;
					
					next (cursor_done);
				end loop;
			end mark_units_done;


			
		begin -- renumber
			while cursor /= et_numbering.pac_devices.no_element loop

				if not element (cursor).done then
					name_before := element (cursor).name; -- R1

					-- If the current device is in given category:
					if category (name_before) = cat then

						log (text => "device " & to_string (name_before) &
							" unit " & to_string (element (cursor).unit), level => log_threshold +1);
						log_indentation_up;
						
						update_index;
						
						-- step width times sheet number: like 100 * 4 = 400
						device_index := step_width * type_name_index (sheet_now);

						-- 400 plus index on sheet: like 407
						device_index := device_index + index_on_sheet;

						-- build the new device name
						name_after := to_device_name (
								prefix	=> get_prefix (name_before), -- R, C, IC
								index 	=> device_index); -- 407

						-- Do the renaming if the new name differs from the old name.
						-- If the renaming fails, set result false. Result remains false
						-- even if other renamings succeed.
						if name_after /= name_before then

							null;
							-- CS
							-- if rename_device (
							-- 	module_cursor		=> module_cursor,
							-- 	device_name_before	=> name_before, -- R1
							-- 	device_name_after	=> name_after, -- R407
							-- 	log_threshold		=> log_threshold + 2) then
       -- 
							-- 	-- Mark all units of the device as done:
							-- 	mark_units_done;
							-- else
							-- 	result := false;
							-- end if;
							
						end if;

						log_indentation_down;
					end if;
				end if;
				
				next (cursor);
			end loop;

			return result;

			exception when event:
				others => 
					log (text => ada.exceptions.exception_message (event), console => true);
				raise;
			
		end renumber;


		
	begin -- renumber_devices
		log (text => "module " & to_string (module_name) &
			" renumbering devices." &
			" step width per sheet" & to_string (step_width),
			level => log_threshold);

		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);

		-- Get a list of devices and units where they are sorted by their coordinates.
		devices := sort_by_coordinates_2 (module_cursor, log_threshold + 2);

		-- Renumber for each device category. If the first run fails, start another
		-- iteration. If that fails too, issue error and abort.
		-- Devices of unknown category are exampted from renumbering.
		for cat in type_device_category'pos (type_device_category'first) .. 
			type_device_category'pos (type_device_category'last) loop

			case type_device_category'val (cat) is
				when UNKNOWN => null;
				
				when others =>

					log (text => "category" & to_string (type_device_category'val (cat)),
						 level => log_threshold + 1);

					log_indentation_up;
					if renumber (type_device_category'val (cat)) = false then
						-- first iteration failed. start a second:
						
						log (text => "another iteration required", level => log_threshold + 2);
						log_indentation_up;
						
						if renumber (type_device_category'val (cat)) = false then
							-- second iteration failed: abort
							log (ERROR, "renumbering failed !", console => true);
							raise constraint_error;
						end if;
						
						log_indentation_down;
					end if;

					log_indentation_down;
			end case;
		end loop;

		log_indentation_down;
	end renumber_devices;

	
	

end et_schematic_ops_device;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    COMMAND PROCESSOR / BOARD / DEVICE                    --
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
-- - rework
--
--

with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;
with ada.strings; 						use ada.strings;

with et_exceptions;						use et_exceptions;
with et_runmode;						use et_runmode;
with et_modes.board;					use et_modes.board;
with et_keywords;						use et_keywords;
with et_module;							use et_module;
with et_module_names;					use et_module_names;
with et_board_geometry;					use et_board_geometry;
with et_board_coordinates;				use et_board_coordinates;
with et_design_rules_board;
with et_device_name;					use et_device_name;
with et_unit_name;						use et_unit_name;
with et_schematic_ops.units;			use et_schematic_ops.units;
with et_board_ops.devices;				use et_board_ops.devices;
with et_devices_electrical;				use et_devices_electrical;
with et_device_property_level;			use et_device_property_level;
with et_canvas_board;
with et_device_prefix;					use et_device_prefix;
with et_package_model_name;				use et_package_model_name;
with et_pcb_sides;						use et_pcb_sides;


package body et_cp_board_device is

	use pac_generic_modules;
	use pac_geometry_2;


	
	procedure show_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);

			
		-- The degree of how much information is to be inqured:
		properties_level : type_properties_level;


		procedure runmode_module is 
			use et_canvas_board;
			use pac_canvas;
		-- CS: Clean up. Move stuff in separate procedures.
			
			device_name : type_device_name;
			error : boolean := false;
		begin
			case cmd_field_count is
				when 6 => 
					-- show device L1 R1
					properties_level := to_properties_level (get_field (cmd, 5), error); -- L1
					
					if not error then						
						-- Get the device name:
						device_name := to_device_name (get_field (cmd, 6)); -- R1, IC1, FD1

						-- Search among the electrical devices first.
						-- Highlight the device and all its units if it
						-- exists.
						-- If it does not exist, then search among the non-electrical
						-- devices:

						-- We do not want to generate warnings in case the device
						-- does not exist. For this reason log_warning is false.
						-- Instead we generate a warning if the device is not among
						-- the electrical nor the non-electrical devices.

						-- CS:
						-- Center on the device and leave the
						-- zoom factor as it is. If the runmode is
						-- headless, then nothing happens here:
						-- zoom_to (get_place (unit_query.position), S);

						
						show_device (
							module_cursor	=> module, 
							device_name		=> device_name,
							all_units		=> true,
							unit_name		=> unit_name_default,
							error			=> error,
							log_warning		=> false, 
							log_threshold	=> log_threshold + 1);

						if not error then
							-- Show some basic information in the staus bar:
							set_status (et_schematic_ops.units.get_device_properties (
								module_cursor	=> module, 
								device_name		=> device_name, 
								level			=> DEVICE_PROPERTIES_LEVEL_1,
								error			=> error,
								log_threshold	=> log_threshold + 1));

							-- For property levels greater 1 we open
							-- the properties window in order to conveniently
							-- show a lot of information:
							case properties_level is
								when DEVICE_PROPERTIES_LEVEL_1 => null;

								when others =>
									
									pac_device_ops.show_properties_window (
										device	=> device_name,
										text	=> et_schematic_ops.units.get_device_properties (
											module_cursor	=> module, 
											device_name		=> device_name, 
											linebreaks		=> true,
											level			=> properties_level,
											error			=> error,
											log_threshold	=> log_threshold + 2));
							end case;

							
						end if;
						
						-- If the device could not be located among the
						-- electrical devices, then search
						-- among non-electrical devices:
						if error then

							-- CS:
							-- Center on the device and leave the
							-- zoom factor as it is. If the runmode is
							-- headless, then nothing happens here:
							-- zoom_to (get_place (unit_query.position), S);
							
							show_non_electrical_device (
								module_cursor	=> module, 
								device_name		=> device_name,
								error			=> error,
								log_warning		=> false, 
								log_threshold	=> log_threshold + 1);

							-- Write some basic information in the status bar:
							if not error then
								set_status (et_board_ops.devices.get_device_properties (
									module_cursor	=> module,
									device_name		=> device_name, 
									level			=> DEVICE_PROPERTIES_LEVEL_1,
									error			=> error,
									log_threshold	=> log_threshold + 1));


								-- For property levels greater 1 we open
								-- the properties window in order to conveniently
								-- show a lot of information:
								case properties_level is
									when DEVICE_PROPERTIES_LEVEL_1 => null;

									when others =>
										
										pac_device_ops.show_properties_window (
											device	=> device_name,
											text	=> et_board_ops.devices.get_device_properties (
												module_cursor	=> module, 
												device_name		=> device_name, 
												linebreaks		=> true,
												level			=> properties_level,
												error			=> error,
												log_threshold	=> log_threshold + 2));
								end case;

							end if;
							
							if error then
								log (WARNING, "Device " 
									& to_string (device_name) & " not found !");
							end if;
						end if;

				end if;
				
				when 7 .. type_field_count'last => 
					command_too_long (cmd, cmd_field_count - 1);
					
				when others => command_incomplete (cmd);
			end case;
		end runmode_module;

		
	begin
		-- CS log message
		
		-- Show operations are only useful and possible in graphical
		-- runmode:
		case runmode is
			when MODE_MODULE =>
				runmode_module;

			when others =>
				skipped_in_this_runmode (log_threshold + 1);
					
		end case;
	end show_device;




	


	procedure add_non_electrical_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);


		procedure do_it is
			
			model : constant pac_package_model_file.bounded_string := 
				to_package_model_name (get_field (cmd, 5));
			
			prefix : constant pac_device_prefix.bounded_string := 
				to_prefix (get_field (cmd, 6));

			xy : constant type_vector_model := type_vector_model (set (
					x => to_distance (dd => get_field (cmd, 7)),
					y => to_distance (dd => get_field (cmd, 8))));

		begin
			case cmd_field_count is
				when 8 =>
					add_non_electrical_device (
						module_cursor	=> active_module,
						package_model	=> model,
						position		=> to_package_position
							(
							point	=> xy
							),
						prefix			=> prefix,
						log_threshold	=> log_threshold + 1);

					
				when 9 =>
					add_non_electrical_device (
						module_cursor	=> active_module,
						package_model	=> model,
						position		=> to_package_position
							(
							point		=> xy,
							rotation	=> to_rotation (get_field (cmd, 9))
							),
						prefix			=> prefix,
						log_threshold	=> log_threshold + 1);

					
				when 10 =>
					add_non_electrical_device (
						module_cursor	=> active_module,
						package_model	=> model,
						position		=> to_package_position
							(
							point		=> xy,
							rotation	=> to_rotation (get_field (cmd, 9)),
							face		=> to_face (get_field (cmd, 10))
							),
						prefix			=> prefix,
						log_threshold	=> log_threshold + 1);
					
				when others => raise constraint_error; -- CS should never happen
			end case;
		end do_it;

		

	begin
		-- CS log message

		case cmd_field_count is
			when 8..10 => do_it;
			-- board led_driver add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac 5 5
			-- board led_driver add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac 5 5 0
			-- board led_driver add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac 5 5 0 top

			when 11 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
			
			when others => command_incomplete (cmd);
		end case;
	end add_non_electrical_device;

	
	
end et_cp_board_device;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

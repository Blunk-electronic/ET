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

--   For correct displaying set tab with in your editor to 4.

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
-- - propose arguments if command incomplete
-- - test existence of targeted device
-- - set exit code if targeted object does not exist
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
with et_board_ops_groups;
with et_design_rules_board;

with et_device_name;					use et_device_name;
with et_unit_name;						use et_unit_name;
with et_schematic_ops_device;			use et_schematic_ops_device;
with et_schematic_ops_groups;
with et_board_ops_devices;				use et_board_ops_devices;
with et_devices_electrical;				use et_devices_electrical;
with et_device_property_level;			use et_device_property_level;
with et_canvas_board;
with et_device_prefix;					use et_device_prefix;
with et_package_model_name;				use et_package_model_name;
with et_pcb_sides;						use et_pcb_sides;
with et_coordinates_abs_rel;			use et_coordinates_abs_rel;

with et_device_placeholders;
with et_device_placeholders.packages;



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


		procedure preprocess_command is 
			use et_canvas_board;
			use pac_canvas;
		-- CS: Clean up. Move stuff in separate procedures.
			
			device_name : type_device_name;
			error : boolean := false;

			
			procedure show_electrical_device is
				-- CS:
				-- Center on the device and leave the
				-- zoom factor as it is. If the runmode is
				-- headless, then nothing happens here:
				-- zoom_to (get_place (unit_query.position), S);
			begin		
				log (text => "show_electrical_device", level => log_threshold + 1);
				log_indentation_up;
				
				show_device (
					module_cursor	=> module, 
					device_name		=> device_name,
					all_units		=> true,
					unit_name		=> unit_name_default,
					log_threshold	=> log_threshold + 2);


				-- Show some basic information in the staus bar:
				set_status (et_schematic_ops_device.get_device_properties (
					module_cursor	=> module, 
					device_name		=> device_name, 
					level			=> DEVICE_PROPERTIES_LEVEL_1,
					error			=> error,
					log_threshold	=> log_threshold + 2));

				-- For property levels greater 1 we open
				-- the properties window in order to conveniently
				-- show a lot of information:
				case properties_level is
					when DEVICE_PROPERTIES_LEVEL_1 => null;

					when others =>
						
						pac_device_ops.show_properties_window (
							device	=> device_name,
							text	=> et_schematic_ops_device.get_device_properties (
								module_cursor	=> module, 
								device_name		=> device_name, 
								linebreaks		=> true,
								level			=> properties_level,
								error			=> error,
								log_threshold	=> log_threshold + 2));
				end case;

				log_indentation_down;
			end show_electrical_device;



			procedure show_non_electrical_device is
			begin
				log (text => "show_non_electrical_device", level => log_threshold + 1);
				log_indentation_up;

				-- Proceed if the non-electrical device exists:
				if non_electrical_device_exists (module, device_name) then
					-- CS:
					-- Center on the device and leave the
					-- zoom factor as it is. If the runmode is
					-- headless, then nothing happens here:
					-- zoom_to (get_place (unit_query.position), S);
					
					show_non_electrical_device (
						module_cursor	=> module, 
						device_name		=> device_name,
						log_threshold	=> log_threshold + 2);

					
					-- Write some basic information in the status bar:
					if not error then
						set_status (et_board_ops_devices.get_device_properties (
							module_cursor	=> module,
							device_name		=> device_name, 
							level			=> DEVICE_PROPERTIES_LEVEL_1,
							error			=> error,
							log_threshold	=> log_threshold + 2));


						-- For property levels greater 1 we open
						-- the properties window in order to conveniently
						-- show a lot of information:
						case properties_level is
							when DEVICE_PROPERTIES_LEVEL_1 => null;

							when others =>
								
								pac_device_ops.show_properties_window (
									device	=> device_name,
									text	=> et_board_ops_devices.get_device_properties (
										module_cursor	=> module, 
										device_name		=> device_name, 
										linebreaks		=> true,
										level			=> properties_level,
										error			=> error,
										log_threshold	=> log_threshold + 2));
						end case;
					end if;

				else
					message_device_not_found (SEVERITY_ERROR, device_name);
				end if;

				log_indentation_down;
			end show_non_electrical_device;

			
			
		begin
			case cmd_field_count is
				when 6 => 
					-- show device L1 R1
					properties_level := to_properties_level (get_field (cmd, 5), error); -- L1
					
					if not error then						
						-- Get the device name:
						device_name := to_device_name (get_field (cmd, 6)); -- R1, IC1, FD1

						-- Search among the electrical devices first.
						-- Highlight the device and all its units (in the schematic)
						-- if it exists.
						-- If it does not exist, then search among the non-electrical
						-- devices:
						if electrical_device_exists (module, device_name) then
							show_electrical_device;
						else
							-- If the device could not be located among the
							-- electrical devices, then search
							-- among non-electrical devices:
							show_non_electrical_device;
						end if;
					end if;

					
				when 7 .. type_field_count'last => 
					command_too_long (cmd, cmd_field_count - 1);
					
				when others => command_incomplete (cmd);
			end case;
		end preprocess_command;

		
	begin
		log (text => "show device", level => log_threshold);
		log_indentation_up;
		
		-- Show operations are only useful and possible in graphical
		-- runmode. So we start preprocessing the given command
		-- only in graphical runmode:
		case runmode is
			when MODE_MODULE =>

				-- Deselect all objects in the schematic
				-- and board drawing. This is required in case
				-- the specified device does not exist.
				-- It is redundant in case the specified device
				-- does exist. The reset would be executed twice,
				-- the first time here and the second time
				-- by procedure show_non_electrical_device in 
				-- package et_board_ops_device:
				et_schematic_ops_groups.reset_objects (
					module, log_threshold + 1);
					
				et_board_ops_groups.reset_objects (
					module, log_threshold + 1);
				
				preprocess_command;

			when others =>
				skipped_in_this_runmode (log_threshold + 1);
					
		end case;

		log_indentation_down;
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
		log (text => "add non-electrical device", level => log_threshold);
		log_indentation_up;


		case cmd_field_count is
			when 8..10 => do_it;
			-- board led_driver add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac 5 5
			-- board led_driver add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac 5 5 0
			-- board led_driver add device $HOME/git/BEL/ET_component_library/packages/fiducials/crosshair_4.pac 5 5 0 top

			when 11 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
			
			when others => command_incomplete (cmd);
		end case;

		log_indentation_down;
	end add_non_electrical_device;









	procedure delete_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);

		
		procedure do_it is 
			use et_board_ops_devices;
			device_name : type_device_name := to_device_name (get_field (cmd, 5));
		begin
			-- Proceed if the specified non-electrical
			-- device exists:
			if non_electrical_device_exists (module, device_name) then
				
				delete_non_electrical_device (
					module_cursor	=> active_module,
					device_name		=> device_name,
					log_threshold	=> log_threshold + 1);

			else
				message_device_not_found (SEVERITY_ERROR, device_name);
			end if;
		end do_it;
		

	begin
		log (text => "delete non-electrical device", level => log_threshold);
		log_indentation_up;

		case cmd_field_count is
			when 5 => do_it;				
			
			when 6 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
			
			when others => 
				command_incomplete (cmd);
		end case;		

		log_indentation_down;
	end delete_device;

		


	


	



	procedure copy_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);


		procedure do_it is
			device_name : type_device_name;
			destination : type_vector_model;
		begin
			device_name := to_device_name (get_field (cmd, 5));
			destination := to_vector_model (get_field (cmd, 6), get_field (cmd, 7)); -- x/y

			-- Proceed if the specified non-electrical
			-- device exists:
			if non_electrical_device_exists (module, device_name) then
				
				et_board_ops_devices.copy_non_electrical_device (
					module_cursor 	=> module,
					device_name		=> device_name,
					destination		=> destination,
					log_threshold	=> log_threshold + 1);

			else
				message_device_not_found (SEVERITY_ERROR, device_name);
			end if;
		end do_it;
		
		
	begin
		log (text => "copy non-electrical device", level => log_threshold);
		log_indentation_up;


		case cmd_field_count is
			when 7 =>
				do_it;

			when 8 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;

		
		log_indentation_down;
	end copy_device;






	


	procedure move_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);


		procedure do_it is
			device_name : type_device_name;
			coordinates	: type_coordinates;
		begin
			device_name := to_device_name (get_field (cmd, 5)); -- IC1
			coordinates := to_coordinates (get_field (cmd, 6));  -- relative/absolute

			-- Proceed if the specified device exists.
			-- It can be an electrical or a non-electrical device:
			if electrical_device_exists (module, device_name) 
			or non_electrical_device_exists (module, device_name) then
				
				et_board_ops_devices.move_device (
					module_cursor 	=> module,
					device_name		=> device_name,
					coordinates		=> coordinates,
					point			=> type_vector_model (set (
										x => to_distance (dd => get_field (cmd, 7)),
										y => to_distance (dd => get_field (cmd, 8)))),
					log_threshold	=> log_threshold + 1);

			else
				message_device_not_found (SEVERITY_ERROR, device_name);
			end if;
		end do_it;

		
	begin
		log (text => "move device", level => log_threshold);
		log_indentation_up;
		
		case cmd_field_count is
			when 8 =>
				do_it;				

			when 9 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others =>
				command_incomplete (cmd);
		end case;

		log_indentation_down;
	end move_device;

	




	


	procedure rotate_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		


		-- This procedure rotates the specified device 
		-- by 90 degrees ccw:
		procedure do_rotate_1 is
			device_name : type_device_name;
		begin
			device_name := to_device_name (get_field (cmd, 5)); -- IC1

			-- Proceed if the specified device exists.
			-- It can be an electrical or a non-electrical device:
			if electrical_device_exists (module, device_name) 
			or non_electrical_device_exists (module, device_name) then

				et_board_ops_devices.rotate_device (
					module_cursor 	=> module,
					device_name		=> device_name,
					coordinates		=> RELATIVE,
					log_threshold	=> log_threshold + 1);

			else
				message_device_not_found (SEVERITY_ERROR, device_name);
			end if;
		end do_rotate_1;


		
		-- This procedure rotates the specified device 
		-- by the specified angle:
		procedure do_rotate_2 is
			device_name : type_device_name;
			coordinates	: type_coordinates;
			rotation	: type_rotation_model;
		begin
			device_name := to_device_name (get_field (cmd, 5)); -- IC1
			coordinates := to_coordinates (get_field (cmd, 6));  -- relative/absolute
			rotation	:= to_rotation (get_field (cmd, 7));

			-- Proceed if the specified device exists.
			-- It can be an electrical or a non-electrical device:
			if electrical_device_exists (module, device_name) 
			or non_electrical_device_exists (module, device_name) then
			
				et_board_ops_devices.rotate_device (
					module_cursor 	=> module,
					device_name		=> device_name,
					coordinates		=> coordinates,
					rotation		=> rotation,
					log_threshold	=> log_threshold + 1);

			else
				message_device_not_found (SEVERITY_ERROR, device_name);
			end if;
		end do_rotate_2;


		
	begin
		log (text => "rotate device", level => log_threshold);
		log_indentation_up;

		
		case cmd_field_count is
			when 5 =>
				do_rotate_1;

			when 7 =>
				do_rotate_2;

			when 8 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others =>
				command_incomplete (cmd);
		end case;

		
		log_indentation_down;
	end rotate_device;





	



	procedure rename_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		
		procedure do_it is
			device_name : type_device_name;
		begin
			device_name := to_device_name (get_field (cmd, 5));
			
			-- Proceed if the specified non-electrical device exists:
			if non_electrical_device_exists (module, device_name) then
			
				rename_non_electrical_device (
					module_cursor		=> active_module,
					device_name_before	=> device_name,
					device_name_after	=> to_device_name (get_field (cmd, 6)),
					log_threshold		=> log_threshold + 1);

			else
				message_device_not_found (SEVERITY_ERROR, device_name);
			end if;
		end do_it;

		
	begin
		log (text => "rename non-electrical device", level => log_threshold);
		log_indentation_up;

		case cmd_field_count is
			when 6 => do_it; 

			when 7 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
			
			when others => command_incomplete (cmd);
		end case;
		
		log_indentation_down;
	end rename_device;







	



	procedure flip_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is 
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
		
		
		-- This procedure toggles the face of the specified device:
		procedure do_flip_1 is
			device_name : type_device_name;
		begin
			device_name := to_device_name (get_field (cmd, 5)); -- IC1
			
			-- Proceed if the specified device exists.
			-- It can be an electrical or a non-electrical device:
			if electrical_device_exists (module, device_name) 
			or non_electrical_device_exists (module, device_name) then

				et_board_ops_devices.flip_device (
					module_cursor 	=> active_module,
					device_name		=> device_name,
					toggle			=> true,
					log_threshold	=> log_threshold + 1);

			else
				message_device_not_found (SEVERITY_ERROR, device_name);
			end if;
		end do_flip_1;
		
		
		
		-- This procedure sets the face of the specified device:
		procedure do_flip_2 is
			device_name : type_device_name;
			face : type_face;
		begin
			device_name := to_device_name (get_field (cmd, 5)); -- IC1
			face := to_face (get_field (cmd, 6));  -- top/bottom
			
			-- Proceed if the specified device exists.
			-- It can be an electrical or a non-electrical device:
			if electrical_device_exists (module, device_name) 
			or non_electrical_device_exists (module, device_name) then

				et_board_ops_devices.flip_device (
					module_cursor 	=> active_module,
					device_name		=> device_name,
					face			=> face,
					log_threshold	=> log_threshold + 1);

			else
				message_device_not_found (SEVERITY_ERROR, device_name);
			end if;
		end do_flip_2;

		
	begin
		log (text => "flip device", level => log_threshold);
		log_indentation_up;
		
		case cmd_field_count is
			when 5 =>
				do_flip_1;

			when 6 =>
				do_flip_2;

			when 7 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
		
		log_indentation_down;
	end flip_device;







	


	procedure move_device_placeholder (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		use et_device_placeholders;
		use et_device_placeholders.packages;

		meaning : type_placeholder_meaning;


		
		procedure do_it is 
		begin
			case cmd_field_count is
				when 11 =>
					move_placeholder (
						module_cursor 	=> module,
						device_name		=> to_device_name (get_field (cmd, 5)), -- IC1
						meaning			=> meaning,
						layer			=> to_placeholder_layer (get_field (cmd, 6)), -- assy
						face			=> to_face (get_field (cmd, 7)), -- top
						index			=> to_placeholder_index (get_field (cmd, 8)), -- 2
						coordinates		=> to_coordinates (get_field (cmd, 9)),  -- relative/absolute
						point			=> to_vector_model (get_field (cmd, 10), get_field (cmd, 11)),
						log_threshold	=> log_threshold + 1);

				when 12 .. type_field_count'last => 
					command_too_long (cmd, cmd_field_count - 1); 
					
				when others => command_incomplete (cmd);
			end case;
		end do_it;

		
	begin
		-- CS log message

		-- CS test existence of targeted device
		
		case noun is
			when NOUN_NAME =>
				meaning := NAME;
				
			when NOUN_VALUE =>
				meaning := VALUE;
								
			when NOUN_PURPOSE =>
				meaning := PURPOSE;

			-- CS partcode ?

			when others => null; -- CS should never happen
		end case;

		do_it;		
	end move_device_placeholder;



	






	procedure rotate_device_placeholder (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		use et_device_placeholders;
		use et_device_placeholders.packages;

		meaning : type_placeholder_meaning;

		
		procedure do_it is 
		begin
			case cmd_field_count is
				when 10 =>
					rotate_placeholder (
						module_cursor 	=> active_module,
						device_name		=> to_device_name (get_field (cmd, 5)), -- IC1
						meaning			=> meaning,
						layer			=> to_placeholder_layer (get_field (cmd, 6)), -- assy
						face			=> to_face (get_field (cmd, 7)), -- top
						index			=> to_placeholder_index (get_field (cmd, 8)), -- 2
						coordinates		=> to_coordinates (get_field (cmd, 9)),  -- relative/absolute
						rotation		=> to_rotation (get_field (cmd, 10)), -- 45
						log_threshold	=> log_threshold + 1);

				when 11 .. type_field_count'last => 
					command_too_long (cmd, cmd_field_count - 1); 
					
				when others => command_incomplete (cmd);
			end case;
		end do_it;

		
	begin
		-- CS log message

		-- CS test existence of targeted device
		
		case noun is
			when NOUN_NAME =>
				meaning := NAME;
				
			when NOUN_VALUE =>
				meaning := VALUE;
								
			when NOUN_PURPOSE =>
				meaning := PURPOSE;

			-- CS partcode ?

			when others => null; -- CS should never happen
		end case;

		do_it;		
	end rotate_device_placeholder;










	procedure restore_device_placeholders (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

	begin
		-- CS test existence of targeted device
		
		case cmd_field_count is
			when 5 =>
				reset_placeholder_positions (
					module_cursor 	=> active_module,
					device_name		=> to_device_name (get_field (cmd, 5)), -- IC1
					log_threshold	=> log_threshold + 1);

			when 6 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1); 
				
			when others => command_incomplete (cmd);
		end case;		
	end restore_device_placeholders;

	
	
end et_cp_board_device;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

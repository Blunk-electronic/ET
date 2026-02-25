------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                COMMAND PROCESSOR / SCHEMATIC / DEVICE                    --
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
-- - propose arguments if command incomplete
-- - test the existence of the requested device.
--   (see comments in et_schematic_ops_device).
--

with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;
with ada.strings; 						use ada.strings;

with et_sheets;							use et_sheets;
with et_schematic_coordinates;			use et_schematic_coordinates;
with et_schematic_geometry;				use et_schematic_geometry;

with et_device_name;					use et_device_name;
with et_device_model_names;				use et_device_model_names;
with et_package_variant_name;			use et_package_variant_name;
with et_package_variant;				use et_package_variant;
with et_device_value;					use et_device_value;
with et_device_purpose;					use et_device_purpose;
with et_device_partcode;				use et_device_partcode;

with et_schematic_ops_units;			use et_schematic_ops_units;
-- with et_schematic_ops.submodules;		use et_schematic_ops.submodules;

-- with et_assembly_variant_name;			use et_assembly_variant_name;
-- with et_assembly_variants;				use et_assembly_variants;

-- with et_module_instance;				use et_module_instance;
with et_schematic_ops_device;			use et_schematic_ops_device;



package body et_cp_schematic_device is

	use pac_generic_modules;
	use pac_geometry_2;




	procedure add_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

	begin
		-- CS log message 

		case cmd_field_count is
			when 9 =>
				-- If a virtual device is added, then no variant is required.
				add_electrical_device (
					module_cursor 	=> module,
					device_model	=> to_file_name (get_field (cmd, 5)),
					destination		=> to_position 
						(
						sheet => to_sheet (get_field (cmd, 6)),
						point => type_vector_model (set 
									(
									x => to_distance (get_field (cmd, 7)),
									y => to_distance (get_field (cmd, 8))
									)),
						rotation => to_rotation (get_field (cmd, 9))
						),
					variant			=> to_variant_name (""),
					log_threshold	=> log_threshold + 1);

				
			when 10 =>
				-- A real device requires specification of a package variant.
				add_electrical_device (
					module_cursor 	=> module,
					device_model	=> to_file_name (get_field (cmd, 5)),
					destination		=> to_position 
						(
						sheet => to_sheet (get_field (cmd, 6)),
						point => type_vector_model (set 
									(
									x => to_distance (get_field (cmd, 7)),
									y => to_distance (get_field (cmd, 8))
									)),
						rotation		=> to_rotation (get_field (cmd, 9))
						),
					variant			=> to_variant_name (get_field (cmd, 10)),
					log_threshold	=> log_threshold + 1);

			when 11 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;


	end add_device;







	procedure rename_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

	begin
		-- CS log message 

		case cmd_field_count is
			when 6 =>
				rename_electrical_device (
					module_cursor 		=> module,
					device_name_before	=> to_device_name (get_field (cmd, 5)), -- IC1
					device_name_after	=> to_device_name (get_field (cmd, 6)), -- IC23
					log_threshold		=> log_threshold + 1);

			when 7 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case; 

	end rename_device;

	






	procedure delete_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		device_name : type_device_name;
	begin
		-- CS log message 

		device_name := to_device_name (get_field (cmd, 5));
		
		case cmd_field_count is
			when 5 =>
				delete_electrical_device (
					module_cursor 	=> module,
					device_name		=> device_name,
					log_threshold	=> log_threshold + 1);

			when 6 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;

	end delete_device;



	


	


	procedure copy_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

	begin
		-- CS log message 

		case cmd_field_count is
			when 9 =>
				copy_device (
					module_cursor 	=> module,
					device_name		=> to_device_name (get_field (cmd, 5)),
					destination		=> to_position 
						(
						sheet => to_sheet (get_field (cmd, 6)),
						point => type_vector_model (set
									(
									x => to_distance (get_field (cmd, 7)),
									y => to_distance (get_field (cmd, 8))
									)),
						rotation		=> to_rotation (get_field (cmd, 9))
						),
					log_threshold	=> log_threshold + 1
					);

			when 10 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;

	end copy_device;

	





	

	procedure set_device_value (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

	begin
		-- CS log message 

		case cmd_field_count is
			when 6 =>
				declare
					value : pac_device_value.bounded_string; -- 470R
				begin
					-- validate value
					value := to_value_with_check (get_field (cmd, 6));

					-- set the value
					set_value (
						module_cursor 	=> module,
						device_name		=> to_device_name (get_field (cmd, 5)), -- R1
						value			=> value, -- 470R
						log_threshold	=> log_threshold + 1);
				end;

			when 7 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;

	end set_device_value;



	





	procedure set_device_purpose (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

	begin
		-- CS log message 

		case cmd_field_count is
			when 6 =>
				declare
					purpose : pac_device_purpose.bounded_string; -- brightness_control
				begin
					purpose := to_purpose (get_field (cmd, 6));
					
					-- set the purpose
					set_purpose (
						module_cursor 	=> module,
						device_name		=> to_device_name (get_field (cmd, 5)), -- R1
						purpose			=> purpose, -- brightness_control
						log_threshold	=> log_threshold + 1);
				end;

			when 7 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;

	end set_device_purpose;








	procedure set_device_partcode (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

	begin
		-- CS log message 

		case cmd_field_count is
			when 6 =>
				declare
					partcode : pac_device_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
				begin
					partcode := to_partcode (get_field (cmd, 6));

					-- set the purpose
					set_partcode (
						module_cursor 	=> module,
						device_name		=> to_device_name (get_field (cmd, 5)), -- R1
						partcode		=> partcode, -- R_PAC_S_0805_VAL_100R
						log_threshold	=> log_threshold + 1);
				end;

			when 7 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;

	end set_device_partcode;

	








	procedure set_device_package_variant (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		variant : pac_package_variant_name.bounded_string; -- N, D
	begin
		-- CS log message 

		case cmd_field_count is
			when 6 =>

				-- validate variant
				check_variant_name_length (get_field (cmd, 6));
				variant := to_variant_name (get_field (cmd, 6));
				check_variant_name_characters (variant);
				
				-- set the variant
				set_package_variant (
					module_cursor	=> module,
					device_name		=> to_device_name (get_field (cmd, 5)), -- IC1
					variant			=> variant, -- N, D
					log_threshold	=> log_threshold + 1);

			when 7 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;

	end set_device_package_variant;








	

	procedure renumber_devices (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		use et_device_name;
	begin
		-- CS log message

		case cmd_field_count is
			when 5 =>
				renumber_devices (
					module_name 	=> key (module),
					step_width		=> to_index (get_field (cmd, 5)), -- 100
					log_threshold	=> log_threshold + 1
					);

			when 6 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;

	end renumber_devices;

	
	
end et_cp_schematic_device;


	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

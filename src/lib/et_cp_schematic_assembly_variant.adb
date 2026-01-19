------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--              COMMAND PROCESSOR / SCHEMATIC / ASSEMBLY VARIANT            --
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
--

with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;
with ada.strings; 						use ada.strings;

with et_schematic_ops;					use et_schematic_ops;
with et_schematic_ops.units;

with et_assembly_variant_name;			use et_assembly_variant_name;
with et_assembly_variants;				use et_assembly_variants;

with et_device_name;
with et_device_purpose;
with et_device_partcode;
with et_device_value;




package body et_cp_schematic_assembly_variant is

	use pac_generic_modules;
	

	
	procedure create_assembly_variant (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

	begin
		-- CS log message
		
		case cmd_field_count is
			when 5 =>
				create_assembly_variant (
					module_name		=> key (module),
					variant_name	=> to_variant (get_field (cmd, 5)),
					log_threshold	=> log_threshold + 1);
				
			when 6 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end create_assembly_variant;






	

	procedure delete_assembly_variant (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

	begin
		-- CS log message

		case cmd_field_count is
			when 5 =>
				delete_assembly_variant (
					module_name		=> key (module),
					variant_name	=> to_variant (get_field (cmd, 5)),
					log_threshold	=> log_threshold + 1);
				
			when 6 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end delete_assembly_variant;






	

	procedure describe_assembly_variant (
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
				describe_assembly_variant (
					module_name		=> key (module),
					variant_name	=> to_variant (get_field (cmd, 5)), -- low_cost
					description		=> et_assembly_variants.to_unbounded_string (get_field (cmd, 6)), -- "the cheap version"
					log_threshold	=> log_threshold + 1);
				
			when 7 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end describe_assembly_variant;







	


	procedure mount_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		use et_schematic_ops.units;
		
		use et_device_name;
		use et_device_purpose;
		use et_device_partcode;
		use et_device_value;
		
		value : pac_device_value.bounded_string; -- 470R
		partcode : pac_device_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
		purpose : pac_device_purpose.bounded_string; -- brightness_control
	begin
		-- CS log message

		
		-- validate value
		value := to_value_with_check (get_field (cmd, 7));

		-- validate partcode
		partcode := to_partcode (get_field (cmd, 8));
		
		case cmd_field_count is
			when 8 =>
				-- set value and partcode
				mount_device (
					module_name		=> key (module),
					variant_name	=> to_variant (get_field (cmd, 5)), -- low_cost
					device			=> to_device_name (get_field (cmd, 6)), -- R1
					value			=> value, -- 220R
					partcode		=> partcode, -- R_PAC_S_0805_VAL_220R
					log_threshold	=> log_threshold + 1);

			when 9 =>
				-- optionally the purpose can be set also
				purpose := to_purpose (get_field (cmd, 9)); -- brightness_control
							
				mount_device (
					module_name		=> key (module),
					variant_name	=> to_variant (get_field (cmd, 5)), -- low_cost
					device			=> to_device_name (get_field (cmd, 6)), -- R1
					value			=> value, -- 220R
					partcode		=> partcode, -- R_PAC_S_0805_VAL_220R
					purpose			=> purpose, -- brightness_control
					log_threshold	=> log_threshold + 1);
				
			when 10 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;

	end mount_device;
	





	

	procedure unmount_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		use et_device_name;
		use et_schematic_ops.units;
	begin
		-- CS log message
		
		case cmd_field_count is
			when 6 =>									
				unmount_device
					(
					module_name		=> key (module),
					variant_name	=> to_variant (get_field (cmd, 5)), -- low_cost
					device			=> to_device_name (get_field (cmd, 6)), -- R1
					log_threshold	=> log_threshold + 1);

			when 7 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end unmount_device;





	
	

	procedure remove_device (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		use et_device_name;
		use et_schematic_ops.units;
	begin
		-- CS log message

		case cmd_field_count is
			when 6 =>
				remove_device (
					module_name		=> key (module),
					variant_name	=> to_variant (get_field (cmd, 5)), -- low_cost
					device			=> to_device_name (get_field (cmd, 6)), -- R1
					log_threshold	=> log_threshold + 1);

			when 7 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end remove_device;
	
	
end et_cp_schematic_assembly_variant;


	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16

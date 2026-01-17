------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--              COMMAND PROCESSOR / SCHEMATIC / SUBMODULE                   --
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

with et_sheets;
with et_schematic_coordinates;			use et_schematic_coordinates;
with et_schematic_geometry;				use et_schematic_geometry;

with et_submodules;
with et_netchangers;					use et_netchangers;
with et_schematic_ops;					use et_schematic_ops;
with et_schematic_ops.submodules;		use et_schematic_ops.submodules;

with et_assembly_variant_name;			use et_assembly_variant_name;
with et_assembly_variants;				use et_assembly_variants;

with et_net_names;						use et_net_names;
with et_module_instance;				use et_module_instance;



package body et_cp_schematic_submodule is

	use pac_generic_modules;
	use pac_geometry_2;




	procedure add_submodule (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		use et_sheets;
		use et_submodules;
	begin
		-- CS log message 
		
		case cmd_field_count is
			when 11 =>
				add_submodule (
					module_name 	=> key (module), -- parent module (where the submodule is to be inserted)
					file			=> to_submodule_path (get_field (cmd, 5)),
					instance		=> to_instance_name (get_field (cmd, 6)), -- submodule instance name
					position		=> to_position 
						(
						sheet => to_sheet (get_field (cmd, 7)),
						point => type_vector_model (set 
									(
									x => to_distance (get_field (cmd, 8)),
									y => to_distance (get_field (cmd, 9))
									))
						),
					size => (
						x => to_distance (get_field (cmd, 10)),
						y => to_distance (get_field (cmd, 11))
						),
					log_threshold	=> log_threshold + 1
					);

			when 12 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end add_submodule;



	


	

	procedure move_submodule (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		use et_sheets;
	begin
		-- CS log message
		
		case cmd_field_count is
			when 9 =>
				move_submodule (
					module_name 	=> key (module),
					instance		=> to_instance_name (get_field (cmd, 5)),
					coordinates		=> to_coordinates (get_field (cmd, 6)),  -- relative/absolute
					sheet			=> to_sheet_relative (get_field (cmd, 7)),
					point			=> type_vector_model (set (
								x => to_distance (get_field (cmd, 8)),
								y => to_distance (get_field (cmd, 9)))),
					log_threshold	=> log_threshold + 1
					);

			when 10 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;

	end move_submodule;



	




	procedure drag_submodule (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
	begin
		-- CS log message

		
		case cmd_field_count is
			when 8 =>
				drag_submodule (
					module_name 	=> key (module),
					instance		=> to_instance_name (get_field (cmd, 5)),
					coordinates		=> to_coordinates (get_field (cmd, 6)),  -- relative/absolute
					point			=> type_vector_model (set (
								x => to_distance (get_field (cmd, 7)),
								y => to_distance (get_field (cmd, 8)))),
					log_threshold	=> log_threshold + 1
					);

			when 9 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;

	end drag_submodule;



	





	procedure copy_submodule (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		use et_sheets;
	begin
		-- CS log message

		case cmd_field_count is
			when 9 =>
				copy_submodule (
					module_name 	=> key (module), -- parent module (where the submodule is to be copied)
					instance_origin	=> to_instance_name (get_field (cmd, 5)), -- submodule instance name
					instance_new	=> to_instance_name (get_field (cmd, 6)), -- submodule instance name
					destination		=> to_position 
						(
						sheet => to_sheet (get_field (cmd, 7)),
						point => type_vector_model (set
									(
									x => to_distance (get_field (cmd, 8)),
									y => to_distance (get_field (cmd, 9))
									))
						),
					log_threshold	=> log_threshold + 1
					);

			when 10 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;		
	end copy_submodule;





	

	

	procedure delete_submodule (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		use et_sheets;
	begin
		-- CS log message

		case cmd_field_count is
			when 5 =>
				delete_submodule (
					module_name 	=> key (module), -- parent module (where the submodule is to be deleted)
					instance		=> to_instance_name (get_field (cmd, 5)), -- submodule instance name
					log_threshold	=> log_threshold + 1
					);

			when 6 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end delete_submodule;

	



	


	
	procedure rename_submodule (
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
				rename_submodule (
					module_name		=> key (module),
					instance_old	=> to_instance_name (get_field (cmd, 5)), -- OSC1
					instance_new	=> to_instance_name (get_field (cmd, 6)), -- OSC2
					log_threshold	=> log_threshold + 1);

			when 7 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end rename_submodule;







		


	procedure mount_submodule (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);
	begin
		-- CS log message
	
		case cmd_field_count is
			when 7 =>
				mount_submodule (
					module_name		=> key (module),
					variant_parent	=> to_variant (get_field (cmd, 5)), -- low_cost
					instance		=> to_instance_name (get_field (cmd, 6)), -- OSC1
					variant_submod	=> to_variant (get_field (cmd, 7)), -- fixed_frequency
					log_threshold	=> log_threshold + 1);

			when 8 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);

		end case;
	end mount_submodule;

	








	procedure remove_submodule (
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
				remove_submodule (
					module_name		=> key (module),
					variant_parent	=> to_variant (get_field (cmd, 5)),
					instance		=> to_instance_name (get_field (cmd, 6)), -- OSC1
					log_threshold	=> log_threshold + 1);

			when 7 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end remove_submodule;




	




	procedure set_submodule_file (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		use et_submodules;
	begin
		-- CS log message
		
		case cmd_field_count is
			when 6 =>
				set_submodule_file (
					module_name 	=> key (module),
					instance		=> to_instance_name (get_field (cmd, 5)),
					file			=> to_submodule_path (get_field (cmd, 6)),
					log_threshold	=> log_threshold + 1
					);

			when 7 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end set_submodule_file;







	

	procedure build_submodules_tree (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
	begin
		-- CS log message
		
		case cmd_field_count is
			when 4 =>
				build_submodules_tree (
					module_name 	=> key (module),
					log_threshold	=> log_threshold + 1);

			when 5 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end build_submodules_tree;











	procedure check_submodules_integrity (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
	begin
		-- CS log message


		case cmd_field_count is
			when 4 =>
				check_integrity (
					module_name 	=> key (module),
					log_threshold	=> log_threshold + 1);

			when 5 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end check_submodules_integrity;




	
	
	
	


-- PORTS:
	
	
	procedure add_port_to_submodule (
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
				add_port (
					module_name 	=> key (module),
					instance		=> to_instance_name (get_field (cmd, 5)),
					port_name		=> to_net_name (get_field (cmd, 6)),
					position		=> type_vector_model (set 
								(
								x => to_distance (get_field (cmd, 7)),
								y => to_distance (get_field (cmd, 8))
								)),
					direction		=> to_port_name (get_field (cmd, 9)),
					log_threshold	=> log_threshold + 1
					);

			when 10 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end add_port_to_submodule;








	procedure drag_port_of_submodule (
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
				drag_port (
					module_name 	=> key (module),
					instance		=> to_instance_name (get_field (cmd, 5)),
					port_name		=> to_net_name (get_field (cmd, 6)),
					coordinates		=> to_coordinates (get_field (cmd, 7)),  -- relative/absolute
					point			=> type_vector_model (set (
								x => to_distance (get_field (cmd, 8)),
								y => to_distance (get_field (cmd, 9)))),
					log_threshold	=> log_threshold + 1
					);

			when 10 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end drag_port_of_submodule;

	









	procedure delete_port_of_submodule (
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
				delete_port (
					module_name 	=> key (module),
					instance		=> to_instance_name (get_field (cmd, 5)),
					port_name		=> to_net_name (get_field (cmd, 6)),
					log_threshold	=> log_threshold + 1);

			when 7 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end delete_port_of_submodule;







	
	procedure move_port_of_submodule (
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
				move_port (
					module_name 	=> key (module),
					instance		=> to_instance_name (get_field (cmd, 5)),
					port_name		=> to_net_name (get_field (cmd, 6)),
					coordinates		=> to_coordinates (get_field (cmd, 7)),  -- relative/absolute
					point			=> type_vector_model (set (
								x => to_distance (get_field (cmd, 8)),
								y => to_distance (get_field (cmd, 9)))),
					log_threshold	=> log_threshold + 1
					);

			when 10 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end move_port_of_submodule;
		

	
	
end et_cp_schematic_submodule;


	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
